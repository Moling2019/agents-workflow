;;; github-prs.el --- GitHub PRs panel for agents-workflow dashboard -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Display open GitHub PRs authored by the user in a dashboard panel.
;; Requires the `gh` CLI to be installed and authenticated.
;; Usage: M-x list-github-prs  (standalone)
;;        (github-prs-panel)   (as dashboard panel)

;;; Code:

(require 'cl-lib)

(defgroup github-prs nil
  "Monitor GitHub pull requests."
  :group 'tools
  :prefix "github-prs-")

(defcustom github-prs-author "@me"
  "GitHub author filter for PR search."
  :type 'string
  :group 'github-prs)

(defcustom github-prs-state "open"
  "PR state filter."
  :type '(choice (const "open") (const "closed") (const "merged"))
  :group 'github-prs)

(defcustom github-prs-limit 30
  "Max PRs to fetch."
  :type 'integer
  :group 'github-prs)

(defcustom github-prs-refresh-interval 120
  "Seconds between auto-refresh cycles."
  :type 'integer
  :group 'github-prs)

(defcustom github-prs-enrich t
  "When non-nil, fetch per-PR review/CI details after initial list."
  :type 'boolean
  :group 'github-prs)

(defcustom github-prs-gh-program "gh"
  "Path to the gh CLI program."
  :type 'string
  :group 'github-prs)

(defcustom github-prs-max-age-days 30
  "Hide PRs not updated in this many days.  Set to 0 to disable."
  :type 'integer
  :group 'github-prs)

;;; --- Internal state ---

(defvar-local github-prs--cache nil
  "Cached vector of PR alists from `gh search prs'.")

(defvar-local github-prs--enrichments (make-hash-table :test #'equal)
  "Hash of \"owner/repo#number\" -> enrichment alist.")

(defvar-local github-prs--list-process nil
  "Async process for the list fetch (phase 1).")

(defvar-local github-prs--enrich-process nil
  "Async process for the current enrichment fetch (phase 2).")

(defvar-local github-prs--enrich-queue nil
  "List of PRs still needing enrichment.")

;;; --- Relative time formatting ---

(defun github-prs--parse-iso-time (iso-time)
  "Parse ISO-TIME string like \"2025-01-15T10:30:00Z\" to Emacs time.
Handles the subset of ISO 8601 that `gh' outputs."
  (when (string-match
         "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
         iso-time)
    (let ((year  (string-to-number (match-string 1 iso-time)))
          (month (string-to-number (match-string 2 iso-time)))
          (day   (string-to-number (match-string 3 iso-time)))
          (hour  (string-to-number (match-string 4 iso-time)))
          (min   (string-to-number (match-string 5 iso-time)))
          (sec   (string-to-number (match-string 6 iso-time))))
      (encode-time sec min hour day month year 0))))

(defun github-prs--format-age (iso-time)
  "Format ISO-TIME as relative age (e.g., \"2h\", \"3d\")."
  (if (or (null iso-time) (string-empty-p iso-time))
      ""
    (let* ((parsed (github-prs--parse-iso-time iso-time))
           (secs (float-time (time-subtract nil parsed))))
      (cond
       ((< secs 60)    (format "%ds" (truncate secs)))
       ((< secs 3600)  (format "%dm" (truncate (/ secs 60))))
       ((< secs 86400) (format "%dh" (truncate (/ secs 3600))))
       (t              (format "%dd" (truncate (/ secs 86400))))))))

;;; --- Icon helpers ---

(defun github-prs--review-icon (decision)
  "Return icon string for review DECISION."
  (require 'claude-dashboard)
  (cond
   ((equal decision "APPROVED")
    (claude-dashboard-icon 'success))
   ((equal decision "CHANGES_REQUESTED")
    (claude-dashboard-icon 'failed))
   ((equal decision "REVIEW_REQUIRED")
    (claude-dashboard-icon 'pending))
   (t "")))

(defun github-prs--build-check-p (check)
  "Return non-nil if CHECK is an actual build/CI check.
Excludes meta-checks like pullapprove, aviator, and PullApprove Config."
  (let* ((name (or (alist-get 'name check)
                   (alist-get 'context check)
                   ""))
         (down (downcase name)))
    (not (or (string-match-p "pullapprove" down)
             (string-match-p "aviator" down)
             (string-match-p "graphite" down)
             (string-match-p "sox" down)))))

(defun github-prs--ci-icon (rollup)
  "Return icon string for build status from ROLLUP.
ROLLUP is a vector of alists with `state' or `conclusion' keys.
Only considers actual build checks, not approval/meta checks."
  (require 'claude-dashboard)
  (if (or (null rollup) (= (length rollup) 0))
      ""
    (let* ((checks (cl-remove-if-not #'github-prs--build-check-p
                                      (append rollup nil)))
           (states (mapcar (lambda (c)
                             (or (alist-get 'conclusion c)
                                 (alist-get 'state c)
                                 ""))
                           checks)))
      (if (null states)
          ""
        (cond
         ((cl-some (lambda (s) (member s '("FAILURE" "ERROR" "TIMED_OUT"
                                            "ACTION_REQUIRED" "STARTUP_FAILURE")))
                   states)
          (claude-dashboard-icon 'failed))
         ((cl-some (lambda (s) (member s '("PENDING" "" "QUEUED" "IN_PROGRESS"
                                            "WAITING" "REQUESTED")))
                   states)
          (claude-dashboard-icon 'running))
         ((cl-every (lambda (s) (member s '("SUCCESS" "NEUTRAL" "SKIPPED")))
                    states)
          (claude-dashboard-icon 'success))
         (t ""))))))

;;; --- Repo short name ---

(defun github-prs--short-repo (full-name)
  "Extract short repo name from FULL-NAME like \"owner/repo\"."
  (if (and full-name (string-match "/" full-name))
      (substring full-name (1+ (match-beginning 0)))
    (or full-name "")))

;;; --- Phase 1: List PRs ---

(defun github-prs--list-command ()
  "Return the gh command to list PRs as JSON."
  (format "NO_COLOR=1 %s search prs --author %s --state %s --json %s --limit %d 2>/dev/null"
          (shell-quote-argument github-prs-gh-program)
          (shell-quote-argument github-prs-author)
          (shell-quote-argument github-prs-state)
          "number,title,repository,updatedAt,url,isDraft,commentsCount"
          github-prs-limit))

(defun github-prs--fetch ()
  "Fetch PRs asynchronously (phase 1: list, then phase 2: enrich)."
  (when (and github-prs--list-process
             (process-live-p github-prs--list-process))
    (delete-process github-prs--list-process))
  (let* ((buf (current-buffer))
         (proc (start-process-shell-command
                "github-prs-list" nil (github-prs--list-command)))
         (output-chunks '()))
    (setq github-prs--list-process proc)
    (set-process-filter proc
                        (lambda (_p chunk) (push chunk output-chunks)))
    (set-process-sentinel
     proc
     (lambda (_p event)
       (when (string-match-p "finished" event)
         (let ((json-str (apply #'concat (nreverse output-chunks))))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (condition-case nil
                   (setq github-prs--cache (json-read-from-string json-str))
                 (error (setq github-prs--cache nil)))
               ;; Start enrichment if enabled
               (when (and github-prs-enrich github-prs--cache)
                 (github-prs--start-enrichment))))))))))

;;; --- Phase 2: Enrich PRs ---

(defun github-prs--pr-key (pr)
  "Return a unique key for PR like \"owner/repo#123\"."
  (let* ((repo (alist-get 'repository pr))
         (full-name (if (stringp repo) repo
                      (alist-get 'nameWithOwner repo)))
         (number (alist-get 'number pr)))
    (format "%s#%d" (or full-name "") number)))

(defun github-prs--start-enrichment ()
  "Build the enrichment queue and start processing."
  (setq github-prs--enrich-queue
        (cl-remove-if
         (lambda (pr)
           (gethash (github-prs--pr-key pr) github-prs--enrichments))
         (append github-prs--cache nil)))
  (github-prs--enrich-next))

(defun github-prs--enrich-next ()
  "Enrich the next PR in the queue.  Stops when queue is empty."
  (when (and github-prs--enrich-queue (buffer-live-p (current-buffer)))
    (let* ((buf (current-buffer))
           (pr (pop github-prs--enrich-queue))
           (repo (alist-get 'repository pr))
           (full-name (if (stringp repo) repo
                        (alist-get 'nameWithOwner repo)))
           (number (alist-get 'number pr))
           (key (github-prs--pr-key pr))
           (cmd (format "NO_COLOR=1 %s pr view %d --repo %s --json %s 2>/dev/null"
                        (shell-quote-argument github-prs-gh-program)
                        number
                        (shell-quote-argument (or full-name ""))
                        "reviewDecision,statusCheckRollup,mergeable"))
           (proc (start-process-shell-command
                  "github-prs-enrich" nil cmd))
           (output-chunks '()))
      (setq github-prs--enrich-process proc)
      (set-process-filter proc
                          (lambda (_p chunk) (push chunk output-chunks)))
      (set-process-sentinel
       proc
       (lambda (_p event)
         (when (string-match-p "finished" event)
           (let ((json-str (apply #'concat (nreverse output-chunks))))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (condition-case nil
                     (puthash key (json-read-from-string json-str)
                              github-prs--enrichments)
                   (error nil))
                 ;; Continue with next PR
                 (github-prs--enrich-next))))))))))

;;; --- Panel entries ---

(defun github-prs--recent-p (pr)
  "Return non-nil if PR was updated within `github-prs-max-age-days'."
  (or (<= github-prs-max-age-days 0)
      (let* ((updated (alist-get 'updatedAt pr))
             (parsed (and updated (not (string-empty-p updated))
                          (github-prs--parse-iso-time updated))))
        (or (null parsed)
            (<= (float-time (time-subtract nil parsed))
                (* github-prs-max-age-days 86400))))))

(defun github-prs--panel-entries ()
  "Return entries from the cached PR data for the dashboard."
  (if (null github-prs--cache)
      nil
    (let ((prs (cl-remove-if-not #'github-prs--recent-p
                                  (append github-prs--cache nil))))
    (mapcar
     (lambda (pr)
       (let* ((repo (alist-get 'repository pr))
              (full-name (if (stringp repo) repo
                           (alist-get 'nameWithOwner repo)))
              (number (alist-get 'number pr))
              (title (alist-get 'title pr))
              (updated (alist-get 'updatedAt pr))
              (key (github-prs--pr-key pr))
              (enrichment (gethash key github-prs--enrichments))
              (review-decision (and enrichment
                                    (alist-get 'reviewDecision enrichment)))
              (ci-rollup (and enrichment
                              (alist-get 'statusCheckRollup enrichment))))
         (list key
               (vector (github-prs--short-repo full-name)
                       (format "#%d" number)
                       (or title "")
                       (github-prs--review-icon review-decision)
                       (github-prs--ci-icon ci-rollup)
                       (github-prs--format-age updated)))))
     prs))))

;;; --- Actions ---

(defun github-prs--find-pr (row-id)
  "Find the PR matching ROW-ID in the cache."
  (cl-find row-id (append github-prs--cache nil)
           :key #'github-prs--pr-key
           :test #'equal))

(defun github-prs--browse (_panel row-id)
  "Open the PR at ROW-ID in a browser."
  (if-let ((pr (github-prs--find-pr row-id)))
      (let ((url (alist-get 'url pr)))
        (if (and url (not (string-empty-p url)))
            (browse-url url)
          (message "No URL for %s" row-id)))
    (message "PR %s not found" row-id)))

(defun github-prs--copy-url (_panel row-id)
  "Copy the URL of the PR at ROW-ID to the kill ring."
  (if-let ((pr (github-prs--find-pr row-id)))
      (let ((url (alist-get 'url pr)))
        (if (and url (not (string-empty-p url)))
            (progn (kill-new url) (message "Copied: %s" url))
          (message "No URL for %s" row-id)))
    (message "PR %s not found" row-id)))

(defcustom github-prs-repo-root "~/Documents/work/repos/"
  "Local directory containing cloned repositories.
Used to resolve repo names to local paths for forge integration."
  :type 'directory
  :group 'github-prs)

(defun github-prs--local-repo-dir (pr)
  "Return the local directory for PR's repository, or nil."
  (let* ((repo-obj (alist-get 'repository pr))
         (full-name (alist-get 'nameWithOwner repo-obj))
         (repo-name (and full-name (file-name-nondirectory full-name)))
         (dir (and repo-name
                   (expand-file-name repo-name github-prs-repo-root))))
    (when (and dir (file-directory-p dir))
      dir)))

(defun github-prs--review (_panel row-id)
  "Show a magit-diff for the PR at ROW-ID.
Fetches the latest remote refs, then diffs origin/HEAD...pr-branch."
  (if-let ((pr (github-prs--find-pr row-id)))
      (let* ((url (alist-get 'url pr))
             (number (alist-get 'number pr))
             (repo-dir (github-prs--local-repo-dir pr)))
        (cond
         ((null repo-dir)
          (if url (browse-url url) (message "PR %s not found locally" row-id))
          (message "Repo not cloned locally. Opened in browser."))
         (t
          (let ((default-directory repo-dir))
            ;; Get branch names first (fast, no fetch needed)
            (let* ((pr-json (shell-command-to-string
                             (format "%s pr view %d --json headRefName,baseRefName"
                                     github-prs-gh-program number)))
                   (parsed (condition-case nil (json-read-from-string pr-json) (error nil)))
                   (head (and parsed (alist-get 'headRefName parsed)))
                   (base (and parsed (alist-get 'baseRefName parsed))))
              (if (not (and head base))
                  (message "Could not determine PR branches for #%d" number)
                ;; Fetch just the two branches we need, then show diff
                (message "Fetching PR #%s branches..." number)
                (let* ((dir repo-dir)
                       (range (format "origin/%s...origin/%s" base head))
                       (proc (start-process
                              "github-prs-fetch" nil
                              "git" "-C" dir "fetch" "origin"
                              (format "+refs/heads/%s:refs/remotes/origin/%s" head head)
                              (format "+refs/heads/%s:refs/remotes/origin/%s" base base))))
                  (set-process-sentinel
                   proc
                   (lambda (p _event)
                     (when (= (process-exit-status p) 0)
                       (let ((default-directory dir))
                         (magit-diff-range range))
                       (message "PR #%s ready." number)))))))))))
    (message "PR %s not found" row-id)))

;;; --- Panel constructor ---

(defun github-prs-panel ()
  "Return a dashboard panel plist for GitHub PRs.
For use with `claude-dashboard-create'."
  (list
   :name "github"
   :title "GitHub PRs"
   :columns [("Repo" 20 nil) ("#" 6 nil) ("Title" 0 nil)
             ("Rev" 5 nil) ("Build" 5 nil) ("Age" 8 nil)]
   :entries #'github-prs--panel-entries
   :refresh #'github-prs--fetch
   :actions `(("RET" . ,#'github-prs--browse)
              ("r"   . ,#'github-prs--review)
              ("w"   . ,#'github-prs--copy-url))
   :interval github-prs-refresh-interval
   :context nil))

(provide 'github-prs)
;;; github-prs.el ends here
