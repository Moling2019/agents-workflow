;;; jira-board.el --- Jira board panel for agents-workflow dashboard -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Display Jira issues grouped by epic in a dashboard panel.
;; Requires JIRA_EMAIL and JIRA_API_TOKEN in the env file.
;; Usage: M-x list-jira-board  (standalone)
;;        (jira-board-panel)   (as dashboard panel)

;;; Code:

(require 'cl-lib)

(defgroup jira-board nil
  "Monitor Jira issues."
  :group 'tools
  :prefix "jira-board-")

(defcustom jira-board-refresh-interval 120
  "Seconds between auto-refresh cycles."
  :type 'integer
  :group 'jira-board)

(defcustom jira-board-python "python3"
  "Path to Python interpreter with requests installed."
  :type 'string
  :group 'jira-board)

(defcustom jira-board-env-file nil
  "Path to .env file with JIRA_EMAIL and JIRA_API_TOKEN."
  :type '(choice (const nil) string)
  :group 'jira-board)

(defcustom jira-board-site nil
  "Atlassian site hostname (e.g., \"mycompany.atlassian.net\")."
  :type '(choice (const nil) string)
  :group 'jira-board)

(defcustom jira-board-project nil
  "Jira project key to query (e.g., \"PROJ\")."
  :type '(choice (const nil) string)
  :group 'jira-board)

(defcustom jira-board-jql nil
  "Custom JQL override.  When nil, uses default query for `jira-board-project'."
  :type '(choice (const nil) string)
  :group 'jira-board)

;;; --- Internal state ---

(defvar-local jira-board--cache nil
  "Cached JSON issue data for panel entries.")

(defvar-local jira-board--json-process nil
  "Async process for JSON fetch.  Buffer-local to avoid races.")

(defvar-local jira-board--pending-refresh nil
  "When non-nil, refresh the dashboard after the next fetch completes.")

(defvar-local jira-board--expanded-epics (make-hash-table :test #'equal)
  "Set of epic keys whose Done children are visible.")

(defvar-local jira-board--done-cache (make-hash-table :test #'equal)
  "Hash-table mapping epic key → vector of Done child issues.")

;;; --- Python script ---

(defun jira-board--python-script ()
  "Return a Python script that fetches Jira issues and outputs JSON."
  (format "
import requests, json, os, sys
email = os.environ['JIRA_EMAIL']
token = os.environ['JIRA_API_TOKEN']
site = %S
project = %S
jql = %S or ('project = ' + project + ' AND assignee = currentUser() AND status != Done ORDER BY issuetype ASC, rank ASC')
url = 'https://' + site + '/rest/api/3/search/jql'
payload = {'jql': jql, 'maxResults': 50,
           'fields': ['summary','status','issuetype','parent']}
resp = requests.post(url, json=payload, auth=(email, token))
resp.raise_for_status()
issues = []
for issue in resp.json()['issues']:
    f = issue['fields']
    parent = f.get('parent') or {}
    pf = parent.get('fields') or {}
    issues.append({
        'key': issue['key'],
        'summary': f['summary'],
        'status': f['status']['name'],
        'status_category': f['status']['statusCategory']['key'],
        'issuetype': f['issuetype']['name'],
        'parent_key': parent.get('key', ''),
        'parent_summary': pf.get('summary', ''),
        'url': 'https://' + site + '/browse/' + issue['key']
    })
print(json.dumps(issues))
" jira-board-site
  jira-board-project
  (or jira-board-jql "")))

;;; --- Env helper ---

(defun jira-board--env-shell-prefix ()
  "Return shell command prefix that sources the .env file."
  (if (file-exists-p jira-board-env-file)
      (format "set -a && source %s && set +a && "
              (shell-quote-argument jira-board-env-file))
    ""))

;;; --- Fetch ---

(defun jira-board--fetch-json ()
  "Fetch issues as JSON and populate `jira-board--cache'."
  (when (and jira-board--json-process
             (process-live-p jira-board--json-process))
    (delete-process jira-board--json-process))
  (let* ((buf (current-buffer))
         (cmd (concat (jira-board--env-shell-prefix)
                      (shell-quote-argument jira-board-python)
                      " -c "
                      (shell-quote-argument (jira-board--python-script))))
         (proc (start-process-shell-command "jira-board-json" nil cmd))
         (output-chunks '()))
    (setq jira-board--json-process proc)
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
                   (setq jira-board--cache (json-read-from-string json-str))
                 (error (setq jira-board--cache nil)))
               (when jira-board--pending-refresh
                 (setq jira-board--pending-refresh nil)
                 (claude-dashboard-refresh-all))))))))))

;;; --- Status icon ---

(defun jira-board--status-icon (status status-category)
  "Return a dashboard icon for STATUS name and STATUS-CATEGORY."
  (require 'claude-dashboard)
  (let ((icon-name (cond
                    ((equal status "Blocked") 'blocked)
                    ((equal status-category "indeterminate") 'running)
                    ((equal status-category "new") 'pending)
                    ((equal status-category "done") 'success)
                    (t 'idle))))
    (claude-dashboard-icon icon-name)))

;;; --- Entries (grouped by epic) ---

(defun jira-board--panel-entries ()
  "Return entries from the cached JSON data, grouped by epic."
  (if (null jira-board--cache)
      nil
    (let* ((issues (append jira-board--cache nil))
           ;; Separate epics and sub-tasks
           (epics (cl-remove-if-not
                   (lambda (i) (equal (alist-get 'issuetype i) "Epic"))
                   issues))
           (tasks (cl-remove-if
                   (lambda (i) (equal (alist-get 'issuetype i) "Epic"))
                   issues))
           ;; Group tasks by parent_key
           (task-groups (make-hash-table :test #'equal))
           (orphan-tasks '())
           (entries '()))
      ;; Bucket tasks under their parent epic
      (dolist (task tasks)
        (let ((parent (alist-get 'parent_key task)))
          (if (and parent (not (string-empty-p parent)))
              (push task (gethash parent task-groups))
            (push task orphan-tasks))))
      ;; Emit epics with their children
      (dolist (epic epics)
        (let* ((key (alist-get 'key epic))
               (summary (alist-get 'summary epic))
               (status (alist-get 'status epic))
               (status-cat (alist-get 'status_category epic))
               (children (nreverse (gethash key task-groups))))
          ;; Epic header row — show ▾ if expanded, ▸ if collapsed
          (let ((expanded (gethash key jira-board--expanded-epics))
                (arrow (if (gethash key jira-board--expanded-epics) "▾" "▸")))
            (push (list (concat "epic:" key)
                        (vector key
                                (jira-board--status-icon status status-cat)
                                (propertize (format "%s %s" arrow summary)
                                            'face '(:weight bold :foreground "#cdd6f4"))))
                  entries)
            ;; Child task rows (active)
            (dolist (task children)
              (push (list (alist-get 'key task)
                          (vector (alist-get 'key task)
                                  (jira-board--status-icon
                                   (alist-get 'status task)
                                   (alist-get 'status_category task))
                                  (concat "  " (alist-get 'summary task))))
                    entries))
            ;; Done children (when expanded)
            (when expanded
              (dolist (task (append (gethash key jira-board--done-cache) nil))
                (push (list (alist-get 'key task)
                            (vector (alist-get 'key task)
                                    (jira-board--status-icon
                                     (alist-get 'status task)
                                     (alist-get 'status_category task))
                                    (propertize
                                     (concat "  " (alist-get 'summary task))
                                     'face '(:foreground "#6c7086"))))
                      entries))))
          ;; Remove from hash so we can detect orphan epics
          (remhash key task-groups)))
      ;; Emit orphan tasks (no epic parent, or parent not in results)
      (maphash (lambda (_k tasks)
                 (dolist (task (nreverse tasks))
                   (push (list (alist-get 'key task)
                               (vector (alist-get 'key task)
                                       (jira-board--status-icon
                                        (alist-get 'status task)
                                        (alist-get 'status_category task))
                                       (alist-get 'summary task)))
                         entries)))
               task-groups)
      (dolist (task (nreverse orphan-tasks))
        (push (list (alist-get 'key task)
                    (vector (alist-get 'key task)
                            (jira-board--status-icon
                             (alist-get 'status task)
                             (alist-get 'status_category task))
                            (alist-get 'summary task)))
              entries))
      (nreverse entries))))

;;; --- Transition ---

(defconst jira-board--transitions
  '(("In Progress" . "31")
    ("Backlog"     . "11")
    ("Done"        . "41")
    ("Blocked"     . "61")
    ("In Review"   . "51")
    ("Selected for Development" . "21"))
  "Alist mapping transition names to Jira transition IDs.")

(defun jira-board--transition-python-script (issue-key transition-id)
  "Return a Python script to transition ISSUE-KEY to TRANSITION-ID."
  (format "
import requests, os, sys
email = os.environ['JIRA_EMAIL']
token = os.environ['JIRA_API_TOKEN']
site = %S
key = %S
tid = %S
url = 'https://' + site + '/rest/api/3/issue/' + key + '/transitions'
resp = requests.post(url, json={'transition': {'id': tid}}, auth=(email, token))
if resp.status_code == 204:
    print('OK')
else:
    print('ERROR: ' + str(resp.status_code) + ' ' + resp.text, file=sys.stderr)
    sys.exit(1)
" jira-board-site issue-key transition-id))

(defun jira-board--transition-issue (issue-key transition-name)
  "Transition ISSUE-KEY to TRANSITION-NAME via Jira API.
Runs synchronously and returns non-nil on success."
  (let* ((tid (alist-get transition-name jira-board--transitions nil nil #'equal))
         (cmd (concat (jira-board--env-shell-prefix)
                      (shell-quote-argument jira-board-python)
                      " -c "
                      (shell-quote-argument
                       (jira-board--transition-python-script issue-key tid))))
         (exit-code (call-process-shell-command cmd nil nil nil)))
    (if (= exit-code 0)
        (progn (message "%s → %s" issue-key transition-name) t)
      (message "Failed to transition %s to %s" issue-key transition-name)
      nil)))

(defun jira-board--do-transition (_panel row-id transition-name)
  "Transition the issue at ROW-ID to TRANSITION-NAME, then refresh."
  (let ((key (if (string-prefix-p "epic:" row-id)
                 (substring row-id 5)
               row-id)))
    (when (jira-board--transition-issue key transition-name)
      ;; Trigger async refresh; re-render dashboard when fetch completes
      (setq jira-board--pending-refresh t)
      (jira-board--fetch-json))))

;;; --- Toggle Done children ---

(defun jira-board--done-python-script (epic-key)
  "Return a Python script that fetches Done children of EPIC-KEY."
  (format "
import requests, json, os
email = os.environ['JIRA_EMAIL']
token = os.environ['JIRA_API_TOKEN']
site = %S
jql = 'parent = %s AND assignee = currentUser() AND status = Done ORDER BY rank ASC'
url = 'https://' + site + '/rest/api/3/search/jql'
payload = {'jql': jql, 'maxResults': 50,
           'fields': ['summary','status','issuetype','parent']}
resp = requests.post(url, json=payload, auth=(email, token))
resp.raise_for_status()
issues = []
for issue in resp.json()['issues']:
    f = issue['fields']
    parent = f.get('parent') or {}
    pf = parent.get('fields') or {}
    issues.append({
        'key': issue['key'],
        'summary': f['summary'],
        'status': f['status']['name'],
        'status_category': f['status']['statusCategory']['key'],
        'issuetype': f['issuetype']['name'],
        'parent_key': parent.get('key', ''),
        'parent_summary': pf.get('summary', ''),
        'url': 'https://' + site + '/browse/' + issue['key']
    })
print(json.dumps(issues))
" jira-board-site epic-key))

(defun jira-board--fetch-done-sync (epic-key)
  "Synchronously fetch Done children for EPIC-KEY.  Returns a vector or nil."
  (let* ((cmd (concat (jira-board--env-shell-prefix)
                      (shell-quote-argument jira-board-python)
                      " -c "
                      (shell-quote-argument
                       (jira-board--done-python-script epic-key))))
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process-shell-command cmd nil t nil)))))
    (condition-case nil
        (json-read-from-string output)
      (error nil))))

(defun jira-board--toggle-done (_panel row-id)
  "Toggle display of Done children for the epic at ROW-ID."
  (let ((epic-key (if (string-prefix-p "epic:" row-id)
                      (substring row-id 5)
                    ;; If on a child task, find its parent epic
                    (when-let ((issue (cl-find row-id
                                               (append jira-board--cache nil)
                                               :key (lambda (i) (alist-get 'key i))
                                               :test #'equal)))
                      (alist-get 'parent_key issue)))))
    (if (null epic-key)
        (message "Not on an epic row")
      (if (gethash epic-key jira-board--expanded-epics)
          ;; Collapse
          (progn
            (remhash epic-key jira-board--expanded-epics)
            (message "%s: Done tasks hidden" epic-key))
        ;; Expand — fetch Done children
        (message "Fetching done tasks for %s..." epic-key)
        (let ((done-issues (jira-board--fetch-done-sync epic-key)))
          (if (and done-issues (> (length done-issues) 0))
              (progn
                (puthash epic-key done-issues jira-board--done-cache)
                (puthash epic-key t jira-board--expanded-epics)
                (message "%s: %d done task(s)" epic-key (length done-issues)))
            (puthash epic-key t jira-board--expanded-epics)
            (message "%s: no done tasks" epic-key))))
      ;; Refresh display immediately
      (claude-dashboard-refresh-all))))

;;; --- Browse action ---

(defun jira-board--browse (_panel row-id)
  "Open the Jira issue ROW-ID in a browser."
  (if (string-prefix-p "epic:" row-id)
      ;; Epic header — browse the epic itself
      (let ((key (substring row-id 5)))
        (browse-url (format "https://%s/browse/%s" jira-board-site key)))
    ;; Regular issue
    (if-let ((issue (cl-find row-id
                             (append jira-board--cache nil)
                             :key (lambda (i) (alist-get 'key i))
                             :test #'equal)))
        (let ((url (alist-get 'url issue)))
          (if (and url (not (string-empty-p url)))
              (browse-url url)
            (message "No URL for %s" row-id)))
      (message "Issue %s not found — try refreshing with 'g'" row-id))))

;;; --- Panel constructor ---

(defun jira-board-panel ()
  "Return a dashboard panel plist for Jira issues.
For use with `claude-dashboard-create'."
  (list
   :name "jira"
   :title "Jira Board"
   :columns [("Key" 10 nil) ("S" 3 nil) ("Summary" 0 nil)]
   :entries #'jira-board--panel-entries
   :refresh #'jira-board--fetch-json
   :actions `(("RET" . ,#'jira-board--browse)
              ("i" . ,(lambda (panel row-id)
                        (jira-board--do-transition panel row-id "In Progress")))
              ("b" . ,(lambda (panel row-id)
                        (jira-board--do-transition panel row-id "Backlog")))
              ("d" . ,(lambda (panel row-id)
                        (jira-board--do-transition panel row-id "Done")))
              ("B" . ,(lambda (panel row-id)
                        (jira-board--do-transition panel row-id "Blocked")))
              ("C-o" . ,#'jira-board--toggle-done))
   :interval jira-board-refresh-interval
   :context nil))

;;; --- Standalone buffer ---

(defvar jira-board--standalone-timer nil
  "Timer for standalone buffer auto-refresh.")

(defun jira-board--standalone-refresh ()
  "Refresh the standalone Jira board buffer."
  (interactive)
  (let ((buf (get-buffer "*Jira Board*")))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (jira-board--fetch-json)
        ;; Re-render after a short delay for async fetch
        (run-at-time 3 nil
                     (lambda ()
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (let ((inhibit-read-only t)
                                 (entries (jira-board--panel-entries)))
                             (erase-buffer)
                             (insert (propertize
                                      (format "Jira Board — %s  |  %s\n\n"
                                              jira-board-project
                                              (format-time-string "%H:%M:%S"))
                                      'face '(:foreground "#a6adc8" :italic t)))
                             (if (null entries)
                                 (insert "  (no issues)\n")
                               (dolist (entry entries)
                                 (let* ((row-data (cadr entry))
                                        (key (aref row-data 0))
                                        (status (aref row-data 1))
                                        (summary (aref row-data 2)))
                                   (insert (format "  %-10s %s  %s\n"
                                                   key status summary))))))))))))))

;;;###autoload
(defun list-jira-board ()
  "Open a standalone buffer showing Jira issues."
  (interactive)
  (let ((buf (get-buffer-create "*Jira Board*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading Jira issues..."))
      ;; Start timer
      (when jira-board--standalone-timer
        (cancel-timer jira-board--standalone-timer))
      (setq jira-board--standalone-timer
            (run-at-time jira-board-refresh-interval
                         jira-board-refresh-interval
                         #'jira-board--standalone-refresh))
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when jira-board--standalone-timer
                    (cancel-timer jira-board--standalone-timer)
                    (setq jira-board--standalone-timer nil)))
                nil t))
    (pop-to-buffer buf)
    (jira-board--standalone-refresh)))

(provide 'jira-board)
;;; jira-board.el ends here
