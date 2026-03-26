;;; slack-monitor.el --- Slack message panel for agents-workflow dashboard -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Display Slack messages from a JSON cache in a dashboard panel.
;; The cache is written by an interactive Claude Code agent running /loop.
;; Usage: (slack-monitor-panel)  (as dashboard panel)

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup slack-monitor nil
  "Slack message monitoring panel."
  :group 'tools
  :prefix "slack-monitor-")

(defcustom slack-monitor-cache-file
  (locate-user-emacs-file "agents-workflow/.slack-monitor/messages.json")
  "Path to the JSON cache file written by the Slack monitor agent."
  :type 'string
  :group 'slack-monitor)

(defcustom slack-monitor-ignore-file
  (locate-user-emacs-file "agents-workflow/.slack-monitor/ignore.yaml")
  "Path to the persistent ignore list YAML file."
  :type 'string
  :group 'slack-monitor)

(defcustom slack-monitor-refresh-interval 30
  "Seconds between dashboard panel refreshes."
  :type 'integer
  :group 'slack-monitor)

;;; --- Internal state ---

(defvar-local slack-monitor--cache nil
  "Cached message data parsed from the JSON cache file.
A vector of alists, or nil.")

(defvar-local slack-monitor--expanded nil
  "When non-nil, show up to 30 messages.  When nil, show top 5.")

;;; --- JSON cache reading ---

(defun slack-monitor--read-json-cache ()
  "Read and parse the JSON cache file into `slack-monitor--cache'."
  (if (and (file-exists-p slack-monitor-cache-file)
           (> (file-attribute-size (file-attributes slack-monitor-cache-file)) 0))
      (condition-case nil
          (let* ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'symbol)
                 (data (json-read-file slack-monitor-cache-file)))
            (setq slack-monitor--cache (alist-get 'messages data)))
        (error (setq slack-monitor--cache nil)))
    (setq slack-monitor--cache nil)))

;;; --- Status icons ---

(defun slack-monitor--status-icon (status)
  "Return a dashboard icon for message STATUS string."
  (require 'claude-dashboard)
  (let ((icon-name (cond
                    ((equal status "needs_reply") 'pending)
                    ((equal status "replied") 'success)
                    ((equal status "skipped") 'idle)
                    ((equal status "ignored") 'canceled)
                    ((equal status "investigate") 'investigate)
                    ((equal status "info") 'idle)
                    (t 'idle))))
    (claude-dashboard-icon icon-name)))

;;; --- Time formatting ---

(defun slack-monitor--format-time (iso-timestamp)
  "Format ISO-TIMESTAMP (UTC) to a short US Eastern time string like \"10:30a\"."
  (if (or (null iso-timestamp) (string-empty-p iso-timestamp))
      "—"
    (condition-case nil
        (let* (;; Parse the ISO timestamp and encode as a time value
               (parsed (parse-time-string iso-timestamp))
               (hour (nth 2 parsed))
               (min (nth 1 parsed))
               (sec (or (nth 0 parsed) 0))
               (day (or (nth 3 parsed) 1))
               (month (or (nth 4 parsed) 1))
               (year (or (nth 5 parsed) 2026))
               ;; Encode as UTC time, then decode in US Eastern
               (utc-time (encode-time sec min hour day month year t))
               (eastern (decode-time utc-time "America/New_York"))
               (e-hour (nth 2 eastern))
               (e-min (nth 1 eastern))
               ;; Format as 12-hour with a/p suffix
               (display-hour (cond ((= e-hour 0) 12)
                                   ((> e-hour 12) (- e-hour 12))
                                   (t e-hour)))
               (ampm (if (>= e-hour 12) "p" "a")))
          (if (and e-hour e-min)
              (format "%d:%02d%s" display-hour e-min ampm)
            "—"))
      (error "—"))))

;;; --- Entries ---

(defun slack-monitor--status-priority (status)
  "Return sort priority for STATUS (lower = shown first).
Active statuses sort first, resolved statuses sort last."
  (cond
   ((equal status "needs_reply") 0)
   ((equal status "investigate") 1)
   ((equal status "info") 2)
   ((equal status "replied") 3)
   ((equal status "skipped") 4)
   ((equal status "ignored") 5)
   (t 6)))

(defun slack-monitor--panel-entries ()
  "Return entries from the cached JSON data.
When collapsed, show top 5 messages.  When expanded, show up to 30.
Messages are sorted by status priority (needs_reply first), then by
recency (newest first)."
  (if (null slack-monitor--cache)
      nil
    (let* ((all-msgs (append slack-monitor--cache nil))
           ;; Sort: active statuses first, then newest first within each group
           (sorted (sort all-msgs
                         (lambda (a b)
                           (let ((pa (slack-monitor--status-priority
                                      (alist-get 'status a)))
                                 (pb (slack-monitor--status-priority
                                      (alist-get 'status b))))
                             (if (= pa pb)
                                 ;; Same priority — sort by timestamp descending
                                 (string> (or (alist-get 'timestamp a) "")
                                          (or (alist-get 'timestamp b) ""))
                               (< pa pb))))))
           (limit (if slack-monitor--expanded 30 5))
           (msgs (cl-subseq sorted 0 (min limit (length sorted)))))
      (mapcar
       (lambda (msg)
         (let-alist msg
           (list .id
                 (vector (slack-monitor--status-icon .status)
                         .from
                         .channel_name
                         (or .text_preview "")
                         (slack-monitor--format-time .timestamp)))))
       msgs))))

;;; --- Ignore list I/O ---

(defun slack-monitor--read-ignore-list ()
  "Read the ignore list YAML file and return a list of entry alists.
Returns nil if the file is missing or empty."
  (when (and (file-exists-p slack-monitor-ignore-file)
             (> (file-attribute-size (file-attributes slack-monitor-ignore-file)) 0))
    (with-temp-buffer
      (insert-file-contents slack-monitor-ignore-file)
      (let ((entries '())
            (current nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ;; New entry starts with "  - thread_ts:"
             ((string-match "^  - thread_ts: *\"?\\([^\"]+\\)\"?" line)
              (when current (push (nreverse current) entries))
              (setq current (list (cons 'thread_ts (match-string 1 line)))))
             ;; Continuation field
             ((and current (string-match "^    \\([a-z_]+\\): *\"?\\([^\"]*\\)\"?" line))
              (push (cons (intern (match-string 1 line)) (match-string 2 line))
                    current))))
          (forward-line 1))
        (when current (push (nreverse current) entries))
        (nreverse entries)))))

(defun slack-monitor--append-ignore-entry (thread-ts channel)
  "Append a thread to the ignore list YAML file.
THREAD-TS is the Slack thread timestamp, CHANNEL is the channel ID."
  (let ((dir (file-name-directory slack-monitor-ignore-file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    ;; Create file with header if missing
    (unless (file-exists-p slack-monitor-ignore-file)
      (with-temp-file slack-monitor-ignore-file
        (insert "ignored_threads:\n")))
    ;; Append entry
    (with-temp-buffer
      (insert (format "  - thread_ts: \"%s\"\n    channel: %s\n    added: %s\n"
                      thread-ts channel
                      (format-time-string "%Y-%m-%d")))
      (append-to-file (point-min) (point-max) slack-monitor-ignore-file))))

;;; --- Actions ---

(defcustom slack-monitor-team-id "E02180LFT96"
  "Slack team/enterprise ID for deep links."
  :type 'string
  :group 'slack-monitor)

(defun slack-monitor--web-url-to-deep-link (url)
  "Convert a Slack web URL to a slack:// deep link.
E.g. https://myworkspace.slack.com/archives/C123/p1234567890123456 becomes
slack://channel?team=...&id=C123&message=1234567890.123456."
  (if (string-match "slack\\.com/archives/\\([A-Z0-9]+\\)/p\\([0-9]+\\)" url)
      (let* ((channel-id (match-string 1 url))
             (raw-ts (match-string 2 url))
             (ts (if (> (length raw-ts) 6)
                     (concat (substring raw-ts 0 (- (length raw-ts) 6))
                             "." (substring raw-ts (- (length raw-ts) 6)))
                   raw-ts)))
        (format "slack://channel?team=%s&id=%s&message=%s"
                slack-monitor-team-id channel-id ts))
    url))

(defun slack-monitor--browse (_panel row-id)
  "Open the Slack message ROW-ID directly in the Slack app."
  (if-let ((msg (cl-find row-id (append slack-monitor--cache nil)
                         :key (lambda (m) (alist-get 'id m))
                         :test #'equal)))
      (let ((url (alist-get 'url msg)))
        (if (and url (not (string-empty-p url)))
            (start-process "slack-open" nil
                           "open" (slack-monitor--web-url-to-deep-link url))
          (message "No URL for message %s" row-id)))
    (message "Message %s not found — try refreshing with 'g'" row-id)))

(defun slack-monitor--ignore-thread (_panel row-id)
  "Add the thread for ROW-ID to the ignore list and refresh."
  (if-let ((msg (cl-find row-id (append slack-monitor--cache nil)
                         :key (lambda (m) (alist-get 'id m))
                         :test #'equal)))
      (let ((thread-ts (or (alist-get 'thread_ts msg) ""))
            (channel (alist-get 'channel_id msg)))
        (if (string-empty-p thread-ts)
            (message "No thread timestamp for %s — cannot ignore" row-id)
          (slack-monitor--append-ignore-entry thread-ts channel)
          (message "Ignored thread %s in %s" thread-ts channel)
          (claude-dashboard-refresh-all)))
    (message "Message %s not found" row-id)))

(defun slack-monitor--toggle-expanded (_panel _row-id)
  "Toggle between showing 5 and 30 Slack messages."
  (setq slack-monitor--expanded (not slack-monitor--expanded))
  (message "Slack: showing %s" (if slack-monitor--expanded "top 30" "top 5"))
  (claude-dashboard-refresh-all))

;;; --- Refresh ---

(defun slack-monitor--refresh-cache ()
  "Refresh the panel cache by re-reading the JSON file.
This is the panel's :refresh function."
  (slack-monitor--read-json-cache))

;;; --- Panel constructor ---

(defun slack-monitor-panel ()
  "Return a dashboard panel plist for Slack messages.
For use with `claude-dashboard-create'."
  (list
   :name "slack"
   :title "Slack Messages"
   :columns [("S" 3 nil) ("From" 14 nil) ("Channel" 12 nil)
             ("Preview" 0 nil) ("Time" 8 nil)]
   :entries #'slack-monitor--panel-entries
   :refresh #'slack-monitor--refresh-cache
   :actions `(("RET" . ,#'slack-monitor--browse)
              ("x"   . ,#'slack-monitor--ignore-thread)
              ("C-o" . ,#'slack-monitor--toggle-expanded))
   :interval slack-monitor-refresh-interval
   :context nil))

(provide 'slack-monitor)
;;; slack-monitor.el ends here
