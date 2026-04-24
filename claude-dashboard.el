;;; claude-dashboard.el --- Composable panel dashboard -*- lexical-binding: t; -*-

;; Author: Moling Zhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (all-the-icons "5.0.0"))
;; Keywords: tools, ai

;;; Commentary:
;; A composable dashboard framework where panels are declared at buffer
;; creation time and rendered as vertical sections within a single buffer.
;; Each panel implements a lightweight plist protocol; the framework provides
;; shared infrastructure for column rendering, key dispatch, refresh
;; coordination, and icon/spinner display.

;;; Code:
(require 'cl-lib)
(require 'all-the-icons)

;;;; Icon Registry

(defvar claude-dashboard-icons
  '(;; Universal status
    (running    :icon "autorenew"        :face (:foreground "#a6e3a1"))
    (success    :icon "check_circle"     :face (:foreground "#a6e3a1"))
    (failed     :icon "error_outline"    :face (:foreground "#f38ba8"))
    (error      :icon "error_outline"    :face (:foreground "#f38ba8"))
    (pending    :icon "schedule"         :face (:foreground "#f9e2af"))
    (waiting    :icon "local_cafe"       :face (:foreground "#f9e2af"))
    (idle       :icon "brightness_3"     :face (:foreground "#6c7086"))
    (canceled   :icon "block"            :face (:foreground "#6c7086"))
    (blocked    :icon "pan_tool"         :face (:foreground "#f38ba8"))
    (done       :icon "check_circle"     :face (:foreground "#a6e3a1"))
    (investigate :icon "visibility"      :face (:foreground "#94e2d5"))
    ;; Entity types
    (interactive :icon "computer"        :face (:foreground "#89b4fa"))
    (autonomous  :icon "memory"          :face (:foreground "#cba6f7"))
    (job         :icon "settings"        :face (:foreground "#89b4fa"))
    (pr          :icon "merge_type"      :face (:foreground "#cba6f7"))
    (slack       :icon "chat"            :face (:foreground "#a6e3a1"))
    ;; Section headers
    (agents      :icon "group"           :face (:foreground "#89b4fa"))
    (databricks  :icon "storage"         :face (:foreground "#fab387"))
    (pulls       :icon "merge_type"      :face (:foreground "#cba6f7"))
    (channels    :icon "forum"           :face (:foreground "#a6e3a1"))
    (jira        :icon "assignment"      :face (:foreground "#89b4fa"))
    (github      :icon "merge_type"      :face (:foreground "#cba6f7"))
    (pace        :icon "dns"             :face (:foreground "#94e2d5")))
  "Icon registry for dashboard panels.
Each entry is (NAME :icon MATERIAL-ICON-NAME :face FACE-SPEC).")

(defun claude-dashboard-icon (name &optional label)
  "Return propertized icon for NAME, optionally followed by LABEL.
Looks up NAME in `claude-dashboard-icons'.  Returns \"?\" for unknown names."
  (let* ((entry (alist-get name claude-dashboard-icons))
         (icon-name (plist-get entry :icon))
         (face (plist-get entry :face))
         (icon-str (if icon-name
                       (condition-case nil
                           (all-the-icons-material icon-name :face face)
                         (error "?"))
                     "?")))
    (if label
        (concat icon-str " " label)
      icon-str)))

;;;; Spinner

(defvar claude-dashboard--spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the running spinner animation.")

(defvar claude-dashboard--spinner-index 0
  "Current frame index for the spinner.  Global, shared across panels.")

(defun claude-dashboard-spinner ()
  "Return the current spinner frame string."
  (nth (mod claude-dashboard--spinner-index
            (length claude-dashboard--spinner-frames))
       claude-dashboard--spinner-frames))

;;;; Column Renderer

(defun claude-dashboard--compute-fill-width (columns padding window-width)
  "Compute the width for a fill (0-width) column.
COLUMNS is the column vector, PADDING is per-column padding,
WINDOW-WIDTH is the available width."
  (let ((fixed-total 0))
    (cl-loop for col across columns
             for w = (cadr col)
             when (> w 0) do (setq fixed-total (+ fixed-total w padding)))
    (max 10 (- window-width fixed-total padding))))

(defun claude-dashboard--format-cell (value width)
  "Format VALUE into a cell of WIDTH characters.
Truncates with ellipsis if too long.  Pads with spaces if too short.
VALUE may be a propertized string."
  (let* ((str (if (stringp value) value (format "%s" value)))
         (visible-len (string-width str)))
    (cond
     ((> visible-len width)
      (truncate-string-to-width str width nil nil "…"))
     ((< visible-len width)
      (concat str (make-string (- width visible-len) ?\s)))
     (t str))))

(defun claude-dashboard--format-row (columns row padding window-width)
  "Format ROW (a vector) according to COLUMNS spec.
PADDING is spaces between columns.  WINDOW-WIDTH is buffer width.
Returns a single-line string."
  (let* ((fill-w (claude-dashboard--compute-fill-width columns padding window-width))
         (pad-str (make-string padding ?\s))
         (parts '()))
    (cl-loop for i from 0 below (length columns)
             for col = (aref columns i)
             for declared-w = (cadr col)
             for w = (if (= declared-w 0) fill-w declared-w)
             for val = (if (< i (length row)) (aref row i) "")
             ;; Last column: don't truncate (let word-wrap handle it)
             do (if (= i (1- (length columns)))
                    (push (if (stringp val) val (format "%s" val)) parts)
                  (push (claude-dashboard--format-cell val w) parts)
                  (push pad-str parts)))
    (apply #'concat (nreverse parts))))

(defun claude-dashboard--format-header-row (columns padding window-width)
  "Format COLUMNS as a header row with underline face.
PADDING and WINDOW-WIDTH as in `claude-dashboard--format-row'."
  (let* ((header-vec (vconcat (mapcar (lambda (col) (car col)) columns)))
         (line (claude-dashboard--format-row columns header-vec padding window-width)))
    (propertize line 'face '(:underline t :foreground "#a6adc8"))))

;;;; Section Renderer

(defun claude-dashboard--render-section-header (panel window-width)
  "Insert the section header line for PANEL.
Shows icon + title on the left, refresh interval on the right."
  (let* ((name-sym (intern (plist-get panel :name)))
         (title (plist-get panel :title))
         (interval (plist-get panel :interval))
         (icon (claude-dashboard-icon name-sym))
         (left (concat "  " icon " "
                       (propertize title 'face '(:foreground "#cdd6f4" :weight bold))))
         (right (if interval
                    (propertize (format "[%ds auto]" interval)
                                'face '(:foreground "#585b70"))
                  ""))
         (gap (max 1 (- window-width (string-width left) (string-width right) 2))))
    (insert left (make-string gap ?\s) right "\n")))

(defun claude-dashboard--render-section (panel window-width)
  "Render PANEL as a section in the current buffer at point.
Inserts section header, column headers, data rows, and trailing blank line.
Each data row gets `dashboard-panel' and `dashboard-row-id' text properties."
  (let* ((columns (plist-get panel :columns))
         (padding (or (plist-get panel :padding) 2))
         (panel-name (plist-get panel :name))
         (entries (condition-case err
                      (funcall (plist-get panel :entries))
                    (error
                     (list (list "error"
                                 (vector (format "Error: %s"
                                                 (error-message-string err)))))))))
    ;; Section header (tagged with panel name but no row-id)
    (let ((hdr-start (point)))
      (claude-dashboard--render-section-header panel window-width)
      (insert "  " (claude-dashboard--format-header-row columns padding window-width) "\n")
      (put-text-property hdr-start (point) 'dashboard-panel panel-name)
      (put-text-property hdr-start (point) 'dashboard-section-header t))
    ;; Data rows — entries are (ID . [vector]) or (ID [vector])
    (dolist (entry entries)
      (let* ((row-id (car entry))
             (row-data (let ((rest (cdr entry)))
                         (if (vectorp rest) rest (car rest))))
             (line (concat "  " (claude-dashboard--format-row
                                  columns row-data padding window-width))))
        (let ((start (point)))
          (insert line "\n")
          (put-text-property start (point) 'dashboard-panel panel-name)
          (put-text-property start (point) 'dashboard-row-id row-id))))
    ;; Trailing blank line
    (insert "\n")))

;;;; Panel State

(defvar-local claude-dashboard--panels nil
  "List of panel plists registered in this dashboard buffer.")

(defvar-local claude-dashboard--name nil
  "Name of this dashboard instance.")

(defvar-local claude-dashboard--header-fn nil
  "Function returning header-line-format, or nil.")

(defvar-local claude-dashboard--timers nil
  "List of active refresh timers for this buffer.")

(defvar-local claude-dashboard--current-panel nil
  "Name of the panel under point (cached for mode-line).")

;;;; Panel Lookup

(defun claude-dashboard--find-panel (name)
  "Find panel with NAME in the current buffer's panel list."
  (cl-find name claude-dashboard--panels
           :key (lambda (p) (plist-get p :name))
           :test #'equal))

;;;; Key Dispatch

(defun claude-dashboard--dispatch-action (key)
  "Dispatch KEY to the panel action for the row at point.
Reads `dashboard-panel' and `dashboard-row-id' text properties."
  (let* ((panel-name (get-text-property (point) 'dashboard-panel))
         (row-id (get-text-property (point) 'dashboard-row-id))
         (panel (and panel-name (claude-dashboard--find-panel panel-name)))
         (actions (and panel (plist-get panel :actions)))
         (action (and actions (cdr (assoc key actions)))))
    (cond
     ((null panel-name)
      (message "Not on a panel row"))
     (action
      (funcall action panel row-id))
     (t
      (message "No %s action for %s panel" key panel-name)))))

;;;; Mode

(defvar claude-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'claude-dashboard-refresh-all)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `claude-dashboard-mode'.
Panel-specific keys are added dynamically by `claude-dashboard-create'.")

(define-derived-mode claude-dashboard-mode special-mode "Dashboard"
  "Major mode for the composable dashboard."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (claude-dashboard-refresh-all)))
  (add-hook 'kill-buffer-hook #'claude-dashboard--cancel-all-timers nil t)
  (add-hook 'post-command-hook #'claude-dashboard--update-mode-line nil t))

;;;; Timer Management

(defun claude-dashboard--cancel-all-timers ()
  "Cancel all panel refresh timers for this dashboard buffer."
  (dolist (timer claude-dashboard--timers)
    (when (timerp timer) (cancel-timer timer)))
  (setq claude-dashboard--timers nil))

;;;; Cursor Navigation

(defun claude-dashboard--goto-row (row-id)
  "Move point to the row with ROW-ID.  Return non-nil if found."
  (when row-id
    (let ((found nil))
      (save-excursion
        (goto-char (point-min))
        (while (and (not found) (not (eobp)))
          (when (equal (get-text-property (point) 'dashboard-row-id) row-id)
            (setq found (point)))
          (forward-line 1)))
      (when found (goto-char found) t))))

(defun claude-dashboard--goto-first-row-of-panel (panel)
  "Move point to the first data row of PANEL.  Return non-nil if found."
  (let ((panel-name (plist-get panel :name))
        (found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (when (and (equal (get-text-property (point) 'dashboard-panel) panel-name)
                   (get-text-property (point) 'dashboard-row-id))
          (setq found (point)))
        (forward-line 1)))
    (when found (goto-char found) t)))

(defun claude-dashboard--goto-first-data-row ()
  "Move point to the first data row in the buffer."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (get-text-property (point) 'dashboard-row-id)))
    (forward-line 1)))

;;;; Rendering

(defun claude-dashboard--render-all ()
  "Render all panels into the current buffer.
Erases buffer and re-renders from scratch."
  (let ((inhibit-read-only t)
        (window-width (max 80 (window-width))))
    (erase-buffer)
    (dolist (panel claude-dashboard--panels)
      ;; Call :refresh if present (async data fetch)
      (when-let ((refresh-fn (plist-get panel :refresh)))
        (condition-case err
            (funcall refresh-fn)
          (error (message "Dashboard: refresh error in %s: %s"
                          (plist-get panel :name)
                          (error-message-string err)))))
      (claude-dashboard--render-section panel window-width))))

;;;; Refresh

(defun claude-dashboard-refresh-all ()
  "Refresh all panels in the current dashboard buffer."
  (interactive)
  (when claude-dashboard--panels
    (let* ((saved-row-id (get-text-property (point) 'dashboard-row-id))
           (saved-panel (get-text-property (point) 'dashboard-panel))
           (saved-line (line-number-at-pos (point)))
           (win (get-buffer-window (current-buffer)))
           (saved-win-line (when win
                             (line-number-at-pos (window-start win)))))
      ;; Advance spinner
      (setq claude-dashboard--spinner-index
            (1+ claude-dashboard--spinner-index))
      ;; Update header
      (when claude-dashboard--header-fn
        (setq header-line-format (funcall claude-dashboard--header-fn)))
      ;; Re-render
      (claude-dashboard--render-all)
      ;; Restore cursor — try row-id first, fall back to line number
      (cond
       ((and saved-row-id (claude-dashboard--goto-row saved-row-id)))
       ((and saved-panel
             (claude-dashboard--goto-first-row-of-panel
              (claude-dashboard--find-panel saved-panel))))
       (t (goto-char (point-min))
          (forward-line (1- saved-line))))
      ;; Restore window scroll position by line number
      (when (and win saved-win-line)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- saved-win-line))
          (set-window-start win (point) t))))))

(defun claude-dashboard--refresh-panel (buf panel)
  "Refresh a single PANEL section within dashboard BUF.
Called by per-panel timers.  Re-renders only the affected section."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((panel-name (plist-get panel :name))
             (win (get-buffer-window buf))
             ;; Save window state (separate from buffer point in timers)
             (saved-win-point (when win (window-point win)))
             (saved-win-start (when win (window-start win)))
             (saved-row-id (when saved-win-point
                             (get-text-property saved-win-point
                                                'dashboard-row-id)))
             (saved-line (when saved-win-point
                           (line-number-at-pos saved-win-point)))
             (saved-win-start-line (when saved-win-start
                                     (line-number-at-pos saved-win-start)))
             (window-width (max 80 (if win (window-width win) 80))))
        ;; Advance spinner
        (setq claude-dashboard--spinner-index (1+ claude-dashboard--spinner-index))
        ;; Call :refresh if present
        (when-let ((refresh-fn (plist-get panel :refresh)))
          (condition-case err
              (funcall refresh-fn)
            (error (message "Dashboard: refresh error in %s: %s"
                            panel-name (error-message-string err)))))
        ;; Find data-row boundaries (skip section header lines)
        (let ((start nil) (end nil))
          (save-excursion
            (goto-char (point-min))
            (while (and (not start) (not (eobp)))
              (when (and (equal (get-text-property (point) 'dashboard-panel) panel-name)
                         (not (get-text-property (point) 'dashboard-section-header))
                         (get-text-property (point) 'dashboard-row-id))
                (setq start (line-beginning-position)))
              (forward-line 1))
            (when start
              (goto-char start)
              (while (and (not (eobp))
                          (equal (get-text-property (point) 'dashboard-panel) panel-name)
                          (not (get-text-property (point) 'dashboard-section-header)))
                (forward-line 1))
              (setq end (point))))
          (unless start
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (and (equal (get-text-property (point) 'dashboard-panel) panel-name)
                           (get-text-property (point) 'dashboard-section-header))
                  (while (and (not (eobp))
                              (get-text-property (point) 'dashboard-section-header))
                    (forward-line 1))
                  (setq start (point) end (point)))
                (forward-line 1))))
          ;; Re-render data rows
          (when (and start end)
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char start)
                (delete-region start end)
                (let* ((columns (plist-get panel :columns))
                       (padding (or (plist-get panel :padding) 2))
                       (entries (condition-case err
                                    (funcall (plist-get panel :entries))
                                  (error
                                   (list (list "error"
                                               (vector (format "Error: %s"
                                                               (error-message-string err)))))))))
                  (dolist (entry entries)
                    (let* ((row-id (car entry))
                           (row-data (let ((rest (cdr entry)))
                                       (if (vectorp rest) rest (car rest))))
                           (line (concat "  " (claude-dashboard--format-row
                                                columns row-data padding window-width))))
                      (let ((s (point)))
                        (insert line "\n")
                        (put-text-property s (point) 'dashboard-panel panel-name)
                        (put-text-property s (point) 'dashboard-row-id row-id))))))))
          ;; Restore window point and scroll position
          (when win
            ;; Restore point: try row-id, fall back to line number
            (let ((new-point
                   (or (and saved-row-id
                            (save-excursion
                              (when (claude-dashboard--goto-row saved-row-id)
                                (point))))
                       (when saved-line
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line (1- saved-line))
                           (point))))))
              (when new-point
                (set-window-point win new-point)))
            ;; Restore scroll position by line number
            (when saved-win-start-line
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- saved-win-start-line))
                (set-window-start win (point) t)))))))))


;;;; Context-Sensitive Mode-Line

(defun claude-dashboard--build-mode-line-for-panel (panel)
  "Build a mode-line hint list for PANEL's actions."
  (if (null panel)
      (list " " (propertize " Dashboard " 'face '(:foreground "#a6adc8")))
    (let ((parts (list " "))
          (actions (plist-get panel :actions)))
      (dolist (action actions)
        (let ((key (car action)))
          (push (propertize (format " %s " key)
                            'face '(:foreground "#1e1e2e" :background "#89b4fa"))
                parts)
          (push " " parts)))
      ;; Always add g and q
      (push (propertize " g " 'face '(:foreground "#1e1e2e" :background "#89b4fa")) parts)
      (push (propertize " refresh " 'face '(:foreground "#a6adc8")) parts)
      (nreverse parts))))

(defun claude-dashboard--update-mode-line ()
  "Update mode-line if the panel under point changed."
  (let ((panel-name (get-text-property (point) 'dashboard-panel)))
    (unless (equal panel-name claude-dashboard--current-panel)
      (setq claude-dashboard--current-panel panel-name)
      (setq mode-line-format
            (claude-dashboard--build-mode-line-for-panel
             (and panel-name (claude-dashboard--find-panel panel-name))))
      (force-mode-line-update))))

;;;; Public API

(cl-defun claude-dashboard-create (&key name header panels)
  "Create (or reuse) a dashboard buffer with NAME, HEADER fn, and PANELS.
PANELS is a list of panel plists.  HEADER is a zero-arg function
returning `header-line-format', or nil.
Returns the dashboard buffer."
  (let* ((buf-name (format "*claude-dashboard:%s*" name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      ;; If re-entering, clean up old timers
      (when claude-dashboard--timers
        (claude-dashboard--cancel-all-timers))
      ;; Activate mode (idempotent if already active)
      (unless (eq major-mode 'claude-dashboard-mode)
        (claude-dashboard-mode))
      ;; Register state
      (setq claude-dashboard--name name)
      (setq claude-dashboard--header-fn header)
      (setq claude-dashboard--panels panels)
      ;; Dynamically bind panel action keys
      (dolist (panel panels)
        (dolist (action (plist-get panel :actions))
          (let ((key (car action)))
            (define-key claude-dashboard-mode-map (kbd key)
              (let ((k key))
                (lambda () (interactive)
                  (claude-dashboard--dispatch-action k)))))))
      ;; Set header
      (when header
        (setq header-line-format (funcall header)))
      ;; Initial render
      (claude-dashboard-refresh-all)
      ;; Start per-panel timers
      (dolist (panel panels)
        (when-let ((interval (plist-get panel :interval)))
          (let ((timer (run-at-time interval interval
                                    #'claude-dashboard--refresh-panel
                                    buf panel)))
            (push timer claude-dashboard--timers)))))
    buf))

(provide 'claude-dashboard)
;;; claude-dashboard.el ends here
