;;; claude-dashboard-tests.el --- Tests for claude-dashboard -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)
(require 'claude-dashboard)

;;;; Icon registry tests

(ert-deftest claude-dashboard-test-icon-lookup ()
  "Looking up a registered icon returns a propertized string."
  (let ((result (claude-dashboard-icon 'running)))
    (should (stringp result))
    (should (get-text-property 0 'face result))))

(ert-deftest claude-dashboard-test-icon-unknown ()
  "Looking up an unknown icon returns a fallback."
  (let ((result (claude-dashboard-icon 'nonexistent)))
    (should (stringp result))
    (should (string= result "?"))))

(ert-deftest claude-dashboard-test-icon-with-label ()
  "Icon with label concatenates icon and label."
  (let ((result (claude-dashboard-icon 'running "my-label")))
    (should (string-match-p "my-label" result))))

(ert-deftest claude-dashboard-test-spinner ()
  "Spinner returns a string from the frames list."
  (let ((claude-dashboard--spinner-index 0))
    (should (stringp (claude-dashboard-spinner)))
    (should (member (claude-dashboard-spinner)
                    claude-dashboard--spinner-frames))))

(ert-deftest claude-dashboard-test-spinner-advances ()
  "Spinner returns different frames for different indices."
  (let ((claude-dashboard--spinner-index 0))
    (let ((frame0 (claude-dashboard-spinner)))
      (setq claude-dashboard--spinner-index 1)
      (should-not (equal frame0 (claude-dashboard-spinner))))))

;;;; Column renderer tests

(ert-deftest claude-dashboard-test-format-row-basic ()
  "Format a row with fixed-width columns."
  (let* ((columns [("Name" 10 nil) ("Status" 8 nil)])
         (row ["alice" "running"])
         (result (claude-dashboard--format-row columns row 2 80)))
    (should (stringp result))
    ;; Name(10) + pad(2) + Status(7, last col no pad) = 19
    (should (>= (length result) 19))))

(ert-deftest claude-dashboard-test-format-row-fill-column ()
  "Width 0 column fills remaining space."
  (let* ((columns [("Name" 10 nil) ("Output" 0 nil)])
         (row ["alice" "hello world"])
         (result (claude-dashboard--format-row columns row 2 80)))
    (should (string-match-p "hello world" result))))

(ert-deftest claude-dashboard-test-format-header-row ()
  "Format column headers as a header row."
  (let* ((columns [("Name" 10 nil) ("Status" 8 nil)])
         (result (claude-dashboard--format-header-row columns 2 80)))
    (should (string-match-p "Name" result))
    (should (string-match-p "Status" result))))

(ert-deftest claude-dashboard-test-format-row-truncates ()
  "Values longer than column width are truncated (non-last columns)."
  (let* ((columns [("Name" 5 nil) ("Other" 5 nil)])
         (row ["longname" "ok"])
         (result (claude-dashboard--format-row columns row 2 80)))
    ;; First column (not last) should be truncated to 5 chars
    ;; "long…" (5) + pad(2) + "ok" = starts correctly
    (should (string-match-p "long" result))))

;;;; Section renderer tests

(ert-deftest claude-dashboard-test-render-section ()
  "Rendering a panel section produces section header + column headers + data rows."
  (let ((panel (list :name "test"
                     :title "Test Panel"
                     :columns [("Col1" 10 nil) ("Col2" 8 nil)]
                     :entries (lambda () '(("r1" . ["a" "b"]) ("r2" . ["c" "d"])))
                     :interval 5)))
    (with-temp-buffer
      (claude-dashboard--render-section panel 80)
      (let ((text (buffer-string)))
        ;; Section header present
        (should (string-match-p "Test Panel" text))
        ;; Refresh interval shown
        (should (string-match-p "5s" text))
        ;; Column headers
        (should (string-match-p "Col1" text))
        (should (string-match-p "Col2" text))
        ;; Data rows
        (should (string-match-p "a" text))
        (should (string-match-p "d" text))))))

(ert-deftest claude-dashboard-test-render-section-text-properties ()
  "Data rows carry dashboard-panel and dashboard-row-id text properties."
  (let ((panel (list :name "mypanel"
                     :title "My"
                     :columns [("X" 5 nil)]
                     :entries (lambda () '(("id1" . ["val"]))))))
    (with-temp-buffer
      (claude-dashboard--render-section panel 80)
      ;; Find the data row
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (get-text-property (point) 'dashboard-row-id)))
        (forward-line 1))
      (should (equal (get-text-property (point) 'dashboard-panel) "mypanel"))
      (should (equal (get-text-property (point) 'dashboard-row-id) "id1")))))

(ert-deftest claude-dashboard-test-render-section-error-handling ()
  "A panel that errors shows an error message instead of crashing."
  (let ((panel (list :name "bad"
                     :title "Bad"
                     :columns [("X" 5 nil)]
                     :entries (lambda () (error "Boom")))))
    (with-temp-buffer
      (claude-dashboard--render-section panel 80)
      (should (string-match-p "Error:.*Boom" (buffer-string))))))

;;;; Key dispatch tests

(ert-deftest claude-dashboard-test-dispatch-calls-action ()
  "Dispatch routes to the correct panel action with panel and row-id args."
  (let* ((called-with nil)
         (panel (list :name "test"
                      :title "T"
                      :columns [("X" 5 nil)]
                      :entries (lambda () '(("r1" . ["v"])))
                      :actions `(("RET" . ,(lambda (p rid) (setq called-with (list p rid))))))))
    (with-temp-buffer
      (let ((claude-dashboard--panels (list panel)))
        ;; Insert a row with text properties
        (let ((start (point)))
          (insert "test row\n")
          (put-text-property start (point) 'dashboard-panel "test")
          (put-text-property start (point) 'dashboard-row-id "r1"))
        (goto-char (point-min))
        (claude-dashboard--dispatch-action "RET")
        (should called-with)
        (should (equal (plist-get (car called-with) :name) "test"))
        (should (equal (cadr called-with) "r1"))))))

(ert-deftest claude-dashboard-test-dispatch-no-panel ()
  "Dispatch on non-panel row produces message, no error."
  (with-temp-buffer
    (let ((claude-dashboard--panels nil))
      (insert "no properties\n")
      (goto-char (point-min))
      ;; Should not error
      (claude-dashboard--dispatch-action "RET"))))

(ert-deftest claude-dashboard-test-dispatch-no-action-for-key ()
  "Dispatch with key not in panel actions produces message."
  (let ((panel (list :name "test" :title "T" :columns [("X" 5 nil)]
                     :entries (lambda () nil)
                     :actions '(("RET" . ignore)))))
    (with-temp-buffer
      (let ((claude-dashboard--panels (list panel)))
        (let ((start (point)))
          (insert "row\n")
          (put-text-property start (point) 'dashboard-panel "test")
          (put-text-property start (point) 'dashboard-row-id "r1"))
        (goto-char (point-min))
        ;; "s" is not in actions — should message, not error
        (claude-dashboard--dispatch-action "s")))))

(ert-deftest claude-dashboard-test-find-panel ()
  "Finding a panel by name returns the correct plist."
  (let ((p1 (list :name "a" :title "A"))
        (p2 (list :name "b" :title "B")))
    (let ((claude-dashboard--panels (list p1 p2)))
      (should (equal (claude-dashboard--find-panel "b") p2))
      (should-not (claude-dashboard--find-panel "c")))))

;;;; Buffer lifecycle tests

(ert-deftest claude-dashboard-test-create-buffer ()
  "Creating a dashboard produces a buffer in claude-dashboard-mode."
  (let ((buf (claude-dashboard-create
              :name "test-create"
              :panels (list (list :name "p1" :title "P1"
                                 :columns [("X" 10 nil)]
                                 :entries (lambda () '(("r1" . ["hi"]))))))))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (eq major-mode 'claude-dashboard-mode))
            (should (equal claude-dashboard--name "test-create"))
            ;; Buffer contains panel content
            (should (string-match-p "P1" (buffer-string)))
            (should (string-match-p "hi" (buffer-string)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-dashboard-test-create-reentrant ()
  "Calling create twice with same name reuses the buffer."
  (let* ((panel (list :name "p" :title "P" :columns [("X" 5 nil)]
                      :entries (lambda () '(("r" . ["v"])))))
         (buf1 (claude-dashboard-create :name "test-reent" :panels (list panel)))
         (buf2 (claude-dashboard-create :name "test-reent" :panels (list panel))))
    (unwind-protect
        (should (eq buf1 buf2))
      (when (buffer-live-p buf1) (kill-buffer buf1)))))

(ert-deftest claude-dashboard-test-timer-cleanup-on-kill ()
  "Killing the buffer cancels all timers."
  (let* ((panel (list :name "p" :title "P" :columns [("X" 5 nil)]
                      :entries (lambda () nil)
                      :interval 5))
         (buf (claude-dashboard-create :name "test-timer" :panels (list panel))))
    (with-current-buffer buf
      (should claude-dashboard--timers)
      (let ((timers (copy-sequence claude-dashboard--timers)))
        (kill-buffer buf)
        ;; All timers should be cancelled
        (dolist (timer timers)
          (should-not (memq timer timer-list)))))))

(ert-deftest claude-dashboard-test-refresh-all ()
  "Refresh-all re-renders all panels."
  (let* ((call-count 0)
         (panel (list :name "p" :title "P" :columns [("X" 5 nil)]
                      :entries (lambda ()
                                 (setq call-count (1+ call-count))
                                 '(("r" . ["v"]))))))
    (let ((buf (claude-dashboard-create :name "test-refresh" :panels (list panel))))
      (unwind-protect
          (with-current-buffer buf
            (let ((before call-count))
              (claude-dashboard-refresh-all)
              (should (> call-count before))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-dashboard-test-cursor-restoration ()
  "After refresh, cursor stays on the same row-id."
  (let* ((panel (list :name "p" :title "P" :columns [("X" 10 nil)]
                      :entries (lambda () '(("r1" . ["a"]) ("r2" . ["b"]))))))
    (let ((buf (claude-dashboard-create :name "test-cursor" :panels (list panel))))
      (unwind-protect
          (with-current-buffer buf
            ;; Move to r2
            (goto-char (point-min))
            (while (and (not (eobp))
                        (not (equal (get-text-property (point) 'dashboard-row-id) "r2")))
              (forward-line 1))
            (should (equal (get-text-property (point) 'dashboard-row-id) "r2"))
            ;; Refresh and verify cursor stays on r2
            (claude-dashboard-refresh-all)
            (should (equal (get-text-property (point) 'dashboard-row-id) "r2")))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-dashboard-test-mode-line-updates ()
  "Mode-line shows actions for the panel under point."
  (let* ((panel (list :name "p" :title "P" :columns [("X" 5 nil)]
                      :entries (lambda () '(("r" . ["v"])))
                      :actions '(("RET" . ignore) ("s" . ignore)))))
    (let ((buf (claude-dashboard-create :name "test-ml" :panels (list panel))))
      (unwind-protect
          (with-current-buffer buf
            ;; Move to a data row
            (goto-char (point-min))
            (while (and (not (eobp))
                        (not (get-text-property (point) 'dashboard-row-id)))
              (forward-line 1))
            (claude-dashboard--update-mode-line)
            (should mode-line-format))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(provide 'claude-dashboard-tests)
;;; claude-dashboard-tests.el ends here
