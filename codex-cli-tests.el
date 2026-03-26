;;; codex-cli-tests.el --- Tests for codex-cli -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'codex-cli)

(ert-deftest codex-cli-test-buffer-name ()
  "Buffer name follows *codex:DIR:INSTANCE* format."
  (let ((name (codex-cli--buffer-name "/tmp/project" "worker")))
    (should (string-prefix-p "*codex:" name))
    (should (string-match-p "worker" name))
    (should (string-suffix-p "*" name))))

(ert-deftest codex-cli-test-buffer-name-no-instance ()
  "Buffer name without instance uses empty string."
  (let ((name (codex-cli--buffer-name "/tmp/project")))
    (should (string-prefix-p "*codex:" name))
    (should (string-suffix-p "*" name))))

(ert-deftest codex-cli-test-buffer-p ()
  "codex-cli--buffer-p identifies codex buffers by name."
  (with-temp-buffer
    (rename-buffer "*codex:/tmp/:test*" t)
    (should (codex-cli--buffer-p (current-buffer))))
  (with-temp-buffer
    (rename-buffer "*claude:/tmp/:test*" t)
    (should-not (codex-cli--buffer-p (current-buffer)))))

(ert-deftest codex-cli-test-extract-instance-name ()
  "Extract instance from buffer name."
  (should (equal (codex-cli--extract-instance-name "*codex:/tmp/proj/:worker*")
                 "worker"))
  (should (null (codex-cli--extract-instance-name "*claude:/tmp/:x*"))))

(ert-deftest codex-cli-test-extract-directory ()
  "Extract directory from buffer name."
  (should (equal (codex-cli--extract-directory "*codex:/tmp/proj/:worker*")
                 "/tmp/proj/"))
  (should (null (codex-cli--extract-directory "*claude:/tmp/:x*"))))

(ert-deftest codex-cli-test-find-buffers ()
  "Find codex buffers among buffer list."
  (let ((buf1 (generate-new-buffer "*codex:/tmp/:a*"))
        (buf2 (generate-new-buffer "*codex:/tmp/:b*"))
        (buf3 (generate-new-buffer "*claude:/tmp/:c*")))
    (unwind-protect
        (let ((found (codex-cli--find-buffers)))
          (should (memq buf1 found))
          (should (memq buf2 found))
          (should-not (memq buf3 found)))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(ert-deftest codex-cli-test-find-buffers-for-directory ()
  "Find codex buffers filtered by directory."
  ;; Use abbreviate+truename so buffer names match what the finder expects
  ;; (on macOS /tmp -> /private/tmp).
  (let* ((dir (abbreviate-file-name (file-truename "/tmp/")))
         (buf1 (generate-new-buffer (format "*codex:%sproj/:a*" dir)))
         (buf2 (generate-new-buffer "*codex:/nonexistent-other/:b*")))
    (unwind-protect
        (let ((found (codex-cli--find-buffers-for-directory "/tmp/proj/")))
          (should (memq buf1 found))
          (should-not (memq buf2 found)))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest codex-cli-test-defcustom-defaults ()
  "Default program and switches are sensible."
  (should (equal codex-cli-program "codex"))
  (should (equal codex-cli-program-switches '("--dangerously-bypass-approvals-and-sandbox")))
  (should (= codex-cli-startup-delay 0.5)))

(ert-deftest codex-cli-test-idle-delay-default ()
  "Default idle delay is 5 seconds."
  (should (= codex-cli-idle-delay 5)))

(ert-deftest codex-cli-test-idle-timer-fires ()
  "Idle timer transitions status from working to idle."
  (with-temp-buffer
    (setq codex-cli--status 'working)
    (codex-cli--idle-timer-fired (current-buffer))
    (should (eq codex-cli--status 'idle))))

(ert-deftest codex-cli-test-cancel-idle-timer ()
  "Cancelling a nil timer is safe."
  (with-temp-buffer
    (setq codex-cli--idle-timer nil)
    (codex-cli--cancel-idle-timer)
    (should (null codex-cli--idle-timer))))

(provide 'codex-cli-tests)
;;; codex-cli-tests.el ends here
