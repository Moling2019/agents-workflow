;;; slack-monitor-tests.el --- Tests for slack-monitor -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'slack-monitor)

;;;; JSON cache reading tests

(ert-deftest slack-monitor-test-read-json-cache-valid ()
  "Reading a valid JSON cache file returns parsed data."
  (let ((tmpfile (make-temp-file "slack-cache" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "{\"last_poll\":\"2026-03-16T15:00:00\",\"messages\":[{\"id\":\"m1\",\"from\":\"alice\",\"from_display\":\"Alice\",\"channel_id\":\"D001\",\"channel_name\":\"DM\",\"thread_ts\":\"\",\"text_preview\":\"hello\",\"timestamp\":\"2026-03-16T15:00:00\",\"status\":\"needs_reply\",\"draft_reply\":\"hi\",\"url\":\"https://slack.com/m1\"}]}"))
          (let ((slack-monitor-cache-file tmpfile))
            (slack-monitor--read-json-cache)
            (should (vectorp slack-monitor--cache))
            (should (= (length slack-monitor--cache) 1))
            (should (equal (alist-get 'id (aref slack-monitor--cache 0)) "m1"))))
      (delete-file tmpfile))))

(ert-deftest slack-monitor-test-read-json-cache-missing-file ()
  "Reading a non-existent cache file sets cache to nil."
  (let ((slack-monitor-cache-file "/tmp/nonexistent-slack-cache.json"))
    (slack-monitor--read-json-cache)
    (should (null slack-monitor--cache))))

(ert-deftest slack-monitor-test-read-json-cache-empty-file ()
  "Reading an empty file sets cache to nil."
  (let ((tmpfile (make-temp-file "slack-cache" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile (insert ""))
          (let ((slack-monitor-cache-file tmpfile))
            (slack-monitor--read-json-cache)
            (should (null slack-monitor--cache))))
      (delete-file tmpfile))))

(ert-deftest slack-monitor-test-read-json-cache-malformed ()
  "Reading malformed JSON sets cache to nil."
  (let ((tmpfile (make-temp-file "slack-cache" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile (insert "not json {{{"))
          (let ((slack-monitor-cache-file tmpfile))
            (slack-monitor--read-json-cache)
            (should (null slack-monitor--cache))))
      (delete-file tmpfile))))

;;;; Entry formatting tests

(ert-deftest slack-monitor-test-panel-entries-nil-cache ()
  "Entries returns nil when cache is nil."
  (let ((slack-monitor--cache nil))
    (should (null (slack-monitor--panel-entries)))))

(ert-deftest slack-monitor-test-panel-entries-formats-rows ()
  "Entries formats cached messages into dashboard rows."
  (let* ((msg `((id . "m1") (from . "alice") (from_display . "Alice")
                (channel_id . "D001") (channel_name . "DM")
                (text_preview . "hello world") (timestamp . "2026-03-16T15:00:00")
                (status . "needs_reply") (url . "https://slack.com/m1")))
         (slack-monitor--cache (vector msg))
         (slack-monitor--expanded nil)
         (entries (slack-monitor--panel-entries)))
    (should (= (length entries) 1))
    (should (equal (car (car entries)) "m1"))
    ;; Row vector: [status-icon from channel preview time]
    (let ((row (cadr (car entries))))
      (should (vectorp row))
      (should (= (length row) 5))
      (should (equal (aref row 1) "alice"))
      (should (equal (aref row 2) "DM"))
      (should (equal (aref row 3) "hello world")))))

(ert-deftest slack-monitor-test-panel-entries-respects-limit ()
  "Entries limits to 5 when collapsed, 30 when expanded."
  (let* ((msgs (cl-loop for i from 1 to 10
                         collect `((id . ,(format "m%d" i))
                                   (from . "alice") (from_display . "Alice")
                                   (channel_id . "D001") (channel_name . "DM")
                                   (text_preview . ,(format "msg %d" i))
                                   (timestamp . "2026-03-16T15:00:00")
                                   (status . "needs_reply")
                                   (url . "https://slack.com"))))
         (slack-monitor--cache (vconcat msgs)))
    (let ((slack-monitor--expanded nil))
      (should (= (length (slack-monitor--panel-entries)) 5)))
    (let ((slack-monitor--expanded t))
      (should (= (length (slack-monitor--panel-entries)) 10)))))

;;;; Status icon tests

(ert-deftest slack-monitor-test-status-icon-needs-reply ()
  "needs_reply status maps to pending icon."
  (let ((icon (slack-monitor--status-icon "needs_reply")))
    (should (stringp icon))))

(ert-deftest slack-monitor-test-status-icon-replied ()
  "replied status maps to success icon."
  (let ((icon (slack-monitor--status-icon "replied")))
    (should (stringp icon))))

;;;; Time formatting tests

(ert-deftest slack-monitor-test-format-time ()
  "Formats ISO timestamp to short time string."
  (let ((result (slack-monitor--format-time "2026-03-16T15:30:00")))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest slack-monitor-test-format-time-nil ()
  "Nil timestamp returns dash."
  (should (equal (slack-monitor--format-time nil) "—")))

(ert-deftest slack-monitor-test-format-time-empty ()
  "Empty timestamp returns dash."
  (should (equal (slack-monitor--format-time "") "—")))

;;;; Toggle tests

(ert-deftest slack-monitor-test-toggle-expanded ()
  "Toggle switches between collapsed and expanded state."
  (let ((slack-monitor--expanded nil))
    (setq slack-monitor--expanded (not slack-monitor--expanded))
    (should (eq slack-monitor--expanded t))
    (setq slack-monitor--expanded (not slack-monitor--expanded))
    (should (eq slack-monitor--expanded nil))))

;;;; Ignore list tests

(ert-deftest slack-monitor-test-read-ignore-list-missing-file ()
  "Reading non-existent ignore file returns empty list."
  (let ((slack-monitor-ignore-file "/tmp/nonexistent-ignore.yaml"))
    (should (null (slack-monitor--read-ignore-list)))))

(ert-deftest slack-monitor-test-read-ignore-list-valid ()
  "Reading a valid ignore file returns list of thread entries."
  (let ((tmpfile (make-temp-file "slack-ignore" nil ".yaml")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ignored_threads:\n  - thread_ts: \"123.456\"\n    channel: C001\n    added: 2026-03-16\n"))
          (let ((slack-monitor-ignore-file tmpfile))
            (let ((entries (slack-monitor--read-ignore-list)))
              (should (= (length entries) 1))
              (should (equal (alist-get 'thread_ts (car entries)) "123.456")))))
      (delete-file tmpfile))))

(ert-deftest slack-monitor-test-read-ignore-list-empty ()
  "Reading an ignore file with empty list returns nil."
  (let ((tmpfile (make-temp-file "slack-ignore" nil ".yaml")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ignored_threads:\n"))
          (let ((slack-monitor-ignore-file tmpfile))
            (should (null (slack-monitor--read-ignore-list)))))
      (delete-file tmpfile))))

(ert-deftest slack-monitor-test-append-ignore-entry ()
  "Appending to ignore list writes a valid YAML entry."
  (let ((tmpfile (make-temp-file "slack-ignore" nil ".yaml")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ignored_threads:\n"))
          (let ((slack-monitor-ignore-file tmpfile))
            (slack-monitor--append-ignore-entry "123.456" "C001")
            (let ((content (with-temp-buffer
                             (insert-file-contents tmpfile)
                             (buffer-string))))
              (should (string-match-p "123\\.456" content))
              (should (string-match-p "C001" content)))))
      (delete-file tmpfile))))

(ert-deftest slack-monitor-test-append-ignore-creates-file ()
  "Appending to a non-existent ignore file creates it."
  (let ((tmpdir (make-temp-file "slack-monitor-test" t)))
    (unwind-protect
        (let ((slack-monitor-ignore-file (expand-file-name "ignore.yaml" tmpdir)))
          (slack-monitor--append-ignore-entry "789.012" "C002")
          (should (file-exists-p slack-monitor-ignore-file))
          (let ((content (with-temp-buffer
                           (insert-file-contents slack-monitor-ignore-file)
                           (buffer-string))))
            (should (string-match-p "789\\.012" content))))
      (delete-directory tmpdir t))))

;;;; Panel constructor tests

(ert-deftest slack-monitor-test-panel-constructor ()
  "Panel constructor returns a valid plist with required keys."
  (let ((panel (slack-monitor-panel)))
    (should (equal (plist-get panel :name) "slack"))
    (should (equal (plist-get panel :title) "Slack Messages"))
    (should (vectorp (plist-get panel :columns)))
    (should (functionp (plist-get panel :entries)))
    (should (functionp (plist-get panel :refresh)))
    (should (listp (plist-get panel :actions)))
    (let ((action-keys (mapcar #'car (plist-get panel :actions))))
      (should (member "RET" action-keys))
      (should (member "x" action-keys))
      (should (member "C-o" action-keys)))))

(ert-deftest slack-monitor-test-refresh-reads-cache ()
  "Refresh function reads the JSON cache file."
  (let ((tmpfile (make-temp-file "slack-cache" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "{\"last_poll\":\"2026-03-16\",\"messages\":[{\"id\":\"m1\",\"from\":\"bob\",\"from_display\":\"Bob\",\"channel_id\":\"D001\",\"channel_name\":\"DM\",\"thread_ts\":\"\",\"text_preview\":\"test\",\"timestamp\":\"2026-03-16T10:00:00\",\"status\":\"needs_reply\",\"draft_reply\":null,\"url\":\"\"}]}"))
          (let ((slack-monitor-cache-file tmpfile))
            (slack-monitor--refresh-cache)
            (should (not (null slack-monitor--cache)))
            (should (= (length slack-monitor--cache) 1))))
      (delete-file tmpfile))))

(provide 'slack-monitor-tests)
;;; slack-monitor-tests.el ends here
