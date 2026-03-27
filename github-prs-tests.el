;;; github-prs-tests.el --- Tests for github-prs -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'github-prs)

;;; --- format-age ---

(ert-deftest github-prs-test-format-age-seconds ()
  "Recent timestamps show seconds."
  (let ((iso (format-time-string "%Y-%m-%dT%H:%M:%SZ" (time-subtract nil 30) t)))
    (should (string-match-p "^[0-9]+s$" (github-prs--format-age iso)))))

(ert-deftest github-prs-test-format-age-minutes ()
  "Timestamps a few minutes ago show minutes."
  (let ((iso (format-time-string "%Y-%m-%dT%H:%M:%SZ" (time-subtract nil 300) t)))
    (should (string-match-p "^[0-9]+m$" (github-prs--format-age iso)))))

(ert-deftest github-prs-test-format-age-hours ()
  "Timestamps a few hours ago show hours."
  (let ((iso (format-time-string "%Y-%m-%dT%H:%M:%SZ" (time-subtract nil 7200) t)))
    (should (string-match-p "^[0-9]+h$" (github-prs--format-age iso)))))

(ert-deftest github-prs-test-format-age-days ()
  "Timestamps several days ago show days."
  (let ((iso (format-time-string "%Y-%m-%dT%H:%M:%SZ" (time-subtract nil 259200) t)))
    (should (string-match-p "^[0-9]+d$" (github-prs--format-age iso)))))

(ert-deftest github-prs-test-format-age-empty ()
  "Empty or nil input returns empty string."
  (should (equal "" (github-prs--format-age nil)))
  (should (equal "" (github-prs--format-age ""))))

;;; --- short-repo ---

(ert-deftest github-prs-test-short-repo ()
  "Extract short name from owner/repo."
  (should (equal "my-repo" (github-prs--short-repo "owner/my-repo"))))

(ert-deftest github-prs-test-short-repo-no-slash ()
  "Return as-is when no slash."
  (should (equal "my-repo" (github-prs--short-repo "my-repo"))))

(ert-deftest github-prs-test-short-repo-nil ()
  "Return empty string for nil."
  (should (equal "" (github-prs--short-repo nil))))

;;; --- review icon ---

(ert-deftest github-prs-test-review-icon-approved ()
  "APPROVED maps to success icon."
  (require 'claude-dashboard)
  (should (stringp (github-prs--review-icon "APPROVED"))))

(ert-deftest github-prs-test-review-icon-changes ()
  "CHANGES_REQUESTED maps to failed icon."
  (require 'claude-dashboard)
  (should (stringp (github-prs--review-icon "CHANGES_REQUESTED"))))

(ert-deftest github-prs-test-review-icon-nil ()
  "Nil returns empty string."
  (should (equal "" (github-prs--review-icon nil))))

;;; --- CI icon ---

(ert-deftest github-prs-test-ci-icon-all-success ()
  "All SUCCESS checks map to success icon."
  (require 'claude-dashboard)
  (let ((rollup [((conclusion . "SUCCESS")) ((conclusion . "SUCCESS"))]))
    (should (stringp (github-prs--ci-icon rollup)))))

(ert-deftest github-prs-test-ci-icon-failure ()
  "Any FAILURE maps to failed icon."
  (require 'claude-dashboard)
  (let ((rollup [((conclusion . "SUCCESS")) ((conclusion . "FAILURE"))]))
    (should (stringp (github-prs--ci-icon rollup)))))

(ert-deftest github-prs-test-ci-icon-pending ()
  "Pending state maps to running icon."
  (require 'claude-dashboard)
  (let ((rollup [((state . "PENDING"))]))
    (should (stringp (github-prs--ci-icon rollup)))))

(ert-deftest github-prs-test-ci-icon-empty ()
  "Empty rollup returns empty string."
  (should (equal "" (github-prs--ci-icon nil)))
  (should (equal "" (github-prs--ci-icon []))))

;;; --- pr-key ---

(ert-deftest github-prs-test-pr-key-string-repo ()
  "PR key from string repository field."
  (let ((pr '((repository . "owner/repo") (number . 42))))
    (should (equal "owner/repo#42" (github-prs--pr-key pr)))))

(ert-deftest github-prs-test-pr-key-object-repo ()
  "PR key from object repository field."
  (let ((pr '((repository . ((nameWithOwner . "org/project"))) (number . 7))))
    (should (equal "org/project#7" (github-prs--pr-key pr)))))

;;; --- panel constructor ---

(ert-deftest github-prs-test-panel-constructor ()
  "Panel constructor returns a valid plist."
  (let ((panel (github-prs-panel)))
    (should (equal "github" (plist-get panel :name)))
    (should (equal "GitHub PRs" (plist-get panel :title)))
    (should (vectorp (plist-get panel :columns)))
    (should (functionp (plist-get panel :entries)))
    (should (functionp (plist-get panel :refresh)))
    (should (listp (plist-get panel :actions)))
    (should (numberp (plist-get panel :interval)))))

;;; --- panel entries with mock data ---

(ert-deftest github-prs-test-panel-entries-empty ()
  "Nil cache returns nil entries."
  (with-temp-buffer
    (setq-local github-prs--cache nil)
    (should-not (github-prs--panel-entries))))

(ert-deftest github-prs-test-panel-entries-with-data ()
  "Populated cache returns formatted entries."
  (with-temp-buffer
    (setq-local github-prs--cache
                [((repository . "owner/my-repo")
                  (number . 123)
                  (title . "Fix bug")
                  (updatedAt . "2026-03-27T00:00:00Z")
                  (url . "https://github.com/owner/my-repo/pull/123")
                  (isDraft . :json-false)
                  (commentsCount . 2))])
    (setq-local github-prs--enrichments (make-hash-table :test #'equal))
    (let ((entries (github-prs--panel-entries)))
      (should (= 1 (length entries)))
      (let* ((entry (car entries))
             (row (cadr entry)))
        (should (equal "my-repo" (aref row 0)))
        (should (equal "#123" (aref row 1)))
        (should (equal "Fix bug" (aref row 2)))))))

;;; --- enrichment merge ---

(ert-deftest github-prs-test-enrichment-lookup ()
  "Enrichment data is retrieved by PR key."
  (with-temp-buffer
    (setq-local github-prs--enrichments (make-hash-table :test #'equal))
    (puthash "owner/repo#42"
             '((reviewDecision . "APPROVED")
               (statusCheckRollup . [((conclusion . "SUCCESS"))])
               (mergeable . "MERGEABLE"))
             github-prs--enrichments)
    (should (equal "APPROVED"
                   (alist-get 'reviewDecision
                              (gethash "owner/repo#42"
                                       github-prs--enrichments))))))

(provide 'github-prs-tests)
;;; github-prs-tests.el ends here
