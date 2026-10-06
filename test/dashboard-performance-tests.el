;;; dashboard-performance-tests.el --- Refresh regression tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'claude-dashboard)
(require 'metaflow-runs)

(ert-deftest dashboard-performance-hidden-timers-do-no-work ()
  "Hidden panels skip fetching and rendering but resume when displayed."
  (let ((calls 0)
        (panel '(:name "test")))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'claude-dashboard--refresh-panel)
                   (lambda (target actual-panel)
                     (should (eq target buf))
                     (should (eq actual-panel panel))
                     (should claude-dashboard--automatic-refresh)
                     (cl-incf calls))))
          (claude-dashboard--refresh-visible-panel buf panel)
          (should (= calls 0))
          (save-window-excursion
            (set-window-buffer (selected-window) buf)
            (claude-dashboard--refresh-visible-panel buf panel)
            (should (= calls 1)))
          (claude-dashboard--refresh-visible-panel buf panel)
          (should (= calls 1)))))))

(defmacro dashboard-performance--with-scan (&rest body)
  "Run BODY with isolated shared state, a fake clock and counted scans."
  (declare (indent 0))
  `(let ((metaflow-runs--local-cache nil)
         (metaflow-runs--local-cache-time nil)
         (metaflow-runs--local-cache-config nil)
         (metaflow-runs-monitor-directories '("/first"))
         (metaflow-runs-stale-seconds 120)
         (metaflow-runs-refresh-interval 30)
         (now 100.0)
         (scans 0))
     (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
               ((symbol-function 'metaflow-runs--scan-local-records)
                (lambda ()
                  (cl-incf scans)
                  (list (list (cons 'start_time 1)
                              (cons 'spark_state "RUNNING"))))))
       ,@body)))

(ert-deftest dashboard-performance-shares-scans-and-expires ()
  "Automatic refreshes share scans until expiry, including across buffers."
  (dashboard-performance--with-scan
    (with-temp-buffer (metaflow-runs--local-records t))
    (setq now 129.0)
    (with-temp-buffer (metaflow-runs--local-records t))
    (should (= scans 1))
    (setq now 130.0)
    (metaflow-runs--local-records t)
    (should (= scans 2))
    (setq metaflow-runs-monitor-directories '("/second"))
    (metaflow-runs--local-records t)
    (should (= scans 3))
    (setq metaflow-runs-stale-seconds 60)
    (metaflow-runs--local-records t)
    (should (= scans 4))))

(ert-deftest dashboard-performance-keeps-local-observations-isolated ()
  "Merging remote status never contaminates later local cache readers."
  (dashboard-performance--with-scan
    (let ((records (metaflow-runs--local-records t)))
      (setf (alist-get 'spark_state (car records)) "FAILED"))
    (should (equal (alist-get 'spark_state
                             (car (metaflow-runs--local-records t)))
                   "RUNNING"))
    (should (= scans 1))))

(ert-deftest dashboard-performance-manual-refresh-bypasses-cache ()
  "Manual refresh reads new observations; timer and callback refreshes reuse."
  (dashboard-performance--with-scan
    (let ((metaflow-runs--cache nil)
          (metaflow-runs-argo-context nil))
      (cl-letf (((symbol-function 'metaflow-runs--argo-fetch) #'ignore))
        (let ((claude-dashboard--automatic-refresh t))
          (metaflow-runs--refresh)
          (metaflow-runs--refresh))
        (should (= scans 1))
        (metaflow-runs--refresh t)
        (should (= scans 1))
        (metaflow-runs--refresh)
        (should (= scans 2))))))

(ert-deftest dashboard-performance-caches-empty-scans ()
  "Empty directories are cached and a backwards clock invalidates the cache."
  (dashboard-performance--with-scan
    (cl-letf (((symbol-function 'metaflow-runs--scan-local-records)
               (lambda () (cl-incf scans) nil)))
      (should-not (metaflow-runs--local-records t))
      (should-not (metaflow-runs--local-records t))
      (should (= scans 1))
      (setq now 99.0)
      (metaflow-runs--local-records t)
      (should (= scans 2)))))

(provide 'dashboard-performance-tests)
;;; dashboard-performance-tests.el ends here
