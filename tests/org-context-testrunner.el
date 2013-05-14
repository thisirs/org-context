(require 'ert)

(defadvice ert--pp-with-indentation-and-newline (around fix-display activate)
  (let ((print-escape-newlines t)
        (print-level nil)
        (print-length nil))
    ad-do-it))

(let* ((current-directory (file-name-directory load-file-name))
       (test-path (expand-file-name "." current-directory))
       (root-path (expand-file-name ".." current-directory))
       (resources-path (expand-file-name "resources" current-directory)))

  ;; Activating org-context
  (load (expand-file-name "org-context.el" root-path) t t)

  ;; Loading the tests
  (load (expand-file-name "org-context-agenda-expansion.el" test-path) t t)
  (load (expand-file-name "org-context-capture-expansion.el" test-path) t t)
  (load (expand-file-name "org-context-agenda-test-exception.el" test-path) t t)

  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil)

  (ert-run-tests-batch-and-exit t))
