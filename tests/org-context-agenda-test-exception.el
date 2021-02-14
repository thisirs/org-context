(defvar resources-path (expand-file-name "tests/resources/ProjectA" default-directory))

(ert-deftest org-context-advice ()
  "Tests if `org-agenda' and `org-capture' are advised."

  (should-not (advice-member-p 'org-capture-advice 'org-capture))
  (should-not (advice-member-p 'org-agenda-advice 'org-agenda))

  (org-context-mode +1)
  (should (advice-member-p 'org-capture-advice 'org-capture))
  (should (advice-member-p 'org-agenda-advice 'org-agenda))

  (org-context-mode -1)
  (should-not (advice-member-p 'org-capture-advice 'org-capture))
  (should-not (advice-member-p 'org-agenda-advice 'org-agenda)))

(ert-deftest org-context-agenda-check-local ()
  "Tests if `org-context-agenda' is local."
  (let ((directory (expand-file-name "ProjectA" resources-path))
        buffer)
    (org-context-mode +1)
    (setq buffer (find-file-noselect (expand-file-name "foo" directory)))
    (with-current-buffer buffer
      (should (local-variable-p 'org-context-agenda)))))

(ert-deftest org-context-agenda-check-local-1 ()
  "Tests if `org-context-agenda' is local."
  (let ((directory (expand-file-name "ProjectA" resources-path))
        buffer)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (foo) t)))
      (org-context-mode +1)
      (setq buffer (find-file-noselect (expand-file-name "foo" directory)))
      (with-current-buffer buffer
        (should (local-variable-p 'org-context-agenda))
        (should (equal org-context-agenda
                       '(("t" "Agenda and all TODO's"
                          todo
                          nil
                          ((org-agenda-files '("todo.org")))))))))))
