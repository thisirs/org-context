
(ert-deftest org-context-advice ()
  "Tests if `org-agenda' and 'org-capture' are advised."
  (should (ad-is-advised 'org-agenda))
  (should (ad-is-advised 'org-capture))
  (should-not (ad-is-active 'org-agenda))
  (should-not (ad-is-active 'org-capture))

  (org-context-activate 1)

  (should (ad-is-active 'org-agenda))
  (should (ad-is-active 'org-capture))

  (org-context-activate -1)
  (should-not (ad-is-active 'org-agenda))
  (should-not (ad-is-active 'org-capture)))

(ert-deftest org-context-agenda-check-local ()
  "Tests if `org-context-agenda' is local."
  (let ((directory (expand-file-name "ProjectA" resources-path))
        buffer)
    (org-context-activate 1)
    (setq buffer (find-file-noselect (expand-file-name "foo" directory)))
    (with-current-buffer buffer
      (should (local-variable-p 'org-context-agenda)))))


(ert-deftest org-context-agenda-check-blah ()
  "Tests if `org-context-agenda' is local."
  (let ((directory (expand-file-name "ProjectA" resources-path))
        buffer)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (foo) t)))
      (org-context-activate)
      (setq buffer (find-file-noselect (expand-file-name "foo" directory)))
      (with-current-buffer buffer
        (should (local-variable-p 'org-context-agenda))
        (should (equal org-context-agenda
                       '(("t" "Agenda and all TODO's"
                          todo
                          nil
                          ((org-agenda-files '("todo.org"))))))))

      (org-agenda nil "t")
      (org-agenda-redo)
      (should (equal (buffer-name) "*Agenda(ProjectA:t)*")))))
