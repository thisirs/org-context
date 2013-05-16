(require 'ert)

(ert-deftest org-context-capture-safe-1 ()
  (should
   (org-context-capture-safe-p
    '(question
      ("q" question)
      ("q" (question))
      ("q" (question "blah.org"))
      ("q" (question "blah.org" "Description"))
      "q"
      ("q" "t")
      ("q" ("t"))
      ("q" ("t" "blah.org"))
      ("q" ("t" "blah.org" "Description"))
      ("q" "Description" entry
       (file "/home/homer/ProjectA/todo.org")
       "* TODO %?\n  OPENED: %U")))))

(ert-deftest org-context-capture-safe-2 ()
  (should-not
   (org-context-capture-safe-p
    '(("q" "Description" entry
       (file get-my-file)
       "* TODO %?\n  OPENED: %U")))))

(ert-deftest org-context-capture-safe-3 ()
  (should-not
   (org-context-capture-safe-p
    '(("q" "Description" entry
       (file "/home/homer/ProjectA/todo.org")
       "* TODO %?\n  %(owned) OPENED: %U")))))

(ert-deftest org-context-capture-safe-4 ()
  (should-not
   (org-context-capture-safe-p
    '(("q" "Description" entry
       (file+headline owned "blah")
       "* TODO %?\n OPENED: %U")))))

(ert-deftest org-context-capture-safe-5 ()
  (should-not
   (org-context-capture-safe-p
    '(("q" "Description" entry
       (file+function "/home/homer/ProjectA/todo.org" owned)
       "* TODO %?\n OPENED: %U")))))

(ert-deftest org-context-capture-safe-6 ()
  (should-not
   (org-context-capture-safe-p
    '(("q" "Description" entry
       (function owned)
       "* TODO %?\n OPENED: %U")))))

(ert-deftest org-context-agenda-safe-1 ()
  (should
   (org-context-agenda-safe-p
    '(question
      ("q" question)
      ("q" (question))
      ("q" (question "blah.org"))
      ("q" (question ("blah.org" "todo.org")))
      ("q" (question ("blah.org" "todo.org") desc))
      "q"
      ("q" "t")
      ("q" ("t"))
      ("q" ("t" "blah.org"))
      ("q" ("t" ("blah.org" "todo.org")))
      ("d" "Description" alltodo nil
       (org-agenda-files
        '("/home/homer/ProjectA/todo.org")))
      ("d" "Description" alltodo ""
       (org-agenda-files
        '("/home/homer/ProjectA/todo.org")))))))

(ert-deftest org-context-agenda-safe-2 ()
  (should-not
   (org-context-agenda-safe-p
    '(("a" alltodo ignore)))))

(ert-deftest org-context-agenda-safe-3 ()
  (should-not
   (org-context-agenda-safe-p
    '(("a" "desc" alltodo ignore)))))

(ert-deftest org-context-agenda-safe-4 ()
  (should-not
   (org-context-agenda-safe-p
    '(("a" "desc" ignore "")))))

(ert-deftest org-context-agenda-safe-4 ()
  (should-not
   (org-context-agenda-safe-p
    '(("a" ignore "")))))

(ert-deftest org-context-agenda-safe-5 ()
  (should-not
   (org-context-agenda-safe-p
    '(("a" "desc" (lambda () owned) "")))))

(ert-deftest org-context-agenda-safe-6 ()
  (should-not
   (org-context-agenda-safe-p
    '(("a" (lambda () owned) "")))))

(ert-deftest org-context-agenda-safe-7 ()
  (should
   (org-context-agenda-safe-p
    '(("t" "TODO + tests" ((alltodo "" ((org-agenda-files '("todo.org"))
                                        (org-agenda-overriding-header "TODO")))
                           (alltodo "" ((org-agenda-overriding-header "TESTS")
                                        (org-agenda-files '("tests/todo.org")))))
       ((org-agenda-buffer-name "blahblah")))))))
