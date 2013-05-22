(require 'ert)

;; Testing expansions
(defmacro org-context-capture-check-expansion (template expected)
  `(let ((directory "/home/homer/ProjectA")
         (org-context-capture-shortcut
          '((question
             "q" "Question" entry
             (file place-holder)
             "* QUESTION %?\n  OPENED: %U")))
         (org-capture-templates
          '(("t" "Todo" entry
             (file "/home/homer/Org/agenda.org")
             "* TODO %?\n  OPENED: %U"))))
     (cl-letf (((symbol-function 'file-exists-p) (lambda (foo) t)))
       (should (equal (car (org-context-capture--expand
                            (list ',template) directory))
                      ',expected)))))

(ert-deftest org-context-capture-check-symbolsteal ()
    (org-context-capture-check-expansion
     question
     ("q" "Question" entry (file "/home/homer/ProjectA/todo.org")
      "* QUESTION %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-symbolsteal+key ()
    (org-context-capture-check-expansion
     ("t" question)
     ("t" "Question" entry (file "/home/homer/ProjectA/todo.org")
      "* QUESTION %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-symbolsteal+key-1 ()
    (org-context-capture-check-expansion
     ("t" (question))
     ("t" "Question" entry (file "/home/homer/ProjectA/todo.org")
      "* QUESTION %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-symbolsteal+key+file ()
    (org-context-capture-check-expansion
     ("t" (question "blah.org"))
     ("t" "Question" entry (file "/home/homer/ProjectA/blah.org")
      "* QUESTION %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-symbolsteal+key+file+desc ()
    (org-context-capture-check-expansion
     ("t" (question "blah.org" "Description"))
     ("t" "Description" entry (file "/home/homer/ProjectA/blah.org")
      "* QUESTION %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-sub-menu ()
    (org-context-capture-check-expansion
     ("q" "My sub-menu")
     ("q" "My sub-menu")))

(ert-deftest org-context-capture-check-keysteal ()
    (org-context-capture-check-expansion
     "t"
     ("t" "Todo" entry
      (file "/home/homer/ProjectA/todo.org")
      "* TODO %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-keysteal+key ()
    (org-context-capture-check-expansion
     ("q" "t")
     ("q" "Todo" entry
      (file "/home/homer/ProjectA/todo.org")
      "* TODO %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-keysteal+key-1 ()
    (org-context-capture-check-expansion
     ("q" ("t"))
     ("q" "Todo" entry
      (file "/home/homer/ProjectA/todo.org")
      "* TODO %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-keysteal+key+file ()
    (org-context-capture-check-expansion
     ("q" ("t" "blah.org"))
     ("q" "Todo" entry
      (file "/home/homer/ProjectA/blah.org")
      "* TODO %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-keysteal+key+file+desc ()
    (org-context-capture-check-expansion
     ("q" ("t" "blah.org" "Description"))
     ("q" "Description" entry
      (file "/home/homer/ProjectA/blah.org")
      "* TODO %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-regular-1 ()
  "Test relative target expansion in regular template"
    (org-context-capture-check-expansion
     ("t" "Todo" entry
      (file "todo.org")
      "* TODO %?\n  OPENED: %U")
     ("t" "Todo" entry
      (file "/home/homer/ProjectA/todo.org")
      "* TODO %?\n  OPENED: %U")))

(ert-deftest org-context-capture-check-regular-2 ()
  "Test target expansion in regular template"
  (org-context-capture-check-expansion
   ("t" "Todo" entry
    (file "/home/homer/ProjectA/todo.org")
    "* TODO %?\n  OPENED: %U")
   ("t" "Todo" entry
    (file "/home/homer/ProjectA/todo.org")
    "* TODO %?\n  OPENED: %U")))
