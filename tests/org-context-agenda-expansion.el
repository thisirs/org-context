(require 'ert)

;; Testing expansions
(defmacro org-context-agenda-check-expansion (command expected)
  `(let ((directory "/home/homer/ProjectA")
         (org-context-agenda-shortcut
          '((todo
             "t" todo "WAITING")))
         (org-agenda-custom-commands
          '(("t" "Agenda and all TODO's"
             ((agenda "")
              (alltodo ""))))))
     (cl-letf (((symbol-function 'file-exists-p) (lambda (foo) t)))
       (should (equal (car (org-context-agenda--expand
                            (list ',command) directory))
                      ',expected)))))

(ert-deftest org-context-agenda-check-symbolsteal ()
  (org-context-agenda-check-expansion
   todo
   ("t" "" todo "WAITING" ((org-agenda-buffer-name "*Agenda(ProjectA:t)*")
                        (org-agenda-files '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-symbolsteal+key ()
  (org-context-agenda-check-expansion
   ("q" todo)
   ("q" "" todo "WAITING" ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
                        (org-agenda-files '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-symbolsteal+key-1 ()
  (org-context-agenda-check-expansion
   ("q" (todo))
   ("q" "" todo "WAITING" ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
                        (org-agenda-files '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-symbolsteal+key+file ()
  (org-context-agenda-check-expansion
   ("q" (todo "blah.org"))
   ("q" "" todo "WAITING" ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
                        (org-agenda-files '("/home/homer/ProjectA/blah.org"))))))

(ert-deftest org-context-agenda-check-symbolsteal+key+file+desc ()
  (org-context-agenda-check-expansion
   ("q" (todo "blah.org" "Desc"))
   ("q" "Desc" todo "WAITING" ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
                           (org-agenda-files '("/home/homer/ProjectA/blah.org"))))))

(ert-deftest org-context-agenda-check-symbolsteal+key+files ()
  (org-context-agenda-check-expansion
   ("q" (todo ("blah.org" "todo.org")))
   ("q" "" todo "WAITING" ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
                        (org-agenda-files '("/home/homer/ProjectA/blah.org"
                                            "/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-symbolsteal+key+files+desc ()
  (org-context-agenda-check-expansion
   ("q" (todo ("blah.org" "todo.org") "Desc"))
   ("q" "Desc" todo "WAITING" ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
                               (org-agenda-files '("/home/homer/ProjectA/blah.org"
                                                   "/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-sub-menu ()
  (org-context-agenda-check-expansion
   ("q" . "My sub-menu")
   ("q" . "My sub-menu")))

(ert-deftest org-context-agenda-check-keysteal ()
  (org-context-agenda-check-expansion
   "t"
   ("t" "Agenda and all TODO's"
    ((agenda "")
     (alltodo ""))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:t)*")
     (org-agenda-files '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-keysteal+key ()
  (org-context-agenda-check-expansion
   ("q" "t")
   ("q" "Agenda and all TODO's"
    ((agenda "")
     (alltodo ""))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
     (org-agenda-files '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-keysteal+key-1 ()
  (org-context-agenda-check-expansion
   ("q" ("t"))
   ("q" "Agenda and all TODO's"
    ((agenda "")
     (alltodo ""))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
     (org-agenda-files '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-keysteal+key+file ()
  (org-context-agenda-check-expansion
   ("q" ("t" "blah.org"))
   ("q" "Agenda and all TODO's"
    ((agenda "")
     (alltodo ""))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
     (org-agenda-files '("/home/homer/ProjectA/blah.org"))))))

(ert-deftest org-context-agenda-check-keysteal+key+files ()
  (org-context-agenda-check-expansion
   ("q" ("t" ("blah.org" "todo.org")))
   ("q" "Agenda and all TODO's"
    ((agenda "")
     (alltodo ""))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:q)*")
     (org-agenda-files '("/home/homer/ProjectA/blah.org"
                         "/home/homer/ProjectA/todo.org"))))))


(ert-deftest org-context-agenda-check-regular-1 ()
  (org-context-agenda-check-expansion
   ("d" todo "WAITING"
    ((org-agenda-files
      '("/home/homer/ProjectA/todo.org"))))
   ("d" "" todo "WAITING"
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-regular-2 ()
  (org-context-agenda-check-expansion
   ("d" todo "WAITING"
    ((org-agenda-files
      '("todo.org"))))
   ("d" "" todo "WAITING"
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-regular-3 ()
  (org-context-agenda-check-expansion
   ("d" "Desc" todo "WAITING"
    ((org-agenda-files
      '("todo.org"))))
   ("d" "Desc" todo "WAITING"
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-regular-4 ()
  (org-context-agenda-check-expansion
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home")
     (tags "garden"))
    nil
    ((org-agenda-files
      '("/home/homer/ProjectB/todo.org"))))
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home")
     (tags "garden"))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectB/todo.org"))))))

(ert-deftest org-context-agenda-check-regular-5 ()
  (org-context-agenda-check-expansion
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home")
     (tags "garden"))
    nil
    ((org-agenda-files
      '("todo.org"))))
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home")
     (tags "garden"))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-regular-6 ()
  (org-context-agenda-check-expansion
   ("d" todo "WAITING"
    ((org-agenda-files
      '("todo.org"))))
   ("d" "" todo "WAITING"
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"))))))

(ert-deftest org-context-agenda-check-regular-7 ()
  (org-context-agenda-check-expansion
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home")
     (tags "garden"))
    nil
    ((org-agenda-files
      '("todo.org" "blah.org"))))
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home")
     (tags "garden"))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"
        "/home/homer/ProjectA/blah.org"))))))

(ert-deftest org-context-agenda-check-regular-8 ()
  (org-context-agenda-check-expansion
   ("d" todo "WAITING"
    ((org-agenda-files
      '("todo.org" "blah.org"))))
   ("d" "" todo "WAITING"
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"
        "/home/homer/ProjectA/blah.org"))))))

(ert-deftest org-context-agenda-check-regular-9 ()
  (org-context-agenda-check-expansion
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home")
     (tags "garden"))
    nil
    ((org-agenda-files
      '("todo.org" "blah.org"))))
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home")
     (tags "garden"))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"
        "/home/homer/ProjectA/blah.org"))))))

(ert-deftest org-context-agenda-check-regular-10 ()
  (org-context-agenda-check-expansion
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home"
                ((org-agenda-files
                  '("todo1.org" "blah1.org"))))
     (tags "garden"))
    nil
    ((org-agenda-files
      '("todo.org" "blah.org"))))
   ("d" "Block agenda"
    ((agenda "")
     (tags-todo "home"
                ((org-agenda-files
                  '("/home/homer/ProjectA/todo1.org"
                    "/home/homer/ProjectA/blah1.org"))))
     (tags "garden"))
    nil
    ((org-agenda-buffer-name "*Agenda(ProjectA:d)*")
     (org-agenda-files
      '("/home/homer/ProjectA/todo.org"
        "/home/homer/ProjectA/blah.org"))))))
