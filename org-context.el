;;; org-context.el --- Context agenda and capture for Org-mode

;; Copyright (C) 2012 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org provides a way to take fast notes. This package advices
;; `org-capture' to allow contextual capture. The variable
;; `org-capture-context-alist' controls which templates are added to
;; the existing ones depending on the file-name the buffer is visiting
;; or the buffer name at the time `org-capture' is called.
;; `org-capture-context-alist' is an alist of elements of the form
;; (REGEX . TEMPLATE-LIST) where REGEX is a regular expression
;; matching the buffer file-name or the buffer name and TEMPLATE-LIST
;; is a list of templates as described in Org manual. In the case the
;; REGEX is a directory path and all the templates are entries in a
;; file in that directory one can use the helper function
;; `org-capture-context-simple'.


;; For example if you set,
;;
;; (setq org-context-capture-alist
;;       '(("/home/homer/project/" .
;;          (("a" "Todo for project" 'entry
;;            ('file "/home/homer/project/todo.org"
;;                   "* TODO %?\n  OPENED: %U"))
;;           ("b" "Todo for project" 'entry
;;             ('file "/home/homer/project/blah.org"
;;                    "* TODO %?\n  OPENED: %U"))))))
;;
;; you have two extra capture templates when capturing from a file
;; matching "/home/homer/project/".
;;
;; This is equivalent to,
;;
;; (setq org-context-capture-alist
;;       (org-context-capture-simple
;;        "/home/homer/project/"
;;        '(("a" "Todo for project"
;;           "* TODO %?\n  OPENED: %U")
;;          ("b" "Todo for project"
;;           "* TODO %?\n  OPENED: %U" "blah.org"))))


;; In the same way it advices `org-agenda' to allow contextual agenda.

(require 'org)

(defvar org-context-capture-alist
  nil
  "Alist of filename patterns vs corresponding capture
template list.")

(defvar org-context-capture-key
  "c"
  "The letter used in the org dispatcher for context command.")

(defvar org-context-agenda-alist
  nil
  "Alist of filename patterns vs corresponding custom agenda
  list.")

(defvar org-context-agenda-key
  "c"
  "org-agenda context key")

(defvar org-context-template-shortcut-alist
  '((question "Question" "* QUESTION %?\n  OPENED: %U")
    (todo "Todo" "* TODO %?\n  OPENED: %U"))
  "Alist of symbols vs their corresponding template name and body.
This is used in `org-context-capture-alist' to shorten the
  template definition.")

(defvar org-context-default-template-list
  '(("q" question)
    ("t" todo))
  "List of default templates to use in a project.")


(defadvice org-capture (around org-context-capture activate)
  "Allow contextual capture templates.
This advice looks into `org-context-capture-alist' to see if the
visited file at the time the capture is made match. If so, it
adds to the existing templates the ones specified in the tail of
the `org-context-capture-alist' corresponding entry."
  (let* ((org-context-capture-templates
          (assoc-default (expand-file-name
                          (or buffer-file-name
                              (and (eq major-mode 'dired-mode) default-directory)
                              (buffer-name)))
                         org-context-capture-alist
                         'string-match))
         (org-capture-templates
          (append
           (and org-context-capture-templates
                ;; > 1 or = 1 and conflicting
                (if (or (cdr org-context-capture-templates)
                        (member (caar org-context-capture-templates)
                                (mapcar #'car org-capture-templates)))
                    (if (member org-context-capture-key
                                (mapcar #'car org-capture-templates))
                        (error "Context key \"%s\" conflicts with one of `org-capture-templates'."
                               org-context-capture-key)
                      (cons (list org-context-capture-key "Contextual templates")
                            (mapcar
                             (lambda (template)
                               (cons (concat org-context-capture-key
                                             (car template))
                                     (cdr template)))
                             org-context-capture-templates)))
                  org-context-capture-templates))
           org-capture-templates)))
    ad-do-it))

(defadvice org-agenda (around org-context-agenda activate)
  "Allow contextual agenda view.
This advice looks into `org-context-agenda-alist' to see if the
visited file at the time `org-agenda' is invoked match. If so, it
adds to the existing views the ones specified in the tail of
the `org-context-agenda-alist' corresponding entry."
  (let* ((org-context-agenda-custom-commands
          (assoc-default (expand-file-name
                          (or buffer-file-name
                              (and (eq major-mode 'dired-mode) default-directory)
                              (buffer-name)))
                         org-context-agenda-alist
                         'string-match))
         (org-agenda-custom-commands
          (append
           (and org-context-agenda-custom-commands
                ;; > 1 or = 1 and conflicting
                (if (or (cdr org-context-agenda-custom-commands)
                        (member (caar org-context-agenda-custom-commands)
                                (mapcar #'car org-agenda-custom-commands)))
                    (if (member org-context-agenda-key
                                (mapcar #'car org-agenda-custom-commands))
                        (error "Context key \"%s\" conflicts with one of `org-agenda-custom-commands'."
                               org-context-agenda-key)
                      (cons (list org-context-agenda-key "Contextual agendas")
                            (mapcar
                             (lambda (template)
                               (cons (concat org-context-agenda-key
                                             (car template))
                                     (cdr template)))
                             org-context-agenda-custom-commands)))
                  org-context-agenda-custom-commands))
           org-agenda-custom-commands)))
    ad-do-it))


;; Helper functions

(defun org-context-capture-simple (path &optional temp-list todo-file)
  "Helper function that returns a form to be added to
`org-context-capture-alist'.

This function assumes that the files you want to capture from are
contained in the directory PATH. If provided, TODO-FILE is the
file that will contain the captures. If TODO-FILE is not
absolute, it is expanded against PATH. If TODO-FILE is nil,
TODO-FILE is set to \"todo.org\". TEMP-LIST is a list of
contextual templates as described in `org-capture-templates'. If
nil, TEMP-LIST is set to `org-context-default-template-list'. The
TODO-FILE can be specified in a per template basis in TEMP-LIST
overwriting the default behaviour."
  (or temp-list (setq temp-list org-context-default-template-list))
  (cons path
        (mapcar
         (lambda (tmp)
           (let* ((key (nth 0 tmp))
                  (id (nth 1 tmp))
                  body todo-file)
             (if (null (symbolp id))
                 (setq body (nth 2 tmp)
                       todo-file (nth 3 tmp))
               (setq todo-file (or todo-file (nth 2 tmp))
                     id (or (cdr (assoc id org-context-template-shortcut-alist))
                            (error "No template %s!" id))
                     body (cadr id)
                     id (car id)))

             (unless (and todo-file
                          (file-name-absolute-p todo-file))
               (setq todo-file
                     (concat (file-name-as-directory path)
                             (or todo-file "todo.org"))))
             (list key id 'entry (list 'file todo-file) body)))
         temp-list)))


(defun org-context-agenda-simple (path &optional todo-file)
  "Helper function that returns a form to be added to
`org-context-agenda-alist'.

"
  (or (stringp todo-file)
      (setq todo-file "todo.org"))
  (unless (file-name-absolute-p todo-file)
    (setq todo-file (concat (file-name-as-directory path) todo-file)))
  `(,path (,org-context-agenda-key
           "Todo" alltodo ""
           ((org-agenda-files '(,todo-file))
            (default-directory ,(file-name-directory todo-file))
            (org-agenda-buffer-name
             ,(format "*Agenda(%s)*"
                      (file-name-nondirectory
                       (directory-file-name
                        (file-name-directory todo-file)))))))))

(defun org-context-add-project (path &optional todo-file temp-list)
  "Helper functions that combines `org-context-agenda-simple' and
`org-context-capture-simple'."
  (add-to-list 'org-context-capture-alist
               (org-context-capture-simple path temp-list todo-file))
  (add-to-list 'org-context-agenda-alist
               (org-context-agenda-simple path todo-file)))

(provide 'org-context)

;;; org-context.el ends here
