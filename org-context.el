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
;; is a list of templates as described in Org manual or of the special
;; form (KEY steal FILE ID) which allows you to steal the template
;; definition specified by letter KEY in `org-capture-templates'. The
;; targeted org file is replaced by FILE and ID is optional and
;; replaces the old id. If FILE is a non-absolute file name and REGEX
;; is a directory, FILE is expanded against REGEX.
;;
;; The merge between `org-capture-templates' and contextual ones is
;; controlled by the variable `org-context-merge-strategy'. You can
;; force all the contextual templates to be located in a submenu
;; accessible by letter `org-context-capture-key'. You can try a merge
;; if key sets are distinct (if not, contextual templates are placed in
;; a submenu as in the previous case). And finally you can override
;; templates.
;;
;; In the case the REGEX is a directory path and all the templates are
;; entries in a file in that directory one can use the helper function
;; `org-capture-context-simple'.


;; For example if you have,
;;
;; (setq org-context-capture-alist
;;       '(
;;         ("t" "todo" entry  (file "~/blah.org") "* my template")
;;         ))
;;
;; you can steal that definition in `org-context-capture-alist' by
;; writing,
;;
;; (setq org-context-capture-alist
;;       '(("/home/homer/projectA/" .
;;          (
;;           ("t" steal "todo.org" "My todo id in projectA")
;;           ("a" "Todo for projectA" entry
;;            (file "/home/homer/projects/todo.org")
;;            "* TODO %?\n  OPENED: %U")
;;           ))))
;;
;; When capturing from files in projectA directory your templates will
;; overridden or in a submenu.


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

(defvar org-context-merge-strategy 'override
  "Controls how the extra templates are merged into the already
  existing ones.
Possible values:
  override: context templates override old ones.
  submenu: context templates are placed in a submenu
  merge: context templates are placed in a submenu if keys
  conflicts, prepended otherwise.")

(defun org-context-steal-maybe (templates &optional directory)
  "Returns TEMPLATES with stolen templates if any."
  (mapcar
   (lambda (steal-temp)
     (if (eq (cadr steal-temp) 'steal)
         (let* ((template (copy-tree (assoc (car steal-temp) org-capture-templates)))
                (id (nth 3 steal-temp))
                (file (nth 2 steal-temp))
                (file (if (and (not (file-name-absolute-p file))
                               (stringp directory))
                          (expand-file-name file directory)
                        file))
                (target (nth 3 template)))
           (if template
               (if (null (string-match "\\`file" (symbol-name (car target))))
                   (error "Couldn't find any target to modify.")
                 (setcar (cdr target) file)
                 (if id (setcar (cdr template) id)))
             (error (format "No template with letter \"%s\" to steal from." (car steal-temp))))
           template)
       steal-temp))
   templates))

(defadvice org-capture (around org-context-capture activate)
  "Allow contextual capture templates.
This advice looks into `org-context-capture-alist' to see if the
visited file at the time the capture is made match. If so, it
adds to the existing templates the ones specified in the tail of
the `org-context-capture-alist' corresponding entry."
  (let* ((file-name (expand-file-name
                     (or buffer-file-name
                         (and (eq major-mode 'dired-mode) default-directory)
                         (buffer-name))))
         (org-context-capture-templates
          (assoc-default file-name org-context-capture-alist 'string-match))
         (directory (and org-context-capture-templates (match-string 0 file-name)))
         (directory (and directory (file-name-absolute-p directory) directory))
         (org-context-capture-templates
          (org-context-steal-maybe org-context-capture-templates directory))
         (org-capture-templates
          (if org-context-capture-templates
              ;; decide if in sub-menu or not
              (if (or (eq org-context-merge-strategy 'submenu)
                      (and (eq org-context-merge-strategy 'merge)
                           (null (intersection
                                  (mapcar #'car org-capture-templates)
                                  (mapcar #'car org-context-capture-templates)))))
                  (if (member org-context-capture-key
                              (mapcar #'car org-capture-templates))
                      (error "Context key \"%s\" conflicts with one of `org-capture-templates'."
                             org-context-capture-key)
                    (append (cons (list org-context-capture-key "Contextual templates")
                                  (mapcar
                                   (lambda (template)
                                     (cons (concat org-context-capture-key
                                                   (car template))
                                           (cdr template)))
                                   org-context-capture-templates))
                            org-capture-templates))
                (let (merged-templates)
                  (mapc
                   (lambda (old-temp)
                     (setq merged-templates
                           (cons (if (nth 2 old-temp)
                                     (or (assoc (car old-temp) org-context-capture-templates)
                                         old-temp)
                                   (if (assoc (car old-temp) org-context-capture-templates)
                                       (error "Trying to override a submenu key!")
                                     old-temp))
                                 merged-templates)))
                   org-capture-templates)
                  (mapc
                   (lambda (c-temp)
                     (if (not (assoc (car c-temp) org-capture-templates))
                         (setq merged-templates
                               (cons c-temp merged-templates))))
                   org-context-capture-templates)
                  (nreverse merged-templates)))
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
