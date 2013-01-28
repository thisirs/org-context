;;; org-context.el --- Contextual agenda and capture for Org-mode

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
;; `org-capture' and `org-agenda' to allow contextual capture and
;; agenda. The templates can be customized in two ways. One way is to
;; set the variable `org-context-capture-alist' that controls which
;; templates are added to the existing ones depending on the file the
;; buffer is visiting, the buffer name, or the major mode at the time
;; `org-capture' is called. Another way is to set the buffer-local
;; variable `org-context-capture'. There are analog variables for
;; custom agenda commands: `org-context-agenda-alist' and
;; `org-context-agenda'.

;; `org-context-capture-alist' is an alist of elements of the form
;; (CONDITION . TEMPLATE-LIST) where CONDITION is either a major mode
;; or a regular expression matching the buffer file-name, the buffer
;; name. TEMPLATE-LIST is a list of templates as described in Org
;; manual that will be added to the existing ones. To shorten the
;; definition of templates, you can also write them as (KEY (ID)) or
;; (KEY (ID FILE)) or (KEY (ID FILE DESC)). The ID is either a letter or a
;; symbol. A symbol indicates to look up template in
;; `org-context-capture-shortcut'. A letter indicates to reuse the
;; definition of corresponding template in `org-capture-templates'. If
;; FILE is specified, the target file in that template is replaced by
;; FILE and the description by DESC. If FILE is not specified or is
;; not an absolute path, FILE is expanded against the string matched
;; by CONDITION if it is a regex.

;; In the same way, `org-context-agenda-alist' is an alist of elements
;; of the form (CONDITION . TEMPLATE-LIST) where CONDITION is either a
;; major mode or a regular expression matching the buffer file-name,
;; the buffer name and TEMPLATE-LIST is a list of agenda custom
;; commands as described in Org manual. Again, to shorten the
;; definition of custom commands, you can also write them as (KEY ID)
;; or (KEY (ID FILE-LIST)) or (KEY (ID FILE-LIST DESC)). If ID is a
;; symbol, it is used to look up custom commands in
;; `org-context-agenda-shortcut'. If ID is a letter, it is used to
;; look up custom commands in `org-agenda-custom-commands'. If
;; specified, FILE-LIST is the list of files used to construct the
;; agenda. All file's name in FILE-LIST are expanded against the
;; string matched by CONDITION if it is a regex.


;; For example, say we have a project named "ProjectA" located in
;; "/home/homer/ProjectA" and several todo files in that project
;; "/home/homer/ProjectA/tests/tests.org"
;; "/home/homer/ProjectA/todo.org". We first set contextual captures:

;; (add-to-list 'org-context-capture-alist
;;              '("/home/homer/ProjectA"
;;                ;; definition relying on the `todo' one stored in
;;                ;; `org-context-capture-shortcut'.
;;                ("t" (todo "tests/tests.org" "Capture a test idea"))

;;                ;; definition relying on the "b" one but targeting
;;                ;; foo.org and with description "other todo"
;;                ("q" ("b" "foo.org" "other todo"))

;;                ;; full definition as in Org manual
;;                ("u" "Todo" entry
;;                 (file "/home/homer/ProjectA/todo.org")
;;                 "* TODO %?\n  OPENED: %U")
;;                ))

;; we now have 3 extra captures for that project eventually
;; overridding existing ones.
;;
;; We then setup the agenda commands for that project.

;; (add-to-list 'org-context-agenda-alist
;;              '("/home/homer/ProjectA"
;;                ;; definition relying on the `todo' one. Assuming
;;                ;; "todo.org" exists in "/home/homer/ProjectA".
;;                ("a" todo)

;;                ;; definition relying on the `todo' one and using
;;                ;; "/home/homer/ProjectA/tests/tests.org"
;;                ("c" (todo ("tests/tests.org")))

;;                ;; full definition as in Org manual
;;                ("d" "TODO view" alltodo ""
;;                 ((org-agenda-files
;;                   '("/home/homer/ProjectA/todo.org"))))
;;                )
;;              )

;;; Code

(require 'org)

(defvar org-context-capture-alist
  nil
  "Alist of filename patterns vs corresponding capture
template list.")

(defvar org-context-capture nil
  "Buffer local variable that holds the templates definitions.")
(make-variable-buffer-local 'org-context-capture)

(defvar org-context-agenda-alist
  nil
  "Alist of filename patterns vs corresponding custom agenda
  list.")

(defvar org-context-agenda nil
  "Buffer local variable that holds the custom agenda commands.")
(make-variable-buffer-local 'org-context-agenda)

(defvar org-context-capture-shortcut
  '((question
     "Question" entry
     (file place-holder)
     "* QUESTION %?\n  OPENED: %U")
    (todo
     "Todo" entry
     (file place-holder)
     "* TODO %?\n  OPENED: %U"))
  "Alist of symbols vs their corresponding template.
This is used in `org-context-capture-alist' to shorten the
  template definition.")

(defvar org-context-agenda-shortcut
  '((todo
     alltodo "")
    (todo-desc
     "ID1" alltodo "")
    (todo-settings
     alltodo ((default-directory "/tmp")))
    (todo-settings-desc
     "ID2" alltodo ((default-directory "/tmp")))))

(defvar org-context-add-overridden t
  "If set to t, add overridden templates in a submenu.")

(defadvice org-capture (around org-context-capture activate)
  "Allow contextual capture templates."
  (let* ((file-name (or buffer-file-name
                        (and (eq major-mode 'dired-mode)
                             default-directory)
                        (buffer-name)))
         (alist org-context-capture-alist)
         condition templates directory merge overridden-templates)
    ;; templates from local var or from `org-context-capture-alist'?
    (if (local-variable-p 'org-context-capture)
        (setq templates org-context-capture
              directory (dir-locals-find-file file-name)
              directory (if (listp directory)
                            (car directory)
                          (file-name-directory directory)))
      (while alist
        (setq condition (caar alist))
        (if (or (eq major-mode condition)
                (and (stringp condition)
                     (string-match condition file-name)))
            (setq directory (and (file-name-absolute-p
                                  (match-string 0 file-name))
                                 (match-string 0 file-name))
                  templates (cdar alist)
                  alist nil)
          (setq alist (cdr alist)))))

    (when templates
      ;; expand templates to look like as expected by org
      (setq templates
            (org-context-capture-expand templates directory))
      ;; merge `templates' and `org-capture-templates'

      ;; add eventually overridden templates to `merge'
      (dolist (temp org-capture-templates)
        (let ((o-temp (assoc (car temp) templates)))
          (if (null o-temp)
              (push temp merge)
            (push temp overridden-templates)
            (push o-temp merge))))

      ;; add the rest in `templates'
      (dolist (temp templates)
        (unless (assoc (car temp) merge)
          (push temp merge)))

      ;; eventually add overridden templates in submenu
      (when (and org-context-add-overridden overridden-templates)
        (push '("o" "Overridden") merge)
        (dolist (temp (nreverse overridden-templates))
          (push (cons (concat "o" (car temp)) (cdr temp))
                merge))))

    (let ((org-capture-templates
           (or (nreverse merge) org-capture-templates)))
      (debug-print org-capture-templates)
      ad-do-it)))

(defun org-context-capture-expand (templates directory)
  "Expand templates in the list of templates TEMPLATES."
  (mapcar
   (lambda (temp)
     (cond
      ;; submenu
      ((stringp (cdr temp))
       temp)
      ;; regular template
      ((and (stringp (nth 1 temp))
            (nth 2 temp))
       (when (and (string-match "\\`file" (symbol-name (car (nth 3 temp))))
                  (not (file-name-absolute-p (cadr (nth 3 temp)))))
         (setcar (cdr (nth 3 temp))
                 (expand-file-name (cadr (nth 3 temp)) directory)))
       temp)
      ;; stolen one
      (t
       (let* ((steal (if (listp (nth 1 temp)) (caadr temp) (nth 1 temp)))
              (org-file (or
                         (and (listp (cadr temp)) (nth 1 (cadr temp)))
                         "todo.org"))
              (id (and (listp (cadr temp)) (nth 2 (cadr temp))))
              new-temp)
         (if (or (file-name-absolute-p org-file)
                 directory)
             (setq org-file (expand-file-name org-file directory))
           (error "Unable to set target file!"))
         (setq new-temp
               (copy-tree
                (cdr
                 (assoc steal
                        (if (stringp steal)
                            org-capture-templates
                          org-context-capture-shortcut)))))
         (push (car temp) new-temp)
         (unless new-temp (error "Unable to steal template!"))
         (if id (setcar (cdr new-temp) id))
         (setcar (cdr (nth 3 new-temp)) org-file)
         new-temp))))
   templates))

(defun org-context-agenda-expand (templates directory)
  "Expand templates in the list of templates TEMPLATES."
  (mapcar
   (lambda (temp)
     (cond
      ((stringp (cdr temp))             ; submenu
       temp)
      ((nth 2 temp)                     ; regular template
       (oc-expand-regular-template temp directory))
      (t                                ; stolen template
       (oc-expand-stolen-template temp directory))))
   templates))

(defun oc-expand-regular-template (temp directory)
  (let (settings entry org-files buffer-name index)
    ;; settings' index
    (setq index (if (stringp (nth 1 temp)) 4 3))

    ;; add settings if there is none
    (unless (> (length temp) index)
      (setq temp (append temp '(nil))))

    (setq settings (nth index temp))

    ;; org-agenda-files setting
    (setq entry (assoc 'org-agenda-files settings))
    (setq org-files (if entry
                        (mapcar
                         (lambda (file)
                           (if (file-name-absolute-p file)
                               file
                             (expand-file-name file directory)))
                         (nth 1 (nth 1 entry)))
                      (list (expand-file-name "todo.org" directory))))
    (if entry
        (setcdr entry (list `(quote ,org-files)))
      (setq settings (cons `(org-agenda-files (quote ,org-files))
                           settings)))

    ;; org-agenda-buffer-name setting
    (setq entry (assoc 'org-agenda-buffer-name settings))
    (setq buffer-name (format
                       "*Agenda(%s:%s)*"
                       (if directory
                           (file-name-nondirectory
                            (directory-file-name
                             directory)) "??")
                       (car temp)))
    (if entry
        (setcdr entry (list buffer-name))
      (setq settings (cons `(org-agenda-buffer-name ,buffer-name)
                           settings)))

    (setcar (nthcdr index temp) settings))
  temp)

(defun oc-expand-stolen-template (temp directory)
  (let* ((steal (if (listp (cadr temp)) (caadr temp) (nth 1 temp)))
         (org-files (or
                     (and (listp (cadr temp)) (nth 1 (cadr temp)))
                     (list "todo.org")))
         (id (and (listp (cadr temp)) (nth 2 (cadr temp))))
         new-temp index extra-settings)
    (setq org-files
          (mapcar
           (lambda (file)
             (if (or (file-name-absolute-p file)
                     directory)
                 (expand-file-name file directory)
               (error "Unable to set target file!")))
           org-files))
    (setq new-temp
          (copy-tree
           (cdr
            (assoc steal
                   (if (stringp steal)
                       org-agenda-custom-commands
                     org-context-agenda-shortcut)))))
    (push (car temp) new-temp)
    (unless new-temp (error "Unable to steal template!"))
    (if id
        (if (stringp (nth 1 new-temp)) ; has an id
            (setcar (cdr new-temp) id)
          (setcdr new-temp (cons id (cdr new-temp)))))
    (setq index (if (stringp (nth 1 new-temp)) 4 3))
    (setq extra-settings
          (list
           `(org-agenda-files (quote ,org-files))
           `(default-directory ,directory)
           `(org-agenda-buffer-name
             ,(format
               "*Agenda(%s:%s)*"
               (if directory
                   (file-name-nondirectory
                    (directory-file-name
                     directory)) "??")
               (car temp)))))
    (if (<= (length new-temp) index) ; settings exists?
        (setcdr (last new-temp)
                (list extra-settings))
      (setcdr (last (nth index new-temp))
              extra-settings))
    new-temp))

(defadvice org-agenda (around org-context-agenda activate)
  "Allow contextual agenda view.
This advice looks into `org-context-agenda-alist' to see if the
visited file at the time `org-agenda' is invoked match. If so, it
adds to the existing views the ones specified in the tail of
the `org-context-agenda-alist' corresponding entry."
  (let* ((file-name (or buffer-file-name
                        (and (eq major-mode 'dired-mode)
                             default-directory)
                        (buffer-name)))
         (alist org-context-agenda-alist)
         condition templates directory merge overridden-templates)
    (if (local-variable-p 'org-context-agenda)
        (setq templates org-context-agenda
              directory (dir-locals-find-file file-name)
              directory (if (listp directory)
                            (car directory)
                          (file-name-directory directory)))
      (while alist
        (setq condition (caar alist))
        (if (or (eq major-mode condition)
                (and (stringp condition)
                     (string-match condition file-name)))
            (setq directory (and (file-name-absolute-p
                                  (match-string 0 file-name))
                                 (match-string 0 file-name))
                  templates (cdar alist)
                  alist nil)
          (setq alist (cdr alist)))))
    (when templates
      (setq templates
            (org-context-agenda-expand templates directory))

      ;; add eventually overridden templates to `merge'
      (dolist (temp org-agenda-custom-commands)
        (let ((o-temp (assoc (car temp) templates)))
          (if (null o-temp)
              (push temp merge)
            (push temp overridden-templates)
            (push o-temp merge))))

      ;; add the rest in `templates'
      (dolist (temp templates)
        (unless (assoc (car temp) merge)
          (push temp merge)))

      ;; eventually add overridden templates in submenu
      (when (and org-context-add-overridden overridden-templates)
        (push '("o" . "Overridden") merge)
        (dolist (temp (nreverse overridden-templates))
          (push (cons (concat "o" (car temp)) (cdr temp))
                merge))))

    (let ((org-agenda-custom-commands
           (or (nreverse merge) org-agenda-custom-commands)))
      ad-do-it)))

(provide 'org-context)

;;; org-context.el ends here
