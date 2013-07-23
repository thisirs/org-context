# org-context

This package advises `org-capture` and `org-agenda` to allow
contextual capture and agenda commands.

## Installation

Just put `org-context.el` in you load path and add this to your
`.emacs`:

```lisp
(require 'org-context)
(org-context-activate)
```

## Settings

Templates or commands are added to the existing ones depending on the
file the buffer is visiting, the buffer name, or the major mode at the
time `org-capture` or `org-agenda` are called.

The capture templates and the agenda commands can be customized in
a global and local way:

- The first way is to use the variable `org-context-capture-alist`
  for contextual capture templates and `org-context-agenda-alist`
  for contextual agenda commands that control how things are added
  globally.

- The second way is to use the buffer-local variables
  `org-context-capture` and `org-context-agenda`, typically in
  `.dir-locals.el`.

### Custom captures

The variable `org-context-capture-alist` is an alist of elements of
the form (CONDITION . TEMPLATE-LIST) and the buffer-local variable
`org-context-capture` is just a TEMPLATE-LIST.

CONDITION is either a major mode or a regular expression matching the
buffer file-name or the buffer name.

TEMPLATE-LIST is a list of templates as described in Org manual that
will be added to the existing ones.

To shorten the definition of templates, you can also write them as ID,
(KEY ID), (KEY (ID)), (KEY (ID FILE)) or (KEY (ID FILE DESC)).

ID is either a letter or a symbol. A symbol indicates to look up in
`org-context-capture-shortcut` for a template. A letter indicates to
reuse the definition of corresponding template in
`org-capture-templates`.

KEY is a letter that will be used to select the template.

If FILE is specified, the target file in that template is replaced by
FILE and the description by DESC. If FILE is not specified or is not
an absolute path, FILE is expanded against the string matched by
CONDITION if it is a regex.

DESC, if specified, is a description of the agenda command.

Valid definitions are for example,

```lisp
(add-to-list 'org-context-capture-alist
             '("/home/homer/ProjectA"
               question
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
                "* TODO %?\n  OPENED: %U")))
```

or in a `dir-locals.el` file,

```lisp
((nil
  (org-context-capture
   question
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
    "* TODO %?\n  OPENED: %U"))))
```

### Custom agenda commands

In the same way, `org-context-agenda-alist` is an alist of elements of
the form (CONDITION . TEMPLATE-LIST) and `org-context-capture` is just
a TEMPLATE-LIST.

CONDITION is either a major mode or a regular expression matching the
buffer file-name, the buffer name.

TEMPLATE-LIST is a list of custom agenda commands as described in Org
manual.

Again, to shorten the definition of custom commands, you can also
write them as ID, (KEY ID), (KEY (ID FILE-LIST)) or (KEY (ID FILE-LIST
DESC)).

If ID is a symbol, it is used to look up custom
commands in `org-context-agenda-shortcut`. If ID is a letter, it is
used to look up custom commands in `org-agenda-custom-commands`.

KEY is a letter that will be used to select the command.

FILE-LIST, if specified, is the list of files used to construct the
agenda. If the file names are not absolute, they are expanded against
the directory containing the `.dir-locals.el` file when the agenda
command are specified locally and against the string matched by
CONDITION if CONDITION is a regex and if the agenda commands are defined
globally.

DESC, if specified, is a description of the agenda command.

Valid definitions are for example,

```lisp
(add-to-list 'org-context-agenda-alist
             '("/home/homer/ProjectA"
               question
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
               ("d" "Description" alltodo
                (org-agenda-files '("/home/homer/ProjectA/todo.org")))))
```

## Examples

### Using `.dir-locals`

The following code placed in the `.dir-locals.el` file at the root of
a project defines two contextual captures targeting the files
`todo.org` and `tests/todo.org` as well as a custom agenda command
displaying a block agenda of those two org files.

```lisp
((nil
  (org-context-capture
   ("t" "Todo" entry
    (file "todo.org")
    "* TODO %?\n  OPENED: %U")
   ("u" "Test" entry
    (file "tests/todo.org")
    "* TODO %?\n  OPENED: %U"))
  (org-context-agenda
   ("t" "TODO + tests" ((alltodo "" ((org-agenda-files '("todo.org"))
                                     (org-agenda-overriding-header "TODO")))
                        (alltodo "" ((org-agenda-overriding-header "TESTS")
                                     (org-agenda-files '("tests/todo.org")))))
    ((org-agenda-buffer-name "TODO: org-context"))))))
```

### Using global definition

Say we have a project named `ProjectA` located in
`/home/homer/ProjectA` and several todo files in that project,
`/home/homer/ProjectA/tests/tests.org` and
`/home/homer/ProjectA/todo.org`. We first set contextual captures:

```lisp
(add-to-list 'org-context-context-alist
             '("/home/homer/ProjectA"
               ;; definition relying on the `todo' one. Assuming
               ;; "todo.org" exists in "/home/homer/ProjectA".
               ("a" todo)

               ;; definition relying on the `todo' one and using
               ;; "/home/homer/ProjectA/tests/tests.org"
               ("c" (todo "tests/tests.org"))))
```

we now have 2 extra captures for that project eventually
overridding existing ones.

We then setup the agenda commands for that project.

```lisp
(add-to-list 'org-context-agenda-alist
             '("/home/homer/ProjectA"
               ;; definition relying on the `todo' one. Assuming
               ;; "todo.org" exists in "/home/homer/ProjectA".
               ("a" todo)

               ;; definition relying on the `todo' one and using
               ;; "/home/homer/ProjectA/tests/tests.org"
               ("c" (todo "tests/tests.org"))

               ;; full definition as in Org manual
               ("d" "TODO view" alltodo ""
                ((org-agenda-files
                  '("/home/homer/ProjectA/todo.org"))))))
```
