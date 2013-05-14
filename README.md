# org-context

This package advises `org-capture` and `org-agenda` to allow
contextual capture templates and agenda commands.

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
two ways:

- The first way is to use the variable `org-context-capture-alist`
  for contextual capture templates and `org-context-agenda-alist`
  for contextual agenda commands that control how things are added
  globally.

- The second way is to use the buffer-local variables
  `org-context-capture` and `org-context-agenda`, typically in
  `.dir-locals.el`.

The variable `org-context-capture-alist` is an alist of elements of
the form (CONDITION . TEMPLATE-LIST) where CONDITION is either a
major mode or a regular expression matching the buffer file-name or
the buffer name. TEMPLATE-LIST is a list of templates as described
in Org manual that will be added to the existing ones. To shorten
the definition of templates, you can also write them as ID or (KEY
ID) or (KEY (ID)) or (KEY (ID FILE)) or (KEY (ID FILE DESC)). The
ID is either a letter or a symbol. A symbol indicates to look up in
`org-context-capture-shortcut` for a template. A letter indicates
to reuse the definition of corresponding template in
`org-capture-templates`. KEY is a letter that will be used to
select the template. If FILE is specified, the target file in
that template is replaced by FILE and the description by DESC. If
FILE is not specified or is not an absolute path, FILE is expanded
against the string matched by CONDITION if it is a regex.

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

In the same way, `org-context-agenda-alist` is an alist of elements
of the form (CONDITION . TEMPLATE-LIST) where CONDITION is either a
major mode or a regular expression matching the buffer file-name,
the buffer name and TEMPLATE-LIST is a list of custom agenda
commands as described in Org manual. Again, to shorten the
definition of custom commands, you can also write them as ID, (KEY
ID) or (KEY (ID FILE-LIST)) or (KEY (ID FILE-LIST DESC)). If ID is
a symbol, it is used to look up custom commands in
`org-context-agenda-shortcut`. If ID is a letter, it is used to
look up custom commands in `org-agenda-custom-commands`. KEY is a
letter that will be used to select the command. If specified,
FILE-LIST is the list of files used to construct the agenda. All
file's name in FILE-LIST are expanded against the string matched by
CONDITION if it is a regex.

Valid definitions are for example,

```lisp
(add-to-list 'org-context-capture-alist
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

## Example

For example, say we have a project named `ProjectA` located in
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
