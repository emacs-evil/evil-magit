Black magic
===========

This library configures Magit and Evil to play well with each
other. For some background see [Issue #1](https://github.com/justbur/evil-magit/issues/1).

Installation and Use
====================

Everything is contained in evil-magit.el, so you may download and load that file
directly. The recommended method is to use MELPA via package.el (`M-x
package-install RET evil-magit RET`).

Evil and Magit are both required. After requiring those packages, the following
will setup the new key bindings for you.

```elisp
;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'motion)
(require 'evil-magit)
```

Use `evil-magit-revert` to revert changes made by evil-magit to the default
evil+magit behavior.

Key Bindings
============

The basic key binding scheme is described in the following tables.

   Command              | Old  | New
  ----------------------|----- |------
   cherry pick          | a/A  |
   branch               | b    |
   bisect               | B    |
   commit               | c    |
   diff                 | d/D  |
   help                 | h/?  |
   ediff                | e/E  |
   fetch                | f    |
   pull                 | F    |
   ignore               | i/I  |
   jump                 | j    | g
   delete               | k    | x
   untrack              | K    | X
   log                  | l/L  |
   merge                | m    |
   remote               | M    |
   next section         | n    | C-j
   next section sibling | M-n  | gj or ]
   submodule            | o    | C-o
   prev section         | p    | C-k
   prev section sibling | M-p  | gk or [
   push                 | P    |
   rebase               | r    |
   refresh              | g    | gr/gR
   rename               | R    |
   stage                | s/S  |
   tag                  | t    |
   notes                | T    |
   unstage              | u/U  |
   revert               | v/V  | o/O
   am                   | w    |
   patch                | W    |
   reset                | x    | C-r
   show-refs            | y    |
   cherry               | Y    |
   stash                | z/Z  |
   git-cmd              | :    | \ |
   run                  | !    |

Evil-specific commands and more

   Command                     | New
  -----------------------------|--------
   evil-goto-line              | G
   evil-next-visual-line       | j
   evil-previous-visual-line   | k
   evil-search-next            | n
   evil-search-previous        | N
   set-mark-command            | v or V
   evil-ex                     | :
   evil-search-forward         | /
   evil-scroll-page-up         | C-b
   evil-scroll-down            | C-d
   evil-scroll-page-down       | C-f
   evil-scroll-up              | C-u (if C-u scroll enabled)
   evil-emacs-state            | C-z

Any other bindings are meant to be consistent with these.

Known Conflicts
===============

These are the third-party packages that conflict with these bindings and will
probably need to be disabled in magit buffers for evil-magit to work properly.

 1. [evil-snipe](https://github.com/hlissner/evil-snipe)
 2. [evil-escape](https://github.com/syl20bnr/evil-escape) with [certain escape sequences](https://github.com/justbur/evil-magit/issues/4)

Disclaimer
==========

Given the complexity of magit key bindings combined with the complexity of git
itself, it is possible that there are some rough edges where the current binding
is not the expected one in a buffer. It will be very helpful for you to report
any such instances.
