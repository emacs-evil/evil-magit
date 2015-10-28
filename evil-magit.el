;;; evil-magit.el --- doing black magic

;; Copyright (C) 2015  The Magit Project Contributors

;; Package-Requires: ((evil "1.2.3") (magit "2.2.2"))
;; Homepage: https://github.com/magit/evil-magit

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library configures Magit and Evil to play well with each
;; other.  Or rather that's what it is supposed to do eventually.

;; Users are encouraged to create a branch and show each other how
;; they have configured these two packages to play well with each
;; other.

;; I hope that eventually all the black magitians can collectively
;; come up with a configuration that works for most users, or that
;; we end up with a collection of reasonable configurations.

;; For a general discussion see
;; https://github.com/magit/evil-magit/issues/1.

;; The basic scheme is as follows:

;;   Command      | Old | New
;;  --------------|-----|------
;;   cherry pick  | a/A |
;;   branch       | b   |
;;   bisect       | B   |
;;   commit       | c   |
;;   diff         | d/D |
;;   help         | h/? |
;;   ediff        | e/E |
;;   fetch        | f   |
;;   pull         | F   |
;;   ignore       | i/I |
;;   jump         | j   | g
;;   delete       | k   | x
;;   untrack      | K   | X
;;   log          | l/L |
;;   merge        | m   |
;;   remote       | M   |
;;   next section | n   | j
;;   submodule    | o   | C-o
;;   prev section | p   | k
;;   push         | P   |
;;   rebase       | r   |
;;   refresh      | g   | gr/gR
;;   rename       | R   |
;;   stage        | s/S |
;;   tag          | t   |
;;   notes        | T   |
;;   unstage      | u/U |
;;   revert       | v/V | o/O
;;   am           | w   |
;;   patch        | W   |
;;   reset        | x   | #
;;   show-refs    | y   |
;;   cherry       | Y   |
;;   stash        | z/Z |
;;   git-cmd      | :   | \|
;;   run          | !   |

;; Additions

;;   Command                                                | New
;;  --------------------------------------------------------|--------
;;   evil-goto-line                                         | G
;;   evil-search-next                                       | n
;;   evil-search-previous                                   | N
;;   set-mark-command                                       | v or V
;;   evil-ex                                                | :
;;   evil-search-forward                                    | /
;;   evil-scroll-page-up                                    | C-b
;;   magit-section-forward-sibling                          | gj or C-d
;;   evil-scroll-page-down                                  | C-f
;;   magit-section-backward-sibling (if C-u scroll enabled) | gk or C-u (if C-u scroll enabled)
;;   evil-emacs-state                                       | C-z
;;   evil-next-visual-line                                  | C-j
;;   evil-previous-visual-line                              | C-k

;; maps changed
;;
;; magit-cherry-mode-map
;; git-commit-mode-map
;; git-rebase-mode-map
;; magit-mode-map
;; magit-blame-mode-map
;; magit-blob-mode-map
;; magit-diff-mode-map
;; magit-log-mode-map
;; magit-log-select-mode-map
;; magit-reflog-mode-map
;; magit-status-mode-map

;; magit-branch-section-map
;; magit-commit-section-map
;; magit-file-sections-map
;; magit-hunk-section-map
;; magit-remote-section-map
;; magit-staged-section-map

;; maps unchanged
;;
;; magit-file-mode-map
;; magit-log-read-revs-map
;; magit-minibuffer-local-ns-map
;; magit-popup-mode-map
;; magit-process-mode-map
;; magit-refs-mode-map
;; with-editor-mode-map

;; magit-module-commit-section-map
;; magit-stash-section-map
;; magit-stashes-section-map
;; magit-tag-section-map
;; magit-unpulled-section-map
;; magit-unpushed-section-map
;; magit-unstaged-section-map
;; magit-untracked-section-map

;; TODO
;; 1. popups
;;    1. DONE: dispatch
;; 2. now that we're using evil-define-key and motion state we're going to have
;;    to define more keys (any that conflict with motion state)

;;; Code:

(require 'evil)
(require 'magit)

(defcustom evil-magit-state 'motion
  "State to use for most magit buffers."
  :group 'magit
  :type  'symbol)

;; temporary until popups are fixed
;; (push '("\\*magit\.+" . motion) evil-buffer-regexps)
;; (push '("\\*magit-\.+popup\\*" . emacs) evil-buffer-regexps)
(dolist (mode '(git-commit-mode
                magit-mode
                magit-cherry-mode
                magit-diff-mode
                magit-log-mode
                magit-log-select-mode
                ;; magit-popup-mode
                ;; magit-popup-sequence-mode
                magit-process-mode
                magit-reflog-mode
                magit-refs-mode
                magit-revision-mode
                magit-stash-mode
                magit-stashes-mode
                magit-status-mode))
  (setq evil-emacs-state-modes (delq mode evil-emacs-state-modes)))
(dolist (mode '(magit-mode
                git-popup-mode
                git-rebase-mode
                magit-blame-mode
                magit-blob-mode
                magit-cherry-mode
                magit-diff-mode
                magit-file-mode
                magit-gitflow-mode
                magit-log-mode
                ;; magit-popup-mode
                ;; magit-popup-sequence-mode
                magit-process-mode
                magit-reflog-mode
                magit-refs-mode
                magit-revision-mode
                magit-stash-mode
                magit-stashes-mode
                magit-status-mode))
  (add-to-list (intern (format "evil-%s-state-modes" evil-magit-state)) mode))

;; evil doesn't override the text property keymaps, so we need special functions
;; for these commands
(defmacro evil-magit-define-key (map key evil-magit-cmd &optional magit-cmd)
  (let* ((evil-magit-cmd (eval evil-magit-cmd))
         (magit-cmd (eval magit-cmd))
         (fun (intern (format "evil-magit-%s-or-%s" evil-magit-cmd magit-cmd)))
         (doc (format "Call %s if in evil's motion state and %s otherwise." evil-magit-cmd magit-cmd)))
    `(progn
       (unless (fboundp ',fun)
         (defun ,fun ()
           ,doc
           (interactive)
           (call-interactively
            (if (funcall (intern (format "evil-%s-state-p" evil-magit-state)))
                ',evil-magit-cmd
              ',magit-cmd))))
       (define-key ,map ,key ',fun))))

(evil-define-key evil-magit-state magit-mode-map
  "g"        nil
  "\t"       'magit-section-toggle
  [C-tab]    'magit-section-cycle
  [M-tab]    'magit-section-cycle-diffs
  [s-tab]    'magit-section-cycle-global
  "^"        'magit-section-up
  "j"        'magit-section-forward ; was n
  "\C-j"     'evil-next-visual-line
  "gj"       'magit-section-forward-sibling ; was M-n
  "k"        'magit-section-backward ; was p
  "\C-k"     'evil-previous-visual-line
  "gk"       'magit-section-backward-sibling ; was M-p
  "+"        'magit-diff-more-context
  "-"        'magit-diff-less-context
  "0"        'magit-diff-default-context
  "1"        'magit-section-show-level-1
  "2"        'magit-section-show-level-2
  "3"        'magit-section-show-level-3
  "4"        'magit-section-show-level-4
  "\M-1"     'magit-section-show-level-1-all
  "\M-2"     'magit-section-show-level-2-all
  "\M-3"     'magit-section-show-level-3-all
  "\M-4"     'magit-section-show-level-4-all
  "gr"       'magit-refresh ; was on g
  "gR"       'magit-refresh-all ; was on G
  "q"        'magit-mode-bury-buffer
  "$"        'magit-process-buffer
  "a"        'magit-cherry-apply
  "A"        'magit-cherry-pick-popup
  "b"        'magit-branch-popup
  "B"        'magit-bisect-popup
  "c"        'magit-commit-popup
  "d"        'magit-diff-popup
  "D"        'magit-diff-refresh-popup
  "h"        'magit-dispatch-popup
  "?"        'magit-dispatch-popup
  "\C-c\C-c" 'magit-dispatch-popup
  "\C-c\C-e" 'magit-dispatch-popup
  "e"        'magit-ediff-dwim
  "E"        'magit-ediff-popup
  "f"        'magit-fetch-popup
  "F"        'magit-pull-popup
  "i"        'magit-gitignore
  "I"        'magit-gitignore-locally
  "x"        'magit-delete-thing ; was on k
  "X"        'magit-file-untrack ; was on K
  "l"        'magit-log-popup
  "L"        'magit-log-refresh-popup
  "m"        'magit-merge-popup
  "M"        'magit-remote-popup
  "\C-o"     'magit-submodule-popup
  "P"        'magit-push-popup
  "r"        'magit-rebase-popup
  "R"        'magit-file-rename
  "t"        'magit-tag-popup
  "T"        'magit-notes-popup
  "\r"       'magit-visit-thing
  [C-return] 'magit-visit-thing
  [M-return] 'magit-dired-jump
  "\s"       'magit-diff-show-or-scroll-up
  "\d"       'magit-diff-show-or-scroll-down
  "s"        'magit-stage-file
  "S"        'magit-stage-modified
  "u"        'magit-unstage-file
  "U"        'magit-unstage-all
  "o"        'magit-revert-no-commit   ; was v
  "O"        'magit-revert-popup       ; was V
  "w"        'magit-am-popup
  "W"        'magit-patch-popup
  "#"        'magit-reset ; was on x
  "y"        'magit-show-refs-popup
  "Y"        'magit-cherry
  "z"        'magit-stash-popup
  "Z"        'magit-stash-popup
  "|"        'magit-git-command ; was :
  "!"        'magit-run-popup
  "\C-xa"    'magit-add-change-log-entry
  "\C-x4a"   'magit-add-change-log-entry-other-window
  "\C-w"     'magit-copy-section-value
  "\M-w"     'magit-copy-buffer-revision
  ;; evil-specific bindings
  "v"        'set-mark-command
  "V"        'set-mark-command
  "gg"       'evil-goto-first-line
  "G"        'evil-goto-line
  ;; "\C-d"     'evil-scroll-down
  "\C-d"     'magit-section-forward-sibling ; was M-n
  "\C-f"     'evil-scroll-page-down
  "\C-b"     'evil-scroll-page-up
  ":"        'evil-ex
  "/"        'evil-search-forward
  "n"        'evil-search-next
  "N"        'evil-search-previous
  "\C-z"     'evil-emacs-state)

(when evil-want-C-u-scroll
  ;; (evil-define-key evil-magit-state map "\C-u" 'evil-scroll-up)
  (evil-define-key evil-magit-state magit-mode-map "\C-u" 'magit-section-backward-sibling)) ; was M-p

;; dispatch-popup
(plist-put magit-dispatch-popup
           :actions '("Popup and dwim commands"
                      (?A "Cherry-picking"  magit-cherry-pick-popup)
                      (?b "Branching"       magit-branch-popup)
                      (?B "Bisecting"       magit-bisect-popup)
                      (?c "Committing"      magit-commit-popup)
                      (?d "Diffing"         magit-diff-popup)
                      (?D "Change diffs"    magit-diff-refresh-popup)
                      (?e "Ediff dwimming"  magit-ediff-dwim)
                      (?E "Ediffing"        magit-ediff-popup)
                      (?f "Fetching"        magit-fetch-popup)
                      (?F "Pulling"         magit-pull-popup)
                      (?l "Logging"         magit-log-popup)
                      (?m "Merging"         magit-merge-popup)
                      (?M "Remoting"        magit-remote-popup)
                      (?O "Reverting"       magit-revert-popup)
                      (?\C-o "Submodules"   magit-submodule-popup)
                      (?P "Pushing"         magit-push-popup)
                      (?r "Rebasing"        magit-rebase-popup)
                      (?t "Tagging"         magit-tag-popup)
                      (?T "Notes"           magit-notes-popup)
                      (?w "Apply patches"   magit-am-popup)
                      (?W "Format patches"  magit-patch-popup)
                      (?y "Show Refs"       magit-show-refs-popup)
                      (?z "Stashing"        magit-stash-popup)
                      (?! "Running"         magit-run-popup)
                      "Applying changes"
                      (?a "Apply"           magit-apply)
                      (?s "Stage"           magit-stage)
                      (?u "Unstage"         magit-unstage)
                      nil
                      (?o "Reverse"         magit-reverse)
                      (?S "Stage all"       magit-stage-modified)
                      (?U "Unstage all"     magit-unstage-all)
                      nil
                      (?x "Discard"         magit-discard)
                      "\
 gr     refresh current buffer
 TAB    toggle section at point
 RET    visit thing at point

 C-h m  show all key bindings" nil))

(magit-change-popup-key 'magit-revert-popup :actions ?v ?o)
(magit-change-popup-key 'magit-revert-popup :actions ?V ?O)

(evil-define-key evil-magit-state magit-status-mode-map
  "gz" 'magit-jump-to-stashes
  "gt" 'magit-jump-to-tracked
  "gn" 'magit-jump-to-untracked
  "gu" 'magit-jump-to-unstaged
  "gs" 'magit-jump-to-staged
  "gf" 'magit-jump-to-unpulled
  "gp" 'magit-jump-to-unpushed)

(evil-define-key evil-magit-state magit-blob-mode-map
  "j" 'magit-blob-next
  "k" 'magit-blob-previous
  "q" 'magit-kill-this-buffer)

(evil-define-key evil-magit-state magit-diff-mode-map
  "j"  'magit-section-forward
  "gd" 'magit-jump-to-diffstat-or-diff)

(evil-define-key 'normal magit-blame-mode-map
  "\r"   'magit-show-commit
  "\s"   'magit-diff-show-or-scroll-up
  "\d"   'magit-diff-show-or-scroll-down
  "b"    'magit-blame-popup
  "j"    'magit-blame-next-chunk ; was n
  "\C-j" 'evil-next-visual-line
  "J"    'magit-blame-next-chunk-same-commit ; was N
  "k"    'magit-blame-previous-chunk
  "\C-k" 'evil-previous-visual-line
  "K"    'magit-blame-previous-chunk-same-commit
  "q"    'magit-blame-quit
  "t"    'magit-blame-toggle-headings
  "\M-w" 'magit-blame-copy-hash)
(add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)

(evil-define-key evil-magit-state git-rebase-mode-map
  "q"          'undefined
  [remap undo] 'git-rebase-undo
  "\r"         'git-rebase-show-commit
  "\s"         'magit-diff-show-or-scroll-up
  "!"          'git-rebase-exec ; was x
  "c"          'git-rebase-pick
  "p"          'git-rebase-pick ; was c
  "r"          'git-rebase-reword
  "w"          'git-rebase-reword
  "e"          'git-rebase-edit
  "s"          'git-rebase-squash
  "f"          'git-rebase-fixup
  "y"          'git-rebase-insert
  "x"          'git-rebase-kill-line ; was k or C-k
  "d"          'git-rebase-kill-line ; was k or C-k
  "k"          'git-rebase-backward-line ; was p
  "j"          'forward-line ; was n
  "gk"         'git-rebase-move-line-up ; was M-p
  "gj"         'git-rebase-move-line-down ; was M-n
  [M-<up>]     'git-rebase-move-line-up
  [M-<down>]   'git-rebase-move-line-down
  "\C-x\C-t"   'git-rebase-move-line-up)

(evil-define-key evil-magit-state git-commit-mode-map
  "gk" 'git-commit-prev-message
  "gj" 'git-commit-next-message)

(evil-define-key evil-magit-state magit-log-mode-map
  "\C-c\C-b" 'magit-go-backward
  "\C-c\C-f" 'magit-go-forward
  "="        'magit-log-toggle-commit-limit
  "+"        'magit-log-double-commit-limit
  "-"        'magit-log-half-commit-limit
  "q"        'magit-log-bury-buffer)

(evil-define-key evil-magit-state magit-log-select-mode-map
  "\C-c\C-b" 'undefined
  "\C-c\C-f" 'undefined
  "."        'magit-log-select-pick
  "e"        'magit-log-select-pick
  "\C-c\C-c" 'magit-log-select-pick
  "q"        'magit-log-select-quit
  "\C-c\C-k" 'magit-log-select-quit)

(evil-define-key evil-magit-state magit-cherry-mode-map
  "q" 'magit-log-bury-buffer
  "L" 'magit-toggle-margin)

(evil-define-key evil-magit-state magit-reflog-mode-map "L" 'magit-toggle-margin)

;; section maps: evil-define-key doesn't work here, because these maps are text overlays

(evil-magit-define-key magit-commit-section-map "v" 'set-mark-command 'magit-revert-no-commit)
(evil-magit-define-key magit-commit-section-map "o" 'magit-revert-no-commit 'magit-submodule-popup)

(evil-magit-define-key magit-file-section-map "K"    'magit-section-backward-sibling 'magit-file-untrack)
(evil-magit-define-key magit-file-section-map "X"    'magit-file-untrack) ; was K
(evil-magit-define-key magit-file-section-map "v"    'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-file-section-map "o"    'magit-reverse 'magit-submodule-popup) ; was v
(evil-magit-define-key magit-file-section-map "\C-j" 'evil-next-visual-line 'magit-diff-visit-file-worktree)

(evil-magit-define-key magit-hunk-section-map "v"    'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-hunk-section-map "o"    'magit-reverse 'magit-submodule-popup) ; was v
(evil-magit-define-key magit-hunk-section-map "\C-j" 'evil-next-visual-line 'magit-diff-visit-file-worktree)

(evil-magit-define-key magit-staged-section-map "v" 'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-staged-section-map "o" 'magit-reverse 'magit-submodule-popup) ; was v

;;; evil-magit.el ends soon
(provide 'evil-magit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-magit.el ends here
