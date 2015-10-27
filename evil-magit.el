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

;; | Command      | Old | New  |
;; |--------------+-----+------|
;; | cherry pick  | a/A |      |
;; | branch       | b   |      |
;; | bisect       | B   |      |
;; | commit       | c   |      |
;; | diff         | d/D |      |
;; | help         | h/? | ?    |
;; | ediff        | e/E |      |
;; | fetch        | f   |      |
;; | pull         | F   |      |
;; | ignore       | i/I |      |
;; | jump         | j   | J    |
;; | delete       | k   | x    |
;; | untrack      | K   | X    |
;; | log          | l/L |      |
;; | merge        | m   |      |
;; | remote       | M   |      |
;; | next section | n   | j    |
;; | submodule    | o   |      |
;; | prev section | p   | k    |
;; | push         | P   |      |
;; | rebase       | r   | R    |
;; | rename       | R   | _    |
;; | stage        | s/S |      |
;; | tag          | t   |      |
;; | notes        | T   |      |
;; | unstage      | u/U |      |
;; | revert       | v/V | h/H  |
;; | am           | w   |      |
;; | patch        | W   |      |
;; | reset        | x   | #    |
;; | show-refs    | y   |      |
;; | cherry       | Y   |      |
;; | stash        | z/Z |      |
;; | git-cmd      | :   | vbar |
;; | run          | !   |      |

;; Additions
;; | Command                                                | New    |
;; |--------------------------------------------------------+--------|
;; | evil-goto-line                                         | G      |
;; | evil-search-next                                       | n      |
;; | evil-search-previous                                   | N      |
;; | set-mark-command                                       | v or V |
;; | evil-ex                                                | :      |
;; | evil-search-forward                                    | /      |
;; | evil-scroll-page-up                                    | C-b    |
;; | magit-section-forward-sibling                          | C-d    |
;; | evil-scroll-page-down                                  | C-f    |
;; | magit-section-backward-sibling (if C-u scroll enabled) | C-u    |
;; | evil-emacs-state                                       | C-z    |

;; maps changed
;;
;; git-commit-mode-map
;; git-rebase-mode-map
;; magit-mode-map
;; magit-blame-mode-map
;; magit-blob-mode-map
;; magit-branch-section-map
;; magit-commit-section-map
;; magit-diff-mode-map
;; magit-file-sections-map
;; magit-hunk-section-map
;; magit-remote-section-map
;; magit-staged-section-map
;; magit-status-mode-map

;; maps unchanged
;;
;; magit-cherry-mode-map
;; magit-file-mode-map
;; magit-log-mode-map
;; magit-log-read-revs-map
;; magit-log-select-mode-map
;; magit-minibuffer-local-ns-map
;; magit-module-commit-section-map
;; magit-popup-mode-map
;; magit-process-mode-map
;; magit-reflog-mode-map
;; magit-refs-mode-map
;; magit-stash-section-map
;; magit-stashes-section-map
;; magit-tag-section-map
;; magit-unpulled-section-map
;; magit-unpushed-section-map
;; magit-unstaged-section-map
;; magit-untracked-section-map
;; with-editor-mode-map

;;; Code:

(require 'evil)
(require 'magit)

(setq magit-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\t"    'magit-section-toggle)
        (define-key map [C-tab] 'magit-section-cycle)
        (define-key map [M-tab] 'magit-section-cycle-diffs)
        (define-key map [s-tab] 'magit-section-cycle-global)
        (define-key map "^"    'magit-section-up)
        (define-key map "j"    'magit-section-forward)          ; was n
        (define-key map "\M-j" 'magit-section-forward-sibling)  ; was M-n
        (define-key map "k"    'magit-section-backward)         ; was p
        (define-key map "\M-k" 'magit-section-backward-sibling) ; was M-p
        (define-key map "+"    'magit-diff-more-context)
        (define-key map "-"    'magit-diff-less-context)
        (define-key map "0"    'magit-diff-default-context)
        (define-key map "1"    'magit-section-show-level-1)
        (define-key map "2"    'magit-section-show-level-2)
        (define-key map "3"    'magit-section-show-level-3)
        (define-key map "4"    'magit-section-show-level-4)
        (define-key map "\M-1" 'magit-section-show-level-1-all)
        (define-key map "\M-2" 'magit-section-show-level-2-all)
        (define-key map "\M-3" 'magit-section-show-level-3-all)
        (define-key map "\M-4" 'magit-section-show-level-4-all)
        (define-key map "r"    'magit-refresh) ; was on g
        (define-key map "\C-r" 'magit-refresh-all) ; was on G
        (define-key map "q" 'magit-mode-bury-buffer)
        (define-key map "$" 'magit-process-buffer)
        (define-key map "a" 'magit-cherry-apply)
        (define-key map "A" 'magit-cherry-pick-popup)
        (define-key map "b" 'magit-branch-popup)
        (define-key map "B" 'magit-bisect-popup)
        (define-key map "c" 'magit-commit-popup)
        (define-key map "d" 'magit-diff-popup)
        (define-key map "D" 'magit-diff-refresh-popup)
        (define-key map "h" 'magit-dispatch-popup)
        (define-key map "?" 'magit-dispatch-popup)
        (define-key map "\C-c\C-c" 'magit-dispatch-popup)
        (define-key map "\C-c\C-e" 'magit-dispatch-popup)
        (define-key map "e" 'magit-ediff-dwim)
        (define-key map "E" 'magit-ediff-popup)
        (define-key map "f" 'magit-fetch-popup)
        (define-key map "F" 'magit-pull-popup)
        (define-key map "i" 'magit-gitignore)
        (define-key map "I" 'magit-gitignore-locally)
        (define-key map "x" 'magit-delete-thing) ; was on k
        (define-key map "X" 'magit-file-untrack) ; was on K
        (define-key map "l" 'magit-log-popup)
        (define-key map "L" 'magit-log-refresh-popup)
        (define-key map "m" 'magit-merge-popup)
        (define-key map "M" 'magit-remote-popup)
        (define-key map "o" 'magit-submodule-popup)
        (define-key map "P" 'magit-push-popup)
        (define-key map "R" 'magit-rebase-popup) ; was on r
        (define-key map "_" 'magit-file-rename) ; was R
        (define-key map "t" 'magit-tag-popup)
        (define-key map "T" 'magit-notes-popup)
        (define-key map "\r"       'magit-visit-thing)
        (define-key map [C-return] 'magit-visit-thing)
        (define-key map [M-return] 'magit-dired-jump)
        (define-key map "\s"       'magit-diff-show-or-scroll-up)
        (define-key map "\d"       'magit-diff-show-or-scroll-down)
        (define-key map "s" 'magit-stage-file)
        (define-key map "S" 'magit-stage-modified)
        (define-key map "u" 'magit-unstage-file)
        (define-key map "U" 'magit-unstage-all)
        (define-key map "h" 'magit-revert-no-commit) ; was v
        (define-key map "H" 'magit-revert-popup)     ; was V
        (define-key map "w" 'magit-am-popup)
        (define-key map "W" 'magit-patch-popup)
        (define-key map "#" 'magit-reset) ; was on x
        (define-key map "y" 'magit-show-refs-popup)
        (define-key map "Y" 'magit-cherry)
        (define-key map "z" 'magit-stash-popup)
        (define-key map "Z" 'magit-stash-popup)
        (define-key map "|" 'magit-git-command) ; was :
        (define-key map "!" 'magit-run-popup)
        (define-key map "\C-xa"  'magit-add-change-log-entry)
        (define-key map "\C-x4a" 'magit-add-change-log-entry-other-window)
        (define-key map "\C-w"   'magit-copy-section-value)
        (define-key map "\M-w"   'magit-copy-buffer-revision)
        ;;evil bindings
        (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
        (define-key map [remap evil-next-line] 'evil-next-visual-line)
        (define-key map "v" 'set-mark-command)
        (define-key map "V" 'set-mark-command)
        (define-key map "g" nil)
        (define-key map "gg" 'evil-goto-first-line)
        (define-key map "G" 'evil-goto-line)
        ;; (define-key map "\C-d" 'evil-scroll-down)
        (define-key map "\C-d" 'magit-section-forward-sibling) ; was M-n
        (define-key map "\C-f" 'evil-scroll-page-down)
        (when evil-want-C-u-scroll
          ;; (define-key map "\C-u" 'evil-scroll-up)
          (define-key map "\C-u" 'magit-section-backward-sibling) ; was M-p
          )
        (define-key map "\C-b" 'evil-scroll-page-up)
        (define-key map ":" 'evil-ex)
        (define-key map "/" 'evil-search-forward)
        (define-key map "n" 'evil-search-next)
        (define-key map "N" 'evil-search-previous)
        (define-key map "\C-z" 'evil-emacs-state)
        map))

(setq magit-status-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map magit-mode-map)
        (define-key map "Jz" 'magit-jump-to-stashes)
        (define-key map "Jt" 'magit-jump-to-tracked)
        (define-key map "Jn" 'magit-jump-to-untracked)
        (define-key map "Ju" 'magit-jump-to-unstaged)
        (define-key map "Js" 'magit-jump-to-staged)
        (define-key map "Jf" 'magit-jump-to-unpulled)
        (define-key map "Jp" 'magit-jump-to-unpushed)
        map))

(define-key magit-branch-section-map "R" nil)
(define-key magit-branch-section-map "_" 'magit-branch-rename)

(define-key magit-remote-section-map "R" nil)
(define-key magit-remote-section-map "_" 'magit-branch-rename)

(setq magit-blob-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "j" 'magit-blob-next)
        (define-key map "k" 'magit-blob-previous)
        (define-key map "q" 'magit-kill-this-buffer)))

(define-key magit-commit-section-map "v" nil)
(define-key magit-commit-section-map "h" 'magit-revert-no-commit)

(define-key magit-diff-mode-map "j" nil)
(define-key magit-diff-mode-map "J" 'magit-jump-to-diffstat-or-diff)

(setq magit-file-section-map
      (let ((map (make-sparse-keymap)))
        (define-key map [C-return] 'magit-diff-visit-file-worktree)
        (define-key map "\C-j"     'magit-diff-visit-file-worktree)
        (define-key map [remap magit-visit-thing]  'magit-diff-visit-file)
        (define-key map [remap magit-delete-thing] 'magit-discard)
        (define-key map "a"  'magit-apply)
        (define-key map "C"  'magit-commit-add-log)
        (define-key map "X"  'magit-file-untrack) ; was K
        (define-key map "_"  'magit-file-rename) ; was R
        (define-key map "s"  'magit-stage)
        (define-key map "u"  'magit-unstage)
        (define-key map "h"  'magit-reverse) ; was v
        map))

(setq magit-hunk-section-map
      (let ((map (make-sparse-keymap)))
        (define-key map [C-return] 'magit-diff-visit-file-worktree)
        (define-key map "\C-j"     'magit-diff-visit-file-worktree)
        (define-key map [remap magit-visit-thing]  'magit-diff-visit-file)
        (define-key map [remap magit-delete-thing] 'magit-discard)
        (define-key map "a"  'magit-apply)
        (define-key map "C"  'magit-commit-add-log)
        (define-key map "s"  'magit-stage)
        (define-key map "u"  'magit-unstage)
        (define-key map "h"  'magit-reverse) ; was v
        map))

(define-key magit-staged-section-map "v" nil)
(define-key magit-staged-section-map "h" 'magit-reverse)

(setq magit-blame-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\r" 'magit-show-commit)
        (define-key map "\s" 'magit-diff-show-or-scroll-up)
        (define-key map "\d" 'magit-diff-show-or-scroll-down)
        (define-key map "b"  'magit-blame-popup)
        (define-key map "j"  'magit-blame-next-chunk) ; was n
        (define-key map "J"  'magit-blame-next-chunk-same-commit) ; was N
        (define-key map "k"  'magit-blame-previous-chunk)
        (define-key map "K"  'magit-blame-previous-chunk-same-commit)
        (define-key map "q"  'magit-blame-quit)
        (define-key map "t"  'magit-blame-toggle-headings)
        (define-key map "\M-w" 'magit-blame-copy-hash)
        map))

(setq git-rebase-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map special-mode-map)
        (define-key map (kbd "q")    'undefined)
        (define-key map [remap undo] 'git-rebase-undo)
        (define-key map (kbd "RET") 'git-rebase-show-commit)
        (define-key map (kbd "SPC") 'magit-diff-show-or-scroll-up)
        (define-key map (kbd "!")   'git-rebase-exec) ; was x
        (define-key map (kbd "c")   'git-rebase-pick)
        (define-key map (kbd "r")   'git-rebase-reword)
        (define-key map (kbd "w")   'git-rebase-reword)
        (define-key map (kbd "e")   'git-rebase-edit)
        (define-key map (kbd "s")   'git-rebase-squash)
        (define-key map (kbd "f")   'git-rebase-fixup)
        (define-key map (kbd "y")   'git-rebase-insert)
        (define-key map (kbd "x")   'git-rebase-kill-line) ; was k or C-k
        (define-key map (kbd "d")   'git-rebase-kill-line) ; was k or C-k
        (define-key map (kbd "k")   'git-rebase-backward-line) ; was p
        (define-key map (kbd "j")   'forward-line) ; was n
        (define-key map (kbd "M-k")      'git-rebase-move-line-up) ; was M-p
        (define-key map (kbd "M-j")      'git-rebase-move-line-down) ; was M-n
        (define-key map (kbd "M-<up>")   'git-rebase-move-line-up)
        (define-key map (kbd "M-<down>") 'git-rebase-move-line-down)
        (define-key map (kbd "C-x C-t")  'git-rebase-move-line-up)
        map))

(define-key git-commit-mode-map (kbd "M-k") 'git-commit-prev-message)
(define-key git-commit-mode-map (kbd "M-j") 'git-commit-next-message)

;;; evil-magit.el ends soon
(provide 'evil-magit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-magit.el ends here
