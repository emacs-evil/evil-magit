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
;;   help         | h/? | ?
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
;;   submodule    | o   |
;;   prev section | p   | k
;;   push         | P   |
;;   rebase       | r   | R
;;   rename       | R   | _
;;   stage        | s/S |
;;   tag          | t   |
;;   notes        | T   |
;;   unstage      | u/U |
;;   revert       | v/V | h/H
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

;; TODO
;; 1. popups
;;    1. DONE: dispatch
;; 2. now that we're using evil-define-key and motion state we're going to have
;;    to define more keys (any that conflict with motion state)

;;; Code:

(require 'evil)
(require 'magit)

;; temporary until popups are fixed
(push '("\\*magit\.+" . motion) evil-buffer-regexps)
(push '("\\*magit-\.+popup\\*" . emacs) evil-buffer-regexps)

;; evil doesn't override the text property keymaps, so we need special functions
;; for these commands
(defmacro evil-magit-define-key (map key evil-magit-cmd &optional magit-cmd)
  (let* ((evil-magit-cmd (eval evil-magit-cmd))
         (magit-cmd (eval magit-cmd))
         (fun (intern (format "evil-magit-%s-or-%s" evil-magit-cmd magit-cmd)))
         (doc (format "Call %s if in evil's motion state and %s otherwise." evil-magit-cmd magit-cmd)))
    `(progn (defun ,fun ()
              ,doc
              (interactive)
              (call-interactively
               (if (evil-motion-state-p)
                   ',evil-magit-cmd
                 ',magit-cmd)))
            (define-key ,map ,key ',fun))))

(evil-define-key 'motion magit-mode-map "g" nil)
(evil-define-key 'motion magit-mode-map "\t"    'magit-section-toggle)
(evil-define-key 'motion magit-mode-map [C-tab] 'magit-section-cycle)
(evil-define-key 'motion magit-mode-map [M-tab] 'magit-section-cycle-diffs)
(evil-define-key 'motion magit-mode-map [s-tab] 'magit-section-cycle-global)
(evil-define-key 'motion magit-mode-map "^"    'magit-section-up)
(evil-define-key 'motion magit-mode-map "j"    'magit-section-forward) ; was n
(evil-define-key 'motion magit-mode-map "\C-j" 'evil-next-visual-line)
(evil-define-key 'motion magit-mode-map "gj"   'magit-section-forward-sibling) ; was M-n
(evil-define-key 'motion magit-mode-map "k"    'magit-section-backward) ; was p
(evil-define-key 'motion magit-mode-map "\C-k" 'evil-previous-visual-line)
(evil-define-key 'motion magit-mode-map "gk"   'magit-section-backward-sibling) ; was M-p
(evil-define-key 'motion magit-mode-map "+"    'magit-diff-more-context)
(evil-define-key 'motion magit-mode-map "-"    'magit-diff-less-context)
(evil-define-key 'motion magit-mode-map "0"    'magit-diff-default-context)
(evil-define-key 'motion magit-mode-map "1"    'magit-section-show-level-1)
(evil-define-key 'motion magit-mode-map "2"    'magit-section-show-level-2)
(evil-define-key 'motion magit-mode-map "3"    'magit-section-show-level-3)
(evil-define-key 'motion magit-mode-map "4"    'magit-section-show-level-4)
(evil-define-key 'motion magit-mode-map "\M-1" 'magit-section-show-level-1-all)
(evil-define-key 'motion magit-mode-map "\M-2" 'magit-section-show-level-2-all)
(evil-define-key 'motion magit-mode-map "\M-3" 'magit-section-show-level-3-all)
(evil-define-key 'motion magit-mode-map "\M-4" 'magit-section-show-level-4-all)
(evil-define-key 'motion magit-mode-map "r"    'magit-refresh) ; was on g
(evil-define-key 'motion magit-mode-map "\C-r" 'magit-refresh-all) ; was on G
(evil-define-key 'motion magit-mode-map "q" 'magit-mode-bury-buffer)
(evil-define-key 'motion magit-mode-map "$" 'magit-process-buffer)
(evil-define-key 'motion magit-mode-map "a" 'magit-cherry-apply)
(evil-define-key 'motion magit-mode-map "A" 'magit-cherry-pick-popup)
(evil-define-key 'motion magit-mode-map "b" 'magit-branch-popup)
(evil-define-key 'motion magit-mode-map "B" 'magit-bisect-popup)
(evil-define-key 'motion magit-mode-map "c" 'magit-commit-popup)
(evil-define-key 'motion magit-mode-map "d" 'magit-diff-popup)
(evil-define-key 'motion magit-mode-map "D" 'magit-diff-refresh-popup)
(evil-define-key 'motion magit-mode-map "h" 'magit-dispatch-popup)
(evil-define-key 'motion magit-mode-map "?" 'magit-dispatch-popup)
(evil-define-key 'motion magit-mode-map "\C-c\C-c" 'magit-dispatch-popup)
(evil-define-key 'motion magit-mode-map "\C-c\C-e" 'magit-dispatch-popup)
(evil-define-key 'motion magit-mode-map "e" 'magit-ediff-dwim)
(evil-define-key 'motion magit-mode-map "E" 'magit-ediff-popup)
(evil-define-key 'motion magit-mode-map "f" 'magit-fetch-popup)
(evil-define-key 'motion magit-mode-map "F" 'magit-pull-popup)
(evil-define-key 'motion magit-mode-map "i" 'magit-gitignore)
(evil-define-key 'motion magit-mode-map "I" 'magit-gitignore-locally)
(evil-define-key 'motion magit-mode-map "x" 'magit-delete-thing) ; was on k
(evil-define-key 'motion magit-mode-map "X" 'magit-file-untrack) ; was on K
(evil-define-key 'motion magit-mode-map "l" 'magit-log-popup)
(evil-define-key 'motion magit-mode-map "L" 'magit-log-refresh-popup)
(evil-define-key 'motion magit-mode-map "m" 'magit-merge-popup)
(evil-define-key 'motion magit-mode-map "M" 'magit-remote-popup)
(evil-define-key 'motion magit-mode-map "o" 'magit-submodule-popup)
(evil-define-key 'motion magit-mode-map "P" 'magit-push-popup)
(evil-define-key 'motion magit-mode-map "R" 'magit-rebase-popup) ; was on r
(evil-define-key 'motion magit-mode-map "_" 'magit-file-rename)  ; was R
(evil-define-key 'motion magit-mode-map "t" 'magit-tag-popup)
(evil-define-key 'motion magit-mode-map "T" 'magit-notes-popup)
(evil-define-key 'motion magit-mode-map "\r"       'magit-visit-thing)
(evil-define-key 'motion magit-mode-map [C-return] 'magit-visit-thing)
(evil-define-key 'motion magit-mode-map [M-return] 'magit-dired-jump)
(evil-define-key 'motion magit-mode-map "\s"       'magit-diff-show-or-scroll-up)
(evil-define-key 'motion magit-mode-map "\d"       'magit-diff-show-or-scroll-down)
(evil-define-key 'motion magit-mode-map "s" 'magit-stage-file)
(evil-define-key 'motion magit-mode-map "S" 'magit-stage-modified)
(evil-define-key 'motion magit-mode-map "u" 'magit-unstage-file)
(evil-define-key 'motion magit-mode-map "U" 'magit-unstage-all)
(evil-define-key 'motion magit-mode-map "h" 'magit-revert-no-commit)   ; was v
(evil-define-key 'motion magit-mode-map "H" 'magit-revert-popup)       ; was V
(evil-define-key 'motion magit-mode-map "w" 'magit-am-popup)
(evil-define-key 'motion magit-mode-map "W" 'magit-patch-popup)
(evil-define-key 'motion magit-mode-map "#" 'magit-reset) ; was on x
(evil-define-key 'motion magit-mode-map "y" 'magit-show-refs-popup)
(evil-define-key 'motion magit-mode-map "Y" 'magit-cherry)
(evil-define-key 'motion magit-mode-map "z" 'magit-stash-popup)
(evil-define-key 'motion magit-mode-map "Z" 'magit-stash-popup)
(evil-define-key 'motion magit-mode-map "|" 'magit-git-command) ; was :
(evil-define-key 'motion magit-mode-map "!" 'magit-run-popup)
(evil-define-key 'motion magit-mode-map "\C-xa"  'magit-add-change-log-entry)
(evil-define-key 'motion magit-mode-map "\C-x4a" 'magit-add-change-log-entry-other-window)
(evil-define-key 'motion magit-mode-map "\C-w"   'magit-copy-section-value)
(evil-define-key 'motion magit-mode-map "\M-w"   'magit-copy-buffer-revision)

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
                      (?H "Reverting"       magit-revert-popup)
                      (?l "Logging"         magit-log-popup)
                      (?m "Merging"         magit-merge-popup)
                      (?M "Remoting"        magit-remote-popup)
                      (?o "Submodules"      magit-submodule-popup)
                      (?P "Pushing"         magit-push-popup)
                      (?R "Rebasing"        magit-rebase-popup)
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
                      (?h "Reverse"         magit-reverse)
                      (?S "Stage all"       magit-stage-modified)
                      (?U "Unstage all"     magit-unstage-all)
                      nil
                      (?x "Discard"         magit-discard)
                      "\
 r      refresh current buffer
 TAB    toggle section at point
 RET    visit thing at point

 C-h m  show all key bindings" nil))

;;evil bindings
(evil-define-key 'motion magit-mode-map [remap evil-previous-line] 'evil-previous-visual-line)
(evil-define-key 'motion magit-mode-map [remap evil-next-line] 'evil-next-visual-line)
(evil-define-key 'motion magit-mode-map "v" 'set-mark-command)
(evil-define-key 'motion magit-mode-map "V" 'set-mark-command)
(evil-define-key 'motion magit-mode-map "gg" 'evil-goto-first-line)
(evil-define-key 'motion magit-mode-map "G" 'evil-goto-line)
;; (evil-define-key 'motion map "\C-d" 'evil-scroll-down)
(evil-define-key 'motion magit-mode-map "\C-d" 'magit-section-forward-sibling) ; was M-n
(evil-define-key 'motion magit-mode-map "\C-f" 'evil-scroll-page-down)
(when evil-want-C-u-scroll
  ;; (evil-define-key 'motion map "\C-u" 'evil-scroll-up)
  (evil-define-key 'motion magit-mode-map "\C-u" 'magit-section-backward-sibling)) ; was M-p
(evil-define-key 'motion magit-mode-map "\C-b" 'evil-scroll-page-up)
(evil-define-key 'motion magit-mode-map ":" 'evil-ex)
(evil-define-key 'motion magit-mode-map "/" 'evil-search-forward)
(evil-define-key 'motion magit-mode-map "n" 'evil-search-next)
(evil-define-key 'motion magit-mode-map "N" 'evil-search-previous)
(evil-define-key 'motion magit-mode-map "\C-z" 'evil-emacs-state)

(evil-define-key 'motion magit-status-mode-map "gz" 'magit-jump-to-stashes)
(evil-define-key 'motion magit-status-mode-map "gt" 'magit-jump-to-tracked)
(evil-define-key 'motion magit-status-mode-map "gn" 'magit-jump-to-untracked)
(evil-define-key 'motion magit-status-mode-map "gu" 'magit-jump-to-unstaged)
(evil-define-key 'motion magit-status-mode-map "gs" 'magit-jump-to-staged)
(evil-define-key 'motion magit-status-mode-map "gf" 'magit-jump-to-unpulled)
(evil-define-key 'motion magit-status-mode-map "gp" 'magit-jump-to-unpushed)

(evil-define-key 'motion magit-blob-mode-map "j" 'magit-blob-next)
(evil-define-key 'motion magit-blob-mode-map "k" 'magit-blob-previous)
(evil-define-key 'motion magit-blob-mode-map "q" 'magit-kill-this-buffer)

(evil-define-key 'motion magit-diff-mode-map "j" 'magit-section-forward)
(evil-define-key 'motion magit-diff-mode-map "gd" 'magit-jump-to-diffstat-or-diff)

(evil-define-key 'normal magit-blame-mode-map "\r" 'magit-show-commit)
(evil-define-key 'normal magit-blame-mode-map "\s" 'magit-diff-show-or-scroll-up)
(evil-define-key 'normal magit-blame-mode-map "\d" 'magit-diff-show-or-scroll-down)
(evil-define-key 'normal magit-blame-mode-map "b"  'magit-blame-popup)
(evil-define-key 'normal magit-blame-mode-map "j"  'magit-blame-next-chunk) ; was n
(evil-define-key 'normal magit-blame-mode-map "J"  'magit-blame-next-chunk-same-commit) ; was N
(evil-define-key 'normal magit-blame-mode-map "k"  'magit-blame-previous-chunk)
(evil-define-key 'normal magit-blame-mode-map "K"  'magit-blame-previous-chunk-same-commit)
(evil-define-key 'normal magit-blame-mode-map "q"  'magit-blame-quit)
(evil-define-key 'normal magit-blame-mode-map "t"  'magit-blame-toggle-headings)
(evil-define-key 'normal magit-blame-mode-map "\M-w" 'magit-blame-copy-hash)
(add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)

(evil-define-key 'motion git-rebase-mode-map (kbd "q")    'undefined)
(evil-define-key 'motion git-rebase-mode-map [remap undo] 'git-rebase-undo)
(evil-define-key 'motion git-rebase-mode-map (kbd "RET") 'git-rebase-show-commit)
(evil-define-key 'motion git-rebase-mode-map (kbd "SPC") 'magit-diff-show-or-scroll-up)
(evil-define-key 'motion git-rebase-mode-map (kbd "!")   'git-rebase-exec) ; was x
(evil-define-key 'motion git-rebase-mode-map (kbd "c")   'git-rebase-pick)
(evil-define-key 'motion git-rebase-mode-map (kbd "p")   'git-rebase-pick) ; was c
(evil-define-key 'motion git-rebase-mode-map (kbd "r")   'git-rebase-reword)
(evil-define-key 'motion git-rebase-mode-map (kbd "w")   'git-rebase-reword)
(evil-define-key 'motion git-rebase-mode-map (kbd "e")   'git-rebase-edit)
(evil-define-key 'motion git-rebase-mode-map (kbd "s")   'git-rebase-squash)
(evil-define-key 'motion git-rebase-mode-map (kbd "f")   'git-rebase-fixup)
(evil-define-key 'motion git-rebase-mode-map (kbd "y")   'git-rebase-insert)
(evil-define-key 'motion git-rebase-mode-map (kbd "x")   'git-rebase-kill-line) ; was k or C-k
(evil-define-key 'motion git-rebase-mode-map (kbd "d")   'git-rebase-kill-line) ; was k or C-k
(evil-define-key 'motion git-rebase-mode-map (kbd "k")   'git-rebase-backward-line) ; was p
(evil-define-key 'motion git-rebase-mode-map (kbd "j")   'forward-line) ; was n
(evil-define-key 'motion git-rebase-mode-map (kbd "gk")      'git-rebase-move-line-up) ; was M-p
(evil-define-key 'motion git-rebase-mode-map (kbd "gj")      'git-rebase-move-line-down) ; was M-n
(evil-define-key 'motion git-rebase-mode-map (kbd "M-<up>")   'git-rebase-move-line-up)
(evil-define-key 'motion git-rebase-mode-map (kbd "M-<down>") 'git-rebase-move-line-down)
(evil-define-key 'motion git-rebase-mode-map (kbd "C-x C-t")  'git-rebase-move-line-up)

(evil-define-key 'motion git-commit-mode-map (kbd "gk") 'git-commit-prev-message)
(evil-define-key 'motion git-commit-mode-map (kbd "gj") 'git-commit-next-message)

;; section maps: evil-define-key doesn't work here, because these maps are text overlays

(evil-magit-define-key magit-remote-section-map "R" 'magit-rebase-popup 'magit-branch-rename)
(evil-magit-define-key magit-remote-section-map "_" 'magit-branch-rename)

(evil-magit-define-key magit-branch-section-map "R" 'magit-rebase-popup 'magit-branch-rename)
(evil-magit-define-key magit-branch-section-map "_" 'magit-branch-rename)

(evil-magit-define-key magit-commit-section-map "v" 'set-mark-command 'magit-revert-no-commit)
(evil-magit-define-key magit-commit-section-map "h" 'magit-revert-no-commit 'magit-dispatch-popup)

(evil-magit-define-key magit-file-section-map "K" 'magit-section-backward-sibling 'magit-file-untrack)
(evil-magit-define-key magit-file-section-map "X" 'magit-file-untrack) ; was K
(evil-magit-define-key magit-file-section-map "_" 'magit-file-rename) ; was R
(evil-magit-define-key magit-file-section-map "v" 'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-file-section-map "h" 'magit-reverse 'magit-dispatch-popup) ; was v

(evil-magit-define-key magit-hunk-section-map "v" 'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-hunk-section-map "h" 'magit-reverse 'magit-dispatch-popup) ; was v

(evil-magit-define-key magit-staged-section-map "v" 'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-staged-section-map "h" 'magit-reverse 'magit-dispatch-popup) ; was v

;;; evil-magit.el ends soon
(provide 'evil-magit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-magit.el ends here
