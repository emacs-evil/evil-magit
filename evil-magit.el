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
;;   next section | n   |
;;   submodule    | o   | C-o
;;   prev section | p   |
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
;;   reset        | x   | C-r
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
;;   evil-scroll-up                                         | C-u (if C-u scroll enabled)
;;   evil-scroll-page-up                                    | C-b
;;   magit-section-forward-sibling                          | ]
;;   evil-scroll-down                                       | C-d
;;   evil-scroll-page-down                                  | C-f
;;   magit-section-backward-sibling (if C-u scroll enabled) | [
;;   evil-emacs-state                                       | C-z
;;   evil-next-visual-line                                  | j
;;   evil-previous-visual-line                              | k

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

;; evil doesn't override the text property keymaps, so we need special functions
;; for these commands
(defmacro evil-magit-define-key (map key evil-magit-cmd &optional magit-cmd)
  (let* ((evil-magit-cmd (eval evil-magit-cmd))
         (magit-cmd (eval magit-cmd))
         (fun (intern (format "evil-magit-%s-or-%s" evil-magit-cmd magit-cmd)))
         (doc (format "Call %s if in an evil state other than emacs-state and %s otherwise." evil-magit-cmd magit-cmd)))
    `(progn
       (unless (fboundp ',fun)
         (defun ,fun ()
           ,doc
           (interactive)
           (let ((cmd (if (or (null evil-state) (evil-emacs-state-p))
                          ',magit-cmd
                        ',evil-magit-cmd)))
             (setq this-command cmd)
             (call-interactively cmd))))
       (define-key ,map ,key ',fun))))

;; without this set-mark-command activates visual-state which is just annoying
;; and introduces possible bugs
(defun evil-magit-remove-visual-activate-hook ()
  (when (derived-mode-p 'magit-mode)
    (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)))
(add-hook 'evil-local-mode-hook 'evil-magit-remove-visual-activate-hook)

(defun evil-magit-maybe-deactivate-mark ()
  "Deactivate mark if region is active. Used for ESC binding."
  (interactive)
  (when (region-active-p) (deactivate-mark)))

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
                ;; git-popup-mode
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

;; Make relevant maps into overriding maps so that they shadow the global evil
;; maps by default
(dolist (map (list magit-mode-map
                   magit-cherry-mode-map
                   ;; git-commit-mode-map
                   magit-mode-map
                   magit-blob-mode-map
                   magit-diff-mode-map
                   magit-log-mode-map
                   magit-log-select-mode-map
                   magit-reflog-mode-map
                   magit-status-mode-map
                   magit-file-mode-map
                   magit-log-read-revs-map
                   ;; magit-minibuffer-local-ns-map
                   ;; magit-popup-mode-map
                   magit-process-mode-map
                   magit-refs-mode-map))
  (evil-make-overriding-map map evil-magit-state))

(evil-make-overriding-map magit-blame-mode-map 'normal)

(evil-define-key evil-magit-state magit-mode-map
  "g"        nil
  "gj"       'magit-section-forward          ; was n
  "\C-j"     'evil-next-visual-line
  "]"        'magit-section-forward-sibling  ; was M-n
  "gk"       'magit-section-backward         ; was p
  "\C-k"     'evil-previous-visual-line
  "["        'magit-section-backward-sibling ; was M-p
  "gr"       'magit-refresh                  ; was on g
  "gR"       'magit-refresh-all              ; was on G
  "x"        'magit-delete-thing             ; was on k
  "X"        'magit-file-untrack             ; was on K
  "o"        'magit-revert-no-commit         ; was v
  "O"        'magit-revert-popup             ; was V
  "\C-r"     'magit-reset                    ; was on x
  "|"        'magit-git-command              ; was :
  "\C-o"     'magit-submodule-popup          ; was o
  ;; evil-specific bindings
  "j"        'evil-next-visual-line
  "k"        'evil-previous-visual-line
  "v"        'set-mark-command
  "V"        'set-mark-command
  "gg"       'evil-goto-first-line
  "G"        'evil-goto-line
  "\C-d"     'evil-scroll-down
  "\C-f"     'evil-scroll-page-down
  "\C-b"     'evil-scroll-page-up
  ":"        'evil-ex
  "/"        'evil-search-forward
  "n"        'evil-search-next
  "N"        'evil-search-previous
  "\C-z"     'evil-emacs-state
  [escape]   'evil-magit-maybe-deactivate-mark)

(when evil-want-C-u-scroll
  (evil-define-key evil-magit-state magit-mode-map "\C-u" 'evil-scroll-up))

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

(define-key magit-popup-mode-map "gr" 'magit-refresh)

(defvar evil-magit-popup-keys-changed nil)
(unless evil-magit-popup-keys-changed
  (magit-change-popup-key 'magit-branch-popup :actions ?x ?\C-r)
  (magit-change-popup-key 'magit-branch-popup :actions ?k ?x)
  (magit-change-popup-key 'magit-remote-popup :actions ?k ?x)
  (magit-change-popup-key 'magit-revert-popup :actions ?v ?o)
  (magit-change-popup-key 'magit-revert-popup :actions ?V ?O)
  (magit-change-popup-key 'magit-tag-popup    :actions ?k ?x)
  (eval-after-load 'magit-gh-pulls
    `(progn
       (magit-change-popup-key 'magit-gh-pulls-popup :actions ?g ?r)))
  (setq evil-magit-popup-keys-changed t))

(evil-define-key evil-magit-state magit-status-mode-map
  "gz" 'magit-jump-to-stashes
  "gt" 'magit-jump-to-tracked
  "gn" 'magit-jump-to-untracked
  "gu" 'magit-jump-to-unstaged
  "gs" 'magit-jump-to-staged
  "gf" 'magit-jump-to-unpulled
  "gp" 'magit-jump-to-unpushed)

(evil-define-key evil-magit-state magit-blob-mode-map
  "gj" 'magit-blob-next
  "gk" 'magit-blob-previous)

(evil-define-key evil-magit-state magit-diff-mode-map
  "gj"  'magit-section-forward
  "gd"  'magit-jump-to-diffstat-or-diff)

(evil-define-key 'normal magit-blame-mode-map
  "gj"    'magit-blame-next-chunk        ; was n
  "j"     'evil-next-visual-line
  "gJ"    'magit-blame-next-chunk-same-commit ; was N
  "gk"    'magit-blame-previous-chunk         ; was p
  "k"     'evil-previous-visual-line
  "gK"    'magit-blame-previous-chunk-same-commit) ; was P
(add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)

(eval-after-load 'git-rebase
  `(progn
     (evil-make-overriding-map git-rebase-mode-map evil-magit-state)
     (evil-define-key evil-magit-state git-rebase-mode-map
       "!"  'git-rebase-exec            ; was x
       "p"  'git-rebase-pick            ; was c
       "x"  'git-rebase-kill-line       ; was k or C-k
       "d"  'git-rebase-kill-line       ; was k or C-k
       "k"  'evil-previous-visual-line  ; was p
       "j"  'evil-next-visual-line      ; was n
       "K"  'git-rebase-move-line-up    ; was M-p
       "J"  'git-rebase-move-line-down  ; was M-n
       "u"  'git-rebase-undo)))

;; (defun evil-magit-remove-evil-state ()
;;   "Remove evil-state annotations from key bindings in Commands
;; section of rebase buffer."
;;   (let ((inhibit-read-only t))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (save-match-data
;;         (when (re-search-forward "^# Commands:\n" nil t)
;;           (--each '((" c," . "c,")
;;                     (" p"  . "p")
;;                     (" r," . "r,")
;;                     (" w, " . "")
;;                     (" e, " . "")
;;                     (" s"  . "s")
;;                     (" f, " . "")
;;                     (" !, " . ""))
;;             (while (re-search-forward
;;                     (format "<%s-state>%s" evil-magit-state (car it)) nil t)
;;               (replace-match (cdr it)))))))))
;; (add-hook 'git-rebase-mode-hook 'evil-magit-remove-evil-state t)

(evil-define-key evil-magit-state git-commit-mode-map
  "gk" 'git-commit-prev-message
  "gj" 'git-commit-next-message)

;; section maps: evil-define-key doesn't work here, because these maps are text overlays


(evil-magit-define-key magit-commit-section-map "v" 'set-mark-command 'magit-revert-no-commit)
(evil-magit-define-key magit-commit-section-map "o" 'magit-revert-no-commit 'magit-submodule-popup)

(evil-magit-define-key magit-file-section-map "K"   'magit-section-backward-sibling 'magit-file-untrack)
(evil-magit-define-key magit-file-section-map "X"   'magit-file-untrack) ; was K
(evil-magit-define-key magit-file-section-map "v"   'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-file-section-map "o"   'magit-reverse 'magit-submodule-popup) ; was v
(evil-magit-define-key magit-file-section-map "j"   'evil-next-visual-line 'magit-diff-visit-file-worktree)

(evil-magit-define-key magit-hunk-section-map "v"   'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-hunk-section-map "o"   'magit-reverse 'magit-submodule-popup) ; was v
(evil-magit-define-key magit-hunk-section-map "j"   'evil-next-visual-line 'magit-diff-visit-file-worktree)

(evil-magit-define-key magit-staged-section-map "v" 'set-mark-command 'magit-reverse)
(evil-magit-define-key magit-staged-section-map "o" 'magit-reverse 'magit-submodule-popup) ; was v

(evil-define-key evil-magit-state magit-file-section-map "K" 'magit-section-backward-sibling)

;;; evil-magit.el ends soon
(provide 'evil-magit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-magit.el ends here
