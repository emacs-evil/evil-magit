;;; evil-magit.el --- evil-based key bindings for magit

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Package-Requires: ((evil "1.2.3") (magit "2.2.2"))
;; Homepage: https://github.com/justbur/evil-magit

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
;; other. For some background see https://github.com/magit/evil-magit/issues/1.

;; Installation and Use
;; ====================

;; Everything is contained in evil-magit.el, so you may download and load that file
;; directly. The recommended method is to use MELPA via package.el (`M-x
;; package-install RET evil-magit RET`).

;; Evil and Magit are both required. After requiring those packages, the following
;; will setup the new key bindings for you.

;; ;; optional: this is the evil state that evil-magit will use
;; ;; (setq evil-magit-state 'motion)
;; (require 'evil-magit)

;; Use `evil-magit-revert` to revert changes made by evil-magit to the default
;; evil+magit behavior.

;; Key Bindings
;; ============

;; The basic key binding scheme is described in the following tables.

;;    Category             | Default | Evil-Magit
;;   ----------------------|---------|-----------
;;    cherry pick          | a/A     |
;;    branch               | b       |
;;    bisect               | B       |
;;    commit               | c       |
;;    diff                 | d/D     |
;;    help                 | h/?     |
;;    ediff                | e/E     |
;;    fetch                | f       |
;;    pull                 | F       |
;;    ignore               | i/I     |
;;    jump                 | j       | g
;;    delete               | k       | x
;;    untrack              | K       | X
;;    log                  | l/L     |
;;    merge                | m       |
;;    remote               | M       |
;;    next section         | n       | C-j
;;    next section sibling | M-n     | gj or ]
;;    submodule            | o       | >
;;    prev section         | p       | C-k
;;    prev section sibling | M-p     | gk or [
;;    push                 | P       |
;;    rebase               | r       |
;;    refresh              | g       | gr/gR
;;    rename               | R       |
;;    stage                | s/S     |
;;    tag                  | t       |
;;    notes                | T       |
;;    unstage              | u/U     |
;;    revert               | v/V     | o/O
;;    am                   | w       |
;;    patch                | W       |
;;    reset                | x       | C-r
;;    show-refs            | y       |
;;    cherry               | Y       |
;;    stash                | z/Z     |
;;    git-cmd              | :       | |
;;    run                  | !       |

;; New Commands
;; ------------

;;    Command                     | Evil-Magit
;;   -----------------------------|-----------
;;    evil-goto-line              | G
;;    evil-next-visual-line       | j
;;    evil-previous-visual-line   | k
;;    evil-search-next            | n
;;    evil-search-previous        | N
;;    set-mark-command            | v or V
;;    evil-ex                     | :
;;    evil-search-forward         | /
;;    evil-scroll-page-up         | C-b
;;    evil-scroll-down            | C-d
;;    evil-scroll-page-down       | C-f
;;    evil-scroll-up              | C-u (if C-u scrolls)
;;    evil-emacs-state            | C-z

;; Any other bindings are meant to be consistent with these.

;; Disclaimer
;; ==========

;; Given the complexity of magit key bindings combined with the complexity of git
;; itself, it is possible that there are some rough edges where the current binding
;; is not the expected one in a buffer. It will be very helpful for you to report
;; any such instances.

;;; Code:

(require 'evil)
(require 'magit)

(defcustom evil-magit-state 'motion
  "State to use for most magit buffers."
  :group 'magit
  :type  'symbol)

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

(defvaralias 'evil-magit-evil-state-modes-var
  (intern (format "evil-%s-state-modes" evil-magit-state)))

(defvar evil-magit-emacs-to-default-state-modes
  '(git-commit-mode)
  "Modes that should be in the default evil state")

(defvar evil-magit-emacs-to-evil-magit-state-modes
  '(magit-mode
    magit-cherry-mode
    magit-diff-mode
    magit-log-mode
    magit-log-select-mode
    magit-process-mode
    magit-reflog-mode
    magit-refs-mode
    magit-revision-mode
    magit-stash-mode
    magit-stashes-mode
    magit-status-mode)
  "Modes that switch from emacs state to `evil-magit-state'")

(defvar evil-magit-default-to-evil-magit-state-modes
  '(git-rebase-mode
    magit-blob-mode
    magit-file-mode
    magit-gitflow-mode)
  "Modes that switch from default state to `evil-magit-state'")

(defvar evil-magit-untouched-modes
  '(git-popup-mode
    magit-blame-mode
    magit-popup-mode
    magit-popup-sequence-mode)
  "Modes whose evil states are unchanged")

(defun evil-magit-adjust-default-states ()
  ;;remove from evil-emacs-state-modes
  (dolist (mode-list (list evil-magit-emacs-to-evil-magit-state-modes
                           evil-magit-emacs-to-default-state-modes))
    (dolist (mode mode-list)
      (setq evil-emacs-state-modes (delq mode evil-emacs-state-modes))))
  ;;add to evil-magit-evil-state-modes-var
  (dolist (mode-list (list evil-magit-emacs-to-evil-magit-state-modes
                           evil-magit-default-to-evil-magit-state-modes))
    (dolist (mode mode-list)
      (add-to-list 'evil-magit-evil-state-modes-var mode))))

(defun evil-magit-revert-default-states ()
  "Revert git/magit modes to evil state before loading
evil-magit."
  (dolist (mode-list (list evil-magit-emacs-to-evil-magit-state-modes
                           evil-magit-emacs-to-default-state-modes))
    (dolist (mode mode-list)
      (add-to-list 'evil-emacs-state-modes mode)))
  (dolist (mode-list (list evil-magit-emacs-to-evil-magit-state-modes
                           evil-magit-default-to-evil-magit-state-modes))
    (dolist (mode mode-list)
      (setq evil-magit-evil-state-modes-var
            (delq mode evil-magit-evil-state-modes-var)))))

;; Make relevant maps into overriding maps so that they shadow the global evil
;; maps by default
(dolist (map (list magit-mode-map
                   magit-cherry-mode-map
                   magit-mode-map
                   magit-blob-mode-map
                   magit-diff-mode-map
                   magit-log-mode-map
                   magit-log-select-mode-map
                   magit-reflog-mode-map
                   magit-status-mode-map
                   magit-file-mode-map
                   magit-log-read-revs-map
                   magit-process-mode-map
                   magit-refs-mode-map))
  (evil-make-overriding-map map evil-magit-state))

(evil-make-overriding-map magit-blame-mode-map 'normal)

(eval-after-load 'magit-gh-pulls
  `(evil-make-overriding-map magit-gh-pulls-mode-map ',evil-magit-state))

(evil-define-key evil-magit-state magit-mode-map
  "g"        nil
  "\C-j"     'magit-section-forward          ; was n
  "gj"       'magit-section-forward-sibling  ; was M-n
  "]"        'magit-section-forward-sibling  ; was M-n
  "\C-k"     'magit-section-backward         ; was p
  "gk"       'magit-section-backward-sibling ; was M-p
  "["        'magit-section-backward-sibling ; was M-p
  "gr"       'magit-refresh                  ; was g
  "gR"       'magit-refresh-all              ; was G
  "x"        'magit-delete-thing             ; was k
  "X"        'magit-file-untrack             ; was K
  "o"        'magit-revert-no-commit         ; was v
  "O"        'magit-revert-popup             ; was V
  "\C-r"     'magit-reset                    ; was x
  "|"        'magit-git-command              ; was :
  ">"        'magit-submodule-popup          ; was o
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
  "j"     'evil-next-visual-line
  "\C-j"  'magit-blame-next-chunk                  ; was n
  "gj"    'magit-blame-next-chunk                  ; was n
  "gJ"    'magit-blame-next-chunk-same-commit      ; was N
  "k"     'evil-previous-visual-line
  "\C-k"  'magit-blame-previous-chunk              ; was p
  "gk"    'magit-blame-previous-chunk              ; was p
  "gK"    'magit-blame-previous-chunk-same-commit) ; was P
(add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)

(eval-after-load 'git-rebase
  `(progn
     (evil-make-overriding-map git-rebase-mode-map evil-magit-state)
     (defvar evil-magit-rebase-commands-w-descriptions
       ;; nil in the first element means don't bind here
       '(("p"    git-rebase-pick           "pick = use commit")
         ("r"    git-rebase-reword         "reword = use commit, but edit the commit message")
         ("e"    git-rebase-edit           "edit = use commit, but stop for amending")
         ("s"    git-rebase-squash         "squash = use commit, but meld into previous commit")
         ("f"    git-rebase-fixup          "fixup = like \"squash\", but discard this commit's log message")
         ("x"    git-rebase-exec           "exec = run command (the rest of the line) using shell")
         ("d"    git-rebase-kill-line      "drop = remove commit") ; was k (still can use C-k)
         ("u"    git-rebase-undo           "undo last change")
         (nil    with-editor-finish        "tell Git to make it happen")
         (nil    with-editor-cancel        "tell Git that you changed your mind, i.e. abort")
         ("k"    evil-previous-visual-line "move point to previous line") ; was p
         ("j"    evil-next-visual-line     "move point to next line") ; was n
         ("M-k"  git-rebase-move-line-up   "move the commit at point up") ; was M-p
         ("M-j"  git-rebase-move-line-down "move the commit at point down") ; was M-n
         (nil    git-rebase-show-commit    "show the commit at point in another buffer")))

     (dolist (cmd evil-magit-rebase-commands-w-descriptions)
       (when (car cmd)
         (evil-define-key evil-magit-state git-rebase-mode-map
           (kbd (car cmd)) (nth 1 cmd))))

     (defun evil-magit-add-rebase-messages ()
       "Remove evil-state annotations and reformat git-rebase buffer."
       (goto-char (point-min))
       (let ((inhibit-read-only t)
             (state-regexp (format "<%s-state> " evil-magit-state))
             (aux-map (evil-get-auxiliary-keymap git-rebase-mode-map evil-magit-state)))
         (save-excursion
           (save-match-data
             (flush-lines "^#.+ = ")
             (goto-char (point-min))
             (when (and git-rebase-show-instructions
                        (re-search-forward "^# Commands:\n" nil t))
               (dolist (cmd evil-magit-rebase-commands-w-descriptions)
                 (insert
                  (format "# %-8s %s\n"
                          (if (and (car cmd)
                                   (eq (nth 1 cmd)
                                       (lookup-key aux-map (car cmd))))
                              (car cmd)
                            (replace-regexp-in-string
                             state-regexp ""
                             (substitute-command-keys
                              (format "\\[%s]" (nth 1 cmd)))))
                          (nth 2 cmd)))))))))
     (remove-hook 'git-rebase-mode-hook 'git-rebase-mode-show-keybindings)
     (add-hook 'git-rebase-mode-hook 'evil-magit-add-rebase-messages t)))

(evil-define-key evil-magit-state git-commit-mode-map
  "gk" 'git-commit-prev-message
  "gj" 'git-commit-next-message)

;; section maps: evil-define-key doesn't work here, because these maps are text overlays

;; These should all have no effect on functionality
(define-key magit-file-section-map "v" nil) ; was magit-reverse
(define-key magit-file-section-map [remap magit-revert-no-commit] 'magit-reverse)

(define-key magit-hunk-section-map "v" nil) ; was magit-reverse
(define-key magit-hunk-section-map [remap magit-revert-no-commit] 'magit-reverse)

(define-key magit-staged-section-map "v" nil) ; was magit-reverse
(define-key magit-staged-section-map [remap magit-revert-no-commit] 'magit-reverse)

(defun evil-magit-adjust-section-bindings ()
  "Revert changed bindings in section maps generated by evil-magit"
  ;; Not sure about this, but it seems unnecessary
  (define-key magit-commit-section-map "v" nil)   ; was magit-revert-no-commit
  (define-key magit-file-section-map "\C-j" nil)  ; breaking change
  (define-key magit-hunk-section-map "\C-j" nil)) ; breaking change

(defun evil-magit-revert-section-bindings ()
  "Revert changed bindings in section maps generated by evil-magit"
  (define-key magit-commit-section-map "v" 'magit-revert-no-commit)
  (define-key magit-file-section-map "\C-j" 'magit-diff-visit-file-worktree)
  (define-key magit-hunk-section-map "\C-j" 'magit-diff-visit-file-worktree))

;; Popups
(defvar evil-magit-dispatch-popup-backup (copy-sequence magit-dispatch-popup))
(defvar evil-magit-popup-keys-changed nil)

(defun evil-magit-adjust-popups ()
  "Adjust popup keys to match evil-magit."
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
                        (?P "Pushing"         magit-push-popup)
                        (?r "Rebasing"        magit-rebase-popup)
                        (?t "Tagging"         magit-tag-popup)
                        (?T "Notes"           magit-notes-popup)
                        (?w "Apply patches"   magit-am-popup)
                        (?W "Format patches"  magit-patch-popup)
                        (?y "Show Refs"       magit-show-refs-popup)
                        (?z "Stashing"        magit-stash-popup)
                        (?! "Running"         magit-run-popup)
                        (?> "Submodules"      magit-submodule-popup)
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
    (setq evil-magit-popup-keys-changed t)))

(defun evil-magit-revert-popups ()
  "Revert popup keys changed by evil-magit."
  (setq magit-dispatch-popup evil-magit-dispatch-popup-backup)
  (when evil-magit-popup-keys-changed
    (magit-change-popup-key 'magit-branch-popup :actions ?\C-r ?x)
    (magit-change-popup-key 'magit-branch-popup :actions ?x ?k)
    (magit-change-popup-key 'magit-remote-popup :actions ?x ?k)
    (magit-change-popup-key 'magit-revert-popup :actions ?o ?v)
    (magit-change-popup-key 'magit-revert-popup :actions ?O ?V)
    (magit-change-popup-key 'magit-tag-popup    :actions ?x ?k)
    (eval-after-load 'magit-gh-pulls
      `(progn
         (magit-change-popup-key 'magit-gh-pulls-popup :actions ?r ?g)))
    (setq evil-magit-popup-keys-changed nil)))

;;;###autoload
(defun evil-magit-init ()
  "This function completes the setup of evil-magit. It is called
automatically when evil-magit is loaded. The only reason to use
this function is if you've called `evil-magit-revert' and wish to
go back to evil-magit behavior."
  (interactive)
  (evil-magit-adjust-section-bindings)
  (evil-magit-adjust-popups)
  (evil-magit-adjust-default-states))
(evil-magit-init)

;;;###autoload
(defun evil-magit-revert ()
  "Revert changes by evil-magit that affect default evil+magit behavior."
  (interactive)
  (evil-magit-revert-section-bindings)
  (evil-magit-revert-popups)
  (evil-magit-revert-default-states))

;;; evil-magit.el ends soon
(provide 'evil-magit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-magit.el ends here
