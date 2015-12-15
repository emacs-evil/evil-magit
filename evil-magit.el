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
;;    reset                | x       | C-r (X in popups)
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

(defcustom evil-magit-use-y-for-yank nil
  "When non nil, replace \"y\" for `magit-show-refs-popup' with
\"yy\" for `evil-yank-line', `ys' `magit-copy-section-value',
\"yb\" for `magit-copy-buffer-revision' and \"yr\" for
`magit-show-refs-popup'. This keeps \"y\" for
`magit-show-refs-popup' in the help
popup (`magit-dispatch-popup')."
  :group 'magit
  :type 'boolean)

(defcustom evil-magit-state (if evil-magit-use-y-for-yank 'normal 'motion)
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

(defun evil-magit-define-key (state map-sym key def)
  "Version of `evil-define-key' without the `evil-delay' stuff.
All of the keymaps should be initialized, so there is no reason
to delay setting the key, but we check that MAP-SYM is actually
keymap anyway. Also it's not a macro like `evil-define-key'."
  (if (and (boundp map-sym)
           (keymapp (symbol-value map-sym)))
      (define-key
        (evil-get-auxiliary-keymap (symbol-value map-sym) state t)
        key def)
    (message "evil-magit: Error the keymap %s is not bound. Note evil-magit assumes\
 you have the latest version of magit." map-sym)))

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

(defvar evil-magit-ignored-modes
  '(git-commit-major-mode
    magit-blame-put-keymap-before-view-mode
    magit-diff-mode
    magit-merge-preview-mode
    magit-popup-help-mode
    magit-rebase-mode
    magit-file-mode-major-mode
    magit-wip-after-save-mode
    magit-wip-after-save-local-mode-major-mode
    magit-wip-after-save-local-mode
    magit-wip-after-apply-mode
    magit-wip-before-change-mode
    ;; git-gutter
    git-gutter-mode
    git-gutter-mode-major-mode
    git-gutter+-commit-mode
    git-gutter+-mode
    git-gutter+-enable-fringe-display-mode
    git-gutter+-enable-default-display-mode)
  "Currently ignored modes. They are collected here for testing
purposes.")

(defun evil-magit-set-initial-states ()
  "Set the initial state for relevant modes."
  (dolist (mode (append evil-magit-emacs-to-evil-magit-state-modes
                        evil-magit-default-to-evil-magit-state-modes))
    (evil-set-initial-state mode evil-magit-state))
  (dolist (mode evil-magit-emacs-to-default-state-modes)
    (evil-set-initial-state mode evil-default-state)))

(defun evil-magit-revert-initial-states ()
  "Revert the initial state for modes to their values before
evil-magit was loaded."
  (dolist (mode (append evil-magit-emacs-to-evil-magit-state-modes
                        evil-magit-emacs-to-default-state-modes))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode evil-magit-default-to-evil-magit-state-modes)
    (evil-set-initial-state mode evil-default-state)))

(defvar evil-magit-section-maps
  '(magit-tag-section-map
    magit-hunk-section-map
    magit-file-section-map
    magit-stash-section-map
    magit-staged-section-map
    magit-remote-section-map
    magit-commit-section-map
    magit-branch-section-map
    magit-stashes-section-map
    magit-unpulled-section-map
    magit-unstaged-section-map
    magit-unpushed-section-map
    magit-untracked-section-map
    magit-module-commit-section-map)
  "All magit section maps")

(when evil-magit-use-y-for-yank
  (dolist (map evil-magit-section-maps)
    (when (and map (keymapp (symbol-value map)))
      (map-keymap
       (lambda (_ def)
         (when (commandp def)
           (evil-set-command-property def :exclude-newline t)))
       (symbol-value map)))))

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

(defvar evil-magit-mode-map-bindings
  (append
   `((,evil-magit-state magit-mode-map "g")
     (,evil-magit-state magit-mode-map "\C-j"   magit-section-forward          "n")
     (,evil-magit-state magit-mode-map "gj"     magit-section-forward-sibling  "\M-n")
     (,evil-magit-state magit-mode-map "]"      magit-section-forward-sibling  "\M-n")
     (,evil-magit-state magit-mode-map "\C-k"   magit-section-backward         "p")
     (,evil-magit-state magit-mode-map "gk"     magit-section-backward-sibling "\M-p")
     (,evil-magit-state magit-mode-map "["      magit-section-backward-sibling "\M-p")
     (,evil-magit-state magit-mode-map "gr"     magit-refresh                  "g")
     (,evil-magit-state magit-mode-map "gR"     magit-refresh-all              "G")
     (,evil-magit-state magit-mode-map "x"      magit-delete-thing             "k")
     (,evil-magit-state magit-mode-map "X"      magit-file-untrack             "K")
     (,evil-magit-state magit-mode-map "o"      magit-revert-no-commit         "v")
     (,evil-magit-state magit-mode-map "O"      magit-revert-popup             "V")
     (,evil-magit-state magit-mode-map "\C-r"   magit-reset                    "x")
     (,evil-magit-state magit-mode-map "|"      magit-git-command              ":")
     (,evil-magit-state magit-mode-map ">"      magit-submodule-popup          "o")
     (,evil-magit-state magit-mode-map "j"      evil-next-visual-line)
     (,evil-magit-state magit-mode-map "k"      evil-previous-visual-line)
     (,evil-magit-state magit-mode-map "gg"     evil-goto-first-line)
     (,evil-magit-state magit-mode-map "G"      evil-goto-line)
     (,evil-magit-state magit-mode-map "\C-d"   evil-scroll-down)
     (,evil-magit-state magit-mode-map "\C-f"   evil-scroll-page-down)
     (,evil-magit-state magit-mode-map "\C-b"   evil-scroll-page-up)
     (,evil-magit-state magit-mode-map ":"      evil-ex)
     (,evil-magit-state magit-mode-map "/"      evil-search-forward)
     (,evil-magit-state magit-mode-map "n"      evil-search-next)
     (,evil-magit-state magit-mode-map "N"      evil-search-previous)
     (,evil-magit-state magit-mode-map "\C-z"   evil-emacs-state)

     (,evil-magit-state magit-status-mode-map "gz"  magit-jump-to-stashes                  "jz")
     (,evil-magit-state magit-status-mode-map "gt"  magit-jump-to-tracked                  "jt")
     (,evil-magit-state magit-status-mode-map "gn"  magit-jump-to-untracked                "jn")
     (,evil-magit-state magit-status-mode-map "gu"  magit-jump-to-unstaged                 "ju")
     (,evil-magit-state magit-status-mode-map "gs"  magit-jump-to-staged                   "js")
     (,evil-magit-state magit-status-mode-map "gfu" magit-jump-to-unpulled-from-upstream   "jfu")
     (,evil-magit-state magit-status-mode-map "gfp" magit-jump-to-unpulled-from-pushremote "jfp")
     (,evil-magit-state magit-status-mode-map "gpu" magit-jump-to-unpushed-to-upstream     "jpu")
     (,evil-magit-state magit-status-mode-map "gpp" magit-jump-to-unpushed-to-pushremote   "jpp")

     (,evil-magit-state magit-blob-mode-map "gj" magit-blob-next     "n")
     (,evil-magit-state magit-blob-mode-map "gk" magit-blob-previous "p")

     (,evil-magit-state magit-diff-mode-map "gj" magit-section-forward)
     (,evil-magit-state magit-diff-mode-map "gd" magit-jump-to-diffstat-or-diff "j")

     (normal magit-blame-mode-map "j"    evil-next-visual-line)
     (normal magit-blame-mode-map "\C-j" magit-blame-next-chunk                 "n")
     (normal magit-blame-mode-map "gj"   magit-blame-next-chunk                 "n")
     (normal magit-blame-mode-map "gJ"   magit-blame-next-chunk-same-commit     "N")
     (normal magit-blame-mode-map "k"    evil-previous-visual-line)
     (normal magit-blame-mode-map "\C-k" magit-blame-previous-chunk             "p")
     (normal magit-blame-mode-map "gk"   magit-blame-previous-chunk             "p")
     (normal magit-blame-mode-map "gK"   magit-blame-previous-chunk-same-commit "P")

     (,evil-magit-state git-commit-mode-map "gk" git-commit-prev-message "\M-p")
     (,evil-magit-state git-commit-mode-map "gj" git-commit-next-message "\M-n"))

   (when evil-want-C-u-scroll
     `((,evil-magit-state magit-mode-map "\C-u" evil-scroll-up)))

   (if evil-magit-use-y-for-yank
       `((,evil-magit-state magit-mode-map "v" evil-visual-line)
         (,evil-magit-state magit-mode-map "V" evil-visual-line))
     `((,evil-magit-state magit-mode-map "v" set-mark-command)
       (,evil-magit-state magit-mode-map "V" set-mark-command)
       (,evil-magit-state magit-mode-map [escape] evil-magit-maybe-deactivate-mark)))

   (when evil-magit-use-y-for-yank
     `((,evil-magit-state magit-mode-map "y")
       (,evil-magit-state magit-mode-map "yy" evil-yank-line)
       (,evil-magit-state magit-mode-map "yr" magit-show-refs-popup      "y")
       (,evil-magit-state magit-mode-map "ys" magit-copy-section-value   "\C-w")
       (,evil-magit-state magit-mode-map "yb" magit-copy-buffer-revision "\M-w"))))
  "All evil-magit bindings not in a section map. Each element of
this list takes the form

\(EVIL-STATE MAGIT-MAP NEW-KEY DEF ORIG-KEY)\.

ORIG-KEY is only used for testing purposes, and
denotes the original magit key for this command.")

(dolist (binding evil-magit-mode-map-bindings)
  (when binding
    (evil-magit-define-key
     (nth 0 binding) (nth 1 binding) (nth 2 binding) (nth 3 binding))))

;; Need to refresh evil keymaps when blame mode is entered.
(add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)

(eval-after-load 'git-rebase
  `(progn
     ;; for the compiler
     (defvar git-rebase-mode-map)
     (evil-make-overriding-map git-rebase-mode-map evil-magit-state)
     (defvar evil-magit-rebase-commands-w-descriptions
       ;; nil in the first element means don't bind here
       '(("p"    git-rebase-pick           "pick = use commit")
         ("r"    git-rebase-reword         "reword = use commit, but edit the commit message")
         ("e"    git-rebase-edit           "edit = use commit, but stop for amending")
         ("s"    git-rebase-squash         "squash = use commit, but meld into previous commit")
         ("f"    git-rebase-fixup          "fixup = like \"squash\", but discard this commit's log message")
         ("x"    git-rebase-exec           "exec = run command (the rest of the line) using shell")
         ("d"    git-rebase-kill-line      "drop = remove commit" "k")
         ("u"    git-rebase-undo           "undo last change")
         (nil    with-editor-finish        "tell Git to make it happen")
         (nil    with-editor-cancel        "tell Git that you changed your mind, i.e. abort")
         ("k"    evil-previous-visual-line "move point to previous line" "p")
         ("j"    evil-next-visual-line     "move point to next line" "n")
         ("M-k"  git-rebase-move-line-up   "move the commit at point up" "\M-p")
         ("M-j"  git-rebase-move-line-down "move the commit at point down" "\M-n")
         (nil    git-rebase-show-commit    "show the commit at point in another buffer")))

     (dolist (cmd evil-magit-rebase-commands-w-descriptions)
       (when (car cmd)
         (evil-magit-define-key evil-magit-state 'git-rebase-mode-map
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
             (when (and (boundp 'git-rebase-show-instructions)
                        git-rebase-show-instructions
                        (re-search-forward "^# Commands:\n" nil t))
               (dolist (cmd evil-magit-rebase-commands-w-descriptions)
                 (insert
                  (format "# %-8s %s\n"
                          (if (and (car cmd)
                                   (eq (nth 1 cmd)
                                       (lookup-key aux-map (kbd (car cmd)))))
                              (car cmd)
                            (replace-regexp-in-string
                             state-regexp ""
                             (substitute-command-keys
                              (format "\\[%s]" (nth 1 cmd)))))
                          (nth 2 cmd)))))))))
     (remove-hook 'git-rebase-mode-hook 'git-rebase-mode-show-keybindings)
     (add-hook 'git-rebase-mode-hook 'evil-magit-add-rebase-messages t)))

;; section maps: evil's auxiliary maps don't work here, because these maps are
;; text overlays

(defvar evil-magit-original-section-bindings
  `((,(copy-keymap magit-file-section-map) "\C-j" magit-diff-visit-file-worktree)
    (,(copy-keymap magit-hunk-section-map) "\C-j" magit-diff-visit-file-worktree))
  "For testing purposes only. The original magit keybindings that
evil-magit affects.")

(defun evil-magit-adjust-section-bindings ()
  "Revert changed bindings in section maps generated by evil-magit"
  (define-key magit-file-section-map "\C-j" nil)  ; breaking change
  (define-key magit-hunk-section-map "\C-j" nil)) ; breaking change

(defun evil-magit-revert-section-bindings ()
  "Revert changed bindings in section maps generated by evil-magit"
  (define-key magit-file-section-map "\C-j" 'magit-diff-visit-file-worktree)
  (define-key magit-hunk-section-map "\C-j" 'magit-diff-visit-file-worktree))

;; Popups

(defvar evil-magit-dispatch-popup-backup (copy-sequence magit-dispatch-popup))
(defvar evil-magit-popup-keys-changed nil)

(defvar evil-magit-popup-changes
  '((magit-branch-popup :actions "x" "X" magit-branch-reset)
    (magit-branch-popup :actions "k" "x" magit-branch-delete)
    (magit-remote-popup :actions "k" "x" magit-remote-remove)
    (magit-revert-popup :actions "v" "o" magit-revert-no-commit)
    (magit-revert-popup :actions "V" "O" magit-revert)
    (magit-revert-popup :sequence-actions "V" "O" magit-sequencer-continue)
    (magit-tag-popup    :actions "k" "x" magit-tag-delete))
  "Changes to popup keys, excluding `magit-dispatch-popup'.")

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
    (dolist (change evil-magit-popup-changes)
      (magit-change-popup-key
       (nth 0 change) (nth 1 change)
       (string-to-char (nth 2 change)) (string-to-char (nth 3 change))))
    (eval-after-load 'magit-gh-pulls
      `(progn
         (magit-change-popup-key 'magit-gh-pulls-popup :actions ?g ?r)))
    (setq evil-magit-popup-keys-changed t)))

(defun evil-magit-revert-popups ()
  "Revert popup keys changed by evil-magit."
  (setq magit-dispatch-popup evil-magit-dispatch-popup-backup)
  (when evil-magit-popup-keys-changed
    (dolist (change evil-magit-popup-changes)
      (magit-change-popup-key
       (nth 0 change) (nth 1 change)
       (string-to-char (nth 3 change)) (string-to-char (nth 2 change))))
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
  (evil-magit-set-initial-states)
  (message "evil-magit initialized"))
(evil-magit-init)

;;;###autoload
(defun evil-magit-revert ()
  "Revert changes by evil-magit that affect default evil+magit behavior."
  (interactive)
  (evil-magit-revert-section-bindings)
  (evil-magit-revert-popups)
  (evil-magit-revert-initial-states)
  (message "evil-magit reverted"))

;;; evil-magit.el ends soon
(provide 'evil-magit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-magit.el ends here
