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
(eval-when-compile (defvar git-rebase-mode-map))

(defvar evil-magit-evil-bindings '()
  "Holds the new evil-magit bindings.")
(defvar evil-magit-original-bindings '()
  "Holds any original magit bindings that were overwritten.")
(defvar evil-magit-original-popups '()
  "Backup of original popups.")

(defun evil-magit-define-key (map key def &rest bindings)
  "Save new and old bindings then use `define-key'."
  (let (temp)
    ;; process keys before binding
    (while key
      (when (lookup-key map key)
        (push (list map key (lookup-key map key)) evil-magit-original-bindings))
      (push (cons key def) temp)
      (setq key (pop bindings)
            def (pop bindings)))
    (dolist (key-def (nreverse temp))
      (push (list map key def) evil-magit-evil-bindings)
      (let ((key-lst (listify-key-sequence (car key-def))))
        (when (< 1 (length key-lst))
          (define-key map (char-to-string (car key-lst)) nil)))
      (define-key map (car key-def) (cdr key-def)))))
(put 'evil-magit-define-key 'lisp-indent-function 'defun)

(defun evil-magit-revert-evil-bindings ()
  "Revert bindings created by `evil-magit-setup-evil-bindings'."
  (interactive)
  (dolist (map-key-def (nreverse evil-magit-original-bindings))
    (define-key (car map-key-def) (cadr map-key-def) (caddr map-key-def)))
  (dolist (popup evil-magit-original-popups)
    (setf (car popup) (cdr popup)))
  (setq evil-magit-evil-bindings nil
        evil-magit-original-bindings nil
        evil-magit-original-popups nil))

(defun evil-magit-backup-popup (popup)
  (let ((popup-name popup)
        (popup-copy (eval popup)))
    (push (cons popup-name popup-copy) evil-magit-original-popups)))

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

(push 'git-rebase-mode evil-emacs-state-modes)

;;;###autoload
(defun evil-magit-setup-evil-bindings ()
  "Setup evil-magit bindings. Run `evil-magit-revert-evil-bindings' to
reverse this operation."
  (interactive)

  (evil-magit-define-key magit-mode-map
    "j"        'magit-section-forward          ; was n
    "\C-j"     'evil-next-visual-line
    "gj"       'magit-section-forward-sibling  ; was M-n
    "]"       'magit-section-forward-sibling   ; was M-n
    "k"        'magit-section-backward         ; was p
    "\C-k"     'evil-previous-visual-line
    "gk"       'magit-section-backward-sibling ; was M-p
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
    "v"        'set-mark-command
    "V"        'set-mark-command
    "gg"       'evil-goto-first-line
    "G"        'evil-goto-line
    ;; "\C-d"     'evil-scroll-down
    "\C-d"     'magit-section-forward-sibling  ; was M-n
    "\C-f"     'evil-scroll-page-down
    "\C-b"     'evil-scroll-page-up
    ":"        'evil-ex
    "/"        'evil-search-forward
    "n"        'evil-search-next
    "N"        'evil-search-previous
    "\C-z"     'evil-emacs-state
    [escape]   'evil-magit-maybe-deactivate-mark)

  (when evil-want-C-u-scroll
    ;; (evil-define-key evil-magit-state map "\C-u" 'evil-scroll-up)
    (evil-magit-define-key magit-mode-map "\C-u" 'magit-section-backward-sibling)) ; was M-p

  (evil-magit-define-key magit-popup-mode-map "gr" 'magit-refresh)

  (defvar evil-magit-popup-keys-changed nil)
  (unless evil-magit-popup-keys-changed
    (evil-magit-backup-popup 'magit-dispatch-popup)
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
    (evil-magit-backup-popup 'magit-branch-popup)
    (evil-magit-backup-popup 'magit-remote-popup)
    (evil-magit-backup-popup 'magit-revert-popup)
    (evil-magit-backup-popup 'magit-tag-popup)
    (magit-change-popup-key  'magit-branch-popup :actions ?x ?\C-r)
    (magit-change-popup-key  'magit-branch-popup :actions ?k ?x)
    (magit-change-popup-key  'magit-remote-popup :actions ?k ?x)
    (magit-change-popup-key  'magit-revert-popup :actions ?v ?o)
    (magit-change-popup-key  'magit-revert-popup :actions ?V ?O)
    (magit-change-popup-key  'magit-tag-popup    :actions ?k ?x)
    (setq evil-magit-popup-keys-changed t))

  (evil-magit-define-key magit-status-mode-map
    "gz" 'magit-jump-to-stashes
    "gt" 'magit-jump-to-tracked
    "gn" 'magit-jump-to-untracked
    "gu" 'magit-jump-to-unstaged
    "gs" 'magit-jump-to-staged
    "gf" 'magit-jump-to-unpulled
    "gp" 'magit-jump-to-unpushed
    ;; the purpose of this is to remove the other j bindings
    "j"  'magit-section-forward)

  (evil-magit-define-key magit-blob-mode-map
    "j" 'magit-blob-next
    "k" 'magit-blob-previous)

  (evil-magit-define-key magit-diff-mode-map
    "j"  'magit-section-forward
    "gd" 'magit-jump-to-diffstat-or-diff)

  (evil-magit-define-key magit-blame-mode-map
    "j"    'magit-blame-next-chunk        ; was n
    "\C-j" 'evil-next-visual-line
    "J"    'magit-blame-next-chunk-same-commit ; was N
    "k"    'magit-blame-previous-chunk         ; was p
    "\C-k" 'evil-previous-visual-line
    "K"    'magit-blame-previous-chunk-same-commit) ; was P
  (add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)

  (eval-after-load 'git-rebase
    `(progn
       (evil-magit-define-key git-rebase-mode-map
         "!"  'git-rebase-exec            ; was x
         "p"  'git-rebase-pick            ; was c
         "x"  'git-rebase-kill-line       ; was k or C-k
         "d"  'git-rebase-kill-line       ; was k or C-k
         "k"  'git-rebase-backward-line   ; was p
         "j"  'forward-line               ; was n
         "K"  'git-rebase-move-line-up    ; was M-p
         "J"  'git-rebase-move-line-down  ; was M-n
         "u"  'git-rebase-undo)))

  (evil-magit-define-key git-commit-mode-map
    "\M-k" 'git-commit-prev-message
    "\M-j" 'git-commit-next-message)

  ;; section maps
  (evil-magit-define-key magit-commit-section-map
    "v" 'set-mark-command
    "o" 'magit-revert-no-commit)

  (evil-magit-define-key magit-file-section-map
    "K"    'magit-section-backward-sibling
    "X"    'magit-file-untrack ; was K
    "v"    'set-mark-command
    "o"    'magit-reverse ; was v
    "\C-j" 'evil-next-visual-line)

  (evil-magit-define-key magit-hunk-section-map
    "v"    'set-mark-command
    "o"    'magit-reverse ; was v
    "\C-j" 'evil-next-visual-line)

  (evil-magit-define-key magit-staged-section-map
    "v" 'set-mark-command
    "o" 'magit-reverse)) ; was v

;;; evil-magit.el ends soon
(provide 'evil-magit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-magit.el ends here
