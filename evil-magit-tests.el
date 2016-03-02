;;; evil-magit-tests.el --- evil-based key bindings for magit

;; Copyright (C) 2015-2016 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
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

(require 'evil-magit)

(ert-deftest evil-magit-mode-map-tests ()
    "Test that original bindings in `evil-magit-mode-map-bindings'
are correct."
  (dolist (binding evil-magit-mode-map-bindings)
    (when (nth 4 binding)
      (should (eq (lookup-key (symbol-value (nth 1 binding)) (nth 4 binding))
                  (nth 3 binding)))))
  (dolist (binding evil-magit-minor-mode-map-bindings)
    (when (nth 4 binding)
      (should (eq (lookup-key (symbol-value (nth 1 binding)) (nth 4 binding))
                  (nth 3 binding))))))

(ert-deftest evil-magit-section-map-tests ()
  "Test that original bindings in
`evil-magit-original-section-bindings' are correct."
  (dolist (binding evil-magit-original-section-bindings)
    (should (eq (lookup-key (nth 0 binding) (nth 1 binding))
                (nth 2 binding)))))

(ert-deftest evil-magit-dispatch-popup-test ()
  "Test that the actions in `magit-dispatch-popup' are unchanged"
  (should (equal (plist-get evil-magit-dispatch-popup-backup :actions)
                 '("Popup and dwim commands"
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
                   (?L "Change logs"     magit-log-refresh-popup)
                   (?m "Merging"         magit-merge-popup)
                   (?M "Remoting"        magit-remote-popup)
                   (?o "Submodules"      magit-submodule-popup)
                   (?P "Pushing"         magit-push-popup)
                   (?r "Rebasing"        magit-rebase-popup)
                   (?t "Tagging"         magit-tag-popup)
                   (?T "Notes"           magit-notes-popup)
                   (?V "Reverting"       magit-revert-popup)
                   (?w "Apply patches"   magit-am-popup)
                   (?W "Format patches"  magit-patch-popup)
                   (?X "Resetting"       magit-reset-popup)
                   (?y "Show Refs"       magit-show-refs-popup)
                   (?z "Stashing"        magit-stash-popup)
                   (?! "Running"         magit-run-popup)
                   "Applying changes"
                   (?a "Apply"           magit-apply)
                   (?s "Stage"           magit-stage)
                   (?u "Unstage"         magit-unstage)
                   nil
                   (?v "Reverse"         magit-reverse)
                   (?S "Stage all"       magit-stage-modified)
                   (?U "Unstage all"     magit-unstage-all)
                   nil
                   (?k "Discard"         magit-discard)
                   "\
 g      refresh current buffer
 TAB    toggle section at point
 RET    visit thing at point

 C-h m  show all key bindings" nil))))

(ert-deftest evil-magit-popup-action-tests ()
  "Test that bindings are as expected in popups."
  (when evil-magit-popup-keys-changed
    (dolist (change evil-magit-popup-changes)
      (let ((alist (plist-get (symbol-value (nth 0 change)) (nth 1 change))))
        (should
         (eq (nth 2 (assoc (string-to-char (nth 3 change)) alist))
             (nth 4 change)))))))

(defun evil-magit-collect-magit-section-maps ()
  (let (res)
    (mapatoms
     (lambda (sym)
       (when (string-match-p "^magit-.*-section-map$" (symbol-name sym))
                     (push sym res))))
    res))

(setq evil-magit-section-maps-test (evil-magit-collect-magit-section-maps))
;; (setq evil-magit-commands-in-section-maps
;;       (let (res)
;;         (dolist (map evil-magit-section-maps-test)
;;           (when (and (boundp map) (keymapp (symbol-value map)))
;;             (map-keymap
;;              (lambda (_ def)
;;                (when (commandp def)
;;                  (if res
;;                      (add-to-list 'res def)
;;                    (setq res (list def)))))
;;              (symbol-value map))))
;;         res))

(ert-deftest evil-magit-section-maps-accounted-for ()
  "Check that `evil-magit-section-maps' includes all section-maps
we can find."
  (dolist (map evil-magit-section-maps-test)
    (when (and (boundp map) (keymapp (symbol-value map)))
      (should (memq map evil-magit-section-maps)))))

(defun evil-magit-collect-git-magit-modes ()
  (let (res)
    (mapatoms
     (lambda (sym)
       (when (and (or (boundp sym)
                      (fboundp sym))
                  (string-match-p "^\\(git\\|magit\\)-.*-mode$" (symbol-name sym)))
         (push sym res))))
    res))

(ert-deftest evil-magit-all-modes-accounted-for ()
  "Check that mode lists include all modes we can find."
  (let ((modes (evil-magit-collect-git-magit-modes))
        res)
    (dolist (mode modes)
      (when (boundp mode)
        (should (memq mode
                      (append
                       evil-magit-emacs-to-default-state-modes
                       evil-magit-emacs-to-evil-magit-state-modes
                       evil-magit-default-to-evil-magit-state-modes
                       evil-magit-untouched-modes
                       evil-magit-ignored-modes)))))))

(ert-deftest evil-magit-expand-region-arg-number ()
  "Check that the number of args accepted by
`evil-visual-expand-region' does not change."
  (should-not (evil-visual-expand-region))
  (should-not (evil-visual-expand-region t))
  (should-error (evil-visual-expand-region t t) :type
                'wrong-number-of-arguments))
