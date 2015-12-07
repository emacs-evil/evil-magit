;;; evil-magit-tests.el --- evil-based key bindings for magit

;; Copyright (C) 2015 Justin Burkett

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
                  (nth 3 binding))))))

(ert-deftest evil-magit-section-map-tests ()
  "Test that original bindings in
`evil-magit-original-section-bindings' are correct."
  (dolist (binding evil-magit-original-section-bindings)
    (should (eq (lookup-key (symbol-value (nth 0 binding)) (nth 1 binding))
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
