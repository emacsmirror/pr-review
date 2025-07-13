;;; pr-review-glab-action.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Yikai Zhao

;; Author: Yikai Zhao <yikai@z1k.dev>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'pr-review-common)
(require 'pr-review-api)
(require 'pr-review-action)

(declare-function pr-review-refresh "pr-review")

(defvar pr-review-glab-quick-actions
  '(("/approve" "Basic Actions" "Approve the merge request")
    ("/unapprove" "Basic Actions" "Unapprove the merge request")
    ("/close" "Basic Actions" "Close the merge request")
    ("/reopen" "Basic Actions" "Reopen the merge request")
    ("/merge" "Basic Actions" "Merge changes (may be when pipeline succeeds or adding to Merge Train)")
    ("/draft" "Basic Actions" "Set the draft status")
    ("/ready" "Basic Actions" "Set the ready status")
    ("/rebase" "Basic Actions" "Rebase source branch on the latest commit of the target branch")
    ("/assign @" "Assignment & Review" "Assign one or more users")
    ("/assign me" "Assignment & Review" "Assign yourself")
    ("/reassign @" "Assignment & Review" "Replace current assignees with those specified")
    ("/unassign @" "Assignment & Review" "Remove specific assignees")
    ("/unassign" "Assignment & Review" "Remove all assignees")
    ("/assign_reviewer @" "Assignment & Review" "Assign one or more users as reviewers")
    ("/reviewer @" "Assignment & Review" "Assign one or more users as reviewers")
    ("/assign_reviewer me" "Assignment & Review" "Assign yourself as a reviewer")
    ("/reviewer me" "Assignment & Review" "Assign yourself as a reviewer")
    ("/reassign_reviewer @" "Assignment & Review" "Replace current reviewers with those specified")
    ("/unassign_reviewer @" "Assignment & Review" "Remove specific reviewers")
    ("/remove_reviewer @" "Assignment & Review" "Remove specific reviewers")
    ("/unassign_reviewer me" "Assignment & Review" "Remove yourself as a reviewer")
    ("/unassign_reviewer" "Assignment & Review" "Remove all reviewers")
    ("/remove_reviewer" "Assignment & Review" "Remove all reviewers")
    ("/request_review @" "Assignment & Review" "Assigns or requests a new review from one or more users")
    ("/request_review me" "Assignment & Review" "Assigns or requests a new review from yourself")
    ("/submit_review" "Assignment & Review" "Submit a pending review")
    ("/label ~" "Labels & Metadata" "Add one or more labels")
    ("/labels ~" "Labels & Metadata" "Add one or more labels")
    ("/relabel ~" "Labels & Metadata" "Replace current labels with those specified")
    ("/unlabel ~" "Labels & Metadata" "Remove specified labels")
    ("/remove_label ~" "Labels & Metadata" "Remove specified labels")
    ("/unlabel" "Labels & Metadata" "Remove all labels")
    ("/remove_label" "Labels & Metadata" "Remove all labels")
    ("/milestone %" "Labels & Metadata" "Set milestone")
    ("/remove_milestone" "Labels & Metadata" "Remove milestone")
    ("/remove_estimate" "Time Tracking" "Remove time estimate")
    ("/remove_time_estimate" "Time Tracking" "Remove time estimate")
    ("/remove_time_spent" "Time Tracking" "Remove time spent")
    ("/cc @" "Discussion & Notifications" "Mention a user (performs no action, just mention)")
    ("/subscribe" "Discussion & Notifications" "Subscribe to notifications")
    ("/unsubscribe" "Discussion & Notifications" "Unsubscribe from notifications")
    ("/lock" "Discussion & Notifications" "Lock the discussions")
    ("/unlock" "Discussion & Notifications" "Unlock the discussions")
    ("/done" "Discussion & Notifications" "Mark to-do item as done")
    ("/todo" "Discussion & Notifications" "Add a to-do item")
    ("/target_branch" "Configuration" "Set target branch")
    ("/title" "Configuration" "Change title")
    ("/react :" "Fun Commands" "Toggle an emoji reaction")
    ("/shrug" "Fun Commands" "Add ¯\\_(ツ)_/¯")
    ("/tableflip" "Fun Commands" "Add (╯°□°)╯︵ ┻━┻"))
  "List of quick actions for gitlab. List of (command, category, description)")


(defun pr-review--glab-interactive-read-quick-action ()
  (let* ((candidates (mapcar (lambda (action)
                               (let ((command (nth 0 action))
                                     (category (nth 1 action))
                                     (description (nth 2 action)))
                                 (propertize command
                                             'category category
                                             'annotation description)))
                             pr-review-glab-quick-actions))
         (completion-extra-properties
          '(:group-function (lambda (candidate transform)
                              (if transform
                                  candidate
                                (get-text-property 0 'category candidate)))
                            :annotation-function (lambda (candidate)
                                                   (concat " " (get-text-property 0 'annotation candidate))))))
    (let ((result (completing-read "Quick action: " candidates nil t)))
      ;; TODO: also support labels
      (if (string-suffix-p "@" result)
          (let* ((users (pr-review--get-assignable-users))
                 (user-candidates (mapcar (lambda (user)
                                            (let ((login (alist-get 'login user))
                                                  (name (alist-get 'name user)))
                                              (propertize login 'annotation name)))
                                          (hash-table-values users)))
                 (completion-extra-properties
                  '(:annotation-function (lambda (candidate)
                                           (concat " " (get-text-property 0 'annotation candidate)))))
                 (selected-users (completing-read-multiple "Select users: " user-candidates)))
            (concat (string-remove-suffix "@" result) " "
                    (mapconcat (lambda (user) (concat "@" user)) selected-users " ")))
        result))))


(pr-review-defmethod-gitlab pr-review-general-interactive-action ()
  "Prompt for quick action command and send as comment."
  (let ((cmd (pr-review--glab-interactive-read-quick-action)))
    (when (and cmd (length> cmd 0))
      (pr-review--post-comment (alist-get 'id pr-review--pr-info) cmd)
      (pr-review-refresh))))


(pr-review-defmethod-gitlab pr-review--get-review-thread-input-at-current-point ()
  (when pr-review--selected-commits
    (error "Adding review with partial selected commits are not supported yet"))
  (save-excursion
    (when (use-region-p)
      (goto-char (1- (region-end))))
    (beginning-of-line)
    ;; https://docs.gitlab.com/api/discussions/#create-a-new-thread-in-the-merge-request-diff
    (let ((left-prop (get-text-property (point) 'pr-review-diff-line-left))
          (right-prop (get-text-property (point) 'pr-review-diff-line-right)))
      (cond
       ((and right-prop (not left-prop))
        `((paths . ((oldPath . ,(alist-get 'path-orig right-prop))
                    (newPath . ,(alist-get 'path right-prop))))
          (newLine . ,(alist-get 'line right-prop))
          (-gh-compat-info . ((path . ,(alist-get 'path right-prop))
                              (side . "RIGHT")
                              (line . ,(alist-get 'line right-prop))))))
       ((and left-prop (not right-prop))
        `((paths . ((oldPath . ,(alist-get 'path-orig left-prop))
                    (newPath . ,(alist-get 'path left-prop))))
          (oldLine . ,(alist-get 'line left-prop))
          (-gh-compat-info . ((path . ,(alist-get 'path left-prop))
                              (side . "LEFT")
                              (line . ,(alist-get 'line left-prop))))))
       ((and left-prop right-prop)
        `((paths . ((oldPath . ,(alist-get 'path-orig left-prop))
                    (newPath . ,(alist-get 'path left-prop))))
          (oldLine . ,(alist-get 'line left-prop))
          (newLine . ,(alist-get 'line right-prop))
          (-gh-compat-info . ((path . ,(alist-get 'path left-prop))
                              (side . "LEFT")
                              (line . ,(alist-get 'line left-prop))))))))))


(provide 'pr-review-glab-action)
;;; pr-review-glab-action.el ends here
