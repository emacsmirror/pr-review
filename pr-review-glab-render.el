;;; pr-review-glab-render.el ---                     -*- lexical-binding: t; -*-

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

(defun pr-review--glab-insert-labels-info (pr-info)
  (let-alist pr-info
    (when .labels.nodes
      (insert
       "  "
       (mapconcat (lambda (label)
                    (propertize (alist-get 'title label)
                                'face
                                `(:background ,(alist-get 'color label)
                                  :foreground ,(alist-get 'textColor label)
                                  :inherit pr-review-label-face)))
                  .labels.nodes " ")))
    (insert "  ")
    ;; TODO: edit labels
    ))

(defun pr-review--glab-insert-mergeability-info (pr-info)
  (let-alist pr-info
    (insert (pr-review--propertize-keyword "MERGEABILITY") ": ")
    (cl-loop for check in .mergeabilityChecks
             if (equal (alist-get 'status check) "FAILED")
             collect (propertize (alist-get 'identifier check) 'face 'pr-review-error-state-face)
             into result
             finally (if result
                         (insert (string-join result ", "))
                       (insert (pr-review--propertize-keyword "SUCCESS"))))
    (when .autoMergeEnabled
      (insert
       " - "
       (pr-review--propertize-keyword (concat "AUTO MERGE: "))
       (propertize .autoMergeStrategy 'face 'pr-review-info-state-face)))
    (insert "\n")))

(defun pr-review--glab-insert-reviewers-info (pr-info)
  (let ((groups (make-hash-table :test 'equal)))
    (let-alist pr-info
      (when .reviewers.nodes
        (dolist (n .reviewers.nodes)
          (let-alist n
            (push .name (gethash .mergeRequestInteraction.reviewState groups)))))
      (when .approvedBy.nodes
        (dolist (n .approvedBy.nodes)
          (push (alist-get 'name n) (gethash "APPROVED" groups))))
      (maphash (lambda (status users)
                 (delete-dups users)
                 (insert (pr-review--propertize-keyword status)
                         ": "
                         (mapconcat #'pr-review--propertize-username users ", "))
                 ;; TODO
                 ;; (when (equal status "UNREVIEWED")
                 ;;   (insert " ")
                 ;;   (insert-button
                 ;;    "Request Review"
                 ;;    'face 'pr-review-button-face
                 ;;    'action (lambda (_) (call-interactively #'pr-review-request-reviews))))
                 (insert "\n"))
               groups))))

(defun pr-review--glab-insert-assignees-info (pr-info)
  (let-alist pr-info
    (when .assignees.nodes
      (insert (pr-review--propertize-keyword "ASSIGNED")
              ": "
              (mapconcat (lambda (assignee) (pr-review--propertize-username (alist-get 'name assignee)))
                         .assignees.nodes ", ")
              "\n"))))

(defun pr-review--glab-insert-system-note (note)
  (let-alist note
    (let ((body-lines (string-split .body "\n")))
      (magit-insert-section (pr-review--event-section .id 'hide)
        (magit-insert-heading
          (propertize "* " 'face 'magit-section-heading)
          (pr-review--propertize-username .author.name)
          " "
          (car body-lines)
          " - "
          (pr-review--format-timestamp .createdAt))
        (when (length> body-lines 1)
          (pr-review--insert-html .bodyHtml)))))
  (insert "\n"))

(defun pr-review--glab-insert-discussion (discussion)
  (let* ((resolved (eq t (alist-get 'resolved discussion)))
         (notes (let-alist discussion .notes.nodes))
         (first-note (car notes))
         goto-diff-line-args)
    ;; use review-thread-section so that pr-review-reply-to-thread works
    (magit-insert-section section (pr-review--review-thread-section
                                    (alist-get 'id discussion)  ;; not discussion's id. for editing
                                    resolved)
        ;; TODO: top-comment-id, updatable, body, id
        (oset section is-resolved resolved)
        (oset section reply-id (alist-get 'replyId discussion))
        (let-alist first-note
          (oset section body .body)
          (oset section updatable .userPermissions.adminNote)
          (oset section update-id .id)
          (when .position.filePath
            (setq goto-diff-line-args (if .position.newLine
                                          (list .position.filePath "RIGHT" .position.newLine)
                                        (list .position.filePath "LEFT" .position.oldLine))))
          (magit-insert-heading
            (propertize "Discussion by " 'face 'magit-section-heading)
            (pr-review--propertize-username .author.name)
            (concat (when goto-diff-line-args
                      (concat " on "
                              (buttonize (format "%s:%s" .position.filePath (or .position.newLine .position.oldLine))
                                         (lambda (_)
                                           (push-mark)
                                           (apply #'pr-review--goto-diff-line goto-diff-line-args)
                                           (recenter))))))
            " - "
            (pr-review--format-timestamp .createdAt)
            (when resolved
              (concat " - " (propertize "RESOLVED" 'face 'pr-review-info-state-face))))
          (pr-review--insert-html .bodyHtml))

        (dolist (note (cdr notes))
          (insert "\n")
          (let-alist note
            (magit-insert-section note-section (pr-review--review-thread-item-section .id)
              (oset note-section body .body)
              (oset note-section updatable .userPermissions.adminNote)
              (magit-insert-heading
                (make-string pr-review-section-indent-width ?\s)
                (pr-review--propertize-username .author.name)
                " - "
                (pr-review--format-timestamp .createdAt))
              (pr-review--insert-html .bodyHtml pr-review-section-indent-width
                                      'pr-review-thread-comment-face))
            ))

        (when (cdr notes)
          (insert "\n")
          (insert (make-string pr-review-section-indent-width ?\s))
          (insert-button "Reply to thread"
                         'face 'pr-review-button-face
                         'action 'pr-review-reply-to-thread)
          (insert "  ")
          (insert-button (if resolved "Unresolve" "Resolve")
                         'face 'pr-review-button-face
                         'action 'pr-review-resolve-thread)
          (insert "\n")))
    (insert "\n")))

(defun pr-review--glab-insert-pr-body (pr diff)
  (let-alist pr
    (pr-review--insert-link .webUrl .webUrl)
    (insert "\n"
            (propertize .targetBranch 'face 'pr-review-branch-face)
            " <- "
            (propertize .sourceBranch 'face 'pr-review-branch-face))
    (pr-review--glab-insert-labels-info pr)
    (insert "\n")
    (insert (pr-review--propertize-keyword (upcase .state))
            " - "
            (propertize (concat "@" .author.name) 'face 'pr-review-author-face)
            " - "
            (pr-review--format-timestamp .createdAt)
            (if .subscribed
                ;; TODO subscribe action
                (concat " - " (pr-review--propertize-keyword "SUBSCRIBED"))
              "")
            "\n")
    (pr-review--glab-insert-mergeability-info pr)
    (insert "\n")
    (pr-review--glab-insert-reviewers-info pr)
    (pr-review--glab-insert-assignees-info pr)
    (insert "\n")
    (magit-insert-section section (pr-review--description-section .id)
                          (oset section body .description)
                          ;; TODO
                          ;; (oset section updatable .viewerCanUpdate)
                          ;; (oset section reaction-groups .reactionGroups)
                          (magit-insert-heading "Description")
                          (pr-review--insert-html .descriptionHtml)
                          ;; TODO
                          ;; (pr-review--maybe-insert-reactions .reactionGroups)
                          )
    (insert "\n")
    (dolist (discussion .discussions.nodes)
      (let-alist discussion
        (when-let ((first-note (car .notes.nodes)))
          (let-alist first-note
            (if .system
                ;; should only have one note??
                (pr-review--glab-insert-system-note first-note)
              (pr-review--glab-insert-discussion discussion)))))
      )

    (when .commits.nodes
      (pr-review--insert-commit-section
       (mapcar (lambda (n) (let-alist n (list .shortId .sha .title)))
               .commits.nodes))
      (insert "\n"))
    (magit-insert-section (pr-review--diff-section)
      (magit-insert-heading
        (concat (format "Files changed (%s files; %s additions, %s deleletions)"
                        .diffStatsSummary.fileCount
                        .diffStatsSummary.additions
                        .diffStatsSummary.deletions)
                (when pr-review--selected-commits
                  (format " - Only viewing selected %d commits" (length pr-review--selected-commits)))))
      (pr-review--insert-diff diff))
    (insert "\n")
    (pr-review--insert-review-action-buttons)
    (pr-review--insert-merge-close-reopen-action-buttons)
    ;; TODO: in-diff-review thread
    ;; TODO: in-diff checks
    )
  )

(pr-review-defmethod-gitlab pr-review--insert-pr (pr diff)
  (magit-insert-section section (pr-review--root-section)
    (let-alist pr
      (oset section title .title)
      ;; TODO
      ;; (oset section updatable .viewerCanUpdate)
      (magit-insert-heading
        (propertize (alist-get 'title pr) 'face 'pr-review-title-face)))
    (insert "\n")
    (pr-review--glab-insert-pr-body pr diff)))

(provide 'pr-review-glab-render)
;;; pr-review-glab-render.el ends here
