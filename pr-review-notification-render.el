;;; pr-review-notification-render.el ---             -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Yikai Zhao

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

(require 'pr-review-api)
(require 'pr-review-listview)
(require 'cl-seq)


(defun pr-review--notification-format-activities (entry)
  "Format activities for notification ENTRY."
  (let ((my-login (let-alist (pr-review--whoami-cached) .viewer.login))
        (op (let-alist entry .pr-info.author.login))
        ;; for the following me-* status: t means yes, 'new means yes+new
        me-mentioned me-assigned me-review-requested me-approved
        new-participants all-participants
        all-reviewers approved-reviewers rejected-reviewers)
    (let-alist entry
      (when op
        (push op all-participants)
        (unless .last_read_at
          ;; add author to commenters if no last read
          (push op new-participants)))
      (dolist (opinionated-review .pr-info.latestOpinionatedReviews.nodes)
        (let-alist opinionated-review
          (pcase .state
            ("APPROVED" (push .author.login approved-reviewers))
            ("CHANGES_REQUESTED" (push .author.login rejected-reviewers)))))
      (setq all-reviewers (mapcar (lambda (n) (let-alist n .requestedReviewer.login)) .pr-info.reviewRequests.nodes)
            me-assigned (cl-find-if (lambda (node) (equal my-login (let-alist node .login)))
                                    .pr-info.assignees.nodes)
            me-review-requested (member my-login all-reviewers)
            me-approved (member my-login approved-reviewers)))
    (dolist (timeline-item (let-alist entry .pr-info.timelineItemsSince.nodes))
      (let-alist timeline-item
        (pcase .__typename
          ("AssignedEvent" (when (equal my-login .assignee.login)
                             (setq me-assigned 'new)))
          ("ReviewRequestedEvent" (when (and (equal my-login .requestedReviewer.login) (not me-approved))
                                    (setq me-review-requested 'new)))
          ("MentionedEvent" (when (equal my-login .actor.login)
                              (setq me-mentioned t)))
          ((or "IssueComment" "PullRequestReview")
           (unless (equal my-login .author.login)
             (push .author.login new-participants)))
          )))
    (dolist (participant-item (let-alist entry .pr-info.participants.nodes))
      (let ((login (let-alist participant-item .login)))
        (unless (or (equal login my-login) (member login new-participants))
          (push login all-participants))))
    (setq all-participants (delete-dups (append (reverse new-participants)
                                                (reverse all-participants))))
    (concat (let-alist entry
              (when (and .pr-info.state (not (equal .pr-info.state "OPEN")))
                (concat (propertize (downcase .pr-info.state) 'face 'pr-review-listview-status-face) " ")))
            (when me-mentioned (propertize "+mentioned " 'face 'pr-review-listview-important-activity-face))
            (pcase me-assigned
              ('new (propertize "+assigned " 'face 'pr-review-listview-important-activity-face))
              ('t (propertize "assigned " 'face 'pr-review-listview-status-face)))
            (pcase me-review-requested
             ('new (propertize "+review_requested " 'face 'pr-review-listview-important-activity-face))
             ('t (propertize "review_requested " 'face 'pr-review-listview-status-face)))
            (when me-approved
              (propertize "approved " 'face 'pr-review-listview-status-face))
            (when all-participants
              (mapconcat
               (lambda (x)
                 (let ((is-new (member x new-participants)))
                   (propertize
                    (concat
                     (when is-new "+")
                     x
                     (cond
                      ((equal x op) "@")
                      ((member x approved-reviewers) "#")
                      ((member x rejected-reviewers) "!")
                      ((member x all-reviewers) "?")))
                    'face
                    (if is-new nil 'pr-review-listview-unimportant-activity-face))))
               all-participants " ")))))

(defun pr-review--notification-format-type (entry)
  "Format type column of notification ENTRY."
  (let-alist entry
    (pcase .subject.type
      ("PullRequest" "PR")
      ("Issue" "ISS")
      (_ .subject.type))))


(cl-defmethod pr-review--notification-format-entry (entry)
  (let-alist entry
    (list
     (pr-review--listview-format-time .updated_at)
     (pr-review--notification-format-type entry)
     (format "[%s] %s" .repository.full_name (string-trim-right .subject.title))
     (pr-review--notification-format-activities entry)
     ;; .reason
     )))


(cl-defmethod pr-review--notification-unsubscribed (entry)
  "Return the subscription state if ENTRY is unsubscribed, nil if subscribed."
  (let-alist entry
    (when (and .pr-info.viewerSubscription
               (not (equal .pr-info.viewerSubscription "SUBSCRIBED")))
      .pr-info.viewerSubscription)))

(cl-defmethod pr-review--notification-unread (entry)
  "Return the unread state of ENTRY."
  (let-alist entry .unread))


(cl-defmethod pr-review--notification-entry-time (entry)
  "Return the timestamp of ENTRY."
  (let-alist entry .updated_at))


(cl-defmethod pr-review--notification-open-args (entry)
  "Return (repo-owner repo-name pr-id last-read-time) for ENTRY, (or nil), for `pr-review-open'."
  (let-alist entry
    (when (equal .subject.type "PullRequest")
      (let ((pr-id (when (string-match (rx (group (+ (any digit))) eos) .subject.url)
                     (match-string 1 .subject.url))))
        (list .repository.owner.login
              .repository.name
              (string-to-number pr-id)
              .last_read_at)))))

(cl-defmethod pr-review--notification-entry-url (entry)
  "Return the url of ENTRY."
  (let-alist entry .subject.url))


(provide 'pr-review-notification-render)
;;; pr-review-notification-render.el ends here
