;;; pr-review-glab-notification.el ---               -*- lexical-binding: t; -*-

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

(require 'pr-review-common)
(require 'pr-review-notification-render)

(pr-review-defmethod-gitlab pr-review--notification-format-entry (entry)
  (let-alist entry
    (list
     (pr-review--listview-format-time .createdAt)
     (pcase .targetType
       ("MERGEREQUEST" "MR")
       ("ISSUE" "ISS")
       (_ .targetType))
     (format "[%s] %s" .project.name (or .target.title .body))
     (let ((is-open (equal .target.state "opened")))
       (concat
        (unless is-open
          (propertize (concat .target.state " ") 'face 'pr-review-listview-status-face))
        (pcase .action
          ((or "assigned" "review_requested" "mentioned")
           (concat
            "@" .author.username " "
            (propertize .action 'face (when is-open 'pr-review-listview-important-activity-face))))
          ((or "build_failed" "unmergeable")
           (propertize .action 'face 'pr-review-listview-unimportant-activity-face))
          (_ .action)))))))

(pr-review-defmethod-gitlab pr-review--notification-unsubscribed (entry)
  nil)

(pr-review-defmethod-gitlab pr-review--notification-entry-time (entry)
  (let-alist entry .createdAt))

(pr-review-defmethod-gitlab pr-review--notification-unread (entry)
  (let-alist entry
    (equal .state "pending")))

(pr-review-defmethod-gitlab pr-review--notification-open-args (entry)
  (let-alist entry
    (when (equal .targetType "MERGEREQUEST")
      (let* ((components (split-string .project.fullPath "/")))
        (list (string-join (butlast components) "/")
              (car (last components))
              (string-to-number .target.iid)
              nil)))))

(pr-review-defmethod-gitlab pr-review--notification-entry-url (entry)
  (let-alist entry .target.webUrl))


(provide 'pr-review-glab-notification-render)
;;; pr-review-glab-notification-render.el ends here
