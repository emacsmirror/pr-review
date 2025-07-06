;;; pr-review-glab-api.el ---                        -*- lexical-binding: t; -*-

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

(require 'pr-review-api)

(pr-review-defmethod-gitlab pr-review--fetch-pr-info ()
  (pcase-let ((`(,repo-owner ,repo-name ,pr-id) pr-review--pr-path))
    (let-alist (pr-review--execute-graphql
                'get-merge-request
                `((repo_path . ,(concat repo-owner "/" repo-name))
                  (iid . ,(format "%s" pr-id))))
      .project.mergeRequest)))

(pr-review-defmethod-gitlab pr-review--current-commit-base ()
  (let-alist pr-review--pr-info
    (or pr-review--selected-commit-base .diffRefs.baseSha)))

(pr-review-defmethod-gitlab pr-review--current-commit-head ()
  (let-alist pr-review--pr-info
    (or pr-review--selected-commit-head .diffRefs.headSha)))

(pr-review-defmethod-gitlab pr-review--fetch-compare (base-ref head-ref)
  (when-let* ((repo-owner (car pr-review--pr-path))
              (repo-name (cadr pr-review--pr-path))
              (resp (apply #'ghub-request
                           "GET"
                           (format "/projects/%s/repository/compare"
                                   (string-replace "/" "%2F" (concat repo-owner "/" repo-name)))
                           `((from . ,base-ref)
                             (to . ,head-ref)
                             (unidiff . "true"))
                           (pr-review--ghub-common-request-args)))
              (res ""))
    (dolist (diff (alist-get 'diffs resp))
      (let-alist diff
        (setq res
              (concat res
                      (format "diff --git %s %s\n" .old_path .new_path)
                      (cond
                       ;; TODO: mode change?
                       (.new_file
                        (format "new file mode %s\n" .b_mode))
                       (.deleted_file
                        (format "deleted file mode %s\n" .a_mode))
                       (.renamed_file
                        (format "rename from %s\nrename to %s\n" .old_path .new_path)))
                      ;; FIXME: large diffs are not visible...
                      .diff))))
    (setq res (replace-regexp-in-string
               (rx line-start (group-n 1 (or "+++" "---")) " " (or "a/" "b/") (group-n 2 (+? not-newline)) line-end)
               "\\1 \\2"
               res))
    res))


(provide 'pr-review-glab-api)
;;; pr-review-glab-api.el ends here
