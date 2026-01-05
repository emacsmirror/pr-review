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

(defun pr-review--glab-project-path ()
  "Return gitlab project path for current buffer."
  (concat (car pr-review--pr-path) "/" (cadr pr-review--pr-path)))

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
  (let (diffs (res ""))
    (setq diffs
          ;; Prefer using https://docs.gitlab.com/api/merge_requests/#get-a-single-merge-request-diff-version
          ;; This API can return full diff with large files.
          ;; However it cannot work with arbitrary commits compare.
          ;; Fallback to /repository/compare
          (or
           (when-let* ((base-url (format "/projects/%s/merge_requests/%d"
                                         (url-hexify-string (pr-review--glab-project-path))
                                         (caddr pr-review--pr-path)))
                       (diff-versions (apply #'ghub-request "GET" (concat base-url "/versions") nil
                                             (pr-review--ghub-common-request-args)))
                       (diff-version-match
                        (car (seq-filter (lambda (item)
                                           (let-alist item
                                             (and (equal .base_commit_sha base-ref)
                                                  (equal .head_commit_sha head-ref))))
                                         diff-versions)))
                       (resp (apply #'ghub-request "GET"
                                    (format "%s/versions/%d" base-url (alist-get 'id diff-version-match))
                                    nil
                                    (pr-review--ghub-common-request-args))))
             (alist-get 'diffs resp))

           (when-let* ((resp (apply #'ghub-request
                                    "GET"
                                    (format "/projects/%s/repository/compare"
                                            (url-hexify-string (pr-review--glab-project-path)))
                                    `((from . ,base-ref)
                                      (to . ,head-ref)
                                      (unidiff . "true"))
                                    (pr-review--ghub-common-request-args))))
             (alist-get 'diffs resp))))

    (dolist (diff diffs)
      (let-alist diff
        (setq res
              (concat res
                      (format "diff --git %s %s\n" .old_path .new_path)
                      (cond
                       (.too_large
                        (format "file too large, cannot be retrieved"))
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

(pr-review-defmethod-gitlab pr-review--fetch-file (filepath commit)
  (apply #'ghub-request
         "GET"
         (format "/projects/%s/repository/files/%s/raw"
                 (url-hexify-string (pr-review--glab-project-path))
                 (url-hexify-string filepath))
         `((ref . ,commit))
         :reader 'ghub--decode-payload
         (pr-review--ghub-common-request-args)))


(pr-review-defmethod-gitlab pr-review--post-comment (noteable-id body)
  (pr-review--execute-graphql 'create-note
                              `((input . ((noteableId . ,noteable-id)
                                          (body . ,body))))))

(pr-review-defmethod-gitlab pr-review--post-thread-reply (noteable-id reply-id body)
  (pr-review--execute-graphql 'create-note
                              `((input . ((noteableId . ,noteable-id)
                                          (discussionId . ,reply-id)
                                          (body . ,body))))))

(pr-review-defmethod-gitlab pr-review--update-thread-item (note-id body)
  (pr-review--execute-graphql 'update-note
                              `((input . ((id . ,note-id)
                                          (body . ,body))))))

(pr-review-defmethod-gitlab pr-review--post-resolve-review-thread (discussion-id resolve-or-unresolve)
  (pr-review--execute-graphql 'discussion-toggle-resolve
                              `((input . ((id . ,discussion-id)
                                          (resolve . ,resolve-or-unresolve))))))

(pr-review-defmethod-gitlab pr-review--update-pr-body (_ body)
  (pr-review--execute-graphql 'update-merge-request
                              `((input . ((iid . ,(format "%s" (nth 2 pr-review--pr-path)))
                                          (projectPath . ,(pr-review--glab-project-path))
                                          (description . ,body))))))

(pr-review-defmethod-gitlab pr-review--update-pr-title (_ title)
  (pr-review--execute-graphql 'update-merge-request
                              `((input . ((iid . ,(format "%s" (nth 2 pr-review--pr-path)))
                                          (projectPath . ,(pr-review--glab-project-path))
                                          (title . ,title))))))


(pr-review-defmethod-gitlab pr-review--get-assignable-users-1 (repo-owner repo-name)
  (when-let* ((resp (apply #'ghub-request
                           "GET"
                           (format "/projects/%s/members/all" (url-hexify-string (concat repo-owner "/" repo-name)))
                           '()
                           (pr-review--ghub-common-request-args)))
              (res (make-hash-table :test 'equal)))
    (mapc (lambda (usr) (let-alist usr
                          (puthash .username (list (cons 'id .id)
                                                   (cons 'login .username)
                                                   (cons 'name .name))
                                   res)))
          resp)
    res))

(pr-review-defmethod-gitlab pr-review--post-review (noteable-id commit-id event pending-threads body)
  (pcase event
    ("COMMENT" nil)
    ("APPROVE" (setq body (concat "/approve\n" body)))
    (_ (warn "Action %s is not supported, ignore" event)))
  (dolist (thread pending-threads)
    (let* ((body (alist-get 'body thread))
           (position (assq-delete-all 'body (copy-alist thread))))
      (let-alist pr-review--pr-info
        (setf (alist-get 'headSha position) .diffRefs.headSha)
        (setf (alist-get 'baseSha position) .diffRefs.baseSha)
        (setf (alist-get 'startSha position) .diffRefs.startSha))
      (pr-review--execute-graphql 'create-diff-note
                                  `((input . ((noteableId . ,noteable-id)
                                              (body . ,body)
                                              (position . ,position)))))))
  (when body
    (pr-review--post-comment noteable-id body)))

(provide 'pr-review-glab-api)
;;; pr-review-glab-api.el ends here
