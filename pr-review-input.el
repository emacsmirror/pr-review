;;; pr-review-input.el --- Input functions for pr-review  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yikai Zhao

;; Author: Yikai Zhao <yikai@z1k.dev>
;; Keywords: tools

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

(defvar-local pr-review--comment-input-saved-window-config nil)
(defvar-local pr-review--comment-input-exit-callback nil)
(defvar-local pr-review--comment-input-refresh-after-exit nil)
(defvar-local pr-review--comment-input-prev-marker nil)

(defun pr-review-comment-input-abort ()
  "Abort current comment input buffer, discard content."
  (interactive)
  (unless pr-review-comment-input-mode (error "Invalid mode"))
  (let ((saved-window-config pr-review--comment-input-saved-window-config))
    (kill-buffer)
    (when saved-window-config
      (unwind-protect
          (set-window-configuration saved-window-config)))))

(defun pr-review-comment-input-exit ()
  "Apply content and exit current comment input buffer."
  (interactive)
  (unless pr-review-comment-input-mode (error "Invalid mode"))
  (let ((content (buffer-string)))
    (when (and pr-review--comment-input-exit-callback
               (not (string-empty-p content)))
      (funcall pr-review--comment-input-exit-callback (buffer-string))))
  (let ((refresh-after-exit pr-review--comment-input-refresh-after-exit)
        (prev-marker pr-review--comment-input-prev-marker))
    (pr-review-comment-input-abort)
    (when refresh-after-exit
      (when-let ((prev-buffer (marker-buffer prev-marker))
                 (prev-pos (marker-position prev-marker)))
        (switch-to-buffer prev-buffer)
        (pr-review-refresh)
        (goto-char prev-pos)))))

(defvar pr-review-comment-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'pr-review-comment-input-exit)
    (define-key map "\C-c\C-k" 'pr-review-comment-input-abort)
    map))

(define-minor-mode pr-review-comment-input-mode
  "Minor mode for PR Review comment input buffer."
  :lighter " PrReviewCommentInput")

(defun pr-review--open-comment-input-buffer (description open-callback exit-callback &optional refresh-after-exit)
  "Open a comment buffer for user input with DESCRIPTION,
OPEN-CALLBACK is called when the buffer is opened,
EXIT-CALLBACK is called when the buffer is exit (not abort),
both callbacks are called inside the comment buffer,
if REFRESH-AFTER-EXIT is not nil, refresh the current pr-review buffer after exit."
  (let ((marker (point-marker)))
    (with-current-buffer (generate-new-buffer "*pr-review comment input*")
      (markdown-mode)
      (pr-review-comment-input-mode)

      (setq-local
       header-line-format (concat description " "
                                  (substitute-command-keys
                                   (concat "Confirm with `\\[pr-review-comment-input-exit]' or "
                                           "abort with `\\[pr-review-comment-input-abort]'")))
       pr-review--comment-input-saved-window-config (current-window-configuration)
       pr-review--comment-input-exit-callback exit-callback
       pr-review--comment-input-refresh-after-exit refresh-after-exit
       pr-review--comment-input-prev-marker marker)

      (when open-callback
        (funcall open-callback))

      (switch-to-buffer-other-window (current-buffer)))))


(provide 'pr-review-input)
;;; pr-review-input.el ends here