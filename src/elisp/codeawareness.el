;;; codeawareness.el --- A low-noise collaboration tool.

;; Copyright (C) 2023 Mark Vasile

;; Author: Mark Vasile <mark@codeawareness.com>
;; Package-Requires: ((emacs "26.1") (web))
;; Homepage: https://github.com/CodeAwareness/codeawareness-emacs
;; Version: 1.0

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

;; Code Awareness highlights the code intersections between your working
;; copy and other team member's. This provides an early warning system
;; for merge conflicts, as well as instant traveling between working
;; copies of multiple developers without needing to commit and push.

;; NOTE: This module is lazy-loaded.


;;; Code:

(require 'web)

(defgroup codeawareness nil
  "Code Awareness, low noise collaboration."
  :group 'code-awareness)

(defcustom codeawareness-update-delay 0.2
  "Delay in seconds for time to wait before running a Code Awareness
update, whenever the active buffer changes."
  :type 'number
  :group 'code-awareness)

(defvar codeawareness-update-timer nil
  "Idle timer to run Code Awareness update function.")

(defvar codeawareness-api-url "http://localhost:3009/v1/test" 
  "URL of the local API endpoint")

(defun codeawareness-api-send-filename ()
  "Send the current buffer's file name to the local API."
  (message "API send filename")
  (let ((filename buffer-file-name))
    (when filename
      (message (format "Retrieving info for: %s" filename))
      (web-json-post
       (lambda (data)
         ;; data will be a list
         (message (format "%S" data)))
       :url (concat codeawareness-api-url "?filename=" (url-hexify-string filename))
       ))))

(defun codeawareness-update ()
  "Code Awareness update code intersections for the current buffer,"
  (setq codeawareness-update-timer nil)
  (message (format "Code Awareness updating... %s" buffer-file-name))
  (codeawareness-api-send-filename)
  )

(defun codeawareness-focus-frame ()
  (if (frame-focus-state) (codeawareness-update)))

(defun codeawareness-setup ()
  "Code Awareness: start."
  (message "Code Awareness enabled.")
  ;; Watch focus changes
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function #'codeawareness-focus-frame))

  ;; Watch buffer activation
  (add-hook 'buffer-list-update-hook #'codeawareness-after-buffer-list-update))

(defun codeawareness-tear-down ()
  "Code Awareness: stop and cleanup."
  (remove-hook 'buffer-list-update-hook #'codeawareness-after-buffer-list-update)
  (remove-function after-focus-change-function #'codeawareness-focus-frame))

(define-minor-mode codeawareness-mode
  "Toggle `codeawareness-mode'.
Enable Code Awareness functionality."
  :init-value nil
  :global t
  :lighter nil
  :group 'code-awareness
  (if codeawareness-mode
      (codeawareness-setup)
      (codeawareness-tear-down)))

(defun codeawareness-after-buffer-list-update ()
  "Debounced call to update Code Awareness"
  (unless codeawareness-update-timer
    (setq codeawareness-update-timer
          (run-with-idle-timer codeawareness-update-delay nil #'codeawareness-update))))

(provide 'codeawareness)
;;; codeawareness.el ends here
