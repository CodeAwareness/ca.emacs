;;; codeawareness-logger.el --- Logging for Code Awareness -*- lexical-binding: t -*-

;; Copyright (C) 2023 - 2024 Mark Vasile

;; Author: Mark Vasile <mark@codeawareness.com>
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

;; Logging functionality for Code Awareness.

;;; Code:

(require 'codeawareness-config)

;;; Variables

(defvar codeawareness--log-buffer "*Code Awareness Log*"
  "Buffer name for Code Awareness logs.")

(defvar codeawareness--log-level 'info
  "Current log level.")

;;; Log Levels

(defconst codeawareness--log-levels
  '((error . 0)
    (warn . 1)
    (info . 2)
    (log . 3)
    (debug . 4))
  "Log levels with their numeric values.")

;;; Utility Functions

(defun codeawareness--get-log-level-value (level)
  "Get the numeric value for a log level."
  (cdr (assoc level codeawareness--log-levels)))

(defun codeawareness--should-log (level)
  "Check if the given level should be logged."
  (<= (codeawareness--get-log-level-value level)
      (codeawareness--get-log-level-value codeawareness--log-level)))

(defun codeawareness--write-to-log-buffer (message)
  "Write a message to the log buffer."
  (let ((buffer (get-buffer-create codeawareness--log-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert message "\n")
      (when (and codeawareness-debug (get-buffer-window buffer))
        (recenter -1)))))

;;; Public API

(defun codeawareness-log-error (message &rest args)
  "Log an error message."
  (when (codeawareness--should-log 'error)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args 
                                   (apply #'format message args) 
                                 message))
           (log-entry (format "[%s] [ERROR] %s" timestamp formatted-message)))
      (codeawareness--write-to-log-buffer log-entry)
      (message "Code Awareness Error: %s" formatted-message))))

(defun codeawareness-log-warn (message &rest args)
  "Log a warning message."
  (when (codeawareness--should-log 'warn)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args 
                                   (apply #'format message args) 
                                 message))
           (log-entry (format "[%s] [WARN] %s" timestamp formatted-message)))
      (codeawareness--write-to-log-buffer log-entry))))

(defun codeawareness-log-info (message &rest args)
  "Log an info message."
  (when (codeawareness--should-log 'info)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args 
                                   (apply #'format message args) 
                                 message))
           (log-entry (format "[%s] [INFO] %s" timestamp formatted-message)))
      (codeawareness--write-to-log-buffer log-entry))))

(defun codeawareness-log (message &rest args)
  "Log a general message."
  (when (codeawareness--should-log 'log)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args 
                                   (apply #'format message args) 
                                 message))
           (log-entry (format "[%s] [LOG] %s" timestamp formatted-message)))
      (codeawareness--write-to-log-buffer log-entry))))

(defun codeawareness-log-debug (message &rest args)
  "Log a debug message."
  (when (and codeawareness-debug (codeawareness--should-log 'debug))
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args 
                                   (apply #'format message args) 
                                 message))
           (log-entry (format "[%s] [DEBUG] %s" timestamp formatted-message)))
      (codeawareness--write-to-log-buffer log-entry))))

;;; Interactive Functions

(defun codeawareness-show-log-buffer ()
  "Show the Code Awareness log buffer."
  (interactive)
  (let ((buffer (get-buffer-create codeawareness--log-buffer)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

(defun codeawareness-clear-log-buffer ()
  "Clear the Code Awareness log buffer."
  (interactive)
  (let ((buffer (get-buffer codeawareness--log-buffer)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)))))

;;; Provide

(provide 'codeawareness-logger)
;;; codeawareness-logger.el ends here
