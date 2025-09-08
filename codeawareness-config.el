;;; codeawareness-config.el --- Configuration for Code Awareness -*- lexical-binding: t -*-

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

;; Configuration constants and settings for Code Awareness.

;;; Code:

;;; Constants

(defconst codeawareness--caw-schema "caw"
  "Schema for Code Awareness URIs.")

(defconst codeawareness--extract-repo-dir "extract"
  "Directory name for extracted repository files.")

(defconst codeawareness--pipe-catalog "catalog"
  "Catalog pipe name.")

;;;###autoload
(defgroup codeawareness-config nil
  "Code Awareness configuration."
  :group 'codeawareness
  :prefix "codeawareness-")

;;;###autoload
(defcustom codeawareness-catalog "catalog"
  "Catalog name for Code Awareness."
  :type 'string
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-highlight-intensity 0.3
  "Intensity of highlighting (0.0 to 1.0)."
  :type 'number
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-highlight-refresh-delay 0.5
  "Delay in seconds before refreshing highlights after changes."
  :type 'number
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-highlight-persistent nil
  "Whether highlights should persist across buffer switches."
  :type 'boolean
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-full-width-highlights t
  "Whether to use full-width highlights that extend to the end of the line."
  :type 'boolean
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-update-delay 0.5
  "Delay in seconds before running a Code Awareness update."
  :type 'number
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-debug nil
  "Enable debug mode for Code Awareness."
  :type 'boolean
  :group 'codeawareness-config)

;;; Theme Support

;;;###autoload
(defcustom codeawareness-change-color-light "#00b1a420"
  "Color for changed lines in light theme."
  :type 'string
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-change-color-dark "#03445f"
  "Color for changed lines in dark theme."
  :type 'string
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-peer-color-light "#ffdd34"
  "Color for peer code in light theme."
  :type 'string
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-peer-color-dark "#1f1cc2"
  "Color for peer code in dark theme."
  :type 'string
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-merge-color-light "#ffc000"
  "Color for merged code in light theme."
  :type 'string
  :group 'codeawareness-config)

;;;###autoload
(defcustom codeawareness-merge-color-dark "#141299"
  "Color for merged code in dark theme."
  :type 'string
  :group 'codeawareness-config)

;;; Utility Functions

(defun codeawareness--get-theme-color (light-color dark-color)
  "Get the appropriate color for the current theme."
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      dark-color
    light-color))

(defun codeawareness--get-change-color ()
  "Get the color for changed lines."
  (codeawareness--get-theme-color
   codeawareness-change-color-light
   codeawareness-change-color-dark))

(defun codeawareness--get-peer-color ()
  "Get the color for peer code."
  (codeawareness--get-theme-color
   codeawareness-peer-color-light
   codeawareness-peer-color-dark))

(defun codeawareness--get-merge-color ()
  "Get the color for merged code."
  (codeawareness--get-theme-color
   codeawareness-merge-color-light
   codeawareness-merge-color-dark))

;;; Provide

(provide 'codeawareness-config)
;;; codeawareness-config.el ends here
