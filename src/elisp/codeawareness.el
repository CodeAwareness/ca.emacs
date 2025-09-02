;;; codeawareness.el --- Code Awareness for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 - 2025 Mark Vasile

;; Author: Mark Vasile <mark@codeawareness.com>
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/CodeAwareness/ca.emacs

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

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'codeawareness-config)
(require 'codeawareness-logger)
(require 'hl-line nil t) ; Optional require for hl-line integration

;;; Customization

(defgroup codeawareness nil
  "Code Awareness, low noise collaboration."
  :group 'applications
  :prefix "codeawareness-")

;;; Internal Variables

(defvar codeawareness--guid nil
  "Unique identifier for this Emacs instance.")

(defvar codeawareness--client-registered nil
  "Whether the client has been registered with the catalog service.")

(defvar codeawareness--ipc-process nil
  "IPC process for communicating with the local service.")

(defvar codeawareness--ipc-catalog-process nil
  "IPC process for catalog communication.")

(defvar codeawareness--response-handlers (make-hash-table :test 'equal)
  "Hash table of response handlers for IPC requests.")

(defvar codeawareness--active-project nil
  "Currently active project data.")

(defvar codeawareness--active-buffer nil
  "Currently active buffer.")

(defvar codeawareness--poll-attempts 0
  "Number of polling attempts for local service socket.")

(defvar codeawareness--update-timer nil
  "Timer for debounced updates.")

(defvar codeawareness--connected nil
  "Whether we're connected to the local service.")

(defvar codeawareness--config nil
  "Configuration data.")

;;; Store/State Management

(defvar codeawareness--store nil
  "Central store for Code Awareness state.")

(defvar codeawareness--projects nil
  "List of all projects.")

(defvar codeawareness--active-selections nil
  "Currently active selections.")

(defvar codeawareness--selected-peer nil
  "Currently selected peer.")

(defvar codeawareness--color-theme 1
  "Current color theme (1=Light, 2=Dark, 3=High Contrast).")

(defvar codeawareness--tmp-dir "/tmp/caw.emacs"
  "Temporary directory for Code Awareness.")

(defvar codeawareness--peer-fs (make-hash-table :test 'equal)
  "Peer file system tree structure.")

(defvar codeawareness--events-table (make-hash-table :test 'equal)
  "Hash table mapping event names to handler functions.")

(defvar codeawareness--user nil
  "Current user data.")

(defvar codeawareness--tokens nil
  "Authentication tokens.")

(defvar codeawareness--authenticated nil
  "Whether the client is authenticated with the local service.")

;;; Highlighting System

(defvar codeawareness--highlights (make-hash-table :test 'equal)
  "Hash table of highlights by buffer, tracking line numbers and overlay objects.")

(defvar codeawareness--highlight-faces nil
  "Predefined faces for different types of highlights.")

(defvar codeawareness--highlight-timer nil
  "Timer for debounced highlight refresh.")

;;; HL-Line Integration

(defvar codeawareness--hl-line-overlays (make-hash-table :test 'equal)
  "Hash table of hl-line overlays by buffer and line number.")

(defvar codeawareness--hl-line-faces (make-hash-table :test 'equal)
  "Hash table of custom hl-line faces by highlight type.")

;;; Configuration

(defun codeawareness--init-config ()
  "Initialize configuration."
  (setq codeawareness--config
        `((catalog . ,codeawareness-catalog)
          (highlight-while-closed . ,codeawareness-highlight-while-closed)
          (update-delay . ,codeawareness-update-delay)))
  (codeawareness-log-info "Configuration initialized"))

(defun codeawareness--init-store ()
  "Initialize the central store."
  (setq codeawareness--store
        `((active-project . ,codeawareness--active-project)
          (projects . ,codeawareness--projects)
          (active-buffer . ,codeawareness--active-buffer)
          (active-selections . ,codeawareness--active-selections)
          (selected-peer . ,codeawareness--selected-peer)
          (color-theme . ,codeawareness--color-theme)
          (tmp-dir . ,codeawareness--tmp-dir)
          (peer-fs . ,codeawareness--peer-fs)
          (user . ,codeawareness--user)
          (tokens . ,codeawareness--tokens)))
  (codeawareness-log-info "Store initialized"))

(defun codeawareness--register-event-handler (event-name handler-function)
  "Register an event handler function for the given event name."
  (puthash event-name handler-function codeawareness--events-table)
  (codeawareness-log-info "Registered event handler for: %s" event-name))

(defun codeawareness--init-event-handlers ()
  "Initialize all event handlers."
  ;; Clear existing handlers
  (clrhash codeawareness--events-table)

  ;; Register event handlers
  (codeawareness--register-event-handler "peer:select" #'codeawareness--handle-peer-select)
  (codeawareness--register-event-handler "peer:unselect" #'codeawareness--handle-peer-unselect)
  (codeawareness--register-event-handler "branch:select" #'codeawareness--handle-branch-select)
  (codeawareness--register-event-handler "branch:unselect" #'codeawareness--handle-branch-unselect)
  (codeawareness--register-event-handler "branch:refresh" #'codeawareness--handle-branch-refresh)
  (codeawareness--register-event-handler "auth:logout" #'codeawareness--handle-auth-logout)
  (codeawareness--register-event-handler "context:add" #'codeawareness--handle-context-add)
  (codeawareness--register-event-handler "context:del" #'codeawareness--handle-context-del)
  (codeawareness--register-event-handler "context:open-rel" #'codeawareness--handle-context-open-rel)
  ;; Add more event handlers here as needed

  (codeawareness-log-info "Event handlers initialized"))

(defun codeawareness--clear-store ()
  "Clear the store and reset all state."
  (codeawareness-log-info "Clearing store")
  (setq codeawareness--tokens nil
        codeawareness--user nil
        codeawareness--authenticated nil
        codeawareness--active-project nil
        codeawareness--active-buffer nil
        codeawareness--active-selections nil
        codeawareness--selected-peer nil
        codeawareness--color-theme 1
        codeawareness--tmp-dir "/tmp/caw.emacs"
        codeawareness--peer-fs (make-hash-table :test 'equal)
        codeawareness--poll-attempts 0)
  (codeawareness--init-store))

(defun codeawareness--reset-store ()
  "Reset store state (keep user/tokens)."
  (codeawareness-log-info "Resetting store")
  (setq codeawareness--peer-fs (make-hash-table :test 'equal)
        codeawareness--active-buffer nil
        codeawareness--active-selections nil)
  (codeawareness--init-store))

;;; Project Management

(defun codeawareness--add-project (project)
  "Add a project to the store."
  (codeawareness-log-info "Adding project %s" (alist-get 'root project))
  (setq codeawareness--active-project project)
  ;; Add to projects list if not already present
  (unless (cl-find (alist-get 'root project) codeawareness--projects
                   :key (lambda (p) (alist-get 'root p)) :test 'string=)
    (push project codeawareness--projects))
  (codeawareness--init-store)
  project)

(defun codeawareness--get-active-file-path ()
  "Get the path of the currently active file."
  (when (and codeawareness--active-buffer
             (buffer-live-p codeawareness--active-buffer))
    (buffer-file-name codeawareness--active-buffer)))

(defun codeawareness--get-active-file-content ()
  "Get the content of the currently active file."
  (when (and codeawareness--active-buffer
             (buffer-live-p codeawareness--active-buffer))
    (with-current-buffer codeawareness--active-buffer
      (buffer-string))))

(defun codeawareness--cross-platform-path (path)
  "Convert path to cross-platform format (forward slashes)."
  (when path
    (replace-regexp-in-string "\\\\" "/" path)))

;;; Workspace Management

;; The refreshActiveFile hook implementation follows the VSCode pattern:
;; 1. Called immediately after authentication is successful (like VSCode's init function)
;; 2. Called whenever the active buffer changes (like VSCode's onDidChangeActiveTextEditor)
;; 3. Sends a repo:active-path message to the local service with the current file path and content
;; 4. Updates highlights and project data based on the response

(defun codeawareness--refresh-active-file ()
  "Refresh the currently active file by sending repo:active-path message."

  ;; Check if we have the necessary components to send a refresh request
  (let ((fpath (codeawareness--get-active-file-path))
        (doc (codeawareness--get-active-file-content)))
    (if (not fpath)
        (codeawareness-log-info "No active file to refresh")
      (if (not codeawareness--authenticated)
          (codeawareness-log-warn "Not authenticated, skipping file refresh")
        (if (not (and codeawareness--ipc-process
                      (eq (process-status codeawareness--ipc-process) 'open)))
            (codeawareness-log-warn "IPC process not ready, skipping file refresh")
          (codeawareness-log-info "Refreshing active file %s" fpath)
          (let ((message-data `((fpath . ,(codeawareness--cross-platform-path fpath))
                                (doc . ,doc)
                                (caw . ,codeawareness--guid))))
            (codeawareness--transmit "repo:active-path" message-data)
            (codeawareness--setup-response-handler "code" "repo:active-path" fpath)))))))

;;; Highlighting System

(defun codeawareness--init-highlight-faces ()
  "Initialize predefined faces for different highlight types."
  (setq codeawareness--highlight-faces
        `((conflict . ,(make-face 'codeawareness-conflict-face))
          (overlap . ,(make-face 'codeawareness-overlap-face))
          (peer . ,(make-face 'codeawareness-peer-face))
          (modified . ,(make-face 'codeawareness-modified-face))))

  ;; Set face attributes based on color theme
  (let ((conflict-face (alist-get 'conflict codeawareness--highlight-faces))
        (overlap-face (alist-get 'overlap codeawareness--highlight-faces))
        (peer-face (alist-get 'peer codeawareness--highlight-faces))
        (modified-face (alist-get 'modified codeawareness--highlight-faces)))

    ;; Detect if we're in a dark theme
    (let ((is-dark-theme (eq (frame-parameter nil 'background-mode) 'dark)))
      (if is-dark-theme
          ;; Dark theme colors
          (progn
            ;; Conflict highlights (red background for dark theme)
            (set-face-attribute conflict-face nil
                                :background "#4a1a1a"
                                :foreground "#ff6b6b"
                                :weight 'bold)

            ;; Overlap highlights (yellow/orange background for dark theme)
            (set-face-attribute overlap-face nil
                                :background "#4a3a1a"
                                :foreground "#ffd93d"
                                :weight 'normal)

            ;; Peer highlights (blue background for dark theme)
            (set-face-attribute peer-face nil
                                :background "#1a2a4a"
                                :foreground "#74c0fc"
                                :weight 'normal)

            ;; Modified highlights (green background for dark theme)
            (set-face-attribute modified-face nil
                                :background "#1a4a1a"
                                :foreground "#69db7c"
                                :weight 'normal))

        ;; Light theme colors
        (progn
          ;; Conflict highlights (red background for light theme)
          (set-face-attribute conflict-face nil
                              :background "#ffebee"
                              :foreground "#c62828"
                              :weight 'bold)

          ;; Overlap highlights (yellow background for light theme)
          (set-face-attribute overlap-face nil
                              :background "#fff8e1"
                              :foreground "#f57f17"
                              :weight 'normal)

          ;; Peer highlights (blue background for light theme)
          (set-face-attribute peer-face nil
                              :background "#e3f2fd"
                              :foreground "#1565c0"
                              :weight 'normal)

          ;; Modified highlights (green background for light theme)
          (set-face-attribute modified-face nil
                              :background "#e8f5e8"
                              :foreground "#2e7d32"
                              :weight 'normal)))))

  ;; Initialize hl-line faces if hl-line is available
  (when (featurep 'hl-line)
    (codeawareness--init-hl-line-faces))

  (codeawareness-log-info "Highlight faces initialized"))

(defun codeawareness--get-highlight-face (type)
  "Get the face for the given highlight type."
  (alist-get type codeawareness--highlight-faces))

(defun codeawareness--create-line-overlay (buffer line-number face &optional properties)
  "Create an overlay for a specific line in the given buffer.
Uses hl-line technique to properly handle empty lines."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let* ((line-count (line-number-at-pos (point-max)))
             ;; Use save-excursion to get absolute line positions regardless of cursor position
             (start (save-excursion
                      ;; Suppress warning: goto-line is needed for absolute positioning
                      (with-suppressed-warnings ((interactive-only goto-line))
                        (goto-line line-number))
                      (line-beginning-position)))
             ;; Use hl-line technique: end at start of next line instead of end of current line
             ;; This ensures empty lines get proper overlay span
             (end (save-excursion
                    ;; Suppress warning: goto-line is needed for absolute positioning
                    (with-suppressed-warnings ((interactive-only goto-line))
                      (goto-line (1+ line-number)))
                    (line-beginning-position))))
        (when (and (<= line-number line-count) (>= line-number 1))
          (let ((overlay (make-overlay start end buffer t nil)))
            (overlay-put overlay 'face face)
            (overlay-put overlay 'codeawareness-type 'line-highlight)
            (overlay-put overlay 'codeawareness-line line-number)
            ;; Add any additional properties
            (when properties
              (dolist (prop properties)
                (overlay-put overlay (car prop) (cdr prop))))
            overlay))))))

(defun codeawareness--clear-buffer-highlights (buffer)
  "Clear all Code Awareness highlights from the given buffer."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (overlay-get overlay 'codeawareness-type)
          (delete-overlay overlay))))
    ;; Remove from highlights hash table
    (remhash buffer codeawareness--highlights)
    ;; Also clear hl-line highlights if using that mode
    (when (and codeawareness-use-hl-line-mode (featurep 'hl-line))
      (codeawareness--clear-buffer-hl-line-highlights buffer))
    (codeawareness-log-info "Cleared highlights for buffer %s" buffer)))

(defun codeawareness--clear-all-highlights ()
  "Clear all Code Awareness highlights from all buffers."
  (dolist (buffer (buffer-list))
    (codeawareness--clear-buffer-highlights buffer))
  (clrhash codeawareness--highlights)
  ;; Also clear hl-line highlights if using that mode
  (when (and codeawareness-use-hl-line-mode (featurep 'hl-line))
    (dolist (buffer (buffer-list))
      (codeawareness--clear-buffer-hl-line-highlights buffer))
    (clrhash codeawareness--hl-line-overlays))
  (codeawareness-log-info "Cleared all highlights"))

(defun codeawareness--add-highlight (buffer line-number type &optional properties)
  "Add a highlight to the specified line in the given buffer."
  (when (and buffer line-number type)
    (let* ((face (codeawareness--get-highlight-face type))
           (overlay (codeawareness--create-line-overlay buffer line-number face properties)))
      (when overlay
        ;; Store highlight information
        (let ((buffer-highlights (gethash buffer codeawareness--highlights)))
          (unless buffer-highlights
            (setq buffer-highlights (make-hash-table :test 'equal))
            (puthash buffer buffer-highlights codeawareness--highlights))
          (puthash line-number overlay buffer-highlights))
        overlay))))

(defun codeawareness--refresh-buffer-highlights (buffer)
  "Refresh highlights for the given buffer by recreating them."
  (when (and buffer (buffer-live-p buffer))
    (let ((buffer-highlights (gethash buffer codeawareness--highlights)))
      (when buffer-highlights
        (codeawareness-log-info "Refreshing highlights for buffer %s" buffer)
        ;; Recreate highlights from stored data
        (maphash (lambda (line-number highlight-data)
                   (let ((type (overlay-get highlight-data 'codeawareness-highlight-type))
                         (properties (overlay-get highlight-data 'codeawareness-properties)))
                     (codeawareness--add-highlight buffer line-number type properties)))
                 buffer-highlights)))))


(defun codeawareness--apply-highlights-from-data (buffer highlight-data)
  "Apply highlights to buffer based on data from the local service."
  (when (and buffer (buffer-live-p buffer) highlight-data)
    (codeawareness-log-info "Highlights for %s, data: %s"
                            (buffer-name buffer) highlight-data)
    ;; Use hl-line mode if configured, otherwise use custom overlays
    (if (and codeawareness-use-hl-line-mode (featurep 'hl-line))
        (progn
          (codeawareness-log-info "Using hl-line mode for highlighting")
          (codeawareness--apply-hl-line-highlights-from-data buffer highlight-data))
      (codeawareness-log-info "Using custom overlay mode for highlighting")
      ;; Clear existing highlights first
      ;; (codeawareness--clear-buffer-highlights buffer)

      ;; Apply new highlights
      (dolist (highlight highlight-data)
        (let ((line (alist-get 'line highlight))
              (type (alist-get 'type highlight))
              (properties (alist-get 'properties highlight)))
          (when (and line type)
            (codeawareness--add-highlight buffer line type properties)))))))

(defun codeawareness--convert-hl-to-highlights (hl-data)
  "Convert hl data structure to highlight format. HL-DATA should be an array
of line numbers. Returns a list of highlight alists with \\='line and \\='type keys."
  (let ((highlights '()))
    ;; Handle both lists and vectors (JSON arrays are parsed as vectors)
    (when (and (or (listp hl-data) (vectorp hl-data))
               (> (length hl-data) 0))
      (dolist (line-number (if (vectorp hl-data)
                               (append hl-data nil)
                             hl-data))
        (when (numberp line-number)
          ;; Convert 0-based line numbers to 1-based (Emacs convention)
          (let ((emacs-line (1+ line-number)))
            (push `((line . ,emacs-line)
                    (type . modified)
                    (properties . ((source . hl))))
                  highlights)))))
    highlights))

;;; HL-Line Integration Functions

(defun codeawareness--init-hl-line-faces ()
  "Initialize hl-line faces for different highlight types."
  (when (featurep 'hl-line)
    (setq codeawareness--hl-line-faces
          `((conflict . ,(make-face 'codeawareness-hl-line-conflict))
            (overlap . ,(make-face 'codeawareness-hl-line-overlap))
            (peer . ,(make-face 'codeawareness-hl-line-peer))
            (modified . ,(make-face 'codeawareness-hl-line-modified))))
    ;; Set face properties based on theme
    (let ((conflict-face (alist-get 'conflict codeawareness--hl-line-faces))
          (overlap-face (alist-get 'overlap codeawareness--hl-line-faces))
          (peer-face (alist-get 'peer codeawareness--hl-line-faces))
          (modified-face (alist-get 'modified codeawareness--hl-line-faces)))
      (if (eq (frame-parameter nil 'background-mode) 'dark)
          ;; Dark theme colors - more prominent
          (progn
            (set-face-attribute conflict-face nil :background "#ff0000" :foreground "#ffffff" :extend t)
            (set-face-attribute overlap-face nil :background "#ff8800" :foreground "#ffffff" :extend t)
            (set-face-attribute peer-face nil :background "#0088ff" :foreground "#ffffff" :extend t)
            (set-face-attribute modified-face nil :background "#13547f" :foreground "#ffffff" :extend t))
        ;; Light theme colors - more prominent
        (progn
          (set-face-attribute conflict-face nil :background "#ffcccc" :foreground "#cc0000" :extend t)
          (set-face-attribute overlap-face nil :background "#ffdd88" :foreground "#884400" :extend t)
          (set-face-attribute peer-face nil :background "#88ccff" :foreground "#004488" :extend t)
          (set-face-attribute modified-face nil :background "#a0e1a4" :foreground "#004400" :extend t))))
    (codeawareness-log-info "HL-line faces initialized")))

(defun codeawareness--get-hl-line-face (type)
  "Get the hl-line face for the given highlight type."
  (or (alist-get type codeawareness--hl-line-faces)
      ;; Fallback to default hl-line face if not found
      'hl-line))

(defun codeawareness--add-hl-line-highlight (buffer line-number type &optional properties)
  "Add a highlight using hl-line mode to the specified line in the given buffer."
  (when (and buffer line-number type (featurep 'hl-line))
    ;; Ensure hl-line faces are initialized
    (unless codeawareness--hl-line-faces
      (codeawareness--init-hl-line-faces))
    (let* ((face (codeawareness--get-hl-line-face type))
           ;; Use save-excursion to get absolute line positions regardless of cursor position
           (overlay (with-current-buffer buffer
                      (make-overlay (save-excursion
                                      ;; Suppress warning: goto-line is needed for absolute positioning
                                      (with-suppressed-warnings ((interactive-only goto-line))
                                        (goto-line line-number))
                                      (line-beginning-position))
                                    (save-excursion
                                      ;; Suppress warning: goto-line is needed for absolute positioning
                                      (with-suppressed-warnings ((interactive-only goto-line))
                                        (goto-line (1+ line-number)))
                                      (line-beginning-position))
                                    buffer t nil))))
      (codeawareness-log-info "Created hl-line overlay for line %d in buffer %s with face %s"
                              line-number (buffer-name buffer) face)
      (overlay-put overlay 'face face)
      (overlay-put overlay 'codeawareness-type 'hl-line-highlight)
      (overlay-put overlay 'codeawareness-line line-number)
      (overlay-put overlay 'codeawareness-highlight-type type)
      (overlay-put overlay 'codeawareness-properties properties)
      ;; Store highlight information
      (let ((buffer-highlights (gethash buffer codeawareness--hl-line-overlays)))
        (unless buffer-highlights
          (setq buffer-highlights (make-hash-table :test 'equal))
          (puthash buffer buffer-highlights codeawareness--hl-line-overlays))
        (puthash line-number overlay buffer-highlights))
      overlay)))

(defun codeawareness--clear-buffer-hl-line-highlights (buffer)
  "Clear all Code Awareness hl-line highlights from the given buffer."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get overlay 'codeawareness-type) 'hl-line-highlight)
          (delete-overlay overlay))))
    ;; Remove from highlights hash table
    (remhash buffer codeawareness--hl-line-overlays)
    (codeawareness-log-info "Cleared hl-line highlights for buffer %s" buffer)))

(defun codeawareness--apply-hl-line-highlights-from-data (buffer highlight-data)
  "Apply hl-line highlights to buffer based on data from the local service."
  (when (and buffer (buffer-live-p buffer) highlight-data (featurep 'hl-line))
    ;; Clear existing highlights first
    (codeawareness--clear-buffer-hl-line-highlights buffer)
    (codeawareness-log-info "Applying hl-line highlights to buffer %s, count: %d"
                            (buffer-name buffer) (length highlight-data))
    ;; Apply new highlights
    (dolist (highlight highlight-data)
      (let ((line (alist-get 'line highlight))
            (type (alist-get 'type highlight))
            (properties (alist-get 'properties highlight)))
        (when (and line type)
          (codeawareness-log-info "Adding hl-line highlight for line %d, type %s" line type)
          (run-with-timer 1.0 nil (codeawareness--add-hl-line-highlight buffer line type properties)))))))

;;; IPC Communication

(defun codeawareness--generate-guid ()
  "Generate a unique GUID for this Emacs instance."
  (concat (number-to-string (emacs-pid)) "-" (number-to-string (random 1000000))))

(defun codeawareness--get-socket-path (guid)
  "Get the socket path for the given GUID."
  (if (eq system-type 'windows-nt)
      (format "\\\\.\\pipe\\caw.%s" guid)
    (format "%s/caw.%s" (directory-file-name (temporary-file-directory)) guid)))

(defun codeawareness--get-catalog-socket-path ()
  "Get the catalog socket path."
  (codeawareness--get-socket-path codeawareness-catalog))

(defun codeawareness--ipc-sentinel (_process event)
  "Handle IPC process sentinel events."
  (codeawareness-log-info "Code Awareness IPC: %s" event)
  (cond
   ((string-match "failed" event)
    (codeawareness-log-error "Local service connection failed")
    (setq codeawareness--connected nil)
    ;; Retry connection
    (run-with-timer 2.0 nil #'codeawareness--connect-to-local-service))
   ((string-match "exited" event)
    (codeawareness-log-warn "Local service connection closed")
    (setq codeawareness--connected nil))
   ((string-match "connection broken by remote peer" event)
    (codeawareness-log-warn "Local service rejected connection")
    (setq codeawareness--connected nil)
    ;; Retry connection after a delay
    (run-with-timer 2.0 nil #'codeawareness--connect-to-local-service))
   ((string-match "open" event)
    (codeawareness-log-info "Successfully connected to local service")
    (setq codeawareness--connected t)
    ;; Initialize workspace after connection (like VS Code)
    (codeawareness--init-workspace))
   (t
    (codeawareness-log-warn "Unknown IPC sentinel event: %s" event))))

(defun codeawareness--ipc-filter (process data)
  "Handle IPC process data."
  ;;(codeawareness-log-info "Received IPC data: %s" data)
  (let ((buffer (process-buffer process)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert data)
        (codeawareness--process-ipc-messages)))))

(defun codeawareness--process-ipc-messages ()
  "Process complete IPC messages from the buffer."
  (let ((delimiter "\f"))
    (goto-char (point-min))
    (while (search-forward delimiter nil t)
      (let* ((end-pos (point))
             (start-pos (point-min))
             (message (buffer-substring-no-properties start-pos (1- end-pos))))
        (delete-region start-pos end-pos)
        (codeawareness--handle-ipc-message message)))))

(defun codeawareness--handle-ipc-message (message)
  "Handle a single IPC message."
  (condition-case err
      (let* ((data (json-read-from-string message))
             (flow (alist-get 'flow data))
             (domain (alist-get 'domain data))
             (action (alist-get 'action data))
             (response-data (alist-get 'data data))
             (error-data (alist-get 'err data)))
        (codeawareness-log-info "%s:%s" domain action)
        ;; (codeawareness-log-info "Raw message: %S" message)
        ;; (codeawareness-log-info "Parsed data: %S" data)
        ;; (codeawareness-log-info "Response data: %S" response-data)
        (if (and (string= flow "res") action)
            (codeawareness--handle-response domain action response-data)
          (if (and (string= flow "err") action)
              (codeawareness--handle-error domain action error-data)
            (if (and (string= flow "req") action)
                (progn
                  (codeawareness-log-info "Received request: %s:%s" domain action)
                  ;; Handle events using the events table
                  (let ((event-key (format "%s:%s" domain action))
                        (handler (gethash action codeawareness--events-table)))
                    (if handler
                        (funcall handler response-data)
                      (codeawareness-log-info "No handler for event: %s" event-key))))
              (codeawareness-log-warn "Unknown message format: %s" message)))))
    (error
     (codeawareness-log-error "Error parsing IPC message: %s" err))))

(defun codeawareness--handle-response (domain action data)
  "Handle an IPC response."
  (let* ((key (format "res:%s:%s" domain action))
         (handler (gethash key codeawareness--response-handlers)))
    ;; Handle auth responses automatically (they may come from external sources)
    (if (and (string= domain "*") (or (string= action "auth:info") (string= action "auth:login")))
        (codeawareness--handle-auth-info-response data)
      ;; Handle other responses with registered handlers
      (when handler
        (remhash key codeawareness--response-handlers)
        (funcall handler data)))))

(defun codeawareness--handle-repo-active-path-response (data &optional expected-file-path)
  "Handle response from repo:active-path request. EXPECTED-FILE-PATH is the
   file path that was originally requested (for validation)."
  (codeawareness-log-info "Received repo:active-path response")
  ;; Add the project to our store
  (codeawareness--add-project data)
  ;; Extract and apply highlights from the hl data structure
  (let* ((hl-data (alist-get 'hl data))
         (buffer codeawareness--active-buffer))
    (codeawareness-log-info "hl-data: %s, buffer: %s" hl-data (if buffer (buffer-name buffer) "nil"))
    (if (and hl-data buffer (buffer-live-p buffer))
        ;; Validate that the buffer still corresponds to the expected file
        (let ((current-file-path (buffer-file-name buffer)))
          (if (and expected-file-path current-file-path
                   (string= (codeawareness--cross-platform-path expected-file-path)
                            (codeawareness--cross-platform-path current-file-path)))
              ;; File paths match, apply highlights
              (progn
                (codeawareness-log-info "Applying highlights to buffer %s (file: %s)"
                                        (buffer-name buffer) current-file-path)
                ;; Convert hl data to highlight format
                (let ((highlights (codeawareness--convert-hl-to-highlights hl-data)))
                  (when highlights
                    (codeawareness--apply-highlights-from-data buffer highlights)))))))))

(defun codeawareness--handle-auth-info-response (data)
  "Handle response from auth:info request."
  ;;(codeawareness-log-info "Received auth:info response")
  ;; (codeawareness-log-info "Auth data: %S" data)
  (if (and data (listp data) (alist-get 'user data))
      (progn
        (setq codeawareness--user (alist-get 'user data))
        (setq codeawareness--tokens (alist-get 'tokens data))
        (setq codeawareness--authenticated t)
        (codeawareness-log-info "Authentication successful")
        (message "Authenticated as %s" (alist-get 'name codeawareness--user))
        ;; Refresh active file immediately after authentication (like VSCode's init function)
        (codeawareness--refresh-active-file))
    (setq codeawareness--authenticated nil)
    (codeawareness-log-warn "No authentication data received - user needs to authenticate")))

(defun codeawareness--handle-peer-select (peer-data)
  "Handle peer selection event from Muninn app."
  (codeawareness-log-info "Peer selected: %s" (alist-get 'name peer-data))
  (setq codeawareness--selected-peer peer-data)

  ;; Get active project information
  (let* ((active-project codeawareness--active-project)
         (origin (alist-get 'origin active-project))
         (fpath (alist-get 'activePath active-project)))
    (if (not fpath)
        (codeawareness-log-warn "No active file path for peer diff")
      ;; Send request for peer diff
      (let ((message-data `((origin . ,origin)
                            (fpath . ,fpath)
                            (caw . ,codeawareness--guid)
                            (peer . ,peer-data))))
        (codeawareness-log-info "Requesting peer diff for %s" fpath)
        (codeawareness--transmit "repo:diff-peer" message-data)
        (codeawareness--setup-response-handler "code" "repo:diff-peer")))))

(defun codeawareness--handle-peer-unselect ()
  "Handle peer unselection event from Muninn app."
  (codeawareness-log-info "Peer unselected")
  (setq codeawareness--selected-peer nil)
  ;; Close any open diff buffers
  (codeawareness--close-diff-buffers))

(defun codeawareness--handle-peer-diff-response (data)
  "Handle response from repo:diff-peer request."
  (codeawareness-log-info "Received peer diff response")
  (let* ((peer-file (alist-get 'peerFile data))
         (title (alist-get 'title data))
         (active-project codeawareness--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (user-file (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (and peer-file user-file)
        (progn
          (codeawareness-log-info "Opening diff: %s vs %s" peer-file user-file)
          (codeawareness--open-diff-view peer-file user-file title))
      (codeawareness-log-error "Missing file paths for diff: peer-file=%s, user-file=%s"
                               peer-file user-file))))

(defun codeawareness--open-diff-view (peer-file user-file title)
  "Open a diff view comparing peer file with user file."
  (let* ((peer-buffer (find-file-noselect peer-file))
         (user-buffer (find-file-noselect user-file)))
    ;; Use ediff for a better diff experience if available
    (if (fboundp 'ediff-buffers)
        (progn
          (codeawareness-log-info "Using ediff for diff view")
          (ediff-buffers peer-buffer user-buffer))
      ;; Fallback to diff-mode in a separate buffer
      (let ((diff-buffer-name (format "*CodeAwareness Diff: %s*" title)))
        (let ((diff-buffer (get-buffer-create diff-buffer-name)))
          (with-current-buffer diff-buffer
            ;; Clear the buffer
            (erase-buffer)
            ;; Insert diff content
            (let ((diff-output (codeawareness--generate-diff peer-file user-file)))
              (insert diff-output)
              ;; Set up the buffer for diff viewing
              (diff-mode)
              ;; Make the buffer read-only
              (setq buffer-read-only t)
              ;; Display the buffer
              (switch-to-buffer diff-buffer)
              (message "Opened diff view: %s" title))))))))

(defun codeawareness--generate-diff (file1 file2)
  "Generate diff output between two files."
  (let ((diff-command (format "diff -u %s %s" file1 file2)))
    (with-temp-buffer
      (let ((exit-code (call-process-shell-command diff-command nil t)))
        (if (= exit-code 0)
            "Files are identical"
          (buffer-string))))))

(defun codeawareness--close-diff-buffers ()
  "Close all CodeAwareness diff buffers."
  (dolist (buffer (buffer-list))
    (when (and (string-match "\\*CodeAwareness Diff:" (buffer-name buffer))
               (buffer-live-p buffer))
      (kill-buffer buffer)))
  (message "Closed CodeAwareness diff buffers"))

(defun codeawareness-show-peer-diff ()
  "Manually trigger peer diff for testing purposes."
  (interactive)
  (if codeawareness--selected-peer
      (let* ((active-project codeawareness--active-project)
             (origin (alist-get 'origin active-project))
             (fpath (alist-get 'activePath active-project)))
        (if (not fpath)
            (message "No active file path for peer diff")
          (let ((message-data `((origin . ,origin)
                                (fpath . ,fpath)
                                (caw . ,codeawareness--guid)
                                (peer . ,codeawareness--selected-peer))))
            (codeawareness-log-info "Manually requesting peer diff for %s" fpath)
            (codeawareness--transmit "repo:diff-peer" message-data)
            (codeawareness--setup-response-handler "code" "repo:diff-peer"))))
    (message "No peer selected")))

;;; Additional Event Handlers

(defun codeawareness--handle-branch-select (branch)
  "Handle branch selection event."
  (codeawareness-log-info "Branch selected: %s" branch)
  (let ((message-data `((branch . ,branch)
                        (caw . ,codeawareness--guid))))
    (codeawareness--transmit "repo:diff-branch" message-data)
    (codeawareness--setup-response-handler "code" "repo:diff-branch")))

(defun codeawareness--handle-branch-unselect ()
  "Handle branch unselection event."
  (codeawareness-log-info "Branch unselected")
  (codeawareness--close-diff-buffers))

(defun codeawareness--handle-branch-refresh (_data)
  "Handle branch refresh event."
  (codeawareness-log-info "Branch refresh requested")
  ;; TODO: Implement branch refresh using git and display in panel
  (message "Branch refresh not yet implemented"))

(defun codeawareness--handle-auth-logout ()
  "Handle auth logout event."
  (codeawareness-log-info "Auth logout requested")
  (codeawareness--clear-store)
  (codeawareness--clear-all-highlights)
  (message "Logged out"))

(defun codeawareness--handle-context-add (context)
  "Handle context add event."
  (codeawareness-log-info "Context add requested: %s" context)
  (let* ((active-project codeawareness--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (full-path (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (not full-path)
        (codeawareness-log-warn "No active file path for context add")
      (let ((message-data `((fpath . ,full-path)
                            (selections . ,codeawareness--active-selections)
                            (context . ,context)
                            (op . "add")
                            (caw . ,codeawareness--guid))))
        (codeawareness--transmit "context:apply" message-data)
        (codeawareness--setup-response-handler "code" "context:apply")))))

(defun codeawareness--handle-context-del (context)
  "Handle context delete event."
  (codeawareness-log-info "Context delete requested: %s" context)
  (let* ((active-project codeawareness--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (full-path (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (not full-path)
        (codeawareness-log-warn "No active file path for context delete")
      (let ((message-data `((fpath . ,full-path)
                            (selections . ,codeawareness--active-selections)
                            (context . ,context)
                            (op . "del")
                            (caw . ,codeawareness--guid))))
        (codeawareness--transmit "context:apply" message-data)
        (codeawareness--setup-response-handler "code" "context:apply")))))

(defun codeawareness--handle-context-open-rel (data)
  "Handle context open relative event."
  (codeawareness-log-info "Context open relative requested: %s" (alist-get 'sourceFile data))
  (let ((source-file (alist-get 'sourceFile data)))
    (when source-file
      (find-file source-file))))

;;; Response Handlers

(defun codeawareness--handle-branch-diff-response (data)
  "Handle response from repo:diff-branch request."
  (codeawareness-log-info "Received branch diff response")
  (let* ((peer-file (alist-get 'peerFile data))
         (user-file (alist-get 'userFile data))
         (title (alist-get 'title data)))
    (if (and peer-file user-file)
        (progn
          (codeawareness-log-info "Opening branch diff: %s vs %s" peer-file user-file)
          (codeawareness--open-diff-view peer-file user-file title))
      (codeawareness-log-error "Missing file paths for branch diff: peer-file=%s, user-file=%s"
                               peer-file user-file))))

(defun codeawareness--handle-context-apply-response (_data)
  "Handle response from context:apply request."
  (codeawareness-log-info "Received context apply response")
  ;; TODO: Handle context update response
  (message "Context applied successfully"))

(defun codeawareness--handle-error (domain action error-data)
  "Handle an IPC error."
  (let* ((key (format "err:%s:%s" domain action))
         (handler (gethash key codeawareness--response-handlers)))
    (when handler
      (remhash key codeawareness--response-handlers)
      (funcall handler error-data))))

(defun codeawareness--transmit (action data)
  "Transmit a message to the local service."
  (let* ((domain (if (member action '("auth:info" "auth:login")) "*" "code"))
         (flow "req")
         (message (json-encode `((flow . ,flow)
                                 (domain . ,domain)
                                 (action . ,action)
                                 (data . ,data)
                                 (caw . ,codeawareness--guid)))))
    (if codeawareness--ipc-process
        (if (eq (process-status codeawareness--ipc-process) 'open)
            (progn
              (codeawareness-log-info "Sending %s:%s" domain action)
              (process-send-string codeawareness--ipc-process (concat message "\f"))
              (codeawareness--setup-response-handler domain action))
          (codeawareness-log-error "IPC process exists but is not open (status: %s)"
                                   (process-status codeawareness--ipc-process)))
      (codeawareness-log-error "No IPC process available for transmission"))))

(defun codeawareness--setup-response-handler (domain action &optional file-path)
  "Setup response handlers for the given domain and action.
FILE-PATH is the file path associated with this request (for validation)."
  (let ((res-key (format "res:%s:%s" domain action))
        (err-key (format "err:%s:%s" domain action)))
    ;; Set up specific handlers for known actions
    (cond
     ((string= (format "%s:%s" domain action) "code:repo:active-path")
      (puthash res-key (lambda (data) (codeawareness--handle-repo-active-path-response data file-path)) codeawareness--response-handlers))
     ((string= (format "%s:%s" domain action) "code:repo:diff-peer")
      (puthash res-key #'codeawareness--handle-peer-diff-response codeawareness--response-handlers))
     ((string= (format "%s:%s" domain action) "code:repo:diff-branch")
      (puthash res-key #'codeawareness--handle-branch-diff-response codeawareness--response-handlers))
     ((string= (format "%s:%s" domain action) "code:context:apply")
      (puthash res-key #'codeawareness--handle-context-apply-response codeawareness--response-handlers))
     ((or (string= (format "%s:%s" domain action) "*:auth:info")
          (string= (format "%s:%s" domain action) "*:auth:login"))
      (puthash res-key #'codeawareness--handle-auth-info-response codeawareness--response-handlers))
     (t
      (puthash res-key #'codeawareness--handle-success codeawareness--response-handlers)))
    (puthash err-key #'codeawareness--handle-failure codeawareness--response-handlers)))

(defun codeawareness--handle-success (data)
  "Handle successful IPC response."
  (codeawareness-log-info "Success - %s" (format "%s" data)))

(defun codeawareness--handle-failure (error-data)
  "Handle failed IPC response."
  (codeawareness-log-error "Error - %s" (format "%s" error-data)))

;;; Connection Management

(defun codeawareness--init-ipc ()
  "Initialize IPC communication."
  (setq codeawareness--guid (codeawareness--generate-guid))
  (codeawareness-log-info "Initializing IPC with GUID %s" (format "%s" codeawareness--guid))
  (codeawareness--connect-to-catalog))

(defun codeawareness--connect-to-catalog ()
  "Connect to the catalog service."
  (let* ((catalog-path (codeawareness--get-catalog-socket-path))
         (process-name "codeawareness-catalog")
         (buffer-name "*codeawareness-catalog*"))
    (codeawareness-log-info "Connecting to catalog")
    (condition-case err
        (progn
          (setq codeawareness--ipc-catalog-process
                (make-network-process
                 :name process-name
                 :buffer buffer-name
                 :family 'local
                 :service catalog-path
                 :sentinel #'codeawareness--catalog-sentinel
                 :filter #'codeawareness--catalog-filter
                 :noquery t))
          (codeawareness-log-info "Catalog connection initiated")
          ;; Check process status immediately and after a delay
          (codeawareness--check-catalog-process-status)
          (run-with-timer 0.5 nil #'codeawareness--check-catalog-process-status))
      (error
       (codeawareness-log-error "Failed to create catalog connection: %s" err)
       (message "Failed to connect to catalog service at %s. Error: %s"
                catalog-path err)))))

(defun codeawareness--catalog-sentinel (process event)
  "Handle catalog process sentinel events."
  (codeawareness-log-info "Code Awareness Catalog: SENTINEL CALLED with event: %s" event)
  (codeawareness-log-info "Code Awareness Catalog: Process: %s" process)
  (cond
   ((string-match "failed" event)
    (codeawareness-log-error "Failed to connect to catalog service at %s"
                             (codeawareness--get-catalog-socket-path))
    (message "Failed to connect to catalog service. Check if the service is running on %s"
             (codeawareness--get-catalog-socket-path))
    (setq codeawareness--connected nil))
   ((string-match "exited" event)
    (codeawareness-log-warn "Catalog connection closed")
    (setq codeawareness--connected nil))
   ((string-match "open" event)
    (codeawareness-log-info "Successfully connected to catalog service")
    (message "Connected to catalog service")
    (setq codeawareness--connected t)
    ;; Send 'connected' message to trigger client registration (matching VSCode behavior)
    (codeawareness--catalog-filter "connected"))))

(defun codeawareness--catalog-filter (data)
  "Handle catalog process data."
  (codeawareness-log-info "Code Awareness Catalog: Received data: %s" data)
  (when (string= data "connected")
    (codeawareness-log-info "Catalog connection established, registering client")
    (codeawareness--register-client)))

(defun codeawareness--register-client ()
  "Register this client with the catalog service."
  (unless codeawareness--client-registered
    (let ((message (json-encode `((flow . "req")
                                  (domain . "*")
                                  (action . "clientId")
                                  (data . ,codeawareness--guid)
                                  (caw . ,codeawareness--guid)))))
      (when codeawareness--ipc-catalog-process
        (process-send-string codeawareness--ipc-catalog-process (concat message "\f"))
        (codeawareness-log-info "Client registered")
        (setq codeawareness--client-registered t)
        (codeawareness--init-server))))
  (codeawareness-log-info "Client already registered, skipping"))

(defun codeawareness--init-server ()
  "Initialize the server connection."
  (codeawareness-log-info "Initializing server connection")
  ;; Start polling for local service socket with exponential backoff
  (codeawareness--poll-for-local-service))

(defun codeawareness--init-workspace ()
  "Initialize workspace."
  (codeawareness-log-info "Workspace initialized")
  ;; Send auth:info request after a short delay to ensure connection is ready
  (run-with-timer 0.1 nil #'codeawareness--send-auth-info))

(defun codeawareness--send-auth-info ()
  "Send auth:info request to the local service."
  (codeawareness-log-info "Sending auth:info request")
  (if (and codeawareness--ipc-process
           (eq (process-status codeawareness--ipc-process) 'open))
      (codeawareness--transmit "auth:info" nil)
    (codeawareness-log-error "IPC process not ready for auth:info request")))

(defvar codeawareness--max-poll-attempts 10
  "Maximum number of polling attempts.")

(defun codeawareness--poll-for-local-service ()
  "Poll for local service socket with exponential backoff."
  (let ((socket-path (codeawareness--get-socket-path codeawareness--guid)))
    (if (file-exists-p socket-path)
        (progn
          (codeawareness-log-info "Local service socket found")
          (setq codeawareness--poll-attempts 0)
          (codeawareness--connect-to-local-service))
      (if (>= codeawareness--poll-attempts codeawareness--max-poll-attempts)
          (progn
            (codeawareness-log-error "Failed to find local service socket after %d attempts"
                                     codeawareness--max-poll-attempts)
            (message "Failed to connect to local service after %d attempts"
                     codeawareness--max-poll-attempts))
        (setq codeawareness--poll-attempts (1+ codeawareness--poll-attempts))
        ;; Exponential backoff: 0.5s, 1s, 2s, 4s, 8s, etc.
        (let ((delay (expt 2 (1- codeawareness--poll-attempts))))
          (run-with-timer delay nil #'codeawareness--poll-for-local-service))))))

(defun codeawareness--connect-to-local-service ()
  "Connect to the local service with retry logic."
  (let* ((socket-path (codeawareness--get-socket-path codeawareness--guid))
         (process-name (format "codeawareness-ipc-%s" codeawareness--guid))
         (buffer-name (format "*%s*" process-name)))
    (codeawareness-log-info "Current GUID: %s" codeawareness--guid)
    (codeawareness-log-info "Attempting to connect to local service at %s" socket-path)
    (codeawareness-log-info "Socket exists: %s" (file-exists-p socket-path))

    (condition-case err
        (progn
          (codeawareness-log-info "Creating network process...")
          (setq codeawareness--ipc-process
                (make-network-process
                 :name process-name
                 :buffer buffer-name
                 :family 'local
                 :service socket-path
                 :sentinel #'codeawareness--ipc-sentinel
                 :filter #'codeawareness--ipc-filter
                 :noquery t))
          (codeawareness-log-info "Connected to local service")
          ;; Set up a timeout to detect stuck connections
          (run-with-timer 5.0 nil #'codeawareness--check-connection-timeout)
          ;; Set up a fallback to trigger workspace init if sentinel doesn't fire
          (run-with-timer 1.0 nil #'codeawareness--fallback-workspace-init))
      (error
       (codeawareness-log-warn "Failed to connect to local service, will retry in 5 seconds")
       (codeawareness-log-warn "Error: %s" err)
       ;; Schedule retry
       (run-with-timer 5.0 nil #'codeawareness--connect-to-local-service)))))

(defun codeawareness--check-connection-timeout ()
  "Check if the connection is stuck and handle timeout."
  (when (and codeawareness--ipc-process
             (not codeawareness--connected))
    (let ((status (process-status codeawareness--ipc-process)))
      (if (eq status 'connect)
          (progn
            (codeawareness-log-error "Connection stuck, retrying")
            (delete-process codeawareness--ipc-process)
            (setq codeawareness--ipc-process nil)
            (run-with-timer 1.0 nil #'codeawareness--connect-to-local-service))))))

(defun codeawareness--fallback-workspace-init ()
  "Fallback workspace initialization if sentinel doesn't fire."
  (when (and codeawareness--ipc-process
             (eq (process-status codeawareness--ipc-process) 'open)
             (not codeawareness--connected))
    (codeawareness-log-info "Using fallback workspace init")
    (codeawareness--init-workspace)))

(defun codeawareness--force-cleanup ()
  "Force cleanup of all Code Awareness processes and state."
  (codeawareness-log-info "Force cleaning up all processes")

  ;; Cancel any pending timers
  (when codeawareness--update-timer
    (cancel-timer codeawareness--update-timer)
    (setq codeawareness--update-timer nil))
  (when codeawareness--highlight-timer
    (cancel-timer codeawareness--highlight-timer)
    (setq codeawareness--highlight-timer nil))

  ;; Force delete processes regardless of status
  (when codeawareness--ipc-catalog-process
    (condition-case err
        (progn
          ;; Try to close the process gracefully first
          (when (eq (process-status codeawareness--ipc-catalog-process) 'open)
            (process-send-eof codeawareness--ipc-catalog-process))
          ;; Force delete the process
          (delete-process codeawareness--ipc-catalog-process)
          (codeawareness-log-info "Force deleted catalog process"))
      (error
       (codeawareness-log-error "Error deleting catalog process: %s" err)))
    (setq codeawareness--ipc-catalog-process nil))

  (when codeawareness--ipc-process
    (condition-case err
        (progn
          ;; Try to close the process gracefully first
          (when (eq (process-status codeawareness--ipc-process) 'open)
            (process-send-eof codeawareness--ipc-process))
          ;; Force delete the process
          (delete-process codeawareness--ipc-process)
          (codeawareness-log-info "Force deleted IPC process"))
      (error
       (codeawareness-log-error "Error deleting IPC process: %s" err)))
    (setq codeawareness--ipc-process nil))

  ;; Remove hooks
  (remove-hook 'after-save-hook #'codeawareness--after-save-hook)
  (remove-hook 'post-command-hook #'codeawareness--post-command-hook)

  ;; Reset all state
  (setq codeawareness--connected nil
        codeawareness--authenticated nil
        codeawareness--active-buffer nil
        codeawareness--active-project nil
        codeawareness--guid nil
        codeawareness--client-registered nil
        codeawareness--poll-attempts 0)

  (codeawareness-log-info "Force cleanup completed"))

(defun codeawareness--send-disconnect-messages ()
  "Send disconnect messages to catalog service."
  (codeawareness-log-info "Sending disconnect messages")

  ;; Send disconnect message to catalog
  (when (and codeawareness--ipc-catalog-process
             (eq (process-status codeawareness--ipc-catalog-process) 'open))
    (let ((message (json-encode `((flow . "req")
                                  (domain . "*")
                                  (action . "clientDisconnect")
                                  (data . ,codeawareness--guid)
                                  (caw . ,codeawareness--guid)))))
      (codeawareness-log-info "Sending clientDisconnect to catalog")
      (condition-case err
          (process-send-string codeawareness--ipc-catalog-process (concat message "\f"))
        (error
         (codeawareness-log-error "Failed to send clientDisconnect to catalog: %s" err))))))

(defun codeawareness--check-catalog-process-status ()
  "Check the status of the catalog process."
  (when codeawareness--ipc-catalog-process
    (let ((status (process-status codeawareness--ipc-catalog-process)))
      (if (eq status 'open)
          (progn
            (codeawareness-log-info "Catalog connected")
            ;; If client is not registered yet, trigger registration as fallback
            (unless codeawareness--client-registered
              (codeawareness-log-info "Registering client")
              (codeawareness--catalog-filter "connected")))
        (codeawareness-log-error "Catalog process not open, status: %s" status)))))

;;; Buffer Management

(defun codeawareness--schedule-update ()
  "Schedule a debounced update."
  (when codeawareness--update-timer
    (cancel-timer codeawareness--update-timer))
  (setq codeawareness--update-timer
        (run-with-timer codeawareness-update-delay nil #'codeawareness--update)))

(defun codeawareness--update ()
  "Update Code Awareness for the current buffer."
  (setq codeawareness--update-timer nil)
  (when (and codeawareness--active-buffer
             (buffer-live-p codeawareness--active-buffer)
             (buffer-file-name codeawareness--active-buffer))
    (let ((filename (buffer-file-name codeawareness--active-buffer)))
      (codeawareness--transmit "repo:file-saved"
                               `((fpath . ,filename)
                                 (doc . ,(buffer-string))
                                 (caw . ,codeawareness--guid))))))

;;; Hooks and Event Handling

(defun codeawareness--after-save-hook ()
  "Hook function for after-save-hook."
  (codeawareness--update))

(defun codeawareness--post-command-hook ()
  "Hook function for post-command-hook."
  (let ((current-buffer (current-buffer)))
    (when (and current-buffer
               (not (eq current-buffer codeawareness--active-buffer)))
      (codeawareness-log-info "Buffer switch detected: %s -> %s (has-file: %s)"
                              (if codeawareness--active-buffer (buffer-name codeawareness--active-buffer) "nil")
                              (buffer-name current-buffer)
                              (if (buffer-file-name current-buffer) "yes" "no"))

      (if (buffer-file-name current-buffer)
          ;; Only update active buffer if switching to a different file
          (let ((current-file (buffer-file-name current-buffer))
                (active-file (when codeawareness--active-buffer
                               (buffer-file-name codeawareness--active-buffer))))
            (if (and codeawareness--active-buffer active-file
                     (string= current-file active-file))
                ;; Same file, no need to update active buffer
                (codeawareness-log-info "Switched to same file buffer")
              ;; Different file or no active buffer, update and refresh
              (progn
                (codeawareness-log-info "Active buffer changed to %s" current-file)
                (setq codeawareness--active-buffer current-buffer)
                (codeawareness--refresh-active-file))))
        ;; Don't clear active buffer when switching to non-file buffers
        (codeawareness-log-info "Switched to buffer without file, keeping active buffer")))))

;;; Public API

(defun codeawareness-refresh ()
  "Refresh Code Awareness data."
  (interactive)
  (codeawareness--refresh-active-file))

(defun codeawareness-clear-highlights ()
  "Clear all Code Awareness highlights from the current buffer."
  (interactive)
  (codeawareness--clear-buffer-highlights (current-buffer))
  (message "Cleared highlights"))

(defun codeawareness-clear-all-highlights ()
  "Clear all Code Awareness highlights from all buffers."
  (interactive)
  (codeawareness--clear-all-highlights)
  (message "Cleared all highlights"))

(defun codeawareness-refresh-highlights ()
  "Refresh highlights for the current buffer."
  (interactive)
  (codeawareness--refresh-buffer-highlights (current-buffer))
  (message "Refreshed highlights"))

(defun codeawareness-auth-status ()
  "Show the current authentication status."
  (interactive)
  (if codeawareness--authenticated
      (message "Authenticated as %s" (alist-get 'name codeawareness--user))
    (message "Not authenticated")))

(defun codeawareness-connection-status ()
  "Show the current connection status."
  (interactive)
  (message "Catalog connected: %s, Local service connected: %s, Authenticated: %s, Client registered: %s"
           (if (and codeawareness--ipc-catalog-process
                    (eq (process-status codeawareness--ipc-catalog-process) 'open)) "yes" "no")
           (if (and codeawareness--ipc-process
                    (eq (process-status codeawareness--ipc-process) 'open)) "yes" "no")
           (if codeawareness--authenticated "yes" "no")
           (if codeawareness--client-registered "yes" "no")))

(defun codeawareness-retry-connection ()
  "Manually retry the connection to the local service."
  (interactive)
  (codeawareness-log-info "Manual connection retry requested")
  (when codeawareness--ipc-process
    (delete-process codeawareness--ipc-process)
    (setq codeawareness--ipc-process nil))
  (setq codeawareness--connected nil)
  (run-with-timer 0.5 nil #'codeawareness--connect-to-local-service)
  (message "Connection retry initiated"))

(defun codeawareness-disconnect ()
  "Manually disconnect from Code Awareness services."
  (interactive)
  (codeawareness-log-info "Manual disconnect requested")
  (codeawareness--send-disconnect-messages)
  (message "Disconnect messages sent"))

(defun codeawareness-test-empty-line-highlighting ()
  "Test highlighting of empty lines to demonstrate the fix."
  (interactive)
  ;; Ensure hl-line faces are initialized
  (when (featurep 'hl-line)
    (codeawareness--init-hl-line-faces))
  (let ((test-data '(((line . 1) (type . modified) (properties . ((test . t))))
                     ((line . 3) (type . conflict) (properties . ((test . t))))
                     ((line . 5) (type . peer) (properties . ((test . t))))
                     ((line . 7) (type . overlap) (properties . ((test . t)))))))
    (message "Testing empty line highlighting...")
    (codeawareness--apply-highlights-from-data (current-buffer) test-data)
    (message "Applied test highlights to lines 1, 3, 5, 7 (including empty lines)")))

(defun codeawareness-test-json-hl-data ()
  "Test highlighting using simulated JSON hl data (0-based line numbers)."
  (interactive)
  ;; Ensure hl-line faces are initialized
  (when (featurep 'hl-line)
    (codeawareness--init-hl-line-faces))
  ;; Simulate JSON hl data with 0-based line numbers (like from local service)
  (let ((json-hl-data [0 2 4 6])) ; 0-based line numbers
    (message "Testing JSON hl data: %s" json-hl-data)
    ;; Convert to highlight format (like the actual flow)
    (let ((highlights (codeawareness--convert-hl-to-highlights json-hl-data)))
      (message "Converted to highlights: %s" highlights)
      ;; Apply highlights using the same pipeline as real JSON data
      (codeawareness--apply-highlights-from-data (current-buffer) highlights)
      (message "Applied JSON hl highlights to lines 1, 3, 5, 7 (0-based: 0, 2, 4, 6)"))))

(defun codeawareness-debug-buffers ()
  "Show debug information about current buffer state."
  (interactive)
  (message "Code Awareness Buffer Debug: Active: %s, Current: %s, Current File: %s"
           (if codeawareness--active-buffer (buffer-name codeawareness--active-buffer) "nil")
           (buffer-name (current-buffer))
           (if (buffer-file-name (current-buffer)) (buffer-file-name (current-buffer)) "nil")))

(defun codeawareness-test-hl-line-faces ()
  "Test if hl-line faces are working correctly."
  (interactive)
  ;; Ensure hl-line faces are initialized
  (when (featurep 'hl-line)
    (codeawareness--init-hl-line-faces))
  ;; Test with a simple overlay using the modified face
  (let* ((face (codeawareness--get-hl-line-face 'modified))
         (overlay (make-overlay (line-beginning-position 1)
                                (line-beginning-position 2)
                                (current-buffer) t nil)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'codeawareness-type 'test)
    (message "Test overlay created with face %s on line 1" face)
    ;; Remove the test overlay after 3 seconds
    (run-with-timer 3.0 nil (lambda () (delete-overlay overlay)))))

(defun codeawareness-test-face-colors ()
  "Test all hl-line face colors to see if they're visible."
  (interactive)
  ;; Ensure hl-line faces are initialized
  (when (featurep 'hl-line)
    (codeawareness--init-hl-line-faces))
  ;; Test each face type
  (dolist (type '(modified conflict peer overlap))
    (let* ((face (codeawareness--get-hl-line-face type))
           (line (+ 2 (cl-position type '(modified conflict peer overlap))))
           (overlay (make-overlay (line-beginning-position line)
                                  (line-beginning-position (1+ line))
                                  (current-buffer) t nil)))
      (overlay-put overlay 'face face)
      (overlay-put overlay 'codeawareness-type 'test)
      (message "Test overlay for %s face on line %d" type line)
      ;; Remove the test overlay after 5 seconds
      (run-with-timer 5.0 nil (lambda () (delete-overlay overlay))))))

;;; Reinit on Emacs restart

(defun codeawareness-reinit-faces ()
  "Force reinitialize hl-line faces."
  (interactive)
  (when (featurep 'hl-line)
    (setq codeawareness--hl-line-faces nil) ; Clear existing faces
    (codeawareness--init-hl-line-faces)
    (message "HL-line faces reinitialized")))

;;; Minor Mode

(define-minor-mode codeawareness-mode
  "Toggle Code Awareness mode. 
  Enable Code Awareness functionality for collaborative development."
  :init-value nil
  :global t
  :lighter " CAW"
  :group 'codeawareness
  (if codeawareness-mode
      (codeawareness--enable)
    (codeawareness--disable)))

(defun codeawareness--enable ()
  "Enable Code Awareness."
  (codeawareness--init-config)
  (codeawareness--init-store)
  (codeawareness--init-event-handlers)
  (codeawareness--init-ipc)
  (codeawareness--init-highlight-faces)
  (add-hook 'after-save-hook #'codeawareness--after-save-hook)
  (add-hook 'post-command-hook #'codeawareness--post-command-hook)
  (add-hook 'buffer-list-update-hook #'codeawareness--buffer-list-update-hook)
  ;; Set the current buffer as active if it has a file (like VSCode's activeTextEditor)
  (when (and (current-buffer) (buffer-file-name (current-buffer)))
    (setq codeawareness--active-buffer (current-buffer)))
  (codeawareness-log-info "Code Awareness enabled"))

(defun codeawareness--disable ()
  "Disable Code Awareness."
  (codeawareness-log-info "Disabling and disconnecting")

  ;; Remove hooks
  (remove-hook 'after-save-hook #'codeawareness--after-save-hook)
  (remove-hook 'post-command-hook #'codeawareness--post-command-hook)
  (remove-hook 'buffer-list-update-hook #'codeawareness--buffer-list-update-hook)

  ;; Clear all highlights
  (codeawareness--clear-all-highlights)

  ;; Send disconnect messages before closing connections
  (codeawareness--send-disconnect-messages)

  ;; Use force cleanup to ensure all processes are properly deleted
  (codeawareness--force-cleanup)

  ;; Clear the store
  (codeawareness--clear-store)

  (codeawareness-log-info "Code Awareness disabled"))

;;; Cleanup on Emacs exit

(defun codeawareness--buffer-list-update-hook ()
  "Hook function for buffer-list-update-hook to detect when buffers are displayed."
  (let ((current-buffer (current-buffer)))
    (when (and current-buffer
               (buffer-file-name current-buffer)
               (not (eq current-buffer codeawareness--active-buffer)))
      (let ((current-file (buffer-file-name current-buffer))
            (active-file (when codeawareness--active-buffer
                           (buffer-file-name codeawareness--active-buffer))))
        (if (and codeawareness--active-buffer active-file
                 (string= current-file active-file))
            ;; Same file, no need to update active buffer
            (codeawareness-log-info "Same file buffer displayed")
          ;; Different file or no active buffer, update and refresh
          (progn
            (codeawareness-log-info "Buffer displayed: %s" current-file)
            (setq codeawareness--active-buffer current-buffer)
            (codeawareness--refresh-active-file)))))))

(defun codeawareness--cleanup-on-exit ()
  "Cleanup Code Awareness when Emacs is about to exit."
  (when codeawareness-mode
    (codeawareness-log-info "Emacs exiting, cleaning up connections")
    ;; Send disconnect messages first, then force cleanup
    (codeawareness--send-disconnect-messages)
    ;; Force synchronous cleanup to ensure processes are deleted
    (codeawareness--force-cleanup)))

;; Register cleanup functions to run when Emacs exits
(add-hook 'kill-emacs-hook #'codeawareness--cleanup-on-exit)

;;; Provide

(provide 'codeawareness)
;;; codeawareness.el ends here
