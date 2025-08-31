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

(defvar codeawareness--last-file-buffer nil
  "Last buffer with a file that was active.")

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
  (codeawareness-log-info "Code Awareness: Configuration initialized"))

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
  (codeawareness-log-info "Code Awareness: Store initialized"))

(defun codeawareness--clear-store ()
  "Clear the store and reset all state."
  (codeawareness-log-info "Code Awareness: Clearing store")
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
  (codeawareness-log-info "Code Awareness: Resetting store")
  (setq codeawareness--peer-fs (make-hash-table :test 'equal)
        codeawareness--active-buffer nil
        codeawareness--active-selections nil)
  (codeawareness--init-store))

;;; Project Management

(defun codeawareness--add-project (project)
  "Add a project to the store."
  (codeawareness-log-info "Code Awareness: Adding project %s" (alist-get 'root project))
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
  (codeawareness-log-info "Code Awareness: refreshActiveFile hook called")
  (codeawareness-log-info "Code Awareness: Authentication status: %s" (if codeawareness--authenticated "yes" "no"))
  (codeawareness-log-info "Code Awareness: IPC process exists: %s" (if codeawareness--ipc-process "yes" "no"))
  (when codeawareness--ipc-process
    (codeawareness-log-info "Code Awareness: IPC process status: %s" (process-status codeawareness--ipc-process)))

  ;; Check if we have the necessary components to send a refresh request
  (let ((fpath (codeawareness--get-active-file-path))
        (doc (codeawareness--get-active-file-content)))
    (if (not fpath)
        (codeawareness-log-info "Code Awareness: No active file to refresh")
      (if (not codeawareness--authenticated)
          (codeawareness-log-warn "Code Awareness: Not authenticated, skipping file refresh")
        (if (not (and codeawareness--ipc-process
                      (eq (process-status codeawareness--ipc-process) 'open)))
            (codeawareness-log-warn "Code Awareness: IPC process not ready, skipping file refresh")
          (codeawareness-log-info "Code Awareness: Refreshing active file %s" fpath)
          (let ((message-data `((fpath . ,(codeawareness--cross-platform-path fpath))
                                (doc . ,doc)
                                (caw . ,codeawareness--guid))))
            (codeawareness--transmit "repo:active-path" message-data)
            (codeawareness--setup-response-handler "code" "repo:active-path")))))))

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

  (codeawareness-log-info "Code Awareness: Highlight faces initialized"))

(defun codeawareness--get-highlight-face (type)
  "Get the face for the given highlight type."
  (alist-get type codeawareness--highlight-faces))

(defun codeawareness--create-line-overlay (buffer line-number face &optional properties)
  "Create an overlay for a specific line in the given buffer. Uses hl-line technique to properly handle empty lines."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let* ((line-count (line-number-at-pos (point-max)))
             (start (line-beginning-position line-number))
             ;; Use hl-line technique: end at start of next line instead of end of current line
             ;; This ensures empty lines get proper overlay span
             (end (line-beginning-position (1+ line-number))))
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
    (codeawareness-log-info "Code Awareness: Cleared highlights for buffer %s" buffer)))

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
  (codeawareness-log-info "Code Awareness: Cleared all highlights"))

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
        (codeawareness-log-info "Code Awareness: Refreshing highlights for buffer %s" buffer)
        ;; Clear existing overlays
        ;; (codeawareness--clear-buffer-highlights buffer)
        ;; Recreate highlights from stored data
        (maphash (lambda (line-number highlight-data)
                   (let ((type (overlay-get highlight-data 'codeawareness-highlight-type))
                         (properties (overlay-get highlight-data 'codeawareness-properties)))
                     (codeawareness--add-highlight buffer line-number type properties)))
                 buffer-highlights)))))


(defun codeawareness--apply-highlights-from-data (buffer highlight-data)
  "Apply highlights to buffer based on data from the local service."
  (when (and buffer (buffer-live-p buffer) highlight-data)
    (codeawareness-log-info "Code Awareness: Applying highlights to buffer %s, data: %s"
                            (buffer-name buffer) highlight-data)
    ;; Use hl-line mode if configured, otherwise use custom overlays
    (if (and codeawareness-use-hl-line-mode (featurep 'hl-line))
        (progn
          (codeawareness-log-info "Code Awareness: Using hl-line mode for highlighting")
          (codeawareness--apply-hl-line-highlights-from-data buffer highlight-data))
      (codeawareness-log-info "Code Awareness: Using custom overlay mode for highlighting")
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
  "Convert hl data structure to highlight format. HL-DATA should be an array of line numbers. Returns a list of highlight alists with 'line and 'type keys."
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
            (set-face-attribute modified-face nil :background "#00ff00" :foreground "#000000" :extend t))
        ;; Light theme colors - more prominent
        (progn
          (set-face-attribute conflict-face nil :background "#ffcccc" :foreground "#cc0000" :extend t)
          (set-face-attribute overlap-face nil :background "#ffdd88" :foreground "#884400" :extend t)
          (set-face-attribute peer-face nil :background "#88ccff" :foreground "#004488" :extend t)
          (set-face-attribute modified-face nil :background "#88ff88" :foreground "#004400" :extend t))))
    (codeawareness-log-info "Code Awareness: HL-line faces initialized")))

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
           (overlay (make-overlay (line-beginning-position line-number)
                                  (line-beginning-position (1+ line-number))
                                  buffer t nil)))
      (codeawareness-log-info "Code Awareness: Created hl-line overlay for line %d in buffer %s with face %s"
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
    (codeawareness-log-info "Code Awareness: Cleared hl-line highlights for buffer %s" buffer)))

(defun codeawareness--apply-hl-line-highlights-from-data (buffer highlight-data)
  "Apply hl-line highlights to buffer based on data from the local service."
  (when (and buffer (buffer-live-p buffer) highlight-data (featurep 'hl-line))
    (codeawareness-log-info "Code Awareness: Applying hl-line highlights to buffer %s, count: %d"
                            (buffer-name buffer) (length highlight-data))
    ;; Clear existing highlights first
    (codeawareness--clear-buffer-hl-line-highlights buffer)
    ;; Apply new highlights
    (dolist (highlight highlight-data)
      (let ((line (alist-get 'line highlight))
            (type (alist-get 'type highlight))
            (properties (alist-get 'properties highlight)))
        (when (and line type)
          (codeawareness-log-info "Code Awareness: Adding hl-line highlight for line %d, type %s" line type)
          (codeawareness--add-hl-line-highlight buffer line type properties))))))

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

(defun codeawareness--ipc-sentinel (process event)
  "Handle IPC process sentinel events."
  (codeawareness-log-info "Code Awareness IPC: %s" event)
  (cond
   ((string-match "failed" event)
    (codeawareness-log-error "Code Awareness: Local service connection failed")
    (setq codeawareness--connected nil)
    ;; Retry connection
    (run-with-timer 2.0 nil #'codeawareness--connect-to-local-service))
   ((string-match "exited" event)
    (codeawareness-log-warn "Code Awareness: Local service connection closed")
    (setq codeawareness--connected nil))
   ((string-match "connection broken by remote peer" event)
    (codeawareness-log-warn "Code Awareness: Local service rejected connection")
    (setq codeawareness--connected nil)
    ;; Retry connection after a delay
    (run-with-timer 2.0 nil #'codeawareness--connect-to-local-service))
   ((string-match "open" event)
    (codeawareness-log-info "Code Awareness: Successfully connected to local service")
    (setq codeawareness--connected t)
    ;; Initialize workspace after connection (like VS Code)
    (codeawareness--init-workspace))
   (t
    (codeawareness-log-warn "Code Awareness: Unknown IPC sentinel event: %s" event))))

(defun codeawareness--ipc-filter (process data)
  "Handle IPC process data."
  ;;(codeawareness-log-info "Code Awareness: Received IPC data: %s" data)
  (let ((buffer (process-buffer process)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert data)
        (codeawareness--process-ipc-messages)))))

(defun codeawareness--process-ipc-messages ()
  "Process complete IPC messages from the buffer."
  (let ((delimiter "\f")
        (buffer (current-buffer)))
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
             (error-data (alist-get 'err data))
             (caw (alist-get 'caw data)))
        (codeawareness-log-info "Code Awareness: %s:%s" domain action)
        ;; (codeawareness-log-info "Code Awareness: Raw message: %S" message)
        ;; (codeawareness-log-info "Code Awareness: Parsed data: %S" data)
        ;; (codeawareness-log-info "Code Awareness: Response data: %S" response-data)
        (if (and (string= flow "res") action)
            (codeawareness--handle-response domain action response-data)
          (if (and (string= flow "err") action)
              (codeawareness--handle-error domain action error-data)
            (if (and (string= flow "req") action)
                (codeawareness-log-info "Code Awareness: Received request (not handling): %s:%s" domain action)
              (codeawareness-log-warn "Code Awareness: Unknown message format: %s" message)))))
    (error
     (codeawareness-log-error "Code Awareness: Error parsing IPC message: %s" err))))

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

(defun codeawareness--handle-repo-active-path-response (data)
  "Handle response from repo:active-path request."
  (codeawareness-log-info "Code Awareness: Received repo:active-path response")
  ;; Add the project to our store
  (codeawareness--add-project data)
  ;; Extract and apply highlights from the hl data structure
  (let* ((hl-data (alist-get 'hl data))
         (buffer (or codeawareness--active-buffer codeawareness--last-file-buffer)))
    (codeawareness-log-info "Code Awareness: hl-data: %s, buffer: %s" hl-data (if buffer (buffer-name buffer) "nil"))
    (if (and hl-data buffer (buffer-live-p buffer))
        ;; Apply highlights immediately if we have an active buffer
        (progn
          (codeawareness-log-info "Code Awareness: Applying highlights to buffer %s" (buffer-name buffer))
          ;; Convert hl data to highlight format
          (let ((highlights (codeawareness--convert-hl-to-highlights hl-data)))
            (when highlights
              (codeawareness--apply-highlights-from-data buffer highlights)))))))

(defun codeawareness--handle-auth-info-response (data)
  "Handle response from auth:info request."
  ;;(codeawareness-log-info "Code Awareness: Received auth:info response")
  ;; (codeawareness-log-info "Code Awareness: Auth data: %S" data)
  (if (and data (listp data) (alist-get 'user data))
      (progn
        (setq codeawareness--user (alist-get 'user data))
        (setq codeawareness--tokens (alist-get 'tokens data))
        (setq codeawareness--authenticated t)
        (codeawareness-log-info "Code Awareness: Authentication successful")
        (message "Code Awareness: Authenticated as %s" (alist-get 'name codeawareness--user))
        ;; Refresh active file immediately after authentication (like VSCode's init function)
        (codeawareness--refresh-active-file))
    (setq codeawareness--authenticated nil)
    (codeawareness-log-warn "Code Awareness: No authentication data received - user needs to authenticate")))

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
              (codeawareness-log-info "Code Awareness: Sending %s:%s" domain action)
              (process-send-string codeawareness--ipc-process (concat message "\f"))
              (codeawareness--setup-response-handler domain action))
          (codeawareness-log-error "Code Awareness: IPC process exists but is not open (status: %s)"
                                   (process-status codeawareness--ipc-process)))
      (codeawareness-log-error "Code Awareness: No IPC process available for transmission"))))

(defun codeawareness--setup-response-handler (domain action)
  "Setup response handlers for the given domain and action."
  (let ((res-key (format "res:%s:%s" domain action))
        (err-key (format "err:%s:%s" domain action)))
    ;; Set up specific handlers for known actions
    (cond
     ((string= (format "%s:%s" domain action) "code:repo:active-path")
      (puthash res-key #'codeawareness--handle-repo-active-path-response codeawareness--response-handlers))
     ((or (string= (format "%s:%s" domain action) "*:auth:info")
          (string= (format "%s:%s" domain action) "*:auth:login"))
      (puthash res-key #'codeawareness--handle-auth-info-response codeawareness--response-handlers))
     (t
      (puthash res-key #'codeawareness--handle-success codeawareness--response-handlers)))
    (puthash err-key #'codeawareness--handle-failure codeawareness--response-handlers)))

(defun codeawareness--handle-success (data)
  "Handle successful IPC response."
  (codeawareness-log-info "Code Awareness: Success - %s" (format "%s" data)))

(defun codeawareness--handle-failure (error-data)
  "Handle failed IPC response."
  (codeawareness-log-error "Code Awareness: Error - %s" (format "%s" error-data)))

;;; Connection Management

(defun codeawareness--init-ipc ()
  "Initialize IPC communication."
  (setq codeawareness--guid (codeawareness--generate-guid))
  (codeawareness-log-info "Code Awareness: Initializing IPC with GUID %s" (format "%s" codeawareness--guid))
  (codeawareness--connect-to-catalog))

(defun codeawareness--connect-to-catalog ()
  "Connect to the catalog service."
  (let* ((catalog-path (codeawareness--get-catalog-socket-path))
         (process-name "codeawareness-catalog")
         (buffer-name "*codeawareness-catalog*"))
    (codeawareness-log-info "Code Awareness: Connecting to catalog")
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
          (codeawareness-log-info "Code Awareness: Catalog connection initiated")
          ;; Check process status immediately and after a delay
          (codeawareness--check-catalog-process-status)
          (run-with-timer 0.5 nil #'codeawareness--check-catalog-process-status))
      (error
       (codeawareness-log-error "Code Awareness: Failed to create catalog connection: %s" err)
       (message "Code Awareness: Failed to connect to catalog service at %s. Error: %s"
                catalog-path err)))))

(defun codeawareness--catalog-sentinel (process event)
  "Handle catalog process sentinel events."
  (codeawareness-log-info "Code Awareness Catalog: SENTINEL CALLED with event: %s" event)
  (codeawareness-log-info "Code Awareness Catalog: Process: %s" process)
  (cond
   ((string-match "failed" event)
    (codeawareness-log-error "Code Awareness: Failed to connect to catalog service at %s"
                             (codeawareness--get-catalog-socket-path))
    (message "Code Awareness: Failed to connect to catalog service. Check if the service is running on %s"
             (codeawareness--get-catalog-socket-path))
    (setq codeawareness--connected nil))
   ((string-match "exited" event)
    (codeawareness-log-warn "Code Awareness: Catalog connection closed")
    (setq codeawareness--connected nil))
   ((string-match "open" event)
    (codeawareness-log-info "Code Awareness: Successfully connected to catalog service")
    (message "Code Awareness: Connected to catalog service")
    (setq codeawareness--connected t)
    ;; Send 'connected' message to trigger client registration (matching VSCode behavior)
    (codeawareness--catalog-filter process "connected"))))

(defun codeawareness--catalog-filter (process data)
  "Handle catalog process data."
  (codeawareness-log-info "Code Awareness Catalog: Received data: %s" data)
  (when (string= data "connected")
    (codeawareness-log-info "Code Awareness: Catalog connection established, registering client")
    (codeawareness--register-client)))

(defun codeawareness--register-client ()
  "Register this client with the catalog service."
  (when codeawareness--client-registered
    (codeawareness-log-info "Code Awareness: Client already registered, skipping")
    (return-from codeawareness--register-client))

  (let ((message (json-encode `((flow . "req")
                                (domain . "*")
                                (action . "clientId")
                                (data . ,codeawareness--guid)
                                (caw . ,codeawareness--guid)))))
    (when codeawareness--ipc-catalog-process
      (process-send-string codeawareness--ipc-catalog-process (concat message "\f"))
      (codeawareness-log-info "Code Awareness: Client registered")
      (setq codeawareness--client-registered t)
      (codeawareness--init-server))))

(defun codeawareness--init-server ()
  "Initialize the server connection."
  (codeawareness-log-info "Code Awareness: Initializing server connection")
  ;; Start polling for local service socket with exponential backoff
  (codeawareness--poll-for-local-service))

(defun codeawareness--init-workspace ()
  "Initialize workspace."
  (codeawareness-log-info "Code Awareness: Workspace initialized")
  ;; Send auth:info request after a short delay to ensure connection is ready
  (run-with-timer 0.1 nil #'codeawareness--send-auth-info))

(defun codeawareness--send-auth-info ()
  "Send auth:info request to the local service."
  (codeawareness-log-info "Code Awareness: Sending auth:info request")
  (if (and codeawareness--ipc-process
           (eq (process-status codeawareness--ipc-process) 'open))
      (codeawareness--transmit "auth:info" nil)
    (codeawareness-log-error "Code Awareness: IPC process not ready for auth:info request")))

(defvar codeawareness--poll-attempts 0
  "Number of polling attempts for local service socket.")

(defvar codeawareness--max-poll-attempts 10
  "Maximum number of polling attempts.")

(defun codeawareness--poll-for-local-service ()
  "Poll for local service socket with exponential backoff."
  (let ((socket-path (codeawareness--get-socket-path codeawareness--guid)))
    (if (file-exists-p socket-path)
        (progn
          (codeawareness-log-info "Code Awareness: Local service socket found")
          (setq codeawareness--poll-attempts 0)
          (codeawareness--connect-to-local-service))
      (if (>= codeawareness--poll-attempts codeawareness--max-poll-attempts)
          (progn
            (codeawareness-log-error "Code Awareness: Failed to find local service socket after %d attempts"
                                     codeawareness--max-poll-attempts)
            (message "Code Awareness: Failed to connect to local service after %d attempts"
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
    (codeawareness-log-info "Code Awareness: Current GUID: %s" codeawareness--guid)
    (codeawareness-log-info "Code Awareness: Attempting to connect to local service at %s" socket-path)
    (codeawareness-log-info "Code Awareness: Socket exists: %s" (file-exists-p socket-path))
    
    (condition-case err
        (progn
          (codeawareness-log-info "Code Awareness: Creating network process...")
          (setq codeawareness--ipc-process
                (make-network-process
                 :name process-name
                 :buffer buffer-name
                 :family 'local
                 :service socket-path
                 :sentinel #'codeawareness--ipc-sentinel
                 :filter #'codeawareness--ipc-filter
                 :noquery t))
          (codeawareness-log-info "Code Awareness: Connected to local service")
          ;; Set up a timeout to detect stuck connections
          (run-with-timer 5.0 nil #'codeawareness--check-connection-timeout)
          ;; Set up a fallback to trigger workspace init if sentinel doesn't fire
          (run-with-timer 1.0 nil #'codeawareness--fallback-workspace-init))
      (error
       (codeawareness-log-warn "Code Awareness: Failed to connect to local service, will retry in 2 seconds")
       (codeawareness-log-warn "Code Awareness: Error: %s" err)
       ;; Schedule retry
       (run-with-timer 2.0 nil #'codeawareness--connect-to-local-service)))))

(defun codeawareness--check-connection-timeout ()
  "Check if the connection is stuck and handle timeout."
  (when (and codeawareness--ipc-process
             (not codeawareness--connected))
    (let ((status (process-status codeawareness--ipc-process)))
      (if (eq status 'connect)
          (progn
            (codeawareness-log-error "Code Awareness: Connection stuck, retrying")
            (delete-process codeawareness--ipc-process)
            (setq codeawareness--ipc-process nil)
            (run-with-timer 1.0 nil #'codeawareness--connect-to-local-service))))))

(defun codeawareness--fallback-workspace-init ()
  "Fallback workspace initialization if sentinel doesn't fire."
  (when (and codeawareness--ipc-process
             (eq (process-status codeawareness--ipc-process) 'open)
             (not codeawareness--connected))
    (codeawareness-log-info "Code Awareness: Using fallback workspace init")
    (codeawareness--init-workspace)))

(defun codeawareness--force-cleanup ()
  "Force cleanup of all Code Awareness processes and state."
  (codeawareness-log-info "Code Awareness: Force cleaning up all processes")

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
          (codeawareness-log-info "Code Awareness: Force deleted catalog process"))
      (error
       (codeawareness-log-error "Code Awareness: Error deleting catalog process: %s" err)))
    (setq codeawareness--ipc-catalog-process nil))

  (when codeawareness--ipc-process
    (condition-case err
        (progn
          ;; Try to close the process gracefully first
          (when (eq (process-status codeawareness--ipc-process) 'open)
            (process-send-eof codeawareness--ipc-process))
          ;; Force delete the process
          (delete-process codeawareness--ipc-process)
          (codeawareness-log-info "Code Awareness: Force deleted IPC process"))
      (error
       (codeawareness-log-error "Code Awareness: Error deleting IPC process: %s" err)))
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

  (codeawareness-log-info "Code Awareness: Force cleanup completed"))

(defun codeawareness--send-disconnect-messages ()
  "Send disconnect messages to catalog service."
  (codeawareness-log-info "Code Awareness: Sending disconnect messages")

  ;; Send disconnect message to catalog
  (when (and codeawareness--ipc-catalog-process
             (eq (process-status codeawareness--ipc-catalog-process) 'open))
    (let ((message (json-encode `((flow . "req")
                                  (domain . "*")
                                  (action . "clientDisconnect")
                                  (data . ,codeawareness--guid)
                                  (caw . ,codeawareness--guid)))))
      (codeawareness-log-info "Code Awareness: Sending clientDisconnect to catalog")
      (condition-case err
          (process-send-string codeawareness--ipc-catalog-process (concat message "\f"))
        (error
         (codeawareness-log-error "Code Awareness: Failed to send clientDisconnect to catalog: %s" err))))))

(defun codeawareness--check-catalog-process-status ()
  "Check the status of the catalog process."
  (when codeawareness--ipc-catalog-process
    (let ((status (process-status codeawareness--ipc-catalog-process)))
      (if (eq status 'open)
          (progn
            (codeawareness-log-info "Code Awareness: Catalog connected")
            ;; If client is not registered yet, trigger registration as fallback
            (unless codeawareness--client-registered
              (codeawareness-log-info "Code Awareness: Registering client")
              (codeawareness--catalog-filter codeawareness--ipc-catalog-process "connected")))
        (codeawareness-log-error "Code Awareness: Catalog process not open, status: %s" status)))))

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
      (codeawareness-log-info "Code Awareness: Buffer switch detected: %s -> %s (has-file: %s)"
                              (if codeawareness--active-buffer (buffer-name codeawareness--active-buffer) "nil")
                              (buffer-name current-buffer)
                              (if (buffer-file-name current-buffer) "yes" "no"))
      ;; (when (and codeawareness--active-buffer
      ;;           (buffer-live-p codeawareness--active-buffer))
      ;;  (codeawareness-log-info "Code Awareness: Clearing highlights from previous buffer %s (switching to: %s)"
      ;;                          (buffer-name codeawareness--active-buffer) (buffer-name current-buffer))
      ;;  (codeawareness--clear-buffer-highlights codeawareness--active-buffer))

      (if (buffer-file-name current-buffer)
          ;; Set the active buffer and refresh immediately (like VS Code's onDidChangeActiveTextEditor)
          (progn
            (codeawareness-log-info "Code Awareness: Active buffer changed to %s" (buffer-file-name current-buffer))
            ;; Clear last hl data if switching to a different file
            (when (and codeawareness--active-buffer
                       (buffer-file-name codeawareness--active-buffer)
                       (not (string= (buffer-file-name codeawareness--active-buffer)
                                     (buffer-file-name current-buffer))))
              (codeawareness-log-info "Code Awareness: Switching to different file"))
            (setq codeawareness--active-buffer current-buffer)
            (setq codeawareness--last-file-buffer current-buffer) ; Update the last file buffer
            (codeawareness--refresh-active-file))
        ;; Clear active buffer if switching to a buffer without a file
        (codeawareness-log-info "Code Awareness: Switched to buffer without file, clearing active buffer")
        (setq codeawareness--active-buffer nil)
        (setq codeawareness--last-file-buffer nil)))))

;;; Public API

(defun codeawareness-toggle ()
  "Toggle Code Awareness mode."
  (interactive)
  (if codeawareness-mode
      (codeawareness-mode -1)
    (codeawareness-mode 1)))

(defun codeawareness-refresh ()
  "Refresh Code Awareness data."
  (interactive)
  (codeawareness--refresh-active-file))

(defun codeawareness-clear-highlights ()
  "Clear all Code Awareness highlights from the current buffer."
  (interactive)
  (codeawareness--clear-buffer-highlights (current-buffer))
  (message "Code Awareness: Cleared highlights"))

(defun codeawareness-clear-all-highlights ()
  "Clear all Code Awareness highlights from all buffers."
  (interactive)
  (codeawareness--clear-all-highlights)
  (message "Code Awareness: Cleared all highlights"))

(defun codeawareness-refresh-highlights ()
  "Refresh highlights for the current buffer."
  (interactive)
  (codeawareness--refresh-buffer-highlights (current-buffer))
  (message "Code Awareness: Refreshed highlights"))

(defun codeawareness-auth-status ()
  "Show the current authentication status."
  (interactive)
  (if codeawareness--authenticated
      (message "Code Awareness: Authenticated as %s" (alist-get 'name codeawareness--user))
    (message "Code Awareness: Not authenticated")))

(defun codeawareness-connection-status ()
  "Show the current connection status."
  (interactive)
  (message "Code Awareness: Catalog connected: %s, Local service connected: %s, Authenticated: %s, Client registered: %s"
           (if (and codeawareness--ipc-catalog-process
                    (eq (process-status codeawareness--ipc-catalog-process) 'open)) "yes" "no")
           (if (and codeawareness--ipc-process
                    (eq (process-status codeawareness--ipc-process) 'open)) "yes" "no")
           (if codeawareness--authenticated "yes" "no")
           (if codeawareness--client-registered "yes" "no")))

(defun codeawareness-retry-connection ()
  "Manually retry the connection to the local service."
  (interactive)
  (codeawareness-log-info "Code Awareness: Manual connection retry requested")
  (when codeawareness--ipc-process
    (delete-process codeawareness--ipc-process)
    (setq codeawareness--ipc-process nil))
  (setq codeawareness--connected nil)
  (run-with-timer 0.5 nil #'codeawareness--connect-to-local-service)
  (message "Code Awareness: Connection retry initiated"))

(defun codeawareness-disconnect ()
  "Manually disconnect from Code Awareness services."
  (interactive)
  (codeawareness-log-info "Code Awareness: Manual disconnect requested")
  (codeawareness--send-disconnect-messages)
  (message "Code Awareness: Disconnect messages sent"))

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
    (message "Code Awareness: Testing empty line highlighting...")
    (codeawareness--apply-highlights-from-data (current-buffer) test-data)
    (message "Code Awareness: Applied test highlights to lines 1, 3, 5, 7 (including empty lines)")))

(defun codeawareness-test-json-hl-data ()
  "Test highlighting using simulated JSON hl data (0-based line numbers)."
  (interactive)
  ;; Ensure hl-line faces are initialized
  (when (featurep 'hl-line)
    (codeawareness--init-hl-line-faces))
  ;; Simulate JSON hl data with 0-based line numbers (like from local service)
  (let ((json-hl-data [0 2 4 6])) ; 0-based line numbers
    (message "Code Awareness: Testing JSON hl data: %s" json-hl-data)
    ;; Convert to highlight format (like the actual flow)
    (let ((highlights (codeawareness--convert-hl-to-highlights json-hl-data)))
      (message "Code Awareness: Converted to highlights: %s" highlights)
      ;; Apply highlights using the same pipeline as real JSON data
      (codeawareness--apply-highlights-from-data (current-buffer) highlights)
      (message "Code Awareness: Applied JSON hl highlights to lines 1, 3, 5, 7 (0-based: 0, 2, 4, 6)"))))

(defun codeawareness-debug-buffers ()
  "Show debug information about current buffer state."
  (interactive)
  (message "Code Awareness Buffer Debug: Active: %s, Last File: %s, Current: %s, Current File: %s"
           (if codeawareness--active-buffer (buffer-name codeawareness--active-buffer) "nil")
           (if codeawareness--last-file-buffer (buffer-name codeawareness--last-file-buffer) "nil")
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
    (message "Code Awareness: Test overlay created with face %s on line 1" face)
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
      (message "Code Awareness: Test overlay for %s face on line %d" type line)
      ;; Remove the test overlay after 5 seconds
      (run-with-timer 5.0 nil (lambda () (delete-overlay overlay))))))

;;; Reinit on Emacs restart

(defun codeawareness-reinit-faces ()
  "Force reinitialize hl-line faces."
  (interactive)
  (when (featurep 'hl-line)
    (setq codeawareness--hl-line-faces nil) ; Clear existing faces
    (codeawareness--init-hl-line-faces)
    (message "Code Awareness: HL-line faces reinitialized")))

;;; Cleanup on Emacs exit

(defun codeawareness--cleanup-on-exit ()
  "Cleanup Code Awareness when Emacs is about to exit."
  (when codeawareness-mode
    (codeawareness-log-info "Code Awareness: Emacs exiting, cleaning up connections")
    ;; Send disconnect messages first, then force cleanup
    (codeawareness--send-disconnect-messages)
    ;; Force synchronous cleanup to ensure processes are deleted
    (codeawareness--force-cleanup)))


(defun codeawareness--buffer-list-update-hook ()
  "Hook function for buffer-list-update-hook to detect when buffers are displayed."
  (let ((current-buffer (current-buffer)))
    (when (and current-buffer
               (buffer-file-name current-buffer)
               (not (eq current-buffer codeawareness--active-buffer)))
      (codeawareness-log-info "Code Awareness: Buffer displayed: %s" (buffer-file-name current-buffer))
      (setq codeawareness--active-buffer current-buffer)
      (setq codeawareness--last-file-buffer current-buffer)
      (codeawareness--refresh-active-file))))

;;; Minor Mode

(define-minor-mode codeawareness-mode
  "Toggle Code Awareness mode. Enable Code Awareness functionality for collaborative development."
  :init-value nil
  :global t
  :lighter " CAW"
  :group 'codeawareness
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-a t") #'codeawareness-toggle)
            (define-key map (kbd "C-c C-a x") #'codeawareness-disconnect)
            (define-key map (kbd "C-c C-a f") #'codeawareness-test-face-colors)
            (define-key map (kbd "C-c C-a l") #'codeawareness-clear-logs)
            map)
  (if codeawareness-mode
      (codeawareness--enable)
    (codeawareness--disable)))

(defun codeawareness--enable ()
  "Enable Code Awareness."
  (codeawareness--init-config)
  (codeawareness--init-store)
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
  (codeawareness-log-info "Code Awareness: Disabling and disconnecting")

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

(defun codeawareness--cleanup-on-exit ()
  "Cleanup Code Awareness when Emacs is about to exit."
  (when codeawareness-mode
    (codeawareness-log-info "Code Awareness: Emacs exiting, cleaning up connections")
    ;; Send disconnect messages first, then force cleanup
    (codeawareness--send-disconnect-messages)
    ;; Force synchronous cleanup to ensure processes are deleted
    (codeawareness--force-cleanup)))

;; Register cleanup functions to run when Emacs exits
(add-hook 'kill-emacs-hook #'codeawareness--cleanup-on-exit)

;; Register cleanup functions to run when Emacs exits
(add-hook 'kill-emacs-hook #'codeawareness--cleanup-on-exit)

;;; Provide

(provide 'codeawareness)
;;; codeawareness.el ends here
