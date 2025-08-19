;;; codeawareness.el --- Code Awareness for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 - 2024 Mark Vasile

;; Author: Mark Vasile <mark@codeawareness.com>
;; Package-Requires: ((emacs "26.1"))
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

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'codeawareness-config)
(require 'codeawareness-logger)

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

(defvar codeawareness--update-timer nil
  "Timer for debounced updates.")

(defvar codeawareness--connected nil
  "Whether we're connected to the local service.")

(defvar codeawareness--config nil
  "Configuration data.")

;;; Store/State Management

(defvar codeawareness--store nil
  "Central store for Code Awareness state.")

(defvar codeawareness--active-project nil
  "Currently active project data.")

(defvar codeawareness--projects nil
  "List of all projects.")

(defvar codeawareness--active-buffer nil
  "Currently active buffer.")

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

(defun codeawareness--refresh-active-file ()
  "Refresh the currently active file by sending repo:active-path message."
  (if (not codeawareness--authenticated)
      (codeawareness-log-warn "Code Awareness: Not authenticated, skipping file refresh")
    (let ((fpath (codeawareness--get-active-file-path))
          (doc (codeawareness--get-active-file-content)))
      (if (not fpath)
          (codeawareness-log-info "Code Awareness: No active file to refresh")
        (codeawareness-log-info "Code Awareness: Refreshing active file %s" fpath)
        (let ((message-data `((fpath . ,(codeawareness--cross-platform-path fpath))
                              (doc . ,doc)
                              (caw . ,codeawareness--guid))))
          (codeawareness--transmit "repo:active-path" message-data)
          (codeawareness--setup-response-handler "code" "repo:active-path"))))))

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
    
    ;; Conflict highlights (red background)
    (set-face-attribute conflict-face nil
                        :background "#ffcccc"
                        :foreground "#cc0000"
                        :weight 'bold)
    
    ;; Overlap highlights (yellow background)
    (set-face-attribute overlap-face nil
                        :background "#ffffcc"
                        :foreground "#cc6600"
                        :weight 'normal)
    
    ;; Peer highlights (blue background)
    (set-face-attribute peer-face nil
                        :background "#cce5ff"
                        :foreground "#0066cc"
                        :weight 'normal)
    
    ;; Modified highlights (green background)
    (set-face-attribute modified-face nil
                        :background "#ccffcc"
                        :foreground "#006600"
                        :weight 'normal))
  
  (codeawareness-log-info "Code Awareness: Highlight faces initialized"))

(defun codeawareness--get-highlight-face (type)
  "Get the face for the given highlight type."
  (alist-get type codeawareness--highlight-faces))

(defun codeawareness--create-line-overlay (buffer line-number face &optional properties)
  "Create an overlay for a specific line in the given buffer."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let* ((line-count (line-number-at-pos (point-max)))
             (start (line-beginning-position line-number))
             (end (line-end-position line-number)))
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
    (codeawareness-log-info "Code Awareness: Cleared highlights for buffer %s" buffer)))

(defun codeawareness--clear-all-highlights ()
  "Clear all Code Awareness highlights from all buffers."
  (dolist (buffer (buffer-list))
    (codeawareness--clear-buffer-highlights buffer))
  (clrhash codeawareness--highlights)
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
        (codeawareness-log-info "Code Awareness: Added %s highlight to line %d in buffer %s" 
                                type line-number buffer)
        overlay))))

(defun codeawareness--remove-highlight (buffer line-number)
  "Remove a highlight from the specified line in the given buffer."
  (let* ((buffer-highlights (gethash buffer codeawareness--highlights))
         (overlay (when buffer-highlights (gethash line-number buffer-highlights))))
    (when overlay
      (delete-overlay overlay)
      (remhash line-number buffer-highlights)
      (codeawareness-log-info "Code Awareness: Removed highlight from line %d in buffer %s" 
                              line-number buffer))))

(defun codeawareness--refresh-buffer-highlights (buffer)
  "Refresh highlights for the given buffer by recreating them."
  (when (and buffer (buffer-live-p buffer))
    (let ((buffer-highlights (gethash buffer codeawareness--highlights)))
      (when buffer-highlights
        (codeawareness-log-info "Code Awareness: Refreshing highlights for buffer %s" buffer)
        ;; Clear existing overlays
        (codeawareness--clear-buffer-highlights buffer)
        ;; Recreate highlights from stored data
        (maphash (lambda (line-number highlight-data)
                   (let ((type (overlay-get highlight-data 'codeawareness-highlight-type))
                         (properties (overlay-get highlight-data 'codeawareness-properties)))
                     (codeawareness--add-highlight buffer line-number type properties)))
                 buffer-highlights)))))

(defun codeawareness--schedule-highlight-refresh (buffer)
  "Schedule a debounced highlight refresh for the given buffer."
  (when codeawareness--highlight-timer
    (cancel-timer codeawareness--highlight-timer))
  (setq codeawareness--highlight-timer
        (run-with-timer 0.5 nil #'codeawareness--refresh-buffer-highlights buffer)))

(defun codeawareness--apply-highlights-from-data (buffer highlight-data)
  "Apply highlights to buffer based on data from the local service."
  (when (and buffer (buffer-live-p buffer) highlight-data)
    (codeawareness-log-info "Code Awareness: Applying highlights to buffer %s" buffer)
    ;; Clear existing highlights first
    (codeawareness--clear-buffer-highlights buffer)
    
    ;; Apply new highlights
    (dolist (highlight highlight-data)
      (let ((line (alist-get 'line highlight))
            (type (alist-get 'type highlight))
            (properties (alist-get 'properties highlight)))
        (when (and line type)
          (codeawareness--add-highlight buffer line type properties))))
    
    (codeawareness-log-info "Code Awareness: Applied %d highlights to buffer %s" 
                            (length highlight-data) buffer)))

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

(defun codeawareness--create-ipc-process (guid)
  "Create an IPC process for the given GUID."
  (let* ((socket-path (codeawareness--get-socket-path guid))
         (process-name (format "codeawareness-ipc-%s" guid))
         (buffer-name (format "*%s*" process-name)))
    (make-network-process
     :name process-name
     :buffer buffer-name
     :family 'local
     :service socket-path
     :sentinel #'codeawareness--ipc-sentinel
     :filter #'codeawareness--ipc-filter
     :noquery t)))

(defun codeawareness--ipc-sentinel (process event)
  "Handle IPC process sentinel events."
  (codeawareness-log-info "Code Awareness IPC SENTINEL CALLED with event: %s" event)
  (codeawareness-log-info "Code Awareness IPC: Process: %s" process)
  (codeawareness-log-info "Code Awareness IPC: Process status: %s" 
                          (if process (process-status process) "nil"))
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
  (codeawareness-log-info "Code Awareness: Received raw data from IPC: %s" data)
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
  (codeawareness-log-info "Code Awareness: Received IPC message: %s" message)
  (condition-case err
      (let* ((data (json-read-from-string message))
             (flow (alist-get 'flow data))
             (domain (alist-get 'domain data))
             (action (alist-get 'action data))
             (response-data (alist-get 'data data))
             (error-data (alist-get 'err data))
             (caw (alist-get 'caw data)))
        (codeawareness-log-info "Code Awareness: Parsed message - flow: %s, domain: %s, action: %s, caw: %s" 
                                flow domain action caw)
        (if (and (string= flow "res") action)
            (codeawareness--handle-response domain action response-data)
          (if (and (string= flow "err") action)
              (codeawareness--handle-error domain action error-data)
            (codeawareness-log-warn "Code Awareness: Unknown message format: %s" message))))
    (error
     (codeawareness-log-error "Code Awareness: Error parsing IPC message: %s" err))))

(defun codeawareness--handle-response (domain action data)
  "Handle an IPC response."
  (let* ((key (format "res:%s:%s" domain action))
         (handler (gethash key codeawareness--response-handlers)))
    (when handler
      (remhash key codeawareness--response-handlers)
      (funcall handler data))))

(defun codeawareness--handle-repo-active-path-response (data)
  "Handle response from repo:active-path request."
  (codeawareness-log-info "Code Awareness: Received repo:active-path response")
  (codeawareness-log-info "Code Awareness: Project data: %s" data)
  ;; Add the project to our store
  (codeawareness--add-project data)
  ;; Apply highlights if available in the response
  (let ((highlights (alist-get 'highlights data))
        (buffer codeawareness--active-buffer))
    (when (and highlights buffer (buffer-live-p buffer))
      (codeawareness--apply-highlights-from-data buffer highlights)))
  (codeawareness-log-info "Code Awareness: Project added successfully"))

(defun codeawareness--handle-auth-info-response (data)
  "Handle response from auth:info request."
  (codeawareness-log-info "Code Awareness: Received auth:info response")
  (codeawareness-log-info "Code Awareness: Auth data: %s" data)
  (if data
      (progn
        (setq codeawareness--user (alist-get 'user data))
        (setq codeawareness--tokens (alist-get 'tokens data))
        (setq codeawareness--authenticated t)
        (codeawareness-log-info "Code Awareness: Authentication successful")
        (message "Code Awareness: Authenticated as %s" (alist-get 'name codeawareness--user)))
    (setq codeawareness--authenticated nil)
    (codeawareness-log-warn "Code Awareness: No authentication data received")))

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
    ;; Log data in a truncated format to avoid huge log output
    (let ((data-summary (if (and data (alist-get 'doc data))
                            (format "((fpath . %s) (doc . [%d chars]))" 
                                    (alist-get 'fpath data) 
                                    (length (alist-get 'doc data)))
                          (format "%s" data))))
      (codeawareness-log-info "Code Awareness: Transmitting %s:%s with data: %s" domain action data-summary))
    (if codeawareness--ipc-process
        (progn
          (codeawareness-log-info "Code Awareness: Sending message: %s" message)
          (process-send-string codeawareness--ipc-process (concat message "\f"))
          (codeawareness--setup-response-handler domain action))
      (codeawareness-log-error "Code Awareness: No IPC process available for transmission"))))

(defun codeawareness--setup-response-handler (domain action)
  "Setup response handlers for the given domain and action."
  (let ((res-key (format "res:%s:%s" domain action))
        (err-key (format "err:%s:%s" domain action)))
    ;; Set up specific handlers for known actions
    (cond
     ((string= (format "%s:%s" domain action) "code:repo:active-path")
      (puthash res-key #'codeawareness--handle-repo-active-path-response codeawareness--response-handlers))
     ((string= (format "%s:%s" domain action) "*:auth:info")
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
    (codeawareness-log-info "Code Awareness: Connecting to catalog at %s" catalog-path)
    (codeawareness-log-info "Code Awareness: Socket exists: %s" (file-exists-p catalog-path))
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
          (codeawareness-log-info "Code Awareness: Process created: %s" codeawareness--ipc-catalog-process)
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
    (codeawareness-log-info "Code Awareness: Registering client with message: %s" message)
    (when codeawareness--ipc-catalog-process
      (process-send-string codeawareness--ipc-catalog-process (concat message "\f"))
      (codeawareness-log-info "Code Awareness: Client registration message sent")
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
  (codeawareness-log-info "Code Awareness: About to send auth:info request")
  (run-with-timer 0.1 nil #'codeawareness--send-auth-info))

(defun codeawareness--send-auth-info ()
  "Send auth:info request to the local service."
  (codeawareness-log-info "Code Awareness: Sending auth:info request")
  (codeawareness-log-info "Code Awareness: IPC process status: %s" 
                          (if codeawareness--ipc-process 
                              (process-status codeawareness--ipc-process) 
                            "nil"))
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
    (codeawareness-log-info "Code Awareness: Polling for local service socket (attempt %d/%d): %s" 
                            (1+ codeawareness--poll-attempts) codeawareness--max-poll-attempts socket-path)
    
    (if (file-exists-p socket-path)
        (progn
          (codeawareness-log-info "Code Awareness: Local service socket found, connecting")
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
          (codeawareness-log-info "Code Awareness: Socket not found, retrying in %.1f seconds" delay)
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
          (codeawareness-log-info "Code Awareness: Network process created: %s" codeawareness--ipc-process)
          (codeawareness-log-info "Code Awareness: Process status after creation: %s" 
                                  (if codeawareness--ipc-process 
                                      (process-status codeawareness--ipc-process) 
                                    "nil"))
          (codeawareness-log-info "Code Awareness: Local service connection initiated")
          ;; Set up a timeout to detect stuck connections
          (run-with-timer 5.0 nil #'codeawareness--check-connection-timeout))
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
      (codeawareness-log-warn "Code Awareness: Connection timeout check - status: %s" status)
      (if (eq status 'connect)
          (progn
            (codeawareness-log-error "Code Awareness: Connection stuck in 'connect' state, retrying")
            (delete-process codeawareness--ipc-process)
            (setq codeawareness--ipc-process nil)
            (run-with-timer 1.0 nil #'codeawareness--connect-to-local-service))
        (codeawareness-log-info "Code Awareness: Connection timeout check - status is %s, not stuck" status)))))

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
  "Send disconnect messages to both catalog and local service."
  (codeawareness-log-info "Code Awareness: Sending disconnect messages")
  
  ;; Send disconnect message to catalog
  (when (and codeawareness--ipc-catalog-process 
             (eq (process-status codeawareness--ipc-catalog-process) 'open))
    (let ((message (json-encode `((flow . "req")
                                  (domain . "*")
                                  (action . "clientDisconnect")
                                  (data . ,codeawareness--guid)
                                  (caw . ,codeawareness--guid)))))
      (codeawareness-log-info "Code Awareness: Sending clientDisconnect to catalog: %s" message)
      (condition-case err
          (process-send-string codeawareness--ipc-catalog-process (concat message "\f"))
        (error
         (codeawareness-log-error "Code Awareness: Failed to send clientDisconnect to catalog: %s" err)))))
  
  ;; Send disconnect message to local service
  (when (and codeawareness--ipc-process 
             (eq (process-status codeawareness--ipc-process) 'open))
    (let ((message (json-encode `((flow . "req")
                                  (domain . "*")
                                  (action . "auth:disconnect")
                                  (data . ,codeawareness--guid)
                                  (caw . ,codeawareness--guid)))))
      (codeawareness-log-info "Code Awareness: Sending auth:disconnect to local service: %s" message)
      (condition-case err
          (process-send-string codeawareness--ipc-process (concat message "\f"))
        (error
         (codeawareness-log-error "Code Awareness: Failed to send auth:disconnect to local service: %s" err))))))

(defun codeawareness--check-catalog-process-status ()
  "Check the status of the catalog process."
  (when codeawareness--ipc-catalog-process
    (let ((status (process-status codeawareness--ipc-catalog-process)))
      (codeawareness-log-info "Code Awareness: Catalog process status: %s" status)
      (if (eq status 'open)
          (progn
            (codeawareness-log-info "Code Awareness: Process is open and ready")
            ;; If client is not registered yet, trigger registration as fallback
            (unless codeawareness--client-registered
              (codeawareness-log-info "Code Awareness: Client not registered, triggering registration")
              (codeawareness--catalog-filter codeawareness--ipc-catalog-process "connected")))
        (codeawareness-log-error "Code Awareness: Process is not open, status: %s" status)))))

(defun codeawareness--schedule-reconnect ()
  "Schedule a reconnection attempt."
  (run-with-timer 2 nil #'codeawareness--init-ipc))

;;; Buffer Management

(defun codeawareness--set-active-buffer (buffer)
  "Set the active buffer for Code Awareness."
  (when (and buffer (buffer-file-name buffer))
    (setq codeawareness--active-buffer buffer)
    (codeawareness--schedule-update)))

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
  (codeawareness--update)
  ;; Refresh highlights after save to ensure they're still valid
  (when codeawareness--active-buffer
    (codeawareness--schedule-highlight-refresh codeawareness--active-buffer)))

(defun codeawareness--post-command-hook ()
  "Hook function for post-command-hook."
  (let ((current-buffer (current-buffer)))
    (when (and current-buffer
               (not (eq current-buffer codeawareness--active-buffer))
               (buffer-file-name current-buffer))
      ;; Set the active buffer and refresh immediately (like VS Code's onDidChangeActiveTextEditor)
      (setq codeawareness--active-buffer current-buffer)
      (codeawareness--refresh-active-file))))

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

(defun codeawareness-debug-connection ()
  "Show detailed connection debug information."
  (interactive)
  (let ((catalog-status (if codeawareness--ipc-catalog-process 
                            (process-status codeawareness--ipc-catalog-process) 
                          "nil"))
        (local-status (if codeawareness--ipc-process 
                          (process-status codeawareness--ipc-process) 
                        "nil")))
    (message "Code Awareness Debug: Catalog process: %s (status: %s), Local process: %s (status: %s), Connected: %s, Authenticated: %s"
             codeawareness--ipc-catalog-process catalog-status
             codeawareness--ipc-process local-status
             codeawareness--connected
             codeawareness--authenticated)))

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

(defun codeawareness-test ()
  "Run basic Code Awareness tests."
  (interactive)
  (message "Running Code Awareness tests...")
  
  ;; Test configuration
  (message "Catalog: %s" codeawareness-catalog)
  (message "Highlight while closed: %s" (if codeawareness-highlight-while-closed "yes" "no"))
  (message "Update delay: %s" (number-to-string codeawareness-update-delay))
  
  ;; Test logging
  (codeawareness-log-info "Test info message")
  (codeawareness-log-warn "Test warning message")
  (codeawareness-log-error "Test error message")
  
  ;; Test GUID generation
  (let ((guid (codeawareness--generate-guid)))
    (message "Generated GUID: %s" guid))
  
  ;; Test socket path generation
  (let ((path (codeawareness--get-socket-path "test-guid")))
    (message "Socket path: %s" path))
  
  ;; Test catalog socket path
  (let ((catalog-path (codeawareness--get-catalog-socket-path)))
    (message "Catalog socket path: %s" catalog-path)
    (message "Catalog socket exists: %s" (file-exists-p catalog-path))
    (message "Catalog socket permissions: %s" (file-attributes catalog-path))
    (message "Temporary directory: %s" (temporary-file-directory))
    (message "Directory file name: %s" (directory-file-name (temporary-file-directory))))
  
  ;; Test current connection status
  (message "Current catalog process: %s" codeawareness--ipc-catalog-process)
  (message "Current IPC process: %s" codeawareness--ipc-process)
  (message "Connected status: %s" codeawareness--connected)
  
  ;; Test if we can connect manually
  (condition-case err
      (let ((test-socket (make-network-process
                          :name "test-catalog"
                          :family 'local
                          :service (codeawareness--get-catalog-socket-path))))
        (message "Manual connection test: SUCCESS")
        (delete-process test-socket))
    (error
     (message "Manual connection test: FAILED - %s" err)))
  
  (message "Basic tests completed"))

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
  (codeawareness--init-ipc)
  (codeawareness--init-highlight-faces)
  (add-hook 'after-save-hook #'codeawareness--after-save-hook)
  (add-hook 'post-command-hook #'codeawareness--post-command-hook)
  (codeawareness-log-info "Code Awareness enabled"))

(defun codeawareness--disable ()
  "Disable Code Awareness."
  (codeawareness-log-info "Code Awareness: Disabling and disconnecting")
  
  ;; Clear all highlights
  (codeawareness--clear-all-highlights)
  
  ;; Send disconnect messages before closing connections
  (codeawareness--send-disconnect-messages)
  
  ;; Use force cleanup to ensure all processes are properly deleted
  (codeawareness--force-cleanup)
  
  ;; Clear the store
  (codeawareness--clear-store)
  
  (codeawareness-log-info "Code Awareness disabled"))

;;; Keybindings

(defvar codeawareness-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a t") #'codeawareness-toggle)
    (define-key map (kbd "C-c C-a r") #'codeawareness-refresh)
    (define-key map (kbd "C-c C-a T") #'codeawareness-test)
    (define-key map (kbd "C-c C-a l") #'codeawareness-show-log-buffer)
    (define-key map (kbd "C-c C-a d") #'codeawareness-disconnect)
    (define-key map (kbd "C-c C-a x") #'codeawareness--handle-exit-request)
    ;; Highlight management
    (define-key map (kbd "C-c C-a h") #'codeawareness-clear-highlights)
    (define-key map (kbd "C-c C-a H") #'codeawareness-clear-all-highlights)
    (define-key map (kbd "C-c C-a R") #'codeawareness-refresh-highlights)
    ;; Authentication
    (define-key map (kbd "C-c C-a s") #'codeawareness-auth-status)
    (define-key map (kbd "C-c C-a c") #'codeawareness-connection-status)
    (define-key map (kbd "C-c C-a D") #'codeawareness-debug-connection)
    (define-key map (kbd "C-c C-a y") #'codeawareness-retry-connection)
    map)
  "Keymap for Code Awareness mode.")

;;; Cleanup on Emacs exit

(defun codeawareness--pre-exit-cleanup ()
  "Pre-exit cleanup that runs before Emacs checks for active processes."
  (when codeawareness-mode
    (codeawareness-log-info "Code Awareness: Pre-exit cleanup triggered")
    ;; Send disconnect messages first
    (codeawareness--send-disconnect-messages)
    ;; Force cleanup to remove processes before Emacs checks
    (codeawareness--force-cleanup)
    ;; Return nil to allow normal exit to continue
    nil))

(defun codeawareness--buffer-kill-cleanup ()
  "Cleanup when the last buffer is being killed (potential exit scenario)."
  (when (and codeawareness-mode
             ;; Only trigger if this is the last buffer
             (= (length (buffer-list)) 1))
    (codeawareness-log-info "Code Awareness: Last buffer being killed, cleaning up")
    (codeawareness--send-disconnect-messages)
    (codeawareness--force-cleanup)
    ;; Return nil to allow buffer kill to continue
    nil))

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

;; Add a function to handle the specific case where we're about to exit
(defun codeawareness--handle-exit-request ()
  "Handle exit requests by cleaning up before Emacs checks for active processes."
  (interactive)
  (when codeawareness-mode
    (codeawareness-log-info "Code Awareness: Exit request detected, cleaning up")
    (codeawareness--send-disconnect-messages)
    (codeawareness--force-cleanup)))



;;; Provide

(provide 'codeawareness)
;;; codeawareness.el ends here
