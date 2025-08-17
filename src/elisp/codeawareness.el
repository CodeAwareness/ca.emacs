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

;;; Configuration

(defun codeawareness--init-config ()
  "Initialize configuration."
  (setq codeawareness--config
        `((catalog . ,codeawareness-catalog)
          (highlight-while-closed . ,codeawareness-highlight-while-closed)
          (update-delay . ,codeawareness-update-delay)))
  (codeawareness-log-info "Code Awareness: Configuration initialized"))

;;; IPC Communication

(defun codeawareness--generate-guid ()
  "Generate a unique GUID for this Emacs instance."
  (concat (number-to-string (car (current-time))) "-" (number-to-string (random 1000000))))

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
     :filter #'codeawareness--ipc-filter)))

(defun codeawareness--ipc-sentinel (process event)
  "Handle IPC process sentinel events."
  (codeawareness-log-info "Code Awareness IPC: %s" event)
  (when (string-match "failed\\|exited" event)
    (setq codeawareness--connected nil)
    (codeawareness--schedule-reconnect)))

(defun codeawareness--ipc-filter (process data)
  "Handle IPC process data."
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
             (error-data (alist-get 'err data)))
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
    (when codeawareness--ipc-process
      (process-send-string codeawareness--ipc-process (concat message "\f"))
      (codeawareness--setup-response-handler domain action))))

(defun codeawareness--setup-response-handler (domain action)
  "Setup response handlers for the given domain and action."
  (let ((res-key (format "res:%s:%s" domain action))
        (err-key (format "err:%s:%s" domain action)))
    (puthash res-key #'codeawareness--handle-success codeawareness--response-handlers)
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
                 :filter #'codeawareness--catalog-filter))
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
  (codeawareness--transmit "auth:info" nil)
  (codeawareness--init-workspace))

(defun codeawareness--init-workspace ()
  "Initialize workspace."
  (codeawareness-log-info "Code Awareness: Workspace initialized"))

(defun codeawareness--force-cleanup ()
  "Force cleanup of all Code Awareness processes and state."
  (codeawareness-log-info "Code Awareness: Force cleaning up all processes")
  
  ;; Cancel any pending timers
  (when codeawareness--update-timer
    (cancel-timer codeawareness--update-timer)
    (setq codeawareness--update-timer nil))
  
  ;; Force delete processes regardless of status
  (when codeawareness--ipc-catalog-process
    (condition-case err
        (progn
          (delete-process codeawareness--ipc-catalog-process)
          (codeawareness-log-info "Code Awareness: Force deleted catalog process"))
      (error
       (codeawareness-log-error "Code Awareness: Error deleting catalog process: %s" err)))
    (setq codeawareness--ipc-catalog-process nil))
  
  (when codeawareness--ipc-process
    (condition-case err
        (progn
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
        codeawareness--active-buffer nil
        codeawareness--active-project nil
        codeawareness--guid nil
        codeawareness--client-registered nil)
  
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
  (codeawareness--update))

(defun codeawareness--post-command-hook ()
  "Hook function for post-command-hook."
  (let ((current-buffer (current-buffer)))
    (when (and current-buffer
               (not (eq current-buffer codeawareness--active-buffer))
               (buffer-file-name current-buffer))
      (codeawareness--set-active-buffer current-buffer))))

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
  (codeawareness--update))

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
  (codeawareness--init-ipc)
  (add-hook 'after-save-hook #'codeawareness--after-save-hook)
  (add-hook 'post-command-hook #'codeawareness--post-command-hook)
  (codeawareness-log-info "Code Awareness enabled"))

(defun codeawareness--disable ()
  "Disable Code Awareness."
  (codeawareness-log-info "Code Awareness: Disabling and disconnecting")
  
  ;; Send disconnect messages before closing connections
  (codeawareness--send-disconnect-messages)
  
  ;; Use force cleanup to ensure all processes are properly deleted
  (codeawareness--force-cleanup)
  
  (codeawareness-log-info "Code Awareness disabled"))

;;; Keybindings

(defvar codeawareness-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a t") #'codeawareness-toggle)
    (define-key map (kbd "C-c C-a r") #'codeawareness-refresh)
    (define-key map (kbd "C-c C-a T") #'codeawareness-test)
    (define-key map (kbd "C-c C-a l") #'codeawareness-show-log-buffer)
    (define-key map (kbd "C-c C-a d") #'codeawareness-disconnect)
    map)
  "Keymap for Code Awareness mode.")

;;; Cleanup on Emacs exit

(defun codeawareness--cleanup-on-exit ()
  "Cleanup Code Awareness when Emacs is about to exit."
  (when codeawareness-mode
    (codeawareness-log-info "Code Awareness: Emacs exiting, cleaning up connections")
    ;; Send disconnect messages first, then force cleanup
    (codeawareness--send-disconnect-messages)
    ;; Force synchronous cleanup to ensure processes are deleted
    (codeawareness--force-cleanup)))

;; Register cleanup function to run when Emacs exits
(add-hook 'kill-emacs-hook #'codeawareness--cleanup-on-exit)

;;; Provide

(provide 'codeawareness)
;;; codeawareness.el ends here
