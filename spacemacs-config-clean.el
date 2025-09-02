;;; spacemacs-config-clean.el --- Clean Spacemacs configuration for Code Awareness -*- lexical-binding: t -*-

;; Clean Code Awareness configuration for Spacemacs
;; This version removes old byte-compiled files and ensures clean loading

;; Remove any old byte-compiled files
(let ((elisp-dir "/home/mv/Code/CodeAwareness/ca.emacs/src/elisp"))
  (when (file-exists-p elisp-dir)
    (dolist (file (directory-files elisp-dir t "\\.elc$"))
      (when (file-exists-p file)
        (delete-file file)
        (message "Deleted old byte-compiled file: %s" file)))))

;; Code Awareness Configuration
(let ((codeawareness-path "/home/mv/Code/CodeAwareness/ca.emacs"))
  (when (file-exists-p codeawareness-path)
    (let ((elisp-dir (expand-file-name "src/elisp" codeawareness-path)))
      (when (file-exists-p elisp-dir)
        (add-to-list 'load-path elisp-dir)
        (message "Added Code Awareness to load path: %s" elisp-dir)
        
        ;; Load modules in order
        (condition-case err
            (progn
              (require 'codeawareness-config)
              (message "✓ codeawareness-config loaded"))
          (error
           (message "✗ Error loading codeawareness-config: %s" err)))
        
        (condition-case err
            (progn
              (require 'codeawareness-logger)
              (message "✓ codeawareness-logger loaded"))
          (error
           (message "✗ Error loading codeawareness-logger: %s" err)))
        
        (condition-case err
            (progn
              (require 'codeawareness)
              (message "✓ codeawareness loaded"))
          (error
           (message "✗ Error loading codeawareness: %s" err)))))))

;; Optional: Enable Code Awareness by default
;; (codeawareness-mode 1)

;; Optional: Add Spacemacs keybindings
;; (spacemacs/set-leader-keys "a r" 'codeawareness-refresh)
;; (spacemacs/set-leader-keys "a l" 'codeawareness-show-log-buffer)

(provide 'spacemacs-config-clean)
