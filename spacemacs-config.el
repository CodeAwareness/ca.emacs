;;; spacemacs-config.el --- Spacemacs configuration for Code Awareness -*- lexical-binding: t -*-

;; Code Awareness configuration for Spacemacs
;; Add this to your ~/.spacemacs file in the dotspacemacs/user-config section

;; Method 1: Load from a specific path (recommended for development)
;; Replace "/path/to/ca.emacs" with the actual path to your ca.emacs directory
(let ((codeawareness-path "~/Code/CodeAwareness/ca.emacs"))  ; Adjust this path
  (when (file-exists-p codeawareness-path)
    (add-to-list 'load-path (expand-file-name "src/elisp" codeawareness-path))
    (require 'codeawareness-config)
    (require 'codeawareness-logger)
    (require 'codeawareness)))

;; Method 2: If you want to enable Code Awareness by default
;; Uncomment the following line:
;; (codeawareness-mode 1)

;; Method 3: If you want to enable it only for specific modes
;; (add-hook 'prog-mode-hook #'codeawareness-mode)

;; Optional: Configure Code Awareness settings
;; (setq codeawareness-debug t)  ; Enable debug mode
;; (setq codeawareness-update-delay 0.3)  ; Faster updates

;; Optional: Add keybindings to Spacemacs
;; (spacemacs/set-leader-keys "a r" 'codeawareness-refresh)
;; (spacemacs/set-leader-keys "a l" 'codeawareness-show-log-buffer)

(provide 'spacemacs-config)
