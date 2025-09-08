;;; spacemacs-config.el --- Spacemacs configuration for Code Awareness

;; Code Awareness DEV configuration for Spacemacs
;; Add this to your ~/.spacemacs file in the dotspacemacs/user-config section

;; Method 1: Load from a specific path (for development)
;; Replace "/path/to/ca.emacs" with the actual path to your ca.emacs directory
(let ((code-awareness-path "~/Code/CodeAwareness/ca.emacs"))  ; Adjust this path
  (when (file-exists-p code-awareness-path)
    (add-to-list 'load-path code-awareness-path)
    (require 'code-awareness)))

;; Method 2: If you want to enable Code Awareness by default
;; Uncomment the following line:
;; (code-awareness-mode 1)

;; Method 3: If you want to enable it only for specific modes
;; (add-hook 'prog-mode-hook #'code-awareness-mode)

;; Optional: Configure Code Awareness settings
;; (setq code-awareness-debug t)  ; Enable debug mode
;; (setq code-awareness-update-delay 0.3)  ; Faster updates

;; Optional: Add keybindings to Spacemacs
;; (spacemacs/set-leader-keys "a r" 'code-awareness-refresh)
;; (spacemacs/set-leader-keys "a l" 'code-awareness-show-log-buffer)
