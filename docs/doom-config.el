;;; doom-config.el --- Doom Emacs configuration for Code Awareness

;; Code Awareness DEV configuration for Doom Emacs
;; Add this to your ~/.doom.d/config.el file

;; Method 1: Load from a specific path (for development)
;; Replace "/path/to/ca.emacs" with the actual path to your ca.emacs directory
(let ((code-awareness-path "~/Code/CodeAwareness/ca.emacs"))  ; Adjust this path
  (when (file-exists-p code-awareness-path)
    (add-to-list 'load-path code-awareness-path)
    (require 'code-awareness)))

;; Method 2: Enable Code Awareness by default
;; (code-awareness-mode 1)

;; Method 3: Enable for specific modes
;; (add-hook 'prog-mode-hook #'code-awareness-mode)

;; Optional: Configure settings
;; (setq code-awareness-debug t)
;; (setq code-awareness-update-delay 0.3)

;; Optional: Add keybindings
;; (map! :leader
;;       (:prefix ("a" . "code-awareness")
;;        "r" #'code-awareness-refresh
;;        "l" #'code-awareness-show-log-buffer))
