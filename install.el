;;; install.el --- Installation script for Code Awareness -*- lexical-binding: t -*-

;; Run this script to install Code Awareness in your Emacs configuration

(defun codeawareness-install ()
  "Install Code Awareness package."
  (interactive)
  
  ;; Get the current directory (where this script is located)
  (let* ((package-dir (file-name-directory (or load-file-name buffer-file-name)))
         (elisp-dir (expand-file-name "src/elisp" package-dir))
         (config-file (expand-file-name "~/.spacemacs"))
         (init-file (expand-file-name "~/.emacs.d/init.el"))
         (config-content nil))
    
    ;; Check if the elisp directory exists
    (unless (file-exists-p elisp-dir)
      (error "Elisp directory not found: %s" elisp-dir))
    
    ;; Add to load path
    (add-to-list 'load-path elisp-dir)
    
    ;; Try to load the package
    (condition-case err
        (progn
          (require 'codeawareness-config)
          (require 'codeawareness-logger)
          (require 'codeawareness)
          (message "Code Awareness loaded successfully!"))
      (error
       (message "Error loading Code Awareness: %s" err)
       (return-from codeawareness-install)))
    
    ;; Offer to add to configuration file
    (when (y-or-n-p "Add Code Awareness to your Emacs configuration?")
      (cond
       ;; Spacemacs
       ((file-exists-p config-file)
        (message "Spacemacs configuration detected. Please add the following to your ~/.spacemacs file in the dotspacemacs/user-config section:")
        (message "")
        (message "(let ((codeawareness-path \"%s\"))" package-dir)
        (message "  (when (file-exists-p codeawareness-path)")
        (message "    (add-to-list 'load-path (expand-file-name \"src/elisp\" codeawareness-path))")
        (message "    (require 'codeawareness-config)")
        (message "    (require 'codeawareness-logger)")
        (message "    (require 'codeawareness)))")
        (message "")
        (message "Then restart Emacs or reload your configuration."))
       
       ;; Regular Emacs
       ((file-exists-p init-file)
        (message "Regular Emacs configuration detected. Please add the following to your ~/.emacs.d/init.el file:")
        (message "")
        (message "(add-to-list 'load-path \"%s\")" elisp-dir)
        (message "(require 'codeawareness-config)")
        (message "(require 'codeawareness-logger)")
        (message "(require 'codeawareness)")
        (message "")
        (message "Then restart Emacs or reload your configuration."))
       
       ;; No configuration found
       (t
        (message "No configuration file found. Please create one and add the load path."))))
    
    ;; Offer to enable the mode
    (when (y-or-n-p "Enable Code Awareness mode now?")
      (codeawareness-mode 1)
      (message "Code Awareness mode enabled!"))))

;; Run installation if this file is loaded
(when (and (not noninteractive) (called-interactively-p 'any))
  (codeawareness-install))

(provide 'install)
