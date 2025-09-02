# Code Awareness Setup Guide

This guide explains how to set up Code Awareness in your Emacs/Spacemacs configuration.

## Quick Setup

### Method 1: Automatic Installation (Recommended)

1. **Open the installation script in Emacs**:
   ```elisp
   M-x find-file ~/Code/CodeAwareness/ca.emacs/install.el
   ```

2. **Run the installation**:
   ```elisp
   M-x eval-buffer
   ```

3. **Follow the prompts** to add the configuration to your Spacemacs file.

### Method 2: Manual Setup

#### For Spacemacs

Add the following to your `~/.spacemacs` file in the `dotspacemacs/user-config` section:

```elisp
;; Code Awareness Configuration
(let ((codeawareness-path "~/Code/CodeAwareness/ca.emacs"))  ; Adjust this path
  (when (file-exists-p codeawareness-path)
    (add-to-list 'load-path (expand-file-name "src/elisp" codeawareness-path))
    (require 'codeawareness-config)
    (require 'codeawareness-logger)
    (require 'codeawareness)))

;; Optional: Enable Code Awareness by default
;; (codeawareness-mode 1)

;; Optional: Add Spacemacs keybindings
;; (spacemacs/set-leader-keys "a r" 'codeawareness-refresh)
;; (spacemacs/set-leader-keys "a l" 'codeawareness-show-log-buffer)
```

#### For Regular Emacs

Add the following to your `~/.emacs.d/init.el` file:

```elisp
;; Code Awareness Configuration
(add-to-list 'load-path "~/Code/CodeAwareness/ca.emacs/src/elisp")
(require 'codeawareness-config)
(require 'codeawareness-logger)
(require 'codeawareness)

;; Optional: Enable Code Awareness by default
;; (codeawareness-mode 1)
```

## Verification

After setup, you can verify the installation:

1. **Check if the package is loaded**:
   ```elisp
   M-x describe-variable codeawareness-mode
   ```

2. **Test basic functionality**:
   ```elisp
   M-x test-codeawareness-all
   ```

3. **Enable the mode**:
   ```elisp
   M-x codeawareness-mode
   ```

## Troubleshooting

### "Cannot open load file" Error

This error occurs when Emacs can't find the package files. Solutions:

1. **Check the path**: Make sure the path in your configuration points to the correct directory
2. **Use absolute paths**: Use `~/Code/CodeAwareness/ca.emacs` instead of relative paths
3. **Verify file structure**: Ensure the files are in `ca.emacs/src/elisp/`

### "Symbol's function definition is void" Error

This means a required function isn't loaded. Solutions:

1. **Load all modules**: Make sure you're requiring all three modules:
   - `codeawareness-config`
   - `codeawareness-logger`
   - `codeawareness`

2. **Check load order**: Load the modules in the correct order (config → logger → main)

### Spacemacs-specific Issues

1. **Configuration location**: Make sure you're adding the code to `dotspacemacs/user-config`, not `dotspacemacs/init`
2. **Reload configuration**: After making changes, reload your Spacemacs configuration:
   ```elisp
   M-x spacemacs/sync-configuration-layers
   ```

## Configuration Options

You can customize Code Awareness behavior:

```elisp
;; Enable debug mode
(setq codeawareness-debug t)

;; Adjust update delay
(setq codeawareness-update-delay 0.3)

;; Change catalog name
(setq codeawareness-catalog "my-catalog")

;; Customize colors
(setq codeawareness-change-color-light "#00b1a420")
(setq codeawareness-change-color-dark "#03445f")
```

## Next Steps

Once the package is loaded successfully, you can:

1. **Enable the mode**: `M-x codeawareness-mode`
2. **View logs**: `M-x codeawareness-show-log-buffer`
3. **Test functionality**: `M-x test-codeawareness-all`
4. **Proceed to Phase 2**: Implement code highlighting features

## Support

If you encounter issues:

1. **Check the log buffer**: `M-x codeawareness-show-log-buffer`
2. **Enable debug mode**: `(setq codeawareness-debug t)`
3. **Verify local service**: Make sure your Code Awareness local service is running
