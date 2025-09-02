# Code Awareness for Emacs - Phase 1

This document describes the Phase 1 implementation of Code Awareness for Emacs, which provides the core infrastructure for communicating with the Code Awareness local service.

## Overview

Phase 1 implements the foundational components needed to establish communication with the Code Awareness local service, matching the architecture of the VS Code extension.

## Components

### 1. Core Package (`codeawareness.el`)

The main package that provides:
- **IPC Communication**: Named pipe/socket communication with the local service
- **Configuration Management**: Settings and customization options
- **Buffer Management**: Tracking active buffers and file changes
- **Event Handling**: Hooks for file saves and buffer changes
- **Connection Management**: Automatic reconnection and error handling

### 2. Configuration (`codeawareness-config.el`)

Configuration constants and settings:
- **Theme Support**: Color customization for light/dark themes
- **IPC Settings**: Socket paths and connection parameters
- **Behavior Settings**: Update delays and highlighting preferences

### 3. Logging (`codeawareness-logger.el`)

Comprehensive logging system:
- **Multiple Log Levels**: Error, warn, info, log, debug
- **Log Buffer**: Dedicated buffer for viewing logs
- **Debug Support**: Conditional debug logging

## Installation

1. **Load the package**:
   ```elisp
   (load-file "path/to/ca.emacs/src/elisp/codeawareness.el")
   ```

2. **Enable Code Awareness**:
   ```elisp
   (codeawareness-mode 1)
   ```

## Configuration

### Basic Settings

```elisp
;; Catalog name (default: "catalog")
(setq codeawareness-catalog "my-catalog")

;; Whether to highlight when panel is closed (default: t)
(setq codeawareness-highlight-while-closed t)

;; Update delay in seconds (default: 0.5)
(setq codeawareness-update-delay 0.5)

;; Enable debug mode (default: nil)
(setq codeawareness-debug t)
```

### Theme Colors

```elisp
;; Change colors for different themes
(setq codeawareness-change-color-light "#00b1a420")
(setq codeawareness-change-color-dark "#03445f")
(setq codeawareness-peer-color-light "#ffdd34")
(setq codeawareness-peer-color-dark "#1f1cc2")
(setq codeawareness-merge-color-light "#ffc000")
(setq codeawareness-merge-color-dark "#141299")
```

## Usage

### Commands

- `M-x codeawareness-refresh` - Refresh Code Awareness data
- `M-x codeawareness-show-log-buffer` - Show the log buffer
- `M-x codeawareness-clear-log-buffer` - Clear the log buffer

### Testing

Load the test file to verify functionality:

```elisp
(load-file "path/to/ca.emacs/test-codeawareness.el")
M-x test-codeawareness-all
```

## Architecture

### IPC Communication

The package uses the same IPC protocol as the VS Code extension:

1. **Catalog Connection**: Connects to the catalog service to register the client
2. **Main IPC Connection**: Establishes the main communication channel
3. **Message Protocol**: Uses JSON messages with flow/domain/action structure
4. **Response Handling**: Hash table-based response handler system

### Buffer Management

- **Active Buffer Tracking**: Monitors the currently active buffer
- **File Change Detection**: Hooks into `after-save-hook` and `post-command-hook`
- **Debounced Updates**: Prevents excessive updates with configurable delays

### Error Handling

- **Automatic Reconnection**: Attempts to reconnect on connection failures
- **Graceful Degradation**: Continues operation even if local service is unavailable
- **Comprehensive Logging**: Detailed logs for debugging

## Status

Phase 1 is **complete** and provides:

✅ **Core Infrastructure**: Package structure and initialization  
✅ **IPC Communication**: Full communication with local service  
✅ **Configuration System**: Comprehensive settings management  
✅ **Logging System**: Multi-level logging with dedicated buffer  
✅ **Buffer Management**: Active buffer tracking and file monitoring  
✅ **Error Handling**: Robust error handling and reconnection  
✅ **Testing Framework**: Basic test functions for verification  

## Next Steps

Phase 1 establishes the foundation for the remaining phases:

- **Phase 2**: Code highlighting using overlays and fringe markers
- **Phase 3**: Editor integration and command system
- **Phase 4**: Advanced features (diff viewing, web panel)
- **Phase 5**: Polish and documentation

## Troubleshooting

### Connection Issues

1. **Check if local service is running**:
   ```bash
   # Check if the catalog socket exists
   ls -la /tmp/caw.catalog
   ```

2. **Enable debug mode**:
   ```elisp
   (setq codeawareness-debug t)
   ```

3. **View logs**:
   ```elisp
   M-x codeawareness-show-log-buffer
   ```

### Common Issues

- **"Connection failed"**: Local service not running
- **"Unknown message format"**: Protocol mismatch with local service
- **"Socket error"**: Permission issues with socket files

## Development

### Adding New Features

1. **IPC Actions**: Add new actions to the `codeawareness--transmit` function
2. **Response Handlers**: Implement handlers for new response types
3. **Configuration**: Add new settings to `codeawareness-config.el`
4. **Logging**: Use appropriate log levels for debugging

### Testing

The test file provides basic functionality verification. For comprehensive testing:

1. **Unit Tests**: Test individual functions
2. **Integration Tests**: Test IPC communication
3. **End-to-End Tests**: Test with actual local service

## License

This implementation follows the same license as the original VS Code extension.
