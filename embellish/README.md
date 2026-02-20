# Embellish

A clean, minimal appearance package for Emacs. Consolidates frame chrome, spacing, scrolling, and subwindow styling into a single configurable package.

## Features

- **Frame chrome** -- Hide toolbar, menubar, scrollbar, fringes, titlebar text, and proxy icon
- **Spacing** -- Internal frame borders and window dividers with configurable width
- **Scrolling** -- Smooth, conservative scrolling defaults
- **Visual line mode** -- Global word wrapping
- **Subwindow styling** -- Give special buffers (REPLs, Magit, Dired, shells) a distinct background and padding

Every setting is individually configurable, and feature groups can be disabled entirely.

## Installation

### use-package + straight.el

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :custom
  (embellish-background-color "#1e2030")
  :config
  (embellish-mode 1)
  (embellish-subwindow-mode 1))
```

### Manual

Clone the repository and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/embellish")
(require 'embellish)

(setq embellish-background-color "#1e2030")
(embellish-mode 1)
(embellish-subwindow-mode 1)
```

## Modes

### `embellish-mode`

Global minor mode that applies frame chrome, spacing, scrolling, and visual line settings. Toggle with `M-x embellish-mode`.

### `embellish-subwindow-mode`

Global minor mode that styles special buffers with a distinct background and padding. Requires `embellish-background-color` to be set. Toggle with `M-x embellish-subwindow-mode`.

## Configuration

### Background color

A single color controls subwindow backgrounds, fringes, header lines, and the minibuffer.

```elisp
(setq embellish-background-color "#1e2030")
```

When `nil` (default), no color styling is applied.

### Group toggles

Disable entire feature groups without touching individual settings:

```elisp
(setq embellish-chrome nil)     ;; Skip all chrome (toolbar, menubar, etc.)
(setq embellish-spacing nil)    ;; Skip all spacing (borders, dividers)
(setq embellish-scrolling nil)  ;; Skip all scrolling settings
```

### Frame chrome

All default to `t`:

| Variable | Description |
|----------|-------------|
| `embellish-hide-toolbar` | Disable tool bar |
| `embellish-hide-menubar` | Disable menu bar |
| `embellish-hide-scrollbar` | Disable scroll bar |
| `embellish-hide-cursor-in-inactive-windows` | Hide cursor in non-selected windows |
| `embellish-disable-cursor-blink` | Stop cursor blinking |
| `embellish-hide-fringes` | Set fringes to zero width |
| `embellish-transparent-titlebar` | Transparent titlebar (macOS) |
| `embellish-hide-title` | Hide buffer name from title bar |
| `embellish-hide-proxy-icon` | Hide proxy icon (macOS) |

### Spacing

| Variable | Default | Description |
|----------|---------|-------------|
| `embellish-internal-border-width` | `24` | Frame internal border in pixels |
| `embellish-window-divider-width` | `24` | Window divider width in pixels |
| `embellish-window-divider-places` | `'right-only` | Where to show dividers (`right-only`, `bottom-only`, or `t`) |

The window divider color automatically inherits from the `default` face background.

### Scrolling

| Variable | Default | Description |
|----------|---------|-------------|
| `embellish-scroll-margin` | `1` | Lines of margin at edges |
| `embellish-scroll-conservatively` | `10000` | Scroll conservatively |
| `embellish-scroll-step` | `1` | Lines per scroll step |

### Visual line mode

| Variable | Default | Description |
|----------|---------|-------------|
| `embellish-visual-line-mode` | `t` | Enable global visual line mode |

### Subwindow styling

| Variable | Default | Description |
|----------|---------|-------------|
| `embellish-subwindow-fringe-width` | `24` | Fringe width for subwindow buffers |
| `embellish-subwindow-header-height` | `190` | Header line height for top padding |
| `embellish-subwindow-hooks` | See below | Mode hooks that trigger styling |
| `embellish-subwindow-style-transient` | `t` | Style transient popups |
| `embellish-subwindow-style-minibuffer` | `t` | Style the minibuffer |
| `embellish-subwindow-minibuffer-margins` | `2` | Minibuffer left/right margin |

Default hooks:

```elisp
(setq embellish-subwindow-hooks
  '(cider-repl-mode-hook
    magit-mode-hook
    dired-mode-hook
    sql-interactive-mode-hook
    eshell-mode-hook
    inf-ruby-mode-hook
    messages-buffer-mode-hook))
```

## Example: Minimal config

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :config
  (embellish-mode 1))
```

This enables all defaults with no subwindow styling (since `embellish-background-color` is nil).

## Example: Full config

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :custom
  (embellish-background-color "#1e2030")
  (embellish-internal-border-width 16)
  (embellish-window-divider-width 16)
  (embellish-subwindow-fringe-width 16)
  (embellish-hide-fringes nil)
  :config
  (embellish-mode 1)
  (embellish-subwindow-mode 1))
```

## Example: Disable a group

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :custom
  (embellish-chrome nil)  ;; Keep toolbar, menubar, etc.
  (embellish-background-color "#1e2030")
  :config
  (embellish-mode 1)
  (embellish-subwindow-mode 1))
```
