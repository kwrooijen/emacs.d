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
  :config
  (set-face-attribute 'embellish-subwindow-face nil :background "#1e2030")
  (embellish-mode 1)
  (embellish-subwindow-mode 1))
```

### Manual

Clone the repository and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/embellish")
(require 'embellish)

(set-face-attribute 'embellish-subwindow-face nil :background "#1e2030")
(embellish-mode 1)
(embellish-subwindow-mode 1)
```

## Modes

### `embellish-mode`

Global minor mode that applies frame chrome, spacing, scrolling, and visual line settings. Toggle with `M-x embellish-mode`.

### `embellish-subwindow-mode`

Global minor mode that styles special buffers with a distinct background and padding. Reads the background color from the `embellish-subwindow-face` face. Toggle with `M-x embellish-subwindow-mode`.

## Configuration

### Subwindow face

The `embellish-subwindow-face` face controls the background color used for subwindow buffers (fringes, header lines, default background, minibuffer). Set its `:background` attribute:

```elisp
(set-face-attribute 'embellish-subwindow-face nil :background "#1e2030")
```

When using `embellish-theme`, this face is set automatically by the theme (to the `:darken` palette color).

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

## Embellish Theme

`embellish-theme` is a theme engine that generates complete Emacs themes from 13 semantic colors. It ships with two built-in themes.

### How it works

A theme file calls `embellish-theme-generate` with a name and a color palette. This generates:

1. A standard Emacs theme (works with `load-theme`, `disable-theme`, etc.)
2. Face triplets for each semantic color (`-fg-face`, `-bg-face`, `-fb-face`) as side effects
3. 150+ face assignments covering core Emacs, Org, Magit, Diredfl, Corfu, Consult, Orderless, Elfeed, and more
4. The `embellish-subwindow-face` background (set to the `:darken` color)

### The 13 semantic colors

| Name | Description |
|------|-------------|
| `:strong` | Bold, primary emphasis |
| `:foreground` | Default text |
| `:subtle` | Muted, secondary elements |
| `:faded` | De-emphasized, comments |
| `:salient` | Accent, keywords, links |
| `:popout` | Strings, special callouts |
| `:error` | Error states |
| `:warning` | Warning states |
| `:success` | Success states |
| `:info` | Informational highlights |
| `:background` | Main background |
| `:darken` | Darker background (sidebars, blocks) |
| `:highlight` | Selection, cursor line |

### Built-in themes

- **`embellish-winter-night`** -- Dark theme with cool blue tones
- **`embellish-winter-morning`** -- Light theme with warm neutral tones

### Usage

Load a theme with the standard Emacs mechanism:

```elisp
(load-theme 'embellish-winter-night t)
```

Switch themes at any time with `M-x load-theme`.

### With use-package

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :config
  (require 'embellish-theme)
  (add-to-list 'custom-theme-load-path "/path/to/embellish")
  (load-theme 'embellish-winter-night t)
  (embellish-mode 1)
  (embellish-subwindow-mode 1))
```

The theme automatically sets `embellish-subwindow-face`, so no manual color configuration is needed.

### Creating a custom theme

Create a file named `my-theme-theme.el`:

```elisp
(require 'embellish-theme)

(embellish-theme-generate 'my-theme
  '(:strong     "#FFFFFF"
    :foreground "#E0E0E0"
    :subtle     "#555555"
    :faded      "#888888"
    :salient    "#6699CC"
    :popout     "#CC9966"
    :error      "#CC6666"
    :warning    "#CCCC66"
    :success    "#66CC66"
    :info       "#66CCCC"
    :background "#1A1A2E"
    :darken     "#16162A"
    :highlight  "#2A2A4E"))
```

Add the directory to `custom-theme-load-path` and load with `M-x load-theme RET my-theme`.

### API

```elisp
;; Get a color from the active palette
(embellish-theme-color :salient)  ;; => "#81A1C1"

;; Get all color names
(embellish-theme-colors)  ;; => (:strong :foreground ...)

;; Color utilities
(embellish-theme-brighten "#222436" 10)   ;; brighten by 10%
(embellish-theme-saturate "#222436" 10)   ;; saturate by 10%
```

### Generated faces

For each of the 13 semantic colors, three faces are generated:

- `embellish-theme-NAME-fg-face` -- foreground only
- `embellish-theme-NAME-bg-face` -- background only
- `embellish-theme-NAME-fb-face` -- foreground + background

Plus a composite: `embellish-theme-faded-darken-fb-face`.

These can be used in your own config, e.g. to style a custom modeline:

```elisp
(set-face-attribute 'my-face nil :inherit 'embellish-theme-salient-fg-face)
```

## Examples

### Minimal (appearance only, no theme)

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :config
  (embellish-mode 1))
```

### Full (appearance + theme + subwindows)

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :config
  (require 'embellish-theme)
  (add-to-list 'custom-theme-load-path "/path/to/embellish")
  (load-theme 'embellish-winter-night t)
  (embellish-mode 1)
  (embellish-subwindow-mode 1))
```

### Disable a group

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :custom
  (embellish-chrome nil)  ;; Keep toolbar, menubar, etc.
  :config
  (require 'embellish-theme)
  (add-to-list 'custom-theme-load-path "/path/to/embellish")
  (load-theme 'embellish-winter-night t)
  (embellish-mode 1)
  (embellish-subwindow-mode 1))
```

### Manual subwindow color (no theme)

```elisp
(set-face-attribute 'embellish-subwindow-face nil :background "#1e2030")
(embellish-subwindow-mode 1)
```
