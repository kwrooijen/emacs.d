# Embellish

A clean, minimal appearance package for Emacs. One mode to rule them all -- `embellish-mode` consolidates frame chrome, spacing, scrolling, font, subwindow styling, and Vertico integration into a single toggle.

Each feature is a separate file that `embellish-mode` loads automatically. Disable any group with a defcustom toggle.

## Installation

### use-package + straight.el

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :config
  (require 'embellish-theme)
  (add-to-list 'custom-theme-load-path "/path/to/embellish")
  (load-theme 'embellish-winter-night t)
  (embellish-mode 1))
```

### Manual

```elisp
(add-to-list 'load-path "/path/to/embellish")
(add-to-list 'custom-theme-load-path "/path/to/embellish")
(require 'embellish)
(require 'embellish-theme)

(load-theme 'embellish-winter-night t)
(embellish-mode 1)
```

## Package overview

| File | Purpose | Auto-loaded by |
|------|---------|----------------|
| `embellish.el` | Frame chrome, spacing, scrolling, visual line mode | -- |
| `embellish-theme.el` | Theme engine (13 semantic colors, 150+ faces) | -- |
| `embellish-font.el` | Font family, weight, size, zoom keybindings | `embellish-mode` |
| `embellish-subwindow.el` | Distinct background for special buffers | `embellish-mode` |
| `embellish-vertico.el` | Vertico padding and truncation hiding | `embellish-mode` |
| `embellish-winter-night-theme.el` | Dark theme | `load-theme` |
| `embellish-winter-morning-theme.el` | Light theme | `load-theme` |

---

## embellish.el

`embellish-mode` is the single entry point. It applies frame chrome, spacing, scrolling, and visual line settings directly, and auto-loads the font, subwindow, and vertico modules.

### Group toggles

Set any of these to `nil` before enabling `embellish-mode` to skip that group:

| Variable | Default | Description |
|----------|---------|-------------|
| `embellish-chrome` | `t` | Frame chrome (toolbar, menubar, scrollbar, etc.) |
| `embellish-spacing` | `t` | Internal borders and window dividers |
| `embellish-scrolling` | `t` | Conservative scrolling defaults |
| `embellish-visual-line-mode` | `t` | Global visual line mode |
| `embellish-subwindow` | `t` | Subwindow styling (auto-loads `embellish-subwindow`) |
| `embellish-font` | `t` | Font settings (auto-loads `embellish-font`) |
| `embellish-vertico` | `t` | Vertico padding (auto-loads `embellish-vertico` when Vertico is available) |

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
| `embellish-window-divider-places` | `'right-only` | Where to show dividers |

The window divider color automatically matches the `default` face background and updates on theme change.

### Scrolling

| Variable | Default | Description |
|----------|---------|-------------|
| `embellish-scroll-margin` | `1` | Lines of margin at edges |
| `embellish-scroll-conservatively` | `10000` | Scroll conservatively |
| `embellish-scroll-step` | `1` | Lines per scroll step |

### Subwindow face

`embellish-subwindow-face` is defined in `embellish.el` (shared by subwindow and vertico modules). Its `:background` attribute controls the subwindow background color. When using `embellish-theme`, this face is set automatically by the theme. Without a theme, set it manually:

```elisp
(set-face-attribute 'embellish-subwindow-face nil :background "#1e2030")
```

---

## embellish-theme.el

A theme engine that generates complete Emacs themes from 13 semantic colors. Ships with two built-in themes.

### How it works

Call `embellish-theme-generate` with a name and a color palette. This generates:

1. A standard Emacs theme (works with `load-theme`, `disable-theme`, etc.)
2. Face triplets for each semantic color (`-fg-face`, `-bg-face`, `-fb-face`)
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

```elisp
(load-theme 'embellish-winter-night t)
```

Switch themes at any time with `M-x load-theme`.

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

These can be used in your own config:

```elisp
(set-face-attribute 'my-face nil :inherit 'embellish-theme-salient-fg-face)
```

---

## embellish-font.el

Font family, weight, size, and zoom keybindings. Auto-loaded by `embellish-mode` when `embellish-font` is non-nil.

### Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `embellish-font-family` | `"Roboto Mono"` | Font family |
| `embellish-font-weight` | `'thin` | Font weight |
| `embellish-font-size` | `200` | Font size (1/10 pt) |
| `embellish-font-step` | `20` | Size adjustment step |

### Keybindings

| Key | Command | Description |
|-----|---------|-------------|
| `C-x C-=` | `embellish-font-size-increase` | Increase font size by step |
| `C-x C--` | `embellish-font-size-decrease` | Decrease font size by step |
| `C-x C-0` | `embellish-font-size-reset` | Reset to default size |

### Standalone usage

```elisp
(require 'embellish-font)
(embellish-font-apply)
```

---

## embellish-subwindow.el

Gives special buffers (REPLs, Magit, Dired, shells) a distinct background and padding. Auto-loaded by `embellish-mode` when `embellish-subwindow` is non-nil.

### Configuration

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
'(cider-repl-mode-hook
  magit-mode-hook
  dired-mode-hook
  sql-interactive-mode-hook
  eshell-mode-hook
  inf-ruby-mode-hook
  messages-buffer-mode-hook
  special-mode-hook)
```

Pre-existing `*Messages*` and `*Warnings*` buffers are automatically marked for styling on enable.

### Standalone usage

```elisp
(require 'embellish-subwindow)
(set-face-attribute 'embellish-subwindow-face nil :background "#1e2030")
(embellish-subwindow-mode 1)
```

---

## embellish-vertico.el

Adds top/bottom padding to the Vertico completion list, enforces minimum minibuffer height, and hides the truncation indicator. Auto-loaded by `embellish-mode` when Vertico is available and `embellish-vertico` is non-nil. If Vertico loads after embellish, the integration activates automatically via `eval-after-load`.

### Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `embellish-vertico-top-padding` | `t` | Add blank line above candidates |
| `embellish-vertico-bottom-padding` | `t` | Add extra height below candidates |
| `embellish-vertico-min-height` | `t` | Enforce minimum height from `vertico-count` |
| `embellish-vertico-hide-truncation` | `t` | Hide `$` truncation indicator |

### Standalone usage

```elisp
(require 'embellish-vertico)
(embellish-vertico-mode 1)
```

---

## Examples

### Full setup (recommended)

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :config
  (require 'embellish-theme)
  (add-to-list 'custom-theme-load-path "/path/to/embellish")
  (load-theme 'embellish-winter-night t)
  (embellish-mode 1))
```

### Minimal (appearance only, no theme)

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :config
  (embellish-mode 1))
```

### Disable specific groups

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :custom
  (embellish-chrome nil)      ;; Keep toolbar, menubar, etc.
  (embellish-subwindow nil)   ;; No subwindow styling
  (embellish-font nil)        ;; Manage fonts yourself
  :config
  (require 'embellish-theme)
  (add-to-list 'custom-theme-load-path "/path/to/embellish")
  (load-theme 'embellish-winter-night t)
  (embellish-mode 1))
```

### Custom font

```elisp
(use-package embellish
  :straight (embellish :type git :host github :repo "kwrooijen/embellish")
  :custom
  (embellish-font-family "Iosevka")
  (embellish-font-weight 'light)
  (embellish-font-size 160)
  :config
  (embellish-mode 1))
```

### Manual subwindow color (no theme)

```elisp
(set-face-attribute 'embellish-subwindow-face nil :background "#1e2030")
(embellish-mode 1)
```
