# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is an Emacs configuration built on the **Wisdom framework**, a custom org-based literate configuration system. Configuration is written in `.org` files under `org/` and compiled into Emacs Lisp.

## Architecture

### Boot Process
1. `early-init.el` - Frame setup (dimensions, borders)
2. `init.el` - Bootstraps straight.el, installs org, then loads Wisdom framework
3. Wisdom processes `org/*.org` files using property headers to generate package configurations

### Wisdom Framework Property Headers
The framework uses org property drawers to configure packages declaratively:
- `:PACKAGE:` - Package name to configure
- `:STRAIGHT:` - Straight.el installation spec (t or git recipe)
- `:HOOK:` - Mode hooks to attach
- `:CUSTOM:` - Customization variables
- `:CONFIG:` - Configuration code blocks
- `:GENERAL:` - Keybindings via general.el
- `:INIT:` - Pre-load initialization code
- `:DEMAND:` - Force immediate loading

### Package Management
- **Straight.el** - Primary package manager (git-based)
- `default.el` - Version lockfile with ~130 package commit hashes
- Custom forks: `multiple-cursors` from `kwrooijen/mc`

### Key Directories
- `org/` - Configuration modules (~7,100 lines of org documentation)
- `org/editing/` - Evil mode, multiple cursors, navigation
- `org/lang/` - Language configs (Clojure, Python, Ruby, Rust, etc.)
- `org/utility/` - Tools (Magit, Dired, shell)
- `db/` - State files (snippets, bookmarks, mc-lists)

## Commands

Update all packages and freeze versions:
```elisp
M-x kwrooijen/upgrade-packages
```
This runs `straight-pull-all` followed by `straight-freeze-versions`.

## Key Configuration Files

| File | Purpose |
|------|---------|
| `org/system.org` | Core keybindings, leader keys (SPC, ') |
| `org/editing/evil.org` | Evil mode + collections |
| `org/lisp.org` | Lispy, Lispyville, Paredit integration |
| `org/lang/clojure.org` | CIDER, clj-refactor setup |
| `org/vertico.org` | Completion UI (Vertico + Consult) |
| `org/utility/git.org` | Magit configuration |
| `org/functions.org` | Custom utility functions |

## Custom Functions

Functions prefixed with `kwrooijen/`:
- `kwrooijen/upgrade-packages` - Update and freeze package versions
- `kwrooijen/org-focus-this-heading` - Focus single org heading
- `kwrooijen/load-secret-env` - Load encrypted .env files
- `kwrooijen/project-root` - Get project root directory
- `kwrooijen/tab` - Tab with Copilot fallback
- `kwrooijen/indent-buffer` - Format entire buffer

## Development Notes

- Wisdom framework is loaded from `~/Programming/Emacs/wisdom/wisdom.el` for local development
- Evil mode keybindings throughout
- Heavy Lisp tooling: Lispy + Lispyville + Paredit for s-expression editing
- macOS-specific: Command key as Meta, transparent titlebar
