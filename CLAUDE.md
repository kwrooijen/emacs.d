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

## MCP Tools

This project provides MCP tools via `mcp-server/emacs-mcp-server.rb`. Always use these tools when relevant:

### Todo Management
- `get_work_todo_files` - List org files in `~/Documents/org/todos/client/` (returns JSON array of paths)
- `get_work_todos` - Get all TODO items tagged :work: (states: TODO, IN PROGRESS, WAITING) with properties and parent_properties
- `get_work_todo(file, heading)` - Get full details of a single todo: properties, parent_properties, logbook (clock entries), body, subtasks
- `edit_work_todo(file, heading, state?, set_properties?, delete_properties?, body?)` - Edit a todo's state (TODO/IN PROGRESS/WAITING/DONE/CANCELLED), properties, or body content
- `create_work_todo(file, heading, title, state?, body?)` - Create a new TODO heading under a parent heading. Inherits :ASANA_PROJECT: from parent
- `edit_work_todo_ticket_description(file, heading, body)` - Set or replace the #+BEGIN_QUOTE ticket block (the Asana ticket description)
- `edit_work_todo_implementation(file, heading, body)` - Set or replace the #+BEGIN_QUOTE implementation block (implementation details sent to agent LLM, not pushed to Asana)

### Asana Integration
- `asana_push(file, heading)` - Push an org TODO to Asana as a new task. Requires inherited :ASANA_PROJECT:, must not already have :ASANA:. Creates task, adds to first board column, sets :ASANA: and :ASANA_SECTION: properties

### Clock Management (org-clock-multi)
- `get_active_clocks` - List all running org-clock-multi clocks with heading, file, elapsed_minutes, properties, and parent_properties
- `clock_in(file, heading)` - Clock in a heading (adds to active clocks without affecting others)
- `clock_out(file, heading)` - Clock out a heading (writes LOGBOOK entry and removes from active clocks)

### Agent Management
- `agent_set_branch(file, heading, branch)` - Set :BRANCH: property on a ticket. Provide a concise, logical kebab-case slug (max 20 chars) that captures the essence of the task. The Asana ID and `feature/` prefix are added automatically (e.g. `sentry-errors` becomes `feature/1213416506379504-sentry-errors`)
- `agent_launch(file, heading)` - Create git worktree, write ticket.org, and start an agent-shell for a ticket. Requires :PROJECT: and :BRANCH: properties
- `agent_list` - List all ticket-managed agent-shell workspaces with status (Ready/Working/Waiting/Killed), transcript path, and ticket metadata
- `agent_get_prompt(file, heading)` - Read the ticket.org prompt that was sent to the agent
- `agent_get_transcript(file, heading)` - Read the agent's session transcript (markdown). Falls back to most recent transcript file if shell is no longer running
- `agent_get_diff(file, heading)` - Get git diff (stat + full diff) for the agent's worktree branch vs base branch
- `agent_send_message(file, heading, message)` - Send a message to a running agent-shell. Agent must be Ready or Waiting (use agent_list to check)
- `agent_resend(file, heading)` - Rewrite ticket.org from org heading body and tell agent to re-read it

## Development Notes

- Wisdom framework is loaded from `~/Programming/Emacs/wisdom/wisdom.el` for local development
- Evil mode keybindings throughout
- Heavy Lisp tooling: Lispy + Lispyville + Paredit for s-expression editing
- macOS-specific: Command key as Meta, transparent titlebar
