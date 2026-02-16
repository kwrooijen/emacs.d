# org-clock-multi

Multiple simultaneous org clocks for Emacs. Maintains a shadow clock system alongside `org-clock`, reusing org-clock internals for LOGBOOK writing to ensure compatibility.

## Requirements

- Emacs 27.1+
- Org 9.0+

## Installation

Load or require `org-clock-multi.el`. State is automatically loaded on startup from `org-clock-multi-persist-file`.

```elisp
(require 'org-clock-multi)
```

## Usage

### Commands

| Command | Description |
|---------|-------------|
| `org-clock-multi-clock-in` | Clock in the heading at point |
| `org-clock-multi-clock-out` | Clock out the heading at point |
| `org-clock-multi-clock-out-all` | Clock out all active clocks |
| `org-clock-multi-clock-pause` | Pause clock (clock out + remember for resumption) |
| `org-clock-multi-clock-resume` | Resume a paused clock (alias for clock-in) |
| `org-clock-multi-clock-cancel` | Cancel clock without writing LOGBOOK |
| `org-clock-multi-clock-cancel-all` | Cancel all clocks without writing LOGBOOK |
| `org-clock-multi-clock-goto` | Jump to an active clock via completing-read |

### Agenda Commands

| Command | Description |
|---------|-------------|
| `org-clock-multi-agenda-clock-in` | Clock in task at point in agenda |
| `org-clock-multi-agenda-clock-out` | Clock out task at point in agenda |
| `org-clock-multi-agenda-clock-out-all` | Clock out all from agenda |
| `org-clock-multi-agenda-clock-pause` | Pause task at point in agenda |
| `org-clock-multi-agenda-clock-cancel` | Cancel task at point in agenda |
| `org-clock-multi-agenda-clock-cancel-all` | Cancel all from agenda |

### Predicates

| Function | Description |
|----------|-------------|
| `org-clock-multi-clocking-p` | Non-nil if heading at point is clocked in |
| `org-clock-multi-paused-p` | Non-nil if heading at point is paused |

## How It Works

Each heading is identified by a `CLOCK_MULTI_ID` property (UUID generated via `org-id-new`). Active clocks are stored as `(id . start-minutes)` pairs where `id` is the `CLOCK_MULTI_ID` string. Headings are located by searching `org-agenda-files`. State is persisted to disk so clocks survive Emacs restarts.

**Note:** Currently only headings in `org-agenda-files` are supported.

LOGBOOK entries are written using `org-clock-find-position` and match org-clock's exact format, so `org-clock-sum` and agenda clock reports work normally.

## Testing

Run the full test suite (31 tests):

```sh
emacs -batch \
  -l org-clock-multi.el \
  -l test/org-clock-multi-test.el \
  -f ert-run-tests-batch-and-exit
```
