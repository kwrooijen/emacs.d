# Agent Dashboard — Design & Task Doc

> A single Emacs buffer that gives a live overview of **all AI agents** (running /
> working / waiting-for-reply / killed) **and** the day's **work**: time clocked
> per client and money earned today / this week / this month.

Status: **v1 built.** `agent-dashboard/agent-dashboard.el` implements the
dashboard; `org-clock-data.el` gained `aggregate-by-client`; `init.el` loads it.
Run with `M-x agent-dashboard`. See **Build status** at the bottom for what's
done vs. deferred.

---

## 1. Goal

One place to answer, at a glance:

- Which agents are **running**? Which are **working** right now, which are **idle/ready**?
- Which agents **need me** (waiting on a permission / question)? — the most important signal.
- Which are **stopped / killed / errored**?
- For each agent: project, branch, model, token usage / cost, last activity, the linked ticket.
- How much time have I **clocked today**, and against **which clients**?
- How much **money** have I earned **today / this week / this month**?
- What are my open **work TODOs** (backlog), grouped by client?

It is a read-mostly *cockpit* with a few high-value actions (jump to agent, send
a message, approve a waiting agent, clock in/out, open diff).

---

## 2. Architecture

```
                ┌─────────────────────────────────────────────┐
                │            agent-dashboard.el                │
                │  (orchestration + refresh timer + actions)   │
                └───────────────┬──────────────────────────────┘
                                │ renders via
                                ▼
                ┌─────────────────────────────────────────────┐
                │   org-ui  (agent-dashboard/org-ui/ui.el)     │
                │  template → org buffer + clickable blocks    │
                └───────────────┬──────────────────────────────┘
                                │ data from
        ┌───────────────────────┼───────────────────────────────────┐
        ▼                       ▼                                     ▼
  agent-shell /          org-clock-multi /                     org TODOs /
  agent-shell-manager    org-clock-data                        org-asana
  (agent state)          (time + money)                        (backlog, clients)
```

The dashboard is **a consumer of existing elisp** — almost every number it needs
already exists as a function in this repo. The new code is mostly *layout +
refresh + a handful of action handlers*.

### Rendering layer: `org-ui`

`org-ui` (`agent-dashboard/org-ui/ui.el`) renders `.template` files into a
read-only org-mode buffer with embedded **clickable blocks**.

Key API we will use:

| Function | Use |
|---|---|
| `ui/create-from-template (template buffer-name args)` | initial build of the dashboard buffer |
| `ui/string-from-template (template args)` | render a row/section fragment to a string |
| `ui/render (template alist)` | `{{var}}` substitution |
| `ui/set-block-value-by-id (id value)` | **partial refresh** of one field (e.g. a clock counter, a status) without redraw |
| `ui/heading-replace-content (id string)` | replace a whole section's body (e.g. the agents list) on refresh |
| `ui/org-buffer-edit-transaction` | atomic buffer rewrite preserving point |
| `ui/toggle-block-opt-by-id (id :hidden)` | show/hide sections |

Interactive block syntax inside a template:

```
{{ {:on-click my-handler :id agent-row-123} ⏎ open }}      ; button
{{ {:input t :id rate :type int} {{rate}} }}               ; editable field
{{ {:select t :options (...) :on-select fn} {{model}} }}    ; dropdown
```

`:on-click` handlers receive the block plist; we'll stash the agent/ticket key in
the block `:id` (org-ui auto-uniquifies `:id foo*` → `:id foo_GUID`).

### Data layer — what to query

#### A. Agents (`agent-shell` + `agent-shell-manager`)

Enumerate, then read per-buffer state:

```elisp
(agent-shell-buffers)                      ; all agent-shell buffers, MRU order
(agent-shell-project-buffers)              ; only current project

;; Per buffer:
(agent-shell-manager--get-combined-status buf) ; "Ready"/"Working"/"Waiting"/"Killed"/"Starting..."/"No Session"
(agent-shell-manager--get-status buf)          ; raw: "ready"/"working"/"waiting"/"killed"/"unknown"
(agent-shell-manager--get-agent-kind buf)      ; "Claude" / "GPT-4" ...
(agent-shell-manager--get-model-id buf)        ; model name
(agent-shell-manager--get-session-mode buf)    ; mode
(agent-shell-manager--count-pending-permissions buf) ; >0 ⇒ needs me
(agent-shell-manager--get-cwd buf)             ; working dir
```

Richer state lives buffer-locally in `agent-shell--state`:

```elisp
(with-current-buffer buf
  (let ((st agent-shell--state))
    (agent-shell-status)                          ; 'busy | 'blocked | 'ready
    (agent-shell--permission-pending-p :shell-buffer buf)
    (map-nested-elt st '(:session :title))        ; session title
    (agent-shell-get-model-name st)
    (agent-shell-get-mode-name st)
    (map-elt st :usage)                           ; tokens + cost (see below)
    (map-elt st :last-activity-time)
    (map-elt st :last-entry-type)))               ; what it's doing now
```

**Usage / cost** (`agent-shell--state` → `:usage`):
`:total-tokens :input-tokens :output-tokens :context-used :context-size
:cost-amount :cost-currency`. Format helpers: `agent-shell--format-usage`,
`agent-shell--format-number-compact`, `agent-shell--context-usage-indicator-detailed`.

**Events** (better than polling, for the "needs me" alert):
`agent-shell-subscribe-to :event 'permission-request` and `'turn-complete`,
`'idle`, `'error`. We can subscribe per buffer to flip a "needs attention" flag
and `force-mode-line-update` / refresh the dashboard immediately.

**Ticket linkage**: ticket-launched agents carry buffer-locals
`org-agent-shell--ticket-info` (alist: heading, file, project, branch, worktree,
worktree_path) and `agent-shell--transcript-file`. This is the bridge between an
agent and its org TODO / client. The MCP layer already exposes this via
`kwrooijen/mcp-agent-list` — we can reuse that exact shape.

#### B. Time clocked (`org-clock-multi` + custom helpers)

```elisp
org-clock-multi-clocks                         ; ((CLOCK_MULTI_ID . start-minutes) ...)
org-clock-multi-paused                         ; (CLOCK_MULTI_ID ...)
(kwrooijen/org-clock-clocked-today)            ; minutes clocked today (all agenda files)
(kwrooijen/org-clock-clocked-today t)          ; as org duration string
(kwrooijen/clocked-today)                       ; "H:MM"
(kwrooijen/clocked-today-percentage)            ; "NN％" of 480-min day
```

Active clocks with their client come from the MCP-style query (reusable logic in
`kwrooijen/mcp-get-active-clocks`): each clock → heading, file, elapsed_minutes,
`parent_properties` (which carries `CLIENT` / `ASANA_PROJECT` / `PROJECT`).

#### C. Money earned (`org-clock-data`)

```elisp
(org-clock-data-extract :from "YYYY-MM-DD" :to "YYYY-MM-DD")  ; clock entries w/ :date :tags :hours :minutes
(org-clock-data-aggregate-by-date entries)                    ; → ((DATE . (:minutes M :earnings E)) ...)
org-clock-data-hourly-rates    ; '(("tool2match" . 75) ("aviation_glass" . 90))  [in local-vars.el]
org-clock-data-currency-symbol ; "€"
```

Earnings = `(minutes/60) * rate`, rate looked up from the entry's **tags** (the
client tag). Summing `aggregate-by-date` over a date range gives day / week /
month totals **and** a per-client breakdown.

> **The "gap" is small — reuse the agenda chart machinery.** The agenda already
> computes time **and** money over a span. `org-clock-data-agenda-chart-mode` does
> exactly the pipeline we want:
>
> ```elisp
> (let* ((range  (org-clock-data--agenda-date-range))            ; (FROM . TO)
>        (entries (org-clock-data-extract :tag "work"
>                                         :from (car range) :to (cdr range)))
>        (by-date (org-clock-data-aggregate-by-date entries)))   ; ((DATE (:minutes :earnings)))
>   ...)
> ```
>
> `aggregate-by-date` already returns **minutes AND earnings** per day (earnings =
> `(minutes/60)*rate`, rate from the entry's client tag). Summing the alist gives
> the period total. So the dashboard needs **no clock-summing logic** — only a way
> to produce the `(FROM . TO)` pair *without a live agenda buffer*.
>
> That is the one new helper, mirroring `org-clock-data--agenda-date-range` but
> driven by the current date instead of `org-starting-day` / `org-agenda-current-span`:
>
> ```elisp
> (defun agent-dashboard--fmt (greg)            ; GREG = (MONTH DAY YEAR)
>   (format "%04d-%02d-%02d" (nth 2 greg) (nth 0 greg) (nth 1 greg)))
>
> (defun agent-dashboard--period-range (period)
>   "Return (FROM . TO) date strings for PERIOD: `today', `week', or `month'."
>   (pcase-let* ((`(_ _ _ ,d ,m ,y . ,_) (decode-time))
>                (abs (calendar-absolute-from-gregorian (list m d y))))
>     (pcase period
>       ('today (let ((s (agent-dashboard--fmt (list m d y)))) (cons s s)))
>       ('week  (let* ((dow (calendar-day-of-week (list m d y)))
>                      (off (mod (- dow calendar-week-start-day) 7))   ; respect week start
>                      (a (- abs off)))
>                 (cons (agent-dashboard--fmt (calendar-gregorian-from-absolute a))
>                       (agent-dashboard--fmt (calendar-gregorian-from-absolute (+ a 6))))))
>       ('month (cons (format "%04d-%02d-01" y m)
>                     (format "%04d-%02d-%02d" y m (calendar-last-day-of-month m y)))))))
>
> (defun agent-dashboard--period-totals (period)
>   "Return (:minutes M :earnings E) for PERIOD."
>   (pcase-let* ((`(,from . ,to) (agent-dashboard--period-range period))
>                (by-date (org-clock-data-aggregate-by-date
>                          (org-clock-data-extract :tag "work" :from from :to to))))
>     (list :minutes  (apply #'+ (mapcar (lambda (c) (plist-get (cdr c) :minutes))  by-date))
>           :earnings (apply #'+ (mapcar (lambda (c) (plist-get (cdr c) :earnings)) by-date)))))
> ```
>
> **Per-client breakdown** (the "By client (today)" block) needs the same data
> grouped by tag rather than date. `org-clock-data` only ships `aggregate-by-date`
> and `aggregate-by-hour`, so we add a tiny `aggregate-by-client` in the same shape
> (`((CLIENT . (:minutes :earnings)))`) — or fold it upstream into `org-clock-data`
> next to the others. That plus `period-range` is the *entire* new time/money code.
>
> *(Alternative considered: native `org-clock-get-clocktable` with
> `:tstart/:tend/:scope 'agenda` — what `org-agenda-clockreport-mode` uses. It sums
> time across agenda files but returns a formatted table string and does no money
> math, so `org-clock-data` is the better fit.)*

#### D. Backlog (work TODOs, grouped by client)

Reuse the MCP query shape `kwrooijen/mcp-get-work-todos`: scans
`org-agenda-files` for `:work:` headings in TODO / IN PROGRESS / WAITING, with
`properties` + `parent_properties` (client). Clients enumerated via
`kwrooijen/org-client-names` (files in `~/Documents/org/todos/client/`).
`org-todo-dashboard.el` already does a client-grouped agenda — a model for grouping.

---

## 3. Layout (mockup)

A single `*Agent Dashboard*` org buffer, top-to-bottom:

```
════════════════════════════════════════════════════════════════════════════
  AGENT DASHBOARD                       Fri 2026-06-30 14:22     [ ⟳ refresh ]
════════════════════════════════════════════════════════════════════════════

  TODAY  4:18 / 8:00  [██████████████░░░░░░░░░░░░░░░░] 54%      €322.50
  WEEK   21:40                                                  €1,705.00
  MONTH  78:15                                                  €6,142.00

  By client (today):
    • tool2match        2:30   €187.50    [ clocked ● ]
    • aviation_glass    1:48   €162.00    [ paused  ‖ ]

────────────────────────────────────────────────────────────────────────────
  AGENTS                                    working 2 · waiting 1 · idle 3 · 6 total
────────────────────────────────────────────────────────────────────────────

  ! WAITING — needs you
  ┌──────────────────────────────────────────────────────────────────────┐
  │ ▲ Claude   sentry-errors         tool2match    perm: edit src/api.rb   │
  │            feature/121…-sentry-errors   ctx 41k/200k  $0.84            │
  │            [ open ]  [ approve ]  [ diff ]  [ transcript ]             │
  └──────────────────────────────────────────────────────────────────────┘

  ● WORKING
    Claude   refactor-auth     aviation_glass   "editing login.rb…"  $1.20  [open][diff]
    Claude   add-tests         tool2match       "running rspec…"     $0.42  [open][diff]

  ○ READY / IDLE
    Claude   docs-update       tool2match       idle 12m   $0.10   [open][msg][diff]
    …

  ✖ KILLED / STOPPED
    Claude   old-experiment    -                exited            [restart][remove]

────────────────────────────────────────────────────────────────────────────
  BACKLOG (work todos)                                              [ refresh ]
────────────────────────────────────────────────────────────────────────────
  tool2match
    TODO        Fix Sentry errors on /api          [launch agent]
    IN PROGRESS Add pagination to list view        [open agent]
  aviation_glass
    WAITING     Awaiting client feedback on export
════════════════════════════════════════════════════════════════════════════
```

Color cues reuse the existing theme faces:
- waiting/needs-you → `embellish-theme-error-fg-face` (red, `!` prefix)
- working → `embellish-theme-popout-fg-face`
- ready/idle → `success` / faded
- killed → `error` / faded, sorted to bottom (mirrors agent-shell-manager)

---

## 4. Features

### Must-have (v1)

1. **Agents section**, grouped by status: Waiting → Working → Ready → Killed.
   Per agent: kind, ticket slug, client, branch, model, cost, context %, last
   activity text, status icon/face.
2. **"Needs you" highlight** — agents with pending permissions float to the top
   with a red `!` and a header count. This is the headline feature.
3. **Time & money summary** — today / week / month, time + earnings, with the
   8h progress bar for today.
4. **Per-client breakdown** for today (time + earnings + clock state).
5. **Live refresh** — a 2s timer (mirroring agent-shell-manager) doing *partial*
   updates via `ui/set-block-value-by-id`, plus event-driven immediate refresh on
   `permission-request` / `turn-complete`.
6. **Core actions** (clickable blocks):
   - `open` → jump to agent buffer (`agent-shell-manager-goto` equivalent / `pop-to-buffer`).
   - `approve` → respond to the pending permission (via `agent-shell` permission responder / send into buffer).
   - `diff` → `agent_get_diff` equivalent → show worktree diff.
   - `transcript` → open the transcript file.
   - `msg` → prompt + `agent-shell-insert :submit t` to a Ready/Waiting agent.
   - `clock in/out` per client/ticket → `org-clock-multi-clock-in/out`.
7. **Backlog section** — open work TODOs grouped by client, with `launch agent`
   on a TODO that has `:PROJECT:`/`:BRANCH:` (reuse `org-agent-shell--launch`).

### Useful / later (v2+)

- **Inline tail preview** — last N lines of the agent buffer (`agent_get_tail`)
  expandable under a row via `ui/toggle-block-opt-by-id … :hidden`.
- **Desktop / sound notification** when an agent transitions to Waiting (hook
  `permission-request` event → `notifications-notify`).
- **Restart / kill** killed/stuck agents (`agent-shell-manager-restart/-kill`).
- **Set model / mode** inline via `:select` blocks
  (`agent-shell-manager-set-model/-set-mode`).
- **Earnings goal tracking** — month target vs. actual, projected month-end.
- **Sparkline / bar chart** of earnings per day this week/month
  (`org-clock-data-bar-chart`).
- **Filter** by client or status (toggle blocks).
- **Idle-time / cost alerts** — flag agents idle > N min or cost > €X.
- **Sort options** — by cost, by last-activity, by client.
- **Launch agent straight from backlog** including `agent_set_branch` flow.
- **Asana push** of a backlog TODO (`asana_push`) inline.

---

## 5. Implementation plan

Suggested milestones (each independently useful):

1. **Skeleton + data probes.** `agent-dashboard.el`: a command
   `agent-dashboard` that opens the buffer; write `agent-dashboard--collect-agents`
   returning a list of plists from `agent-shell-buffers`, and
   `agent-dashboard--collect-time` (today/week/month + per-client). Print raw to
   a buffer first to validate the data shape before styling.
2. **Templates.** Author `org-ui` templates: `dashboard.template` (frame +
   section anchors), `agent-row.template`, `client-line.template`,
   `backlog-row.template`. Render with `ui/create-from-template`.
3. **Static render.** Wire collectors → templates → grouped sections via
   `ui/heading-replace-content`. Get it looking right, no refresh yet.
4. **Refresh.** Add the 2s timer (cancel on `kill-buffer-hook`) doing partial
   updates; add `agent-shell-subscribe-to` for immediate waiting/turn-complete.
5. **Actions.** Implement `:on-click` handlers: open, transcript, diff, approve,
   msg, clock in/out.
6. **Period money helpers.** Add `agent-dashboard--period-range` (today/week/
   month) + `agent-dashboard--period-totals`, reusing `org-clock-data-extract` +
   `aggregate-by-date` (same pipeline as `org-clock-data-agenda-chart-mode`). Add
   `aggregate-by-client` for the per-client breakdown.
7. **Backlog section + launch.** Group work TODOs by client; `launch agent`.
8. **Polish.** Faces, sorting, hidden/expand tail preview, notifications.

### New code we actually have to write

- Period range helper (`period-range`/`period-totals`) + `aggregate-by-client` —
  *the only missing computation*; everything else reuses `org-clock-data-extract`
  + `aggregate-by-date`, the same pipeline as `org-clock-data-agenda-chart-mode`.
- Collectors that normalize agent + clock + todo data into render-ready plists.
- The `.template` files and the row-formatting glue.
- Refresh timer + event subscriptions + the handful of action handlers.

Everything else (agent state, status, usage/cost, clock totals, earnings, client
discovery, todo scanning, agent launch) is **already implemented** in
`agent-shell*`, `org-clock-multi`, `org-clock-data`, and the `kwrooijen/mcp-*`
helpers — the dashboard composes them.

---

## 6. Open questions / decisions

1. **Polling vs. events.** agent-shell-manager polls every 2s; agent-shell also
   emits events. Plan: events for the *urgent* signal (waiting), 2s timer for the
   *numbers* (cost, clock counters). Confirm acceptable CPU.
2. **Which agents to show** — all `agent-shell-buffers`, or only ticket-managed
   ones (those with `org-agent-shell--ticket-info`)? Proposal: show all, but
   enrich ticket-managed ones with client/branch.
3. **Approve action semantics** — does the dashboard auto-pick the "allow once"
   option, or pop the agent buffer for the user to choose? Proposal: a button
   that *jumps to the agent at the prompt*; a separate "allow always" power action
   later via `agent-shell-permission-responder-function`.
4. **Reuse MCP elisp directly?** `kwrooijen/mcp-agent-list` / `-get-active-clocks`
   return exactly the shapes we want but as JSON strings. Better to factor their
   pre-`json-encode` body into plain elisp helpers the dashboard and MCP both call,
   to avoid a JSON round-trip. (Light refactor of `mcp.el`.)
5. **Where does this live in the wisdom config** — a new `org/` module that
   `straight`-loads `agent-dashboard.el`, with a leader keybinding. Confirm the
   key.
6. **org-ui is a git submodule/checkout** (`agent-dashboard/org-ui/.git`). Decide
   whether to vendor or `require` it; ensure it's on `load-path`.

---

## 7. Reference — file map

| Concern | File(s) |
|---|---|
| Rendering | `agent-dashboard/org-ui/ui.el` (+ `*.template`) |
| This package | `agent-dashboard/agent-dashboard.el` (empty) |
| Agent state | `vendor/agent-shell/agent-shell.el`, `-usage.el`, `-config.el`, `-heartbeat.el`, `-project.el`, `-worktree.el` |
| Agent list/status | `vendor/agent-shell-manager/agent-shell-manager.el` |
| Multi-clock | `org-clock-multi/org-clock-multi.el` |
| Time + money | `org-clock-data/org-clock-data.el`; rates in `local-vars.el` |
| Clock/agent modeline reuse | `org/theme/modeline.org`, `org/utility/agent-shell.org` |
| Today helpers / agenda | `org/org-agenda.org` |
| Client-grouped agenda model | `org-todo-dashboard/org-todo-dashboard.el` |
| MCP query helpers (reusable shapes) | `mcp-server/mcp.el`, `mcp-server/tools.json` |
| Clients | `~/Documents/org/todos/client/*.org` |

---

## 8. Build status (v1)

**Done** (`agent-dashboard.el`, ~450 lines):

- Entry point `M-x agent-dashboard`; buffer stays in `org-mode` so org-ui's
  block machinery works. A buffer-local minor mode `agent-dashboard-mode` holds
  the refresh timer + `g`/`r`/`q` keys (with evil normal-state bindings added
  when evil loads). All per-row actions are org-ui clickable blocks (RET).
- **Summary**: today / week / month time + earnings, today progress bar,
  per-client breakdown, and currently running clocks — all via
  `org-clock-data-extract` + the new `aggregate-by-date` / `aggregate-by-client`
  (mirrors `org-clock-data-agenda-chart-mode`). New helpers
  `agent-dashboard--period-range` / `--period-totals` supply the date span
  without a live agenda buffer.
- **Agents**: grouped Waiting → Working → Ready → Other → Killed, "needs you"
  count in the header, per-agent kind / slug / branch / model / context /
  cost / session title. Actions: `open`, `answer`, `diff`, `transcript`, `msg`.
- **Backlog**: open `:work:` todos grouped by client, capped per client, with a
  `go` button that jumps to the org heading.
- **Refresh**: background timer (`agent-dashboard-refresh-interval`, default 2s)
  that redraws *only when a cheap state signature changes*, so an idle dashboard
  doesn't churn; cursor line and scroll are preserved across redraws.
- **Defensive data layer**: every dependency probed with `fboundp`/`ignore-errors`,
  so the buffer always renders (missing deps show placeholders).
- **`org-clock-data.el`**: added `org-clock-data--entry-client` +
  `org-clock-data-aggregate-by-client`.
- **`init.el`**: `(require 'agent-dashboard)` added next to the other local pkgs.

**Verified in batch** (`emacs --batch`): byte-compiles clean (only expected
runtime-load warnings); `period-range` gives correct Sun–Sat week / full month;
`aggregate-by-client` math correct; full `--build` renders; org-ui converts the
`{{ ... }}` blocks and each of the 5 adjacent agent-row buttons parses back to
the right `:on-click` + `:key`; point-based block resolution (what RET calls)
returns the correct handler per button. *Not* exercised in batch: live
font-lock display, real agent-shell/clock data, evil keybindings — verify these
interactively.

**Deferred to v2** (noted in the file header too):

- Event-driven instant refresh via `agent-shell-subscribe-to`
  (`permission-request` / `turn-complete`) instead of ≤2s polling latency.
- Inline transcript-tail preview (`agent_get_tail`) expandable per row.
- Desktop/sound notification on transition to Waiting.
- `restart` / `kill` actions; inline `set-model` / `set-mode` `:select` blocks.
- Launch an agent straight from a backlog todo (`agent_set_branch` +
  `org-agent-shell--launch`).
- Earnings goal / month projection; per-day sparkline; filter/sort controls.

### Suggested keybinding

Not added (don't want to guess your leader map). To bind under your `SPC`
leader, e.g. in `org/system.org`:

```elisp
(kwrooijen/leader-key "a d" #'agent-dashboard)  ; adjust to your macro
```
