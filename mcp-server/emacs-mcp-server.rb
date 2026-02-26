#!/usr/bin/env ruby
# frozen_string_literal: true

# MCP (Model Context Protocol) stdio server that bridges Claude to Emacs.
# Receives JSON-RPC 2.0 tool calls on stdin, executes elisp via emacsclient,
# and returns results on stdout.

require "json"
require "open3"

TOOLS = [
  {
    name: "get_work_todos",
    description: "Get all org-mode TODO items tagged :work: with states TODO, IN PROGRESS, or WAITING. " \
                 "Returns a JSON array of objects with heading, state, tags, file, properties, and parent_properties fields.",
    inputSchema: {
      type: "object",
      properties: {}
    }
  },
  {
    name: "get_work_todo_files",
    description: "List org files in the client todos directory. " \
                 "Returns a JSON array of absolute file paths.",
    inputSchema: {
      type: "object",
      properties: {}
    }
  },
  {
    name: "get_active_clocks",
    description: "Get all currently running org-clock-multi clocks. " \
                 "Returns a JSON array of objects with clock_id, heading, file, elapsed_minutes, " \
                 "properties, and parent_properties (including CLIENT). " \
                 "Use this to check what is currently being tracked and detect conflicts " \
                 "(e.g. OVERIGE and a task for the same client should not run simultaneously).",
    inputSchema: {
      type: "object",
      properties: {}
    }
  },
  {
    name: "clock_in",
    description: "Clock in a heading using org-clock-multi. " \
                 "Adds to the list of active clocks without affecting other running clocks.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the heading"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "clock_out",
    description: "Clock out a heading using org-clock-multi. " \
                 "Writes a LOGBOOK entry and removes from active clocks.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the heading"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "get_work_todo",
    description: "Get full details of a single work TODO item including properties, " \
                 "parent properties, body content, subtasks, and logbook (clock entries with start, end, duration). " \
                 "Identify the todo by its file path and heading text.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "edit_work_todo",
    description: "Edit a work TODO item. Can change state, set/delete properties, or replace body content. " \
                 "Identify the todo by its file path and heading text.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        },
        state: {
          type: "string",
          description: "New TODO state",
          enum: ["TODO", "IN PROGRESS", "WAITING", "DONE", "CANCELLED"]
        },
        set_properties: {
          type: "object",
          description: "Key-value pairs of properties to set or update",
          additionalProperties: { type: "string" }
        },
        delete_properties: {
          type: "array",
          description: "Property names to remove",
          items: { type: "string" }
        },
        body: {
          type: "string",
          description: "New body content to replace existing body (text between properties and sub-headings)"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "create_work_todo",
    description: "Create a new TODO heading under a parent heading (e.g. a client project). " \
                 "The new heading inherits :ASANA_PROJECT:, :PROJECT:, and tags from the parent. " \
                 "Identify the parent by its file path and heading text.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the parent heading"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the parent (e.g. the client/project heading)"
        },
        title: {
          type: "string",
          description: "Title for the new TODO heading"
        },
        state: {
          type: "string",
          description: "TODO state (default: TODO)",
          enum: ["TODO", "IN PROGRESS", "WAITING", "DONE", "CANCELLED"]
        },
        body: {
          type: "string",
          description: "Optional initial body text for the heading"
        }
      },
      required: ["file", "heading", "title"]
    }
  },
  {
    name: "edit_work_todo_body",
    description: "Set or replace the #+BEGIN_QUOTE ticket block on a TODO heading. " \
                 "Creates the block if it doesn't exist, or replaces its contents if it does. " \
                 "This is the ticket description that will be pushed to Asana.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        },
        body: {
          type: "string",
          description: "Content for the #+BEGIN_QUOTE ticket block"
        }
      },
      required: ["file", "heading", "body"]
    }
  },
  {
    name: "asana_push",
    description: "Push an org TODO heading to Asana as a new task. " \
                 "The heading must have an inherited :ASANA_PROJECT: property and must NOT already have an :ASANA: property. " \
                 "Creates the task in Asana, adds it to the first board column, " \
                 "and sets :ASANA: and :ASANA_SECTION: properties on the heading.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "agent_set_branch",
    description: "Set the :BRANCH: property on an org TODO heading for agent-shell. " \
                 "If no branch is provided, auto-generates a concise branch name from the heading text, " \
                 "prefixed with the Asana ticket ID when available " \
                 "(e.g. 'feature/1213416506379504-sentry-errors').",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        },
        branch: {
          type: "string",
          description: "Branch name to set. If omitted, auto-generated from heading text and Asana ID"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "agent_launch",
    description: "Launch an agent-shell workspace for a specific org TODO ticket. " \
                 "Creates a git worktree, writes ticket.org from the heading body, " \
                 "and starts an agent-shell (Claude Code). Requires :PROJECT: (inherited) and :BRANCH: properties. " \
                 "Use agent_set_branch first if :BRANCH: is not set.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "agent_list",
    description: "List all ticket-managed agent-shell workspaces. " \
                 "Returns only agents started from org TODO tickets (not manually started ones). " \
                 "Each entry includes heading, file, project, branch, worktree, " \
                 "status (Ready/Working/Waiting/Killed/Starting.../No Session), and transcript file path.",
    inputSchema: {
      type: "object",
      properties: {}
    }
  },
  {
    name: "agent_get_prompt",
    description: "Read the ticket.org prompt file for an agent workspace. " \
                 "Returns the content that was sent to the agent when it was launched.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "agent_get_transcript",
    description: "Read the transcript of an agent-shell session. " \
                 "Returns the markdown transcript including messages and tool calls. " \
                 "Falls back to most recent transcript file on disk if the shell is no longer running.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        }
      },
      required: ["file", "heading"]
    }
  },
  {
    name: "agent_get_diff",
    description: "Get the git diff for an agent's worktree branch compared to its base branch. " \
                 "Returns both a stat summary and the full diff output.",
    inputSchema: {
      type: "object",
      properties: {
        file: {
          type: "string",
          description: "Absolute path to the org file containing the todo"
        },
        heading: {
          type: "string",
          description: "Exact heading text of the todo (without TODO state prefix or tags)"
        }
      },
      required: ["file", "heading"]
    }
  }
].freeze

def elisp_string(str)
  escaped = str.gsub('\\', '\\\\\\\\').gsub('"', '\\"').gsub("\n", "\\n")
  "\"#{escaped}\""
end

def emacsclient_eval(expression)
  stdout, stderr, status = Open3.capture3("emacsclient", "--eval", expression)
  unless status.success?
    raise "emacsclient failed: #{stderr.strip}"
  end
  stdout.strip
end

def handle_tool_call(name, arguments)
  case name
  when "get_work_todos"
    emacsclient_eval("(kwrooijen/mcp-get-work-todos)")
  when "get_work_todo_files"
    emacsclient_eval("(kwrooijen/mcp-get-work-todo-files)")
  when "get_active_clocks"
    emacsclient_eval("(kwrooijen/mcp-get-active-clocks)")
  when "clock_in"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-clock-in #{elisp_string(file)} #{elisp_string(heading)})"
    )
  when "clock_out"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-clock-out #{elisp_string(file)} #{elisp_string(heading)})"
    )
  when "get_work_todo"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-get-work-todo #{elisp_string(file)} #{elisp_string(heading)})"
    )
  when "edit_work_todo"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading

    parts = [elisp_string(file), elisp_string(heading)]

    if arguments["state"]
      parts << ":state #{elisp_string(arguments["state"])}"
    end

    if arguments["set_properties"]
      pairs = arguments["set_properties"].map do |k, v|
        "(#{elisp_string(k)} . #{elisp_string(v.to_s)})"
      end
      parts << ":set-properties '(#{pairs.join(' ')})"
    end

    if arguments["delete_properties"]
      props = arguments["delete_properties"].map { |p| elisp_string(p) }
      parts << ":delete-properties '(#{props.join(' ')})"
    end

    if arguments["body"]
      parts << ":body #{elisp_string(arguments["body"])}"
    end

    emacsclient_eval("(kwrooijen/mcp-edit-work-todo #{parts.join(' ')})")

  when "create_work_todo"
    file = arguments["file"]
    heading = arguments["heading"]
    title = arguments["title"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    raise "Missing required parameter: title" unless title

    parts = [elisp_string(file), elisp_string(heading), elisp_string(title)]

    if arguments["state"]
      parts << ":state #{elisp_string(arguments["state"])}"
    end

    if arguments["body"]
      parts << ":body #{elisp_string(arguments["body"])}"
    end

    emacsclient_eval("(kwrooijen/mcp-create-work-todo #{parts.join(' ')})")

  when "edit_work_todo_body"
    file = arguments["file"]
    heading = arguments["heading"]
    body = arguments["body"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    raise "Missing required parameter: body" unless body
    emacsclient_eval(
      "(kwrooijen/mcp-edit-work-todo-body #{elisp_string(file)} #{elisp_string(heading)} #{elisp_string(body)})"
    )

  when "asana_push"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-asana-push #{elisp_string(file)} #{elisp_string(heading)})"
    )

  when "agent_set_branch"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading

    if arguments["branch"]
      emacsclient_eval(
        "(kwrooijen/mcp-agent-set-branch #{elisp_string(file)} #{elisp_string(heading)} #{elisp_string(arguments["branch"])})"
      )
    else
      emacsclient_eval(
        "(kwrooijen/mcp-agent-set-branch #{elisp_string(file)} #{elisp_string(heading)})"
      )
    end

  when "agent_launch"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-agent-launch #{elisp_string(file)} #{elisp_string(heading)})"
    )

  when "agent_list"
    emacsclient_eval("(kwrooijen/mcp-agent-list)")

  when "agent_get_prompt"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-agent-get-prompt #{elisp_string(file)} #{elisp_string(heading)})"
    )

  when "agent_get_transcript"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-agent-get-transcript #{elisp_string(file)} #{elisp_string(heading)})"
    )

  when "agent_get_diff"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-agent-get-diff #{elisp_string(file)} #{elisp_string(heading)})"
    )

  else
    raise "Unknown tool: #{name}"
  end
end

def respond(id, result)
  msg = { jsonrpc: "2.0", id: id, result: result }
  out = JSON.generate(msg)
  $stdout.write(out + "\n")
  $stdout.flush
end

def respond_error(id, code, message)
  msg = { jsonrpc: "2.0", id: id, error: { code: code, message: message } }
  out = JSON.generate(msg)
  $stdout.write(out + "\n")
  $stdout.flush
end

$stderr.puts("emacs-mcp-server: starting")

$stdin.each_line do |line|
  line = line.strip
  next if line.empty?

  begin
    request = JSON.parse(line)
  rescue JSON::ParserError => e
    $stderr.puts("emacs-mcp-server: parse error: #{e.message}")
    next
  end

  id = request["id"]
  method = request["method"]
  params = request["params"] || {}

  case method
  when "initialize"
    respond(id, {
      protocolVersion: "2024-11-05",
      capabilities: { tools: {} },
      serverInfo: { name: "emacs-mcp-server", version: "0.1.0" }
    })

  when "notifications/initialized"
    # No response needed for notifications
    nil

  when "tools/list"
    respond(id, { tools: TOOLS })

  when "tools/call"
    tool_name = params["name"]
    arguments = params["arguments"] || {}
    begin
      result = handle_tool_call(tool_name, arguments)
      respond(id, {
        content: [{ type: "text", text: result }]
      })
    rescue => e
      respond(id, {
        content: [{ type: "text", text: "Error: #{e.message}" }],
        isError: true
      })
    end

  else
    if id
      respond_error(id, -32601, "Method not found: #{method}")
    end
  end
end
