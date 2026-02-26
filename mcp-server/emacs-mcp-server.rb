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
