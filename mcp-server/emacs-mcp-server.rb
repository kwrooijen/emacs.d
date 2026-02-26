#!/usr/bin/env ruby
# frozen_string_literal: true

# MCP (Model Context Protocol) stdio server that bridges Claude to Emacs.
# Receives JSON-RPC 2.0 tool calls on stdin, executes elisp via emacsclient,
# and returns results on stdout.

require "json"
require "open3"

TOOLS = [
  {
    name: "emacs_eval",
    description: "Evaluate an Emacs Lisp expression and return the result. " \
                 "Use this to query or manipulate Emacs state.",
    inputSchema: {
      type: "object",
      properties: {
        expression: {
          type: "string",
          description: "Emacs Lisp expression to evaluate"
        }
      },
      required: ["expression"]
    }
  },
  {
    name: "get_work_todos",
    description: "Get all org-mode TODO items tagged :work: with states TODO, IN PROGRESS, or WAITING. " \
                 "Returns a JSON array of objects with heading, state, tags, and file fields.",
    inputSchema: {
      type: "object",
      properties: {}
    }
  }
].freeze

def emacsclient_eval(expression)
  stdout, stderr, status = Open3.capture3("emacsclient", "--eval", expression)
  unless status.success?
    raise "emacsclient failed: #{stderr.strip}"
  end
  stdout.strip
end

def handle_tool_call(name, arguments)
  case name
  when "emacs_eval"
    expression = arguments["expression"]
    raise "Missing required parameter: expression" unless expression
    emacsclient_eval(expression)
  when "get_work_todos"
    emacsclient_eval("(kwrooijen/mcp-get-work-todos)")
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
