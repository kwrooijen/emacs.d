#!/usr/bin/env ruby
# frozen_string_literal: true

# MCP (Model Context Protocol) stdio server that bridges Claude to Emacs.
# Receives JSON-RPC 2.0 tool calls on stdin, executes elisp via emacsclient,
# and returns results on stdout.

require "json"
require "open3"

TOOLS = JSON.parse(
  File.read(File.join(__dir__, "tools.json")),
  symbolize_names: true
).freeze

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

  when "edit_work_todo_ticket_description"
    file = arguments["file"]
    heading = arguments["heading"]
    body = arguments["body"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    raise "Missing required parameter: body" unless body
    emacsclient_eval(
      "(kwrooijen/mcp-edit-work-todo-ticket-description #{elisp_string(file)} #{elisp_string(heading)} #{elisp_string(body)})"
    )

  when "edit_work_todo_implementation"
    file = arguments["file"]
    heading = arguments["heading"]
    body = arguments["body"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    raise "Missing required parameter: body" unless body
    emacsclient_eval(
      "(kwrooijen/mcp-edit-work-todo-implementation #{elisp_string(file)} #{elisp_string(heading)} #{elisp_string(body)})"
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

  when "agent_get_tail"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading

    if arguments["n"]
      emacsclient_eval(
        "(kwrooijen/mcp-agent-get-tail #{elisp_string(file)} #{elisp_string(heading)} #{arguments["n"].to_i})"
      )
    else
      emacsclient_eval(
        "(kwrooijen/mcp-agent-get-tail #{elisp_string(file)} #{elisp_string(heading)})"
      )
    end

  when "agent_get_diff"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-agent-get-diff #{elisp_string(file)} #{elisp_string(heading)})"
    )

  when "agent_send_message"
    file = arguments["file"]
    heading = arguments["heading"]
    message = arguments["message"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    raise "Missing required parameter: message" unless message
    emacsclient_eval(
      "(kwrooijen/mcp-agent-send-message #{elisp_string(file)} #{elisp_string(heading)} #{elisp_string(message)})"
    )

  when "agent_resend"
    file = arguments["file"]
    heading = arguments["heading"]
    raise "Missing required parameter: file" unless file
    raise "Missing required parameter: heading" unless heading
    emacsclient_eval(
      "(kwrooijen/mcp-agent-resend #{elisp_string(file)} #{elisp_string(heading)})"
    )

  when "agent_notifications_list"
    emacsclient_eval("(kwrooijen/mcp-agent-notifications-list)")

  when "agent_notifications_pop"
    if arguments["all"]
      emacsclient_eval("(kwrooijen/mcp-agent-notifications-pop t)")
    else
      emacsclient_eval("(kwrooijen/mcp-agent-notifications-pop)")
    end

  when "agent_notifications_pause"
    emacsclient_eval("(kwrooijen/mcp-agent-notifications-pause)")

  when "agent_notifications_resume"
    emacsclient_eval("(kwrooijen/mcp-agent-notifications-resume)")

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
