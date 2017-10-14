# swank.rb --- swank server for Ruby.
#
# This is my first Ruby program and looks probably rather strange.  Some
# people write Scheme interpreters when learning new languages, I
# write swank backends.  
#
# Only a few things work.  
# 1. Start the server with something like: ruby -r swank -e swank
# 2. Use M-x slime-connect to establish a connection

require "socket"

def swank(port=4005) 
  accept_connections port, false
end

def start_swank(port_file)
  accept_connections false, port_file
end

def accept_connections(port, port_file)
  server = TCPServer.new("localhost", port || 0)
  puts "Listening on #{server.addr.inspect}\n"
  if port_file
    write_port_file server.addr[1], port_file
  end
  socket = begin server.accept ensure server.close end
  begin
    serve socket.to_io
  ensure
    socket.close
  end
end

def write_port_file(port, filename)
  File.open(filename, File::CREAT|File::EXCL|File::WRONLY) do |f|
    f.puts port 
  end
end

def serve(io)
  main_loop(io)
end

def main_loop(io)
  c = Connection.new(io)
  while true
    catch :swank_top_level do
      c.dispatch(read_packet(io))
    end
  end
end

class Connection
  
  def initialize(io)
    @io = io
  end

  def dispatch(event)
    puts "dispatch: %s\n" % event.inspect
    case event[0]
    when :":emacs-rex"
      emacs_rex *event[1..4]
    else raise "Unhandled event: #{event.inspect}"
    end
  end

  def send_to_emacs(obj)
    payload = write_sexp_to_string(obj)
    @io.write("%06x" % payload.length)
    @io.write payload
    @io.flush
  end

  def emacs_rex(form, pkg, thread, id)
    proc = $rpc_entries[form[0]]
    args = form[1..-1];
    begin
      raise "Undefined function: #{form[0]}" unless proc
      value = proc[*args]
    rescue Exception => exc
      begin
        pseudo_debug exc
      ensure
        send_to_emacs [:":return", [:":abort"], id]
      end
    else
      send_to_emacs [:":return", [:":ok", value], id]
    end
  end

  def pseudo_debug(exc)
    level = 1
    send_to_emacs [:":debug", 0, level] + sldb_info(exc, 0, 20)
    begin
      sldb_loop exc
    ensure
      send_to_emacs [:":debug-return", 0, level, :nil]
    end
  end

  def sldb_loop(exc)
    $sldb_context = [self,exc]
    while true
      dispatch(read_packet(@io))
    end
  end

  def sldb_info(exc, start, _end)
    [[exc.to_s,
      "  [%s]" % exc.class.name,
      :nil],
     sldb_restarts(exc),
     sldb_backtrace(exc, start, _end),
     []]
  end

  def sldb_restarts(exc) 
    [["Quit", "SLIME top-level."]]
  end

  def sldb_backtrace(exc, start, _end)
    bt = []
    exc.backtrace[start.._end].each_with_index do |frame, i|
      bt << [i, frame]
    end
    bt
  end

  def frame_src_loc(exc, frame)
    string = exc.backtrace[frame]
    match = /([^:]+):([0-9]+)/.match(string)
    if match
      file,line = match[1..2]
      [:":location", [:":file", file], [:":line", line.to_i], :nil]
    else
      [:":error", "no src-loc for frame: #{string}"]
    end
  end
  
end

$rpc_entries = Hash.new

$rpc_entries[:"swank:connection-info"] = lambda do ||
    [:":pid", $$,
     :":package", [:":name", "ruby", :":prompt", "ruby> "],
     :":lisp-implementation", [:":type", "Ruby",
                               :":name", "ruby",
                               :":version", RUBY_VERSION]]
end

def swank_interactive_eval(string)
  eval(string,TOPLEVEL_BINDING).inspect
end

$rpc_entries[:"swank:interactive-eval"] = \
$rpc_entries[:"swank:interactive-eval-region"] = \
$rpc_entries[:"swank:pprint-eval"] = lambda { |string|
  swank_interactive_eval string
}

$rpc_entries[:"swank:throw-to-toplevel"] = lambda { 
  throw :swank_top_level
}

$rpc_entries[:"swank:backtrace"] = lambda do |from, to|
  conn, exc = $sldb_context
  conn.sldb_backtrace(exc, from, to)
end

$rpc_entries[:"swank:frame-source-location"] = lambda do |frame|
  conn, exc = $sldb_context
  conn.frame_src_loc(exc, frame)
end

#ignored
$rpc_entries[:"swank:buffer-first-change"] = \
$rpc_entries[:"swank:operator-arglist"] = lambda do 
  :nil
end

$rpc_entries[:"swank:simple-completions"] = lambda do |prefix, pkg|
  swank_simple_completions prefix, pkg
end

# def swank_simple_completions(prefix, pkg)

def read_packet(io)
  header = read_chunk(io, 6)
  len = header.hex
  payload = read_chunk(io, len)
  #$deferr.puts payload.inspect
  read_sexp_from_string(payload)
end

def read_chunk(io, len)
  buffer = io.read(len)
  raise "short read" if buffer.length != len
  buffer
end

def write_sexp_to_string(obj)
  string = ""
  write_sexp_to_string_loop obj, string
  string
end

def write_sexp_to_string_loop(obj, string)
  if obj.is_a? String
    string << "\""
    string << obj.gsub(/(["\\])/,'\\\\\1')
    string << "\""
  elsif obj.is_a? Array
    string << "("
    max = obj.length-1
    obj.each_with_index do |e,i|
      write_sexp_to_string_loop e, string
      string << " " unless i == max
    end
    string << ")"
  elsif obj.is_a? Symbol or obj.is_a? Numeric
    string << obj.to_s
  elsif obj == false
    string << "nil"
  elsif obj == true
    string << "t"
  else raise "Can't write: #{obj.inspect}"
  end
end

def read_sexp_from_string(string)
  stream = StringInputStream.new(string)
  reader = LispReader.new(stream)
  reader.read
end

class LispReader
  def initialize(io)
    @io = io
  end
  
  def read(allow_consing_dot=false)
    skip_whitespace
    c = @io.getc
    case c
    when ?( then read_list(true)
    when ?" then read_string
    when ?' then read_quote
    when nil then raise EOFError.new("EOF during read")
    else
      @io.ungetc(c)
      obj = read_number_or_symbol
      if obj == :"." and not allow_consing_dot
        raise "Consing-dot in invalid context"
      end
      obj
    end
  end

  def read_list(head)
    list = []
    loop do
      skip_whitespace
      c = @io.readchar
      if c == ?) 
        break
      else
        @io.ungetc(c)
        obj = read(!head)
        if obj == :"."
          error "Consing-dot not implemented" # would need real conses
        end
        head = false
        list << obj
      end
    end
    list
  end

  def read_string
    string = ""
    loop do 
      c = @io.getc
      case c 
      when ?" 
        break 
      when ?\\ 
        c = @io.getc
        case c
        when ?\\, ?" then string << c
        else raise "Invalid escape char: \\%c" % c
        end
      else
        string << c
      end
    end
    string
  end

  def read_quote
    [:quote, read]
  end

  def read_number_or_symbol
    token = read_token
    if token.empty?
      raise EOFError.new
    elsif /^[0-9]+$/.match(token)
      token.to_i
    elsif /^[0-9]+\.[0-9]+$/.match(token)
      token.to_f
    else
      token.intern
    end
  end

  def read_token
    token = ""
    loop do
      c = @io.getc
      if c.nil? 
        break
      elsif terminating?(c)
        @io.ungetc(c)
        break
      else
        token << c
      end
    end
    token
  end

  def skip_whitespace
    loop do
      c = @io.getc
      case c 
      when ?\s, ?\n, ?\t then next
      when nil then break
      else @io.ungetc(c); break
      end
    end
  end
  
  def terminating?(char) 
    " \n\t()\"'".include?(char)
  end

end


class StringInputStream
  def initialize(string)
    @string = string
    @pos = 0
    @max = string.length
  end

  def pos() @pos end
  
  def getc
    if @pos == @max
      nil
    else
      c = @string[@pos]
      @pos += 1
      c
    end
  end
  
  def readchar
    getc or raise EOFError.new
  end

  def ungetc(c)
    if @pos > 0 && @string[@pos-1] == c 
      @pos -= 1
    else
      raise "Invalid argument: %c [at %d]" % [c, @pos]
    end
  end
  
end

