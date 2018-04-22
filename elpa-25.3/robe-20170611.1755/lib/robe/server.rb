require 'json'
require 'tmpdir'
require 'socket'
require 'webrick'
require 'logger'

module Robe
  class Server
    attr_reader :running, :port

    def initialize(handler, port)
      @handler = handler
      @server = TCPServer.new("127.0.0.1", port)
      @running = true
      @port = @server.addr[1]
    end

    def start
      access = File.open("#{Dir.tmpdir}/robe-access-#{@port}.log", "w")
      access.sync = true

      error_logger = Logger.new($stderr)
      access_logger = Logger.new(access)

      client = nil

      loop do
        begin
          client = @server.accept

          next if client.eof?

          req = WEBrick::HTTPRequest.new(:InputBufferSize => 1024,
                                         :Logger => error_logger)
          req.parse(client)
          access_logger.info "#{req.request_method} #{req.path}"

          begin
            body = @handler.call(req.path, req.body)
          rescue Exception => e
            error_logger.error "Request failed: #{req.path}. Please file an issue."
            error_logger.error "#{e.message}\n#{e.backtrace.join("\n")}"
          end

          resp = WEBrick::HTTPResponse.new(:OutputBufferSize => 1024,
                                           :Logger => error_logger,
                                           :HTTPVersion => "1.1")
          resp.status = 200
          resp.content_type = "application/json; charset=utf-8"
          resp.body = body

          begin
            resp.send_response(client)
            client.close
          rescue Errno::EPIPE
            error_logger.error "Connection lost, unsent response:"
            error_logger.error body
          end
        rescue Errno::EINVAL
          break
        rescue IOError
          # Hello JRuby
          break
        end
      end
    end

    def wait_for_it
      begin
        TCPSocket.new("127.0.0.1", @port).close
      rescue
        sleep 0.05
        retry
      end
    end

    def shutdown
      @running = false
      begin
        @server && @server.shutdown(Socket::SHUT_RDWR)
      rescue Errno::ENOTCONN
        # Hello JRuby
        @server.close
      end
    end
  end
end
