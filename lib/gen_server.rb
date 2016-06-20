require "gen_server/version"
require "gen_server/signal_handler"

module GenServer

  attr_reader :pid

  def initialize(*args)
    init(*args)
    start_child_process
    parent_setup_mailbox
  end

  def init(*args)
  end

  def start_child_process
    @pid = Process.fork do
      @pid = Process.pid # Set our own pid.
      child_start_watchdog
      child_signal_handler
      child_setup_mailbox
      child_loop
    end
  end

  def call(*message)
    system("mkfifo #{reply_file_name}") unless File.exists?(reply_file_name)
    parent_write_message [:call, Process.pid, message]
    parent_read_reply
  end

  def cast(*message)
    parent_write_message [:cast, message]
    nil
  end

  def wait
    Process.wait(@pid)
  end

  def inspect
    "#PID<#{@pid}>"
  end

private

  def mailbox_file
    "#{@pid}.mailbox"
  end

  def reply_file_name
    "#{Process.pid}.reply"
  end

  def child_start_watchdog
    Thread.new do
      while true
        # If our parent pid magically becomes 1, then we know our
        # parent died... :(
        if Process.ppid == 1
          exit(1)
        end
        sleep(1)
      end
    end
  end

  def child_signal_handler
    load "gen_server/signal_handler.rb"
  end

  def child_setup_mailbox
    system("mkfifo #{@pid}.mailbox") or raise "cannot create fifo pipes"
    @mailbox = File.open(mailbox_file, "r")
  end

  def child_loop
    while true
      message = child_read_message
      case message[0]
      when :call
        _, from, message = message
        reply = handle_call(from, *message)
        child_write_reply(from, reply)
      when :cast
        _, message = message
        handle_cast(*message)
      else
        raise ArgumentError, "unexpected message type: #{type.inspect}"
      end
    end
  rescue EOFError
    retry
  end

  def child_read_message
    Marshal.load(@mailbox)
  rescue EOFError
    retry
  end

  def child_write_reply(reply_pid, reply)
    @reply_files ||= {}
    @reply_files[reply_pid] ||= File.open("#{reply_pid}.reply", "w+")
    reply_file = @reply_files[reply_pid]

    Marshal.dump(reply, reply_file)
    reply_file.flush
  end

  def parent_setup_mailbox
    retry_count = 0
    while !File.exists?(mailbox_file)
      raise "cannot find named pipes" if retry_count > 10
      sleep(0.05)
      retry_count += 1
    end

    @mailbox = File.open(mailbox_file, "w+")
  end

  def parent_write_message(message)
    Marshal.dump(message, @mailbox)
    @mailbox.flush
  end

  def parent_read_reply

    # Pipe semantics are such that opening a pipe on one end will block until
    # something opens it on the other end. Thus we have to fire off a message
    # that will be replied to *before* we try to open the pipe.
    # This will NOT work:
    #   open_pipe
    #   send_msg
    #   recv_msg
    # This will work:
    #   send_msg
    #   open_pipe
    #   recv_msg

    @reply_file ||= File.open(reply_file_name, "r")
    Marshal.load(@reply_file)
  end

  def marshal_dump
    @pid
  end

  def marshal_load(pid)
    @pid = pid
    parent_setup_mailbox
  end

end
