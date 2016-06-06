require "gen_server/version"

module GenServer

  def initialize(*args)
    init(*args)
    @pid = start_child_process
    parent_open_pipes
  end

  def init(*args)
  end

  def start_child_process
    Process.fork do
      @pid = Process.pid # Set our own pid.
      child_start_watchdog
      child_signal_handler
      child_make_pipes
      child_open_pipes
      child_loop
    end
  end

  def call(message)
    parent_write_message [:call, message]
    parent_read_reply
  end

  def cast(message)
    parent_write_message [:cast, message]
  end

  def wait
    Process.wait(@pid)
  end

private

  def mailbox_file
    "#{@pid}.mailbox"
  end

  def reply_file
    "#{@pid}.reply"
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
    Signal.trap("EXIT") do
      FileUtils.rm_rf(mailbox_file)
      FileUtils.rm_rf(reply_file)
    end
  end

  def child_make_pipes
    system("mkfifo #{@pid}.mailbox; mkfifo #{@pid}.reply") or raise "cannot create fifo pipes"
  end

  def child_open_pipes
    @mailbox = File.open(mailbox_file, "r")
    @reply = File.open(reply_file, "w+")
  end

  def child_loop
    while true
      type, message = child_read_message
      case type
      when :call
        reply = handle_call(message)
        child_write_reply(reply)
      when :cast
        handle_cast(message)
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

  def child_write_reply(reply)
    Marshal.dump(reply, @reply)
    @reply.flush
  end

  def parent_open_pipes
    retry_count = 0
    while !File.exists?(mailbox_file) || !File.exists?(reply_file)
      raise "cannot find named pipes" if retry_count > 5
      sleep(0.01)
      retry_count += 1
    end

    @mailbox = File.open(mailbox_file, "w+")
    @reply = File.open(reply_file, "r")
  end

  def parent_write_message(message)
    Marshal.dump(message, @mailbox)
    @mailbox.flush
  end

  def parent_read_reply
    Marshal.load(@reply)
  end

  def marshal_dump
    @pid
  end

  def marshal_load(pid)
    @pid = pid
    parent_open_pipes
  end

end
