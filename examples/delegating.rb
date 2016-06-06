require "gen_server"

class EchoServer
  include GenServer

  def init
    @state = "!"
  end

  def handle_cast(message)
    puts "#{message}#{@state}"
  end

  def handle_call(message)
    "#{message}#{@state}"
  end

end

class DelegatingServer
  include GenServer

  def handle_cast(message)
    @server = message
  end

  def handle_call(message)
    @server.call("#{message}!")
  end

end

echo_server = EchoServer.new
delegating_server = DelegatingServer.new

echo_server.cast("hi")
echo_server.cast("bye?")
puts echo_server.call("byyyeeeee")

delegating_server.cast(echo_server)

puts delegating_server.call("do it work?")
echo_server.cast("it do")
puts delegating_server.call("ok, done")
