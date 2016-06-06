Signal.trap("EXIT") do
  FileUtils.rm_rf("#{Process.pid}.reply")
  FileUtils.rm_rf("#{Process.pid}.mailbox")
end
