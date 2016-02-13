class NotifyMailer < ApplicationMailer
  default from: 'tkondoh58@gmail.com'

  def notify_global_ip
    @global_ip = `curl inet-ip.info`
    mail to: 'tkondoh58@gmail.com', subject: 'Global IP Notification'
  end
end
