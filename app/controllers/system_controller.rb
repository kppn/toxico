class SystemController < ApplicationController
  def notify_global_ip 
    NotifyMailer.notify_global_ip.deliver_now
    render :layout => false
  end
end
