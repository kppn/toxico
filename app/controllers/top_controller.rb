class TopController < ApplicationController
  before_action :logined_user

  def index
    @params = params.permit(:hoge, :url)
    logger.debug "params"
    @params.to_h.each do |k, v|
      logger.debug "#{k} #{v}"
    end
  end

  private

    def logined_user
      @user = current_user
    end
end
