class TopController < ApplicationController
  before_action :logined_user

  def index
  end

  private

    def logined_user
      @user = current_user
    end
end
