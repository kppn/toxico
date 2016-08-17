class Works::FilesController < ApplicationController
  before_action :set_work, only: [:create]
  before_action :logined_user

  rescue_from StandardError, with: :rescue_standard_error
  rescue_from Errno::ENOTDIR, with: :rescue_errno_enotdir_error

  # POST /works/121/files
  def create
    url = @work.code.publish_file
    render(json: {result: url, notice: ''}) and return
  end

  # Error handlers
  def rescue_standard_error
    return redirect_to request.referer if request.referer
    render(json: {result: '', notice: 'internal error'}) and return
  end

  def rescue_errno_enotdir_error
    return redirect_to request.referer if request.referer
    render(json: {result: '', notice: 'internal error'}) and return
  end 


  private

    # Use callbacks to share common setup or constraints between actions.
    def set_work
      @work = Work.find(params[:id])
    end

    def logined_user
      @user = current_user
    end
end
