class ApplicationController < ActionController::Base
  # Prevent CSRF attacks by raising an exception.
  # For APIs, you may want to use :null_session instead.
  protect_from_forgery with: :exception

  rescue_from ActionController::RoutingError, with: :render_404

  before_filter :configure_permitted_parameters, if: :devise_controller?

  helper_method :user_dir_path


  def render_404
    render template: 'errors/error_404', status: 404, layout: 'application', content_type: 'text/html'
  end

  protected
    def configure_permitted_parameters
      devise_parameter_sanitizer.permit(:sign_up) do |u|
        u.permit(:name, :email, :password, :password_confirmation)
      end
    end

    def user_dir_path
      "#{Rails.root.to_s}/user_files/#{current_user.id}"
    end

    def logined_user
      @user = current_user
    end
end
