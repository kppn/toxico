class DeviseCustom::RegistrationsController < Devise::RegistrationsController
  def create 
    tmp = super

    create_user_file_directory
    if user = current_user
      user.admin = false
      user.save
    end
    tmp
  end

  private
    
    def create_user_file_directory
      Dir.mkdir user_dir_path
    end
end

