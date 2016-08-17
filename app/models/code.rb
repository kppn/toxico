
class Code < ActiveRecord::Base
  belongs_to :work
  # accepts_nested_attributes_for :work

  include ApplicationHelper

  attr_reader :content
  attr_reader :input

  before_destroy :delete_file
  before_save :save_content_to_file
  after_find :read_content_from_file



  def initialize(params = {})
    if params.present?
      @content, @input = *params.extract!(:content, :input).values
    end
    super(params)
  end


  # ファイルへ保存する
  def write_file(path = user_file_path)
    raise StandardError.new if content.blank?

    File.write(path, content.encode(Encoding::UTF_8, :universal_newline => true))
    path
  end


  # コンテンツ(コード・インプット)を更新する
  def update_content(params)
    @content, @input = *params.extract!(:content, :input).values
    # Railsにモデルの更新を検知させるため、同じ内容で上書き
    self.instance_variable_set :@changed_attributes, {'file' => file}
    self
  end


  # ファイルをダウンロード用ディレクトリに置く
  def publish_file
    dir_path = "#{Rails.root.to_s}/#{public_user_dir}"
    Dir.mkdir(dir_path) unless Dir.exists?(dir_path)
    write_file "#{Rails.root.to_s}/#{public_user_file_path}"
    user_file_url
  end


  def naming(name)
    self.file = name
  end


  protected

    require 'json'

    def save_content_to_file
      content_enc = content.encode(Encoding::UTF_8, :universal_newline => true)
      content_json = { 'input' => input, 'content' => content_enc }.to_json

      File.write(user_file_path, content_json)
      self
    end

    def read_content_from_file
      content_json = JSON.parse(File.read(user_file_path))
      @content = content_json['content']
      @input   = content_json['input']
      self
    end

    def delete_file
      begin
        File.delete(user_file_path)
      rescue SystemCallError => e
        # 何もしない。ファイルが消せなくてもユーザには関係ない
      end
    end


  private

    def user_dir_base
      "#{Rails.root.to_s}/user_files"
    end

    # ユーザファイルのパスを返す
    def user_file_path
      file ? "#{user_dir_base}/#{work.user_id}/#{file}" : nil
    end

    # 
    def public_user_dir_base
      "public/user_files"
    end

    def public_user_dir
      "#{public_user_dir_base}/#{work.user_id}"
    end

    def public_user_file_path
      file ? "#{public_user_dir}/#{file}" : nil
    end

    def user_file_url
      file ? "/user_files/#{work.user_id}/#{file}" : nil
    end
end

