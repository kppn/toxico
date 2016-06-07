class Work < ActiveRecord::Base
  include ApplicationHelper

  belongs_to :user

  scope :ruby,         ->{where language: 'ruby'}
  scope :c,            ->{where language: 'c'}
  scope :crystal,      ->{where language: 'crystal'}
  scope :go,           ->{where language: 'go'}
  scope :javascript,   ->{where language: 'javascript'}
  scope :coffeescript, ->{where language: 'coffeescript'}

  attr_reader :content
  attr_reader :input
  before_destroy :delete_file

  before_save :save_content_to_file
  after_find :read_content_from_file


  def self.generate(params, user)
    content, input = *params.extract!(:content, :input).values

    work = Work.new(params)

    work.user_id = user.id
    work.naming
    work.instance_variable_set(:@content, content)
    work.instance_variable_set(:@input,   input)
    work
  end


  def update_content(params)
    content, input = *params.extract!(:content, :input).values

    # コンテンツの修正ではカラムの変更がないため、touchしてupdated_atを更新
    self.touch

    self.instance_variable_set(:@content, content)
    self.instance_variable_set(:@input,   input)
    self
  end


  def naming
    if self.name.present?
      self.file = name
    else
      self.name = "unknown_#{make_basename}"
      self.file = make_basename
    end
    self
  end


  def thumbnail
    content.split("\n").first(3).map{|line|
      line.chomp!
       line = line.slice(0,60) + '...' if line.length >= 63
       line
    }.join("\n")
  end


  # ファイルを保存する
  def write_file(path = nil)
    raise StandardError.new if @content.blank?

    content = @content.encode(Encoding::UTF_8, :universal_newline => true)
    path ||= user_file_path

    File.open path, "wb" do |f|
      f.write content
    end
    path
  end


  def publish_file
    dir_path = "#{Rails.root.to_s}/#{public_user_dir}"
    Dir.mkdir(dir_path) unless Dir.exists?(dir_path)
    write_file "#{Rails.root.to_s}/#{public_user_file_path}"
    user_file_url
  end


  private

    def make_basename
      DateTime.now.strftime('%Y%m%d_%H%M%S')
    end

    def user_dir_base
      "#{Rails.root.to_s}/user_files"
    end

    # ユーザファイルのパスを返す
    def user_file_path
      file ? "#{user_dir_base}/#{user_id}/#{file}" : nil
    end

    # 
    def public_user_dir_base
      "public/user_files"
    end

    def public_user_dir
      "#{public_user_dir_base}/#{user_id}"
    end

    def public_user_file_path
      file ? "#{public_user_dir}/#{file}" : nil
    end

    def user_file_url
      file ? "/user_files/#{user_id}/#{file}" : nil
    end


  require 'json'

  protected

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

    # ファイルを削除する
    def delete_file
      begin
        File.delete(user_file_path)
      rescue SystemCallError => e
        # 何もしない。ファイルが消せなくてもユーザには関係ない
      end
    end
end

