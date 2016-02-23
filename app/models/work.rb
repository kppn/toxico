class Work < ActiveRecord::Base
  include ApplicationHelper

  belongs_to :user

  scope :ruby,    ->{where language: 'ruby'}
  scope :c,       ->{where language: 'c'}
  scope :crystal, ->{where language: 'crystal'}
  scope :go,      ->{where language: 'go'}

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
  def write_file(content)
    raise StandardError.new if content.blank?

    content = content.encode(Encoding::UTF_8, :universal_newline => true)

    File.open user_file_path, "wb" do |f|
      f.write content
    end
    user_file_path 
  end


  # ファイルから読み込む
  def read_file
    content = nil
    begin
      content = File.open(user_file_path).read 
    rescue SystemCallError => e
      # 何もしない。ファイルが開けない/読み込めない場合はnil
    end
    content
  end


  private

    def make_basename
      DateTime.now.strftime('%Y%m%d_%H%M%S')
    end


    def merge_input(content, input)
      lines = input.lines.map {|line| '# ' + line}
      lines.unshift "#==user_input==\n"
      lines.push    "\n#==user_input_end==\n"
      lines.join + content
    end


    def split_input(content)
      return ['', content] unless content =~ /\A#==user_input==/
      lines = content.lines[1..-1]

      n = lines.find_index {|l| l =~ /\A#==user_input_end==/}

      input_lines = lines[0..n-1].map {|l| l.sub /\A# /, ''}
      input_lines[-1].sub! "\n", ''

      input   = input_lines.join
      content = lines[n+1..-1].join

      [input, content]
    end


    def user_dir_base
      "#{Rails.root.to_s}/user_files"
    end

    # ユーザファイルのパスを返す
    def user_file_path
      file ? "#{user_dir_base}/#{user_id}/#{file}" : nil
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

