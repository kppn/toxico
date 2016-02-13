class Work < ActiveRecord::Base
  include ApplicationHelper

  belongs_to :user

  scope :ruby, ->{where language: 'ruby'}
  scope :c,    ->{where language: 'c'}

  def naming(n)
    if n.present?
      self.name = n
      self.file = n + '.rb'
    else
      self.name = 'unknown_' + make_base_name
      self.file = make_filename
    end
    self
  end

  def naming_file
    self.file = make_filename
  end

  def thumbnail
    if content = read_file
      content.each_line.first(3).map{|line|
        line.chomp!
        line = line.slice(0,60) + '...' if line.length >= 63
        line
      }.join("\n")
    else
      nil
    end
  end


  # ユーザファイルのパスを返す
  def user_file_path
    file ? "#{user_dir_base}/#{user_id}/#{file}" : nil
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

  # ファイルを削除する
  def delete_file
    begin
      File.delete(user_file_path)
    rescue SystemCallError => e
      # 何もしない。ファイルが消せなくてもユーザには関係ない
    end
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

    def make_base_name
      DateTime.now.strftime('%Y%m%d_%H%M%S')
    end

    def make_filename
      make_base_name + ".rb"
    end
end
