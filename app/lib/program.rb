require 'json'
require 'open3'

# Class Program, that store and execute abstruct program.
#
# EXAMPLE
#
#   language = 'ruby'
#   code = 'puts hello #{ARGV[0]}"'
#   input = 'takashi'
#
#   program = Program.new(language, code, input)
#   out = program.execute
#
#   if program.success?
#     puts "conguraturations. #{out}"       #=> "conguraturations. hello takashi"
#   else
#     puts "so sad! invalid result #{out}"
#   end
#
# Supported language
#   * ruby
#   * c
#   * crystal
#
# @author Takashi Kondoh (tkondoh58@gmail.com)
# @copyright copyleft
# @license MIT Licence
#
class Program
  # @return [String] language
  attr_reader :language
  # @return [String] program code.
  attr_reader :code
  # @return [String] stdin string for this program
  attr_reader :input

  # @return [String] result(valid)
  attr_reader :out
  # @return [String] result(invalid)
  attr_reader :err
  # @return [String] result(stat)
  attr_reader :stat

  # New Program.
  #
  # @param [Object] program Hash or Array.
  #   Hash: Must include :code. ex) {code: 'puts "hello #{ARGV[0]}"', input: 'takashi'}
  #   Array: recognize [code(Must), input(optional)] .  ex) ['puts hello "#{ARGV[0]}"']  # without
  #
  # @return self
  def initialize(language, code, input = nil)
    @language = language
    @code = code
    @input = input
    @out = @err = @stat = @sucess = nil
    self
  end

  # Execute this program
  # @param [String] language language ex) ruby
  # @param [String] file_path path to program file.
  # @return [String] result. if error happened, the result has the error message.
  def execute(file_path = nil)
    language_type = get_type(language)
    file_path ||= make_tempfile(code, ".#{language_type[:extension]}")

    if language_type[:type] == :script
      cmd = make_script_cmd(language_type, file_path)
      @out, @err, @stat = execute_cmd(cmd, input)
    else
      @out, @err, @stat, cmd = compile(file_path, language_type)
      if stat != 0
        @success = false
        return @out || @err
      end
      @out, @err, @stat = execute_cmd(cmd, input)
    end
    @success = (stat == 0 ? true : false)
    @out || @err
  end

  # success? this program. before execution, success? return nil
  # @return [TrueClass|FalseClass|NilClass] result of this program executed
  def success?
    @success
  end

  # return json formatted
  # @return [JSON] json formatted.
  def to_json
    JSON.pretty_generate input: input, code: code
  end


  private

  def make_script_cmd(language_type, file_path)
    "#{language_type[:executer]} #{file_path}"
  end

  # make tempfile
  # @param [String] content content to write
  # @param [String] suffix suffix of temp file
  # @return [String] path to tempfile
  def make_tempfile(content, suffix = nil)
    file = Tempfile.open(['program', suffix]) do |f|
      f.write content
      f
    end
    file.path
  end

  # compile program.
  # @param [String] file_path path to program file.
  # @param [String] path to executable file.
  # @param [Hash] compile_info information to compile for this language.
  # @param [String] out_file_path path to output executable file. if omitted, same path without extention.
  # @return [String, String] compile result stdout, stderr.
  def compile(file_path, language_type, out_file_path = nil)
    unless out_file_path
      out_file_path = "#{File.dirname(file_path)}/#{File.basename(file_path, '.*')}"
    end
    options = "#{language_type[:opt_out_file]} #{out_file_path}"
    cmd = "#{language_type[:compiler]} #{options} #{file_path}"
    [*execute_cmd(cmd), out_file_path]
  end

  # transform type of script or compile
  # @param [String] language ex) 'ruby'
  # @return [Hash] language information hash
  def get_type(language)
    types = {
      ruby:    {type: :script,  executer: 'ruby',                         extension: 'rb'},
      c:       {type: :compile, compiler: 'gcc',      opt_out_file: '-o', extension: 'c'},
      crystal: {type: :script,  executer: 'crystal',                      extension: 'cr'}
    }
    types[language.to_sym]
  end

  # execute command.
  # @param [String] cmd command to execute.
  # @return [String, String] compile result stdout, stderr.
  def execute_cmd(cmd, stdin_data = nil)
    out = err = stat = nil
    Open3.popen3(cmd) do |stdin, stdout, stderr, thr|
      stdin.print stdin_data if stdin_data
      stdin.close
      out = stdout.read
      err = stderr.read
      stat = thr.value
    end
    stat == 0 ? err = nil : out = nil
    [out, err, stat]
  end
end


