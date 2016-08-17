require 'spec_helper'
require 'rails_helper'

RSpec.describe 'Program' do
  describe 'new object' do
    it 'simple new' do
      expect{ Program.new('ruby', 'print :hello') }.not_to raise_error
      expect{ Program.new('ruby', 'print :hello', '123') }.not_to raise_error
    end

    it 'valid arguments?' do
      expect{ Program.new()                                       }.to raise_error ArgumentError
      expect{ Program.new('ruby')                                 }.to raise_error ArgumentError
      expect{ Program.new('ruby', 'print :hello', '123', 'dummy') }.to raise_error ArgumentError
    end
  end

  describe 'access property' do
    before do 
      @program = Program.new('ruby', 'print :hello', '123')
    end

    it 'valid' do
      expect( @program.language ).to eql 'ruby'
      expect( @program.code     ).to eql 'print :hello'
      expect( @program.input    ).to eql '123'
    end

    it 'invalid' do
      expect{ @program.hoge }.to raise_error( NoMethodError )
    end
  end

  describe 'tempfile' do
    before do 
      @program = Program.new('ruby', 'print :hello', '123')
      @code = @program.code
    end

    it 'make tempfile' do
      path = @program.send(:make_tempfile, @code)
      expect( path ).to match /program/
      expect( File.exists?(path) ).to be true
    end

    it 'readable tempfile' do
      path = @program.send(:make_tempfile, @code)
      File.open(path) do |f|
        expect( f.read ).to eql 'print :hello'
      end
    end
  end

  describe 'make_script_cmd' do
    it 'ruby' do
      program = Program.new('ruby', 'print :hello')
      path = program.send(:make_tempfile, program.code)
      type = program.send(:get_type, program.language)

      cmd = program.send(:make_script_cmd, type, path)
      expect( cmd ).to match %r(ruby +/tmp/[/a-zA-Z0-9_-]+)
    end

    it 'c' do
      program = Program.new('c', 'main(){printf("hello");}')
      path = program.send(:make_tempfile, program.code)
      type = program.send(:get_type, program.language)

      cmd = program.send(:make_script_cmd, type, path)
      expect( cmd ).to match %r(/tmp/[/a-zA-Z0-9_-]+)
    end
  end

  describe 'execute' do
    it 'ruby valid without stdin' do
      program = Program.new('ruby', 'print "hello"')
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( out ).to eql 'hello'
      expect( program.success? ).to be true
    end

    it 'ruby valid with stdin' do
      program = Program.new('ruby', 'print $stdin.readline', '123')
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( out ).to eql '123'
      expect( program.success? ).to be true
    end

    it 'ruby invalid without stdin' do
      program = Program.new('ruby', 'print $stdin.hoge')
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( out ).to match /undefined method/
      expect( program.success? ).to be false
    end

    it 'ruby invalid with stdin' do
      program = Program.new('ruby', 'print $stdin.readline + 1', '123')
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( out ).to match /no implicit conversion/
      expect( program.success? ).to be false
    end

    it 'c valid without stdin' do
      code = <<-EOL
        #include <stdio.h>
        int main(){
          printf("hello");
          return 0;
        }
      EOL

      program = Program.new('c', code)
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( out ).to eql 'hello'
      expect( program.success? ).to be true
    end

    it 'c valid with stdin' do
      code = <<-EOL
        #include <stdio.h>
        int main(){
          char buf[8];
          fgets(buf, 7, stdin);
          printf("%s", buf);
          return 0;
        }
      EOL

      program = Program.new('c', code, '123')
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( out ).to eql '123'
      expect( program.success? ).to be true
    end

    it 'c invalid compile error(semicolon missing)' do
      code = <<-EOL
        #include <stdio.h>
        int main(){
          printf("hello")
          return 0
        }
      EOL

      program = Program.new('c', code)
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( out ).to match /main/
      expect( program.success? ).to be false
    end

    it 'c invalid runtime error(divide by zero)' do
      code = <<-EOL
        #include <stdio.h>
        int main(){
          int i = 0;
          printf("%d", 1/i);
          return 0;
        }
      EOL

      program = Program.new('c', code)
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( program.success? ).to be false
    end

    it 'crystal valid without stdin' do
      program = Program.new('crystal', 'print "hello"')
      out = nil
      expect{ out = program.execute }.not_to raise_error
      expect( out ).to eql 'hello'
      expect( program.success? ).to be true
    end
  end

  describe 'ext' do
    it 'ruby extension' do
      program = Program.new('ruby', 'print :hello')
      expect( program.ext).to eql 'rb'
    end

    it 'c extension' do
      code = <<-EOL
        #include <stdio.h>
        int main(){
          printf("hello")
          return 0
        }
      EOL
      program = Program.new('c', code)
      expect( program.ext).to eql 'c'
    end

    it 'crystal extension' do
      program = Program.new('crystal', 'print "hello"')
      expect( program.ext).to eql 'cr'
    end
  end
end

