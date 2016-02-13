class WorksController < ApplicationController
  before_action :set_work, only: [:show, :edit, :update, :destroy]
  before_action :logined_user


  # GET /works
  # GET /works.json
  # def index
  #   @works = Work.all
  # end


  # GET /works/1
  # GET /works/1.json
  def show
    content = @work.read_file
    flash[:input], flash[:content] = split_input content
  end


  # GET /works/new
  def new
    @work = Work.new
  end


  # GET /works/1/edit
  # def edit
  # end
  

  # POST /works
  # POST /works.json
  def create
    @work = Work.new

    @work.user_id = @user.id
    @work.language = work_params[:language].present? ? work_params[:language] : 'unknown'
    @work.naming work_params[:name]

    content = work_params[:content]
    flash[:content] = content
    path = ''
    begin
      path = @work.write_file content
    rescue StandardError => e
      flash[:notice] = 'content empty'
      return redirect_to :action => 'new'
    rescue SystemCallError => e
      flash[:notice] = 'internal error'
      return redirect_to :action => 'new'
    end

    respond_to do |format|
      if @work.save
        format.html { redirect_to @work, notice: 'Work was successfully created.' }
        format.json { render :show, status: :created, location: @work }
      else
        format.html { render :new }
        format.json { render json: @work.errors, status: :unprocessable_entity }
      end
    end
  end


  # PATCH/PUT /works/1
  # PATCH/PUT /works/1.json
  def update
    @work.language = work_params[:language].present? ? work_params[:language] : 'unknown'

    content = work_params[:content]
    input   = work_params[:input]
    flash[:content] = content
    path = ''
    begin
      path = @work.write_file merge_input(content, input)
    rescue StandardError => e
      flash[:notice] = 'content empty'
      return redirect_to :action => 'new'
    rescue SystemCallError => e
      flash[:notice] = 'internal error'
      return redirect_to :action => 'new'
    end

    # コンテンツのみ修正ではカラムの変更がないため、touchしてupdated_atを更新
    @work.touch

    if @work.save!
      render(json: {result: '', notice: ''})            and return
    else
      render(json: {result: '', notice: 'save error'})  and return
    end
    #respond_to do |format|
    #  if @work.save!
    #    format.html { render :show, status: :ok, location: @work }
    #    format.json { render :show, status: :ok, location: @work }
    #  else
    #    format.html { render :edit }
    #    format.json { render json: @work.errors, status: :unprocessable_entity }
    #  end
    #end
  end


  # DELETE /works/1
  # DELETE /works/1.json
  def destroy
    @work.delete_file
    @work.destroy
    return redirect_to request.referer if request.referer
    respond_to do |format|
      format.html { redirect_to works_url, notice: 'Work was successfully destroyed.' }
      format.json { head :no_content }
    end
  end


  # POST /works/execute
  def execute
    content = work_params[:content]
    work = Work.new
    work.file = 'temporary_work'
    work.user_id = @user.id

    begin
      path = work.write_file content
    rescue StandardError => e
      render(json: {result: '', notice: 'content empty'}) and return
    rescue SystemCallError => e
      render(json: {result: '', notice: 'internal error'}) and return
    end

    input = work_params[:input].present? ? work_params[:input] : nil
    out, err = exec_work(work.user_file_path, input)
    result = out || err.sub(/.*?:/, '')  # exclude file path from ruby error message

    #render text: result
    render json: {result: result, notice: ''}
  end


  require 'open3'

  private

    # Use callbacks to share common setup or constraints between actions.
    def set_work
      @work = Work.find(params[:id])
    end

    def logined_user
      @user = current_user
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def work_params
      params.require(:work).permit(:name, :content, :language, :input)
    end
    
    # ユーザファイルのパスを返す
    def user_file_path(file)
      file ? "#{user_dir_path}/#{file}" : nil
    end


    # ファイルから読み込む
    def read_file(file_path)
      content = nil
      begin
        content = File.open(file_path).read 
      rescue SystemCallError => e
        puts 'Illegal'
        # 何もしない。ファイルが開けない/読み込めない場合はnil
      end
    end

    # workを実行する
    def exec_work(file, stdin_data = nil)
      out = err = stat = nil
      Open3.popen3("ruby #{file}") do |stdin, stdout, stderr, thr|
          stdin.puts stdin_data if stdin_data
          stdin.close
          out = stdout.read
          err = stderr.read
          stat = thr.value
      end
      out = nil if stat != 0
      [out, err]
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
end
