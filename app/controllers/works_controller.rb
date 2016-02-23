class WorksController < ApplicationController
  before_action :set_work, only: [:show, :edit, :update, :destroy]
  before_action :logined_user

  rescue_from StandardError, with: :rescue_standard_error
  rescue_from ContentError, with: :rescue_content_error


  # GET /works
  # GET /works.json
  # def index
  #   @works = Work.all
  # end


  # GET /works/1
  # GET /works/1.json
  def show
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
    @work = Work.generate(work_params, @user)

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
    @work.update_content(work_params)

    if @work.save!
      render(json: {result: '', notice: ''})            and return
    else
      render(json: {result: '', notice: 'save error'})  and return
    end
  end


  # DELETE /works/1
  # DELETE /works/1.json
  def destroy
    @work.destroy
    return redirect_to request.referer if request.referer
    respond_to do |format|
      format.html { redirect_to works_url, notice: 'Work was successfully destroyed.' }
      format.json { head :no_content }
    end
  end


  # POST /works/execute
  def execute
    params = work_params.slice(:language, :content, :input).values
    result = Program.new(*params).execute
    render json: {result: result, notice: ''}
  end


  # Error handlers
  def rescue_standard_error
    return redirect_to request.referer if request.referer
    render(json: {result: '', notice: 'internal error'}) and return
  end

  def rescue_content_error
  end 


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
