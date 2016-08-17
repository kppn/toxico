class WorksController < ApplicationController
  before_action :set_work, only: [:show, :edit, :update, :destroy]
  before_action :logined_user

  rescue_from StandardError, with: :rescue_standard_error
  rescue_from ContentError, with: :rescue_content_error


  # GET /works
  # GET /works.json
  def index
    @works = WorkWithContent.all
  end


  # GET /works/1
  # GET /works/1.json
  def show
  end


  # GET /works/new
  def new
    @work = Work.new
    @work.build_code
  end


  # GET /works/1/edit
  # def edit
  # end
  

  # POST /works
  # POST /works.json
  def create
    @work = Work.new work_params

    @work.user_id = @user.id
    @work.naming
    @work.code.naming @work.name

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
    @work.code.update_content(work_params[:code_attributes])
    @work.touch

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
    language = work_params[:language]
    content, input = work_params[:code_attributes].slice(:content, :input).values

    result = Program.new(language, content, input).execute

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
      params.require(:work).permit(:name, :language, code_attributes: [:content, :input])
    end
end

