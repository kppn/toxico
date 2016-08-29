class Work < ActiveRecord::Base
  belongs_to :user

  has_one :code
  accepts_nested_attributes_for :code


  scope :ruby,         ->{where language: 'ruby'}
  scope :c,            ->{where language: 'c'}
  scope :crystal,      ->{where language: 'crystal'}
  scope :go,           ->{where language: 'go'}
  scope :javascript,   ->{where language: 'javascript'}
  scope :coffeescript, ->{where language: 'coffeescript'}
  scope :bash,         ->{where language: 'bash'}


  def naming
    if self.name.empty?
      naming_by_default
    end
  end

  protected
    def naming_by_default
      basetime = DateTime.now.strftime('%Y%m%d_%H%M%S')
      self.name = "unknown_#{basetime}"
      self
    end
end

