require 'rails_helper'

describe CodeDecorator do
  let(:code) { Code.new.extend CodeDecorator }
  subject { code }
  it { should be_a Code }
end
