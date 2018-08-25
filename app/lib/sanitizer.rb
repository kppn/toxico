

class Sanitizer
  include ActionView::Helpers::SanitizeHelper

  def initialize(params, targets)
    targets.each do |target|
      case target
      when :html
        :aaa
      when :sql
        true
      else
        raise 'sanitizer unknown target'
      end
    end
  end


  private
    def sanitize_html_tag
      self.instance_variables.each do |val_name|
        orig_val = instance_variable_get(val_name)
        sanitized_val = sanitize(orig_val)
        log if orig_val != sanitized_val
        instance_variables_set(val_name, sanitized_val)
      end
    end

end

