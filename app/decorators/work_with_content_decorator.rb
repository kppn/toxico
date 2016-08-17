module WorkWithContentDecorator
  def hoge
    "hogefuga"
  end
  
  def thumbnail
    content.split("\n").first(3).map{|line|
      line.chomp!
       line = line.slice(0,60) + '...' if line.length >= 63
       line
    }.join("\n")
  end
end


