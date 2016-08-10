class Emacsable
  def self.readline(prompt)
    print prompt
    (gets || '').chomp
  end
end
Pry.config.input = Emacsable

Pry.config.pager = false if ENV["INSIDE_EMACS"]
Pry.config.correct_indent = false if ENV["INSIDE_EMACS"]
Pry.config.print = proc { |output, value| output.puts "=> #{value.inspect}" }
Pry.config.exception_handler = proc do |output, exception, _|
  output.puts "#{exception.class}: #{exception.message}"
  output.puts "from #{exception.backtrace.first}"
end
