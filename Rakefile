# Common build system
require 'rubygems'
require 'rake'


class String
    def red; colorize(self, "\e[1m\e[31m"); end
    def green; colorize(self, "\e[1m\e[32m"); end
    def dark_green; colorize(self, "\e[32m"); end
    def yellow; colorize(self, "\e[1m\e[33m"); end
    def blue; colorize(self, "\e[1m\e[34m"); end
    def dark_blue; colorize(self, "\e[34m"); end
    def pur; colorize(self, "\e[1m\e[35m"); end
    def colorize(text, color_code)  "#{color_code}#{text}\e[0m" end
end

RUN_INCLUDE_PATHS = "-pa ./ebin -pa ./ebin/eunit -pa ./include -pa ./src"
ERLC_TEST_FLAGS = "#{RUN_INCLUDE_PATHS} -pa ../ebin/eunit -I .. -I ../test -I ../include/eunit -DTEST"
ERLC_FLAGS = "+debug_info -W2 -I ../include -o ../ebin -pa ../ebin"



desc "Compiles all files and writes the binaries to ./ebin"
task :build do
  trace_messages = ENV['TRACE'] ? " -DFILE_TRACE" : "" # write messages from/to clients to messages-[nick].txt files
  bot_mode = ENV['BOT'] ? " -DBOT_MODE" : "" # no same-ip check and no login pwd check
  debug_cards = ENV['DEBUG_CARDS'] ? " -DDEBUG_CARDS" : "" # give fixed cards to players
  modules = ENV['MOD'] ? "#{ENV['MOD']}.erl" : "*.erl"
  cp "src/edbi.app", "ebin"
  cd "src"
  sh "erlc  #{ERLC_FLAGS} #{trace_messages} #{bot_mode} #{debug_cards} -v #{modules}"
  cd ".."
end


desc "Compiles all files including test definition and writes the binaries to ./ebin"
task :build_test do
  modules = ENV['MOD'] ? "#{ENV['MOD']}.erl" : "*.erl"
  cd "src"
  sh "erlc  #{ERLC_FLAGS} #{ERLC_TEST_FLAGS} -v #{modules}"
  cd ".."
end

desc "starts the erlang console with correct load path"
task :console do
  node_name = "console_#{Time.now.to_i}"
  sh "erl #{RUN_INCLUDE_PATHS} -sname #{node_name} -setcookie edbi"
end

desc "prepares the erlang dialyzer"
task :prepare_dialyzer do
  erlang_home = "/opt/local/lib/erlang"
  libs = ["stdlib*","kernel*", "asn1*", "compiler*", "crypto*", "syntax_tools*", "inets*", "parsetools*", "xmerl*", "mnesia*", "ssl*", "dbg*", "hipe*", "tools*"] 
  libString = libs.collect {|lib| "#{erlang_home}/lib/#{lib}/ebin"}.join(" ")
  sh "dialyzer --output_plt .dialyzer_otp.plt -r #{libString}"
end

desc "starts the erlang dialyzer "
task :dialyzer do
  sh "dialyzer --plt .dialyzer_otp_edbi_mochiweb.plt --verbose -Wno_unused -c ./ebin/*.beam"
end

desc "removes the build binaries"
task :clean do
  cd "ebin"
  sh "rm -f *.beam"
  cd ".."
end

desc "starts the server and opens the erlang console"
task :start do
  sh "erl +K true #{RUN_INCLUDE_PATHS} -sname edbi -setcookie edbi -s mnesia start -run application load edbi -run application start edbi"
end


desc "builds and starts the edbi project"
task :startc => [:build, :start]  do
end



task :run_singe_test do
  mods = [ENV['MOD']]
  mod_directives = mods.map {|m| "-run #{m} test"}.join(" ")
  sh %Q{erl +K true -pz ./test -pz ./ebin/ -pa ./ebin/eunit -s mnesia start -sname master1 -noshell #{mod_directives} -run erlang halt}
end

task :test => [:build_test] do
  mods = []
  mod_directives = ""
  env_peek = ENV['MOD'] || ENV['MODS'] || ENV['MODULE'] || ENV['MODULES']
  env_variables = ENV['ENV'] ? "-env ENV " + ENV['ENV'] + " " : ""
  if env_peek
    mods = env_peek.split(",")
  else 
    mods = Dir["test/*_test.erl"].map { |x| x.match(/test\/(.*)_test.erl/)[1] }
  end
  mod_directives = mods.map {|m| "-run #{m} test"}.join(" ")
  # -run #{ENV['MOD']} test
  sh %Q{erl +K true -pz ./test -pz ./ebin/ -pa ./ebin/eunit -pa ./ebin/edbi -s mnesia start -sname test #{env_variables} -noshell #{mod_directives} -run erlang halt}
end

desc "Generates edoc documentation and writes it to the doc folder"
task :docs do
  files = Dir["src/edbi.erl"].map { |x| "'../" + x + "'"}.join " "
  sh %|cd doc && erl -noshell -s init stop -run edoc files #{files}|
end

desc "Starts textmate wit required directories only"
task :mate do
  sh "mate include src test Rakefile"
end

desc "Prints statistics about lines of code and tests"
task :stats do
  puts "+--------------------------+--------+---------+------------+----------+---------+"
  puts "| Module                   | Lines  | LOC     | Test Lines | Test LOC | Tests   |"
  puts "+--------------------------+--------+---------+------------+----------+---------+"
  
  pattern = /.*\.erl$/
  totals = { :lines => 0, :loc => 0, :test_lines => 0, :test_loc => 0, :tests => 0 }
  Dir.foreach("./src/") do |file_name| 
    next unless file_name =~ pattern
    module_name = file_name.gsub(".erl", "")
    stats = { :lines => 0, :loc => 0, :test_lines => 0, :test_loc => 0, :tests => 0 }
    f = File.open("./src/" + file_name)
    while line = f.gets
      stats[:lines] += 1
      stats[:loc]   += 1 unless line =~ /^\s*$/ || line =~ /^\s*#/
    end
    if File.exists?("./test/#{module_name}_test.erl")
      f = File.open("./test/#{module_name}_test.erl")
      while line = f.gets
        stats[:test_lines] += 1
        stats[:test_loc]   += 1 unless line =~ /^\s*$/ || line =~ /^\s*#/
        stats[:tests] += 1 if line =~ /_test()/
      end
    end
    totals.each_key { |key| totals[key] += stats[key] }
    puts "| #{module_name[0..23].to_s.ljust(24)} | #{stats[:lines].to_s.rjust(6)} |  #{stats[:loc].to_s.rjust(6)} | #{stats[:test_lines].to_s.rjust(10)} | #{stats[:test_loc].to_s.rjust(8)} | #{stats[:tests].to_s.rjust(7).send(stats[:tests] == 0 ? :red : :green)} |"
  end
  stats = totals
  puts "+--------------------------+--------+---------+------------+----------+---------+"
  puts "| #{"Total".ljust(24)} | #{stats[:lines].to_s.rjust(6)} |  #{stats[:loc].to_s.rjust(6)} | #{stats[:test_lines].to_s.rjust(10)} | #{stats[:test_loc].to_s.rjust(8)} | #{stats[:tests].to_s.rjust(7)} |"
  puts "+--------------------------+--------+---------+------------+----------+---------+"
end
