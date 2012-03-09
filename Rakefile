require 'rake/clean'

MAIN = "dagger"
FORMAT = "polycode.fmt"

CLEAN.include('*.ltex')
CLEAN.include('*.tex')
CLEAN.include('*.aux')
CLEAN.include('*.log')
CLEAN.include('*.ptb')
CLEAN.include('*.out')
CLOBBER.include('*.pdf')

SRC = FileList['*.lhs']
LHSTEX = SRC.ext('ltex')
TEX = SRC.ext('tex')

rule '.ltex' => '.lhs' do |t|
  template = "include.latex"
  template = "polycode.latex" if t.source == MAIN + ".lhs"
  sh "pandoc -f markdown+lhs -t latex+lhs --template=#{template} --variable=format:#{FORMAT} -o #{t.name} #{t.source}"
end

rule '.tex' => '.ltex' do |t|
  if t.source == MAIN + ".ltex"
    sh "lhs2TeX -o  #{t.name} #{t.source}"
  else
    sh "lhs2TeX -o  #{t.name} #{t.source}"
    f = File.open t.name
    l = f.readlines
    f.close
    f = File.new(t.name, "w")
    until l.shift == "\\EndFmtInput\n"
    end
    until l.shift == "\\EndFmtInput\n"
    end
    l.each {|l| f.puts l}
    f.close
  end
  # If its not the main file strip off all things after \EndFmtInput
end


task :xetex => TEX do
  sh "xelatex #{MAIN}.tex"
end

task :default => :xetex
