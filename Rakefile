require 'rake/clean'

MAIN = "Dagger"
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
  sh "pandoc -f markdown+lhs -t latex+lhs -N --template=#{template} --variable=format:#{FORMAT} -o #{t.name} #{t.source}"
end

rule '.tex' => '.ltex' do |t|
  if t.source == MAIN + ".ltex"
    sh "lhs2TeX --poly -o #{t.name} #{t.source}"
  else
    sh "lhs2TeX --poly -o #{t.name} #{t.source}"
    f = File.open t.name
    l = f.readlines
    f.close
    f = File.new(t.name, "w")
    r = []
    while l.size > 0
      r = []
      cline = ""
      until cline == "\\EndFmtInput\n" or l.size == 0
        cline = l.shift
        r << cline unless cline == "%\n"
      end
    end
    r.each {|r| f.puts r}
    f.close
  end
  # If its not the main file strip off all things after \EndFmtInput
end



task :xetex => TEX do
  sh "xelatex #{MAIN}.tex"
end

task :default => :xetex
