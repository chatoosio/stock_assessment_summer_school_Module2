
PDFS := $(patsubst %.md,../%.md.pdf,$(wildcard *.md))
TEMPLATE = `git rev-parse --show-toplevel`/Template/jrc/jrc.tex

all : $(PDFS)

../%.md.pdf : %.md
	pandoc -t beamer -S --template $(TEMPLATE) $< -o $@

clean :
	rm $(PDFS)
