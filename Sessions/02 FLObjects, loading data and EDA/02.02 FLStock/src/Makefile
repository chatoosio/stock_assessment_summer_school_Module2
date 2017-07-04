
PDFS := $(patsubst %.md,../%.md.pdf,$(wildcard *.md))
TEMPLATE = `git rev-parse --show-toplevel`/Template/jrc/jrc.tex

all : $(PDFS)

../%.md.pdf : %.md
	pandoc -t beamer -S --template $(TEMPLATE) $< -o $@

%.md: %.Rmd
	Rscript -e "library(knitr)" \
		-e "knit('$*.Rmd')"

clean :
	rm $(PDFS)
	rm -rf figure
