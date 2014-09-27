pandoc := pandoc

pandocflags :=        \
  --normalize         \
  --number-sections   \
  --preserve-tabs     \
  --section-divs      \
  --standalone        \
  --table-of-contents \

source-format := mdown
formats       := pdf
clean-formats := aux log out toc

pandoc-latex-args :=             \
  --latex-engine=xelatex         \
  --include-in-header=header.tex \
  --variable=fontsize:12pt       \
  --variable=lang:spanish        \

extra-latex-sources := \
  header.tex           \
  header.png           \



sources := $(wildcard *.$(source-format))
files   := $(sources:%.$(source-format)=%.pdf)

pandocrule = $(pandoc) $(pandocflags) -o $@ $<



.PHONY: all clean

all: $(files)

clean: ; rm -f $(clean-formats:%=./*.%) $(files)



%.pdf : %.$(source-format) Makefile $(extra-latex-sources) ; $(pandocrule) $(pandoc-latex-args)
%.tex : %.$(source-format) Makefile $(extra-latex-sources) ; $(pandocrule) $(pandoc-latex-args)
