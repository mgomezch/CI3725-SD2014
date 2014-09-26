pandoc := pandoc

pandocflags :=      \
  --normalize       \
  --number-sections \
  --preserve-tabs   \
  --section-divs    \
  --standalone

formats := html
sources := $(wildcard ./*.mdown)
files   := $(sources:%.mdown=%.html)

pandocrule = $(pandoc) $(pandocflags) -o $@ $<



.PHONY: all clean

all: $(files)

clean: ; rm -f $(files)

%.html: %.mdown; $(pandocrule)
