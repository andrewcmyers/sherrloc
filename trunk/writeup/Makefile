# This is a makefile template to be copied & modified. Copy it to
# "Makefile" in your directory and do "chmod u+w Makefile".
# -- Andrew Myers

DOC = draft
#
# change this line to contain the name of the document without extension

TARGETS = $(DOC).pdf
#
# TARGETS could be $(DOC).dvi instead, if there are no figures

TEXS = $(DOC).tex main.tex declarations.sty abstract.tex header.tex \
    tightheaders.sty
#*.tex *.sty
#
# Add any included TeX files to TEXS

FIGS = breakdownhw.pdf breakdownstudent.pdf consgraph.pdf consgraph.pdf \
       hypograph.pdf ocaml-performance.pdf ocaml.pdf onecons.pdf seminal.pdf

#
# List of postscript figures to be included

MAKEDIR=$(HOME)/aplpapers/make

include $(MAKEDIR)/commondefs
#
# Remove the include above and uncomment the following line if you have
# commondefs installed in the current directory.

# include commondefs

default: $(TARGETS)

BIBFILE = ../bibtex/pm-master.bib constraint.bib
#
# Ignore this if you're not using bibtex. Change the $(DOC) part if your bib
# file has a different name from your document

$(DOC).dvi: $(TEXS) $(FIGS) $(DOC).stamp $(DOC).bbl $(BIBFILE)
$(DOC).bbl: $(BIBFILE) $(DOC).stamp
$(DOC).pdf: $(TEXS) $(FIGS) $(DOC).stamp $(DOC).bbl $(BIBFILE)
#
# To use bibtex, add these to dependency list:
# 
# To use makeindex, add this to dependency list:
# $(DOC).ind
#
# To use glossaries, add this to dependency list:
# $(DOC).glo

.ps.pdf: 
	epstopdf $< > $@

$(DOC).glo $(DOC).ind: $(DOC).stamp
$(DOC).ps: $(DOC).dvi $(FIGS)
#
# These are standard dependencies. Shouldn't need to modify these.

VPATH = ../tex-macros:graph:

include $(COMMONRULES)
