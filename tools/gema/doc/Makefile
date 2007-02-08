
# $Id: Makefile,v 1.6 2003/12/28 02:15:03 dngray Exp $

all: doc.ps gema.ps doc.html gema.html gema.lpr

#	user/reference manual

TEXFILES = doc.tex overview.tex notation.tex function.tex conclude.tex

doc.dvi: $(TEXFILES)
	latex doc.tex || rm -f doc.dvi

doc.ps: doc.dvi
	dvi2ps doc.dvi > $@ || dvips doc.dvi -o $@

view: doc.dvi
	xtex doc.dvi &

doc.html: $(TEXFILES) latex-to-html
	./latex-to-html doc.tex $@

latex-to-html:
	gema -line -literal './' -p '/*/latex.dat=../examples/latex.dat' \
		-in ../examples/latex.sh -out $@
	chmod a+x $@

# ------------------------------------------------------------------

#	man page

gema.lpr: gema.1
	nroff -man gema.1 > $@

gema.ps: gema.1
	ptroff -t -man gema.1 > $@ || groff -Tps -man gema.1 > $@

gema.html: gema.1
	gema -f ../examples/man-html.dat gema.1 $@

# ------------------------------------------------------------------

# remove all generated files:
clean:
	rm -f *.aux doc.dvi doc.ps doc.toc doc.*log *.html *.bak \
		doc.IX* doc.LX gema.lpr gema.ps

# remove temporary files:
tidy:
	rm -f *.aux doc.toc doc.*log *.bak doc.IX*
