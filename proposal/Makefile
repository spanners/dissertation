# This is a makefile. Look up how to use this by running:
# 	$ man make
# to view the manual page on the command make

all: diss.pdf
	mv diss.pdf sdab20-proposal.pdf

diss.pdf: diss.dvi
	pdflatex diss

diss.dvi: diss.aux diss.bbl diss.gls
	latex diss

diss.gls: diss.glo diss.ist
	makeindex diss.glo -s diss.ist -t diss.glg -o diss.gls

diss.ist:
	latex diss

diss.glo:
	latex diss

diss.aux:
	latex diss

diss.bbl:
	bibtex diss

clean:
	rm -f diss.dvi diss.aux diss.bbl diss.log diss.lof diss.lot diss.toc diss.blg sdab20-proposal.pdf diss.out diss_defs.aux diss.gls diss.glo diss.ist diss.glg diss.gls
