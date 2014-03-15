# This is a makefile. Look up how to use this by running:
# 	$ man make
# to view the manual page on the command make

all: diss.pdf
	mv diss.pdf sdab20-litreview.pdf

diss.pdf: diss.dvi
	pdflatex diss

diss.dvi: diss.aux diss.bbl
	latex diss

diss.aux:
	latex diss

diss.bbl:
	bibtex diss

clean:
	rm -f diss.dvi diss.aux diss.bbl diss.log diss.lof diss.lot diss.toc diss.blg sdab20-litreview.pdf diss.out diss_defs.aux 
