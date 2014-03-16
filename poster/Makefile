all: poster.pdf

poster.pdf: poster.dvi
	pdflatex poster

poster.dvi: poster.aux poster.bbl
	pdflatex poster

poster.aux:
	pdflatex poster

poster.bbl:
	bibtex poster

clean:
	rm -f poster.dvi poster.aux poster.bbl poster.log poster.lof poster.lot poster.toc poster.blg poster.pdf poster.out poster_defs.aux

