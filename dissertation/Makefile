submission: diss.pdf
	mv diss.pdf sdab20-WRITEUP-SimonBuist-Dissertation-2014.pdf
	tar cjvf sdab20-CODE-SimonBuist-Dissertation-2014.tar.bz code

diss.pdf: diss.markdown appendices.tex
	pandoc -s --variable mainfont=Georgia --variable sansfont=Arial --variable monofont="Bitstream Vera Sans Mono" --variable fontsize=12pt -A appendices.tex --template custom.tex --toc -N --smart --bibliography $*.bib --csl the-open-university-harvard.csl -f markdown -o diss.pdf diss.markdown

diss.html: diss.markdown appendices.html
	pandoc -N --template=html.template --variable mainfont=Georgia --variable sansfont=Arial --variable monofont="Bitstream Vera Sans Mono" --variable fontsize=12pt -A appendices.html --toc --smart --bibliography $*.bib --csl the-open-university-harvard.csl -f markdown -t html5 -o $@ $<

%.tex: %.markdown
	pandoc -s --toc -N --smart --bibliography $*.bib -f markdown -o $@ $<


appendices.tex: includeCode
	pandoc --filter ./includeCode -o appendices.tex appendices.markdown

appendices.html: includeCode
	pandoc --filter ./includeCode -o appendices.html appendices.markdown

diss.markdown: includeMarkdown
	pandoc --filter ./includeMarkdown -o diss.markdown master.markdown

includeMarkdown:
	ghc includeMarkdown.hs

includeCode:
	ghc includeCode.hs
