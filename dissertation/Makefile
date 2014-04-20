submission: diss.pdf
	mv diss.pdf sdab20-WRITEUP-SimonBuist-Dissertation-2014.pdf
	tar cjvf sdab20-CODE-SimonBuist-Dissertation-2014.tar.bz code

diss.pdf: diss.markdown appendices.tex
	pandoc -s --variable mainfont=Georgia --variable sansfont=Arial --variable monofont="Bitstream Vera Sans Mono" --variable fontsize=12pt -A appendices.tex --template style/latex.template --toc -N --smart --bibliography sections/biblio.bib --csl style/the-open-university-harvard.csl -f markdown -o diss.pdf diss.markdown

diss.html: diss.markdown appendices.html
	pandoc -N --template=style/html.template --variable mainfont=Georgia --variable sansfont=Arial --variable monofont="Bitstream Vera Sans Mono" --variable fontsize=12pt -A appendices.html --toc --smart --bibliography sections/biblio.bib --csl the-open-university-harvard.csl -f markdown -t html5 -o diss.html diss.markdown

appendices.tex: sections/appendices.markdown
	pandoc --filter ./includeCode.hs -o appendices.tex sections/appendices.markdown

appendices.html: sections/appendices.markdown
	pandoc --filter ./includeCode.hs -o appendices.html sections/appendices.markdown

diss.markdown: master.markdown sections/title-abstract.markdown sections/conclusions.markdown sections/design.markdown sections/experimental-methodology.markdown sections/implementation.markdown sections/introduction.markdown sections/literature-survey.markdown sections/pilot1.markdown sections/pilot2.markdown sections/project-plan.markdown sections/appendices.markdown
	pandoc --filter ./includeMarkdown.hs -o diss.markdown master.markdown
