%.pdf: %.markdown appendices.tex
	pandoc -s --variable mainfont=Georgia --variable sansfont=Arial --variable monofont="Bitstream Vera Sans Mono" --variable fontsize=12pt -A appendices.tex --template custom.tex --toc -N --smart --bibliography $*.bib --csl the-open-university-harvard.csl -f markdown -o $@ $<

%.beamer.pdf: %.markdown
	pandoc -s -N --smart --bibliography $*.bib -f markdown -t beamer -o $@ $<

%.html: %.markdown appendices.html
	pandoc -N --variable mainfont=Georgia --variable sansfont=Arial --variable monofont="Bitstream Vera Sans Mono" --variable fontsize=12pt -A appendices.html --toc --smart --bibliography $*.bib --csl the-open-university-harvard.csl -f markdown -t html --css style.css --standalone -o $@ $<

%.docx: %.markdown
	pandoc -s --smart --bibliography $*.bib -f markdown -t docx -o $@ $<

%.tex: %.markdown
	pandoc -s --toc -N --smart --bibliography $*.bib -f markdown -o $@ $<

%.cleaned.markdown: %.markdown
	pandoc -f markdown -o $@ $< && mv $@ $<

appendices.tex:
	pandoc -o appendices.tex appendices.markdown

appendices.html:
	pandoc -o appendices.html appendices.markdown
