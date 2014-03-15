%.pdf: %.markdown footer.tex
	pandoc -s -A footer.tex --template custom.tex --toc -N --smart --bibliography $*.bib -f markdown -o $@ $<

%.beamer.pdf: %.markdown
	pandoc -s -N --smart --bibliography $*.bib -f markdown -t beamer -o $@ $<

%.html: %.markdown
	pandoc -N --toc --smart --bibliography $*.bib -f markdown -t html --css style.css --standalone -o $@ $<

%.docx: %.markdown
	pandoc -s --smart --bibliography $*.bib -f markdown -t docx -o $@ $<

%.tex: %.markdown
	pandoc -s --toc -N --smart --bibliography $*.bib -f markdown -o $@ $<

%.cleaned.markdown: %.markdown
	pandoc -f markdown -o $@ $< && mv $@ $<

footer.tex:
	pandoc -o footer.tex footer.markdown
