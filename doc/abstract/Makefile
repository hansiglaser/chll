
all: abstract.pdf

abstract.pdf: abstract.tex abstract.bbl
	pdflatex -shell-escape $<

abstract.aux: abstract.tex
	pdflatex -shell-escape $<

abstract.bbl: abstract.aux ../bibliography.bib
	@bibtex abstract

clean:
	rm -f *.aux *.bbl *.log *.out *.blg *.toc *~ *.pdf
