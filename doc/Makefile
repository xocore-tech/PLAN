.PHONY: all clean

all: out/primop.pdf out/judge.pdf out/planvm-amd64.pdf

clean:
	rm -f *.aux *.log *.out *.fdb_latexmk *.synctex.gz *.fls *.html

%.s: %.tex common.tex
	x/extract < $< > $@

out/%.pdf: %.tex
	latexmk -outdir=out -interaction=nonstopmode -file-line-error -synctex=1 -pdf $<

out/%.html: %.md
	pandoc -i $< -o $@

%.tex: %.md
	pandoc -i $< -o $@
