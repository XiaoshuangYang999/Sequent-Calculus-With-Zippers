
.PHONY: build run test clean bench-quick bench-list

build:
	stack build

run:
	stack build && stack exec myprogram

test:
	stack test --coverage

clean:
	stack clean
	rm -f *.aux *.log *.out *.snm *.toc *.vrb *.nav *.synctex.gz *.blg *.bbl *.fdb_latexmk *.fls *.ind *.idx *.ilg *.bcf *.run.xml *.xdv

# TODO: adjust list of benchmarks to include
bench-quick:
	stack bench :Bench --benchmark-arguments \
		"boxesTop-K-GenZ/20 boxesTop-K-GenZ/40 boxesTop-K-GenT/20 boxesTop-K-GenT/40 conBot-R-CPL-GenT/20 conBot-R-CPL-GenT/40 conBot-R-CPL-GenZ/20 conBot-R-CPL-GenZ/40 conBot-L-CPL-GenT/20 conBot-L-CPL-GenT/40 conBot-L-CPL-GenZ/20 conBot-L-CPL-GenZ/40" 
	cd bench && latexmk -pdf -interaction=nonstopmode bench

bench-list:
	stack bench :Bench --benchmark-arguments "--list"
