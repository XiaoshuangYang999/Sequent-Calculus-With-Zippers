
.PHONY: build run test clean bench-runtime-list

build:
	stack build

run:
	stack build && stack exec myprogram

test:
	stack test --coverage

clean:
	stack clean
	rm -f *.aux *.log *.out *.snm *.toc *.vrb *.nav *.synctex.gz *.blg *.bbl *.fdb_latexmk *.fls *.ind *.idx *.ilg *.bcf *.run.xml *.xdv

SELECTION = conPieL-IPL- conPieR-IPL- boxesTop-K- lobBoxes-K4- lobBoxes-GL-

bench/runtime.pdf:
	@(which pdflatex > /dev/null)|| (echo "Could not find pdflatex, please install it first." && false)
	stack bench :runtime --benchmark-arguments "$(SELECTION)"
	cd bench && pdflatex -interaction=nonstopmode runtime

bench-runtime-list:
	stack bench :Bench --benchmark-arguments "--list"

bench/memory.pdf:
	@(which pandoc > /dev/null) || (echo "Could not find pandoc, please install it first." && false)
	@(which pdflatex > /dev/null) || (echo "Could not find pdflatex, please install it first." && false)
	stack bench :memory --no-run-benchmarks # check if it compiles
	stack bench :memory --ba "--markdown " > bench/table.md 2>&1
	echo "|Logic|Formula|Prover|Size|Result|Allocated|GCs|\n|:---|:---|:---|:---|:---|---:|---:|" > bench/memory.md
	cd bench && (cat table.md | sed '0,/--:/d' |  head -n -1 | sort) >> memory.md
	cd bench && (pandoc -t latex -V geometry:margin=2cm -s memory.md > memory.tex)
	cd bench && pdflatex -interaction=nonstopmode memory && pdflatex -interaction=nonstopmode memory
