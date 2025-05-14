
.PHONY: build test clean bench-runtime-list

build:
	stack build

test:
	stack test --coverage

clean:
	stack clean
	rm -f *.aux *.log *.out *.snm *.toc *.vrb *.nav *.synctex.gz *.blg *.bbl *.fdb_latexmk *.fls *.ind *.idx *.ilg *.bcf *.run.xml *.xdv

SELECTION = conPieL-IPL- conPieR-IPL- boxesTop-K- lobBoxes-K4- lobBoxes-GL-

bench/runtime.pdf: bench/runtime.hs lib/*.hs
	@(which pdflatex > /dev/null)|| (echo "Could not find pdflatex, please install it first." && false)
	@date
	stack bench :runtime --benchmark-arguments "$(SELECTION)"
	@date
	cd bench && pdflatex -interaction=nonstopmode runtime

bench-runtime-list:
	stack bench :runtime --benchmark-arguments "--list"

bench/runtime-all.pdf: bench/runtime.hs lib/*.hs
	@(which pdflatex > /dev/null)|| (echo "Could not find pdflatex, please install it first." && false)
	@date
	stack bench :runtime # runs all by default
	@date
	cd bench && pdflatex -interaction=nonstopmode runtime-all

bench/memory.pdf: bench/memory.hs lib/*.hs
	@(which pandoc > /dev/null) || (echo "Could not find pandoc, please install it first." && false)
	@(which pdflatex > /dev/null) || (echo "Could not find pdflatex, please install it first." && false)
	stack bench :memory --no-run-benchmarks # check if it compiles
	stack bench :memory --ba "--markdown " > bench/table.md 2>&1
	echo "|Logic|Formula|Prover|Size|Result|Allocated|GCs|\n|:---|:---|:---|:---|:---|---:|---:|" > bench/memory.md
	cd bench && (cat table.md | sed '0,/--:/d' |  head -n -1 | sort) >> memory.md
	cd bench && (pandoc -t latex -V geometry:margin=2cm -s memory.md > memory.tex)
	cd bench && pdflatex -interaction=nonstopmode memory && pdflatex -interaction=nonstopmode memory

bench/memory-all.pdf: bench/memory.hs lib/*.hs
	@(which pandoc > /dev/null) || (echo "Could not find pandoc, please install it first." && false)
	@(which pdflatex > /dev/null) || (echo "Could not find pdflatex, please install it first." && false)
	stack bench :memory --no-run-benchmarks # check if it compiles
	stack bench :memory --ba "--markdown --all-formulas" > bench/table-all.md 2>&1
	echo "|Logic|Formula|Prover|Size|Result|Allocated|GCs|\n|:---|:---|:---|:---|:---|---:|---:|" > bench/memory-all.md
	cd bench && (cat table-all.md | sed '0,/--:/d' |  head -n -1 | sort) >> memory-all.md
	cd bench && (pandoc -t latex -V geometry:margin=2cm -s memory-all.md > memory-all.tex)
	cd bench && pdflatex -interaction=nonstopmode memory-all && pdflatex -interaction=nonstopmode memory-all
