
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

# TODO adjust list of benchmark cases

bench/runtime.pdf:
	@(which pdflatex > /dev/null)|| (echo "Could not find pdflatex, please install it first." && false)
	stack bench :Bench --benchmark-arguments \
		"boxesTop-K-GenZ/20 boxesTop-K-GenZ/40 boxesTop-K-GenT/20 boxesTop-K-GenT/40 conBot-R-CPL-GenT/20 conBot-R-CPL-GenT/40 conBot-R-CPL-GenZ/20 conBot-R-CPL-GenZ/40 conBot-L-CPL-GenT/20 conBot-L-CPL-GenT/40 conBot-L-CPL-GenZ/20 conBot-L-CPL-GenZ/40"
	cd bench && pdflatex -interaction=nonstopmode runtime

bench-runtime-list:
	stack bench :Bench --benchmark-arguments "--list"

bench/memory.pdf:
	@(which pandoc > /dev/null) || (echo "Could not find pandoc, please install it first." && false)
	@(which pdflatex > /dev/null) || (echo "Could not find pdflatex, please install it first." && false)
	stack bench seqzip:bench:memory --no-run-benchmarks # check if it compiles
	stack bench seqzip:bench:memory --ba "--markdown " > bench/table.md 2>&1
	echo "|Logic|Formula|Prover|Size|Result|Allocated|GCs|\n|:---|:---|:---|:---|:---|---:|---:|" > bench/memory.md
	cd bench && (cat table.md | sed '0,/--:/d' |  head -n -1 | sort) >> memory.md
	cd bench && (pandoc -t latex -V geometry:margin=2cm -s memory.md > memory.tex)
	cd bench && pdflatex -interaction=nonstopmode memory && pdflatex -interaction=nonstopmode memory
