
bench:
	cd .. && stack bench

memory:
	stack build report:bench:Memory --no-run-benchmarks
	stack bench report:bench:Memory


.PHONY: bench memory
