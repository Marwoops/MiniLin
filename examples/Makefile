EXAMPLES=$(wildcard *.minilin)
OUTPUTS=$(subst .minilin,.res,$(EXAMPLES))

.PHONY: all

all: $(OUTPUTS)

%.res: %.minilin
	dune exec ../bin/minilin.exe $< > $@
