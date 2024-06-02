
# paths

tis=test_inputs
tos=test_outputs
cps=compiled_progs
grs=$(tos)/grammar_rules
prs=programs
pis=PredefImports/

tos_cps=$(tos)/$(cps)
tos__cps=$(tos)\/$(cps)

tos_prs=$(tos)/$(prs)
tos__prs=$(tos)\/$(prs)

tis_prs=$(tis)/$(prs)

#commands

ghc=ghc -no-keep-hi-files -no-keep-o-files

execs=$(shell ls $(tis_prs) | sed "s/\(.*\).lc/$(tos__cps)\/\1.out/g")
hs_prs=$(shell ls $(tis_prs) | sed "s/\(.*\).lc/$(tos__prs)\/\1.hs/g")

# rules: all .out .hs

all: dirs $(hs_prs) $(execs) grules

dirs: $(tos_prs) $(tos_cps) $(grs) $(tos_prs)/$(pis)

$(tos_prs):
	mkdir -p $@

$(tos_cps):
	mkdir -p $@

$(grs):
	mkdir -p $@

$(tos_prs)/$(pis):
	ln -sf $(shell pwd)/$(pis) $(tos_prs)

$(tos_cps)/%.out: $(tos_prs)/%.hs
	$(ghc) $< -o $@

$(tos_prs)/%.hs: lcc $(tis_prs)/%.lc
	./$< $(word 2, $^); mv $(basename $(word 2, $^)).hs $@

# rules: lcc gruls

lcc: src/lcc.hs
	cd src; $(ghc) $@.hs -o ../$@

grules: src/grules.hs
	cd src; $(ghc) $@.hs -o ../$@; cd ..; ./$@

# rules: clean

clean:
	rm -rf lcc grules $(tos_cps)/* $(hs_prs) $(grs)/* $(tos)

clean_execs:
	rm $(tos_cps)/*

clean_hs_prs:
	rm $(hs_prs)

clean_grs:
	rm $(grs)/*

# rules: test

test_cps:
	cd $(tos_cps); for f in $$(ls); do echo ""; echo $$f; echo ""; ./$$f; done
