
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

tos_prs_pis=$(tos)/$(prs)/$(pis)

#commands
ghc=ghc -no-keep-hi-files -no-keep-o-files

execs=$(shell ls $(tis_prs) | sed "s/\(.*\).lc/$(tos__cps)\/\1.out/g")
hs_prs=$(shell ls $(tis_prs) | sed "s/\(.*\).lc/$(tos__prs)\/\1.hs/g")

# rules
all: $(hs_prs) $(execs) grules

$(tos_cps)/%.out: $(tos_prs)/%.hs $(tos_cps)
	$(ghc) $< -o $@

$(tos_prs)/%.hs: lcc $(tis_prs)/%.lc $(tos_prs) $(tos_prs_pis)
	./$< $(word 2, $^); mv $(basename $(word 2, $^)).hs $@

$(tos_cps): $(tos)
	mkdir $@

$(tos_prs): $(tos)
	mkdir $@

$(grs): $(tos)
	mkdir $@

$(tos_prs_pis): $(tos_prs)
	ln -s $(shell pwd)/$(pis) $(tos_prs)

$(tos):
	mkdir $@

lcc: src/lcc.hs
	cd src; $(ghc) $@.hs -o ../$@

grules: src/grules.hs $(grs)
	cd src; $(ghc) $@.hs -o ../$@; cd ..; ./$@

clean:
	rm -rf lcc grules $(tos_cps)/* $(hs_prs) $(grs)/* $(tos)

clean_execs:
	rm $(tos_cps)/*

clean_hs_prs:
	rm $(hs_prs)

clean_grs:
	rm $(grs)/*

test_cps:
	cd $(tos_cps); for f in $$(ls); do echo ""; echo $$f; echo ""; ./$$f; done
