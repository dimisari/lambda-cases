
# paths

tis=test_inputs
tos=test_outputs
cps=compiled_progs
grs=grammar_rules
prs=programs
pis=src/PredefImports/
pi1=$(pis)/Predefined.hs
pi2=$(pis)/OpsInHaskell.hs

tos_cps=$(tos)/$(cps)
tos__cps=$(tos)\/$(cps)

tos_prs=$(tos)/$(prs)
tos__prs=$(tos)\/$(prs)

tos_grs=$(tos)/$(grs)
tos__grs=$(tos)\/$(grs)

tis_prs=$(tis)/$(prs)
tis_grs=$(tis)/$(grs)

#commands

ghc=ghc -no-keep-hi-files -no-keep-o-files

execs=$(shell ls $(tis_prs) | sed "s/\(.*\).lc/$(tos__cps)\/\1.out/g")
hs_prs=$(shell ls $(tis_prs) | sed "s/\(.*\).lc/$(tos__prs)\/\1.hs/g")
hs_grs=$(shell ls $(tis_grs) | sed "s/\(.*\).txt/$(tos__grs)\/\1.hs/g")

# rules: all .out .hs

all: dirs $(hs_prs) $(execs) $(hs_grs)

dirs: $(tos_prs) $(tos_cps) $(tos_grs) $(tos_prs)/$(pis)

$(tos_prs):
	mkdir -p $@

$(tos_cps):
	mkdir -p $@

$(tos_grs):
	mkdir -p $@

$(tos_prs)/$(pis):
	ln -sf $(shell pwd)/$(pis) $(tos_prs)

$(tos_cps)/%.out: lcc $(tis_prs)/%.lc $(pi1) $(pi2)
	./$< $(word 2, $^); mv $(basename $(word 2, $^)) $@; \
	rm -f $(basename $(word 2, $^)).hs

$(tos_prs)/%.hs: lcc $(tis_prs)/%.lc
	./$< -h $(word 2, $^); mv $(basename $(word 2, $^)).hs $@

$(tos_grs)/%.hs: grules $(tis_grs)/%.txt
	./$<

# rules: lcc grules

lcc: $(shell find src -type f -not -name grules.hs)
	cd src; $(ghc) $@.hs -o ../$@

grules: src/grules.hs
	cd src; $(ghc) $@.hs -o ../$@

# rules: clean

clean:
	rm -rf lcc grules $(tos) hello_world \
	hello_world.hs; find test_inputs/programs/ -name "*.hs" -delete

clean_execs:
	rm $(tos_cps)/*

clean_hs_prs:
	rm $(hs_prs)

clean_grs:
	rm $(tos_grs)/*

# rules: test

test_cps:
	cd $(tos_cps); for f in $$(ls); do echo ""; echo $$f; echo ""; ./$$f; done
