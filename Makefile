
# paths

is=inputs
os=outputs
tis=test/$(is)
tos=test/$(os)
esc_tos=test\/$(os)

cps=compiled_progs
grs=grammar_rules
prs=programs

pis=src/PredefImports/
pi1=$(pis)/Predefined.hs
pi2=$(pis)/OpsInHaskell.hs

tos_cps=$(tos)/$(cps)
esc_tos_cps=$(esc_tos)\/$(cps)

tos_prs=$(tos)/$(prs)
esc_tos_prs=$(esc_tos)\/$(prs)

tos_grs=$(tos)/$(grs)
esc_tos_grs=$(esc_tos)\/$(grs)

tis_prs=$(tis)/$(prs)
tis_grs=$(tis)/$(grs)

#commands

ghc=ghc -no-keep-hi-files -no-keep-o-files

mkd=@mkdir -p $(@D)

hs_prs=$(shell ls $(tis_prs) | sed "s/\(.*\).lc/$(esc_tos_prs)\/\1.hs/g")
hs_grs=$(shell ls $(tis_grs) | sed "s/\(.*\).txt/$(esc_tos_grs)\/\1.hs/g")
execs=$(shell ls $(tis_prs) | sed "s/\(.*\).lc/$(esc_tos_cps)\/\1.out/g")

# rules: all .out .hs

all: $(hs_prs) $(hs_grs) $(execs)

$(tos_cps)/%.out: lcc $(tis_prs)/%.lc $(pi1) $(pi2)
	$(mkd)
	./$< $(word 2, $^)
	mv $(basename $(word 2, $^)) $@
	rm -f $(basename $(word 2, $^)).hs

$(tos_prs)/%.hs: lcc $(tis_prs)/%.lc
	$(mkd); ./$< -h $(word 2, $^); mv $(basename $(word 2, $^)).hs $@

$(tos_grs)/%.hs: grules $(tis_grs)/%.txt
	$(mkd); ./$<

# rules: lcc grules

lcc: $(shell find src -type f -not -name grules.hs)
	cd src; $(ghc) $@.hs -o ../$@

grules: src/grules.hs
	cd src; $(ghc) $@.hs -o ../$@

# rules: clean

clean:
	rm -rf lcc grules hello_world $(tos) hello_world.hs
	find $(tis_prs) -name "*.hs" -delete

clean_execs:
	rm $(tos_cps)/*

clean_hs_prs:
	rm $(hs_prs)

clean_grs:
	rm $(tos_grs)/*

# rules: test

test_cps:
	cd $(tos_cps); for f in $$(ls); do echo ""; echo $$f; echo ""; ./$$f; done
