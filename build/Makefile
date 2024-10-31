
include Makefile.paths

# commands

ghc := ghc -no-keep-hi-files -no-keep-o-files

mkd = @mkdir -p $(@D)

# prerequisites

lcc_prereqs := $(shell find $(src) -type f -not -name grules.hs)
grs_prereqs := $(shell find $(src) -type f -not -name lcc.hs)

# files to create

hs_prs=$(shell ls $(t_ins_prs) | sed "s/\(.*\).lc/$(esc_t_outs_prs)\/\1.hs/g")
hs_grs=$(shell ls $(t_ins_grs) | sed "s/\(.*\).txt/$(esc_t_outs_grs)\/\1.hs/g")
execs=$(shell ls $(t_ins_prs) | sed "s/\(.*\).lc/$(esc_t_outs_c_prs)\/\1.out/g")

all: $(hs_prs) $(hs_grs) $(execs)

# haskell programs in $(hs_prs)

$(t_outs_prs)/%.hs: $(b_lcc) $(t_ins_prs)/%.lc
	$(mkd); ./$< -h $(word 2, $^); mv $(basename $(word 2, $^)).hs $@

# haskell results of examples for particular grammar rules in $(hs_grs)

$(t_outs_grs)/%.hs: $(b_grs) $(t_ins_grs)/%.txt
	$(mkd); ./$<

# executables in $(execs)

$(t_outs_c_prs)/%.out: $(b_lcc) $(t_ins_prs)/%.lc $(p_imp_pr) $(p_imp_ops)
	$(mkd)
	./$< $(word 2, $^)
	mv $(basename $(word 2, $^)) $@
	rm -f $(basename $(word 2, $^)).hs

# lambda-cases compiler

$(b_lcc): $(lcc_prereqs)
	$(ghc) -i$(src) $(src)/$@.hs -o $@

# executable running grammar rule examples

$(b_grs): $(grs_prereqs)
	$(ghc) -i$(src) $(src)/$@.hs -o $@

# cleaning

clean:
	rm -rf $(b_lcc) $(b_grs) $(hw) $(t_outs) $(hw_hs)
	find $(t_ins_prs) -name "*.hs" -delete

clean_execs:
	rm $(t_outs_c_prs)/*

clean_hs_prs:
	rm $(hs_prs)

clean_grs:
	rm $(t_outs_grs)/*

# testing

test_c_prs:
	cd $(t_outs_c_prs); for f in $$(ls); do echo ""; echo $$f; echo ""; ./$$f; done
