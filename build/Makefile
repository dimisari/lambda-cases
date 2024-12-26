
include Makefile.paths

# commands

ghc_flags := -no-keep-hi-files -no-keep-o-files
ghc = ghc $(ghc_flags) -i$(src) -o $@
mkd = @mkdir -p $(@D)

# prerequisites

common_prereqs := \
	$(shell find $(src) -type f -not -name lcc.hs -not -name grules.hs)
lcc_prereqs := $(src)/lcc.hs $(common_prereqs)
grs_prereqs := $(src)/grules.hs $(common_prereqs)

prereq2 = $(word 2, $^)
prereq2_no_ext = $(basename $(prereq2))

# files to create

hs_prs := $(patsubst %.lc, $(t_outs_prs)/%.hs, $(pr_names))
hs_grs := $(patsubst %.txt, $(t_outs_grs)/%.hs, $(gr_names))
execs := $(patsubst %.lc, $(t_outs_c_prs)/%.out, $(pr_names))

all: $(hs_prs) $(hs_grs) $(execs)

# haskell programs in $(hs_prs)

$(t_outs_prs)/%.hs: $(b_lcc) $(t_ins_prs)/%.lc
	$(mkd)
	lcc -h $(prereq2)
	mv $(prereq2_no_ext).hs $@

# haskell results of examples for particular grammar rules in $(hs_grs)

$(t_outs_grs)/%.hs: $(b_grs) $(t_ins_grs)/%.txt
	$(mkd)
	./$<

# executables in $(execs)

$(t_outs_c_prs)/%.out: $(b_lcc) $(t_ins_prs)/%.lc $(p_imp_cp_pr) $(p_imp_cp_ops)
	$(mkd)
	lcc $(prereq2)
	mv $(prereq2_no_ext) $@

$(p_imp_cp_pr):
	$(mkd)
	cp -u $(p_imp_pr) $@

$(p_imp_cp_ops):
	$(mkd)
	cp -u $(p_imp_ops) $@

# lambda-cases compiler

$(b_lcc): $(lcc_prereqs)
	$(ghc) $(src)/lcc.hs

# executable running grammar rule examples

$(b_grs): $(grs_prereqs)
	$(ghc) $(src)/$@.hs

# cleaning

clean:
	$(RM) -r $(b_lcc) $(b_grs) $(t_outs) $(hello) $(basename $(hello))
	find $(examples) -name "*.hs" -delete

clean_hello:
	$(RM) $(hello) $(basename $(hello))

# testing

test_hello:
	cp -u $(examples)/$(hello) .
	lcc $(hello)

