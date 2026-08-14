
include Makefile.paths

# commands

ghc_flags := -no-keep-hi-files -no-keep-o-files
ghc = ghc $(ghc_flags) -i$(src) -o $@
mkdir = @mkdir -p $(@D)

# prerequisites

common_prereqs := \
	$(shell find $(src) -type f -not -name lcc.hs -not -name grules.hs)
lcc_prereqs := $(src)/lcc.hs $(common_prereqs)
grs_prereqs := $(src)/grules.hs $(common_prereqs)

prereq2 = $(word 2, $^)
prereq2_no_ext = $(basename $(prereq2))

# rules

.PHONY: all
all:
	make examples_clean
	make test_all

.PHONY: install
install: $(lcc_exec) $(p_imp_cp_pr) $(p_imp_cp_ops)

# lambda-cases compiler

$(lcc_exec): $(lcc_prereqs)
	$(ghc) $(src)/lcc.hs

# predefined imports

$(p_imp_cp_pr): $(p_imp_pr)
	$(mkdir)
	cp -u $< $@

$(p_imp_cp_ops): $(p_imp_ops)
	$(mkdir)
	cp -u $< $@

$(test_hs_prog_pattern): $(lcc_exec) $(test_prog_pattern)
	$(mkdir)
	lcc -h $(prereq2)
	mv $(prereq2_no_ext).hs $@

$(test_hs_rule_pattern): $(rules_exec) $(test_ins_grs)/%.txt
	$(mkdir)
	./$< $(test_ins_grs) $(test_rules_out_dir)

$(test_exec_pattern): $(lcc_exec) $(test_prog_pattern) $(p_imp_cp_pr) $(p_imp_cp_ops)
	$(mkdir)
	lcc -c $(prereq2)
	mv $(prereq2_no_ext) $@

$(rules_exec): $(grs_prereqs)
	$(ghc) $(src)/$@.hs

# cleaning

.PHONY: clean
clean:
	$(RM) -r $(lcc_exec) $(rules_exec) $(test_outs)
	find $(examples) -name "*.hs" -delete

.PHONY: examples_clean
examples_clean:
	find $(examples) -name "*.hs" -delete

.PHONY: uninstall
uninstall:
	$(RM) -r $(lcc_exec) $(local_stuff)

# testing

.PHONY: test_all
test_all: $(test_hs_prog_files) $(hs_grs) $(execs)

.PHONY: test_haskell_progs
test_haskell_progs: $(test_hs_prog_files)

.PHONY: test_grammar_rules
test_grammar_rules: $(hs_grs)

.PHONY: test_executables
test_executables: $(execs)

.PHONY: test_rules_correct
test_rules_correct:
	grep -n --color=auto Err $(hs_grs)

.PHONY: run_executables
run_executables:
	for f in $(execs); do echo "$$f start\n"; ./$$f; echo "\n$$f finish\n" ; done
