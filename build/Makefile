
include Makefile.paths

# commands

ghc_flags := -no-keep-hi-files -no-keep-o-files
ghc = ghc $(ghc_flags) -i$(src) -o $@
mkdir = @mkdir -p $(@D)

# rules

.PHONY: all
all:
	make examples_clean
	make test_all
	make test_rules_correct

.PHONY: install
install: $(lcc_exec) $(predef_cp_files)

# lambda-cases compiler

$(lcc_exec): $(lcc_prereqs)
	$(ghc) $(src)/lcc.hs

# predefined imports

$(predef_cp_file_pattern): $(predef_file_pattern)
	$(mkdir)
	cp -u $< $@

# testing files

$(test_hs_prog_pattern): $(test_prog_pattern) $(lcc_exec)
	$(mkdir)
	lcc -h $<
	mv $(basename $<).hs $@

$(test_hs_rule_pattern): $(rules_exec) $(test_ins_grs)/%.txt
	$(mkdir)
	./$< $(test_ins_grs) $(test_rules_out_dir)

$(test_exec_pattern): $(test_prog_pattern) $(lcc_exec) $(predef_cp_files)
	$(mkdir)
	lcc -c $<
	mv $(basename $<) $@

$(rules_exec): $(grs_prereqs)
	$(ghc) $(src)/$@.hs

# testing

.PHONY: test_all
test_all: $(test_hs_prog_files) $(test_hs_rules_files) $(test_exec_files)

.PHONY: test_haskell_progs
test_haskell_progs: $(test_hs_prog_files)

.PHONY: test_grammar_rules
test_grammar_rules: $(test_hs_rules_files)

.PHONY: test_executables
test_executables: $(test_exec_files)

.PHONY: test_rules_correct
test_rules_correct:
	grep -rn --color=auto Err $(test_rules_out_dir)

.PHONY: run_executables
run_executables:
	for f in $(test_exec_files); do \
		echo "$$f start\n"; ./$$f; echo "\n$$f finish\n" ; \
	done

print-%: ; @echo '$* = $($*)'

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

