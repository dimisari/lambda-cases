
include Makefile.paths

# commands

ghc_flags := -no-keep-hi-files -no-keep-o-files
ghc = ghc $(ghc_flags) -i$(src) -o $@
mkdir = @mkdir -p $(@D)

# rules

.PHONY: lcc
lcc: $(lcc_exec)

.PHONY: all
all:
	make examples_clean
	make test_dot_trees
	make test_png_trees
	make test_haskell_progs
	make test_grammar_rules
	make test_executables
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

$(test_dot_tree_pattern): $(test_prog_pattern) $(lcc_exec)
	$(mkdir)
	lcc -d $<
	mv $(basename $<).dot $@

$(test_png_tree_pattern): $(test_prog_pattern) $(lcc_exec)
	$(mkdir)
	lcc -p $<
	mv $(basename $<).png $@

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

.PHONY: test_dot_trees
test_dot_trees: $(test_dot_tree_files)

.PHONY: test_png_trees
test_png_trees: $(test_png_tree_files)

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
	make examples_clean

.PHONY: examples_clean
examples_clean:
	find $(examples) -name "*.dot" -delete -name "*.png" -delete -name "*.hs" -delete

.PHONY: uninstall
uninstall:
	$(RM) -r $(lcc_exec) $(local_stuff)

