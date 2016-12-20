STEPS = step0_repl.ml step1_read_print.ml
MODULES = types.ml reader.ml printer.ml
MAL_LIB = mal_lib.cmxa

STEP_BINS = $(STEPS:%.ml=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

all: $(STEP_BINS)

dist: mal

mal: $(LAST_STEP_BIN)
	cp $< $@

$(MAL_LIB): $(MODULES)
	ocamlfind ocamlopt -package pcre -a $(MODULES) -o $@

$(STEP_BINS): %: %.ml $(MAL_LIB)
	ocamlfind ocamlopt -linkpkg -thread -package core,pcre $(MAL_LIB) $< -o $@

clean:
	rm -f $(STEP_BINS) mal mal_lib.* *.cmo *.cmx *.cmi *.o

stats: $(MODULES) stepA_mal.ml
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\(\*|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: env.ml core.ml stepA_mal.ml
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\(\*|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

.PHONY: all repl clean stats stats-lisp
