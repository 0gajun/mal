STEPS = step0_repl.ml

STEP_BINS = $(STEPS:%.ml=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

all: $(STEP_BINS)

dist: aml

mal: $(LAST_STEP_BIN)
	cp $< $@

$(STEP_BINS): %: %.ml
	ocamlopt $< -o $@

clean:
	rm -f $(STEP_BINS) mal mal_lib.* *.cmo *.cmx *.cmi *.o

stats: $(MODULES) stepA_mal.ml
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\(\*|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: env.ml core.ml stepA_mal.ml
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\(\*|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

.PHONY: all repl clean stats stats-lisp
