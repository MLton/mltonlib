.PHONY : all check clean help

help :
	@echo "Targets:"
	@echo "    all      Build the unit test program"
	@echo "    check    Build and run the unit test and report results"
	@echo "    clean    Removes generated files"
	@echo "    help     You are reading it"

all : tests

-include $(wildcard *.dep)

tests : tests.mlb Makefile
	mlton -stop f $<            				  \
	  | sed $$'s#\r##g'                                       \
	  | awk 'BEGIN { srcs = "" ; printf "$@ :" }              \
	               { srcs = srcs $$1 ":\n" ; printf " " $$1 } \
	           END { printf "\n" srcs }'                      \
	  > $@.dep
	mlton -verbose 1 -output $@ $<

check : tests
	./tests > test/first-5000.out
	@if diff test/first-5000.ok test/first-5000.out; then \
	  echo "Passed."; \
	else \
	  echo "FAILED!"; \
	fi

clean :
	rm -f tests tests.dep test/first-5000.out
