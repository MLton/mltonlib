mlton ?= mlton
mltonlib-common-mk = $(lastword $(MAKEFILE_LIST))

% : %.mlb $(mltonlib-common-mk) $(mlb-deps) | $(mlb-path-map)
	$(mlton) -stop f -mlb-path-map $(mlb-path-map) $<	  \
	  | sed $$'s#\r##g'					  \
	  | awk 'BEGIN { srcs = "" ; printf "$@ :" }		  \
		       { srcs = srcs $$1 ":\n" ; printf " " $$1 } \
		   END { printf "\n" srcs }'			  \
	  > $@.dep
	$(mlton) @MLton $(mlton-args) -- -mlb-path-map $(mlb-path-map) \
	         $(mlton-opts) $<

-include $(wildcard *.dep)
