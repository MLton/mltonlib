$(info $(patsubst %.mlb,%,$(wildcard tests/*.mlb)))
all : $(patsubst %.mlb,%,$(wildcard tests/*.mlb))

mlb-path-map = mlb-path-map
mlb-path-map : GNUmakefile
	echo 'APPLICATION $(PWD)' > $@
	echo 'MLTON_LIB $(MLTON_LIB)' >> $@
	echo 'SML_COMPILER mlton' >> $@

mlton-opts = -const 'Exn.keepHistory true'

include common.mk
-include tests/*.dep
