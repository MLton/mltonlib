TARGETS = $(patsubst %.test,%,$(wildcard *.test))

all:	$(TARGETS)
clean:
	rm -f *.sml $(TARGETS)

%.sml:	%.test
	cp ../sb.sig $@.tmp
	../pickle Test < $< >> $@.tmp
	mv $@.tmp $@

%:	%.sml
	mlton $<
