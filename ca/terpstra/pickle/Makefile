TARGETS = pickle

all:	$(TARGETS)

clean:
	rm -f *.grm.* *.lex.* *.dep $(TARGETS)
	
%.dep:	%.mlb
	echo -en "$(basename $@) $@:\t" > $@.tmp
	mlton -stop f $< | sed 's/^/	/;s/$$/ \\/' >> $@.tmp
	mv $@.tmp $@

%:	%.mlb
	mlton -output $@ $<

%.grm.sml %.grm.sig %.grm.desc:	%.grm
	mlyacc $<

%.lex.sml:	%.lex
	mllex $<

-include $(patsubst %.mlb,%.dep,$(wildcard *.mlb))
