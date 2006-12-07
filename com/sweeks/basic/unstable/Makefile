EXPORT: $(shell mlton -stop f lib.mlb)
	mlton -show-basis EXPORT -stop tc lib.mlb

TAGS: *.fun *.sig *.sml
	etags *.{fun,sig,sml}
