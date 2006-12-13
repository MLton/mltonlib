(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
EXPORT: $(shell mlton -stop f lib.mlb)
	mlton -show-basis EXPORT -stop tc lib.mlb

TAGS: *.fun *.sig *.sml
	etags *.{fun,sig,sml}
