(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic pickle/unpickle function.
 *)
signature PICKLE = sig
   structure Pickle : OPEN_GENERIC_REP

   val pickle : ('a, 'x) Pickle.t -> (Char.t, 'b) Writer.t -> ('a, 'b) Writer.t
   (** Extracts the pickling function. *)

   val unpickle : ('a, 'x) Pickle.t -> (Char.t, 'b) Reader.t -> ('a, 'b) Reader.t
   (** Extracts the unpickling function. *)
end

signature PICKLE_GENERIC = sig
   include OPEN_GENERIC PICKLE
   sharing Rep = Pickle
end

signature WITH_PICKLE_DOM = sig
   include OPEN_GENERIC EQ DUMMY HASH TYPE_INFO
   sharing Rep = Eq = Dummy = Hash = TypeInfo
end
