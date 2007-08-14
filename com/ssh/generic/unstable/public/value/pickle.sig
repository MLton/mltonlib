(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic pickle/unpickle function.
 *
 * WARNING: At the moment, pickles are not portable.
 *)
signature PICKLE = sig
   structure Pickle : OPEN_REP

   val pickle : ('a, 'x) Pickle.t -> (Char.t, 'b) Writer.t -> ('a, 'b) Writer.t
   (** Extracts the pickling function. *)

   val unpickle : ('a, 'x) Pickle.t -> (Char.t, 'b) Reader.t -> ('a, 'b) Reader.t
   (** Extracts the unpickling function. *)
end

signature PICKLE_CASES = sig
   include OPEN_CASES PICKLE
   sharing Rep = Pickle
end

signature WITH_PICKLE_DOM = sig
   include OPEN_CASES DATA_REC_INFO EQ HASH SOME TYPE_INFO
   sharing Rep = DataRecInfo = Eq = Hash = Some = TypeInfo
end
