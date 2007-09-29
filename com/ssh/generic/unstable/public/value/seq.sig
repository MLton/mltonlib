(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a structural equality predicate.
 *
 * By default, the semantics of the predicate corresponds to the equality
 * relation that can be achieved through pickling and unpickling.  While
 * the identities of mutable objects need not be equal, it is required that
 * there is a one-to-one correspondence between the identities of the
 * mutable objects of the compared values.
 *
 * This equality predicate is unlikely to be useful in most applications.
 * However, this is useful for testing the correctness of pickling and
 * other similar generics.
 *)
signature SEQ = sig
   structure SeqRep : OPEN_REP

   val seq : ('a, 'x) SeqRep.t -> 'a BinPr.t
   (** Extracts the equality predicate. *)

   val notSeq : ('a, 'x) SeqRep.t -> 'a BinPr.t
   (** {notSeq t = not o seq t} *)

   val withSeq : 'a BinPr.t -> ('a, 'x) SeqRep.t UnOp.t
   (** Functionally updates the equality predicate. *)
end

signature SEQ_CASES = sig
   include CASES SEQ
   sharing Open.Rep = SeqRep
end

signature WITH_SEQ_DOM = HASH_CASES
