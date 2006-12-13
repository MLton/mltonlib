(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature FILE = sig

   structure AccessMode: sig
      type t

      val exec: t
      val read: t
      val write: t
   end

   structure Id: sig
      type t

      val == : t * t -> Bool.t
      val compare: t * t -> Order.t
      val hash: t -> Word.t
   end

   type t = String.t
   (**
    * The type of files.  It's an alias for string.
    *)

   val canAccess: t * AccessMode.t List.t -> Bool.t
   val id: t -> Id.t
   val modTime: t -> Time.t
   val openIn: t -> In.t
   (**
    * openIn f opens file f for reading.
    *)
   val openOut: t -> Out.t
   val rename: {new: String.t, old: String.t} -> Unit.t
   val remove: t -> Unit.t
   val setTime: t * Time.t -> Unit.t
   val size: t -> Position.t
   val temp: {prefix: String.t, suffix: String.t} -> t * Out.t
   (**
    * openIn f opens file f for writing.
    *)
   val withIn: t * (In.t -> 'a) -> 'a
   (**
    * withIn (f, g) opens f for reading, runs g on the resulting instream,
    * closes the instream, and returns the result of g.  withIn uses Exn.finally
    * to ensure that the instream is closed.
    *)
   val withOut: t * (Out.t -> 'a) -> 'a
   (**
    * withOut (f, g) opens f for writing, runs g on the resulting outstream,
    * closes the outstream, and returns the result of g.  withOut uses
    * Exn.finally to ensure that the outstream is closed.
    *)

end
