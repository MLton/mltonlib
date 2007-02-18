(*
** 2007 February 18
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** $Id$
*)
signature BUFFER =
   sig
      type 'a t
      val empty: unit -> 'a t
      val subOpt: 'a t * int -> 'a option
      val sub: 'a t * int -> 'a
      val push: 'a t * 'a -> int
      val free: 'a t * int -> unit
   end
