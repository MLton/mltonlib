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
(* Stolen from Vesa, and treated like assembler.
 * See: http://mlton.org/Fold if you like pain.
 *)
signature FOLD =
   sig
      type ('a, 'b, 'c, 'd) t
      type ('a1, 'a2, 'b, 'c, 'd) step0
      type ('a11, 'a12, 'a2, 'b, 'c, 'd) step1
      
      val fold: 'a * ('b -> 'c) -> ('a, 'b, 'c, 'd) t
      val step0: ('a1 -> 'a2) -> ('a1, 'a2, 'b, 'c, 'd) step0
      val step1: ('a11 * 'a12 -> 'a2) -> ('a11, 'a12, 'a2, 'b, 'c, 'd) step1
   end
