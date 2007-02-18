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
structure Buffer :> BUFFER =
   struct
      (* Use a smaller int type to allow representation optimization *)
      structure IntX = Int30
      type intx = IntX.int
      
      datatype 'a free = FREE of intx | FULL of 'a
      type 'a t = { buf: 'a free array ref, free: int ref }
      
      fun empty () = {
         buf = ref (Array.tabulate (32, fn i => FREE (IntX.fromInt (i-1)))), 
         free = ref 31 }
      
      fun subOpt ({ buf, free=_ }, i) = 
        if i >= Array.length (!buf) then NONE else
        case Array.sub (!buf, i) of
           FREE _ => NONE
         | FULL x => SOME x
      
      fun sub (b, i) = valOf (subOpt (b, i))
      
      fun double { buf, free } =
         let
            val oldlen = Array.length (!buf)
            val newlen = oldlen  * 2
            fun get i = if i = oldlen then FREE (IntX.fromInt (!free)) else
                        if i > oldlen then FREE (IntX.fromInt (i-1)) else
                        Array.sub (!buf, i)
            val () = buf := Array.tabulate (newlen, get)
         in
            free := newlen-1
         end
      
      fun push (b as { buf, free }, v) = (
         if !free = ~1 then double b else ();
         case Array.sub (!buf, !free) of
            FULL _ => raise Fail "Buggy free list in Buffer.push"
          | FREE n => (
               Array.update (!buf, !free, FULL v);
               !free before free := IntX.toInt n))
      
      fun free ({ buf, free }, i) = (
(*
         case Array.sub (!buf, i) of
            FREE _ => raise Fail "Free of unused space in Buffer.free"
          | FULL _ =>
*)
         Array.update (!buf, i, FREE (IntX.fromInt (!free)));
         free := i)
   end
