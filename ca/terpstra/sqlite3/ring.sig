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
signature RING =
   sig
      (* handle to a link in the ring *)
      type 'a t
      
      (* Create a new ring with just this one element *)
      val new: 'a -> 'a t
      
      (* Add a value to the ring, get a handle to the link *)
      val add: 'a * 'a t -> 'a t
      
      (* Remove a link from the ring, it is in a new ring *)
      val remove: 'a t -> unit
      
      (* Run methods over all links in the ring *)
      val fold: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
      val app: ('a -> unit) -> 'a t -> unit
      (* val map: ('a -> 'b) -> 'a t -> 'b t *)
      
      (* Retrieve the value in this link *)
      val get: 'a t -> 'a
   end
