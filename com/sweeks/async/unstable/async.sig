signature ASYNC = sig
   exception Closed
   exception Full
   val runHandlers: Unit.t -> Unit.t
   structure Deferred: sig
      type 'a t

      val >>= : 'a t * ('a -> 'b t) -> 'b t
      val return: 'a -> 'a t
      val upon: 'a t * ('a -> Unit.t) -> Unit.t
   end
   structure Ivar: sig
      type 'a t

      val fill: 'a t * 'a -> Unit.t (* may raise Full *)
      val new: Unit.t -> 'a t
      val read: 'a t -> 'a Deferred.t
   end
   structure Stream: sig
      type 'a t

      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b Deferred.t
      val for: 'a t * ('a -> Unit.t) -> Unit.t Deferred.t
      val map: 'a t * ('a -> 'b) -> 'b t
      val read: 'a t -> ('a * 'a t) Option.t Deferred.t
      val recur:
         'a t * 'b * ('b -> Unit.t) * ('a * 'b * ('b -> Unit.t) -> Unit.t)
         -> Unit.t
   end
   structure Multicast: sig
      type 'a t

      val close: 'a t -> Unit.t (* may raise Closed *)
      val listen: 'a t -> 'a Stream.t
      val new: Unit.t -> 'a t
      val send: 'a t * 'a -> Unit.t (* may raise Closed *)
   end
   structure Event: sig
      type 'a t

      val >>= : 'a t * ('a -> 'b t) -> 'b t
      val always: 'a -> 'a t
      val any: 'a t List.t -> 'a t
      val commit: 'a t -> 'a Deferred.t
      val never: Unit.t -> 'a t
      val return: 'a -> 'a t
   end
   structure Channel: sig
      type 'a t

      val give: 'a t * 'a -> Unit.t Event.t
      val new: Unit.t -> 'a t
      val take: 'a t -> 'a Event.t
   end
   structure Mailbox: sig
      type 'a t

      val new: Unit.t -> 'a t
      val send: 'a t * 'a -> Unit.t
      val take: 'a t -> 'a Event.t
   end
end
