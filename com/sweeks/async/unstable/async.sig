signature ASYNC = sig
   val runHandlers: Unit.t -> Unit.t
   structure Deferred: sig
      type 'a t

      val upon: 'a t * ('a -> Unit.t) -> Unit.t
   end
   structure Ivar: sig
      type 'a t

      val fill: 'a t * 'a -> Unit.t
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

      val close: 'a t -> Unit.t
      val new: Unit.t -> 'a t
      val reader: 'a t -> 'a Stream.t
      val send: 'a t * 'a -> Unit.t
   end
end
