signature DIR = sig

   structure Stream: sig
      type t

      val close: t -> Unit.t
      val read: t -> String.t Option.t
      val rewind: t -> Unit.t
   end

   type t = String.t

   val changeTo: t -> Unit.t
   val create: t -> Unit.t
   val current: Unit.t -> t
   val openStream: t -> Stream.t
   val remove: t -> Unit.t

end
