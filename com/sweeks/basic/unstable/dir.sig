signature DIR = sig

   structure Stream: sig
      type t

      val close: t -> unit
      val read: t -> string option
      val rewind: t -> unit
   end

   type t = string

   val changeTo: t -> unit
   val create: t -> unit
   val current: unit -> t
   val openStream: t -> Stream.t
   val remove: t -> unit

end
