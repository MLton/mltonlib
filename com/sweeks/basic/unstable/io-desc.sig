signature IO_DESC = sig

   structure Kind: sig
      type t

      val == : t * t -> Bool.t
      val device: t
      val dir: t
      val file: t
      val pipe: t
      val socket: t
      val symlink: t
      val tty: t
   end

   type t

   val hash: t -> Word.t
   val compare: t * t -> Order.t

end
