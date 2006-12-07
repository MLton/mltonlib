signature IO_DESC = sig

   structure Kind: sig
      type t

      val == : t * t -> bool
      val device: t
      val dir: t
      val file: t
      val pipe: t
      val socket: t
      val symlink: t
      val tty: t
   end

   type t

   val hash: t -> word
   val compare: t * t -> order

end
