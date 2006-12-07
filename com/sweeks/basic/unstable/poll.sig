signature POLL = sig

   structure Desc: sig
      type t

      exception Poll
      
      val addIn: t -> t
      (**
       * may raise Poll.
       *)
      val addOut: t -> t
      (**
       * may raise Poll.
       *)
      val addPri: t -> t
      (**
       * may raise Poll.
       *)
      val ofIO: IoDesc.t -> t option
      val toIO: t -> IoDesc.t
   end

   structure Info: sig
      type t

      val isIn: t -> bool
      val isOut: t -> bool
      val isPri: t -> bool
      val toDesc: t -> Desc.t
   end

   val poll: Desc.t list * Time.t option -> Info.t list

end
