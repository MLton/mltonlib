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
      val ofIO: IoDesc.t -> t Option.t
      val toIO: t -> IoDesc.t
   end

   structure Info: sig
      type t

      val isIn: t -> Bool.t
      val isOut: t -> Bool.t
      val isPri: t -> Bool.t
      val toDesc: t -> Desc.t
   end

   val poll: Desc.t List.t * Time.t Option.t -> Info.t List.t

end
