signature SYS_ERROR = sig
   
   structure Exn: sig
      type t
      type error

      exception E of t

      val message: t -> string
      val error: t -> error option
   end
   
   type t
   sharing type t = Exn.error
      
   val == : t * t -> bool
   val message: t -> string
   val name: t -> string
   val ofName: string -> t option

end
