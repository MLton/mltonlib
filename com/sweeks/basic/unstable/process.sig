signature PROCESS = sig

   structure Status: sig
      type t

      val failure: t
      val success: t
      val isSuccess: t -> bool
   end

   val atExit: (unit -> unit) -> unit
   val exit: Status.t -> 'a
   val getEnv: string -> string option
   val sleep: Time.t -> unit
   val system: string -> Status.t
   val terminate: Status.t -> 'a

end
