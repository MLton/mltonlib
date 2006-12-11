signature PROCESS = sig

   structure Status: sig
      type t

      val failure: t
      val success: t
      val isSuccess: t -> Bool.t
   end

   val atExit: (Unit.t -> Unit.t) -> Unit.t
   val exit: Status.t -> 'a
   val getEnv: String.t -> String.t Option.t
   val sleep: Time.t -> Unit.t
   val system: String.t -> Status.t
   val terminate: Status.t -> 'a

end
