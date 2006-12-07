structure SysError: SYS_ERROR = struct

   open OS

   structure Exn = struct
      type error = syserror
      type t = string * error Basis.option

      exception E = OS.SysErr

      val message = fst
      val error = Option.ofBasis o snd
   end

   type t = Exn.error

   val == = op =
   val message = errorMsg
   val name = errorName
   val ofName = Option.ofBasis o syserror

end
