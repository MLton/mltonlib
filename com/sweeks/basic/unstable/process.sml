structure Process: PROCESS = struct

   structure Status = struct
      open OS.Process

      type t = status
   end

   open OS.Process

   val getEnv = Option.ofBasis o getEnv

end
