signature BOOL = sig

   datatype t = datatype Bool.t

   val not: t -> t
   (**
    * not true = false.  not false = true.
    *)     

end
