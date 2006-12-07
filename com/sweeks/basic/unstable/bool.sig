signature BOOL = sig

   datatype t = datatype Bool.bool

   val not: t -> t
   (**
    * not true = false.  not false = true.
    *)     

end
