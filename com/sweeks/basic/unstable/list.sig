signature LIST = sig

   datatype t = datatype List.t
   (**
    * SML's built-in list type.
    *)

   include SEQUENCE where type 'a t0 = 'a t


end
