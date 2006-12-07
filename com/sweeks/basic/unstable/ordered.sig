signature ORDERED = sig

   type t

   val < : t * t -> bool
   (**
    * x < y = toInt x < toInt y
    *)
   val <= : t * t -> bool
   (**
    * x <= y = toInt x <= toInt y
    *)
   val > : t * t -> bool
   (**
    * x > y = toInt x > toInt y
    *)
   val >= : t * t -> bool
   (**
    * x >= y = toInt x >= toInt y
    *)
   val == : t * t -> bool
   (**
    * == (x, y) is true iff x equals y.
    *)
   val compare: t * t -> Order.t
end
