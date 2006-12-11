signature ORDERED = sig

   type t

   val < : t * t -> Bool.t
   (**
    * x < y = toInt x < toInt y
    *)
   val <= : t * t -> Bool.t
   (**
    * x <= y = toInt x <= toInt y
    *)
   val > : t * t -> Bool.t
   (**
    * x > y = toInt x > toInt y
    *)
   val >= : t * t -> Bool.t
   (**
    * x >= y = toInt x >= toInt y
    *)
   val == : t * t -> Bool.t
   (**
    * == (x, y) is true iff x equals y.
    *)
   val compare: t * t -> Order.t
end
