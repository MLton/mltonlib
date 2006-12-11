signature DATE = sig

   structure Month: sig
      type t

      val april: t
      val august: t
      val december: t
      val february: t
      val january: t
      val july: t
      val june: t
      val march: t
      val may: t
      val november: t
      val october: t
      val september: t
   end

   structure WeekDay: sig
      type t

      val friday: t
      val monday: t
      val saturday: t
      val sunday: t
      val thursday: t
      val tuesday: t
      val wednesday: t
   end

   type t

   val format: t * String.t -> String.t
   val hour: t -> Int.t
   val isDst: t -> Bool.t Option.t
   val localOffset: Unit.t -> Time.t
   val make: {hour: Int.t,
              minute: Int.t,
              month: Month.t,
              monthDay: Int.t,
              offset: Time.t Option.t,
              second: Int.t,
              year: Int.t} -> t
   val minute: t -> Int.t
   val month: t -> Month.t
   val monthDay: t -> Int.t
   val offset: t -> Time.t Option.t
   val ofString: String.t -> t Option.t
   (**
    * ofString is like the basis library's Date.fromString, except that it
    * returns None if the entire string is not consumed.
    *)
   val ofTimeLocal: Time.t -> t
   val ofTimeUniv: Time.t -> t
   val scanner: Char.t Seq.t -> (t * Char.t Seq.t) Option.t
   val second: t -> Int.t
   val toString: t -> String.t
   val toTime: t -> Time.t
   val weekDay: t -> WeekDay.t
   val year: t -> Int.t
   val yearDay: t -> Int.t

end
