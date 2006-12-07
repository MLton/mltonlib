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

   val format: t * string -> string
   val hour: t -> int
   val isDst: t -> bool option
   val localOffset: unit -> Time.t
   val make: {hour: int,
              minute: int,
              month: Month.t,
              monthDay: int,
              offset: Time.t option,
              second: int,
              year: int} -> t
   val minute: t -> int
   val month: t -> Month.t
   val monthDay: t -> int
   val offset: t -> Time.t option
   val ofString: string -> t option
   (**
    * ofString is like the basis library's Date.fromString, except that it
    * returns None if the entire string is not consumed.
    *)
   val ofTimeLocal: Time.t -> t
   val ofTimeUniv: Time.t -> t
   val scanner: char seq -> (t * char seq) option
   val second: t -> int
   val toString: t -> string
   val toTime: t -> Time.t
   val weekDay: t -> WeekDay.t
   val year: t -> int
   val yearDay: t -> int

end
