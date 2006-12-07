structure Date: DATE = struct

   open Date

   type t = date

   structure Month = struct
      datatype t = datatype month

      val april = Apr
      val august = Aug
      val december = Dec
      val february = Feb
      val january = Jan
      val july = Jul
      val june = Jun
      val march = Mar
      val may = May
      val november = Nov
      val october = Oct
      val september = Sep
   end

   structure WeekDay = struct
      datatype t = datatype weekday

      val friday = Fri
      val monday = Mon
      val saturday = Sat
      val sunday = Sun
      val thursday = Thu
      val tuesday = Tue
      val wednesday = Wed
   end

   fun format (d, s) = fmt s d

   val monthDay = day

   fun make {hour, minute, month, monthDay, offset, second, year} =
      date {day = monthDay,
            hour = hour,
            minute = minute,
            month = month,
            offset = Option.toBasis offset,
            second = second,
            year = year}

   val isDst = Option.ofBasis o isDst

   val offset = Option.ofBasis o offset

   val scanner = Scanner.ofBasis scan

   fun ofString s = Scanner.scanString (scanner, s)

   val ofTimeLocal = fromTimeLocal

   val ofTimeUniv = fromTimeUniv

end
