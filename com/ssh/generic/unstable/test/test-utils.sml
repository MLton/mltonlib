(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Some tests need the structural/sharing equality {Seq}. *)
structure Generic = struct
   open Generic
   local
      structure Open = WithSeq (Open)
      structure Closed = CloseCases (Open)
      structure Extra = WithExtra (structure Open = Open open Open Closed)
   in
      val seq = Open.seq
      open Extra
   end
end

(* A simplistic graph for testing with cyclic data. *)
structure Graph :> sig
   type 'a t
   val t : 'a Generic.Rep.t -> 'a t Generic.Rep.t
   val intGraph1 : Int.t t
end = struct
   datatype 'a t = VTX of 'a * 'a t List.t Ref.t

   local
      open Tie Generic
      val vtx = C "VTX"
   in
      fun t a =
          fix Y (fn aT =>
                    iso (data (C1 vtx (tuple2 (a, refc (list aT)))))
                        (fn VTX ? => ?, VTX))
   end

   fun arcs (VTX (_, r)) = r

   val intGraph1 = let
      val a = VTX (1, ref [])
      val b = VTX (2, ref [])
      val c = VTX (3, ref [])
      val d = VTX (4, ref [])
      val e = VTX (5, ref [])
      val f = VTX (6, ref [])
   in
      arcs a := [b, d]
    ; arcs b := [c, e]
    ; arcs c := [a, f]
    ; arcs d := [f]
    ; arcs e := [d]
    ; arcs f := [e]
    ; a
   end
end

(* A contrived recursive exception constructor for testing with cyclic data. *)
structure ExnArray :> sig
   exception ExnArray of Exn.t Array.t
   val exnArray1 : Exn.t Array.t
end = struct
   open Generic

   exception ExnArray of Exn.t Array.t
   val () = regExn1' "ExnArray" (array exn) ExnArray (fn ExnArray ? => ?)

   val exnArray1 = Array.fromList [Empty]
   val () = Array.update (exnArray1, 0, ExnArray exnArray1)
end
