(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   (* <--- SML/NJ workaround *)
   open TopLevel
   infix +`
   (* SML/NJ workaround --> *)

   open Generic
in
   (* A simplistic graph for testing with cyclic data. *)
   structure Graph :> sig
      type 'a t
      val t : ('a, 'x) Open.Rep.t -> 'a t Rep.t
      val intGraph1 : Int.t t
   end = struct
      datatype 'a v = VTX of 'a * 'a t
      withtype 'a t = 'a v List.t Ref.t

      local
         val cVTX = C "VTX"
         fun withT aV = refc (list aV)
      in
         fun v a = Tie.fix Y (fn aV =>
             data' (C1 cVTX (tuple2 (a, withT aV)))
                   (fn VTX ? => ?, VTX))
         fun t a = withT (v a)
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
       ; ref [a, b, c, d, e, f]
      end
   end

   (* A contrived recursive exception constructor for testing with cyclic
    * data. *)
   structure ExnArray :> sig
      exception ExnArray of Exn.t Array.t
      val exnArray1 : Exn.t Array.t
   end = struct
      exception ExnArray of Exn.t Array.t
      val () = regExn1' "ExnArray" (array exn) ExnArray (fn ExnArray ? => ?)

      val exnArray1 = Array.fromList [Empty]
      val () = Array.update (exnArray1, 0, ExnArray exnArray1)
   end

   (* A simple binary tree. *)
   structure BinTree :> sig
      datatype 'a t = LF | BR of 'a t * 'a * 'a t
      val t : ('a, 'x) Open.Rep.t -> 'a t Rep.t
   end = struct
      datatype 'a t = LF | BR of 'a t * 'a * 'a t

      local
         val cLF = C "LF"
         val cBR = C "BR"
      in
         fun t a = Tie.fix Y (fn aT =>
             data' (C0 cLF +` C1 cBR (tuple3 (aT, a, aT)))
                   (fn LF => INL () | BR ? => INR ?,
                    fn INL () => LF | INR ? => BR ?))
      end
   end

   structure Lambda :> sig
      structure Id : sig
         type t = String.t
         val t : t Rep.t
      end

      datatype 't f =
         FUN of Id.t * 't
       | APP of 't Sq.t
       | REF of Id.t

      datatype t = IN of t f
      val out : t -> t f

      val f : 't Rep.t -> 't f Rep.t
      val t : t Rep.t
   end = struct
      structure Id = struct
         type t = String.t
         val t = string
      end

      datatype 't f =
         FUN of Id.t * 't
       | APP of 't Sq.t
       | REF of Id.t

      datatype t = IN of t f
      fun out (IN ?) = ?

      local
         val cFUN = C "FUN"
         val cAPP = C "APP"
         val cREF = C "REF"
      in
         fun f t =
             data' (C1 cFUN (tuple2 (Id.t, t))
                 +` C1 cAPP (sq t)
                 +` C1 cREF Id.t)
                   (fn FUN ? => INL (INL ?)
                     | APP ? => INL (INR ?)
                     | REF ? => INR ?,
                    fn INL (INL ?) => FUN ?
                     | INL (INR ?) => APP ?
                     | INR ? => REF ?)
      end

      local
         val cIN = C "IN"
      in
         val t = Tie.fix Y (fn t => data' (C1 cIN (f t)) (out, IN))
      end
   end
end
