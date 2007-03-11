(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open UnitTest RawMem.Type

   exception OutOfMem

   local
      val malloc = _import "malloc" : Word.t -> RawMem.Ptr.t ;
      val free = _import "free" : RawMem.Ptr.t Effect.t ;

      fun alloc s = let
         val p = malloc s
      in
         if RawMem.Ptr.null = p then raise OutOfMem else p
      end
   in
      fun withMem s = With.around (fn () => alloc s) free
   end
in
   unitTests
      (title "RawMem")

      (test (fn () => let
                   datatype t = A
                              | B of Int8.t * Int8.t * Word16.t
                              | C of Word32.t
                   val t =
                       iso (data (C0
                                  +` C1 (tuple (T int8 *` T int8 *` T word16))
                                  +` C1 word32))
                           (fn A => INL (INL ())
                             | B (i8, i8', w16) => INL (INR (i8 & i8' & w16))
                             | C w32 => INR w32,
                            fn INL (INL ()) => A
                             | INL (INR (i8 & i8' & w16)) => B (i8, i8', w16)
                             | INR w32 => C w32)
                in
                   verifyTrue (size t = 0w8)
                 ; With.for
                      (withMem (size t))
                      (fn m => let
                             fun tst v =
                                 verifyTrue (v = (RawMem.set t m v
                                                ; RawMem.get t m))
                          in
                             tst A
                           ; tst (B (0x12, 0x34, 0wx5678))
                           ; tst (C 0wxFEDCBA98)
                          end)
                end))

      $
end
