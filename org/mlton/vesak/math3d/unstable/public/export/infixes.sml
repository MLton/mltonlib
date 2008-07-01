(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Infix Operators ==
 *
 * The choice of symbols has been made based on their appearance:
 *
 *     Symbol | Type       | Mnemonic
 *    --------+------------+----------------------------------------------
 *            | Scalar     | Same as usual
 *          # | Matrix     | Rows and columns of scalars
 *          ! | Quaternion | Consists of two parts: a scalar and a vector
 *          @ | Rot        |
 *          % | RBT        |
 *          | | Vector     | A single row or column of scalars
 *
 * So, for example,
 *
 *> m #*| v |* s *! q
 *
 * means to compute the product of a matrix {m}, a vector {v}, a scalar
 * {s}, and a quaternion {q}.
 *
 * Note that scalar operators have higher precedence in the Extended
 * Basis library.
 *)

infix 7 !*! !* *! !/! !/ /!
        #*! !*#
        #*# #* *# #/# #/ /#
        #*| |*#
        @*@
        %*% %*@ @*% %*| |*%
        |*| |* *| |/| |/ /|

infix 6 !+! !+ +! !-! !- -!
        #+# #+ +# #-# #- -#
        |+| |+ +| |-| |- -|
