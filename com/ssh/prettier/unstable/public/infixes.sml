(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Global operator precedence table ==
 *
 * We assume here the modified precedence table of the Extended Basis library.
 *)

(* ************************************************************************** *)
(*       !  Prettier !                                                        *)
(* ========================================================================== *)
infixr 7 ! <^> <+>   !
(* ========================================================================== *)
infixr 6 ! <$> <$$>  !
         ! </> <//>  !
(* ************************************************************************** *)

nonfix ! (* We just used ! above as a visual separator. *)
