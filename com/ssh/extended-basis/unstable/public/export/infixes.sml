(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * == Operator Precedence Table ==
 *
 * The precendences of most Basis Library operators are modified here to
 * accommodate the sectioning, application, and piping operators:
 *
 *    * / div mod      are +1 from Basis Library
 *    + - ^            are +1 from Basis Library
 *    :: @             are +1 from Basis Library
 *    = <> > >= < <=   are +1 from Basis Library
 *    :=               is  -2 from Basis Library
 *
 * The changed precedences should not cause (major) problems as the
 * relative precedences of only a couple of operators are changed.  If you
 * do encounter problems, it would nice to hear about them.
 *
 * See
 *
 *    http://mlton.org/InfixingOperators
 *
 * for a discussion of fixity declarations in SML.
 *
 * While one should think twice before introducing new infix declarations
 * at the top-level, a carefully chosen set of top-level infix
 * declarations can make programs much more aesthetically pleasing and
 * readable.
 *
 * Categories used below:
 *
 *    Basis     Operators from the Standard ML Basis Library
 *    B & P     Bitwise and Predicate operators
 *    P & S     Piping and Sectioning operators
 *    Monadic   Monadic operators
 *    Ticked    Ticked operators
 *    Misc      Uncategorized
 *
 * See also: <top-level.sml>
 *)

(* ************************************************************************** *)
(*       ! Basis   ! B & P   ! P & S ! Monadic ! Ticked ! Misc                *)
(* ========================================================================== *)
infix  8 !  * div  ! >>   << !       !         !   *`   !
         !  / mod  !   ~>>   !       !         !   /`   !
(* ========================================================================== *)
infix  7 !  + - ^  !  andb   !       !         ! +`  -` !
         !         !         !       !         !   ^`   !
(* ========================================================================== *)
infix  6 !         !  xorb   !       !         !        !
(* -------------------------------------------------------------------------- *)
infixr 6 !  ::  @  !         !       !         ! ::` @` !
(* ========================================================================== *)
infix  5 ! > >= =  ! orb ==  !       !         !   =`   !
         ! < <= <> ! != ?=   !       !         !        !
(* ========================================================================== *)
infix  4 !         !         ! <\ \> !         !        !
(* -------------------------------------------------------------------------- *)
infixr 4 !         !         ! </ /> !         !        !
(* ========================================================================== *)
infix  3 !    o    !         !       !         !        ! <-->
(* ========================================================================== *)
infix  2 !         ! andAlso !  >|   !         !        !
(* -------------------------------------------------------------------------- *)
infixr 2 !         !         !   |<  !         !        !
(* ========================================================================== *)
infix  1 !    :=   ! orElse  !       ! >>= >>& !        ! :=: += -=
         !         !         !       ! >>* >>@ !        !
         !         !         !       ! ><      !        !
(* -------------------------------------------------------------------------- *)
infixr 1 !         !         !       !   =<<   !        !
(* ========================================================================== *)
infix  0 ! before  !         !       !   <|>   !   &`   ! &
(* -------------------------------------------------------------------------- *)
infixr 0 !         !         !       !         !        ! -->
(* ************************************************************************** *)

nonfix ! (* We just used ! above as a visual separator. *)
