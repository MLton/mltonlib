(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a simple program variable editor module {VarEd}.
 *
 * Suppose, for example, that you're implementing a real-time physics
 * simulation of some sort.  It is typical that such a simulation has many
 * heuristic parameters (e.g. damping factors, friction coefficients,
 * ...).  In order to achieve good results, it is imperative to be able to
 * quickly experiment with different parameters.  {VarEd} makes it easy to
 * implement a simple command-line / console editor for program variables.
 *)
signature VAR_ED = sig
   (**
    * Variables are attached to a hierarchy of groups.
    *)
   structure Group : sig
      type t
      (** The type of variable groups. *)

      val new : {parent : t,
                 name : String.t} -> t
      (** Creates a new group. *)
   end

   (**
    * Like a ref cell, each variable holds a value that can be accessed by
    * the program and can additionally be accessed interactively using the
    * editor.
    *)
   structure Var : sig
      type 'a t
      (** Type of variables. *)

      val new : {group : Group.t,
                 name : String.t,
                 rep : 'a Generic.Rep.t,
                 value : 'a,
                 assert : 'a UnOp.t,
                 signal : 'a t Effect.t} -> 'a t
      (**
       * Creates a new variable.
       *
       * Generic functions obtained from the type representation {rep} are
       * used to show the values of variables and read new values from the
       * user.
       *
       * Values assigned to the variable (either by the user or by the
       * program) go through the {assert} function before the variable is
       * changed and the {signal} function is called after the variable has
       * been changed.  Both the assert function and the signal function
       * may raise exceptions.
       *)

      val ! : 'a t -> 'a
      (** Returns the current value of the variable. *)

      val := : ('a t * 'a) Effect.t
      (** Assigns a new value to the variable. *)
   end

   type t
   (** Type of variable editors. *)

   val new : {name : String.t} -> t
   (** Creates a new variable editor.  The name is for the root group. *)

   val root : t -> Group.t
   (** Returns the root group of a variable editor. *)

   val update : t -> {instream : TextIO.instream,
                      outstream : TextIO.outstream,
                      ansi : Bool.t,
                      columns : Int.t Option.t} -> String.t Option.t
   (**
    * Updates the interactive variable editor.
    *
    * The editor is written to the given output stream and input is read
    * from the given input stream.  This function does not block; the
    * input stream is read only when it doesn't block.  Input that is not
    * recognized by the editor is returned as {SOME text} and does not
    * change the state of the editor or variables.
    *
    * The {ansi} flag specifies whether the editor may use ANSI control
    * codes to better control the output.
    *)
end
