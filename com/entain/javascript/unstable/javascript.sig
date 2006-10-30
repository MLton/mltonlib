(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
   
signature JAVASCRIPT_STRUCTS =
   sig
      structure Regexp: REGEXP
   end

signature JAVASCRIPT =
   sig
      include JAVASCRIPT_STRUCTS
         
      structure Id:
         sig
            type t

            val equals: t * t -> bool
            val fromString: string -> t
            val layout: t -> Layout.t
            val toString: t -> string
         end
      structure AssignOp:
         sig
            datatype t =
               Add
             | BitwiseAnd
             | BitwiseOr
             | BitwiseXor
             | Div
             | Equals
             | LeftShift
             | Mul
             | Mod
             | RightShiftSigned
             | RightShiftUnsigned
             | Sub
         end
      structure BinaryOp:
         sig
            datatype t =
               Add
             | BitwiseAnd
             | BitwiseOr
             | BitwiseXor
             | Div
             | Equals
             | GreaterThan
             | GreaterThanEqual
             | In
             | InstanceOf
             | LeftShift
             | LessThan
             | LessThanEqual
             | LogicalAnd
             | LogicalOr
             | Mod
             | Mul
             | NotEquals
             | RightShiftSigned
             | RightShiftUnsigned
             | StrictEquals
             | StrictNotEquals
             | Sub

            val equals: t * t -> bool
            val layout: t -> Layout.t
            (* In decreasing order of precedence. *)
            val precedences: t list list
         end
      structure UnaryOp:
         sig
            datatype t =
               Add
             | BitwiseNot
             | Delete
             | LogicalNot
             | Neg
             | PreDecrement
             | PreIncrement
             | PostDecrement
             | PostIncrement
             | TypeOf
             | Void

            val hasSideEffect: t -> bool
         end
      structure Number:
         sig
            (* Positive numbers only.  For negatives, use UnaryOp.Neg. *)
            type t

            val equals: t * t -> bool
            val fromInt: int -> t
            val fromReal: real -> t
            val isZero: t -> bool
            val toReal: t -> real
            val zero: t
         end
      structure String:
         sig
            type t

            val escape: t -> string
            val fromString: string -> t
            val make: word vector -> t
            val toString: t -> string
         end
      structure PropertyName:
         sig
            datatype t =
               Number of Number.t
             | String of String.t

            val fromInt: int -> t
            val fromString: string -> t
            val layout: t -> Layout.t
         end
      structure Joint:
         sig
            datatype exp =
               Array of exp option vector
             | Assign of {lhs: exp,
                          oper: AssignOp.t,
                          rhs: exp}
             | Bool of bool
             | Binary of {lhs: exp,
                          oper: BinaryOp.t,
                          rhs: exp}
             | Call of {args: exp vector,
                        func: exp}
             | Cond of {elsee: exp,
                        test: exp,
                        thenn: exp}
             | Function of {args: Id.t vector,
                            body: statement vector,
                            name: Id.t option}
             | Id of Id.t
             | New of {args: exp vector,
                       object: exp}
             | Number of Number.t
             | Null
             | Object of objectInit vector
             | Regexp of Regexp.t
             | Seq of exp vector
             | Select of {object: exp,
                          property: exp}
             | SelectId of {object: exp,
                            property: Id.t}
             | String of String.t
             | Unary of {exp: exp,
                         oper: UnaryOp.t}
             | This

            and objectInit =
               Get of {args: Id.t vector,
                       body: statement vector,
                       name: Id.t}
              | Property of {property: PropertyName.t,
                             value: exp}
              | Set of {args: Id.t vector,
                        body: statement vector,
                        name: Id.t}

            and statement =
               Block of statement vector
              | Break of Id.t option
              | Const of (Id.t * exp) vector
              | Continue of Id.t option
              | Do of {body: statement,
                       test: exp}
              | Empty
              | Exp of exp
              | For of {body: statement,
                        inc: exp option,
                        init: exp option,
                        test: exp option}
              | ForIn of {body: statement,
                          lhs: exp,
                          object: exp}
              | ForVar of {body: statement,
                           inc: exp option,
                           init: (Id.t * exp option) vector,
                           test: exp option}
              | ForVarIn of {body: statement,
                             id: Id.t,
                             init: exp option,
                             object: exp}
              | FunctionDec of {args: Id.t vector,
                                body: statement vector,
                                name: Id.t}
              | If of {elsee: statement option,
                       test: exp,
                       thenn: statement}
              | Labeled of Id.t * statement
              | Return of exp option
              | Switch of {clauses: (exp option * statement vector) vector,
                           test: exp}
              | Throw of exp
              | Try of {body: statement vector,
                        catch: (Id.t * statement vector) option,
                        finally: statement vector option}
              | Var of (Id.t * exp option) vector (* vector must be nonempty *)
              | While of {body: statement,
                          test: exp}
              | With of {body: statement,
                         object: exp}
         end
      
      structure Exp:
         sig
            datatype t = datatype Joint.exp

            val array: t -> t
            val falsee: t
            val int: int -> t
            val isBool: t -> bool
            val isFalse: t -> bool
            val isTrue: t -> bool
            val not: t -> t
            val object: {property: PropertyName.t, value: t} vector -> t
            val select: {object: t, property: t} -> t
            val seq: t vector -> t
            val layout: t -> Layout.t
            val string: string -> t
            val toString: t -> string
            val truee: t
            val word: word -> t
         end
      structure ObjectInit:
         sig
            datatype t = datatype Joint.objectInit
         end
      structure Statement:
         sig
            datatype t = datatype Joint.statement

            val layout: t -> Layout.t
            val scope: t vector -> t
         end
      structure Program:
         sig
            datatype t = T of Statement.t vector

            val layout: t -> Layout.t
            val layouts: t * (Layout.t -> unit) -> unit
            val simplify: t -> t
         end
   end
