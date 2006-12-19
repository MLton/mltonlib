type tyvar = string
type tag = string

(* Types which can be written down without a declaration: *)
datatype inline_typ = RECORD of (string * inline_typ) vector
                    | RECURSIVE of string * (inline_typ * tag) vector * tag
                    | TUPLE of inline_typ vector
                    | TYVAR of tyvar * tag * tag

(* At the top-level, the most general declaration looks like:
 *
 * datatype d1 = C of d1 | D of d2 | E of t1
 * and d2 = F of d1 | G of d2 | F of t2
 * withtypes t1 = string * d2
 * and t2 = t1 (* the OLD binding of t1, not the one above *)
 * 
 * The datatypes are evaluated within the scope of the withtypes and
 * the other datatypes.
 *
 * The withtypes are evaluated within the scope of the datatypes (but
 * not of the other withtypes).
 *
 * An entire such clause is a 'toplevel_typ' below
 *)

type data_typ = tag * tag * (string * inline_typ option) vector

type 'a bind_typ = { name   : string, 
                     reader : string,
                     writer : string,
                     tyvars : (tyvar * tag) vector, 
                     typ    : 'a }

type toplevel_typ = data_typ bind_typ vector * inline_typ bind_typ vector
