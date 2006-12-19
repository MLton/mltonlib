signature AUTOMATA =
  sig
    eqtype char
    eqtype string
    
    structure ZTree : ZTREE
    
    structure Deterministic :
      sig
        eqtype state
        type t
        
        val size: t -> int
        val start: t -> state
        val accepts: t -> state -> bool
        val step: t -> (char * state) -> state
        val multistep: t -> (string * state) -> state
        val test: t -> string -> bool
        
        val any: t
        val empty: t
        val char: bool ZTree.t -> t
        
        (* minimizes states and puts in canonical order *)
        val optimize: t -> t
        (* compares two minimal, canonical DFAs for equality *)
        val equal: (t * t) -> bool
        
        val complement: t -> t
        val union: (t * t) -> t
        val intersect: (t * t) -> t
        
        (* The passed function is the 'cost' of a character in length *)
        val shortestMatch: (char option * char option -> int * char) -> t 
            -> char list option
        
        val toDot: (String.string * t) -> String.string
        val toSML: (String.string * t) -> String.string
        val toC:   (String.string * t) -> String.string
      end
      
    structure NonDeterministic :
      sig
        eqtype state
        type t
        
        val size: t -> int
        val start: t -> state
        val accepts: t -> state -> bool
        val step: t -> (char * state list) -> state list
        val multistep: t -> (string * state list) -> state list
        val test: t -> string -> bool
        
        val power: t -> t
        val concat: (t * t) -> t
        
        val toDFA: t -> Deterministic.t
        val fromDFA: Deterministic.t -> t
        
        val toDot: (String.string * t) -> String.string
      end
    
    structure Expression :
      sig
        datatype t = 
          Empty | Any | Char of bool ZTree.t | Not of t | Star of t | 
          Concat of t * t | Union of t * t | Intersect of t * t
        
        (* val toString: t -> String.string *)
        val toDFA: t -> Deterministic.t
      end
    
    structure RegularExpression :
      sig
        type t
        
        val fromString: String.string -> t
        val toExpression: t -> Expression.t
      end
  end
