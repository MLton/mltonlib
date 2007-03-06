(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This cache implementation is basically an imperative separate chaining
 * hashtable.  The keys are generated using a quick-and-dirty pseudo RNG.
 *)
structure Cache :> CACHE where type Key.t = MLRep.Long.Unsigned.word = struct
   structure T = WordTable and A = WordTable.Action
         and W = WordTable.Key
         and Dbg = MkDbg (open DbgDefs val name = "Cache")

   structure Key = struct
      open MLRep.Long.Unsigned
      type t = word
   end

   val () = Dbg.verify (W.wordSize <= Key.wordSize)

   datatype 'a t = IN of {table : 'a T.t, seed : W.t ref}

   exception NotFound

   val (keyToWord, wordToKey) =
       Iso.<--> (Iso.swap W.isoLarge, (Key.toLarge, Key.fromLarge))

   fun new () = IN {table = T.new (), seed = ref 0w0}

   fun size (IN {table, ...}) = T.size table

   fun isEmpty c = 0 = size c

   fun putWith (t as IN {table, seed}) keyToValue = let
      val word = !seed before seed := Misc.ranqd1 (!seed)
      val key = wordToKey word
   in
      case T.access
              table word
              (A.peek {some = fn () => A.return NONE,
                       none = fn () => let
                                 val value = keyToValue key
                              in
                                 A.insert value (SOME value)
                              end}) of
         NONE => putWith t keyToValue
       | SOME value => (key, value)
   end

   fun put t = #1 o putWith t o const

   fun access action (IN {table, ...}) key =
       T.access table (keyToWord key) action

   fun get ? = access (A.get {none = raising NotFound, some = A.return}) ?
   fun use ? = access (A.get {none = raising NotFound, some = A.remove}) ?
   fun rem ? = access (A.peek {none = raising NotFound, some = A.remove}) ?

   fun values (IN {table, ...}) = T.toList table
end
