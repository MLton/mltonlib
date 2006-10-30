(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor TopDownParser (S: TOP_DOWN_PARSER_STRUCTS): TOP_DOWN_PARSER = 
struct

open S

val showConsider = ref false

structure Plist = PropertyList

structure Terminal =
   struct
      datatype t = T of {index: int ref,
                         name: string,
                         plist: Plist.t,
                         uniq: unit ref}

      local
         fun make f (T r) = f r
      in
         val index = ! o (make #index)
         val name = make #name
         val uniq = make #uniq
      end

      val toString = name

      val layout = Layout.str o toString

      fun new n = T {index = ref ~1,
                     name = n,
                     plist = Plist.new (),
                     uniq = ref ()}

      fun equals (t, t') = uniq t = uniq t'
   end

structure Terminals =
   struct
      datatype t = T of Terminal.t list

      fun layout (T ts) = List.layout Terminal.layout ts

      val empty = T []

      fun single t = T [t]

      fun exists (T ts, f) = List.exists (ts, f)
         
      fun contains (ts, t) = exists (ts, fn t' => Terminal.equals (t, t'))

      fun isSubset (T ts, ts') = List.forall (ts, fn t => contains (ts', t))

      val isSubset =
         Trace.trace2 ("Terminals.isSubset", layout, layout, Bool.layout)
         isSubset
         
      fun union (T ts, T ts') =
         T (List.fold (ts', ts, fn (t, ts') =>
                       if List.exists (ts', fn t' => Terminal.equals (t, t'))
                          then ts'
                       else t :: ts'))
   end

structure TokenStream =
   struct
      datatype 'a t =
         T of {insert: (Terminal.t * 'a) option -> (Terminal.t * 'a) option,
               stream: (Terminal.t * 'a) Stream.t}

      fun layout (T {stream, ...}) =
         Stream.layout (Terminal.layout o #1) (Stream.firstN (stream, 10))
      val _ = layout
         
      fun isEmpty (T {stream, ...}) = Stream.isEmpty stream

      fun getTerminal (T {insert, stream}) =
         Option.map (Stream.get stream, fn (s, t) =>
                     (T {insert = insert, stream = s}, t))

      fun 'a insert (ts as T {insert, stream}): 'a t option =
         Option.map
         (insert (Option.map (getTerminal ts, #2)), fn tv =>
          (T {insert = insert,
              stream = Stream.prefix (stream, tv)}))
   end

structure Result:>
   sig
      type ('a, 'b) t

      val fail: 'a TokenStream.t -> ('a, 'b) t
      val or: ('a, 'b) t * (unit -> ('a, 'b) t) -> ('a, 'b) t
      val seq: ('a, 'b) t * ('a TokenStream.t * 'b -> ('a, 'c) t) -> ('a, 'c) t
      val succeed: 'a TokenStream.t * 'b -> ('a, 'b) t
      val toOpt: ('a, 'b) t -> 'b option
   end =
   struct
      datatype ('a, 'b) t = T of {stream: 'a TokenStream.t,
                                  value: 'b} option

      fun fail _ = T NONE
         
      fun succeed (ts, v) = T (SOME {stream = ts, value = v})

      fun or (T s, f) = case s of NONE => f () | SOME _ => T s

      fun seq (T s, f) =
         case s of
            NONE => T NONE
          | SOME {stream, value} => f (stream, value)

      fun toOpt (T z) =
         case z of
            NONE => NONE
          | SOME {stream, value} =>
               if TokenStream.isEmpty stream
                  then SOME value
               else NONE
   end

structure Univ:>
   sig
      type t

      val new: unit -> ('a -> t) * (t -> 'a)
   end =
   struct
      type t = exn

      fun new () =
         let
            exception E of 'a
         in
            (E, fn E a => a | _ => Error.bug "proj")
         end
   end

structure Bval = TwoPointLattice (val bottom = "false"
                                  val top = "true")

structure Tval = JoinLattice (structure Element =
                                 struct
                                    open Terminals

                                    val op <= = isSubset
                                    val join = union
                                 end)
   
structure Exp =
   struct
      datatype 'a t =
         T of {first: Tval.t,
               follow: Tval.t,
               isNullable: Bval.t,
               node: 'a node,
               parse: ('a TokenStream.t -> ('a, Univ.t) Result.t) ref,
               visited: bool ref}
      and 'a node =
         Indirect of {exp: 'a t option ref,
                      name: string}
       | Or of 'a t list * {mayBacktrack: bool}
       | Seq of {exps: 'a t list,
                 wrapper: Univ.t list -> Univ.t}
       | Terminal of Terminal.t * ('a -> Univ.t)

      fun parse (T {parse, ...}, ts) = !parse ts

      local
         fun make f (T r) = f r
      in
         val follow' = fn z => make #follow z
         val first' = fn z => make #first z
         val isNullable' = fn z => make #isNullable z
         val node = fn z => make #node z
      end

      local
         open Layout
         fun layoutNode (n, lay) =
            case n of
               Indirect {name, ...} => str name
             | Or (es, _) => seq [str "Or ", List.layout lay es]
             | Seq {exps, ...} =>
                  (case exps of
                      [e] => lay e
                    | _ => seq [str "Seq ", List.layout lay exps])
             | Terminal (t, _) => Terminal.layout t
      in
         fun layout (T {node, ...}) = layoutNode (node, layout)
         fun layoutDetailed (T {first, follow, isNullable, node, ...}) =
            record [("first", Tval.layout first),
                    ("follow", Tval.layout follow),
                    ("isNullable", Bval.layout isNullable),
                    ("node", layoutNode (node, layoutDetailed))]
      end

      fun first e = Tval.value (first' e)

      fun follow e = Tval.value (follow' e)
         
      fun isNullable e = Bval.isTop (isNullable' e)

      local
         fun make f n = T {first = Tval.new Terminals.empty,
                           follow = Tval.new Terminals.empty,
                           isNullable = Bval.new (),
                           node = f n,
                           parse = ref (fn _ => Error.bug "parse not defined"),
                           visited = ref false}
      in
         val indirect = fn z => make Indirect z
         val or = fn z => make Or z
         val seq = fn z => make Seq z
         val terminal = fn z => make Terminal z
      end

      val splitOff: 'a list * int -> 'a list * 'a list =
         fn (xs, n) =>
         let
            fun loop (xs, i, ac) =
               if i = 0
                  then (rev ac, xs)
               else
                  case xs of
                     [] => Error.bug "splitOff"
                   | x :: xs => loop (xs, i - 1, x :: ac)
         in
            loop (xs, n, [])
         end

      fun isSeq (T {node, ...}) =
         case node of
            Seq _ => true
          | _ => false

      val seq = fn (es, f) =>
         if List.exists (es, not o isSeq)
            then seq {exps = es, wrapper = f}
         else
            let
               val (es, nfs) =
                  List.fold
                  (rev es, ([], []), fn (T {node, ...}, (es, nfs)) =>
                   case node of
                      Seq {exps = es', wrapper} =>
                         (es' @ es, (List.length es', wrapper) :: nfs)
                    | _ => Error.bug "seq")
               fun wrapper us =
                  let
                     fun loop (us, nfs, ac) =
                        case nfs of
                           [] => f (rev ac)
                         | (n, f) :: nfs =>
                              let
                                 val (us', us) = splitOff (us, n)
                              in
                                 loop (us, nfs, f us' :: ac)
                              end
                  in
                     loop (us, nfs, [])
                  end
            in
               seq {exps = es, wrapper = wrapper}
            end

      val wrap: 'a t * (Univ.t -> Univ.t) -> 'a t =
         fn (e, f) =>
         seq ([e],
              fn [u] => f u
               | us =>
                    let
                       open Layout
                       val () =
                          outputl (seq [Int.layout (List.length us),
                                        str " ", layout e],
                                   Out.error)
                    in
                       Error.bug "wrap"
                    end)
   end
         
datatype ('a, 'b) t = T of {exp: 'a Exp.t,
                            proj: Univ.t -> 'b}

local
   fun make f (T r) = f r
in
   val exp = fn z => make #exp z
end

fun delayDef {name: string} =
   let
      val r = ref NONE
      val pr = ref (fn _ => Error.bug "delayDef undefined")
      fun define (T {exp, proj}) = (r := SOME exp; pr := proj)
   in
      (T {exp = Exp.indirect {exp = r, name  = name},
          proj = fn z => !pr z},
       {define = define})
   end

local
   val c = Counter.new 0
in
   fun recur (f: ('a, 'b) t -> ('a, 'b) t): ('a, 'b) t =
      let
         val (p, {define}) =
            delayDef {name = concat ["Recur", Int.toString (Counter.next c)]}
         val () = define (f p)
      in
         p
      end
end

fun terminal t =
   let
      val (inj, proj) = Univ.new ()
   in
      T {exp = Exp.terminal (t, inj),
         proj = proj}
   end
   
fun orGen (es, mb) =
   let
      val (inj, proj) = Univ.new ()
   in
      T {exp = Exp.or (List.map (es, fn T {exp, proj} =>
                                 Exp.wrap (exp, inj o proj)),
                       mb),
         proj = proj}
   end

fun or es = orGen (es, {mayBacktrack = false})

fun orB es = orGen (es, {mayBacktrack = true})

fun seq es =
   let
      val (inj', proj') = Univ.new ()
      val (inj'', proj'') = Univ.new ()
   in
      T {exp = Exp.seq (List.map (es, fn T {exp, proj} =>
                                  Exp.wrap (exp, inj' o proj)),
                        fn us => inj'' (List.map (us, proj'))),
         proj = proj''}
   end
         
fun seq1 (T {exp, proj}, f) =
   let
      val (inj', proj') = Univ.new ()
   in
      T {exp = Exp.wrap (exp, inj' o f o proj),
         proj = proj'}
   end

fun seq2 (p1, p2, f) =
   let
      datatype ('a, 'b) z = V1 of 'a | V2 of 'b
   in
      seq1 (seq [seq1 (p1, V1), seq1 (p2, V2)],
            fn [V1 v1, V2 v2] => f (v1, v2)
             | _ => Error.bug "seq2")
   end

fun seq3 (p1, p2, p3, f) =
   seq2 (p1, seq2 (p2, p3, fn z => z),
         fn (v1, (v2, v3)) => f (v1, v2, v3))

fun seq4 (p1, p2, p3, p4, f) =
   seq2 (p1, seq3 (p2, p3, p4, fn z => z),
         fn (v1, (v2, v3, v4)) => f (v1, v2, v3, v4))

fun seq5 (p1, p2, p3, p4, p5, f) =
   seq2 (p1, seq4 (p2, p3, p4, p5, fn z => z),
         fn (v1, (v2, v3, v4, v5)) => f (v1, v2, v3, v4, v5))

fun seq6 (p1, p2, p3, p4, p5, p6, f) =
   seq2 (p1, seq5 (p2, p3, p4, p5, p6, fn z => z),
         fn (v1, (v2, v3, v4, v5, v6)) => f (v1, v2, v3, v4, v5, v6))

fun seq7 (p1, p2, p3, p4, p5, p6, p7, f) =
   seq2 (p1, seq6 (p2, p3, p4, p5, p6, p7, fn z => z),
         fn (v1, (v2, v3, v4, v5, v6, v7)) =>
         f (v1, v2, v3, v4, v5, v6, v7))

fun empty () = seq1 (seq [], fn _ => ())

fun opt p = or [seq1 (empty (), fn _ => NONE),
                seq1 (p, SOME)]

fun zeroOrMore p =
   recur (fn me => or [seq1 (empty (), fn () => []),
                       seq2 (p, me, op ::)])

fun oneOrMore p = seq2 (p, zeroOrMore p, op ::)

fun parse (p: ('a, 'b) t) =
   let
      (* Collect all expressions. *)
      val all = ref []
      datatype z = datatype Exp.node
      fun loop (exp as Exp.T {node, visited, ...}) =
         if !visited
            then ()
         else
            let
               val () = visited := true
               val () = List.push (all, exp)
            in
               case node of
                  Indirect {exp, ...} => loop (valOf (!exp))
                | Or (es, _) => List.foreach (es, loop)
                | Seq {exps, ...} => List.foreach (exps, loop)
                | Terminal _ => ()
            end
      val () = loop (exp p)
      val all = !all
      fun foreachExp f = List.foreach (all, f)
      fun foreachNode f = foreachExp (f o Exp.node)
      (* Set terminal indices. *)
      val numTerminals = ref 0
      val terminals = ref []
      val () =
         foreachNode
         (fn n =>
          case n of
             Terminal (t as Terminal.T {index, ...}, _) =>
                if ~1 = !index
                   then (index := !numTerminals
                         ; List.push (terminals, t)
                         ; Int.inc numTerminals)
                else ()
           | _ => ())
      val terminals = Vector.fromListRev (!terminals)
      (* Compute isNullable. *)
      val () =
         foreachExp
         (fn Exp.T {isNullable, node, ...} =>
          case node of
             Indirect {exp, ...} =>
                Bval.<= (Exp.isNullable' (valOf (!exp)), isNullable)
           | Or (es, _) =>
                List.foreach (es, fn e =>
                              Bval.<= (Exp.isNullable' e, isNullable))
           | Seq {exps, ...} =>
                if List.isEmpty exps
                   then Bval.makeTop isNullable
                else
                   let
                      val r = ref (List.length exps)
                   in
                      List.foreach
                      (exps, fn e =>
                       Bval.addHandler
                       (Exp.isNullable' e, fn () =>
                        (Int.dec r
                         ; if 0 = !r then Bval.makeTop isNullable else ())))
                   end
           | Terminal _ => ())
      (* Compute first. *)
      val () =
         foreachExp
         (fn Exp.T {first, node, ...} =>
          let
             fun up e = Tval.<= (Exp.first' e, first)
          in
             case node of
                Indirect {exp, ...} => up (valOf (!exp))
              | Or (es, _) => List.foreach (es, up)
              | Seq {exps, ...} =>
                   let
                      fun loop es =
                         case es of
                            [] => ()
                          | e :: es =>
                               (up e; if Exp.isNullable e then loop es else ())
                   in
                      loop exps
                   end
              | Terminal (t, _) =>
                   Tval.ensureAtLeast (first, Terminals.single t)
          end)
      (* Compute follow. *)
      val () =
         foreachExp
         (fn Exp.T {follow, node, ...} =>
          case node of
             Indirect {exp, ...} =>
                Tval.<= (follow, Exp.follow' (valOf (!exp)))
           | Or (es, _) =>
                List.foreach (es, fn e => Tval.<= (follow, Exp.follow' e))
           | Seq {exps, ...} =>
                ignore
                (List.fold (rev exps, follow, fn (e, tv) =>
                            (Tval.<= (tv, Exp.follow' e)
                             ; if not (Exp.isNullable e)
                                  then Exp.first' e
                               else
                                  let
                                     val tv' = Tval.new (Exp.first e)
                                     val () = Tval.<= (tv, tv')
                                  in
                                     tv'
                                  end)))
           | Terminal _ => ())
      val () =
         if true then () else
            let
               val () = print "(Grammar info\n"
               val () =
                  List.foreach
                  (all, fn e =>
                   case Exp.node e of
                      Indirect {exp, name} =>
                         let
                            val exp = valOf (!exp)
                            open Layout
                         in
                            outputl
                            (align [str name,
                                    indent (align [Exp.layout exp,
                                                   Exp.layoutDetailed exp],
                                            3)],
                             Out.error)
                         end
                    | _ => ())
               val () = print "end grammar info)\n"
            in
               ()
            end
      (* Build parsers. *)
      fun memoTerm (f: Terminal.t -> 'z): Terminal.t -> 'z =
         let
            val v = Vector.map (terminals, f)
         in
            fn t => Vector.sub (v, Terminal.index t)
         end
      fun autoInsert f ts =
         Result.or
         (f ts, fn () =>
          case TokenStream.insert ts of
             NONE => Result.fail ts
           | SOME ts => f ts)
      fun warning msg =
         if true then () else
            let
               open Layout
            in
               outputl (msg, Out.error)
            end
      fun or (es, {mayBacktrack}, follow) =
         let
            val nulls = List.keepAll (es, Exp.isNullable)
            val numNulls = List.length nulls
            val () =
               if numNulls <= 1
                  then ()
               else
                  warning
                  let
                     open Layout
                  in
                     align
                     [str "resolving conflict on nullification in favor of first choice:",
                      indent (List.layout Exp.layout nulls, 3)]
                  end
            val memo =
               memoTerm
               (fn t =>
                let
                   val choices =
                      List.keepAll (es, fn e =>
                                    Terminals.contains (Exp.first e, t))
                   val nullify =
                      numNulls > 0 andalso Terminals.contains (follow, t)
                in
                   if mayBacktrack
                      then {choices = choices,
                            nullify = nullify}
                   else
                      let
                         val numChoices = List.length choices
                         val () =
                            if numChoices <= 1
                               then ()
                            else
                               warning
                               let
                                  open Layout
                               in
                                  align [seq [str "resolving conflict on ",
                                              Terminal.layout t,
                                              str " in favor of first choice:"],
                                         indent
                                         (List.layout Exp.layout choices, 3)]
                               end
                         val () =
                            if numChoices = 0 orelse not nullify
                               then ()
                            else
                               warning
                               let
                                  open Layout
                               in
                                  align
                                  [seq [str "resolving conflict on ",
                                        Terminal.layout t,
                                        str " in favor of choice"],
                                   indent
                                   (align [Exp.layout (hd choices),
                                           str "over nullification",
                                           List.layout Exp.layout nulls],
                                    3)]
                               end
                         val (choices, nullify) =
                            case choices of
                               [] => (choices, nullify)
                             | p :: _ => ([p], false)
                      in
                         {choices = choices,
                          nullify = nullify}
                      end
                end)
         in
            autoInsert
            (fn ts =>
             let
                fun null () =
                   case nulls of
                      [] => Result.fail ts
                    | e :: _ => Exp.parse (e, ts)
             in
                case TokenStream.getTerminal ts of
                   NONE => null ()
                 | SOME (_, (t, _)) =>
                      let
                         val {choices, nullify} = memo t
                         fun loop choices =
                            case choices of
                               [] => if nullify then null () else Result.fail ts
                             | e :: choices =>
                                  Result.or (Exp.parse (e, ts), fn () =>
                                             loop choices)
                      in
                          loop choices
                      end
             end)
         end
      val () =
         foreachExp
         (fn e as Exp.T {node, parse, ...} =>
          let
             val p =
                case node of
                   Indirect {exp, ...} => (fn z => Exp.parse (valOf (!exp), z))
                 | Or (es, mb) => or (es, mb, Exp.follow e)
                 | Seq {exps, wrapper} =>
                      let
                         fun loop (es, ts, us): ('a, Univ.t) Result.t =
                            case es of
                               [] => Result.succeed (ts, wrapper (rev us))
                             | e :: es =>
                                  Result.seq (Exp.parse (e, ts), fn (ts, u) =>
                                              loop (es, ts, u :: us))
                      in
                         fn ts => loop (exps, ts, [])
                      end
                 | Terminal (t, f) =>
                      autoInsert
                      (fn ts =>
                       case TokenStream.getTerminal ts of
                          NONE => Result.fail ts
                        | SOME (ts, (t', v)) =>
                             if Terminal.equals (t, t')
                                then Result.succeed (ts, f v)
                             else Result.fail ts)
             val p =
                fn ts =>
                let
                   open Layout
                   val () =
                      if not (!showConsider) then () else
                         outputl
                         (align [str "(consider",
                                 indent (align [TokenStream.layout ts,
                                                Exp.layout e],
                                         3)],
                          Out.error)
                   val res = p ts
                   val ()  =
                      if not (!showConsider) then () else
                         outputl (seq [Bool.layout (isSome (Result.toOpt res)),
                                       str ")"],
                                  Out.error)
                in
                   res
                end
          in
             parse := p
          end)
   in
      fn z =>
      let
         val T {exp, proj} = p
         val ts = TokenStream.T z
      in
         Result.toOpt (Result.seq (Exp.parse (exp, ts), fn (ts, u) =>
                                   Result.succeed (ts, proj u)))
      end
   end

end
