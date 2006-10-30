(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor Parse (S: PARSE_STRUCTS): PARSE = 
struct

open S

structure Parser = TopDownParser

val showConsider = Parser.showConsider
val showAutoSemi = ref false

local
   open Token
in
   structure SourcePos = SourcePos
   structure Variant = Variant
end
local
   open Parser
in
   structure Terminal = Terminal
end

structure Terminal =
   struct
      open Terminal
         
      val ADDOP = new "ADDOP"
      val ASSIGNOP = new "ASSIGNOP"
      val AUTO_SEMICOLON = new "AUTO_SEMICOLON"
      val BANG = new "BANG"
      val BITOP = new "BITOP"
      val BOOL = new "BOOL"
      val BREAK = new "BREAK"
      val CASE = new "CASE"
      val CATCH = new "CATCH"
      val COLON = new "COLON"
      val COMMA = new "COMMA"
      val CONST = new "CONST"
      val CONTINUE = new "CONTINUE"
      val DEFAULT = new "DEFAULT"
      val DELETE = new "DELETE"
      val DO = new "DO"
      val DOT = new "DOT"
      val ELSE = new "ELSE"
      val EOF = new "EOF"
      val EQUALOP = new "EQUALOP"
      val EQUALS = new "EQUALS"
      val FINALLY = new "FINALLY"
      val FOR = new "FOR"
      val FUNCTION = new "FUNCTION"
      val IDENTIFIER = new "IDENTIFIER"
      val IF = new "IF"
      val IN = new "IN"
      val INCOP = new "INCOP"
      val INSTANCE_OF = new "INSTANCE_OF"
      val LBRACE = new "LBRACE"
      val LBRACKET = new "LBRACKET"
      val LOGICOP = new "LOGICOP"
      val LPAREN = new "LPAREN"
      val MULOP = new "MULOP"
      val NEW = new "NEW"
      val NEWLINE_INCOP = new "NEWLINE_INCOP"
      val NULL = new "NULL"
      val NUMBER = new "NUMBER"
      val QUESTION = new "QUESTION"
      val RBRACE = new "RBRACE"
      val RBRACKET = new "RBRACKET"
      val REGEXP = new "REGEXP"
      val RELOP = new "RELOP"
      val RETURN = new "RETURN"
      val RPAREN = new "RPAREN"
      val SEMICOLON = new "SEMICOLON"
      val SHIFTOP = new "SHIFTOP"
      val STRING = new "STRING"
      val SWITCH = new "SWITCH"
      val THIS = new "THIS"
      val THROW = new "THROW"
      val TILDE = new "TILDE"
      val TRY = new "TRY"
      val TYPEOF = new "TYPEOF"
      val VAR = new "VAR"
      val VOID = new "VOID"
      val WHILE = new "WHILE"
      val WITH = new "WITH"

      local
         structure V = Variant
      in
         fun fromToken (t: Token.t): t =
            case Token.variant t of
               V.ADDOP _ => ADDOP
             | V.ASSIGNOP _ => ASSIGNOP
             | V.AUTO_SEMICOLON => AUTO_SEMICOLON
             | V.BANG => BANG
             | V.BITOP _ => BITOP
             | V.BOOL _ => BOOL
             | V.BREAK => BREAK
             | V.CASE => CASE
             | V.CATCH => CATCH
             | V.COLON => COLON
             | V.COMMA => COMMA
             | V.CONST => CONST
             | V.CONTINUE => CONTINUE
             | V.DEFAULT => DEFAULT
             | V.DELETE => DELETE
             | V.DO => DO
             | V.DOT => DOT
             | V.ELSE => ELSE
             | V.EOF => EOF
             | V.EQUALOP _ => EQUALOP
             | V.EQUALS => EQUALS
             | V.FINALLY => FINALLY
             | V.FOR => FOR
             | V.FUNCTION => FUNCTION
             | V.IDENTIFIER _ => IDENTIFIER
             | V.IF => IF
             | V.IN => IN
             | V.INCOP _ => INCOP
             | V.INSTANCE_OF => INSTANCE_OF
             | V.LBRACE => LBRACE
             | V.LBRACKET => LBRACKET
             | V.LOGICOP _ => LOGICOP
             | V.LPAREN => LPAREN
             | V.MULOP _ => MULOP
             | V.NEW => NEW
             | V.NEWLINE_INCOP _ => NEWLINE_INCOP
             | V.NULL => NULL
             | V.NUMBER _ => NUMBER
             | V.QUESTION => QUESTION
             | V.RBRACE => RBRACE
             | V.RBRACKET => RBRACKET
             | V.REGEXP _ => REGEXP
             | V.RELOP _ => RELOP
             | V.RETURN => RETURN
             | V.RPAREN => RPAREN
             | V.SEMICOLON => SEMICOLON
             | V.SHIFTOP _ => SHIFTOP
             | V.STRING _ => STRING
             | V.SWITCH => SWITCH
             | V.THIS => THIS
             | V.THROW => THROW
             | V.TILDE => TILDE
             | V.TRY => TRY
             | V.TYPEOF => TYPEOF
             | V.VAR => VAR
             | V.VOID => VOID
             | V.WHILE => WHILE
             | V.WITH => WITH
      end
   end


structure V = Variant

local
   open Javascript
in
   structure AssignOp = AssignOp
   structure BinaryOp = BinaryOp
   structure Exp = Exp
   structure Id = Id
   structure ObjectInit = ObjectInit
   structure Program = Program
   structure PropertyName = PropertyName
   structure Statement = Statement
   structure UnaryOp = UnaryOp
end

fun precedenceParse (e: Exp.t, opes: (BinaryOp.t * Exp.t) list): Exp.t =
   let
      fun loop (e, opes, precedences) =
         case (opes, precedences) of
            ([], _) => e
          | (_, []) => Error.bug "precedenceParse"
          | (_, ops :: precedences) =>
               let
                  fun loop1 (e, opes, ac) =
                     case opes of
                        [] =>
                           let
                              val (e, opes) =
                                 List.fold
                                 (ac, (e, []), fn ((e', oper), (e, ac)) =>
                                  (e', (oper, e) :: ac))
                           in
                              loop (e, opes, precedences)
                           end
                    | (oper, e') :: opes =>
                         if List.exists (ops, fn oper' =>
                                         BinaryOp.equals (oper, oper'))
                            then loop1 (Exp.Binary {lhs = e,
                                                   oper = oper,
                                                   rhs = e'},
                                       opes, ac)
                         else loop1 (e', opes, (e, oper) :: ac)
               in
                  loop1 (e, opes, [])
               end
   in
      loop (e, opes, BinaryOp.precedences)
   end

val precedenceParse =
   Trace.trace2 ("precedenceParse",
                 Exp.layout,
                 List.layout (Layout.tuple2 (BinaryOp.layout, Exp.layout)),
                 Exp.layout)
   precedenceParse

val Program =
   let
      local
         open Terminal
         val t = Parser.terminal 
      in
         val ADDOP = t ADDOP
         val ASSIGNOP = t ASSIGNOP
         val AUTO_SEMICOLON = t AUTO_SEMICOLON
         val BANG = t BANG
         val BITOP = t BITOP
         val BOOL = t BOOL
         val BREAK = t BREAK
         val CASE = t CASE
         val CATCH = t CATCH
         val COLON = t COLON
         val COMMA = t COMMA
         val CONST = t CONST
         val CONTINUE = t CONTINUE
         val DEFAULT = t DEFAULT
         val DELETE = t DELETE
         val DO = t DO
         val DOT = t DOT
         val ELSE = t ELSE
         val EQUALOP = t EQUALOP
         val EQUALS = t EQUALS
         val FINALLY = t FINALLY
         val FOR = t FOR
         val FUNCTION = t FUNCTION
         val IDENTIFIER = t IDENTIFIER
         val IF = t IF
         val IN = t IN
         val INCOP = t INCOP
         val INSTANCE_OF = t INSTANCE_OF
         val LBRACE = t LBRACE
         val LBRACKET = t LBRACKET
         val LOGICOP = t LOGICOP
         val LPAREN = t LPAREN
         val MULOP = t MULOP
         val NEW = t NEW
         val NEWLINE_INCOP = t NEWLINE_INCOP
         val NULL = t NULL
         val NUMBER = t NUMBER
         val QUESTION = t QUESTION
         val RBRACE = t RBRACE
         val RBRACKET = t RBRACKET
         val REGEXP = t REGEXP
         val RELOP = t RELOP
         val RETURN = t RETURN
         val RPAREN = t RPAREN
         val SEMICOLON = t SEMICOLON
         val SHIFTOP = t SHIFTOP
         val STRING = t STRING
         val SWITCH = t SWITCH
         val THIS = t THIS
         val THROW = t THROW
         val TILDE = t TILDE
         val TRY = t TRY
         val TYPEOF = t TYPEOF
         val VAR = t VAR
         val VOID = t VOID
         val WHILE = t WHILE
         val WITH = t WITH
      end

      open Parser

      val IDENTIFIER =
         seq1 (IDENTIFIER, fn v =>
               case Token.variant v of
                  V.IDENTIFIER s => Id.fromString s
                | _ => Error.bug "IDENTIFIER")

      val NUMBER =
         seq1 (NUMBER, fn v =>
               case Token.variant v of
                  V.NUMBER r => Javascript.Number.fromReal r
                | _ => Error.bug "NUMBER")

      val REGEXP =
         seq1 (REGEXP, fn v =>
               case Token.variant v of
                  V.REGEXP r => r
                | _ => Error.bug "REGEXP")

      val STRING =
         seq1 (STRING, fn v =>
               case Token.variant v of
                  V.STRING s => Javascript.String.make s
                | _ => Error.bug "STRING")

      fun commaList e =
         seq1 (seq2 (e, zeroOrMore (seq2 (COMMA, e, #2)), op ::),
               Vector.fromList)

      fun optCommaList e =
         seq1 (opt (commaList e),
               fn NONE => Vector.new0 ()
                | SOME v => v)

      fun oper (t, getString, trans) =
         seq1 (t, fn v =>
               case getString (Token.variant v) of
                  NONE => Error.bug "oper"
                | SOME s =>
                     case List.peek (trans, fn (s', _) => s = s') of
                        NONE => Error.bug (concat ["missing oper: ", s])
                      | SOME (_, z) => z)

      fun incop (dec, inc) =
         oper (INCOP, fn V.INCOP s => SOME s | _ => NONE,
               [("--", dec), ("++", inc)])

      val PostInc =
         seq1 (opt (incop (UnaryOp.PostDecrement, UnaryOp.PostIncrement)),
               fn NONE => (fn e => e)
                | SOME oper => (fn e => Exp.Unary {exp = e, oper = oper}))

      val PropertyName =
         or [seq1 (IDENTIFIER, fn id =>
                   PropertyName.String
                   (Javascript.String.fromString (Id.toString id))),
             seq1 (NUMBER, PropertyName.Number),
             seq1 (STRING, PropertyName.String)]

      val Semicolon = or [AUTO_SEMICOLON, SEMICOLON]
   
      val AssignmentOperator =
         or [seq1 (EQUALS, fn _ => AssignOp.Equals),
             oper (ASSIGNOP, fn V.ASSIGNOP s => SOME s | _ => NONE,
                   let
                      datatype z = datatype AssignOp.t
                   in
                      [("-=", Sub),
                       ("*=", Mul),
                       ("%=", Mod),
                       ("/=", Div),
                       ("+=", Add),
                       ("&=", BitwiseAnd),
                       ("^=", BitwiseXor),
                       ("|=", BitwiseOr),
                       ("<<=", LeftShift),
                       (">>=", RightShiftSigned),
                       (">>>=", RightShiftUnsigned)]
                   end)]

      val BinaryOp =
         let
            datatype z = datatype BinaryOp.t
         in
            or [oper (ADDOP, fn V.ADDOP s => SOME s | _ => NONE,
                      [("+", Add), ("-", Sub)]),
                oper (BITOP, fn V.BITOP s => SOME s | _ => NONE,
                      [("|", BitwiseOr), ("&", BitwiseAnd), ("^", BitwiseXor)]),
                oper (EQUALOP, fn V.EQUALOP s => SOME s | _ => NONE,
                      [("==", Equals),
                       ("!=", NotEquals),
                       ("===", StrictEquals),
                       ("!==", StrictNotEquals)]),
                seq1 (INSTANCE_OF, fn _ => InstanceOf),
                oper (LOGICOP, fn V.LOGICOP s => SOME s | _ => NONE,
                      [("&&", LogicalAnd), ("||", LogicalOr)]),
                oper (MULOP, fn V.MULOP s => SOME s | _ => NONE,
                      [("*", Mul), ("%", Mod), ("/", Div)]),
                oper (RELOP, fn V.RELOP s => SOME s | _ => NONE,
                      [("<", LessThan),
                       ("<=", LessThanEqual),
                       (">", GreaterThan),
                       (">=", GreaterThanEqual)]),
                oper (SHIFTOP, fn V.SHIFTOP s => SOME s | _ => NONE,
                      [("<<", LeftShift),
                       (">>", RightShiftSigned),
                       (">>>", RightShiftUnsigned)])]
         end

      val UnaryOp =
         let
            datatype z = datatype UnaryOp.t
         in
            or [incop (PreDecrement, PreIncrement),
                oper (NEWLINE_INCOP, fn V.NEWLINE_INCOP s => SOME s | _ => NONE,
                      [("--", PreDecrement), ("++", PreIncrement)]),
                seq1 (ADDOP, fn t =>
                      case Token.variant t of
                         V.ADDOP s =>
                            (case s of
                                "-" => Neg
                              | "+" => Add
                              | _ => Error.bug "ADDOP")
                       | _ => Error.bug "ADDOP"),
                seq1 (TILDE, fn _ => BitwiseNot),
                seq1 (BANG, fn _ => LogicalNot),
                seq1 (DELETE, fn _ => Delete),
                seq1 (VOID, fn _ => Void),
                seq1 (TYPEOF, fn _ => TypeOf)]
         end

      val (AssignmentExpression, {define = defineAssignmentExpression}) =
         delayDef {name = "AssignmentExpression"}
      val (Statement, {define = defineStatement}) =
         delayDef {name = "Statement"}

      val Expression = seq1 (commaList AssignmentExpression, Exp.seq)

      val Arguments =
         seq3 (LPAREN, optCommaList AssignmentExpression, RPAREN, #2)

      val FormalParameterList =
         seq3 (LPAREN, optCommaList IDENTIFIER, RPAREN, #2)

      val Statements = seq1 (zeroOrMore Statement, fn ss =>
                             Vector.fromList ss)
         
      val Block = seq3 (LBRACE, Statements, RBRACE, #2)

      val FunctionBody = Block

      val LeftHandSideExpression: (Token.t, Exp.t) Parser.t =
         seq3 (zeroOrMore NEW,
               or [seq1 (BOOL, fn t =>
                         case Token.variant t of
                            V.BOOL b => Exp.Bool b
                          | _ => Error.bug "BOOL"),
                   seq4 (FUNCTION, opt IDENTIFIER, FormalParameterList,
                         FunctionBody,
                         fn (_, name, args, body) =>
                         Exp.Function {args = args,
                                       body = body,
                                       name = name}),
                   seq1 (IDENTIFIER, Exp.Id),
                   seq1 (NULL, fn _ => Exp.Null),
                   seq1 (NUMBER, Exp.Number),
                   seq1 (REGEXP, Exp.Regexp),
                   seq1 (STRING, Exp.String),
                   seq1 (THIS, fn _ => Exp.This),
                   seq3 (LBRACE,
                         optCommaList
                         (orB [seq4 (IDENTIFIER, IDENTIFIER,
                                     FormalParameterList, FunctionBody,
                                     fn (gs, name, args, body) =>
                                     let
                                        val make =
                                           case Id.toString gs of
                                              "get" => ObjectInit.Get
                                            | "set" => ObjectInit.Set
                                            | _ => Error.bug "must be get or set"
                                     in
                                        make {args = args,
                                              body = body,
                                              name = name}
                                     end),
                               seq3 (PropertyName, COLON, AssignmentExpression,
                                     fn (p, _, v) =>
                                     ObjectInit.Property {property = p,
                                                          value = v})]),
                          RBRACE,
                         Exp.Object o #2),
                   seq4 (LBRACKET,
                         seq1 (zeroOrMore COMMA, fn cs =>
                               List.map (cs, fn _ => NONE)),
                         opt (seq2 (AssignmentExpression,
                                    zeroOrMore
                                    (seq2 (COMMA, opt AssignmentExpression,
                                           fn (_, e) =>
                                           NONE :: (case e of
                                                       NONE => []
                                                     | SOME e => [SOME e]))),
                                    fn (e, es) => SOME e :: List.concat es)),
                         RBRACKET,
                         fn (_, es1, es2, _) =>
                         Exp.Array
                         (Vector.fromListRev
                          (#2 (List.fold
                               (es1 @ (case es2 of
                                          NONE => []
                                        | SOME es2 => es2),
                                (false, []), fn (e, (z, ac)) =>
                                case e of
                                   NONE => (false,
                                            if z then ac else NONE :: ac)
                                 | SOME e => (true, SOME e :: ac)))))),
                   seq3 (LPAREN, Expression, RPAREN, #2)],
               zeroOrMore
               (or [seq1 (Arguments, fn args =>
                          fn (n, func) =>
                          if n > 0
                             then (n - 1, Exp.New {args = args,
                                                   object = func})
                          else (n, Exp.Call {args = args, func = func})),
                    seq3 (LBRACKET, Expression, RBRACKET,
                          fn (_, property, _) => fn (n, object) =>
                          (n, Exp.Select {object = object,
                                          property = property})),
                    seq2 (DOT, IDENTIFIER,
                          fn (_, property) => fn (n, object) =>
                          (n, Exp.SelectId {object = object,
                                            property = property}))]),
               fn (news, exp, sufs) =>
               let
                  val (n, e) =
                     List.fold
                     (sufs, (List.length news, exp), fn (s, (n, e)) =>
                      s (n, e))
               in
                  Int.fold (0, n, e, fn (_, e) =>
                            Exp.New {args = Vector.new0 (),
                                     object = e})
               end)

      val UnaryExpression =
         seq3 (zeroOrMore UnaryOp, LeftHandSideExpression, PostInc,
               fn (uops, e, pi) =>
               List.fold (rev uops, pi e, fn (oper, e) =>
                          Exp.Unary {exp = e, oper = oper}))
   
      fun assignmentExpression {mayHaveIn} =
         recur
         (fn AssignmentExpression =>
          let
             val FinishExpression =
                seq3 (PostInc,
                      zeroOrMore (seq2 (or [BinaryOp,
                                            if mayHaveIn
                                               then
                                                  seq1 (IN, fn _ => BinaryOp.In)
                                            else or []],
                                        UnaryExpression,
                                        fn z => z)),
                      opt (seq4 (QUESTION, AssignmentExpression,
                                 COLON, AssignmentExpression,
                                 fn {2 = thenn, 4 = elsee, ...} =>
                                 (thenn, elsee))),
                      fn (pi, opes, cond) => fn (uops, lhs) =>
                      let
                         val e =
                            List.fold (rev uops, pi lhs, fn (oper, e) =>
                                       Exp.Unary {exp = e, oper = oper})
                         val e = precedenceParse (e, opes)
                      in
                         case cond of
                            NONE => e
                          | SOME (thenn, elsee) =>
                               Exp.Cond {elsee = elsee,
                                         test = e,
                                         thenn = thenn}
                      end)
          in
             or [seq3 (oneOrMore UnaryOp,
                       LeftHandSideExpression,
                       FinishExpression,
                       fn (uops, lhs, fe) => fe (uops, lhs)),
                 seq2 (LeftHandSideExpression,
                       or [FinishExpression,
                           seq2 (AssignmentOperator, AssignmentExpression,
                                 fn (oper, rhs) => fn (_, lhs) =>
                                 Exp.Assign {lhs = lhs,
                                             oper = oper,
                                             rhs = rhs})],
                       fn (lhs, fe) => fe ([], lhs))]
          end)

      val () = defineAssignmentExpression (assignmentExpression
                                           {mayHaveIn = true})

      val AssignmentExpressionNoIn = assignmentExpression {mayHaveIn = false}

      val ExpressionNoIn = seq1 (commaList AssignmentExpressionNoIn, Exp.seq)

      local
         fun make AssignmentExpression =
            seq2 (IDENTIFIER, opt (seq2 (EQUALS, AssignmentExpression, #2)),
                  fn z => z)
      in
         val VariableDeclaration = make AssignmentExpression
         val VariableDeclarationNoIn = make AssignmentExpressionNoIn
      end

      val () =
         defineStatement
         (or
          [seq1 (SEMICOLON, fn _ => Statement.Empty),
           orB [seq3 (IDENTIFIER, COLON, Statement, fn (l, _, s) =>
                      Statement.Labeled (l, s)),
                (* Function declaration comes before Expression statement
                 * so that conflict resolution chooses to parse a function
                 * declaration instead of a function expression.
                 *)
                seq4 (FUNCTION, IDENTIFIER, FormalParameterList,
                      FunctionBody,
                      fn (_, name, args, body) =>
                      Statement.FunctionDec {args = args,
                                             body = body,
                                             name = name}),
                (* Block comes before expression statement so that conflict
                 * resolution parses a block instead of an object expression
                 * -- expression statement is not allowed to begin with a
                 * left brace.
                 *)
                or [seq1 (Block, Statement.Block),
                    seq2 (Expression, Semicolon, Statement.Exp o #1)]],
           seq3 (BREAK, opt IDENTIFIER, Semicolon, Statement.Break o #2),
           seq3 (CONST,
                 commaList (seq3 (IDENTIFIER, EQUALS, AssignmentExpression,
                                  fn (x, _, e) => (x, e))),
                 Semicolon,
                 Statement.Const o #2),
           seq3 (CONTINUE, opt IDENTIFIER, Semicolon,
                 Statement.Continue o #2),
           seq7 (DO, Statement, WHILE, LPAREN, Expression, RPAREN, Semicolon,
                 fn {2 = s, 5 = e, ...} => Statement.Do {body = s, test = e}),
           seq5 (FOR, LPAREN,
                 orB [seq5 (opt ExpressionNoIn, SEMICOLON,
                            opt Expression, SEMICOLON,
                            opt Expression,
                            fn {1 = init, 3 = test, 5 = inc, ...} => fn body =>
                            Statement.For {body = body,
                                           inc = inc,
                                           init = init,
                                           test = test}),
                      seq3 (LeftHandSideExpression, IN, Expression,
                            fn (lhs, _, object) => fn body =>
                            Statement.ForIn {body = body,
                                             lhs = lhs,
                                             object = object}),
                      seq6 (VAR, commaList VariableDeclarationNoIn, SEMICOLON,
                            opt Expression, SEMICOLON,
                            opt Expression,
                            fn (_, init, _, test, _, inc) => fn body =>
                            Statement.ForVar {body = body,
                                              inc = inc,
                                              init = init,
                                              test = test}),
                      seq4 (VAR, VariableDeclarationNoIn, IN, Expression,
                            fn (_, (id, init), _, object) =>
                            fn body =>
                            Statement.ForVarIn {body = body,
                                                id = id,
                                                init = init,
                                                object = object})],
                 RPAREN,
                 Statement,
                 fn {3 = f, 5 = s, ...} => f s),
           seq6 (IF, LPAREN, Expression, RPAREN, Statement,
                 (* ELSE clause comes before empty so that conflict resolution
                  * chooses to use ELSE.
                  *)
                 or [seq2 (ELSE, Statement, SOME o #2),
                     seq1 (empty (), fn () => NONE)],
                 fn (_, _, test, _, thenn, elsee) =>
                 Statement.If {elsee = elsee, test = test, thenn = thenn}),
           seq3 (RETURN, opt Expression, Semicolon, Statement.Return o #2),
           seq5 (SWITCH, LPAREN, Expression, RPAREN,
                 let
                    val CaseClauses =
                       zeroOrMore (seq4 (CASE, Expression, COLON, Statements,
                                         fn (_, e, _, ss) => (SOME e, ss)))
                 in
                    seq4 (LBRACE,
                          CaseClauses,
                          opt (seq4 (DEFAULT, COLON, Statements, CaseClauses,
                                     fn (_, _, ss, cs) =>
                                     (NONE, ss) :: cs)),
                          RBRACE,
                          fn (_, cs, cso, _) =>
                          Vector.fromList (case cso of
                                              NONE => cs
                                            | SOME cs' => cs @ cs'))
                 end,
                 fn {3 = test, 5 = clauses, ...} =>
                 Statement.Switch {clauses = clauses, test = test}),
           seq3 (THROW, Expression, Semicolon, Statement.Throw o #2),
           seq3 (TRY, Block,
                 let
                    val Finally = seq2 (FINALLY, Block, #2)
                 in
                    or [seq2 (seq5 (CATCH, LPAREN, IDENTIFIER, RPAREN, Block,
                                    fn {3 = id, 5 = block, ...} =>
                                    SOME (id, block)),
                              opt Finally,
                              fn z => z),
                        seq1 (Finally, fn f => (NONE, SOME f))]
                 end,
                 fn (_, body, (catch, finally)) =>
                 Statement.Try {body = body, catch = catch, finally = finally}),
           seq3 (VAR, commaList VariableDeclaration, Semicolon,
                 Statement.Var o #2),
           seq5 (WHILE, LPAREN, Expression, RPAREN, Statement,
                 fn {3 = test, 5 = body, ...} =>
                 Statement.While {body = body, test = test}),
           seq5 (WITH, LPAREN, Expression, RPAREN, Statement,
                 fn {3 = object, 5 = body, ...} =>
                 Statement.With {body = body, object = object})])
   in
      seq1 (Statements, Program.T)
   end

fun insert to =
   let
      fun doit pos =
         let
            val () =
               if !showAutoSemi
                  then
                     let
                        open Layout
                     in
                        outputl (str (concat ["inserting SEMICOLON at ",
                                              SourcePos.toString pos]),
                                 Out.error)
                     end
               else ()
         in
            SOME
            (Terminal.AUTO_SEMICOLON,
             Token.make {left = pos,
                         right = pos,
                         startsLine = false,
                         variant = Variant.SEMICOLON})
         end
   in
      case to of
         NONE => doit SourcePos.bogus
       | SOME (t, tok) =>
            if (not (Terminal.equals (t, Terminal.AUTO_SEMICOLON))
                andalso Token.startsLine tok)
               orelse Terminal.equals (t, Terminal.RBRACE)
               then doit (Token.left tok)
            else NONE
   end

val parse =
   let
      val p = Promise.lazy (fn () => Parser.parse Program)
   in
      fn (ts: Token.t Stream.t) =>
      p () {insert = insert,
            stream = Stream.map (ts, fn t => (Terminal.fromToken t, t))}
   end

end
