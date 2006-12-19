structure T = Automata(Alphabet)
structure DFA = T.Deterministic
structure NFA = T.NonDeterministic
structure E = T.Expression
structure RE = T.RegularExpression
open E

val exp = (RE.toExpression o RE.fromString o hd o CommandLine.arguments) ()
val s = toDFA exp
val () = print (DFA.toDot ("dotfile", s))
