(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val conn = Client.Conn.byName {host = "127.0.0.1", port = 4321}

local
   fun mk signature' conn =
       Client.Reply.sync o Client.declare signature' conn
in
   val bind = mk (Pair.t (String.t, Int.t), Unit.t, "bind") conn
   val find = mk (String.t, Option.t Int.t, "find") conn
   val bindings =
       mk (Unit.t, List.t (Pair.t (String.t, Int.t)), "bindings") conn
end

fun tell x =
    printlns [x, " => ",
              case find x
               of NONE => "undefined"
                | SOME x => Int.toString x]

val () =
    (tell "x"
   ; bind ("x", 1234)
   ; tell "x"
   ; println (Generic.show (List.t (Pair.t (String.t, Int.t))) (bindings ())))

val () = Client.Conn.close conn
