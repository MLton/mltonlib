(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val conn = Client.Conn.byName {host = "127.0.0.1", port = 45678}

local
   fun mk s = verbose "client: " s (Client.Reply.sync o Client.declare s conn)
in
   val {bind, bindings, find} =
       mkLib {bind = mk, bindings = mk, find = mk}
end

val () =
    (find "x" >| ignore
   ; bind ("x", 1234)
   ; find "x" >| ignore
   ; bindings () >| ignore)

val () = Client.Conn.close conn
