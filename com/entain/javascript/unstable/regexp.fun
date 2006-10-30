(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor Regexp (S: REGEXP_STRUCTS): REGEXP =
struct

open S

datatype t = T of {body: string,
                   flags: string}

fun equals (T {body = b, flags = f}, T {body = b', flags = f'}) =
   b = b' andalso f = f'

val make = T

fun toString (T {body, flags}) = concat ["/", body, "/", flags]

val layout = Layout.str o toString

end
