(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Source = Source ()
structure Regexp = Regexp ()
structure Token = Token (structure Regexp = Regexp)
structure Lex = Lex (structure Source = Source
                     structure Token = Token)
structure Javascript = Javascript (structure Regexp = Regexp)
structure Parse = Parse (structure Javascript = Javascript
                         structure Token = Token)
