(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 * Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Signature specifying an abstract non-parameterized type {t}. *)
signature T = sig type t end

(** Signature specifying an abstract type {t} with 1 type parameter. *)
signature T'1 = sig type 'a t end

(** Signature specifying an abstract type {t} with 2 type parameters. *)
signature T'2 = sig type ('a, 'b) t end
