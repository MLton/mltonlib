(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature EXT_GENERIC_DOM = sig
   structure Ext : EXT_GENERIC
   structure New : GENERIC
end

signature EXT_GENERIC_COD = sig
   structure Ext : EXT_GENERIC
   structure Gen : GENERIC
      where type 'a Index.t = ('a, Unit.t) Ext.Index.t
      where type 'a Index.s = ('a, Unit.t) Ext.Index.s
      where type ('a, 'k) Index.p = ('a, 'k, Unit.t) Ext.Index.p
end
