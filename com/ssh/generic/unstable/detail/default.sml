(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generic : sig
   structure Ext : EXT_GENERIC

   include GENERIC_WITH_CONVENIENCE
      where type 'a Index.t = ('a, Unit.t) Ext.Index.t
      where type 'a Index.s = ('a, Unit.t) Ext.Index.s
      where type ('a, 'k) Index.p = ('a, 'k, Unit.t) Ext.Index.p

   include ARBITRARY sharing Ext.Index = Arbitrary
   include DUMMY     sharing Ext.Index = Dummy
   include EQ        sharing Ext.Index = Eq
   include ORD       sharing Ext.Index = Ord
   include SHOW      sharing Ext.Index = Show
   include TYPE_INFO sharing Ext.Index = TypeInfo
end = struct
   structure Ext = ExtGeneric

   structure Ext = WithShow      (Ext) open Ext
   structure Ext = WithTypeInfo  (Ext) open Ext structure TypeInfo = Ext
   structure Ext = WithEq        (Ext) open Ext
   structure Ext = WithOrd       (Ext) open Ext
   structure Ext = WithDummy     (Ext) open Ext

   structure Ext = struct
      structure Outer = Ext
      structure TypeInfo = struct
         open TypeInfo
         structure TypeInfo = Outer.Index
      end
      structure RandomGen = RanQD1Gen
   end

   structure Ext = WithArbitrary (Ext) open Ext

   structure Arbitrary = Ext.Index
   structure Dummy     = Ext.Index
   structure Eq        = Ext.Index
   structure Ord       = Ext.Index
   structure Show      = Ext.Index
   structure TypeInfo  = Ext.Index

   structure Grounded = WithConvenience (GroundGeneric (Ext)) open Grounded
end
