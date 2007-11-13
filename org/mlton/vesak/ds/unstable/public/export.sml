(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature HASH_MAP = HASH_MAP
signature LINKED_QUEUE = LINKED_QUEUE
signature NODE = NODE
signature QUEUE = QUEUE
signature UNLINKABLE_LIST = UNLINKABLE_LIST

(** == Exported Structures == *)

structure HashMap : HASH_MAP = HashMap
structure LinkedQueue : LINKED_QUEUE = LinkedQueue
structure Node : NODE = Node
structure UnlinkableList : UNLINKABLE_LIST = UnlinkableList
