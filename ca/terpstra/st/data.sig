signature SPARSE_ARRAY =
  sig
    type 'a sparse_array
    
    val new: unit -> 'a sparse_array
    
    val sub: 'a sparse_array * int -> 'a option
    val update: 'a sparse_array * int * 'a -> unit
    val erase: 'a sparse_array * int -> unit
  end

signature DYNAMIC_ARRAY =
  sig
    type 'a dynamic_array
    
    val new: unit -> 'a dynamic_array
    val size: 'a dynamic_array -> int
    
    val sub: 'a dynamic_array * int -> 'a
    val update: 'a dynamic_array * int * 'a -> unit
    val swap: 'a dynamic_array * int * int -> unit
    
    val push: 'a dynamic_array * 'a -> unit
    val pop: 'a dynamic_array -> unit
  end

signature HEAP =
  sig
    type 'a heap
    val new: ('a * 'a -> bool) -> 'a heap
    val push: 'a heap * 'a -> unit
    val pop: 'a heap -> unit
    val peek: 'a heap -> 'a option
  end

signature QUEUE =
  sig
    type 'a queue
    val new: unit -> 'a queue
    
    val empty: 'a queue -> bool
    val enque: 'a queue * 'a -> unit
    val deque: 'a queue -> 'a option
    
(*    val enqueList: 'a queue * 'a list -> unit *)
 end
