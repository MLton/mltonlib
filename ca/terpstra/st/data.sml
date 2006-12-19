structure SparseArray :> SPARSE_ARRAY =
  struct
    type 'a sparse_array = 'a option array ref
    
    fun new () = ref (Array.array (8, NONE))
    
    fun sub (ref array, i) =
      if i >= (Array.length array) then NONE else
      Array.sub (array, i)
    
    fun update (array, i, x) = (
      if i < Array.length (!array) then () else
      let val a = Array.array (i*2 + 1, NONE)
      in
        Array.copy { src = !array, dst = a, di = 0 };
        array := a
      end;
      Array.update (!array, i, SOME x))
    
    fun erase (ref array, i) =
      if i >= (Array.length array) then () else
      Array.update (array, i, NONE)
  end

structure DynamicArray :> DYNAMIC_ARRAY =
  struct
    type 'a dynamic_array = 'a option array ref * int ref
    
    fun new () = (ref (Array.array (8, NONE)), ref 0)
    fun size (_, ref length) = length
    
    fun sub ((ref array, _), i) = valOf (Array.sub (array, i))
    fun update ((ref array, _), i, x) = Array.update (array, i, SOME x)
    
    fun swap ((ref array, _), i, j) = 
      let
        val iv = Array.sub (array, i)
        val jv = Array.sub (array, j)
      in
        Array.update (array, i, jv);
        Array.update (array, j, iv)
      end
    
    fun push ((array, length), x) = (
      if Array.length (!array) > !length then () else
      let val a = Array.array (!length * 2, NONE)
      in
        Array.copy { src = !array, dst = a, di = 0 };
        array := a
      end;
      update ((array, length), !length, x);
      length := !length + 1)
    
    fun pop (ref array, length) = (
      length := !length - 1;
      Array.update (array, !length, NONE))
  end

structure Heap :> HEAP =
  struct
    open DynamicArray
    type 'a heap = 'a dynamic_array * ('a * 'a -> bool)
    
    fun left i = 2*i + 1
    fun right i = 2*i + 2
    fun parent i = (i - 1) div 2
    
    fun new cmp = (DynamicArray.new (), cmp)
    
    fun push ((a, cmp), x) =
      let
        fun fixtail 0 = () | fixtail i = 
          let
            val parent = parent i
          in
            if cmp (sub (a, parent), sub (a, i)) then () else
            (swap (a, parent, i); fixtail parent)
          end
      in
        DynamicArray.push (a, x);
        fixtail (size a - 1)
      end
    
    fun pop (a, cmp) =
      let
        val newsize = size a - 1
        
        fun fixhead i =
          let
            val left = left i
            val right = right i
          in
            if left >= newsize then () else
            if right >= newsize then
              if cmp (sub (a, i), sub (a, left)) then () else
              swap (a, i, left)
            else
              if cmp (sub (a, left), sub (a, right)) then
                if cmp (sub (a, i), sub (a, left)) then () else
                (swap (a, i, left); fixhead left)
              else
                if cmp (sub (a, i), sub (a, right)) then () else
                (swap (a, i, right); fixhead right)
          end
      in
        update (a, 0, sub (a, newsize));
        DynamicArray.pop a;
        fixhead 0
      end
    
    fun peek (a, cmp) =
      if size a = 0 then NONE else SOME (sub (a, 0))
  end

structure Queue :> QUEUE =
   struct
      datatype 'a queue = T of {front: 'a list ref, back: 'a list ref}

      fun new() = T{front = ref [], back = ref []}
      
      fun empty (T {front=ref [], back=ref []}) = true
        | empty _ = false
        
      fun enque(T{back, ...}, x) = back := x :: !back

      fun deque(T{front, back}) =
         case !front of
            [] => (case !back of
                      [] => NONE
                    | l => let val l = rev l
                           in case l of
                              [] => raise Fail "deque"
                            | x :: l => (back := []; front := l; SOME x)
                           end)
          | x :: l => (front := l; SOME x) 
   end
