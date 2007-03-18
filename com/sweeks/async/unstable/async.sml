structure Async: ASYNC = struct
   exception Full

   val todo = ref []

   fun schedule (f, v) = todo := (fn () => f v) :: !todo

   fun runHandlers () =
      case !todo of
         [] => ()
       | t :: ts => (todo := ts; t (); runHandlers ())

   structure Deferred = struct
      datatype 'a v = Filled of 'a | Unfilled of ('a -> Unit.t) List.t
      datatype 'a t = T of 'a v Ref.t

      fun upon (T r, f) =
         case !r of
            Filled v => schedule (f, v)
          | Unfilled fs => r := Unfilled (f :: fs)
   end

   val upon = Deferred.upon

   structure Ivar = struct
      open Deferred

      fun new () = T (ref (Unfilled []))
               
      fun fill (T r, v) =
         case !r of
            Filled _ => raise Full
          | Unfilled fs => (r := Filled v; List.for (fs, pass v))

      val read = id
   end
      
   structure Stream = struct
      datatype 'a t = T of ('a * 'a t) Option.t Ivar.t

      fun new () = T (Ivar.new ())

      fun read (T d) = Ivar.read d

      val recur = fn (t, b, done, step) =>
         recur ((t, b), fn ((t, b), loop) =>
                upon (read t,
                      fn None => done b
                       | Some (a, t) => step (a, b, fn b => loop (t, b))))
         
      fun fold (t, b, f) = let
         val i = Ivar.new ()
         val () = recur (t, b, fn b => Ivar.fill (i, b),
                         fn (a, b, k) => k (f (a, b)))
      in
         Ivar.read i
      end

      fun for (t, f) = fold (t, (), f o #1)

      fun fill (T i, v) = Ivar.fill (i, v)

      fun close t = fill (t, None)

      fun extend (t, v) = let
         val t' = new ()
         val () = fill (t, Some (v, t'))
      in
         t'
      end
                        
      fun map (t, f) = let
         val t' = new ()
         val () = recur (t, t', fn t' => fill (t', None),
                         fn (a, t', k) => k (extend (t', f a)))
      in
         t'
      end
   end

   structure Tail = struct
      datatype 'a t = T of 'a Stream.t Ref.t

      fun new () = T (ref (Stream.new ()))

      fun extend (t as T r, v) = r := Stream.extend (!r, v)

      fun close (T r) = Stream.close (!r)
   end

   structure Multicast = struct
      open Tail

      fun reader (T r) = !r

      val send = extend
   end
end
