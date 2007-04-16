structure Async: ASYNC = struct
   exception Closed
   exception Full

   structure Queue:> sig
      type 'a t

      val deque: 'a t -> 'a Option.t
      val enque: 'a t -> 'a -> Unit.t
      val new: Unit.t -> 'a t
   end = struct
      structure Node = struct
         datatype 'a t = T of ('a * 'a t) Option.t Ref.t
            
         fun new () = T (ref None)
      end
   
      datatype 'a t = T of {back: 'a Node.t Ref.t,
                            front: 'a Node.t Ref.t}

      fun new () =
         let
            val n = Node.new ()
         in
            T {back = ref n, front = ref n}
         end

      fun enque (T {back, ...}) = fn a =>
         let
            val Node.T r = !back
            val n = Node.new ()
         in
            r := Some (a, n);
            back := n
         end

      fun deque (T {front, ...}) =
         let
            val Node.T r = !front
         in
            Option.map (!r, fn (a, n) => (front := n; a))
         end
   end

   val todo = Queue.new ()

   fun schedule (f, v) = Queue.enque todo (fn () => f v)

   fun runHandlers () =
      case Queue.deque todo of
         None => ()
       | Some t => (t (); runHandlers ())

   structure Deferred = struct
      datatype 'a v = Filled of 'a | Unfilled of ('a -> Unit.t) List.t
      datatype 'a t = T of 'a v Ref.t

      fun empty () = T (ref (Unfilled []))
         
      fun full x = T (ref (Filled x))

      val return = full

      fun fill (T r, v) =
         case !r of
            Filled _ => raise Full
          | Unfilled fs => (r := Filled v; List.for (fs, pass v))

      fun upon (T r, f) =
         case !r of
            Filled v => schedule (f, v)
          | Unfilled fs => r := Unfilled (f :: fs)

      fun >>= (d, f) = let
         val d' = empty ()
         val () = upon (d, fn a => upon (f a, fn b => fill (d', b)))
      in
         d'
      end
   end

   val upon = Deferred.upon

   structure Ivar = struct
      datatype 'a t = T of 'a Deferred.t

      fun new () = T (Deferred.empty ())
               
      fun fill (T d, v) = Deferred.fill (d, v)

      fun read (T d) = d
   end
      
   structure Stream = struct
      datatype 'a t = T of ('a * 'a t) Option.t Ivar.t

      fun new () = T (Ivar.new ())

      fun read (T i) = Ivar.read i

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

      fun close t = fill (t, None) handle Full => raise Closed

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

   structure Multicast = struct
      datatype 'a t = T of 'a Stream.t Ref.t

      fun new () = T (ref (Stream.new ()))

      fun listen (T r) = !r

      fun send (t as T r, v) = r := Stream.extend (!r, v)

      fun close (T r) = Stream.close (!r)
   end

   structure Handler = struct
      datatype 'a t = T of {handler: 'a -> Unit.t,
                            isScheduled: Bool.t Ref.t}

      fun new f =
         T {handler = f,
            isScheduled = ref false}

      val ignore = fn () => new ignore

      fun isScheduled (T {isScheduled = h, ...}) = !h

      val schedule = fn (T {handler, isScheduled}: 'a t, a: 'a) =>
         if !isScheduled then
            die "Handler.schedule of handler that isScheduled"
         else
            (isScheduled := true; schedule (handler, a))

      fun maybeSchedule (h, a) = if isScheduled h then () else schedule (h, a)

      fun precompose (T {handler, isScheduled}, f) =
         T {handler = handler o f,
            isScheduled = isScheduled}
   end

   structure Event = struct
      datatype 'a t = T of 'a Handler.t -> Unit.t
      (* Invariant: we never pass a Handler that isScheduled *)

      fun send (T f, h) = f h

      fun commit t = let
         val i = Ivar.new ()
         val () = send (t, Handler.new (fn v => Ivar.fill (i, v)))
      in
         Ivar.read i
      end

      fun >>= (t, f) =
         T (fn Handler.T {handler, isScheduled} =>
            send (t,
                  Handler.T
                  {handler = fn a => upon (commit (f a), handler),
                   isScheduled = isScheduled}))
   
      fun any ts =
         T (fn h =>
            List.recur (ts, (), ignore, fn (t, (), k) =>
                        if Handler.isScheduled h then
                           ()
                        else
                           (send (t, h); k ())))

      fun always a = T (fn h => Handler.maybeSchedule (h, a))

      val return = always
         
      fun never () = T ignore        

      fun map (t, f) = T (fn h => send (t, Handler.precompose (h, f)))
   end

   structure Channel = struct
      datatype 'a t = T of {givers: ('a * Unit.t Handler.t) Multicast.t,
                            takers: 'a Handler.t Multicast.t}

      fun 'a new () = let
         val givers: ('a * Unit.t Handler.t) Multicast.t = Multicast.new ()
         val takers = Multicast.new ()
         fun loop (gs, ts) =
            upon (Stream.read gs, fn opt =>
                  Option.for (opt, fn (g, gs) => loopG (g, gs, ts)))
         and loopG (g, gs, ts) =
            upon (Stream.read ts, fn opt =>
                  Option.for (opt, fn (t, ts) => loopGT (g, gs, t, ts)))
         and loopT (gs, t, ts) =
            upon (Stream.read gs, fn opt =>
                  Option.for (opt, fn (g, gs) => loopGT (g, gs, t, ts)))
         and loopGT (g as (a, gh), gs, t, ts) =
            case (Handler.isScheduled gh, Handler.isScheduled t) of
               (false, false) =>
                  (Handler.schedule (gh, ());
                   Handler.schedule (t, a);
                   loop (gs, ts))
             | (false, true) => loopG (g, gs, ts)
             | (true, false) => loopT (gs, t, ts)
             | (true, true) => loop (gs, ts)
         val () = loop (Multicast.listen givers, Multicast.listen takers)
      in
         T {givers = givers,
            takers = takers}
      end

      fun give (T {givers, ...}, a) =
         Event.T (fn h => Multicast.send (givers, (a, h)))

      fun take (T {takers, ...}) =
         Event.T (fn h => Multicast.send (takers, h))
   end

   structure Mailbox = struct
      open Channel

      fun send (T {givers, ...}, a) =
         Multicast.send (givers, (a, Handler.ignore ()))
   end
end
