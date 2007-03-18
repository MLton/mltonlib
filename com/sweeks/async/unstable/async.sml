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

   structure Tail = struct
      datatype 'a t = T of 'a Stream.t Ref.t

      fun toStream (T r) = !r

      fun new () = T (ref (Stream.new ()))

      fun extend (t as T r, v) = r := Stream.extend (!r, v)

      fun close (T r) = Stream.close (!r)
   end

   structure Multicast = struct
      open Tail

      fun reader (T r) = !r

      val send = extend
   end

   structure Handler: sig
      type 'a t

      val ignore: Unit.t -> 'a t
      val isScheduled: 'a t -> Bool.t
      val new: ('a -> Unit.t) -> 'a t
      val maybeSchedule: 'a t * 'a -> Unit.t
      val precompose: 'a t * ('b -> 'a) -> 'b t
      (* It is an error to call Handler.schedule h if Handler.isScheduled h.
       *)
      val schedule: 'a t * 'a -> Unit.t
   end = struct
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

      fun any ts =
         T (fn h =>
            List.recur (ts, (), ignore, fn (t, (), k) =>
                        if Handler.isScheduled h then
                           ()
                        else
                           (send (t, h); k ())))

      fun always a = T (fn h => Handler.maybeSchedule (h, a))
         
      fun never () = T ignore

      fun map (t, f) = T (fn h => send (t, Handler.precompose (h, f)))
   end

   structure Channel = struct
      datatype 'a t = T of {givers: ('a * Unit.t Handler.t) Tail.t,
                            takers: 'a Handler.t Tail.t}

      fun 'a new () = let
         val givers: ('a * Unit.t Handler.t) Tail.t = Tail.new ()
         val takers = Tail.new ()
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
         val () = loop (Tail.toStream givers, Tail.toStream takers)
      in
         T {givers = givers,
            takers = takers}
      end

      fun give (T {givers, ...}, a) =
         Event.T (fn h => Tail.extend (givers, (a, h)))

      fun take (T {takers, ...}) =
         Event.T (fn h => Tail.extend (takers, h))
   end

   structure Mailbox = struct
      open Channel

      fun send (T {givers, ...}, a) =
         Tail.extend (givers, (a, Handler.ignore ()))
   end
end
