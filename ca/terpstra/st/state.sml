structure State :> STATE =
  struct
    type ('val, 'diff) state = {
      value: unit -> 'val,
      release: unit -> unit,
      watch: ('val * 'diff -> unit) -> unit
      }
    type 'diff signal = 'diff -> unit
    exception RaceCondition
    exception UnWatched
    
    fun delta update init =
      let
        val state = ref init
        val block = ref NONE
        
        fun value () = !state
        fun release () = 
          case !block of
              NONE => raise UnWatched
            | SOME _ => block := NONE
        fun watch f =
          case !block of
              NONE => block := SOME f
            | SOME _ => raise RaceCondition
        fun signal diff =
          case update (!state, diff) of
              NONE => ()
            | SOME newval =>
                case !block of
                    NONE => state := newval
                  | SOME f => (state := newval; f (newval, diff))
      in
        ({ value = value, release = release, watch = watch }, signal)
      end
    fun state init = delta (fn (s, d) => if s = d then NONE else SOME d) init
    
    fun value    { value, release=_, watch=_ } = value ()
    fun release  { value=_, release, watch=_ } = release ()
    fun dwatch f { value=_, release=_, watch } = watch f
    fun swatch f = dwatch (fn (x, _) => f x)
    
    fun dmap (valmap, diffmap) state =
      let
        val valproxy = ref NONE
        val block = ref NONE
        
        fun proxy (val1, diff1) =
          case diffmap (val1, diff1, valOf (!valproxy)) of
              NONE => ()
            | SOME (newval2, diff2) =>
                case !block of
                    NONE => valproxy := SOME newval2
                  | SOME f => (valproxy := SOME newval2; f (newval2, diff2))
        
        val watch = fn f =>
          case !block of
              NONE => (dwatch proxy state; (* first b/c it might raise *)
                       block := SOME f; 
                       valproxy := SOME (valmap (value state)))
            | SOME _ => raise RaceCondition
        val value = fn () =>
          case !valproxy of
              NONE => valmap (value state)
            | SOME x => x
        val release = fn () =>
          case !block of
              NONE => raise UnWatched
            | SOME _ => (release state; block := NONE; valproxy := NONE)
      in
        { value = value, release = release, watch = watch }
      end
    fun smap valmap = 
      dmap (valmap, fn (v, _, _) => let val v2 = valmap v in SOME (v2, v2) end)
    
    datatype ('diff1, 'diff2) alt = DIFF1 of 'diff1 | DIFF2 of 'diff2
    fun dcompose (state1, state2) =
      let
        val block = ref NONE
        fun proxy1 (val1, diff1) =
          (valOf (!block)) ((val1, value state2), DIFF1 diff1)
        fun proxy2 (val2, diff2) =
          (valOf (!block)) ((value state1, val2), DIFF2 diff2)
        
        val watch = fn f =>
          case !block of
              NONE => (
                dwatch proxy1 state1;
                (dwatch proxy2 state2 handle ex => (release state1; raise ex));
                block := SOME f)
            | SOME _ => raise RaceCondition
        val value = fn () =>
          (value state1, value state2)
        val release = fn () =>
          case !block of
              NONE => raise UnWatched
            | SOME _ => (release state1; release state2; block := NONE)
      in
        { value = value, release = release, watch = watch }
      end
      
    fun scompose (state1, state2) =
      let
        val block = ref NONE
        fun proxy1 (val1, diff1) =
          let val val2 = value state2 in
          (valOf (!block)) ((val1, val2), (val1, val2)) end
        fun proxy2 (val2, diff2) =
          let val val1 = value state1 in
          (valOf (!block)) ((val1, val2), (val1, val2)) end
        
        val watch = fn f =>
          case !block of
              NONE => (
                dwatch proxy1 state1;
                (dwatch proxy2 state2 handle ex => (release state1; raise ex));
                block := SOME f)
            | SOME _ => raise RaceCondition
        val value = fn () =>
          (value state1, value state2)
        val release = fn () =>
          case !block of
              NONE => raise UnWatched
            | SOME _ => (release state1; release state2; block := NONE)
      in
        { value = value, release = release, watch = watch }
      end
      
      (* !!! fixme *)
      type ('val, 'diff) broadcast = ('val, 'diff) state
      fun broadcast state = state
      fun clone broadcaster = broadcaster
  end
