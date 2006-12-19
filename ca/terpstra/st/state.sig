(* Attempts to classify states as 'level-triggered' or 'edge-triggered'
 * will fail, as these terms make sense only at the intersection of states
 * and blocking primitives. Both styles (and others) can be realized using
 * the watch and value methods.
 * 
 * A given state may only be watched once (1 time in 1 thread).
 * If a second watch is attempted, the RaceCondition exception is raised.
 *)
signature STATE =
  sig
    type ('val, 'diff) state
    type 'diff signal = 'diff -> unit
    
    (* create a new state *)
    val state: ''val -> (''val, ''val) state * ''val signal
    val delta: ('val * 'diff -> 'val option) -> 'val -> 
               ('val, 'diff) state * 'diff signal
    
    (* get the current value of a state *)
    val value: ('val, 'diff) state -> 'val
    
    (* hook a callback invoked when the state changes *)
    exception RaceCondition
    exception UnWatched
    val dwatch: ('val * 'diff -> unit) -> ('val, 'diff) state -> unit
    val swatch: ('val -> unit) -> ('val, 'diff) state -> unit
    val release: ('val, 'diff) state -> unit
    
    (* map this state into a new derived state *)
    val smap: ('val1 -> 'val2) -> 
              ('val1, 'val1) state -> ('val2, 'val2) state
    val dmap: ('val1 -> 'val2) * 
              ('val1 * 'diff1 * 'val2 -> ('val2 * 'diff2) option) ->
              ('val1, 'diff1) state -> ('val2, 'diff2) state
    
    (* compose two states into their product *)
    datatype ('diff1, 'diff2) alt = DIFF1 of 'diff1 | DIFF2 of 'diff2
    val scompose: ('val1, 'val1) state * ('val2, 'val2) state ->
                  ('val1 * 'val2, 'val1 * 'val2) state
    val dcompose: ('val1, 'diff1) state * ('val2, 'diff2) state ->
                  ('val1 * 'val2, ('diff1, 'diff2) alt) state
    
    (* If you want multiple watchers on the same state *)
    type ('val, 'diff) broadcast
    val broadcast: ('val, 'diff) state -> ('val, 'diff) broadcast
    val clone: ('val, 'diff) broadcast -> ('val, 'diff) state
  end
