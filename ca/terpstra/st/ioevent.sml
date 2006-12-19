structure IoEvent : IOEVENT = 
  struct
    open State
    open SparseArray
    
    type ioh = int
    exception Unmonitored
    
    type status = { 
      hasinput:  bool, 
      canoutput: bool }
    type filedes = {
      fhasinput:  (bool, bool) state * bool signal,
      fcanoutput: (bool, bool) state * bool signal }
    val filedes : filedes sparse_array = new ()

    type 'a t = (unit -> 'a) * ('a -> unit)
    val (geti, _) = _symbol "side_channel_hack" alloc: int t;
    val (_, sets) = _symbol "side_channel_hack": ('a, 'b) Socket.sock t;
    val (_, setd) = _symbol "side_channel_hack": Socket.sock_desc t;
    val (_, setf) = _symbol "side_channel_hack": Posix.IO.file_desc t;
    
    fun socket  sock f = f (sets sock; geti ())
    fun sockdes des  f = f (setd des;  geti ())
    fun file    file f = f (setf file; geti ())
    
    fun test select fd = case sub (filedes, fd) of
        NONE => raise Unmonitored
      | SOME x => case select x of (state, _) => state
    
    val HASINPUT  = test #fhasinput
    val CANOUTPUT = test #fcanoutput
    
    fun notify select fd = case sub (filedes, fd) of
        NONE => raise Unmonitored
      | SOME x => case select x of (_, signal) => signal
    
    val notifyHASINPUT  = notify #fhasinput
    val notifyCANOUTPUT = notify #fcanoutput
    
    fun monitor fd (status:status) =
      let
        val entry = {
          fhasinput  = state (#hasinput  status),
          fcanoutput = state (#canoutput status) }
      in
        update (filedes, fd, entry)
      end
    
    fun unmonitor fd = case sub (filedes, fd) of
        NONE => raise Unmonitored
      | SOME _ => erase (filedes, fd)
  end
