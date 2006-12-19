signature IOEVENT =
  sig
    exception Unmonitored
    
    type status = { hasinput: bool, canoutput: bool}
    type ioh
    
    val socket:  ('af, 'sock_type) Socket.sock -> (ioh -> 'a) -> 'a
    val sockdes: Socket.sock_desc -> (ioh -> 'a) -> 'a
    val file:    Posix.IO.file_desc -> (ioh -> 'a) -> 'a
    
    val HASINPUT:  ioh -> (bool, bool) State.state
    val CANOUTPUT: ioh -> (bool, bool) State.state
    
    val notifyHASINPUT:  ioh -> bool State.signal
    val notifyCANOUTPUT: ioh -> bool State.signal
    
    val monitor: ioh -> status -> unit
    val unmonitor: ioh -> unit
  end
