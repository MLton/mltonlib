structure Socket : SOCKET =
  struct
    open Socket
    open State
    open IoEvent
    open Timeout
    open Thread
    
    fun wrapInNB f s x =
      case f x of
          NONE => NONE before socket (s x) notifyHASINPUT false
        | SOME v => SOME v
    
    val recvVecNB  = fn x => wrapInNB recvVecNB  #1 x
    val recvVecNB' = fn x => wrapInNB recvVecNB' #1 x
    val recvArrNB  = fn x => wrapInNB recvArrNB  #1 x
    val recvArrNB' = fn x => wrapInNB recvArrNB' #1 x
    
    val recvVecFromNB  = fn x => wrapInNB recvVecFromNB  #1 x
    val recvVecFromNB' = fn x => wrapInNB recvVecFromNB' #1 x
    val recvArrFromNB  = fn x => wrapInNB recvArrFromNB  #1 x
    val recvArrFromNB' = fn x => wrapInNB recvArrFromNB' #1 x
    
    fun wrapIn f s x = (
      stopTill (socket (s x) HASINPUT);
      case f x of
          NONE => wrapIn f s x
        | SOME x => x)
    
    fun recvVec  x = wrapIn recvVecNB  #1 x
    fun recvVec' x = wrapIn recvVecNB' #1 x
    fun recvArr  x = wrapIn recvArrNB  #1 x
    fun recvArr' x = wrapIn recvArrNB' #1 x
    
    fun recvVecFrom  x = wrapIn recvVecFromNB  #1 x
    fun recvVecFrom' x = wrapIn recvVecFromNB' #1 x
    fun recvArrFrom  x = wrapIn recvArrFromNB  #1 x
    fun recvArrFrom' x = wrapIn recvArrFromNB' #1 x
    
    fun wrapOutNB f s x =
      case f x of
          NONE => NONE before socket (s x) notifyCANOUTPUT false
        | SOME v => SOME v
    
    val sendVecNB  = fn x => wrapOutNB sendVecNB  #1 x
    val sendVecNB' = fn x => wrapOutNB sendVecNB' #1 x
    val sendArrNB  = fn x => wrapOutNB sendArrNB  #1 x
    val sendArrNB' = fn x => wrapOutNB sendArrNB' #1 x
    
    fun wrapOutNBbool f s x =
      case f x of
          false => false before socket (s x) notifyCANOUTPUT false
        | true => true
    
    val sendVecToNB  = fn x => wrapOutNBbool sendVecToNB  #1 x
    val sendVecToNB' = fn x => wrapOutNBbool sendVecToNB' #1 x
    val sendArrToNB  = fn x => wrapOutNBbool sendArrToNB  #1 x
    val sendArrToNB' = fn x => wrapOutNBbool sendArrToNB' #1 x
    
    fun wrapOut f s x = (
      stopTill (socket (s x) CANOUTPUT);
      case f x of
          NONE => wrapOut f s x
        | SOME x => x)
    
    fun sendVec  x = wrapOut sendVecNB  #1 x
    fun sendVec' x = wrapOut sendVecNB' #1 x
    fun sendArr  x = wrapOut sendArrNB  #1 x
    fun sendArr' x = wrapOut sendArrNB' #1 x
    
    fun wrapOutbool f s x = (
      stopTill (socket (s x) CANOUTPUT);
      case f x of
          false => wrapOutbool f s x
        | true => ())
    
    fun sendVecTo  x = wrapOutbool sendVecToNB  #1 x
    fun sendVecTo' x = wrapOutbool sendVecToNB' #1 x
    fun sendArrTo  x = wrapOutbool sendArrToNB  #1 x
    fun sendArrTo' x = wrapOutbool sendArrToNB' #1 x
    
    val acceptNB = fn s => 
      case acceptNB s of
          NONE => NONE before socket s notifyHASINPUT false
        | SOME (s, a) => 
            (* It is safe to say no input, b/c edge triggered APIs always 
             * give at least one initial status report. It is also safe
             * for level triggered, since this gets it added to the poll.
             * Thus, no really fast sends are lost.
             *
             * This is the smart thing to do, because SYN+ACK takes a while
             * to reach the client. So, there's no point wasting a recv()
             * when it's almost surely not going to have data yet anyways.
             *)
            SOME (s, a) before socket s monitor { hasinput  = false, 
                                                  canoutput = true }
    fun accept x = wrapIn acceptNB (fn s => s) x
    
    val close = fn s => (socket s unmonitor; close s)
    
    val listen = fn (s, i) =>
      (* due to a bug in BSD's kqueue API, we must re-monitor *)
      (socket s unmonitor;
       listen (s, i);
       socket s monitor { hasinput = false, canoutput = true })
    
    val connect = fn (s, a) =>
      case connectNB (s, a) of
        true => ()
      | false => (
          stopTill (socket s CANOUTPUT);
          (* Get the error status, if getERROR doesn't raise, we raise
           * something generic since we only know that it failed.
           *)
          if Socket.Ctl.getERROR s
          then raise OS.SysErr ("Connection failed", NONE)
          else ())
    
    fun select {rds, wrs, exs, timeout} = 
      let
        datatype which = 
          RDS of sock_desc | WRS of sock_desc | TIMER
        
        val rds = List.map (fn rd => (sockdes rd HASINPUT,  RDS rd)) rds
        val wrs = List.map (fn wr => (sockdes wr CANOUTPUT, WRS wr)) wrs
        val tmr = case timeout of SOME x => [(TIMEOUT x, TIMER)] | NONE => []
        val events = List.concat [rds, wrs, tmr]
        
        val ords = ref []
        val owrs = ref []
        
        fun split (RDS rd) = ords := rd :: !ords
          | split (WRS wr) = owrs := wr :: !owrs
          | split TIME = ()
      in
        List.app split (Thread.select events);
        {rds = !ords, wrs = !owrs, exs = []}
      end
  end

structure Wrap =
  struct
    local
      open IoEvent
    in
      val monitor = fn s =>
        s before socket s monitor { hasinput = false, canoutput = false }
      
      fun monitorPair (s, t) = (monitor s, monitor t)
    end
  end

structure GenericSock : GENERIC_SOCK =
  struct
    open GenericSock
    open Wrap
    
    val socket  = fn x => monitor (socket  x)
    val socket' = fn x => monitor (socket' x)
    val socketPair  = fn x => monitorPair (socketPair  x)
    val socketPair' = fn x => monitorPair (socketPair' x)
  end

structure INetSock : INET_SOCK =
  struct
    open INetSock
    open Wrap
    
    structure UDP =
      struct
        open UDP
        val socket  = fn x => monitor (socket  x)
        val socket' = fn x => monitor (socket' x)
      end
    
    structure TCP =
      struct
        open TCP
        val socket  = fn x => monitor (socket  x)
        val socket' = fn x => monitor (socket' x)
      end
  end

structure UnixSock : UNIX_SOCK =
  struct
    open UnixSock
    open Wrap
    
    structure Strm =
      struct
        open Strm
        val socket = fn x => monitor (socket x)
        val socketPair = fn x => monitorPair (socketPair x)
      end
      
    structure DGrm =
      struct
        open DGrm
        val socket = fn x => monitor (socket x)
        val socketPair = fn x => monitorPair (socketPair x)
      end
  end
