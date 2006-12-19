(* Edge-triggered *)
structure EPoll :> EPOLL =
  struct
    type poll = MLRep.Int.Signed.int
    type ioh = IoEvent.ioh
    
    fun create events = F_epoll_create.f (MLRep.Int.Signed.fromInt events)
    fun close epoll = ignore (F_close.f epoll)
    
    fun ctl cmd (epoll, fd) = 
      let
        open E_EPOLL_EVENTS
        val makeUnsigned = MLRep.Int.Unsigned.fromInt o MLRep.Int.Signed.toInt
        val flags = makeUnsigned (e_EPOLLIN + e_EPOLLOUT + e_EPOLLERR + 
                                  e_EPOLLHUP + e_EPOLLET)
        val epoll_event = C.new S_epoll_event.typ
      in
        C.Set.uint (S_epoll_event.f_events epoll_event, flags);
        C.Set.sint (U_epoll_data.f_fd (S_epoll_event.f_data epoll_event),
                    MLRep.Int.Signed.fromInt fd);
        F_epoll_ctl.f (epoll, cmd, MLRep.Int.Signed.fromInt fd, 
                       C.Ptr.|&| epoll_event);
        C.discard epoll_event
      end
    
    val add    = ctl E_EPOLL_CTL.e_CTL_ADD
    val remove = ctl E_EPOLL_CTL.e_CTL_DEL
    
    val nevents = 500
    val events = C.alloc S_epoll_event.typ (Word.fromInt nevents)
    
    fun wait (epoll, time) = 
      let
        val roundup = Time.fromMicroseconds 999
        val delay = case time of
            NONE => ~1
          | SOME x => LargeInt.toInt (Time.toMilliseconds (Time.+ (x, roundup)))
        
        val nevents = F_epoll_wait.f (epoll, events, nevents, delay)
        
        fun event ees =
          let
            open E_EPOLL_EVENTS
            val makeUnsigned = MLRep.Int.Unsigned.fromInt o MLRep.Int.Signed.toInt
            val EPOLLIN  = makeUnsigned e_EPOLLIN
            val EPOLLOUT = makeUnsigned e_EPOLLOUT
            val EPOLLERR = makeUnsigned e_EPOLLERR
            val EPOLLHUP = makeUnsigned e_EPOLLHUP
            
            val fdf = U_epoll_data.f_fd (S_epoll_event.f_data ees)
            val fd = MLRep.Int.Signed.toInt (C.Get.sint fdf)
            val flags = C.Get.uint (S_epoll_event.f_events ees)
            
            fun value bit = MLRep.Int.Unsigned.andb (flags, bit) = bit
            val broken = value EPOLLERR orelse value EPOLLHUP
          in
            IoEvent.notifyHASINPUT  fd (value EPOLLIN  orelse broken);
            IoEvent.notifyCANOUTPUT fd (value EPOLLOUT orelse broken)
          end
        
        fun process i =
          if i = nevents then () else
	  (event (C.Ptr.sub (events, i)); process (i + 1))
      in
        process 0
      end
  end

structure Scheduler = Edge(EPoll)
structure IoEvent = Scheduler.IoEvent
structure Scheduler :> SCHEDULER = Scheduler
