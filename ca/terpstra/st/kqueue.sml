structure KQueue :> EPOLL =
  struct
    type poll = MLRep.Int.Signed.int
    type ioh = IoEvent.ioh
    
    fun create _ = F_kqueue.f ()
    fun close epoll = ignore (F_close.f epoll)
    
(*
	    val () = print ("change: " ^ Int.toString fd ^ ": ")
	    val () = print (Int.toString filter ^ " ")
	    val () = print (Int.toString flags)
	    val () = print "\n"
*)
    fun kevent (ke, fd, filter, flags) =
      (C.Set.ulong  (S_kevent.f_ident  ke, 
		     MLRep.Long.Unsigned.fromInt fd);
       C.Set.sshort (S_kevent.f_filter ke, 
		     MLRep.Short.Signed.fromInt 
			(MLRep.Int.Signed.toInt filter));
       C.Set.ushort (S_kevent.f_flags  ke, 
		     MLRep.Short.Unsigned.fromInt 
			(MLRep.Int.Signed.toInt flags)))
    
    fun control flags (epoll, fd) = 
      let
        val changes = C.alloc S_kevent.typ (Word.fromInt 2)
        val zero = C.new S_timespec.typ
      in
        kevent (C.Ptr.sub (changes, 0), fd, E_filter.e_read,  flags);
        kevent (C.Ptr.sub (changes, 1), fd, E_filter.e_write, flags);
        C.Set.slong (S_timespec.f_tv_sec zero, 0);
        C.Set.slong (S_timespec.f_tv_nsec zero, 0);
        F_kevent.f (epoll, 
                    C.Ptr.ro changes, 2, 
                    C.Ptr.null (C.T.pointer S_kevent.typ), 0, 
                    C.Ptr.ro (C.Ptr.|&| zero));
        C.discard zero;
        C.free changes
      end
    
    val add    = control (E_action.e_add + E_action.e_clear)
    val remove = control E_action.e_delete
    
    val nevents = 500
    val events = C.alloc S_kevent.typ (Word.fromInt nevents)
    
    fun event ke =
	let
	    val fd = C.Get.ulong  (S_kevent.f_ident ke)
	    val io = C.Get.sshort (S_kevent.f_filter ke)
	    
	    val fd = MLRep.Long.Unsigned.toInt fd
	    
	    val cvt = MLRep.Short.Signed.fromInt o MLRep.Int.Signed.toInt
	    val read  = cvt E_filter.e_read
	    val write = cvt E_filter.e_write
(*
	    val () = print ("event: " ^ Int.toString fd ^ ":")
	    val () = if io = read  then print " read"  else ()
	    val () = if io = write then print " write" else ()
	    val () = print "\n"
*)
	in
	    if io = read  then IoEvent.notifyHASINPUT  fd true else ();
	    if io = write then IoEvent.notifyCANOUTPUT fd true else ()
	end
    
    fun wait (epoll, time) = 
	let
            fun timespec NONE = C.Ptr.null (C.T.pointer S_timespec.typ)
              | timespec (SOME t) = 
	          let
	             val ts = C.alloc S_timespec.typ (Word.fromInt 1)
                     val (seconds, nano) = 
                         IntInf.quotRem (Time.toNanoseconds t, 1000000000)
	          in
	             C.Set.slong (S_timespec.f_tv_sec  (C.Ptr.|*| ts), 
		                  MLRep.Long.Signed.fromLarge seconds);
	             C.Set.slong (S_timespec.f_tv_nsec (C.Ptr.|*| ts), 
			          MLRep.Long.Signed.fromLarge nano);
		     ts
		  end
	    val ts = timespec time
	    
	    val changes = C.Ptr.ro (C.Ptr.null (C.T.pointer S_kevent.typ))
	    val nevents = F_kevent.f (MLRep.Int.Signed.fromInt epoll, 
				      changes, 0,
				      events, nevents, 
				      C.Ptr.ro ts)
	    fun process i =
	        if i = nevents then () else
	        (event (C.Ptr.sub (events, i)); process (i + 1))
	in
	    process 0;
	    if C.Ptr.isNull ts then () else C.free ts
	end
  end
  
structure Scheduler = Edge(KQueue)
structure IoEvent   :> IOEVENT   = Scheduler.IoEvent
structure Scheduler :> SCHEDULER = Scheduler
