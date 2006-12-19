signature EPOLL =
  sig
    type poll
    type ioh = IoEvent.ioh
    
    val create: int -> poll
    val close: poll -> unit
    
    (* Track changes to state of the io handle *)
    val add:    poll * ioh -> unit
    val remove: poll * ioh -> unit
    
    (* will automatically change IoEvent's status *)
    val wait: poll * Time.time option -> unit
  end
