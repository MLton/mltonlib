(* Signature for level-triggered poll *)
signature LPOLL =
  sig
    type poll
    type ioh = IoEvent.ioh
    datatype level = HASINPUT | CANOUTPUT
    
    val create: int -> poll
    val close: poll -> unit
    
    (* add a watch to the list *)
    val watch: poll * ioh * level -> unit
    
    (* called prior to closing the io handle *)
    val unwatchall: poll * ioh -> unit
    
    (* automatically change IoEvent's status
     * triggered watches are automatically removed from the poll (ie: oneshot)
     *)
    val wait: poll * Time.time option -> unit
  end
