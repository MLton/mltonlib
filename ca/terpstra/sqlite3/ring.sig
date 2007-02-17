signature RING =
   sig
      (* handle to a link in the ring *)
      type 'a t
      
      (* Create a new ring with just this one element *)
      val new: 'a -> 'a t
      
      (* Add a value to the ring, get a handle to the link *)
      val add: 'a * 'a t -> 'a t
      
      (* Remove a link from the ring, it is in a new ring *)
      val remove: 'a t -> unit
      
      (* Run the method over all links in the ring *)
      val fold: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
      
      (* Retrieve the value in this link *)
      val get: 'a t -> 'a
   end
