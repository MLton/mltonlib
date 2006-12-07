signature NET = sig

   structure Family: sig
      type inet (* type *)
      type unix (* type *)
      type unknown (* type *)
      type 'a t

      val == : 'a t * 'a t -> bool
      val all: unit -> (unknown t * {name: string}) vector
      val inet: inet t
      val ofString: string -> unknown t option 
      val toString: 'a t -> string
      val unix: unix t
   end

   structure Host: sig
      structure Address: sig
         type t
            
         val == : t * t -> bool
         val ofString: string -> t option 
         val toString: t -> string
         val scanner: t Scanner.t
      end
   
      type t
            
      val address: t -> Address.t
      val addresses: t -> Address.t vector
      val aliases: t -> string vector
      val family: t -> Family.unknown Family.t
      val getByAddress: Address.t -> t option
      val getByName: string -> t option
      val name: t -> string
   end

   structure Protocol: sig
      type t
         
      val aliases: t -> string vector
      val getByName: string -> t option
      val getByNumber: int -> t option
      val name: t -> string
      val number: t -> int
   end

   structure Service: sig
      type t

      val aliases: t -> string vector
      val getByName: string * string option -> t option
      val getByPort: int * string option -> t option 
      val name: t -> string
      val port: t -> int
      val protocol: t -> string
   end

   val getHostName: unit -> string

   structure Socket: sig

      type ('family, 'type) t
      (**
       * A socket of a particular address family and type.
       * family is "inet", "unix", or "unknown".
       * type is "dgram", "active stream", or "passive stream".  See the Type
       * substructure.
       *)
      type ('a, 'b) sock = ('a, 'b) t
      (**
       * An alias so that substructures can refer to the socket type.
       *)

      structure Address: sig
         type 'a t
         (**
          * A socket address.
          * 'a is a family (inet, unix, or unknown).
          *)

         val == : 'a t * 'a t -> bool
         val family: 'a t -> 'a Family.t
         val inet: {port: int} -> Family.inet t
         val ofHost: Host.Address.t * {port: int} -> Family.inet t
         val ofUnix: string -> Family.unix t
         val toHost: Family.inet t -> Host.Address.t * {port: int}
         val toUnix: Family.unix t -> string
      end

      structure Block: sig
         type ('a, 'b, 'c) t
         type ('a, 'b) u = ('a, 'a option, 'b) t

         val may: ('a, 'b, 'a) t
         val non: ('a, 'b, 'b) t
      end

      structure Desc: sig
         type t

         val == : t * t -> bool
      end

      structure Type: sig
         type dgram (* type *)
         type passive (* mode *)
         type active (* mode *)
         type 'mode stream (* type *)
         type activeStream = active stream
         type passiveStream = passive stream
         type unknown
         type 'a t
         (**
          * 'a is dgram, active stream, passive stream, or unknown.
          *)

         val == : 'a t * 'a t -> bool
         val all: unit -> (unknown t * {name: string}) vector
         val dgram: dgram t
         val ofString: string -> unknown t option
         val stream: 'mode stream t
         val toString: 'a t -> string
      end

      structure Option: sig
         type ro (* read only *)
         type rw (* read write *)
         type ('a, 'b, 'c, 'd) t
         (**
          * A socket option, as in {get,set}sockopt.
          * It is only applicable to a ('b, 'c) sock.
          * 'd is either "ro" or "rw".  setOpt can only be called if 'd is rw.
          *)

         val atmark: (bool, 'a, Type.activeStream, ro) t
         val broadcast: (bool, 'a, 'b, rw) t
         val debug: (bool, 'a, 'b, rw) t
         val dontRoute: (bool, 'a, 'b, rw) t
         val error: (bool, 'a, 'b, ro) t
         val keepAlive: (bool, 'a, 'b, rw) t
         val linger: (Time.t option, 'a, 'b, rw) t
         val noDelay: (bool, Family.inet, 'a Type.stream, rw) t
         val nRead: (int, 'a, 'b, ro) t
         val oobInline: (bool, 'a, 'b, rw) t
         val rcvBuf: (int, 'a, 'b, rw) t
         val reuseAddr: (bool, 'a, 'b, rw) t
         val sndBuf: (int, 'a, 'b, rw) t
         val ty: (Type.unknown Type.t, 'a, 'b, ro) t
      end

      structure ShutdownMode: sig
         type t

         val noRecvs: t
         val noSends: t
         val noRecvsSends: t
      end

      structure Receive: sig
         
         structure Block: sig
            type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) t

            val may: ('a,
                      'b, 'c, 'd, 'e,
                      'b, 'c, 'd, 'e) t
            val non: ('a,
                      'b, 'c, 'd, 'e,
                      'b option, 'c option, 'd option, 'e option) t
         end
      
         structure Flag: sig
            type t
               
            val peek: t
            val outOfBand: t
         end
      
         structure From: sig
            type ('a, 'b, 'c, 'd) t
               
            val dgram: ('a, Type.dgram) sock -> ('a, 'b, 'c, 'b) t
            val stream: ('a, Type.activeStream) sock -> ('a, 'b, 'c, 'c) t
         end
      
         structure To: sig
            type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t

            val array: Word8.t ArraySlice.t -> ('a, 'b, 'c, 'd, 'e, 'b, 'c) t
            val vector: {numBytes: int} -> ('a, 'b, 'c, 'd, 'e, 'd, 'e) t
         end
      
         val receive:
            ('a, 'f, 'g, 'h) From.t
            * ('a, 'b, 'c, 'd, 'e, 'f, 'g) To.t
            * ('a,
               int * 'a Address.t,
               int,
               Word8.t vector * 'a Address.t,
               Word8.t vector,
               'b, 'c, 'd, 'e) Block.t
            * Flag.t list
            -> 'h
      (**
       * receive (from, to, block, flags) reads data from the socket
       * specified by "from", storing the bytes read as specified by "to",
       * blocking according to "block".
       * receive encompasses 16 functions of the basis library:
       *   Socket.recv{Arr,Vec}{,From}{,NB}{,'}
       * receive has the same static guarantees regarding return value as
       * the basis library.  In particular, the return type of receive
       * depends on block in the following way:
       * from   to     block return type
       * ------ ------ ----- -----------------
       * dgram  array  may   int * 'a Address.t
       * dgram  array  non   (int * 'a Address.t) option
       * dgram  vector may   Word8.t vector * 'a addr
       * dgram  vector non   (Word8.t vector * 'a Address.t) option
       * stream array  may   int
       * stream array  non   int option
       * stream vector may   Word8.t vector
       * stream vector non   Word8.t vector option
       *)
      end

      structure Send: sig

         structure Block: sig
            type ('a, 'b1, 'c1, 'b2, 'c2, 'b3, 'c3) t

            val may: ('a, 'b1, 'c1, 'b2, 'c2, 'b1, 'c1) t
            val non: ('a, 'b1, 'c1, 'b2, 'c2, 'b2, 'c2) t
         end

         structure From: sig
            type t

            val array: Word8.t ArraySlice.t -> t
            val vector: Word8.t VectorSlice.t -> t
         end

         structure To: sig
            type ('a, 'b, 'c, 'd) t

            val dgram:
               ('a, Type.dgram) sock * 'a Address.t -> ('a, 'b, 'c, 'b) t
            val stream: ('a, Type.activeStream) sock -> ('a, 'b, 'c, 'c) t
         end

         structure Flag: sig
            type t
               
            val dontRoute: t
            val outOfBand: t
         end
      
         val send:
            From.t
            * ('a, 'b, 'c, 'd) To.t
            * ('a, unit, int, bool, int option, 'b, 'c) Block.t
            * Flag.t list
            -> 'd
      (**
       * send (dest, bytes, block, flags) sends the bytes to the socket
       * described by dest, blocking according to block.
       * send encapsulates 16 functions of the standard basis library:
       *   Socket.send{Arr,Vec}{,To}{,NB}{,'}
       * send has the same static guarantees regarding return value as the
       * basis library.  In particular, the return type of send depends on
       * the values of dest and block in the following way:
       *
       * dest   block return type
       * ------ ----- -----------
       * dgram   may  unit
       * dgram   non  bool
       * stream  may  int
       * stream  non  int option
       *)
      end

      val accept:
         ('a, Type.passiveStream) t
         * (('a, Type.activeStream) t * 'a Address.t, 'b) Block.u
         -> 'b
      val bind: ('a, 'b) t * 'a Address.t -> unit
      val close: ('a, 'b) t -> unit
      val connect: ('a, 'b) t * 'a Address.t * (unit, 'c) Block.u -> 'c
      val desc: ('a, 'b) t -> Desc.t
      val getOption: ('a, 'b) t * ('c, 'a, 'b, 'd) Option.t -> 'c
      val ioDesc: ('a, 'b) t -> IoDesc.t
      val listen: ('a, Type.passiveStream) t * int -> unit
      val make: 'a Family.t * 'b Type.t * {protocol: int} option -> ('a, 'b) t
      val makePair: ('a Family.t * 'b Type.t * {protocol: int} option
                     -> ('a, 'b) t * ('a, 'b) t)
      val myAddress: ('a, 'b) t -> 'a Address.t
      val peerAddress: ('a, 'b) t -> 'a Address.t
      val select: ({exs: Desc.t list,
                    rds: Desc.t list,
                    timeout: Time.t option,
                    wrs: Desc.t list}
                   -> {exs: Desc.t list,
                       rds: Desc.t list,
                       wrs: Desc.t list})
      val setOption: ('a, 'b) t * ('c, 'a, 'b, Option.rw) Option.t * 'c -> unit
      val shutdown: ('a, 'b Type.stream) t * ShutdownMode.t -> unit
   end

end
