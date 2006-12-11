signature NET = sig

   structure Family: sig
      type inet (* type *)
      type unix (* type *)
      type unknown (* type *)
      type 'a t

      val == : 'a t * 'a t -> Bool.t
      val all: Unit.t -> (unknown t * {name: String.t}) vector
      val inet: inet t
      val ofString: String.t -> unknown t Option.t 
      val toString: 'a t -> String.t
      val unix: unix t
   end

   structure Host: sig
      structure Address: sig
         type t
            
         val == : t * t -> Bool.t
         val ofString: String.t -> t Option.t 
         val toString: t -> String.t
         val scanner: t Scanner.t
      end
   
      type t
            
      val address: t -> Address.t
      val addresses: t -> Address.t vector
      val aliases: t -> String.t vector
      val family: t -> Family.unknown Family.t
      val getByAddress: Address.t -> t Option.t
      val getByName: String.t -> t Option.t
      val name: t -> String.t
   end

   structure Protocol: sig
      type t
         
      val aliases: t -> String.t vector
      val getByName: String.t -> t Option.t
      val getByNumber: Int.t -> t Option.t
      val name: t -> String.t
      val number: t -> Int.t
   end

   structure Service: sig
      type t

      val aliases: t -> String.t vector
      val getByName: String.t * String.t Option.t -> t Option.t
      val getByPort: Int.t * String.t Option.t -> t Option.t 
      val name: t -> String.t
      val port: t -> Int.t
      val protocol: t -> String.t
   end

   val getHostName: Unit.t -> String.t

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

         val == : 'a t * 'a t -> Bool.t
         val family: 'a t -> 'a Family.t
         val inet: {port: Int.t} -> Family.inet t
         val ofHost: Host.Address.t * {port: Int.t} -> Family.inet t
         val ofUnix: String.t -> Family.unix t
         val toHost: Family.inet t -> Host.Address.t * {port: Int.t}
         val toUnix: Family.unix t -> String.t
      end

      structure Block: sig
         type ('a, 'b, 'c) t
         type ('a, 'b) u = ('a, 'a Option.t, 'b) t

         val may: ('a, 'b, 'a) t
         val non: ('a, 'b, 'b) t
      end

      structure Desc: sig
         type t

         val == : t * t -> Bool.t
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

         val == : 'a t * 'a t -> Bool.t
         val all: Unit.t -> (unknown t * {name: String.t}) vector
         val dgram: dgram t
         val ofString: String.t -> unknown t Option.t
         val stream: 'mode stream t
         val toString: 'a t -> String.t
      end

      structure Opt: sig
         type ro (* read only *)
         type rw (* read write *)
         type ('a, 'b, 'c, 'd) t
         (**
          * A socket option, as in {get,set}sockopt.
          * It is only applicable to a ('b, 'c) sock.
          * 'd is either "ro" or "rw".  setOpt can only be called if 'd is rw.
          *)

         val atmark: (Bool.t, 'a, Type.activeStream, ro) t
         val broadcast: (Bool.t, 'a, 'b, rw) t
         val debug: (Bool.t, 'a, 'b, rw) t
         val dontRoute: (Bool.t, 'a, 'b, rw) t
         val error: (Bool.t, 'a, 'b, ro) t
         val keepAlive: (Bool.t, 'a, 'b, rw) t
         val linger: (Time.t Option.t, 'a, 'b, rw) t
         val noDelay: (Bool.t, Family.inet, 'a Type.stream, rw) t
         val nRead: (Int.t, 'a, 'b, ro) t
         val oobInline: (Bool.t, 'a, 'b, rw) t
         val rcvBuf: (Int.t, 'a, 'b, rw) t
         val reuseAddr: (Bool.t, 'a, 'b, rw) t
         val sndBuf: (Int.t, 'a, 'b, rw) t
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
                      'b Option.t, 'c Option.t, 'd Option.t, 'e Option.t) t
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
            val vector: {numBytes: Int.t} -> ('a, 'b, 'c, 'd, 'e, 'd, 'e) t
         end
      
         val receive:
            ('a, 'f, 'g, 'h) From.t
            * ('a, 'b, 'c, 'd, 'e, 'f, 'g) To.t
            * ('a,
               Int.t * 'a Address.t,
               Int.t,
               Word8.t vector * 'a Address.t,
               Word8.t vector,
               'b, 'c, 'd, 'e) Block.t
            * Flag.t List.t
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
       * dgram  array  may   Int.t * 'a Address.t
       * dgram  array  non   (Int.t * 'a Address.t) Option.t
       * dgram  vector may   Word8.t vector * 'a addr
       * dgram  vector non   (Word8.t vector * 'a Address.t) Option.t
       * stream array  may   Int.t
       * stream array  non   Int.t Option.t
       * stream vector may   Word8.t vector
       * stream vector non   Word8.t vector Option.t
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
            * ('a, Unit.t, Int.t, Bool.t, Int.t Option.t, 'b, 'c) Block.t
            * Flag.t List.t
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
       * dgram   may  Unit.t
       * dgram   non  Bool.t
       * stream  may  Int.t
       * stream  non  Int.t Option.t
       *)
      end

      val accept:
         ('a, Type.passiveStream) t
         * (('a, Type.activeStream) t * 'a Address.t, 'b) Block.u
         -> 'b
      val bind: ('a, 'b) t * 'a Address.t -> Unit.t
      val close: ('a, 'b) t -> Unit.t
      val connect: ('a, 'b) t * 'a Address.t * (Unit.t, 'c) Block.u -> 'c
      val desc: ('a, 'b) t -> Desc.t
      val getOpt: ('a, 'b) t * ('c, 'a, 'b, 'd) Opt.t -> 'c
      val ioDesc: ('a, 'b) t -> IoDesc.t
      val listen: ('a, Type.passiveStream) t * Int.t -> Unit.t
      val make: 'a Family.t * 'b Type.t * {protocol: Int.t} Option.t -> ('a, 'b) t
      val makePair: ('a Family.t * 'b Type.t * {protocol: Int.t} Option.t
                     -> ('a, 'b) t * ('a, 'b) t)
      val myAddress: ('a, 'b) t -> 'a Address.t
      val peerAddress: ('a, 'b) t -> 'a Address.t
      val select: ({exs: Desc.t List.t,
                    rds: Desc.t List.t,
                    timeout: Time.t Option.t,
                    wrs: Desc.t List.t}
                   -> {exs: Desc.t List.t,
                       rds: Desc.t List.t,
                       wrs: Desc.t List.t})
      val setOpt: ('a, 'b) t * ('c, 'a, 'b, Opt.rw) Opt.t * 'c -> Unit.t
      val shutdown: ('a, 'b Type.stream) t * ShutdownMode.t -> Unit.t
   end

end
