structure Net: NET = struct

   fun convertList list () = 
      Vector.map (vector (list ()), fn (name, f) => (f, {name = name}))

   structure Family = struct
      type inet = INetSock.inet
      type unix = UnixSock.unix
      type unknown = Unit.t
      type 'a t = NetHostDB.addr_family

      val == = op =
      local
         open Socket.AF
      in
         val all = convertList list
         val ofString = Option.ofBasis o fromString
         val toString = toString
      end
      val inet = INetSock.inetAF
      val unix = UnixSock.unixAF
   end

   structure Host = struct
      structure Address = struct
         type t = NetHostDB.in_addr

         val == = op =
         val scanner = Scanner.ofBasis NetHostDB.scan
         fun ofString s = Scanner.scanString (scanner, s)
         val toString = NetHostDB.toString
      end

      type t = NetHostDB.entry
         
      val address = NetHostDB.addr
      val addresses = vector o NetHostDB.addrs
      val aliases = vector o NetHostDB.aliases
      val family = NetHostDB.addrType
      val getByAddress = Option.ofBasis o NetHostDB.getByAddr
      val getByName = Option.ofBasis o NetHostDB.getByName
      val name = NetHostDB.name
   end

   structure Protocol = struct
      open NetProtDB
      type t = entry
      val aliases = vector o aliases
      val getByName = Option.ofBasis o getByName
      val getByNumber = Option.ofBasis o getByNumber
      val number = protocol
   end

   structure Service = struct
      open NetServDB
      type t = entry
      val aliases = vector o aliases
      local
         fun make f (x, y) = Option.ofBasis (f (x, Option.toBasis y))
      in
         val getByName = make getByName
         val getByPort = make getByPort
      end
   end

   structure Socket = struct
      type ('a, 'b) t = ('a, 'b) Socket.sock
      type ('a, 'b) sock = ('a, 'b) t

      structure Address = struct
         type 'a t = 'a Socket.sock_addr

         val == = Socket.sameAddr
         val family = Socket.familyOfAddr
         fun inet {port} = INetSock.any port
         fun ofHost (a, {port}) = INetSock.toAddr (a, port)
         val ofUnix = UnixSock.toAddr
         fun toHost a = let
            val (a, p) = INetSock.fromAddr a
         in
            (a, {port = p})
         end
         val toUnix = UnixSock.fromAddr
      end

      structure Type = struct
         local
            open Socket
         in
            type dgram = dgram
            type passive = passive
            type active = active
            type 'a stream = 'a stream
         end

         type activeStream = active stream
         type passiveStream = passive stream
         type unknown = Unit.t

         local
            open Socket.SOCK
         in
            type 'a t = sock_type
            val == = op =
            val all = convertList list
            val dgram = dgram
            val ofString = Option.ofBasis o fromString
            val stream = stream
            val toString = toString
         end
      end

      structure Opt = struct
         type ro = Unit.t
         type rw = Unit.t
         datatype ('a, 'b, 'c, 'd) t =
            T of (('b, 'c) sock -> 'a) * (('b, 'c) sock * 'a -> Unit.t)

         fun get (T (get, _), s) = get s

         fun set (T (_, set), s, x) = set (s, x)

         local
            fun noSet _ = die "Socket.Option.set"
            open Socket.Ctl
         in
            val atmark = T (getATMARK, noSet)
            val broadcast = T (getBROADCAST, setBROADCAST)
            val debug = T (getDEBUG, setDEBUG)
            val dontRoute = T (getDONTROUTE, setDONTROUTE)
            val error = T (getERROR, noSet)
            val keepAlive = T (getKEEPALIVE, setKEEPALIVE)
            val getLINGER = fn ? => (Option.ofBasis o getLINGER) ?
            val setLINGER = fn (x, y) => setLINGER (x, Option.toBasis y)
            val linger = T (getLINGER, setLINGER)
            val noDelay = T (INetSock.TCP.getNODELAY, INetSock.TCP.setNODELAY)
            val nRead = T (getNREAD, noSet)
            val oobInline = T (getOOBINLINE, setOOBINLINE)
            val rcvBuf = T (getRCVBUF, setRCVBUF)
            val reuseAddr = T (getREUSEADDR, setREUSEADDR)
            val sndBuf = T (getSNDBUF, setSNDBUF)
            val ty = T (getTYPE, noSet)
         end
      end
   
      structure Desc = struct
         type t = Socket.sock_desc

         val == = Socket.sameDesc
      end

      structure ShutdownMode = struct
         datatype t = datatype Socket.shutdown_mode

         val noRecvs = NO_RECVS
         val noSends = NO_SENDS
         val noRecvsSends = NO_RECVS_OR_SENDS
      end

      local
         open Socket
      in
         val bind = bind
         val close = close
         val desc = sockDesc
         val ioDesc = ioDesc
         val listen = listen
         val shutdown = shutdown
      end

      fun getOpt (s, opt) = Opt.get (opt, s)
      fun setOpt (s, opt, x) = Opt.set (opt, s, x)

      fun make (f, t, p) =
         case p of
            None => GenericSock.socket (f, t)
          | Some {protocol = p} => GenericSock.socket' (f, t, p)

      fun makePair (f, t, p) =
         case p of
            None => GenericSock.socketPair (f, t)
          | Some {protocol = p} => GenericSock.socketPair' (f, t, p)

      local
         open Socket.Ctl
      in
         val myAddress = getSockName
         val peerAddress = getPeerName
      end

      fun select {exs, rds, timeout, wrs} =
         Socket.select {exs = exs,
                        rds = rds,
                        timeout = Option.toBasis timeout,
                        wrs = wrs}

      structure Sum = StaticSum

      structure Block = struct
         type ('a, 'b, 'c) t = (Unit.t, 'a, Unit.t, 'b, 'c) Sum.t
         type ('a, 'b) u = ('a, 'a Option.t, 'b) t

         val may = fn ? => Sum.left () ?
         val non = fn ? => Sum.right () ?
         fun switch (b, {may, non}) = Sum.switch (b, may, non)
      end

      fun accept (s, b) =
         Block.switch (b, {may = fn () => Socket.accept s,
                           non = fn () => Option.ofBasis (Socket.acceptNB s)})

      fun connect (s, a, b) =
         Block.switch
         (b, {may = fn () => Socket.connect (s, a),
              non = fn () => if Socket.connectNB (s, a) then Some () else None})

      structure Receive = struct

         structure Flag = struct
            datatype t =
               OutOfBand
             | Peek

            val peek = Peek

            val outOfBand = OutOfBand

            val eval: t List.t -> {peek: Bool.t, oob: Bool.t} =
               fn l => let
                  val (oob, p) =
                     List.fold (l, (false, false), fn (f, (oob, p)) =>
                                case f of
                                   OutOfBand => (true, p)
                                 | Peek => (oob, true))
               in
                  {oob = oob, peek = p}
               end

         end
      
         structure From = struct
            type 'a dgram = ('a, Type.dgram) sock
            type 'a stream = ('a, Type.activeStream) sock
            type ('a, 'b, 'c, 'd) t =
               ('a dgram, 'b, 'a stream, 'c, 'd) Sum.t
            type ('a, 'b, 'c) cases = ('a dgram -> 'b) * ('a stream -> 'c)
               
            val dgram = Sum.left
            val stream = Sum.right
         end
  
         structure To = struct
            type arr = Word8.t ArraySlice.t
            type vec = Int.t
            type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t =
               (arr,
                ('a, 'b, 'c) From.cases,
                vec,
                ('a, 'd, 'e) From.cases,
                ('a, 'f, 'g) From.cases) Sum.t
            type ('a, 'b) cases = (arr -> 'a) * (vec -> 'b)

            val array = Sum.left
            fun vector {numBytes = n} = Sum.right n
         end

         structure Block = struct
            type ('a, 'b, 'c, 'd, 'e) one =
               (('a, 'b, 'c) From.cases, ('a, 'd, 'e) From.cases) To.cases
            type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) t =
               (Unit.t,
                ('a, 'b, 'c, 'd, 'e) one,
                Unit.t,
                ('a, 'b Option.t, 'c Option.t, 'd Option.t, 'e Option.t) one,
                ('a, 'f, 'g, 'h, 'i) one) Sum.t

            val may = Block.may
            val non = Block.non
         end

         fun receive (from, to, block, flags) = let
            val fs = Flag.eval flags
            val (l, r) =
               Sum.switch
               (block,
                const (fn a => (fn s => Socket.recvArrFrom' (s, a, fs),
                                fn s => Socket.recvArr' (s, a, fs)),
                       fn n => (fn s => Socket.recvVecFrom' (s, n, fs),
                                fn s => Socket.recvVec' (s, n, fs))),
                const (fn a => (fn s => (Option.ofBasis
                                         (Socket.recvArrFromNB' (s, a, fs))),
                                fn s => (Option.ofBasis
                                         (Socket.recvArrNB' (s, a, fs)))),
                       fn n => (fn s => (Option.ofBasis
                                         (Socket.recvVecFromNB' (s, n, fs))),
                                fn s => (Option.ofBasis
                                         (Socket.recvVecNB' (s, n, fs))))))
            val (l, r) = Sum.switch (to, l, r)
         in
            Sum.switch (from, l, r)
         end
      end

      structure Send = struct

         structure From = struct
            datatype t =
               Array of Word8.t ArraySlice.t
             | Vector of Word8.t VectorSlice.t
               
            val array = Array
            val vector = Vector
         end
      
         structure Flag = struct
            datatype t =
               DontRoute
             | OutOfBand

            val dontRoute = DontRoute

            val outOfBand = OutOfBand
            
            val eval: t List.t -> {don't_route: Bool.t, oob: Bool.t} =
               fn l => let
                  val (dr, oob) =
                     List.fold (l, (false, false), fn (f, (dr, oob)) =>
                                case f of
                                   DontRoute => (true, oob)
                                 | OutOfBand => (dr, true))
               in
                  {don't_route = dr, oob = oob}
               end
         end

         structure To = struct
            type 'a dgram = ('a, Type.dgram) sock * 'a Address.t
            type 'a stream = ('a, Type.activeStream) sock
            type ('a, 'b, 'c, 'd) t = ('a dgram, 'b, 'a stream, 'c, 'd) StaticSum.t
            type ('a, 'b, 'c) cases = ('a dgram -> 'b) * ('a stream -> 'c)
            
            val dgram = Sum.left
            val stream = Sum.right
         end
            
         structure Block = struct
            type ('a, 'b1, 'c1, 'b2, 'c2, 'b3, 'c3) t =
               (('a, 'b1, 'c1) To.cases,
                ('a, 'b2, 'c2) To.cases,
                ('a, 'b3, 'c3) To.cases) Block.t

            val may = Block.may
            val non = Block.non
         end

         fun send (from, to, block, flags) = let
            val flags = Flag.eval flags
            datatype z = datatype From.t
            fun dgram (s, a) =
               case from of
                  Array ar => Socket.sendArrTo' (s, a, ar, flags)
                | Vector v => Socket.sendVecTo' (s, a, v, flags)
            fun dgramNB (s, a) =
               case from of
                  Array a' => Socket.sendArrToNB' (s, a, a', flags)
                | Vector v => Socket.sendVecToNB' (s, a, v, flags)
            fun stream s =
               case from of
                  Array a => Socket.sendArr' (s, a, flags)
                | Vector v => Socket.sendVec' (s, v, flags)
            fun streamNB s =
               Option.ofBasis
               (case from of
                   Array a => Socket.sendArrNB' (s, a, flags)
                 | Vector v => Socket.sendVecNB' (s, v, flags))
            val (l, r) =
               Sum.switch
               (block, const (dgram, stream), const (dgramNB, streamNB))
         in
            Sum.switch (to, l, r)
         end
      end      

   end

   val getHostName = NetHostDB.getHostName
end
