structure Posix: POSIX = struct
   open Posix
   open FileSys IO Process ProcEnv SysDB TTY

   structure FileDesc = struct
      type t = file_desc

      val ofIODesc = Option.ofBasis o FileSys.iodToFD
      val ofWord = FileSys.wordToFD
      val toIODesc = FileSys.fdToIOD
      val toWord = FileSys.fdToWord
      open FileSys
   end

   structure FileDescFlags = struct
      open IO.FD
      local
         structure S = BitFlags (IO.FD)
      in
         open S
      end
   end

   structure FileStatusFlags = struct
      open IO.O
      local
         structure S = BitFlags (IO.O)
      in
         open S
      end
   end

   structure Fcntl = struct
      type ('a, 'b) t = FileDesc.t * 'a -> 'b

      fun dupfd (old, base) = IO.dupfd {base = base, old = old}
      fun getfd (fd, ()) = IO.getfd fd
      fun getfl (fd, ()) = IO.getfl fd
      val getlk = IO.getlk
      val setfd = IO.setfd
      val setfl = IO.setfl
      val setlk = IO.setlk
      val setlkw = IO.setlkw
   end

   fun fcntl (fd, c, a) = c (fd, a)

   structure Gid = struct
      type t = ProcEnv.gid

      val ofWord = ProcEnv.wordToGid
      val toWord = ProcEnv.gidToWord
   end

   structure Mode = struct
      open FileSys.S
      local
         structure S = BitFlags (FileSys.S)
      in
         open S
      end
   end

   structure OpenFlags = struct
      open FileSys.O
      local
         structure S = BitFlags (FileSys.O)
      in
         open S
      end
   end

   structure Pid = struct
      type t = Process.pid

      val ofWord = Process.wordToPid
      val toWord = Process.pidToWord
   end

   structure Uid = struct
      type t = ProcEnv.uid

      val ofWord = ProcEnv.wordToUid
      val toWord = ProcEnv.uidToWord
   end
        
   structure Error = struct
      open Error

      type t = syserror

      val message = errorMsg
      val name = errorName
      val ofName = Option.ofBasis o syserror
      val ofWord = fromWord
   end

   structure Signal = struct
      open Signal

      type t = signal

      val ofWord = fromWord
   end

   structure ExitStatus = struct
       datatype t =
          Exited
        | ExitStatus of Word8.t
        | Signaled of Signal.t
        | Stopped of Signal.t
          
      datatype z = datatype Process.exit_status

      val ofBasis =
         fn W_EXITED => Exited
          | W_EXITSTATUS w => ExitStatus w
          | W_SIGNALED s => Signaled s
          | W_STOPPED s => Stopped s

      val ofStatus = ofBasis o Process.fromStatus
   end

   structure AccessMode = struct
      datatype t = datatype FileSys.access_mode

      val exec = A_EXEC
      val read = A_READ
      val write = A_WRITE
   end

   structure Dev = struct
      type t = FileSys.dev

      val ofWord = FileSys.wordToDev
      val toWord = FileSys.devToWord
   end

   structure DirStream = struct
      type t = FileSys.dirstream
   end

   structure FlowAction = struct
      open TTY.TC
      type t = flow_action
   end

   structure Group = struct
      open SysDB.Group

      type t = group
   end

   structure QueueSel = struct
      open TTY.TC

      type t = queue_sel

      val both = ioflush
      val input = iflush
      val output = oflush
   end

   structure SetAction = struct
      open TTY.TC

      type t = set_action

      val drain = sadrain
      val flush = saflush
      val now = sanow
   end

   structure Ino = struct
      type t = FileSys.ino

      val ofWord = FileSys.wordToIno
      val toWord = FileSys.inoToWord
   end

   structure KillArg = struct
      datatype t = datatype Process.killpid_arg

      val group = K_GROUP
      val proc = K_PROC
      val sameGroup = K_SAME_GROUP
   end

   structure Lock = struct
      structure Type = struct
         datatype t = datatype IO.lock_type

         val read = F_RDLCK
         val unlocked = F_UNLCK
         val write = F_WRLCK
      end

      open IO.FLock

      type t = flock

      fun make {len, pid, start, ty, whence} =
         flock {len = len,
                ltype = ty,
                pid = Option.toBasis pid,
                start = start,
                whence = whence}

      val pid = Option.ofBasis o pid

      val ty = ltype
   end

   structure OpenMode = struct
      datatype t = datatype FileSys.open_mode

      val read = O_RDONLY
      val readWrite = O_RDWR
      val write = O_WRONLY
   end
   
   structure Passwd = struct
      open SysDB.Passwd

      type t = passwd
   end

   structure Stat = struct
      open FileSys.ST
      type t = stat
   end

   structure Termios = struct

      open TTY         

      structure C = struct
         open C
         local
            structure S = BitFlags (C)
         in
            open S
         end
      end

      structure CC = struct
         open V

         type t = cc
         val make = cc
      end

      structure I = struct
         open I
         local
            structure S = BitFlags (I)
         in
            open S
         end
      end

      structure L = struct
         open L
         local
            structure S = BitFlags (L)
         in
            open S
         end
      end

      structure O = struct
         open O
         local
            structure S = BitFlags (O)
         in
            open S
         end
      end

      structure Speed = struct
         open TTY

         type t = speed
         val ofWord = wordToSpeed
         val toWord = speedToWord
      end

      datatype t = T of {c: C.t,
                         cc: CC.t,
                         i: I.t,
                         ispeed: Speed.t,
                         l: L.t,
                         o: O.t,
                         ospeed: Speed.t}

      fun toBasis (T {c, cc, i, ispeed, l, o, ospeed}) =
         termios {cc = cc,
                  cflag = c,
                  iflag = i,
                  ispeed = ispeed,
                  lflag = l,
                  oflag = op o,
                  ospeed = ospeed}

      fun fromBasis t = let
         val {cc, cflag, iflag, ispeed, lflag, oflag, ospeed} = TTY.fieldsOf t
      in
         T {c = cflag,
            cc = cc,
            i = iflag,
            ispeed = ispeed,
            l = lflag,
            o = oflag,
            ospeed = ospeed}
      end

   end

   structure WaitPidArg = struct
      datatype t = datatype Process.waitpid_arg

      val anyChild = W_ANY_CHILD
      val child = W_CHILD
      val group = W_GROUP
      val sameGroup = W_SAME_GROUP
   end

   structure WaitPidFlags = struct
      open Process.W
      local
         structure S = BitFlags (Process.W)
      in
         open S
      end
   end

   structure Whence = struct
      datatype t = datatype IO.whence

      val cur = SEEK_CUR
      val endd = SEEK_END
      val set = SEEK_SET
   end
      
   local
      open CF
   in
      local
         fun make f = f o Termios.toBasis
      in
         val cfgetispeed = make getispeed
         val cfgetospeed = make getospeed
      end
      local
         fun make f (t, s) = Termios.fromBasis (f (Termios.toBasis t, s))
      in
         val cfsetispeed = make setispeed
         val cfsetospeed = make setospeed
      end
   end

   local
      open TC
   in
      val tcdrain = drain
      val tcflow = flow
      val tcflush = flush
      val tcgetattr = Termios.fromBasis o getattr
      val tcgetpgrp = getpgrp
      val tcsendbreak = sendbreak
      fun tcsetattr (fd, sa, t) = setattr (fd, sa, Termios.toBasis t)
      val tcsetpgrp = setpgrp
   end

   local
      fun conv (x, s) = (x, ExitStatus.ofBasis s)
      fun make f = conv o f
   in
      val wait = make wait
      val waitpid = make waitpid
      fun waitpidNohang x = Option.map (Option.ofBasis (waitpid_nh x), conv)
   end

   val fork = Option.ofBasis o fork
   val fpathconf = Option.ofBasis o fpathconf
   val getenv = Option.ofBasis o getenv
   val pathconf = Option.ofBasis o pathconf
   val readdir = Option.ofBasis o readdir
   val setpgid = fn {pgid, pid} =>
      setpgid {pgid = Option.toBasis pgid, pid = Option.toBasis pid}
   val utime = fn (f, opt) => utime (f, Option.toBasis opt)

end
