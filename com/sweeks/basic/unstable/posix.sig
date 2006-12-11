signature LIKE_SYSWORD = sig
   type t

   val ofWord: SysWord.t -> t
   val toWord: t -> SysWord.t
end

signature POSIX = sig
   
   structure Dev: LIKE_SYSWORD
   structure Gid: LIKE_SYSWORD
   structure Ino: LIKE_SYSWORD
   structure Pid: LIKE_SYSWORD
   structure Uid: LIKE_SYSWORD

   structure Signal: sig
      include LIKE_SYSWORD

      val abrt: t
      val alrm: t
      val bus: t
      val chld: t
      val cont: t
      val fpe: t
      val hup: t
      val ill: t
      val int: t
      val kill: t
      val pipe: t
      val quit: t
      val segv: t
      val stop: t
      val term: t
      val tstp: t
      val ttin: t
      val ttou: t
      val usr1: t
      val usr2: t
   end

   structure Error: sig
      include LIKE_SYSWORD

      val acces: t
      val again: t
      val badf: t
      val badmsg: t
      val busy: t
      val canceled: t
      val child: t
      val deadlk: t
      val dom: t
      val exist: t
      val fault: t
      val fbig: t
      val inprogress: t
      val intr: t
      val inval: t
      val io: t
      val isdir: t
      val loop: t
      val message: t -> String.t
      val mfile: t
      val mlink: t
      val msgsize: t
      val name: t -> String.t
      val nametoolong: t
      val nfile: t
      val nodev: t
      val noent: t
      val noexec: t
      val nolck: t
      val nomem: t
      val nospc: t
      val nosys: t
      val notdir: t
      val notempty: t
      val notsup: t
      val notty: t
      val nxio: t
      val ofName: String.t -> t Option.t
      val perm: t
      val pipe: t
      val range: t
      val rofs: t
      val spipe: t
      val srch: t
      val toobig: t
      val xdev: t
   end

   structure AccessMode: sig
      type t

      val exec: t
      val read: t
      val write: t
   end

   structure DirStream: sig
      type t
   end

   structure ExitStatus: sig
       datatype t =
          Exited
        | ExitStatus of Word8.t
        | Signaled of Signal.t
        | Stopped of Signal.t

       val ofStatus: OS.Process.status -> t
    end

   structure FileDesc: sig
      include LIKE_SYSWORD

      val ofIODesc: IoDesc.t -> t Option.t
      val stderr: t
      val stdin: t
      val stdout: t
      val toIODesc: t -> IoDesc.t
   end

   structure FileDescFlags: sig
      include BIT_FLAGS

      val cloexec: t
   end

   structure FileStatusFlags: sig
      include BIT_FLAGS
         
      val append: t
      val nonblock: t
      val sync: t
   end

   structure FlowAction: sig
      type t
         
      val ooff: t
      val oon: t
      val ioff: t
      val ion: t
   end

   structure Group: sig
      type t

      val name: t -> String.t
      val gid: t -> Gid.t
      val members: t -> String.t List.t
   end

   structure KillArg: sig
      type t

      val group: Pid.t -> t
      val proc: Pid.t -> t
      val sameGroup: t
   end

   structure Mode: sig
      include BIT_FLAGS
    
      val irgrp: t
      val iroth: t
      val irusr: t
      val irwxg: t
      val irwxo: t
      val irwxu: t
      val isgid: t
      val isuid: t
      val iwgrp: t
      val iwoth: t
      val iwusr: t
      val ixgrp: t
      val ixoth: t
      val ixusr: t
   end

   structure OpenFlags: sig
      include BIT_FLAGS
      val append: t
      val excl: t
      val noctty: t
      val nonblock: t
      val sync: t
      val trunc: t
   end

   structure OpenMode: sig
      type t

      val read: t
      val readWrite: t
      val write: t
   end

   structure Whence: sig
      type t

      val cur: t
      val endd: t
      val set: t
    end

   structure Stat: sig
      type t
    
      val atime: t -> Time.t
      val ctime: t -> Time.t
      val dev: t -> Dev.t
      val gid: t -> Gid.t
      val ino: t -> Ino.t
      val isBlk: t -> Bool.t
      val isChr: t -> Bool.t
      val isDir: t -> Bool.t
      val isFIFO: t -> Bool.t
      val isLink: t -> Bool.t
      val isReg: t -> Bool.t
      val isSock: t -> Bool.t
      val mode: t -> Mode.t
      val mtime: t -> Time.t
      val nlink: t -> Int.t
      val size: t -> Position.t
      val uid: t -> Uid.t
   end

   structure Lock: sig
      structure Type: sig
         type t

         val read: t
         val unlocked: t
         val write: t
      end
   
      type t

      val len: t -> Position.t
      val make: {len: Position.t,
                 pid: Pid.t Option.t,
                 start: Position.t,
                 ty: Type.t,
                 whence: Whence.t} -> t
      val pid: t -> Pid.t Option.t
      val start: t -> Position.t
      val ty: t -> Type.t
      val whence: t -> Whence.t
   end

   structure Fcntl: sig
      type ('a, 'b) t

      val dupfd: (FileDesc.t, FileDesc.t) t
      val getfd: (Unit.t, FileDescFlags.t) t
      val getfl: (Unit.t, FileStatusFlags.t * OpenMode.t) t
      val getlk: (Lock.t, Lock.t) t
      val setfd: (FileDescFlags.t, Unit.t) t
      val setfl: (FileStatusFlags.t, Unit.t) t
      val setlk: (Lock.t, Lock.t) t
      val setlkw: (Lock.t, Lock.t) t
   end

   structure Passwd: sig
      type t

      val gid: t -> Gid.t
      val home: t -> String.t
      val name: t -> String.t
      val shell: t -> String.t
      val uid: t -> Uid.t
   end

   structure QueueSel: sig
      type t
         
      val both: t
      val input: t
      val output: t
   end

   structure SetAction: sig
      type t
         
      val now: t
      val drain: t
      val flush: t
   end

   structure Termios: sig

      structure C: sig
         include BIT_FLAGS
            
         val clocal: t
         val cread: t
         val cs5: t
         val cs6: t
         val cs7: t
         val cs8: t
         val csize: t
         val cstopb: t
         val hupcl: t
         val parenb: t
         val parodd: t
      end
   
      structure CC: sig
         val eof: Int.t
         val eol: Int.t
         val erase: Int.t
         val intr: Int.t
         val kill: Int.t
         val min: Int.t
         val quit: Int.t
         val susp: Int.t
         val time: Int.t
         val start: Int.t
         val stop: Int.t
         val nccs: Int.t

         type t

         val make: (Int.t * Char.t) List.t -> t
         val update: t * (Int.t * Char.t) List.t -> t
         val sub: t * Int.t -> Char.t
      end

      structure I: sig
         include BIT_FLAGS

         val brkint: t
         val icrnl: t
         val ignbrk: t
         val igncr: t
         val ignpar: t
         val inlcr: t
         val inpck: t
         val istrip: t
         val ixoff: t
         val ixon: t
         val parmrk: t
      end

      structure L: sig
         include BIT_FLAGS
            
         val echo: t
         val echoe: t
         val echok: t
         val echonl: t
         val icanon: t
         val iexten: t
         val isig: t
         val noflsh: t
         val tostop: t
      end
   
      structure O: sig
         include BIT_FLAGS
            
         val opost: t
      end

      structure Speed: sig
         include LIKE_SYSWORD
         
         val b0: t
         val b110: t
         val b1200: t
         val b134: t
         val b150: t
         val b1800: t
         val b19200: t
         val b200: t
         val b2400: t
         val b300: t
         val b38400: t
         val b4800: t
         val b50: t
         val b600: t
         val b75: t
         val b9600: t
      end

      datatype t = T of {c: C.t,
                         cc: CC.t,
                         i: I.t,
                         ispeed: Speed.t,
                         l: L.t,
                         o: O.t,
                         ospeed: Speed.t}
   end

   structure WaitPidArg: sig
      type t

      val anyChild: t
      val child: Pid.t -> t
      val group: Pid.t -> t
      val sameGroup: t
   end

   structure WaitPidFlags: sig
      include BIT_FLAGS
      val untraced: t
   end

   val access: String.t * AccessMode.t List.t -> Bool.t
   val alarm: Time.t -> Time.t
   val cfgetispeed: Termios.t -> Termios.Speed.t
   val cfgetospeed: Termios.t -> Termios.Speed.t
   val cfsetispeed: Termios.t * Termios.Speed.t -> Termios.t
   val cfsetospeed: Termios.t * Termios.Speed.t -> Termios.t
   val chdir: String.t -> Unit.t
   val chmod: String.t * Mode.t -> Unit.t
   val chown: String.t * Uid.t * Gid.t -> Unit.t
   val close: FileDesc.t -> Unit.t
   val closedir: DirStream.t -> Unit.t
   val creat: String.t * Mode.t -> FileDesc.t
   val createf: String.t * OpenMode.t * OpenFlags.t * Mode.t -> FileDesc.t
   val ctermid: Unit.t -> String.t
   val dup2: {old: FileDesc.t, new: FileDesc.t} -> Unit.t
   val dup: FileDesc.t -> FileDesc.t
   val environ: Unit.t -> String.t List.t
   val exec: String.t * String.t List.t -> 'a
   val exece: String.t * String.t List.t * String.t List.t -> 'a
   val execp: String.t * String.t List.t -> 'a
   val exit: Word8.t -> 'a
   val fchmod: FileDesc.t * Mode.t -> Unit.t
   val fchown: FileDesc.t * Uid.t * Gid.t -> Unit.t
   val fcntl: FileDesc.t * ('a, 'b) Fcntl.t * 'a -> 'b
   val fork: Unit.t -> Pid.t Option.t
   val fpathconf: FileDesc.t * String.t -> SysWord.t Option.t
   val fstat: FileDesc.t -> Stat.t
   val fsync: FileDesc.t -> Unit.t
   val ftruncate: FileDesc.t * Position.t -> Unit.t
   val getcwd: Unit.t -> String.t
   val getegid: Unit.t -> Gid.t
   val getenv: String.t -> String.t Option.t
   val geteuid: Unit.t -> Uid.t
   val getgid: Unit.t -> Gid.t
   val getgrgid: Gid.t -> Group.t
   val getgrnam: String.t -> Group.t
   val getgroups: Unit.t -> Gid.t List.t
   val getlogin: Unit.t -> String.t
   val getpgrp: Unit.t -> Pid.t
   val getpid: Unit.t -> Pid.t
   val getppid: Unit.t -> Pid.t
   val getpwnam: String.t -> Passwd.t
   val getpwuid: Uid.t -> Passwd.t
   val getuid: Unit.t -> Uid.t
   val isatty: FileDesc.t -> Bool.t
   val kill: KillArg.t * Signal.t -> Unit.t
   val link: {old: String.t, new: String.t} -> Unit.t
   val lseek: FileDesc.t * Position.t * Whence.t -> Position.t
   val lstat: String.t -> Stat.t
   val mkdir: String.t * Mode.t -> Unit.t
   val mkfifo: String.t * Mode.t -> Unit.t
   val opendir: String.t -> DirStream.t
   val openf: String.t * OpenMode.t * OpenFlags.t -> FileDesc.t
   val pathconf: String.t * String.t -> SysWord.t Option.t
   val pause: Unit.t -> Unit.t
   val pipe: Unit.t -> {infd: FileDesc.t, outfd: FileDesc.t}
   val readArr: FileDesc.t * Word8.t ArraySlice.t -> Int.t
   val readVec: FileDesc.t * Int.t -> Word8.t vector
   val readdir: DirStream.t -> String.t Option.t
   val readlink: String.t -> String.t
   val rename: {old: String.t, new: String.t} -> Unit.t
   val rewinddir: DirStream.t -> Unit.t
   val rmdir: String.t -> Unit.t
   val setgid: Gid.t -> Unit.t
   val setpgid: {pid: Pid.t Option.t, pgid: Pid.t Option.t} -> Unit.t
   val setsid: Unit.t -> Pid.t
   val setuid: Uid.t -> Unit.t
   val sleep: Time.t -> Time.t 
   val stat: String.t -> Stat.t
   val symlink: {old: String.t, new: String.t} -> Unit.t
   val sysconf: String.t -> SysWord.t
   val tcdrain: FileDesc.t -> Unit.t
   val tcflow: FileDesc.t * FlowAction.t -> Unit.t
   val tcflush: FileDesc.t * QueueSel.t -> Unit.t
   val tcgetattr: FileDesc.t -> Termios.t
   val tcgetpgrp: FileDesc.t -> Pid.t
   val tcsendbreak: FileDesc.t * Int.t -> Unit.t
   val tcsetattr: FileDesc.t * SetAction.t * Termios.t -> Unit.t
   val tcsetpgrp: FileDesc.t * Pid.t -> Unit.t
   val time: Unit.t -> Time.t
   val times: Unit.t -> {elapsed: Time.t,
                       utime: Time.t,
                       stime: Time.t,
                       cutime: Time.t,
                       cstime: Time.t}
   val ttyname: FileDesc.t -> String.t
   val umask: Mode.t -> Mode.t
   val uname: Unit.t -> (String.t * String.t) List.t
   val unlink: String.t -> Unit.t
   val utime: String.t * {actime: Time.t, modtime: Time.t} Option.t -> Unit.t
   val wait: Unit.t -> Pid.t * ExitStatus.t
   val waitpid: WaitPidArg.t * WaitPidFlags.t List.t -> Pid.t * ExitStatus.t
   val waitpidNohang: (WaitPidArg.t * WaitPidFlags.t List.t
                       -> (Pid.t * ExitStatus.t) Option.t)
   val writeArr: FileDesc.t * Word8.t ArraySlice.t -> Int.t
   val writeVec: FileDesc.t * Word8.t VectorSlice.t -> Int.t

end
