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
      val message: t -> string
      val mfile: t
      val mlink: t
      val msgsize: t
      val name: t -> string
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
      val ofName: string -> t option
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

      val ofIODesc: IoDesc.t -> t option
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

      val name: t -> string
      val gid: t -> Gid.t
      val members: t -> string list
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
      val isBlk: t -> bool
      val isChr: t -> bool
      val isDir: t -> bool
      val isFIFO: t -> bool
      val isLink: t -> bool
      val isReg: t -> bool
      val isSock: t -> bool
      val mode: t -> Mode.t
      val mtime: t -> Time.t
      val nlink: t -> int
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
                 pid: Pid.t option,
                 start: Position.t,
                 ty: Type.t,
                 whence: Whence.t} -> t
      val pid: t -> Pid.t option
      val start: t -> Position.t
      val ty: t -> Type.t
      val whence: t -> Whence.t
   end

   structure Fcntl: sig
      type ('a, 'b) t

      val dupfd: (FileDesc.t, FileDesc.t) t
      val getfd: (unit, FileDescFlags.t) t
      val getfl: (unit, FileStatusFlags.t * OpenMode.t) t
      val getlk: (Lock.t, Lock.t) t
      val setfd: (FileDescFlags.t, unit) t
      val setfl: (FileStatusFlags.t, unit) t
      val setlk: (Lock.t, Lock.t) t
      val setlkw: (Lock.t, Lock.t) t
   end

   structure Passwd: sig
      type t

      val gid: t -> Gid.t
      val home: t -> string
      val name: t -> string
      val shell: t -> string
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
         val eof: int
         val eol: int
         val erase: int
         val intr: int
         val kill: int
         val min: int
         val quit: int
         val susp: int
         val time: int
         val start: int
         val stop: int
         val nccs: int

         type t

         val make: (int * char) list -> t
         val update: t * (int * char) list -> t
         val sub: t * int -> char
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

   val access: string * AccessMode.t list -> bool
   val alarm: Time.t -> Time.t
   val cfgetispeed: Termios.t -> Termios.Speed.t
   val cfgetospeed: Termios.t -> Termios.Speed.t
   val cfsetispeed: Termios.t * Termios.Speed.t -> Termios.t
   val cfsetospeed: Termios.t * Termios.Speed.t -> Termios.t
   val chdir: string -> unit
   val chmod: string * Mode.t -> unit
   val chown: string * Uid.t * Gid.t -> unit
   val close: FileDesc.t -> unit
   val closedir: DirStream.t -> unit
   val creat: string * Mode.t -> FileDesc.t
   val createf: string * OpenMode.t * OpenFlags.t * Mode.t -> FileDesc.t
   val ctermid: unit -> string
   val dup2: {old: FileDesc.t, new: FileDesc.t} -> unit
   val dup: FileDesc.t -> FileDesc.t
   val environ: unit -> string list
   val exec: string * string list -> 'a
   val exece: string * string list * string list -> 'a
   val execp: string * string list -> 'a
   val exit: Word8.t -> 'a
   val fchmod: FileDesc.t * Mode.t -> unit
   val fchown: FileDesc.t * Uid.t * Gid.t -> unit
   val fcntl: FileDesc.t * ('a, 'b) Fcntl.t * 'a -> 'b
   val fork: unit -> Pid.t option
   val fpathconf: FileDesc.t * string -> SysWord.t option
   val fstat: FileDesc.t -> Stat.t
   val fsync: FileDesc.t -> unit
   val ftruncate: FileDesc.t * Position.t -> unit
   val getcwd: unit -> string
   val getegid: unit -> Gid.t
   val getenv: string -> string option
   val geteuid: unit -> Uid.t
   val getgid: unit -> Gid.t
   val getgrgid: Gid.t -> Group.t
   val getgrnam: string -> Group.t
   val getgroups: unit -> Gid.t list
   val getlogin: unit -> string
   val getpgrp: unit -> Pid.t
   val getpid: unit -> Pid.t
   val getppid: unit -> Pid.t
   val getpwnam: string -> Passwd.t
   val getpwuid: Uid.t -> Passwd.t
   val getuid: unit -> Uid.t
   val isatty: FileDesc.t -> bool
   val kill: KillArg.t * Signal.t -> unit
   val link: {old: string, new: string} -> unit
   val lseek: FileDesc.t * Position.t * Whence.t -> Position.t
   val lstat: string -> Stat.t
   val mkdir: string * Mode.t -> unit
   val mkfifo: string * Mode.t -> unit
   val opendir: string -> DirStream.t
   val openf: string * OpenMode.t * OpenFlags.t -> FileDesc.t
   val pathconf: string * string -> SysWord.t option
   val pause: unit -> unit
   val pipe: unit -> {infd: FileDesc.t, outfd: FileDesc.t}
   val readArr: FileDesc.t * Word8.t ArraySlice.t -> int
   val readVec: FileDesc.t * int -> Word8.t vector
   val readdir: DirStream.t -> string option
   val readlink: string -> string
   val rename: {old: string, new: string} -> unit
   val rewinddir: DirStream.t -> unit
   val rmdir: string -> unit
   val setgid: Gid.t -> unit
   val setpgid: {pid: Pid.t option, pgid: Pid.t option} -> unit
   val setsid: unit -> Pid.t
   val setuid: Uid.t -> unit
   val sleep: Time.t -> Time.t 
   val stat: string -> Stat.t
   val symlink: {old: string, new: string} -> unit
   val sysconf: string -> SysWord.t
   val tcdrain: FileDesc.t -> unit
   val tcflow: FileDesc.t * FlowAction.t -> unit
   val tcflush: FileDesc.t * QueueSel.t -> unit
   val tcgetattr: FileDesc.t -> Termios.t
   val tcgetpgrp: FileDesc.t -> Pid.t
   val tcsendbreak: FileDesc.t * int -> unit
   val tcsetattr: FileDesc.t * SetAction.t * Termios.t -> unit
   val tcsetpgrp: FileDesc.t * Pid.t -> unit
   val time: unit -> Time.t
   val times: unit -> {elapsed: Time.t,
                       utime: Time.t,
                       stime: Time.t,
                       cutime: Time.t,
                       cstime: Time.t}
   val ttyname: FileDesc.t -> string
   val umask: Mode.t -> Mode.t
   val uname: unit -> (string * string) list
   val unlink: string -> unit
   val utime: string * {actime: Time.t, modtime: Time.t} option -> unit
   val wait: unit -> Pid.t * ExitStatus.t
   val waitpid: WaitPidArg.t * WaitPidFlags.t list -> Pid.t * ExitStatus.t
   val waitpidNohang: (WaitPidArg.t * WaitPidFlags.t list
                       -> (Pid.t * ExitStatus.t) option)
   val writeArr: FileDesc.t * Word8.t ArraySlice.t -> int
   val writeVec: FileDesc.t * Word8.t VectorSlice.t -> int

end
