type port = (INetSock.inet, Socket.passive Socket.stream) Socket.sock

(* There must be a better (faster!) way to convert a string to unsigned char *)
fun msg s = Word8VectorSlice.full (Word8Vector.tabulate 
  (String.size s, Word8.fromInt o Char.ord o (fn i => String.sub (s, i))))
fun str v = CharVector.tabulate (Word8Vector.length v,
  Char.chr o Word8.toInt o (fn i => Word8Vector.sub (v, i)))

val delay = Time.fromSeconds 5
val port : port = INetSock.TCP.socket ()
val () = Socket.Ctl.setREUSEADDR (port, true)
val () = Socket.bind (port, INetSock.any 12467)
val () = Socket.listen (port, 100)

val google = valOf (NetHostDB.getByName "www.google.de")
val ghttp = INetSock.toAddr (NetHostDB.addr google, 80)

fun http () =
  let
    val s = INetSock.TCP.socket ()
    val () = print "connecting...\n"
    val () = Socket.connect (s, ghttp)
    val () = print "sending...\n"
    val _  = Socket.sendVec (s, msg "GET / HTTP/1.1\nHost: www.google.de\n\n")
    val () = print "reading...\n"
    val got = Socket.recvVec (s, 4096)
    val () = print "done!\n"
  in
    print ("response: " ^ str got ^ "\n")
  end

fun worker s () = 
  let
    val _ = Socket.sendVec (s, msg "hello and welcome!\n");
    val got = Word8VectorSlice.full (Socket.recvVec (s, 400))
  in
    if Word8VectorSlice.length got = 0 then Socket.close s else
    (Socket.sendVec (s, got); worker s ())
  end

fun welcome () = 
  let
    val (s, _) = Socket.accept port
  in
    spawn (worker s);
    welcome ()
  end

fun beat () = (
  stopTill (TIMEOUT delay);
  print "hello world\n";
  beat ())
  
val () = spawn welcome
val () = spawn beat
val () = spawn http
val () = spawn http

val () = main ()
