open SQL.Template

fun die x = (
   print ("Caught exception: " ^ x ^ "\n");
   OS.Process.exit OS.Process.failure)

val (dbname, query) = case CommandLine.arguments () of
     [x, y] => (x, y)
   | _ => die "Expecting: <database name> <query>\n"

local
  open SQL.Template
in
  (* query templates I might execute *)
  val T1 = query "select x, y from peanuts\n\
                 \where y="iI" or x="iS";" oS oI $
end

fun dump (q, a) = (
    Vector.app (fn x => print (#name x ^ " ")) (SQL.meta q);
    print "\n";
    Vector.app (fn (s & i) => print (s ^ " " ^ Int.toString i ^ "\n")) a
    )

val db = SQL.openDB dbname handle Fail x => die x
val Q1 = T1 (db & valOf (Int.fromString query) & "hi") handle Fail x => die x
val a  = SQL.pull Q1 handle Fail x => die x
val () = dump (Q1, a)
val () = SQL.close Q1

val () = SQL.closeDB db
