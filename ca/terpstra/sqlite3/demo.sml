val () = print ("SQLite version: " ^ SQL.version ^ "\n")

fun die x = (
   print ("Caught exception: " ^ x ^ "\n");
   OS.Process.exit OS.Process.failure)

val (dbname, query) = case CommandLine.arguments () of
     [x, y] => (x, y)
   | _ => die "Expecting: <database name> <query>\n"
val arg = valOf (Int.fromString query)
val db = SQL.openDB dbname handle SQL.Error x => die x

local
  open SQL.Template
in
  val Q1 = query db "select x, y from peanuts\n\
                    \where y="iI" or x="iS";" oS oI $
           handle SQL.Error x => die x
end

fun dump (s & i) = print (s ^ " " ^ Int.toString i ^ "\n")
val a  = SQL.app dump Q1 (arg & "hi") handle SQL.Error x => die x

val () = SQL.close Q1
val () = SQL.closeDB db
