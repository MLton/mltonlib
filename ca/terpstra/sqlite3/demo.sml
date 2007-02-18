(*
** 2007 February 18
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** $Id$
*)
val () = print ("SQLite version: " ^ SQL.version ^ "\n")

fun die x = (
   print ("Caught exception: " ^ x ^ "\n");
   OS.Process.exit OS.Process.failure)

val (dbname, query) = case CommandLine.arguments () of
     [x, y] => (x, y)
   | _ => die "Expecting: <database name> <query>\n"
val db = SQL.openDB dbname handle SQL.Error x => die x

local
  open SQL.Function
in
  fun concat (a & b) = a ^ b
  fun debug v = Vector.app (fn s => print (s ^ "\n")) v
  fun glom (s & i) = if i = 0 then raise SQL.Error "bad integer" else s ^ Int.toString i
  val sum2 = { init = fn () => 0, 
               step = fn (i, (j & k)) => i + Int64.fromInt j + k, 
               finish = fn x => Int64.toString x }
  val () = SQL.registerFunction  (db, "wes", fnS iS iS $ concat)
  val () = SQL.registerFunction  (db, "debug", fnN iAS $ debug)
  val () = SQL.registerFunction  (db, "glom", fnS iS iI $ glom)
  val () = SQL.registerCollation (db, "sless", String.compare)
  val () = SQL.registerAggregate (db, "sum2", aggrS iI iZ $ sum2)
end

local
  open SQL.Query
in
  val Q1 = prepare db "select x, y from peanuts\n\
                      \where y="iI" or x="iS";" oS oI $
           handle SQL.Error x => die x
  val Q2 = prepare db query oAS $
           handle SQL.Error x => die x
end

(*
local
   open SQL.SQLite
   fun auth (INSERT { table, db }) = 
          (print (db ^ ":" ^ table ^ ": insert denied\n"); DENY)
     | auth _ = ALLOW
in
   val () = setAuthorizer (db, SOME auth)
end
*)

fun dumpP (s & i) = print (s ^ " " ^ Int.toString i ^ "\n")
fun dumpV v = (Vector.app (fn s => print (s ^ " ")) v; print "\n")

val () = SQL.app dumpP Q1 (4 & "hi") handle SQL.Error x => die x
val () = SQL.app dumpV Q2 ()         handle SQL.Error x => die x

val pq = SQL.SQLite.preparedQueries
val () = print ("Prepared queries: " ^ Int.toString (pq db) ^ "\n")
val () = print "Running garbage collector... "
val () = MLton.GC.collect ()
val () = print "done\n"
val () = print ("Prepared queries: " ^ Int.toString (pq db) ^ "\n")

val () = SQL.closeDB db              handle SQL.Error x => die x
