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
structure Prim :> PRIM =
   struct
      structure DB :> PTR = Ptr
      structure Query :> PTR = Ptr
      structure Context :> PTR = Ptr
      structure Value :> PTR = Ptr
      structure FnPtr :> PTR = Ptr
      structure CStr :> CSTR = CStr
      structure Blob :> BLOB = Blob
      
      exception Retry of string (* retriable error; busy/locked/etc *)
      exception Abort of string (* transaction aborted *)
      exception Error of string (* database corrupt; close it *)
     
      val PopenDB = _import "sqlite3_open"   : CStr.t * DB.t ref -> int;
      val PcloseDB= _import "sqlite3_close"  : DB.t -> int;
      val Perrmsg = _import "sqlite3_errmsg" : DB.t -> CStr.out;
(*    val Pfree   = _import "sqlite3_free"   : CStr.t -> unit; *)
(*    val Perrcode= _import "sqlite3_errcode": DB.t -> int; *)
      
      val Pfinalize = _import "sqlite3_finalize" : Query.t -> int;
      val Pprepare  = _import "sqlite3_prepare_v2" : DB.t * CStr.t * int * Query.t ref * MLton.Pointer.t ref -> int;
      val Pstep     = _import "sqlite3_step" : Query.t -> int;
      val Preset    = _import "sqlite3_reset" : Query.t -> int;
      val Pclearbindings = _import "sqlite3_clear_bindings" : Query.t -> int;
      
      val Pbind_blob   = _import "sqlite3_bind_blob"   : Query.t * int * Blob.t * int * word -> int;
      val Pbind_double = _import "sqlite3_bind_double" : Query.t * int * real -> int;
      val Pbind_int    = _import "sqlite3_bind_int"    : Query.t * int * int -> int;
      val Pbind_int64  = _import "sqlite3_bind_int64"  : Query.t * int * Int64.int -> int;
      val Pbind_null   = _import "sqlite3_bind_null"   : Query.t * int -> int;
      val Pbind_text   = _import "sqlite3_bind_text"   : Query.t * int * CStr.t * int * word -> int;
(*    val Pbind_text16 = _import "sqlite3_bind_text16" : Query.t * int * WideString.string * int * word -> int; *)
      val Pbind_parameter_count = _import "sqlite3_bind_parameter_count" : Query.t -> int;
      val PTRANSIENT = Word.~ 0w0
      
      val Pcolumn_blob   = _import "sqlite3_column_blob"   : Query.t * int -> Blob.out;
      val Pcolumn_double = _import "sqlite3_column_double" : Query.t * int -> real;
      val Pcolumn_int    = _import "sqlite3_column_int"    : Query.t * int -> int;
      val Pcolumn_int64  = _import "sqlite3_column_int64"  : Query.t * int -> Int64.int;
      val Pcolumn_text   = _import "sqlite3_column_text"   : Query.t * int -> CStr.out;
(*    val Pcolumn_text16 = _import "sqlite3_column_text16" : Query.t * int -> MLton.Pointer.t; *)
      val Pcolumn_bytes  = _import "sqlite3_column_bytes"  : Query.t * int -> int;
      val Pcolumn_type   = _import "sqlite3_column_type"   : Query.t * int -> int;
      val Pcolumn_count  = _import "sqlite3_column_count"  : Query.t -> int;
      
      (* used to satifsy meta-information queries *)
      val Pcolumn_database_name = _import "sqlite3_column_database_name" : Query.t * int -> CStr.out;
      val Pcolumn_decltype      = _import "sqlite3_column_decltype"      : Query.t * int -> CStr.out;
      val Pcolumn_name          = _import "sqlite3_column_name"          : Query.t * int -> CStr.out;
      val Pcolumn_origin_name   = _import "sqlite3_column_origin_name"   : Query.t * int -> CStr.out;
      val Pcolumn_table_name    = _import "sqlite3_column_table_name"    : Query.t * int -> CStr.out;
      
      val Pdb_handle = _import "sqlite3_db_handle" : Query.t -> DB.t;
(*    val Pquery_string = _import "sqlite3_query_string" : Query.t -> CStr.out; *)
      
      (* bind a user function *)
      val Pcreate_function = _import "sqlite3_create_function" : DB.t * CStr.t * int * int * word * FnPtr.t * FnPtr.t * FnPtr.t -> int;
      val Pcreate_collation = _import "sqlite3_create_collation" : DB.t * CStr.t * int * word * FnPtr.t -> int;
      val Puser_data = _import "sqlite3_user_data" : Context.t -> word;
      val Paggregate_context = _import "sqlite3_aggregate_context" : Context.t * int -> MLton.Pointer.t;
      
      (* fetch user function values *)
      val Pvalue_blob   = _import "sqlite3_value_blob"   : Value.t -> Blob.out;
      val Pvalue_double = _import "sqlite3_value_double" : Value.t -> real;
      val Pvalue_int    = _import "sqlite3_value_int"    : Value.t -> int;
      val Pvalue_int64  = _import "sqlite3_value_int64"  : Value.t -> Int64.int;
      val Pvalue_text   = _import "sqlite3_value_text"   : Value.t -> CStr.out;
(*    val Pvalue_text16 = _import "sqlite3_value_text16" : Value.t -> MLton.Pointer.t; *)
      val Pvalue_bytes  = _import "sqlite3_value_bytes"  : Value.t -> int;
      val Pvalue_type   = _import "sqlite3_value_type"   : Value.t -> int;
      
      (* set user return values *)
      val Presult_blob   = _import "sqlite3_result_blob"   : Context.t * Blob.t * int * word -> unit;
      val Presult_double = _import "sqlite3_result_double" : Context.t * real -> unit;
      val Presult_int    = _import "sqlite3_result_int"    : Context.t * int -> unit;
      val Presult_int64  = _import "sqlite3_result_int64"  : Context.t * Int64.int -> unit;
      val Presult_null   = _import "sqlite3_result_null"   : Context.t -> unit;
      val Presult_text   = _import "sqlite3_result_text"   : Context.t * CStr.t * int * word -> unit;
(*    val Presult_text16 = _import "sqlite3_result_text16" : Context.t * WideString.string * int * word -> unit; *)
      val Presult_error  = _import "sqlite3_result_error"  : Context.t * CStr.t * int -> unit;
      
      (* we don't support extended result codes; that would break the case statement *)
      (* the exec & get_table methods are better reimplemented in SML *)
      (* we don't need silly printf; this is SML! *)
      (* interrupt is a dangerous thing to add *)
      (* omitting all experimental features *)
      
      val Plibversion = _import "sqlite3_libversion" : unit -> CStr.out;
      
      val PlastInsertRowid = _import "sqlite3_last_insert_rowid" : DB.t -> Int64.int;
      val Pchanges = _import "sqlite3_changes" : DB.t -> int;
      val PtotalChanges = _import "sqlite3_total_changes" : DB.t -> int;
      val PgetAutocommit = _import "sqlite3_get_autocommit" : DB.t -> int;
      val PsetAuthorizer = _import "sqlite3_set_authorizer" : DB.t * FnPtr.t * word -> int;
      
      (* ---------------------------------------------------------------------------- *)
      
      val version = CStr.toString (Plibversion ())
      
      fun why db = valOf (CStr.toStringOpt (Perrmsg db))
      fun code (_,   0) = ()                   (* #define SQLITE_OK           0   /* Successful result */ *)
        | code (db,  1) = raise Error (why db) (* #define SQLITE_ERROR        1   /* SQL error or missing database */ *)
        | code (db,  2) = raise Error (why db) (* #define SQLITE_INTERNAL     2   /* An internal logic error in SQLite */ *)
        | code (db,  3) = raise Error (why db) (* #define SQLITE_PERM         3   /* Access permission denied */ *)
        | code (db,  4) = raise Abort (why db) (* #define SQLITE_ABORT        4   /* Callback routine requested an abort */ *)
        | code (db,  5) = raise Retry (why db) (* #define SQLITE_BUSY         5   /* The database file is locked */ *)
        | code (db,  6) = raise Retry (why db) (* #define SQLITE_LOCKED       6   /* A table in the database is locked */ *)
        | code (db,  7) = raise Abort (why db) (* #define SQLITE_NOMEM        7   /* A malloc() failed */ *)
        | code (db,  8) = raise Abort (why db) (* #define SQLITE_READONLY     8   /* Attempt to write a readonly database */ *)
        | code (db,  9) = raise Retry (why db) (* #define SQLITE_INTERRUPT    9   /* Operation terminated by sqlite_interrupt() */ *)
        | code (db, 10) = raise Error (why db) (* #define SQLITE_IOERR       10   /* Some kind of disk I/O error occurred */ *)
        | code (db, 11) = raise Error (why db) (* #define SQLITE_CORRUPT     11   /* The database disk image is malformed */ *)
        | code (db, 12) = raise Error (why db) (* #define SQLITE_NOTFOUND    12   /* (Internal Only) Table or record not found */ *)
        | code (db, 13) = raise Abort (why db) (* #define SQLITE_FULL        13   /* Insertion failed because database is full */ *)
        | code (db, 14) = raise Error (why db) (* #define SQLITE_CANTOPEN    14   /* Unable to open the database file */ *)
        | code (db, 15) = raise Error (why db) (* #define SQLITE_PROTOCOL    15   /* Database lock protocol error */ *)
        | code (db, 16) = raise Error (why db) (* #define SQLITE_EMPTY       16   /* (Internal Only) Database table is empty */ *)
        | code (db, 17) = raise Retry (why db) (* #define SQLITE_SCHEMA      17   /* The database schema changed */ *)
        | code (db, 18) = raise Abort (why db) (* #define SQLITE_TOOBIG      18   /* Too much data for one row of a table */ *)
        | code (db, 19) = raise Abort (why db) (* #define SQLITE_CONSTRAINT  19   /* Abort due to constraint violation */ *)
        | code (db, 20) = raise Abort (why db) (* #define SQLITE_MISMATCH    20   /* Data type mismatch */ *)
        | code (db, 21) = raise Error (why db) (* #define SQLITE_MISUSE      21   /* Library used incorrectly */ *)
        | code (db, 22) = raise Error (why db) (* #define SQLITE_NOLFS       22   /* Uses OS features not supported on host */ *)
        | code (db, 23) = raise Abort (why db) (* #define SQLITE_AUTH        23   /* Authorization denied */ *)
        | code _  = raise Error "SQLite returned an unknown error code"
      
      fun openDB filename =
         let
            val dbp = ref DB.null
            val r = PopenDB (CStr.fromString filename, dbp)
            val db = !dbp
         in
            if r = 0 then db else
            raise Error (why db before ignore (PcloseDB db))
         end
      
      fun closeDB db = code (db, PcloseDB db)
      
      fun prepare (db, qs) =
         let
            val l = String.size qs
            val q = ref Query.null
            val t = ref MLton.Pointer.null (* we can't use this... GC could happen *)
         in
            code (db, Pprepare (db, CStr.fromString qs, l, q, t));
            !q
         end
      
      fun wrap (q, r) =
         if r = 0 then () else
         code (Pdb_handle q, r)
      
      fun finalize q = wrap (q, Pfinalize q)
      fun reset q = wrap (q, Preset q)
      fun step q = 
         case (Pstep q) of
            100 => true  (* #define SQLITE_ROW         100  /* sqlite_step() has another row ready */ *)
          | 101 => false (* #define SQLITE_DONE        101  /* sqlite_step() has finished executing */ *)
          | r => (wrap (q, r); raise Error "unreachable; step wrapper should raise")
      fun clearbindings q = wrap (q, Pclearbindings q)
      
(*    fun query_string q = valOf (CStr.toStringOpt (Pquery_string q)) *)
      
      datatype storage = INTEGER of Int64.int
                       | REAL of real
                       | STRING of string (* WideString.string? *)
                       | BLOB of Word8Vector.vector
                       | NULL
      
      fun bindings q = Pbind_parameter_count q
      
      fun bindB (q, i, b) = wrap (q, Pbind_blob (q, i, Blob.fromVector b, Word8Vector.length b, PTRANSIENT))
      fun bindR (q, i, d) = wrap (q, Pbind_double (q, i, d))
      fun bindI (q, i, z) = wrap (q, Pbind_int (q, i, z))
      fun bindZ (q, i, z) = wrap (q, Pbind_int64 (q, i, z))
      fun bindN (q, i,()) = wrap (q, Pbind_null (q, i))
      fun bindS (q, i, s) = wrap (q, Pbind_text (q, i, CStr.fromString s, String.size s, PTRANSIENT))
      
      fun bindX (q, i, INTEGER z) = bindZ (q, i, z)
        | bindX (q, i, REAL r) = bindR (q, i, r)
        | bindX (q, i, STRING s) = bindS (q, i, s)
        | bindX (q, i, BLOB b) = bindB (q, i, b)
        | bindX (q, i, NULL) = bindN (q, i, ())
      
      fun cols q = Pcolumn_count q
      
      
      fun fetchB (q, i) = Blob.toVector (Pcolumn_blob (q, i),
                                         Pcolumn_bytes (q, i))
      fun fetchR (q, i) = Pcolumn_double (q, i)
      fun fetchI (q, i) = Pcolumn_int (q, i)
      fun fetchZ (q, i) = Pcolumn_int64 (q, i)
      fun fetchN (_, _) = ()
      fun fetchS (q, i) = CStr.toStringLen (Pcolumn_text (q, i), 
                                            Pcolumn_bytes (q, i))
      
      fun fetchX (q, i) =
         case Pcolumn_type (q, i) of
            1 => INTEGER (fetchZ (q, i))
          | 2 => REAL (fetchR (q, i))
          | 3 => STRING (fetchS (q, i))
          | 4 => BLOB (fetchB (q, i))
          | 5 => NULL
          | _ => raise Error "SQLite handed SML an invalid storage type"
      
      type column = { name: string,
                      origin: { table:  string,
                                db:     string,
                                decl:   string,
                                schema: string } 
                              option }
      fun fetchName (q, i) = CStr.toString (Pcolumn_name (q, i))
      fun fetchMeta (q, i) = 
         let
            fun get f = CStr.toString (f (q, i))
            val name = get Pcolumn_name
         in
            case CStr.toStringOpt (Pcolumn_decltype (q, i)) of
               NONE => { name = name, origin = NONE }
             | SOME decl =>
              { name = name,
                origin = SOME { table  = get Pcolumn_table_name,
                                db     = get Pcolumn_database_name,
                                decl   = decl,
                                schema = get Pcolumn_origin_name } }
         end
      fun columns q = Vector.tabulate (cols q, fn i => fetchName (q, i))
      fun meta q = Vector.tabulate (cols q, fn i => fetchMeta (q, i))
      
      fun valueB v = Blob.toVector (Pvalue_blob v, Pvalue_bytes v)
      fun valueR v = Pvalue_double v
      fun valueI v = Pvalue_int v
      fun valueZ v = Pvalue_int64 v
      fun valueN _ = ()
      fun valueS v = CStr.toStringLen (Pvalue_text v, Pvalue_bytes v)
      
      fun valueX v =
         case Pvalue_type v of
            1 => INTEGER (valueZ v)
          | 2 => REAL (valueR v)
          | 3 => STRING (valueS v)
          | 4 => BLOB (valueB v)
          | 5 => NULL
          | _ => raise Error "SQLite handed SML an invalid storage type"
      
      fun resultB (c, b) = Presult_blob (c, Blob.fromVector b, Word8Vector.length b, PTRANSIENT)
      fun resultR (c, d) = Presult_double (c, d)
      fun resultI (c, z) = Presult_int (c, z)
      fun resultZ (c, z) = Presult_int64 (c, z)
      fun resultN (c,()) = Presult_null c
      fun resultS (c, s) = Presult_text (c, CStr.fromString s, String.size s, PTRANSIENT)
      
      fun resultX (c, INTEGER z) = resultZ (c, z)
        | resultX (c, REAL r) = resultR (c, r)
        | resultX (c, STRING s) = resultS (c, s)
        | resultX (c, BLOB b) = resultB (c, b)
        | resultX (c, NULL) = resultN (c, ())
      
      datatype hook = UFN of int | COLL of int | AGGR of int | AUTH of int
      fun catch error f x =
         f x
         handle Error x => error ("Exception Error \"" ^ x ^ "\" escaped callback.")
              | Retry x => error ("Exception Retry \"" ^ x ^ "\" escaped callback.")
              | Abort x => error ("Exception Abort \"" ^ x ^ "\" escaped callback.")
              | _ => error ("SML Exception escaped callback.")
      
      (************************************************* Scalar functions *)
      val fnt : (Context.t * Value.t vector -> unit) Buffer.t = Buffer.empty ()
      fun fnCallback (context, numargs, args) =
         let
            val f = Buffer.sub (fnt, Word.toInt (Puser_data context))
            fun get i = Value.fromPtr (MLton.Pointer.getPointer (args, i))
            val args = Vector.tabulate (numargs, get)
            fun error s = Presult_error (context, CStr.fromString s, String.size s)
         in
            catch error f (context, args)
         end
      val () = _export "mlton_sqlite3_ufnhook" private : (Context.t * int * MLton.Pointer.t -> unit) -> unit;
                  fnCallback
      val fnCallbackPtr = _address "mlton_sqlite3_ufnhook" private : FnPtr.t;
      
      fun createFunction (db, name, f, n) =
         let
            val id = Buffer.push (fnt, f)
            val r = Pcreate_function (
                       db, CStr.fromString name, n, 1, Word.fromInt id,
                       fnCallbackPtr, FnPtr.null, FnPtr.null)
         in
            code (db, r); 
            UFN id
         end
      
      (************************************************* Aggregate functions *)
      type aggregate = {
         step: Context.t * Value.t vector -> unit,
         final: Context.t -> unit }
      val aggen : (unit -> aggregate) Buffer.t = Buffer.empty ()
      val agtbl : aggregate Buffer.t = Buffer.empty ()
      fun fetchAggr context =
         let
            val magic = 0wxa72b (* new records are zero, we mark them magic *)
            val ptr = Paggregate_context (context, 8)
         in
            if MLton.Pointer.getWord32 (ptr, 0) = magic
            then Word32.toInt (MLton.Pointer.getWord32 (ptr, 1)) else
            let 
               val ig = Word.toInt (Puser_data context)
               val ag = Buffer.sub (aggen, ig) ()
               val it = Buffer.push (agtbl, ag)
               val () = MLton.Pointer.setWord32 (ptr, 0, magic)
               val () = MLton.Pointer.setWord32 (ptr, 1, Word32.fromInt it)
            in
               it
            end
         end
      fun agStepCallback (context, numargs, args) =
         let
            val it = fetchAggr context
            fun get i = Value.fromPtr (MLton.Pointer.getPointer (args, i))
            val args = Vector.tabulate (numargs, get)
            fun error s = Presult_error (context, CStr.fromString s, String.size s)
            val { step, final=_ } = Buffer.sub (agtbl, it)
         in
            catch error step (context, args)
         end
      fun agFinalCallback context =
         let
            val it = fetchAggr context
            fun error s = Presult_error (context, CStr.fromString s, String.size s)
            val { step=_, final } = Buffer.sub (agtbl, it)
         in
            catch error final context;
            Buffer.free (agtbl, it)
         end
      val () = _export "mlton_sqlite3_uagstep" private : (Context.t * int * MLton.Pointer.t -> unit) -> unit;
                  agStepCallback
      val () = _export "mlton_sqlite3_uagfinal" private : (Context.t -> unit) -> unit;
                  agFinalCallback
      val agStepCallbackPtr = _address "mlton_sqlite3_uagstep" private : FnPtr.t;
      val agFinalCallbackPtr = _address "mlton_sqlite3_uagfinal" private : FnPtr.t;
      
      fun createAggregate (db, name, gen, n) =
         let
            val id = Buffer.push (aggen, gen)
            val r = Pcreate_function (
                       db, CStr.fromString name, n, 1, Word.fromInt id,
                       FnPtr.null, agStepCallbackPtr, agFinalCallbackPtr)
         in
             code (db, r); 
             AGGR id
         end
         
      (************************************************* Collation functions *)
      val colt : (string * string -> order) Buffer.t = Buffer.empty ()
      fun colCallback (uarg, s1l, s1p, s2l, s2p) =
         let
            val col = Buffer.sub (colt, Word.toInt uarg)
            (* don't propogate an exception up as it will segfault.
             * do complain that this is bad!
             *)
            fun error s = (
               TextIO.output (TextIO.stdErr,
                              s ^ " -- forbidden! Collations cannot tell SQLite.\n");
               EQUAL)
         in
            case catch error col (CStr.toStringLen (s1p, s1l), 
                                  CStr.toStringLen (s2p, s2l)) of
                LESS => ~1
              | EQUAL => 0
              | GREATER => 1
         end
      val () = _export "mlton_sqlite3_colhook" private : (word * int * CStr.out * int * CStr.out -> int) -> unit;
                  colCallback
      val colCallbackPtr = _address "mlton_sqlite3_colhook" private : FnPtr.t;
      fun createCollation (db, name, f) =
         let
            val id = Buffer.push (colt, f)
            val r = Pcreate_collation (
                       db, CStr.fromString name, 1, Word.fromInt id,
                       colCallbackPtr)
         in
            code (db, r);
            COLL id
         end
      
      (************************************************* End of user functions *)
      
      val lastInsertRowid = PlastInsertRowid
      val changes = Pchanges
      val totalChanges = PtotalChanges
      fun getAutocommit db = PgetAutocommit db <> 0
      
      datatype access = ALLOW | DENY | IGNORE
      datatype request =
         CREATE_INDEX of { index: string, table: string, db: string, temporary: bool }
       | CREATE_TABLE of { table: string, db: string, temporary: bool }
       | CREATE_TRIGGER of { trigger: string, table: string, db: string, temporary: bool }
       | CREATE_VIEW of { view: string, db: string, temporary: bool }
       | DROP_INDEX of { index: string, table: string, db: string, temporary: bool }
       | DROP_TABLE of { table: string, db: string, temporary: bool }
       | DROP_TRIGGER of { trigger: string, table: string, db: string, temporary: bool }
       | DROP_VIEW of { view: string, db: string, temporary: bool }
       | ALTER_TABLE of { db: string, table: string }
       | REINDEX of { index: string, db: string }
       | ANALYZE of { table: string, db: string }
       | INSERT of { table: string, db: string }
       | UPDATE of { table: string, column: string, db: string }
       | DELETE of { table: string, db: string }
       | TRANSACTION of { operation: string }
       | SELECT
       | READ of { table: string, column: string, db: string  }
       | PRAGMA of { pragma: string, arg: string, db: string option }
       | ATTACH of { file: string }
       | DETACH of { db: string }
       | CREATE_VTABLE of { table: string, module: string, db: string }
       | DROP_VTABLE of { table: string, module: string, db: string  }
       | FUNCTION of { function: string }
      
      fun switchRequest ( 1, a, b, c) = CREATE_INDEX { index = valOf a, table = valOf b, db = valOf c, temporary = false }
        | switchRequest ( 3, a, b, c) = CREATE_INDEX { index = valOf a, table = valOf b, db = valOf c, temporary = true }
        | switchRequest ( 2, a, _, c) = CREATE_TABLE { table = valOf a, db = valOf c, temporary = false }
        | switchRequest ( 4, a, _, c) = CREATE_TABLE { table = valOf a, db = valOf c, temporary = true }
        | switchRequest ( 7, a, b, c) = CREATE_TRIGGER { trigger = valOf a, table = valOf b, db = valOf c, temporary = false }
        | switchRequest ( 5, a, b, c) = CREATE_TRIGGER { trigger = valOf a, table = valOf b, db = valOf c, temporary = true }
        | switchRequest ( 8, a, _, c) = CREATE_VIEW { view = valOf a, db = valOf c, temporary = false }
        | switchRequest ( 6, a, _, c) = CREATE_VIEW { view = valOf a, db = valOf c, temporary = true }
        | switchRequest (10, a, b, c) = DROP_INDEX { index = valOf a, table = valOf b, db = valOf c, temporary = false }
        | switchRequest (12, a, b, c) = DROP_INDEX { index = valOf a, table = valOf b, db = valOf c, temporary = true }
        | switchRequest (11, a, _, c) = DROP_TABLE { table = valOf a, db = valOf c, temporary = false }
        | switchRequest (13, a, _, c) = DROP_TABLE { table = valOf a, db = valOf c, temporary = true }
        | switchRequest (16, a, b, c) = DROP_TRIGGER { trigger = valOf a, table = valOf b, db = valOf c, temporary = false }
        | switchRequest (14, a, b, c) = DROP_TRIGGER { trigger = valOf a, table = valOf b, db = valOf c, temporary = true }
        | switchRequest (17, a, _, c) = DROP_VIEW { view = valOf a, db = valOf c, temporary = false }
        | switchRequest (15, a, _, c) = DROP_VIEW { view = valOf a, db = valOf c, temporary = true }
        | switchRequest (26, a, b, _) = ALTER_TABLE { db = valOf a, table = valOf b }
        | switchRequest (27, a, _, c) = REINDEX { index = valOf a, db = valOf c }
        | switchRequest (28, a, _, c) = ANALYZE { table = valOf a, db = valOf c }
        | switchRequest (18, a, _, c) = INSERT { table = valOf a, db = valOf c }
        | switchRequest (23, a, b, c) = UPDATE { table = valOf a, column = valOf b, db = valOf c }
        | switchRequest ( 9, a, _, c) = DELETE { table = valOf a, db = valOf c }
        | switchRequest (22, a, _, _) = TRANSACTION { operation = valOf a }
        | switchRequest (21, _, _, _) = SELECT
        | switchRequest (20, a, b, c) = READ { table = valOf a, column = valOf b, db = valOf c }
        | switchRequest (19, a, b, c) = PRAGMA { pragma = valOf a, arg = valOf b, db = c }
        | switchRequest (24, a, _, _) = ATTACH { file = valOf a }
        | switchRequest (25, a, _, _) = DETACH { db = valOf a }
        | switchRequest (29, a, b, c) = CREATE_VTABLE { table = valOf a, module = valOf b, db = valOf c }
        | switchRequest (30, a, b, c) = DROP_VTABLE { table = valOf a, module = valOf b, db = valOf c }
        | switchRequest (31, _, b, _) = FUNCTION { function = valOf b }
        | switchRequest (_, _, _, _) = raise Error "SQLite requested impossible authorization code"
      fun parseRequest (code, a, b, c, d) = (* !!! expose trigged? => d !!! *)
         switchRequest (code, CStr.toStringOpt a, 
                              CStr.toStringOpt b, 
                              CStr.toStringOpt c)
         handle Option => raise Error "SQLite did not provided expected authorization paramater"
      
      val autht : (request -> access) Buffer.t = Buffer.empty ()
      fun authCallback (uarg, code, a, b, c, d) =
         let
            val auth = Buffer.sub (autht, Word.toInt uarg)
            (* don't propogate an exception up as it will segfault.
             * do complain that this is bad!
             *)
            fun error s = (
               TextIO.output (TextIO.stdErr,
                              s ^ " -- forbidden! Authorizers cannot tell SQLite.\n");
               DENY)
         in
            case catch error auth (parseRequest (code, a, b, c, d)) of
                ALLOW => 0
              | DENY => 1
              | IGNORE => 2
         end
      val () = _export "mlton_sqlite3_authhook" private : (word * int * CStr.out * CStr.out * CStr.out * CStr.out -> int) -> unit;
                  authCallback
      val authCallbackPtr = _address "mlton_sqlite3_authhook" private : FnPtr.t;
      fun unsetAuthorizer db = code (db, PsetAuthorizer (db, FnPtr.null, 0w0))
      fun setAuthorizer (db, auth) =
         let
            val id = Buffer.push (autht, auth)
            val r = PsetAuthorizer (db, authCallbackPtr, Word.fromInt id)
         in
            code (db, r);
            AUTH id
         end
      
      fun unhook (UFN  x) = Buffer.free (fnt, x)
        | unhook (COLL x) = Buffer.free (colt, x)
        | unhook (AGGR x) = Buffer.free (aggen, x)
        | unhook (AUTH x) = Buffer.free (autht, x)
      
      type db = DB.t
      type query = Query.t
      type value = Value.t
      type context = Context.t
   end
