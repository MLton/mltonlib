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
      val Pfree   = _import "sqlite3_free"   : CStr.t -> unit;
      val Perrmsg = _import "sqlite3_errmsg" : DB.t -> CStr.out;
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
      val Pquery_string = _import "sqlite3_query_string" : Query.t -> CStr.out;
      
      (* bind a user function *)
      val Pcreate_function = _import "sqlite3_create_function" : DB.t * CStr.t * int * int * word * FnPtr.t * FnPtr.t * FnPtr.t -> int;
      val Puser_data = _import "sqlite3_user_data" : Context.t -> word;
      
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
      
      (* expiry should just raise an exception... *)
      
      (* we don't support extended result codes; that would break the case statement *)
      
      (* the exec & get_table methods are better reimplemented in SML *)
      
      (* autocommit defaults to on. let's leave it that way! *)
      
      (* interrupt would require callback hooks during progress; we have none *)
      
      val Plibversion = _import "sqlite3_libversion" : unit -> CStr.out;
      
      (* we don't need silly printf; this is SML! *)
      
      (* changes and total_changes might be useful to add *)
      
      (* ---------------------------------------------------------------------------- *)
      
      val version = CStr.toString (Plibversion ())
      
      fun why db = valOf (CStr.toStringOpt (Perrmsg db))
      fun code (db,  0) = ()                   (* #define SQLITE_OK           0   /* Successful result */ *)
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
        | code (db, _)  = raise Error"unknown error code"
      
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
          | r => (wrap (q, r); raise Error "unreachable")
      fun clearbindings q = wrap (q, Pclearbindings q)
      
      fun query_string q = valOf (CStr.toStringOpt (Pquery_string q))
      
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
      fun bindN (q, i) = wrap (q, Pbind_null (q, i))
      fun bindS (q, i, s) = wrap (q, Pbind_text (q, i, CStr.fromString s, String.size s, PTRANSIENT))
      
      fun bindX (q, i, INTEGER z) = bindZ (q, i, z)
        | bindX (q, i, REAL r) = bindR (q, i, r)
        | bindX (q, i, STRING s) = bindS (q, i, s)
        | bindX (q, i, BLOB b) = bindB (q, i, b)
        | bindX (q, i, NULL) = bindN (q, i)
      
      fun cols q = Pcolumn_count q
      
      
      fun fetchB (q, i) = Blob.toVector (Pcolumn_blob (q, i),
                                         Pcolumn_bytes (q, i))
      fun fetchR (q, i) = Pcolumn_double (q, i)
      fun fetchI (q, i) = Pcolumn_int (q, i)
      fun fetchZ (q, i) = Pcolumn_int64 (q, i)
      fun fetchN (q, i) = ()
      fun fetchS (q, i) = CStr.toStringLen (Pcolumn_text (q, i), 
                                            Pcolumn_bytes (q, i))
      
      fun fetchX (q, i) =
         case Pcolumn_type (q, i) of
            1 => INTEGER (fetchZ (q, i))
          | 2 => REAL (fetchR (q, i))
          | 3 => STRING (fetchS (q, i))
          | 4 => BLOB (fetchB (q, i))
          | 5 => NULL
          | _ => raise Error "Invalid storage type"
      
      type column = { name: string }
(*                    origin: { table:  string,
                                db:     string,
                                decl:   string,
                                schema: string } 
                              option }
*)
      fun fetchMeta (q, i) = 
         let
            fun get f = CStr.toString (f (q, i))
            val name = get Pcolumn_name
         in
            { name = name }
(* usually not compiled into sqlite3:
            case CStr.toStringOpt (Pcolumn_decltype (q, i)) of
               NONE => { name = name, origin = NONE }
             | SOME decl =>
              { name = name,
                origin = SOME { table  = get Pcolumn_table_name,
                                db     = get Pcolumn_database_name,
                                decl   = decl,
                                schema = get Pcolumn_origin_name } }
*)
         end
      fun meta q = Vector.tabulate (cols q, fn i => fetchMeta (q, i))
      
      fun valueB v = Blob.toVector (Pvalue_blob v, Pvalue_bytes v)
      fun valueR v = Pvalue_double v
      fun valueI v = Pvalue_int v
      fun valueZ v = Pvalue_int64 v
      fun valueN v = ()
      fun valueS v = CStr.toStringLen (Pvalue_text v, Pvalue_bytes v)
      
      fun valueX v =
         case Pvalue_type v of
            1 => INTEGER (valueZ v)
          | 2 => REAL (valueR v)
          | 3 => STRING (valueS v)
          | 4 => BLOB (valueB v)
          | 5 => NULL
          | _ => raise Error "Invalid storage type"
      
      fun resultB (c, b) = Presult_blob (c, Blob.fromVector b, Word8Vector.length b, PTRANSIENT)
      fun resultR (c, d) = Presult_double (c, d)
      fun resultI (c, z) = Presult_int (c, z)
      fun resultZ (c, z) = Presult_int64 (c, z)
      fun resultN c = Presult_null c
      fun resultS (c, s) = Presult_text (c, CStr.fromString s, String.size s, PTRANSIENT)
      
      fun resultX (c, INTEGER z) = resultZ (c, z)
        | resultX (c, REAL r) = resultR (c, r)
        | resultX (c, STRING s) = resultS (c, s)
        | resultX (c, BLOB b) = resultB (c, b)
        | resultX (c, NULL) = resultN c
      
      type callback = Context.t * Value.t vector -> unit
      
      (* !!! Space leak !!! *)
      val cfns = Buffer.empty ()
      
      fun fnCallback (context, numargs, args) =
         let
            val cfn = Buffer.sub (cfns, Word.toInt (Puser_data context))
            fun get i = Value.fromPtr (MLton.Pointer.getPointer (args, i))
            val args = Vector.tabulate (numargs, get)
         in
            cfn (context, args)
         end
      val () = _export "mlton_sqlite3_ufnhook" : (Context.t * int * MLton.Pointer.t -> unit) -> unit;
                  fnCallback
      val fnCallbackPtr = _address "mlton_sqlite3_ufnhook" : FnPtr.t;
      
(*
      fun createFunction (db, name, NONE, _) = 
             code (db, Pcreate_function (db, CStr.fromString name, 0, 1, 0w0,
                                         FnPtr.null, FnPtr.null, FnPtr.null))
*)
      fun createFunction (db, name, f, n) =
             code (db, Pcreate_function (db, CStr.fromString name, n, 1, 
                                         Word.fromInt (Buffer.push (cfns, f)),
                                         fnCallbackPtr, FnPtr.null, FnPtr.null))
      
      type db = DB.t
      type query = Query.t
      type value = Value.t
      type context = Context.t
   end
