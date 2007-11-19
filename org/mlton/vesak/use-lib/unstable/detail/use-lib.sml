(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

${PRELUDE}

structure UseLib :> USE_LIB = struct
   fun after (th, ef) =
       ((case th () of v => fn () => (ef () ; v))
        handle e => fn () => (ef () ; raise e)) ()

   fun error strs = raise Fail (concat strs)

   structure Var = struct
      val vars = ref [("SML_COMPILER", ${SML_COMPILER})]

      fun get var =
          case List.find (fn (i, _) => i = var) (!vars)
           of SOME (_, v) => v
            | NONE =>
              case OS.Process.getEnv var
               of NONE   => error ["Undefined variable: ", var]
                | SOME v => v

      fun expand path = let
         fun outside os =
          fn #"$" :: #"{" :: is => inside os [] is
           | c            :: is => outside (c::os) is
           |                 [] => implode (rev os)
         and inside os vs =
          fn #"}" :: is => outside os (explode (get (implode (rev vs))) @ is)
           | c    :: is => inside os (c::vs) is
           |         [] => error ["Unclosed variable reference"]
      in
         outside [] (explode path)
      end
   end

   local
      type entry = {path : string, loading : string list}
      val libStack : entry list list ref = ref []
      val useQueue : entry list ref = ref []
   in
      fun pushUse e = useQueue := !useQueue @ [e]
      fun popUse () =
          case !useQueue
           of [] => error ["Each lib must be used as a unique .use file"]
            | e::es => (useQueue := es ; e)
      fun pushLib () =
          (libStack := !useQueue :: !libStack
         ; useQueue := [])
      fun popLib () =
          case !libStack
           of [] => error ["Internal error: Unmatched popLib"]
            | e::es => (libStack := es
                      ; useQueue := !useQueue @ e)
      fun clear () = (libStack := [] ; useQueue := [])
   end

   fun useLoading loading path = let
      val path = Var.expand path
      val () = if OS.FileSys.access (path, [OS.FileSys.A_READ])
               then ()
               else error ["File ", path, " is unreadable from ",
                           OS.FileSys.getDir ()]
      val path = OS.FileSys.fullPath path
   in
      if not (String.isSuffix ".use" path)
      then ()
      else if List.exists (fn p => path = p) loading
      then error ("Cyclic library dependency: "::
                  foldl (fn (p, ps) => p::" -> "::ps) [path] loading)
      else pushUse {path = path, loading = loading}
    ; use path
   end

   local
      val loaded : string list ref = ref []
   in
      fun lib paths = let
         val {path, loading} = popUse ()
      in
         if List.exists (fn p => path = p) (!loaded)
         then ()
         else let
               val dir = OS.Path.mkRelative
                            {path = OS.Path.dir path,
                             relativeTo = OS.FileSys.getDir ()}
               val cv = ${SILENT}
               val loading = path :: loading
            in
               pushLib ()
             ; after (fn () =>
                         (app (fn file =>
                                  useLoading
                                     loading
                                     (OS.Path.concat (dir, file))) paths
                        ; loaded := path :: !loaded),
                      fn () =>
                         (popLib ()
                        ; ${VERBOSE} cv))
            end
      end
   end

   val use = useLoading []

   val use = fn path => use path handle e => (clear () ; raise e)
   val lib = fn paths => lib paths handle e => (clear () ; raise e)
end
