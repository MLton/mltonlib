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

   val vars = ref [("SML_COMPILER", ${SML_COMPILER})]

   fun getVar var =
       case List.find (fn (i, _) => i = var) (!vars)
        of SOME (_, v) => v
         | NONE =>
           case OS.Process.getEnv var
            of NONE   => error ["Undefined variable: ", var]
             | SOME v => v

   fun expandVars path = let
      fun outside os =
       fn #"$" :: #"{" :: is => inside os [] is
        | c            :: is => outside (c::os) is
        |                 [] => implode (rev os)
      and inside os vs =
       fn #"}" :: is => outside os (explode (getVar (implode (rev vs))) @ is)
        | c    :: is => inside os (c::vs) is
        |         [] => error ["Unclosed variable reference"]
   in
      outside [] (explode path)
   end

   val using : string option ref = ref NONE

   fun useNoTrace path = let
      val path = expandVars path
      val () = if OS.FileSys.access (path, [OS.FileSys.A_READ])
               then ()
               else error ["Unreadable file: ", path]
      val path = OS.FileSys.fullPath path
      val oldUsing = !using
   in
      using := SOME path
    ; after (fn () => use path,
             fn () => using := oldUsing)
   end

   structure Trace = struct
      datatype t =
         CHDIR of string
       | USE   of string
      local
         val theTrace : t list ref = ref []
         val recTrace = ref false
         fun scoped t th =
             case !recTrace
              of old => (recTrace := t
                       ; after (th, fn () => recTrace := old))
      in
         fun load path =
             scoped true (fn () => (useNoTrace path
                                  ; rev (!theTrace) before theTrace := []))

         fun fmt {expandVars = e} = let
            val expandVars = if e then expandVars else fn x => x
         in
            concat o List.concat o
            map (fn CHDIR path =>
                    ["OS.FileSys.chDir \"", expandVars path, "\" ;\n"]
                  | USE path =>
                    ["use \"", expandVars path, "\" ;\n"])
         end

         fun disabled th = scoped false th

         fun trace th = if !recTrace then theTrace := th () :: !theTrace else ()
      end
   end

   open Trace

   fun use path = (trace (fn () => USE path) ; useNoTrace path)

   val loading : string list ref = ref []
   val loaded : string list ref = ref []

   fun lib {reqs, self} =
       case !using
        of NONE      => error ["Current file unknown"]
         | SOME path =>
           if List.exists (fn p => path = p) (!loaded)
           then ()
           else if List.exists (fn p => path = p) (!loading)
           then error ("Cyclic library dependency: " ::
                       foldl (fn (p, ps) => p::" -> "::ps) [path] (!loading))
           else let
                 val cwd = OS.FileSys.getDir ()
                 val dir = OS.Path.dir path
                 val () = if dir <> cwd
                          then (OS.FileSys.chDir dir
                              ; trace (fn () => CHDIR (OS.Path.mkRelative
                                                          {path = dir,
                                                           relativeTo = cwd})))
                          else ()
                 val cv = ${SILENT}
                 val was = !loading
              in
                 loading := path :: was
               ; after (fn () =>
                           (app useNoTrace reqs
                          ; app use self
                          ; loaded := path :: !loaded),
                        fn () =>
                           (${VERBOSE} cv
                          ; loading := was
                          ; if dir <> cwd
                            then (OS.FileSys.chDir cwd
                                ; trace (fn () =>
                                            CHDIR (OS.Path.mkRelative
                                                      {path = cwd,
                                                       relativeTo = dir})))
                            else ()))
              end
end
