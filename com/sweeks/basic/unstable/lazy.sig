signature LAZY = sig

   val memo: 'a thunk -> 'a thunk
   (**
    * memo f returns a function g that, the first time it is called, calls f,
    * caches the value, and upon subsequent calls returns the cached value.
    *)

end
