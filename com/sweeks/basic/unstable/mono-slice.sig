signature MONO_SLICE = sig

   type t

   include GENERIC_SLICE where type 'a t0 = t
   
end
