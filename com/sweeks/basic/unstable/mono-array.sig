signature MONO_ARRAY = sig

   type t

   include GENERIC_ARRAY where type 'a t0 = t

end
