signature SLICE = sig

   type 'a t

   include GENERIC_SLICE where type 'a t0 = 'a t

end
