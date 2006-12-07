signature ARRAY = sig

   type 'a t

   include GENERIC_ARRAY where type 'a t0 = 'a t
      
end
