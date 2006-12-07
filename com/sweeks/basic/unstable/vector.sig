signature VECTOR = sig

   type 'a t

   include GENERIC_VECTOR where type 'a t0 = 'a t
      
end
