(****************************************************************** Operators *)

infix 8 ++% **%      (* additive and multiplicative exponentiation *)
infix 7 *% /% %% //% (*  //% = (/%, %%) = (div, mod) *)
infix 6 +% -%
infix 4 =% !=% <%    (* <>% would imply <% which does not always exist *)
nonfix ~% !% #%

(* the operators in an algebra are always these (above for scalars) *)
infix 8 ++$ **$
infix 7 *$ /$ %$ //$
infix 6 +$ -$
infix 4 =$ !=$ <$
nonfix ~$ !$ #$

infix 7 *& (* for scalar operations with a vector *)
