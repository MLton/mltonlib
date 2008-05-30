(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkParsec (Arg : MK_PARSEC_DOM) :> PARSEC
   where type Sequence.t      = Arg.Sequence.t
   where type Sequence.Elem.t = Arg.Sequence.Elem.t
   where type Sequence.Pos.t  = Arg.Sequence.Pos.t
   where type State.t         = Arg.State.t =
struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infixr 6 <^> <+>
   infixr 5 <$> <$$> </> <//>
   infix  4 <\ \>
   infixr 4 </ />
   infix  2 >| andAlso
   infixr 2 |<
   infix  1 orElse >>=
   infix  0 & <|>
   infixr 0 -->
   (* SML/NJ workaround --> *)

   open Arg
   type 'a etaexp_dom = Sequence.t * State.t
   type msg = Sequence.Pos.t
   datatype 'a reply =
      OK   of 'a * 'a etaexp_dom * msg
    | FAIL of msg
   datatype 'a etaexp_cod =
      TASTE of 'a reply Thunk.t
    | EMPTY of 'a reply
   type 'a etaexp = 'a etaexp_dom -> 'a etaexp_cod
   type 'a t = 'a etaexp

   fun get (s, t) = Option.map (fn (e, s) => (e, (s, t))) (Sequence.get s)
   fun pos (s, _) = Sequence.pos s

   fun getState (s, t) = EMPTY (OK (t, (s, t), Sequence.pos s))
   fun setState t (s, _) = EMPTY (OK ((), (s, t), Sequence.pos s))

   fun taste r = TASTE (const r)

   fun parse p s =
       case case p s
             of EMPTY r  => r
              | TASTE th => th ()
        of FAIL p       => INL p
         | OK (x, s, _) => INR (x, s)

   fun fromReader reader (s, t) =
       case reader s
        of SOME (x, s) => taste (OK (x, (s, t), Sequence.pos s))
         | NONE        => EMPTY (FAIL (Sequence.pos s))

   fun fromScan scan = fromReader (scan Sequence.get)

   fun merge m =
    fn OK (x, s, _) => OK (x, s, m)
     | FAIL _       => FAIL m

   fun bindSome m =
    fn EMPTY r  => merge m r
     | TASTE th => th ()

   fun replyNone m =
    fn EMPTY r => EMPTY (merge m r)
     | other   => other

   fun return x s = EMPTY (OK (x, s, pos s))

   fun (xM >>= x2yM) s =
       case xM s
        of EMPTY (FAIL m)       => EMPTY (FAIL m)
         | EMPTY (OK (x, s, m)) => replyNone m (x2yM x s)
         | TASTE th             =>
           TASTE (fn () => case th ()
                            of FAIL e       => FAIL e
                             | OK (x, s, m) => bindSome m (x2yM x s))

   fun zero s = EMPTY (FAIL (pos s))

   fun (p <|> q) s =
       case p s
        of EMPTY (FAIL m) => replyNone m (q s)
         | other          => other

   fun guess p s =
       case p s
        of EMPTY r  => EMPTY r
         | TASTE th => case th ()
                        of FAIL _ => EMPTY (FAIL (pos s))
                         | result => taste result

   fun elem s =
       case get s
        of NONE        => EMPTY (FAIL (pos s))
         | SOME (c, s) => taste (OK (c, s, pos s))

   fun drop p s = let
      fun done f s = f (OK ((), s, pos s))
      fun some (c, s') s = if p c then lp s' else done taste s
      and body f s =
          case get s
           of NONE    => done f s
            | SOME cs => some cs s
      and lp s = body taste s
   in
      body EMPTY s
   end

   fun sat p s =
       case get s
        of NONE         => EMPTY (FAIL (pos s))
         | SOME (c, s') =>
           if p c then taste (OK (c, s', pos s')) else EMPTY (FAIL (pos s))

   fun take p = let
      fun done s =
       fn [] => EMPTY (OK ([], s, pos s))
        | cs => taste (OK (rev cs, s, pos s))
      fun lp cs s =
          case get s
           of NONE => done s cs
            | SOME (c, s') =>
              if p c
              then lp (c::cs) s'
              else done s cs
   in
      lp []
   end

   fun peek p s =
       case p s
        of EMPTY (OK (x, _, m)) => EMPTY (OK (x, s, m))
         | EMPTY (FAIL m)       => EMPTY (FAIL m)
         | TASTE th             => case th ()
                                    of OK (x, _, m) => taste (OK (x, s, m))
                                     | FAIL m       => taste (FAIL m)

   fun many p = many1 p <|> return []
   and many1 p = p >>= (fn x => many p >>= (fn xs => return (x::xs)))

   fun between b a p = b >>= (fn _ => p >>= (fn r => a >>= (fn _ => return r)))

   fun option alt p = p <|> return alt

   fun sepBy1 p s =
       p >>= (fn x => many (s >>= (fn _ => p)) >>= (fn xs => return (x::xs)))
   fun sepBy p s = sepBy1 p s <|> return []

   fun skip p = p >>= return o ignore
   fun skipMany p = skipMany1 p <|> return ()
   and skipMany1 p = p >>= (fn _ => skipMany p)

   structure Monad = MkMonadP
     (type 'a monad = 'a t
      val return = return
      val op >>= = op >>=
      val zero = zero
      val op <|> = op <|>)

   open Monad
end
