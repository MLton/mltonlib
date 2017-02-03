signature BIT_FIELD = BIT_FIELD
signature BIT_FIELD_W = BIT_FIELD_W
functor BitField (W : WORD) :> BIT_FIELD_W where type word = W.t = BitField (W)

signature HISTOGRAM = HISTOGRAM
structure Histogram :> HISTOGRAM = Histogram

signature PROBABILITY_VECTOR = PROBABILITY_VECTOR
functor ProbabilityVector (type item
                           val items : item vector) : PROBABILITY_VECTOR
  = ProbabilityVector (type item = item
                       val items = items)

signature SUBSETS = SUBSETS
structure Subsets :> SUBSETS = Subsets

signature TIME_LIMIT = TIME_LIMIT
structure TimeLimit :> TIME_LIMIT = TimeLimit

signature RANDOM = RANDOM
functor MkRandom (RNG : RNG)
   :> RANDOM where type RNG.t = RNG.t
   = MkRandom (RNG)

(**
 * A high-quality pseudo random value generator for simulations (but
 * not for cryptographical purposes).
 *)
structure Random = MkRandom (MersenneTwister)

signature DECK = DECK
structure Deck = MkDeck (MersenneTwister)
