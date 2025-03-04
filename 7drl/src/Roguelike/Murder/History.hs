module Roguelike.Murder.History where

import Yaifl.Prelude
import System.Random.Stateful
import Data.List ((!!), delete)

data Murder a = Murder
  { motive :: Motive
  , cause :: HistoryEvent a
  , murderer :: a
  , victim :: a
  , involved :: Maybe a
  }

data Motive =
  Revenge
  | Jealousy
  | JealousLover
  | SpurnedLove
  | Inheritance
  | Stealing

data HistoryEvent a =
    -- linked to revenge
    -- V wronged #2
    WrongedSomeone a a
    -- V wronged person #1, who was friends with #2.
    | WrongedAFriend a a a
    -- linked to jealousy
    -- V was envied by #2
    | EnviedBy a a
    -- linked to jealous lover
    -- V was in love with #2, and #3 was also in love with #2.
    | LoveTriangle a a a
    -- linked to spurned love
    -- V was in a relationship with #2, who was loved by #3.
    | SpurnedLoveFrom a a a
    -- linked to inheritance
    -- V was passed over for some inheritance in favour of their sibling #2
    | PassedOver a a
    -- linked to stealing
    -- V was rich and #2 was poor.
    | IsPoorDesperate a a

deconstructEvent ::
  HistoryEvent a
  -> (a, a, Maybe a) -- victim, murderer, potential involved party
deconstructEvent = \case
  WrongedSomeone v p1 -> (v, p1, Nothing)
  WrongedAFriend v p1 p2 -> (v, p2, Just p1)
  EnviedBy v p1 -> (v, p1, Nothing)
  LoveTriangle v p1 p2 -> (v, p2, Just p1)
  SpurnedLoveFrom v p1 p2 -> (v, p2, Just p1)
  PassedOver v p1 -> (v, p1, Nothing)
  IsPoorDesperate v p1 -> (v, p1, Nothing)

murdererFromEvent ::
  HistoryEvent a
  -> a
murdererFromEvent = view _1 . deconstructEvent

victimFromEvent ::
  HistoryEvent a
  -> a
victimFromEvent = view _2 . deconstructEvent

justifyMotive ::
  Eq a
  => [a] -- ^ and a selection of people
  -> a -- ^ and a specific victim
  -> Motive -- ^ for a given motive
  -> IO (HistoryEvent a) -- create the key event that leads to this murder
justifyMotive people victim motive = do
  let pickOne l = uniformRM (0, length l - 1) globalStdGen >>= \i -> return (l !! i)
  p1 <- pickOne people
  p2 <- pickOne (p1 `delete` people)
  case motive of
    Revenge -> pickOne [WrongedSomeone victim p1, WrongedAFriend victim p1 p2]
    Jealousy -> pickOne [EnviedBy victim p1]
    JealousLover -> pickOne [LoveTriangle victim p1 p2]
    SpurnedLove -> pickOne [SpurnedLoveFrom victim p1 p2]
    Inheritance -> pickOne [WrongedSomeone victim p1, WrongedAFriend victim p1 p2]
    Stealing -> pickOne [WrongedSomeone victim p1, WrongedAFriend victim p1 p2]
