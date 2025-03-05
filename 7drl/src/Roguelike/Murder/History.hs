module Roguelike.Murder.History where

import Yaifl.Prelude
import System.Random.Stateful
import Data.List ((!!), delete)
import qualified Data.Vector as V

randomVectorElement ::
  MonadIO m
  => V.Vector a
  -> m a
randomVectorElement v = do
  i <- uniformRM (0, V.length v - 1) globalStdGen
  return $ v V.! i

data Murder a = Murder
  { motive :: Motive
  , cause :: HistoryEvent a
  , murderer :: a
  , victim :: a
  , involved :: Maybe a
  } deriving stock (Eq, Ord, Show, Generic, Functor)

data Motive =
  Revenge
  | Jealousy
  | JealousLover
  | SpurnedLove
  | Inheritance
  | Stealing
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

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
    -- V rejected the advances of #2.
    | SpurnedLoveFrom a a
    -- linked to inheritance
    -- V was passed over for some inheritance in favour of their sibling #2
    | PassedOver a a
    -- linked to stealing
    -- V was rich and #2 was poor.
    | IsPoorDesperate a a
    deriving stock (Eq, Ord, Show, Generic, Functor)
makeMurder ::
  Motive
  -> HistoryEvent a
  -> Murder a
makeMurder m h = let (v, p1, mbP2) = deconstructEvent h in Murder m h p1 v mbP2

deconstructEvent ::
  HistoryEvent a
  -> (a, a, Maybe a) -- victim, murderer, potential involved party
deconstructEvent = \case
  WrongedSomeone v p1 -> (v, p1, Nothing)
  WrongedAFriend v p1 p2 -> (v, p2, Just p1)
  EnviedBy v p1 -> (v, p1, Nothing)
  LoveTriangle v p1 p2 -> (v, p2, Just p1)
  SpurnedLoveFrom v p1 -> (v, p1, Nothing)
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
  MonadIO m
  => Eq a
  => V.Vector a -- ^ and a selection of people
  -> a -- ^ and a specific victim
  -> Motive -- ^ for a given motive
  -> m (HistoryEvent a) -- create the key event that leads to this murder
justifyMotive people victim motive = do
  let pickOne = randomVectorElement
  p1 <- pickOne people
  p2 <- whileM (\p -> (p == p1) || (p == victim) ) $ pickOne people
  when (p1 == p2) (error "murderer and victim were the same person")
  case motive of
    Revenge -> randomVectorElement $ fromList [WrongedSomeone victim p1, WrongedAFriend victim p1 p2]
    Jealousy -> randomVectorElement $ fromList  [EnviedBy victim p1]
    JealousLover -> randomVectorElement $ fromList  [LoveTriangle victim p1 p2]
    SpurnedLove -> randomVectorElement $ fromList  [SpurnedLoveFrom victim p1]
    Inheritance -> randomVectorElement $ fromList  [PassedOver victim p1]
    Stealing -> randomVectorElement $ fromList  [IsPoorDesperate victim p1]
