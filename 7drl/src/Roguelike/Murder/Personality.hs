module Roguelike.Murder.Personality where

import Yaifl.Prelude
import qualified Data.Map as M
import System.Random.Stateful
import Data.Ord (clamp)

data Loyalty = Distant | Normal | Loyal | FiercelyLoyal
  deriving stock (Eq, Ord, Show, Generic, Bounded, Enum)

data Opinion =
  Loathes -- -3
  | Hates -- -2
  | Dislikes -- 1
  | Neutral -- 0
  | Likes -- 1
  | GoodFriend -- 2
  | Loves -- 3
  deriving stock (Eq, Ord, Show, Generic, Bounded, Enum)

data Personality = Personality
  { loyalty :: Loyalty
  , trusting :: Double
  , relationships :: M.Map Int Opinion
  } deriving stock (Eq, Ord, Show, Generic)

loyaltyThreshold :: (Int, Int, Int)
loyaltyThreshold = (6, 8, 9)

weightByThree :: MonadIO m => m Loyalty
weightByThree = (\case
  x
    | x <= 1 -> Distant
  x
    | x <= (view _1 loyaltyThreshold) -> Normal
  x
    | x <= (view _2 loyaltyThreshold) -> Loyal
  _ -> FiercelyLoyal) <$> uniformRM (1, 10) globalStdGen

-- the int is expected to be [-3, 3] so adding 3 makes it [0, 6]
opinionFromInt :: Int -> Opinion
opinionFromInt = toEnum . clamp (0, 6) . (+ 3)

generatePersonality :: MonadIO m => m Personality
generatePersonality = do
  loyalty <- weightByThree
  trusting <- uniformRM (0.0, 1.0) globalStdGen >>= \x1 -> uniformRM (0.0, 1.0) globalStdGen >>= \x2 -> pure (x1+x2/2.0)
  return $ Personality loyalty trusting (M.empty)

makeRelationship :: (MonadIO m, UniformRange a1, Fractional a1, Ord a1) => (a2, a1) -> (a2, a1) -> m [(a2, (a2, Opinion))]
makeRelationship (p1Id, trust)  (p2Id, trust2) = do
    -- random number between -3 and 3 for their relationship
    rel <- uniformRM @Int (-3, 3) globalStdGen
    -- number between -0.5 and 0.5, then add paranoia
    -- this makes it between -1.5 and 1.5
    -- less than -1 or greater than 1 = adjust by 1
    p1Paranoia <- (+ trust) <$> uniformRM (-0.5, 1.5) globalStdGen
    p2Paranoia <- (+ trust2) <$> uniformRM (0.0, 2.0) globalStdGen
    let calcParanoia p = if p > 0.8 then rel + 1 else if p < (-0.8) then rel - 1 else rel
    return [(p1Id, (p2Id, opinionFromInt $ calcParanoia p1Paranoia)), (p2Id, (p1Id, opinionFromInt $  calcParanoia p2Paranoia))]