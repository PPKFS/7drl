{-# LANGUAGE RecordWildCards #-}
module Roguelike.Murder.NPC where

import Yaifl.Prelude
import Data.Vector qualified as V
import Yaifl.Std.Kinds.Person
import System.Random.Stateful (UniformRange(uniformRM), globalStdGen, uniformEnumM)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (groupBy)
import qualified Data.List as L
import Roguelike.Murder.Personality
import Roguelike.Murder.History

-- the first index we use for the sprite pack
npcSpriteIndex :: Int
npcSpriteIndex = 0xE000 + (57*17) --0xE3C9

npcSpriteTextIndex :: Text
npcSpriteTextIndex = "0xE3C9"

liftToNPCSheet ::
  Int
  -> Char
liftToNPCSheet = toEnum @Char . (npcSpriteIndex +)

data Corpora = Corpora
  { maleNames :: V.Vector Text
  , femaleNames :: V.Vector Text
  , nbNames :: V.Vector Text
  , surnames :: V.Vector Text
  , professions :: V.Vector Text
  , images :: M.Map Gender (V.Vector Char)
  , personalities :: V.Vector Text
  } deriving stock (Eq, Ord, Show, Generic)

data BasicPerson = BasicPerson
  { personId :: Int
  , gender :: Gender
  , firstName :: Text
  , surname :: Text
  , profession :: Text
  , profilePicture :: Char
  , personality :: Personality
  } deriving stock (Eq, Ord, Show, Generic)

instance Display BasicPerson where
  displayBuilder BasicPerson{..} = TLB.fromText $ T.intercalate " " [firstName, surname, "("<>profession<>", "<>show gender<>")"]
genderRatios :: V.Vector Gender
genderRatios = V.fromList [Male, Male, Male, Male, Female, Female, Female, Female, NonBinary, NonBinary]



dataPath :: FilePath
dataPath = "data"

categoriseGenderedLoregretPortraits :: M.Map Gender (V.Vector Char)
categoriseGenderedLoregretPortraits =
  let numberedItems = (zip [0..]) $
        [ mn, mn, x, m, fn, x, n, x, x, n, x, m, m, m, a, mn, a, n, mn, mn
        , a, a, mn, m, m, m, x, x, x, x, m, a, a, m, m, m, a, x, a, m
        , a, x, m, m, mn, a, fn, m, a, m, x, x, x, x, x, a, m, fn, x, x
        , m, m, mn, x, fn, n, f, m, x, m, m, m, mn, m, a, x, m, m, mn, fn
        , a, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x
        , m, m, a, m, a, mn, fn, fn, f, f, m, x, m, x, m, m, m, fn, m, n
        , a, f, x, fn, f, x, a, a, a, a, m, m, m, m, x, m, mn, a, m, x
        , fn, m, x, fn, fn, x, fn, x, x, mn, x, m, a, x, m, m, fn, fn, m, m
        , m, m, m, m, x, x, x, x, x, x, x, x, f, fn, fn, fn, a, a, a, a
        , a, a, m, a, m, f, fn, fn, m, x, x, x, x, x, x, x, x, fn, x, x ]
      inds g = (g,) . V.fromList . map (liftToNPCSheet . fst) . filter (\(_, gs) -> g `elem` gs) $ numberedItems
  in M.fromList $ [inds Male, inds Female, inds NonBinary]

  where
    x = []
    m =  [Male]
    f =  [Female]
    n =  [NonBinary]
    mn = [Male, NonBinary]
    fn = [Female, NonBinary]
    a =  [Male, Female, NonBinary]

loadDatasets ::
  MonadIO m
  => m Corpora
loadDatasets = do
  let readLines t = lines <$> readFileText (dataPath <> "/" <> t)
  mn <- readLines "male_names.txt"
  nn <- readLines "nb_names.txt"
  fn <- readLines "female_names.txt"
  sn <- readLines "surnames.txt"
  j <- readLines "jobs.txt"
  return Corpora
    { maleNames = V.fromList mn
    , femaleNames = V.fromList fn
    , nbNames = V.fromList nn
    , surnames = V.fromList sn
    , professions = V.fromList j
    , images = categoriseGenderedLoregretPortraits
    , personalities = V.fromList []
    }

generatePerson ::
  MonadIO m
  => Corpora
  -> Int
  -> m BasicPerson
generatePerson corpora i = do
  gender <- randomVectorElement genderRatios
  firstName <- randomVectorElement $ case gender of
    Male -> maleNames corpora
    Female -> femaleNames corpora
    NonBinary -> nbNames corpora
    Other _ -> nbNames corpora
  surname <- randomVectorElement $ surnames corpora
  profession <- randomVectorElement $ professions corpora
  profilePicture <- randomVectorElement $ fromMaybe (error "impossible") $ M.lookup gender $ images corpora
  personality <- generatePersonality
  return $ BasicPerson { personId = i, gender, firstName, surname, profession, profilePicture, personality}

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

generateAllPeople ::
    MonadIO m
  => Corpora
  -> Int
  -> m (V.Vector BasicPerson)
generateAllPeople corpora num = do
  people <- forM [0..num] (generatePerson corpora)
  let relationships = pairs people
  initRels <- mconcat <$> forM relationships (\(p1, p2) -> makeRelationship (personId p1, trusting . personality $ p1) (personId p2, trusting . personality $ p2))
  let splitRels = M.fromList $ map (\ls -> (fst $ L.head ls, map snd ls)) $ groupBy (\(a, _) (b, _) -> a == b) initRels
  let peopleWithRels = map (\person -> let relevantRels = fromMaybe (error "impossible") $ M.lookup (personId person) splitRels
        in person & #personality % #relationships .~ M.fromList relevantRels) people
  return $ V.fromList peopleWithRels

murderSomeone ::
  MonadIO m
  => V.Vector BasicPerson
  -> m (Murder Int, V.Vector BasicPerson)
murderSomeone people = do
  victim <- randomVectorElement people
  motive <- uniformEnumM @Motive globalStdGen
  event <- justifyMotive people victim motive
  return (fmap personId $ makeMurder motive event, people)
