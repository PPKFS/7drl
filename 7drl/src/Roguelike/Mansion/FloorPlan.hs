{-# LANGUAGE RecordWildCards #-}
module Roguelike.Mansion.FloorPlan where

import Yaifl.Prelude hiding (Reader)
import Rogue.Geometry.Rectangle
import Data.Aeson
import Rogue.Geometry.V2
import BearLibTerminal
import Rogue.Rendering.Viewport
import Effectful.Reader.Static
import System.Random.Shuffle
import System.Random.Stateful (UniformRange(uniformRM), globalStdGen)
import Data.List (partition)
import Rogue.Colour (Colour(..))
import Roguelike.Murder.NPC

data RoomType =
  Conservatory
  | Stairway
  | Foyer
  | Garage
  | ServantCorridor
  | Corridor
  | GreatHall
  | DiningRoom
  | Library
  | Kitchen
  | Pantry
  | Bathroom
  | CloakRoom
  | Gallery
  | BallRoom
  | PianoRoom
  | BilliardRoom
  | DrawingRoom
  | Study
  | TrophyRoom
  | Armory
  | Larder
  | WineCellar
  | Parlor
  | Theatre
  | Chapel
  | SmokingRoom
  | TelegraphRoom
    deriving stock (Eq, Read, Ord, Show, Generic)

roomSizeBounds :: RoomType -> (Double, Double)
roomSizeBounds = \case
  -- these ones are always fixed
  Conservatory -> (1, 1)
  Stairway -> (1, 1)
  Foyer -> (1, 1)
  Garage -> (1, 1)
  ServantCorridor -> (1, 1)
  Corridor -> (1, 1)
  GreatHall -> (5, 10)
  DiningRoom -> (4, 5)
  Library -> (1, 5)
  Kitchen -> (3, 6)
  Pantry -> (0.5, 2)
  Bathroom -> (0.2, 0.5)
  CloakRoom -> (0.5, 1)
  Gallery -> (3, 5)
  BallRoom -> (4, 8)
  PianoRoom -> (2, 5)
  BilliardRoom -> (3, 6)
  DrawingRoom -> (1.5, 5)
  Study -> (2, 6)
  TrophyRoom -> (1, 4)
  Armory -> (2, 8)
  Larder -> (0.5, 1)
  WineCellar -> (1, 2)
  Parlor -> (4, 8)
  Theatre -> (7, 10)
  Chapel -> (1, 6)
  SmokingRoom -> (1, 3)
  TelegraphRoom -> (1, 3)

requiredGroundFloorPool :: [RoomType]
requiredGroundFloorPool =
  [ GreatHall
  , DiningRoom
  , Library
  , Kitchen
  , Pantry
  , Bathroom
  ]

emptyDownstairs :: Int
emptyDownstairs = 16

-- there are currently 23 downstairs rooms, of which 7 are fixed, and some more are required.
seededGroundFloorRoomPool :: [RoomType]
seededGroundFloorRoomPool =
  [ CloakRoom
  , Gallery
  , BallRoom
  , PianoRoom
  , BilliardRoom
  , DrawingRoom
  , Study
  , TrophyRoom
  , Armory
  , Larder
  , WineCellar
  , DiningRoom
  , Parlor
  , Bathroom
  , Theatre
  , Chapel
  , SmokingRoom
  , TelegraphRoom
  ]

instance FromJSON RoomType where
  parseJSON = withText "roomtype" $ \s ->
    case readMaybe (toString s) of
      Nothing -> fail $ "could not parse roomtype " <> show s
      Just r -> pure r

data RawRoomArea = RawRoomArea
  { rectangle :: Rectangle
  , level :: Int
  , fixedType :: Maybe RoomType
  , isIndoor :: Bool
  } deriving stock (Eq, Ord, Show, Generic)

instance FromJSON RawRoomArea

data RawFloorPlan = RawFloorPlan
  { rooms :: [RawRoomArea]
  , doors :: [()]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data RoomArea = RoomArea
  { rectangle :: Rectangle
  , level :: Int
  , roomType :: RoomType
  , isIndoor :: Bool
  } deriving stock (Eq, Ord, Show, Generic)


data FloorPlan = FloorPlan
  { rooms :: [RoomArea]
  , doors :: [()]
  } deriving stock (Eq, Ord, Show, Generic)

data FullPart = FullPart
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance AsLayer FullPart where
  toLayer = const 250

translateFloorPlan ::
  V2
  -> RawFloorPlan
  -> RawFloorPlan
translateFloorPlan offset = #rooms %~ (map (\x -> x & (#rectangle %~ translate offset) & (#rectangle % #bottomRight %~ (+ (V2 1 1)) )))

liftToSheet ::
  Int
  -> Char
liftToSheet = toEnum @Char . (0xE000 +)

--indor sheet: 27
--outdoor: 57
sheetCoordToChar ::
  V2
  -> Char
sheetCoordToChar (V2 x y) = liftToSheet ((y * 57) + x)

rawRoomToRoom ::
  RoomType
  -> RawRoomArea
  -> RoomArea
rawRoomToRoom rt raw = RoomArea
  { rectangle = view #rectangle raw
  , level = view #level raw
  , roomType = fromMaybe rt (fixedType raw)
  , isIndoor = view #isIndoor raw
  }

makeFloorPlan ::
  IOE :> es
  => Eff es FloorPlan
makeFloorPlan = do
  fp <- loadFp
  seededRoomList <- take emptyDownstairs . (requiredGroundFloorPool <>) <$> (liftIO $ shuffleM (seededGroundFloorRoomPool))
  sizedRoomList <- sortOn fst <$> mapM sizeUpRoom seededRoomList
  let (fixedRooms, sortedRooms) =
        bimap (map (rawRoomToRoom (error "fixed room had no room type"))) (map (\(x, y) -> rawRoomToRoom x y))
        . second (zip (map snd sizedRoomList)
        . sortOn (area . view #rectangle))
        . partition (isJust . fixedType) $ (view #rooms fp)
  return $ FloorPlan
    { doors = []
    , rooms = fixedRooms <> sortedRooms
    }
  where
    loadFp = translateFloorPlan (V2 25 50) . either (\e -> error $ "could not load floorplan " <> show e) id <$> liftIO (eitherDecodeFileStrict' @RawFloorPlan "7drl/data/rooms.json")
    sizeUpRoom r = do
      s <- uniformRM (roomSizeBounds r) globalStdGen
      return (s, r)

drawDebugFloorPlan ::
  IOE :> es
  => Viewport FullPart
  -> Eff es ()
drawDebugFloorPlan vp = renderViewport vp $ do
  terminalClear
  fp <- makeFloorPlan
  let zSorted = sortOn (\r -> (r ^. #rectangle % #topLeft % _2)) (fp ^. #rooms)
  terminalComposition CompositionOn
  forM_ (zSorted) $ \r -> do
    terminalLayer 1
    forM_ (rectanglePoints Horizontal $ view #rectangle r) $ \t -> viewportDrawTile (2*t) Nothing 0xFFFFFFFF (sheetCoordToChar (V2 8 4))
    forM_ (rectangleEdges $ view #rectangle r) $ \t -> viewportDrawTile (2*t) Nothing 0xFFFFFFFF (sheetCoordToChar (V2 8 4))
    let c = liftToSheet 0
    renderBorderTileset (view #rectangle r) (BorderTileSet
      { -- tl = sheetCoordToChar (V2 4 13)
      -- , tr = sheetCoordToChar (V2 0 13)
        tl = sheetCoordToChar (V2 37 12)
      , tr = sheetCoordToChar (V2 37 12)
      , bl = sheetCoordToChar (V2 37 12)
      , br = sheetCoordToChar (V2 37 12)
      , b = sheetCoordToChar (V2 35 12)
      , l = sheetCoordToChar (V2 36 13)
      , r = sheetCoordToChar (V2 36 13)
      , t = sheetCoordToChar (V2 35 12)
      })
    terminalLayer 5
    viewportPrint (2*((view (#rectangle % #topLeft) r) + (V2 1 1))) Nothing (Colour 0xFF000000) $ (show $ view #roomType r)

renderBorderTileset ::
  IOE :> es
  => AsLayer a
  => Reader (Viewport a) :> es
  => Rectangle
  -> BorderTileSet
  -> Eff es ()
renderBorderTileset rect BorderTileSet{..} =  do
  let f v = viewportDrawTile (2*v) Nothing (0xFFFFFFFF)
  f (topLeft rect) tl
  f (bottomRight rect - V2 1 1) br
  f (topRight rect - V2 1 0) tr
  f (bottomLeft rect - V2 0 1) bl
  forM_ [view _1 (topLeft rect) + 1 .. view _1 (topRight rect) - 2 ] $ \x -> do
    f (V2 x (rect ^. #topLeft % _2)) t
    f (V2 x (rect ^. #bottomRight % _2 - 1)) b
  forM_ [view _2 (topLeft rect) + 1 .. view _2 (bottomLeft rect) - 2 ] $ \y -> do
    f (V2 (rect ^. #topLeft % _1) y) l
    f (V2 (rect ^. #bottomRight % _1 - 1) y) r