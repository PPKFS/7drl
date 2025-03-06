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
import Data.List (partition, nub, groupBy)
import Rogue.Colour (Colour(..))
import Yaifl.Std.Kinds.Direction (Direction (..), HasOpposite (..))
import qualified Data.Map as M
import Rogue.Geometry.Line
import qualified Data.List as L
import Yaifl.Std.Create (addRoom', addRoom, done, AddObjects)
import Yaifl (HasStandardProperties)

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
  , openThresholds :: [Rectangle]
  , doors :: [V2]
  , peopleLocations :: [V2]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

type RoomId = Int
data RoomArea = RoomArea
  { roomId :: RoomId
  , rectangle :: Rectangle
  , level :: Int
  , roomType :: RoomType
  , isIndoor :: Bool
  , thresholds :: [([V2], RoomId, Direction)]
  , doors :: [(V2, RoomId, Direction)]
  , connections :: [RoomId]
  } deriving stock (Eq, Ord, Show, Generic)

data FloorPlan = FloorPlan
  { rooms :: [RoomArea]
  , openThresholds :: [(V2, V2, RoomId, RoomId, Direction)]
  , doors :: [(V2, RoomId, RoomId, Direction)]
  } deriving stock (Eq, Ord, Show, Generic)

data FullPart = FullPart
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance AsLayer FullPart where
  toLayer = const 250

scaleUpTiles :: Num a => a
scaleUpTiles = 3

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
  -> RoomId
  -> RawRoomArea
  -> RoomArea
rawRoomToRoom rt i raw = RoomArea
  { roomId = i
  , rectangle = view #rectangle raw
  , level = view #level raw
  , roomType = fromMaybe rt (fixedType raw)
  , isIndoor = view #isIndoor raw
  , thresholds = []
  , doors = []
  , connections = []
  }

makeFloorPlan ::
  IOE :> es
  => Eff es FloorPlan
makeFloorPlan = do
  fp <- loadFp
  seededRoomList <- take emptyDownstairs . (requiredGroundFloorPool <>) <$> (liftIO $ shuffleM (seededGroundFloorRoomPool))
  sizedRoomList <- sortOn fst <$> mapM sizeUpRoom seededRoomList
  let -- this is all about sorting and assigning rooms to their specific room type and id
      partRooms = partition (isJust . fixedType . snd) $ zip [0..] (view #rooms fp)
      p1 = (zip (map snd sizedRoomList)) . sortOn (area . view #rectangle . snd) $ (snd partRooms)
      zippedRooms = (map (\(x, (xId, y)) -> rawRoomToRoom x xId y) ) p1
      fixedRooms = (map (\(xId, r) -> rawRoomToRoom (error "fixed room had no room type") xId r)) (fst partRooms)
      allRooms = fixedRooms <> zippedRooms

      allRects = map (\r -> (roomId r, (view #rectangle r))) allRooms
      -- for each threshold (a line between two rooms), find whichever overlapping wall it is cutting out and find those two adjoining rooms
      assignThresholds = map (\x -> let transX = translate (V2 25 50) x in case filter (\(_, rl) ->
        let (V2 _ y3) = topLeft transX
            isHoriz = y3 == view _2 (bottomRight transX) in
        liesOnAWallOfRectangle rl isHoriz (topLeft transX)
        ) allRects
        of
          [x1, x2] -> (topLeft transX, bottomRight transX, fst x1, fst x2,

          -- same y = horizontal = north or south
            if view _2 (bottomRight transX) == view _2 (topLeft transX)
            -- if it matches the y of the top of the first room then it's north
            then if view _2 (topLeft transX) == view _2 (topLeft $ snd x1) then North else South
            else if view _1 (topLeft transX) == view _1 (topLeft $ snd x1) then West else East)
          xr -> error $ "unexpected threshold" <> show transX <> show xr) (view #openThresholds fp)
      assignDoors = map (\x -> let transX = x + (V2 25 50) in case filter (\(_, rl) ->
        liesOnAWallOfRectangle rl True (transX) || liesOnAWallOfRectangle rl False (transX)
        ) allRects
        of
          [x1, x2] -> (transX, fst x1, fst x2, if
            | view _1 transX == (view _1 (topLeft (snd x1))) -> West
            | view _1 transX == (view _1 (bottomRight (snd x1)) - 1) -> East
            | view _2 transX == (view _2 (topLeft (snd x1))) -> North
            | view _2 transX == (view _2 (bottomRight (snd x1)) - 1) -> South
            | otherwise -> error (show transX <> show (snd x1) <> " had no match on door location")
            )
          xr -> error $ "unexpected door" <> show x <> show xr) (view #doors fp)
  print assignDoors
  let addThresh = foldl' (\rAcc (t1, t2, r1, r2, d) -> rAcc & ix r1 % #thresholds %~ (amendThr r2 t1 t2 d) & ix r2 % #thresholds %~ (amendThr r1 t1 t2 (opposite d))) (sortOn roomId allRooms) assignThresholds
      addDoors = foldl' (\rAcc (dl, r1, r2, d) -> rAcc & ix r1 % #doors %~ ((dl, r2, d):) & ix r2 % #doors %~ ((dl, r1, (opposite d)):)) addThresh assignDoors
      addConns = map (\r -> r & #connections .~ (sortNub $ (map (view _2) $ view #thresholds r) <> map (view _2) (view #doors r))) addDoors
  return $ FloorPlan
    { doors = assignDoors
    , rooms = addConns
    , openThresholds = assignThresholds
    }
  where
    liesOnAWallOfRectangle rl isHoriz p =
      let (V2 x1 y1) = topLeft rl
          (V2 x2 y2) = bottomRight rl
          (V2 x3 y3) = p
      in if isHoriz then ((y3 == (y2 - 1)) || (y3 == y1)) && x3 <= (x2 - 1) && x3 >= x1 else ((x3 == x1) || ((x2 - 1) == x3)) && y3 <= (y2 - 1) && y3 >= y1

    amendThr r (V2 x1 y1) (V2 x2 y2) d = (([V2 x y | x <- [x1 .. x2], y <- [y1 .. y2]], r, d) :)
    loadFp = translateFloorPlan (V2 25 50) . either (\e -> error $ "could not load floorplan " <> show e) id <$> liftIO (eitherDecodeFileStrict' @RawFloorPlan "data/rooms.json")
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
      roomsById = M.fromList (map (\x -> (view #roomId x,x)) zSorted)
      roomLookup k = fromMaybe (error "impossible") $ M.lookup k roomsById
  terminalComposition CompositionOn
  forM_ (zSorted) $ \r -> do
    terminalLayer 1
    forM_ (rectanglePoints Horizontal $ view #rectangle r) $ \t -> viewportDrawTile (scaleUpTiles*t) Nothing 0xFFFFFFFF (sheetCoordToChar (V2 8 4))
    forM_ (rectangleEdges $ view #rectangle r) $ \t -> viewportDrawTile (scaleUpTiles*t) Nothing 0xFFFFFFFF (sheetCoordToChar (V2 8 4))
    renderBorderTileset (mconcat $ map (view _1) $ thresholds r) (view #rectangle r) (BorderTileSet
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
    viewportPrint (scaleUpTiles*((view (#rectangle % #topLeft) r) + (V2 1 1))) Nothing (Colour 0xFF000000) $ (show $ view #roomType r)
    let lis = mconcat $ map (\i -> tranThong (centre (view #rectangle r) - V2 1 1) id i)
          (map (view _1) (view #doors r) <> (map (\x -> (view _1 x) L.!! (length (view _1 x) `div` 2)) $ (thresholds r)))
    forM_ lis $ \l -> viewportDrawTile (scaleUpTiles*l) Nothing 0xFFFFFFFF (sheetCoordToChar (V2 32 9))
    pass
  forM_ (view #doors fp) (\(l, r1, r2, d) -> viewportDrawTile (scaleUpTiles*l) Nothing 0xFFFFFFFF (sheetCoordToChar (V2 32 0)))

renderBorderTileset ::
  IOE :> es
  => AsLayer a
  => Reader (Viewport a) :> es
  => [V2]
  -> Rectangle
  -> BorderTileSet
  -> Eff es ()
renderBorderTileset thr rect BorderTileSet{..} =  do
  let f v a = unless (v `elem` thr) $ viewportDrawTile (scaleUpTiles*v) Nothing (0xFFFFFFFF) a
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

floorPlanToYaiflWorld ::
  HasStandardProperties wm
  => AddObjects wm es
  => FloorPlan
  -> Eff es ()
floorPlanToYaiflWorld fp = do
  -- we want to make each room, with a name
  roomIds <- (zip (map roomId (view #rooms fp))) <$> forM (view #rooms fp) (\room -> do
    addRoom (show (roomType room)) ! #description ("It's a room.") ! done)

  -- then we want to go back over and add connections and doors
  forM_ (view #openThresholds fp) (const pass)
  --  and doors
  forM_ (view #doors fp) (const pass)

