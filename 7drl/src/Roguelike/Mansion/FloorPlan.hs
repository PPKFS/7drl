{-# LANGUAGE RecordWildCards #-}
module Roguelike.Mansion.FloorPlan where

import Yaifl.Prelude hiding (Reader)
import Rogue.Geometry.Rectangle
import Data.Aeson
import Rogue.Geometry.V2
import BearLibTerminal
import Rogue.Rendering.Viewport
import Rogue.Colour
import Effectful.Reader.Static


data RoomType = Conservatory | Stairway | Foyer | Garage | ServantCorridor | Corridor
    deriving stock (Eq, Read, Ord, Show, Generic)

instance FromJSON RoomType where
  parseJSON = withText "roomtype" $ \s ->
    case readMaybe (toString s) of
      Nothing -> fail $ "could not parse roomtype " <> show s
      Just r -> pure r

data RoomArea = RoomArea
  { rectangle :: Rectangle
  , level :: Int
  , fixedType :: Maybe RoomType
  , isIndoor :: Bool
  } deriving stock (Eq, Ord, Show, Generic)

instance FromJSON RoomArea

data FloorPlan = FloorPlan
  { rooms :: [RoomArea]
  , doors :: [()]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)


data FullPart = FullPart
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance AsLayer FullPart where
  toLayer = const 250

translateFloorPlan ::
  V2
  -> FloorPlan
  -> FloorPlan
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

drawDebugFloorPlan ::
  IOE :> es
  => Viewport FullPart
  -> Eff es ()
drawDebugFloorPlan vp = renderViewport vp $ do
  terminalClear
  fp <- translateFloorPlan (V2 25 50) . either (\e -> error $ "could not load floorplan " <> show e) id <$> liftIO (eitherDecodeFileStrict' @FloorPlan "7drl/rooms.json")
  let zSorted = sortOn (\r -> (r ^. #rectangle % #topLeft % _2)) (fp ^. #rooms)
  terminalComposition CompositionOn
  forM_ (zSorted) $ \r -> do
    terminalLayer 1
    forM_ (rectanglePoints Horizontal $ rectangle r) $ \t -> viewportDrawTile t Nothing 0xFFFFFFFF (sheetCoordToChar (V2 8 4))
    forM_ (rectangleEdges $ rectangle r) $ \t -> viewportDrawTile t Nothing 0xFFFFFFFF (sheetCoordToChar (V2 8 4))
    let c = liftToSheet 0
    renderBorderTileset (rectangle r) (BorderTileSet
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

renderBorderTileset ::
  IOE :> es
  => AsLayer a
  => Reader (Viewport a) :> es
  => Rectangle
  -> BorderTileSet
  -> Eff es ()
renderBorderTileset rect BorderTileSet{..} =  do
  let f v = viewportDrawTile v Nothing (0xFFFFFFFF)
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