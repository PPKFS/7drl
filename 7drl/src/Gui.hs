{-# LANGUAGE RecordWildCards #-}
module Gui where

import BearLibTerminal
import Breadcrumbs
import Effectful.Dispatch.Dynamic
import Effectful.State.Dynamic
import Rogue.Colour
import Rogue.Events
import Rogue.Geometry.Rectangle
import Rogue.Geometry.V2
import Rogue.Rendering.Viewport
import Yaifl
import Yaifl.Core.Effects
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Core.Rules.Run
import Yaifl.Prelude hiding (Reader)
import Yaifl.Std.Create
import Yaifl.Std.EffectHandlers
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl.Text.Print
import Yaifl.Text.ResponseCollection
import qualified Data.Text as T
import MessageLog
import Data.Char (isPrint)
import Yaifl.Std.Actions.Collection
import Yaifl.Std.Kinds.Person
import Yaifl.Core.Query.Enclosing (getLocation)
import Rooms
import Yaifl.Core.Kinds.Object
import Rogue.Array2D.Boxed
import Yaifl.Std.Actions.Imports
import Roguelike.Mansion.FloorPlan
import Roguelike.Murder.NPC
import Effectful.Reader.Static (Reader)
import Roguelike.Murder.Personality
import Roguelike.Murder.History
import qualified Data.Vector as V


screenSize :: V2
screenSize = 2 * V2 80 80

topViewportRectangle :: Int -> Rectangle
topViewportRectangle topViewportSize = Rectangle (V2 0 0) (screenSize-V2 20 (view #y screenSize - topViewportSize))

bottomViewportRectangle :: Int -> Rectangle
bottomViewportRectangle topViewportSize = rectangleFromDimensions
  (V2 0 (bottomEdge $ topViewportRectangle topViewportSize))
  (V2 (view _1 screenSize - 20) (view #y screenSize - 4))

sideViewportRectangle :: Int -> Rectangle
sideViewportRectangle topViewportSize = rectangleFromDimensions
  (V2 (view _1 (bottomRight $ topViewportRectangle topViewportSize)) 0)
  (V2 20 (view _2 screenSize))

mapViewport :: Int -> Viewport MapPart
mapViewport topViewportSize = Viewport (topViewportRectangle topViewportSize) (Just (Colour 0xFF333333)) Nothing
bottomViewport :: Int -> Viewport MainPart
bottomViewport topViewportSize = Viewport (bottomViewportRectangle topViewportSize) (Just (Colour 0xFFFCF5E5)) (Just (unicodeBorders, Colour 0xFFDDDDDD))
sideViewport :: Int -> Viewport SidePart
sideViewport topViewportSize = Viewport (sideViewportRectangle topViewportSize) (Just (Colour 0xFF22AAFF)) (Just (unicodeBorders, Colour 0xFFFFFFFF))

debugFullViewport :: Viewport FullPart
debugFullViewport = Viewport (rectangleFromDimensions (V2 0 0) screenSize) (Just (Colour 0xFFAAAAAA)) (Nothing)

dialogueMenuVP :: Viewport FullPart
dialogueMenuVP = Viewport (rectangleFromDimensions (V2 20 0) (V2 80 120)) (Just (Colour 0xFF577dba)) (Nothing)

data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  }

defaultOptions :: (WMActivities wm ~ ActivityCollection wm, WMResponses wm ~ ResponseCollection wm) => ConstructionOptions wm
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector

data MainPart = MainPart
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance AsLayer MainPart where
  toLayer = const 1

data SidePart = SidePart
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance AsLayer SidePart where
  toLayer = const 2

data MapPart = MapPart
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance AsLayer MapPart where
  toLayer = const 3
data GuiState = GuiState
  { messageLog :: MessageLog MainPart
  , pendingQuit :: Bool
  , topViewportSize :: Int
  } deriving stock (Show, Generic)

modifyMessageLog ::
  State GuiState :> es
  => (MessageLog MainPart -> MessageLog MainPart)
  -> Eff es ()
modifyMessageLog f = #messageLog %= f


runInputFromGUI ::
  IOE :> es
  => State GuiState :> es
  => Eff (Input : es) a
  -> Eff es a
runInputFromGUI = interpret $ \_ -> \case
  WaitForInput -> let
    go = do
      do
        terminalLayer 5
        s <- terminalReadStr 30 30 50
        when (isNothing s) $ #pendingQuit .= True
        if (s == Just "" || (not $ T.all isPrint <$?> s)) then return Nothing else return s
    in do
      s <- use @GuiState #topViewportSize
      withViewport (bottomViewport s) $ go

beginPlay ::
  YaiflEffects SpatialWorldModel es
  => State GuiState :> es
  => WorldActions SpatialWorldModel
  -> Eff es ()
beginPlay wa = do
  failHorriblyIfMissing (runRulebook Nothing False (wa ^. #whenPlayBegins) ())
  s <- use @GuiState #topViewportSize
  runOnce s
  pass

initialiseTerminal :: IO ()
initialiseTerminal = do
  let
      size :: Either (Int, Int) Int
      size = {- Left (7, 16)-- -} Right 12
      scaleSize :: Int -> Either (Int, Int) Int
      scaleSize s = bimap (bimap (s*) (s*)) (s*) size
      sizeStr s = either (\(x, y) -> show x <> "x" <> show y) (\v -> show v <> "x" <> show v) s
      fnt = "square.ttf"
      -- _fnt = "Iosevka-Term-02.ttf"
  terminalSetText "log: file='awa.log', level=trace;"
  terminalSetText $ "font: '" <> fnt <> "', codepage=437, size=" <> sizeStr size
  terminalSetText $ "0xE000: roguelikeSheet_transparent2.png, size=16x16, align='top-left', resize=" <> sizeStr (scaleSize scaleUpTiles)
  terminalSetText $ npcSpriteTextIndex <> ": portraits_border.png, align='top-left', size=80x80, resize=" <> sizeStr (scaleSize scaleUpPfp)
  -- terminalSetText "bold font: 'Iosevka-Term-Bold-02.ttf', codepage=437, size=16"
  pass

makeWorld ::
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => ConstructionOptions wm
  -> Text
  -> Game wm b
  -> IO (World wm)
makeWorld conOptions fullTitle initWorld = do
  let emptyWorld = blankWorld (activityCollectionBuilder conOptions) (responseCollectionBuilder conOptions)
  snd <$$> runGame runPrintPure runInputAsBuffer emptyWorld blankActionCollection $ do
    withSpan' "worldbuilding" fullTitle $ do
      newWorld
      initWorld
      -- this just moves the actions from the indexed, static, standard library collection
      -- into the dynamic collection
      -- we do it here because we need to copy over changes to actions and we can't modify WrappedActions directly
      addStandardActions

runOnce ::
  YaiflEffects SpatialWorldModel es
  => State GuiState :> es
  => Int
  -> Eff es Bool
runOnce topViewportSize = do
  renderAll topViewportSize
  terminalRefresh
  runTurn
  fmap (any id) $ handleEvents NotBlocking $ \case
    Keypress TkEsc -> return True
    WindowEvent Resize -> pass >> return False
    WindowEvent WindowClose -> return True
    x -> print x >> return False

runLoop ::
  YaiflEffects SpatialWorldModel es
  => State GuiState :> es
  => Eff es ()
runLoop = do
  s <- use @GuiState #topViewportSize
  r <- runOnce s
  pend <- use @GuiState #pendingQuit
  if r || pend then pass else runLoop

getMessageBuffer :: forall wm. Game wm [StyledDoc MessageAnnotation]
getMessageBuffer = gets @(World wm) (view $ #messageBuffer % #buffer)

renderAll ::
  forall es.
  YaiflEffects SpatialWorldModel es
  => State GuiState :> es
  => Int
  -> Eff es ()
renderAll topViewportSize = do
  msgList <- gets @(World SpatialWorldModel) (view $ #messageBuffer % #buffer % reversed)
  modifyBuffer (#buffer .~ [])
  updateMessageLog msgList
  -- let msgLog = for_ msgList
  --renderBottomTerminal
  --renderSideTerminal
  --renderTopTerminal topViewportSize
  drawDebugFloorPlan debugFullViewport
  --drawDebugDialogueBox dialogueMenuVP
  where
    renderSideTerminal :: Eff es ()
    renderSideTerminal = do
      renderViewport (sideViewport topViewportSize) $
        viewportPrint (V2 3 3) Nothing (Colour 0xFF000000) "More info..."

updateMessageLog ::
  IOE :> es
  => State GuiState :> es
  => [StyledDoc MessageAnnotation]
  -> Eff es ()
updateMessageLog msgs = do
  gs <- get
  ml' <- addMessage msgs (messageLog gs)
  #messageLog .= ml'


textSpaces :: Int -> Text
textSpaces n = T.replicate n (T.singleton ' ')

renderBottomTerminal ::
  IOE :> es
  => State GuiState :> es
  => Eff es ()
renderBottomTerminal = do
  gs <- get
  renderMessageLog AnchorTop (messageLog gs)


tile :: V2 -> Lens' (Array2D TileInfo) TileInfo
tile loc = lens (\w -> fromMaybe (error "") $ w ^? ix loc) (\w t -> w & ix loc .~ t)

renderTopTerminal ::
  IOE :> es
  => RuleEffects SpatialWorldModel es
  => NoMissingObjects SpatialWorldModel es
  => State GuiState :> es
  => Int
  -> Eff es ()
renderTopTerminal topViewportSize = do
  renderViewport (mapViewport topViewportSize) $ do
    -- get the current room the player is in and render that
    p' <- getPlayer'
    currRoom <- getLocation p'
    let tilemapData = currRoom ^. #objectData % #roomData % #space
    traverseArrayWithCoord_ tilemapData $ \p td -> whenInViewport (mapViewport topViewportSize) p $ do
      let r = renderable td
      terminalLayer' (toLayer MapPart)
      terminalColour (r ^. #foreground)
      terminalBkColour (r ^. #background)
      void $ withV2 (V2 15 15 + p) terminalPrintText (one $ r ^. #glyph)
    rName <- sayText (currRoom ^. #name)
    withV2 (V2 15 14) $ \x y -> terminalPrintText x y rName
  pass

scaleUpPfp :: Int
scaleUpPfp = 8
vpDrawSprite :: (AsLayer l, Reader (Viewport l) :> es,  IOE :> es) => V2 -> Char -> Eff es ()
vpDrawSprite pos v = viewportDrawTile pos Nothing (0xFFFFFFFF) v

drawDebugDialogueBox ::
  IOE :> es
  => AsLayer l
  => Bounded l
  => Enum l
  => Viewport l
  -> Eff es ()
drawDebugDialogueBox v = renderViewport v $ do
  cor <- loadDatasets
  people <- generateAllPeople cor 20
  (murder, _) <- murderSomeone people Nothing
  redHerrings <- forM [0..5] (const $ murderSomeone people (Just $ people V.! (victim murder) ))
  let (murdId, vicId, mbOtherId) = deconstructEvent (cause murder)
  clearViewport v

  forM_ (zip [0..] (toList people)) $ \(i, p1) -> do
    let yval = (1+((i `mod` 10) * scaleUpPfp) + (i `mod` 10))
    let xval = if i < 10 then 1 else 42
    vpDrawSprite (V2 xval yval) (profilePicture p1)
    let rightOfImage y = V2 (scaleUpPfp + xval) (yval + y)
    let col = if
          | vicId == personId p1 -> 0xFF801121
          | murdId == personId p1 -> 0xFF640d6e
          | mbOtherId == Just (personId p1) -> 0xFF5e5a17
          | otherwise -> 0xFFFFFFFFF
    if (col /= 0xFFFFFFFF) then terminalBkColour 0xFF222222 else terminalBkColour 0xFF577dba
    viewportPrint (rightOfImage 1) Nothing col $ "Name: " <> niceName p1
    viewportPrint (rightOfImage 2) Nothing col $ "Gender: " <> (display $ view #gender p1)
    viewportPrint (rightOfImage 3) Nothing col $ "Profession: " <> (profession p1)
    viewportPrint (rightOfImage 4) Nothing col $ "Personality: " <> (show . loyalty . personality $ p1)
  viewportPrint (V2 1 (4+(11 * scaleUpPfp))) Nothing (0xFFFFFFFF) (prettyMurder murder people)
  forM_ (zip [1..] redHerrings) $ \(i, (x, _)) -> viewportPrint (V2 1 (4+(11 * scaleUpPfp) + (3*i))) Nothing (0xFFFFFFFF) (prettyMurder x people)
  pass

niceName :: BasicPerson -> Text
niceName p1 = (firstName p1) <> " " <> (surname p1)
prettyMurder :: Murder Int -> V.Vector BasicPerson -> Text
prettyMurder murder bp =
  let lookupPerson i = bp V.! i
      nicerMurder = fmap lookupPerson murder
      disp = niceName
      mbOther = involved nicerMurder
      causeBy = case cause nicerMurder of
        WrongedSomeone v p1 -> [disp p1, " was wronged by ", disp v]
        WrongedAFriend v p1 p2 -> [disp p1, ", the friend of ", disp p2, ", was wronged by ", disp v]
        EnviedBy v p1 -> [disp v, " was envied by ", disp p1]
        LoveTriangle v p1 p2 -> [disp p1, " loved ", disp p2, ", but was caught in a love triangle with ", disp v]
        SpurnedLoveFrom v p1 -> [disp p1, " loved ", disp v, " but the love was not returned"]
        PassedOver v p1 -> [disp p1, ", the sibling of ", disp v, " was passed over in the line of inheritance"]
        IsPoorDesperate v p1 -> [disp p1, " was poor and struggling, so aimed to steal from ", disp v]
      motiveBy = case motive nicerMurder of
        Revenge -> "Driven by revenge"
        Jealousy -> "Driven by jealousy"
        JealousLover -> "Driven by the desire to have " <> (fromMaybe (error "impossible") $ firstName <$> mbOther) <> " for themselves"
        SpurnedLove -> "Driven by spite"
        Inheritance -> "Driven by envy"
        Stealing -> "Driven by desperation"
      pronounOf p = case view #gender p of
        Male -> "he"
        Female -> "she"
        _ -> "they"
  in mconcat $ causeBy <> [". ", motiveBy, ", ", pronounOf (murderer nicerMurder), " killed ", (firstName (victim nicerMurder)), "." ]