module Main (main) where

import Data.Char (isSpace)
import Data.Int (Int32)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Graphics.Win32.GDI
import Graphics.Win32.Message
import Graphics.Win32.Misc
import Graphics.Win32.Window
import Graphics.Win32.Window.PostMessage
import System.Win32.DLL (getModuleHandle)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Variant = Classical | LRTM | BBTM deriving (Eq, Show)
data Move = GoLeft | GoRight | Reset deriving (Eq, Show)
data Verdict = Accepted | Rejected deriving (Eq, Show)

data Rule = Rule
  { fromState :: String
  , readSymbol :: Char
  , toState :: String
  , writeSymbol :: Char
  , movement :: Move
  } deriving (Eq, Show)

data Machine = Machine
  { initialState :: String
  , acceptState :: String
  , rejectState :: Maybe String
  , variant :: Variant
  , rules :: [Rule]
  , ruleMap :: Map.Map (String, Char) Rule
  } deriving (Eq, Show)

data Config = Config
  { tape :: [Char]
  , headPos :: Int
  , currentState :: String
  , firedRule :: Maybe Rule
  , stepNumber :: Int
  } deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [descPath, tmInput, "M1"] -> do
      machine <- loadMachine descPath
      putStrLn . renderVerdict . runMachine machine $ tmInput
    [descPath, tmInput, "M2"] -> do
      machine <- loadMachine descPath
      let traceData = runTrace machine tmInput
      runNativeGui descPath tmInput machine traceData
    _ -> do
      hPutStrLn stderr "Usage: coursework <absolute-path-to-desc> <input-string> <M1|M2>"
      exitFailure

loadMachine :: FilePath -> IO Machine
loadMachine path = do
  content <- readFile path
  either die return (parseMachine content)

die :: String -> IO a
die message = hPutStrLn stderr message >> exitFailure

parseMachine :: String -> Either String Machine
parseMachine content = do
  let entries = Map.fromList (map parseLine (filter (not . null) (map trim (lines content))))
  initState <- require "initialState" entries
  accState <- require "acceptState" entries
  varText <- require "variant" entries
  rulesText <- require "rules" entries
  var <- parseVariant varText
  parsedRules <- mapM parseRule (splitOn "<>" rulesText)
  mapM_ (validateRule var) parsedRules
  let rejState = Map.lookup "rejectState" entries
      indexed = Map.fromList [((fromState r, readSymbol r), r) | r <- parsedRules]
  return Machine
    { initialState = initState
    , acceptState = accState
    , rejectState = rejState
    , variant = var
    , rules = parsedRules
    , ruleMap = indexed
    }
  where
    parseLine line =
      let (key, rest) = break (== '=') line
       in (trim key, trim (drop 1 rest))

require :: String -> Map.Map String String -> Either String String
require key entries =
  maybe (Left ("Missing field: " ++ key)) Right (Map.lookup key entries)

parseVariant :: String -> Either String Variant
parseVariant "CLASSICAL" = Right Classical
parseVariant "LRTM" = Right LRTM
parseVariant "BBTM" = Right BBTM
parseVariant other = Left ("Unknown variant: " ++ other)

parseMove :: String -> Either String Move
parseMove "LEFT" = Right GoLeft
parseMove "RIGHT" = Right GoRight
parseMove "RESET" = Right Reset
parseMove other = Left ("Unknown movement: " ++ other)

parseRule :: String -> Either String Rule
parseRule raw =
  case splitOn "," raw of
    [p, [s], q, [t], m] -> do
      move <- parseMove m
      return Rule
        { fromState = trim p
        , readSymbol = s
        , toState = trim q
        , writeSymbol = t
        , movement = move
        }
    _ -> Left ("Invalid rule: " ++ raw)

validateRule :: Variant -> Rule -> Either String ()
validateRule var rule
  | readSymbol rule `notElem` alphabet = Left ("Invalid read symbol in rule: " ++ show rule)
  | writeSymbol rule `notElem` alphabet = Left ("Invalid write symbol in rule: " ++ show rule)
  | var == LRTM && movement rule == GoLeft = Left ("LRTM rules cannot use LEFT: " ++ show rule)
  | var == BBTM && (readSymbol rule `notElem` "01" || writeSymbol rule `notElem` "01") =
      Left ("BBTM rules can only use 0 and 1: " ++ show rule)
  | otherwise = Right ()
  where
    alphabet = "01X*"

runMachine :: Machine -> String -> Verdict
runMachine machine tmInput = finalVerdict machine (last (runTrace machine tmInput))

runTrace :: Machine -> String -> [Config]
runTrace machine tmInput = go initialConfig
  where
    initialConfig = Config (initialTape machine tmInput) (initialHead machine) (initialState machine) Nothing 0
    go config
      | isAccept machine config = [config]
      | isReject machine config = [config]
      | otherwise =
          case Map.lookup (currentState config, symbolAt (tape config) (headPos config)) (ruleMap machine) of
            Nothing -> [config { currentState = fromMaybe "qr" (rejectState machine) }]
            Just rule -> config : go (applyRule machine config rule)

initialTape :: Machine -> String -> [Char]
initialTape machine tmInput
  | variant machine == BBTM = replicate 20 '0'
  | length tmInput > 19 = error "Input is too long; maximum length is 19"
  | any (`notElem` "01") tmInput = error "Input must contain only 0 and 1"
  | otherwise = take 20 (tmInput ++ repeat '*')

initialHead :: Machine -> Int
initialHead machine
  | variant machine == BBTM = 9
  | otherwise = 0

applyRule :: Machine -> Config -> Rule -> Config
applyRule machine config rule =
  let writtenTape = replaceAt (headPos config) (writeSymbol rule) (tape config)
      nextHead = moveHead machine (headPos config) (movement rule)
   in Config writtenTape nextHead (toState rule) (Just rule) (stepNumber config + 1)

moveHead :: Machine -> Int -> Move -> Int
moveHead machine pos move =
  case (variant machine, move) of
    (LRTM, Reset) -> 0
    (_, Reset) -> 0
    (_, GoLeft) -> max 0 (pos - 1)
    (_, GoRight) -> min 19 (pos + 1)

symbolAt :: [Char] -> Int -> Char
symbolAt xs index = xs !! max 0 (min 19 index)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index value xs =
  let (before, after) = splitAt index xs
   in before ++ [value] ++ drop 1 after

isAccept :: Machine -> Config -> Bool
isAccept machine config = currentState config == acceptState machine

isReject :: Machine -> Config -> Bool
isReject machine config = maybe False (== currentState config) (rejectState machine)

finalVerdict :: Machine -> Config -> Verdict
finalVerdict machine config
  | isAccept machine config = Accepted
  | otherwise = Rejected

renderVerdict :: Verdict -> String
renderVerdict Accepted = "ACCEPTED"
renderVerdict Rejected = "REJECTED"

showMove :: Move -> String
showMove GoLeft = "LEFT"
showMove GoRight = "RIGHT"
showMove Reset = "RESET"

data GuiModel = GuiModel
  { guiDescPath :: FilePath
  , guiInput :: String
  , guiMachine :: Machine
  , guiTrace :: [Config]
  }

runNativeGui :: FilePath -> String -> Machine -> [Config] -> IO ()
runNativeGui descPath tmInput machine traceData = do
  currentFrame <- newIORef 0
  hinst <- getModuleHandle Nothing
  let model = GuiModel descPath tmInput machine traceData
      className = mkClassName "CNSCC212_Universal_Turing_Machine"
      wndProc hwnd msg wParam lParam
        | msg == wM_DESTROY = do
            killTimer (Just hwnd) 1
            postQuitMessage 0
            return 0
        | msg == wM_TIMER = do
            advanceFrame currentFrame traceData
            invalidateRect (Just hwnd) Nothing True
            return 0
        | msg == wM_PAINT = do
            allocaPAINTSTRUCT $ \paintStruct -> do
              hdc <- beginPaint hwnd paintStruct
              frameIndex <- readIORef currentFrame
              drawGui hdc model (traceData !! frameIndex)
              endPaint hwnd paintStruct
            return 0
        | otherwise = defWindowProc (Just hwnd) msg wParam lParam
  cursor <- loadCursor Nothing iDC_ARROW
  brush <- createSolidBrush colorBackground
  _ <- registerClass (cS_HREDRAW + cS_VREDRAW, hinst, Nothing, Just cursor, Just brush, Nothing, className)
  hwnd <- createWindow className "CNSCC212 Universal Turing Machine" windowStyle
    (Just 80) (Just 80) (Just 1180) (Just 760)
    Nothing Nothing hinst wndProc
  _ <- setWinTimer hwnd 1 520
  _ <- showWindow hwnd sW_SHOWNORMAL
  updateWindow hwnd
  messageLoop
  deleteBrush brush
  where
    windowStyle = wS_OVERLAPPEDWINDOW

advanceFrame :: IORef Int -> [Config] -> IO ()
advanceFrame ref traceData = do
  index <- readIORef ref
  let lastIndex = length traceData - 1
  writeIORef ref (if index >= lastIndex then 0 else index + 1)

messageLoop :: IO ()
messageLoop =
  allocaMessage $ \msg -> do
    continue <- getMessage msg Nothing
    if continue
      then do
        _ <- translateMessage msg
        _ <- dispatchMessage msg
        messageLoop
      else return ()

drawGui :: HDC -> GuiModel -> Config -> IO ()
drawGui hdc model config = do
  _ <- setBkMode hdc tRANSPARENT
  fill hdc (0, 0, 1600, 1000) colorBackground
  titleFont <- makeFont 30 fW_BOLD "Segoe UI"
  bodyFont <- makeFont 16 fW_NORMAL "Segoe UI"
  monoFont <- makeFont 18 fW_BOLD "Consolas"
  smallFont <- makeFont 14 fW_NORMAL "Segoe UI"
  withFont hdc titleFont $ do
    text hdc 28 24 colorText "Universal Turing Machine"
    text hdc 390 30 colorAccent (show (variant (guiMachine model)))
  withFont hdc bodyFont $ do
    text hdc 30 72 colorMuted ("Description: " ++ guiDescPath model)
    text hdc 30 98 colorMuted ("Input: " ++ displayInput model)
  drawStatus hdc bodyFont (guiMachine model) (guiTrace model) config
  drawTape hdc monoFont config
  drawRules hdc bodyFont smallFont (guiMachine model) config
  deleteFont titleFont
  deleteFont bodyFont
  deleteFont monoFont
  deleteFont smallFont

displayInput :: GuiModel -> String
displayInput model
  | variant (guiMachine model) == BBTM = "(ignored for BBTM)"
  | otherwise = guiInput model

drawStatus :: HDC -> HFONT -> Machine -> [Config] -> Config -> IO ()
drawStatus hdc font machine traceData config =
  withFont hdc font $ do
    fillRound hdc (740, 26, 1118, 112) colorPanel colorBorder
    text hdc 764 48 colorMuted ("Step " ++ show (stepNumber config) ++ " / " ++ show (stepNumber (last traceData)))
    text hdc 912 48 colorMuted ("State " ++ currentState config)
    text hdc 764 78 verdictColor verdict
  where
    verdict
      | isAccept machine config = "ACCEPTED"
      | isReject machine config = "REJECTED"
      | otherwise = "RUNNING"
    verdictColor
      | isAccept machine config = colorAccent
      | isReject machine config = colorReject
      | otherwise = colorWarning

drawTape :: HDC -> HFONT -> Config -> IO ()
drawTape hdc font config = do
  fillRound hdc (28, 138, 1118, 300) colorPanel colorBorder
  withFont hdc font $ mapM_ drawCell (zip [0 :: Int ..] (tape config))
  drawHead hdc config
  where
    cellW = 52
    cellH = 60
    gap = 2
    startX = 48
    y = 176
    drawCell (index, symbol) = do
      let x = startX + index * (cellW + gap)
          active = index == headPos config
      fillRound hdc (rectI x y (x + cellW) (y + cellH)) (if active then colorActiveCell else colorCell) (if active then colorAccent else colorBorder)
      textCentered hdc (rectI x y (x + cellW) (y + cellH)) (if active then colorBackground else colorText) [symbol]

drawHead :: HDC -> Config -> IO ()
drawHead hdc config = do
  brush <- createSolidBrush colorAccent
  pen <- createPen pS_SOLID 1 colorAccent
  oldBrush <- selectBrush hdc brush
  oldPen <- selectPen hdc pen
  polygon hdc [pointI cx 260, pointI (cx - 14) 284, pointI (cx + 14) 284]
  _ <- selectBrush hdc oldBrush
  _ <- selectPen hdc oldPen
  deleteBrush brush
  deletePen pen
  where
    cx = 48 + headPos config * 54 + 26

drawRules :: HDC -> HFONT -> HFONT -> Machine -> Config -> IO ()
drawRules hdc bodyFont smallFont machine config = do
  fillRound hdc (28, 326, 1118, 708) colorPanel colorBorder
  withFont hdc bodyFont $ text hdc 50 350 colorText "Rules"
  withFont hdc smallFont $ do
    tableHeader
    mapM_ drawRuleRow (zip [0 :: Int ..] (rules machine))
  where
    tableHeader = do
      text hdc 58 386 colorMuted "Current"
      text hdc 170 386 colorMuted "State"
      text hdc 294 386 colorMuted "Read"
      text hdc 410 386 colorMuted "Next"
      text hdc 536 386 colorMuted "Write"
      text hdc 660 386 colorMuted "Move"
    drawRuleRow (row, rule) = do
      let y = 416 + row * 28
          active = firedRule config == Just rule
      whenGui active (fill hdc (rectI 48 (y - 4) 1088 (y + 22)) colorHighlight)
      text hdc 68 y (if active then colorAccent else colorMuted) (if active then ">" else "")
      text hdc 170 y colorText (fromState rule)
      text hdc 294 y colorText [readSymbol rule]
      text hdc 410 y colorText (toState rule)
      text hdc 536 y colorText [writeSymbol rule]
      text hdc 660 y colorText (showMove (movement rule))

whenGui :: Bool -> IO () -> IO ()
whenGui True action = action
whenGui False _ = return ()

makeFont :: Int -> FontWeight -> FaceName -> IO HFONT
makeFont size weight face =
  createFont (fromIntegral size) 0 0 0 weight False False False dEFAULT_CHARSET
    oUT_DEFAULT_PRECIS cLIP_DEFAULT_PRECIS dEFAULT_QUALITY (vARIABLE_PITCH + fF_SWISS) face

withFont :: HDC -> HFONT -> IO a -> IO a
withFont hdc font action = do
  old <- selectFont hdc font
  result <- action
  _ <- selectFont hdc old
  return result

text :: HDC -> Int -> Int -> COLORREF -> String -> IO ()
text hdc x y color value = do
  _ <- setTextColor hdc color
  textOut hdc (fromIntegral x) (fromIntegral y) value

textCentered :: HDC -> RECT -> COLORREF -> String -> IO ()
textCentered hdc (left, top, right, bottom) color value =
  text hdc (fromIntegral (left + ((right - left) `div` 2) - 5)) (fromIntegral (top + ((bottom - top) `div` 2) - 12)) color value

rectI :: Int -> Int -> Int -> Int -> RECT
rectI left top right bottom = (i32 left, i32 top, i32 right, i32 bottom)

pointI :: Int -> Int -> POINT
pointI x y = (i32 x, i32 y)

i32 :: Int -> Int32
i32 = fromIntegral

fill :: HDC -> RECT -> COLORREF -> IO ()
fill hdc rect color = do
  brush <- createSolidBrush color
  fillRect hdc rect brush
  deleteBrush brush

fillRound :: HDC -> RECT -> COLORREF -> COLORREF -> IO ()
fillRound hdc (left, top, right, bottom) fillColor borderColor = do
  brush <- createSolidBrush fillColor
  pen <- createPen pS_SOLID 1 borderColor
  oldBrush <- selectBrush hdc brush
  oldPen <- selectPen hdc pen
  roundRect hdc left top right bottom 10 10
  _ <- selectBrush hdc oldBrush
  _ <- selectPen hdc oldPen
  deleteBrush brush
  deletePen pen

colorBackground, colorPanel, colorCell, colorActiveCell, colorBorder, colorText, colorMuted, colorAccent, colorWarning, colorReject, colorHighlight :: COLORREF
colorBackground = rgb 18 20 26
colorPanel = rgb 28 32 42
colorCell = rgb 40 47 60
colorActiveCell = rgb 80 218 166
colorBorder = rgb 56 64 80
colorText = rgb 242 246 252
colorMuted = rgb 154 164 180
colorAccent = rgb 80 218 166
colorWarning = rgb 255 205 102
colorReject = rgb 255 107 122
colorHighlight = rgb 38 72 62

splitOn :: String -> String -> [String]
splitOn delimiter source =
  case findDelimiter source of
    Nothing -> [source]
    Just index ->
      let (before, after) = splitAt index source
       in before : splitOn delimiter (drop (length delimiter) after)
  where
    findDelimiter xs = find (\i -> take (length delimiter) (drop i xs) == delimiter) [0 .. length xs - length delimiter]

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
