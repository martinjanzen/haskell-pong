{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent (threadDelay)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import Data.String (IsString (..))
import Data.Word (Word32, Word8)
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF
import System.Console.ANSI
import System.Console.ANSI.Codes (csi)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hReady, hSetBuffering, hSetEcho, stdin)

type Position = (Int, Int)
data Game = Game
    { ballPos :: Position
    , ballDir :: Position
    , paddlePos :: Int
    , opponentPos :: Int
    , score :: Int
    , gameOutcome :: Maybe GameOutcome
    , geometry :: GameGeometry
    }

data GameGeometry = GameGeometry
    { paddleWidth :: Int
    , hitboxWidth :: Int
    , paddleSize :: Int
    , screenWidth :: Int
    , screenHeight :: Int
    , borderWidth :: Int
    , paddleMovementSpeed :: Int
    , opponentMovementSpeed :: Int
    }
    deriving (Show)

data GameOutcome = PlayerLost | PlayerWon | Quit

-- | A rectangle, using inclusive math
data Rect = Rect {topLeft :: Position, bottomRight :: Position}
    deriving (Show)

data DrawCommand
    = Paddle Int WhichPaddle
    | Ball Position
    deriving (Show)

data WhichPaddle = Player | Opponent
    deriving (Show)

consoleGeometry :: GameGeometry
consoleGeometry =
    GameGeometry
        { paddleWidth = 1
        , hitboxWidth = 1
        , paddleSize = 4
        , screenWidth = 80
        , screenHeight = 24
        , borderWidth = 1
        , paddleMovementSpeed = 1
        , opponentMovementSpeed = 1
        }

guiGeometry :: GameGeometry
guiGeometry =
    GameGeometry
        { paddleWidth = 10
        , hitboxWidth = 1
        , paddleSize = 40
        , screenWidth = 800
        , screenHeight = 400
        , borderWidth = 10
        , paddleMovementSpeed = 5
        , opponentMovementSpeed = 5
        }

paddleHitbox :: GameGeometry -> WhichPaddle -> Int -> Rect
paddleHitbox g@GameGeometry{..} Player yPos =
    let theTopLeft = fst (playingAreaX g) + paddleWidth
     in Rect
            { topLeft = (theTopLeft, yPos)
            , bottomRight = (theTopLeft + hitboxWidth, yPos + paddleSize)
            }
paddleHitbox g@GameGeometry{..} Opponent yPos =
    let theTopLeft = snd (playingAreaX g) - paddleWidth
     in Rect
            { topLeft = (theTopLeft - hitboxWidth, yPos)
            , bottomRight = (theTopLeft, yPos + paddleSize)
            }

inHitbox :: (Int, Int) -> Rect -> Bool
(x, y) `inHitbox` Rect{topLeft = (tlX, tlY), bottomRight = (brX, brY)} = x `inRange` (tlX, brX) && y `inRange` (tlY, brY)

gameAreaHeight :: GameGeometry -> Int
gameAreaHeight GameGeometry{..} = screenHeight - borderWidth * 2

gameAreaWidth :: GameGeometry -> Int
gameAreaWidth GameGeometry{..} = screenWidth - borderWidth * 2

maxPaddleY :: GameGeometry -> Int
maxPaddleY g@GameGeometry{..} = borderWidth + gameAreaHeight g - paddleSize

playingAreaY :: GameGeometry -> (Int, Int)
playingAreaY GameGeometry{..} = (borderWidth, (screenHeight - 1) - borderWidth)

playingAreaX :: GameGeometry -> (Int, Int)
playingAreaX GameGeometry{..} = (borderWidth, (screenWidth - 1) - borderWidth)

inRange :: (Ord a) => a -> (a, a) -> Bool
a `inRange` (x, y) = a >= x && a <= y

initialGame :: Game
initialGame =
    Game
        { ballPos = (40, 12)
        , ballDir = (1, 1)
        , paddlePos = 10
        , opponentPos = 10
        , score = 0
        , gameOutcome = Nothing
        , geometry = consoleGeometry
        }

initialGameGui :: Game
initialGameGui =
    Game
        { ballPos = (40, 12)
        , ballDir = (2, 2)
        , paddlePos = 10
        , opponentPos = 10
        , score = 0
        , gameOutcome = Nothing
        , geometry = guiGeometry
        }

data Renderer s = Renderer
    { makeState :: IO s
    , drawInitial :: s -> GameGeometry -> IO ()
    , drawFrame :: s -> GameGeometry -> [DrawCommand] -> Int -> IO ()
    , collectInput :: s -> Game -> IO Game
    , waitForFrame :: s -> IO ()
    , finishGame :: s -> Game -> IO ()
    }

consoleRenderer :: Renderer ()
consoleRenderer =
    Renderer
        { makeState = pure ()
        , drawInitial = const drawInitialFrameConsole
        , drawFrame = const drawConsole
        , collectInput = const chompInputConsole
        , waitForFrame = const $ threadDelay 100000
        , finishGame = const finishConsole
        }

runGame :: Renderer s -> Game -> IO ()
runGame renderer game = do
    s <- makeState renderer
    drawInitial renderer s (geometry game)
    gameLoop (s, renderer) game

main :: IO ()
main = do
    -- let renderer = consoleRenderer
    --     game = initialGame
    arghs <- getArgs
    case arghs of
        ["gui"] -> runGame guiRenderer initialGameGui
        _ -> runGame consoleRenderer initialGame

leftBorder :: GameGeometry -> Rect
leftBorder GameGeometry{..} =
    Rect
        { topLeft = (0, borderWidth)
        , bottomRight = (borderWidth - 1, screenHeight - borderWidth - 1)
        }

rightBorder :: GameGeometry -> Rect
rightBorder GameGeometry{..} =
    Rect
        { topLeft = (screenWidth - 1 - (borderWidth - 1), borderWidth)
        , bottomRight = (screenWidth - 1, screenHeight - borderWidth - 1)
        }

topBorder :: GameGeometry -> Rect
topBorder GameGeometry{..} =
    Rect
        { topLeft = (0, 0)
        , bottomRight = (screenWidth - 1, borderWidth - 1)
        }

bottomBorder :: GameGeometry -> Rect
bottomBorder GameGeometry{..} =
    Rect
        { topLeft = (0, screenHeight - 1 - (borderWidth - 1))
        , bottomRight = (screenWidth - 1, screenHeight - 1)
        }

drawInitialFrameConsole :: GameGeometry -> IO ()
drawInitialFrameConsole g = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hideCursor

    clearScreen
    fillAreaTerminal '-' (topBorder g)
    fillAreaTerminal '|' (leftBorder g)
    fillAreaTerminal '|' (rightBorder g)
    fillAreaTerminal '-' (bottomBorder g)

drawCommands :: Game -> [DrawCommand]
drawCommands Game{..} =
    [ Ball ballPos
    , Paddle opponentPos Opponent
    , Paddle paddlePos Player
    ]

-- | Chomps (potentially multiple) characters of input until there is no more input
chompInputConsole :: Game -> IO Game
chompInputConsole game = do
    charAvailable <- hReady stdin
    if charAvailable
        then do
            char <- getChar
            chompInputConsole (movePaddle game char)
        else return game

gameLoop :: (s, Renderer s) -> Game -> IO ()
gameLoop (s, renderer) game = do
    drawFrame renderer s (geometry game) (drawCommands game) (score game)
    if isNothing (gameOutcome game)
        then do
            let game' = moveBall . moveOpponent $ game
            game'' <- collectInput renderer s game'
            waitForFrame renderer s
            gameLoop (s, renderer) game''
        else finishGame renderer s game

moveBall :: Game -> Game
moveBall game@Game{geometry = g}
    | isJust outcome = game{gameOutcome = outcome}
    | otherwise = game{ballPos = (newPosX, newPosY), ballDir = newDir, score = newScore}
  where
    (x, y) = ballPos game
    (dx, dy) = ballDir game
    newPosX = x + dx
    newPosY = y + dy
    hitPaddle = (newPosX, newPosY) `inHitbox` paddleHitbox g Player (paddlePos game)
    hitOpponent = (newPosX, newPosY) `inHitbox` paddleHitbox g Opponent (opponentPos game)
    hitWall = newPosY <= fst (playingAreaY g) || newPosY >= snd (playingAreaY g)
    outcome
        | newPosX < 0 = Just PlayerLost
        | newPosX > screenWidth g - borderWidth g = Just PlayerWon
        | otherwise = Nothing
    newDir = (if hitPaddle || hitOpponent then -dx else dx, if hitWall then -dy else dy)
    newScore = if hitPaddle || hitOpponent then score game + 1 else score game

movePaddle :: Game -> Char -> Game
movePaddle game@Game{geometry = g} 'w' = game{paddlePos = max (borderWidth g) (paddlePos game - paddleMovementSpeed g)}
movePaddle game@Game{geometry = g} 's' = game{paddlePos = min (maxPaddleY g) (paddlePos game + paddleMovementSpeed g)}
movePaddle game 'q' = game{gameOutcome = Just Quit}
movePaddle game _ = game

moveOpponent :: Game -> Game
moveOpponent game@Game{geometry = g}
    | ballY < opponentY + paddleSize g `div` 2 = game{opponentPos = max (borderWidth g) (opponentY - opponentMovementSpeed g)}
    | ballY > opponentY + paddleSize g `div` 2 = game{opponentPos = min (maxPaddleY g) (opponentY + opponentMovementSpeed g)}
    | otherwise = game
  where
    ballY = snd $ ballPos game
    opponentY = opponentPos game

fillAreaTerminal :: Char -> Rect -> IO ()
fillAreaTerminal ch Rect{topLeft = tl, bottomRight = br} = do
    let width = fst br - fst tl + 1
    for_ [snd tl .. snd br] $ \y -> do
        setCursorPosition y (fst tl)
        putStr (replicate width ch)

drawConsole :: GameGeometry -> [DrawCommand] -> Int -> IO ()
drawConsole g cmds gameScore = do
    -- Only redraw dynamic content
    clearGameArea
    for_ cmds performDraw

    setCursorPosition (screenHeight g + 1) 0
    clearLine
    putStrLn $ "Score: " ++ show gameScore
  where
    -- https://terminalguide.namepad.de/seq/csi_cx/
    deleteLine width = csi [width] "X"

    performDraw (Ball pos) = drawBall pos
    performDraw (Paddle y which) = drawPaddle y which

    clearGameArea = for_ [fst (playingAreaY g) .. snd (playingAreaY g)] $ \y -> do
        setCursorPosition y (fst (playingAreaX g))
        putStr $ deleteLine (gameAreaWidth g)

    drawBall (x, y) = do
        setCursorPosition y x
        putStr "O"

    drawPaddle y kind = do
        let x = case kind of
                Player -> fst (playingAreaX g)
                Opponent -> snd (playingAreaX g)
        fillAreaTerminal
            '|'
            Rect
                { topLeft = (x, y)
                , bottomRight = (x, y + paddleSize g - 1)
                }

outcomeMessage :: GameOutcome -> String
outcomeMessage Quit = "Quit!"
outcomeMessage PlayerLost = "You lost!"
outcomeMessage PlayerWon = "You won!"

formatOutcome :: Game -> String
formatOutcome game =
    maybe "" outcomeMessage (gameOutcome game) ++ " Your score: " ++ show (score game)

finishConsole :: Game -> IO ()
finishConsole game@Game{geometry = g} = do
    setCursorPosition (screenHeight g + 1) 0
    clearLine
    putStrLn $ formatOutcome game
    showCursor

data GuiState = GuiState
    { gsWindow :: SDL.Window
    , gsRenderer :: SDL.Renderer
    , gsLastFrameTime :: IORef Word32
    , gsFont :: TTF.Font
    }

fontSize :: Int
fontSize = 32

initGui :: IO GuiState
initGui = do
    SDL.initializeAll
    TTF.initialize

    font <- TTF.load "./ComicNeue-Bold.ttf" fontSize
    TTF.setStyle font [TTF.Bold]

    win <- SDL.createWindow "pong" SDL.defaultWindow
    renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
    lastFrameTime <- newIORef =<< SDL.ticks
    pure GuiState{gsWindow = win, gsRenderer = renderer, gsLastFrameTime = lastFrameTime, gsFont = font}

v2 :: (Int, Int) -> SDL.V2 CInt
v2 (x, y) = SDL.V2 (fromIntegral x) (fromIntegral y)

rectangleToSDL :: Rect -> SDL.Rectangle CInt
rectangleToSDL Rect{topLeft = tl@(tlX, tlY), bottomRight = (brX, brY)} =
    SDL.Rectangle
        (SDL.P (v2 tl))
        (SDL.V2 (fromIntegral (brX - tlX)) (fromIntegral (brY - tlY)))

backgroundColour :: SDL.V4 Word8
backgroundColour = SDL.V4 255 255 255 255

paddleColour :: SDL.V4 Word8
paddleColour = SDL.V4 0 255 0 255

borderColour :: SDL.V4 Word8
borderColour = SDL.V4 255 0 0 255

ballColour :: SDL.V4 Word8
ballColour = SDL.V4 255 100 0 255

guiRect :: SDL.Renderer -> Rect -> IO ()
guiRect r rect = SDL.fillRect r (Just . rectangleToSDL $ rect)

guiDrawBorder :: GuiState -> GameGeometry -> IO ()
guiDrawBorder gs g = do
    let r = gsRenderer gs
    SDL.rendererDrawColor r $= borderColour
    guiRect r (leftBorder g)
    guiRect r (rightBorder g)
    guiRect r (topBorder g)
    guiRect r (bottomBorder g)

guiDrawInitial :: GuiState -> GameGeometry -> IO ()
guiDrawInitial gs g = do
    let r = gsRenderer gs
    guiDrawBorder gs g
    SDL.present r
    pure ()

guiRenderText :: GuiState -> Position -> TTF.Color -> String -> IO ()
guiRenderText gs pos colour text = do
    let r = gsRenderer gs
    surf <- TTF.blended (gsFont gs) colour (fromString text)
    tex <- SDL.createTextureFromSurface r surf
    sz <- SDL.surfaceDimensions surf
    SDL.copy r tex Nothing (Just $ SDL.Rectangle (SDL.P $ v2 pos) sz)
    SDL.destroyTexture tex
    SDL.freeSurface surf
    pure ()

guiDrawFrame :: GuiState -> GameGeometry -> [DrawCommand] -> Int -> IO ()
guiDrawFrame gs g draws score = do
    SDL.rendererDrawColor r $= backgroundColour
    SDL.clear r

    guiDrawBorder gs g
    for_ draws drawObject

    guiRenderText gs (0, screenHeight g) ballColour ("Score: " ++ show score)

    SDL.present r
  where
    r = gsRenderer gs

    ballSize = 4
    ballOffs = ballSize `div` 2

    drawObject (Ball (px, py)) = do
        SDL.rendererDrawColor r $= ballColour
        guiRect
            r
            Rect
                { topLeft = (px - ballOffs, py - ballOffs)
                , bottomRight = (px + ballOffs, py + ballOffs)
                }
    drawObject (Paddle y kind) = do
        let x = case kind of
                Player -> fst (playingAreaX g)
                Opponent -> snd (playingAreaX g) - paddleWidth g
        SDL.rendererDrawColor r $= paddleColour

        guiRect
            r
            Rect
                { topLeft = (x, y)
                , bottomRight = (x + paddleWidth g, y + paddleSize g - 1)
                }

guiCollectInput :: GuiState -> Game -> IO Game
guiCollectInput _gs game = do
    events <- SDL.pollEvents
    let game' = foldr (applyEvent . SDL.eventPayload) game events

    pure game'
  where
    keycodeMapping =
        [ (SDL.KeycodeQ, 'q')
        , (SDL.KeycodeW, 'w')
        , (SDL.KeycodeS, 's')
        ]
    applyEvent ev game' = case ev of
        SDL.KeyboardEvent ke
            | SDL.keyboardEventKeyMotion ke == SDL.Pressed ->
                let keycodeMay = flip lookup keycodeMapping . SDL.keysymKeycode . SDL.keyboardEventKeysym $ ke
                 in maybe game' (movePaddle game') keycodeMay
        SDL.QuitEvent ->
            game'{gameOutcome = Just Quit}
        _ -> game'

msPerFrame :: Int
msPerFrame = 1000 `div` 30

guiWaitForFrame :: GuiState -> IO ()
guiWaitForFrame gs = do
    lastFrameTime <- readIORef (gsLastFrameTime gs)
    now <- SDL.ticks
    let nextFrameIn = min msPerFrame (fromIntegral (now - lastFrameTime))
    writeIORef (gsLastFrameTime gs) now
    -- Takes microseconds
    threadDelay ((msPerFrame - nextFrameIn) * 1000)

guiFinishGame :: GuiState -> Game -> IO ()
guiFinishGame gs game@Game{geometry = g} = do
    guiRenderText gs (screenWidth g `div` 2, screenHeight g `div` 2) ballColour (formatOutcome game)
    SDL.present (gsRenderer gs)
    loopy
  where
    loopy = do
        event <- SDL.waitEvent

        if isQuit (SDL.eventPayload event)
            then do
                SDL.destroyWindow (gsWindow gs)
            else loopy

    isQuit ev = case ev of
        SDL.KeyboardEvent ke
            | SDL.keyboardEventKeyMotion ke == SDL.Pressed
                && (SDL.keysymKeycode . SDL.keyboardEventKeysym) ke == SDL.KeycodeQ ->
                True
        SDL.QuitEvent -> True
        _ -> False

guiRenderer :: Renderer GuiState
guiRenderer =
    Renderer
        { makeState = initGui
        , drawInitial = guiDrawInitial
        , drawFrame = guiDrawFrame
        , collectInput = guiCollectInput
        , waitForFrame = guiWaitForFrame
        , finishGame = guiFinishGame
        }

-- ADDITIONAL COMMENTS --
-- Pretty simple AI just tries to keep the ball in play by moving towards it.
-- AI is unbeatable -> increase ball speed and AI speed
