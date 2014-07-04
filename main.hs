{-# LANGUAGE TemplateHaskell #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Lens

import System.Random

init' [] = []
init' x = init x

gameScale = 10.0
gameSize = 500.0
coolDown = 1

gameWindowTitle = "Snake"

data Direction = DLeft | DRight | DUp | DDown

canTurn :: Direction -> Direction -> Bool
canTurn DDown  DUp    = False
canTurn DUp    DDown  = False
canTurn DLeft  DRight = False
canTurn DRight DLeft  = False
canTurn _      _      = True

mCanTurn (Just b) a = canTurn a b
mCanTurn _ _ = False

data RedBox = RedBox
    {
        _redBoxX :: Float ,
        _redBoxY :: Float 
    }
makeLenses ''RedBox

playerBody = Polygon
    [
        ( -gameScale/2 , -gameScale/2 ) ,
        ( -gameScale/2 ,  gameScale/2 ) ,
        (  gameScale/2 ,  gameScale/2 ) ,
        (  gameScale/2 , -gameScale/2 ) ,
        ( -gameScale/2 , -gameScale/2 )
    ]

data GameState = GameState
    {
        _player :: [(Float,Float)] ,
        _grow :: Bool ,
        _playerDirection :: Direction ,
        _movementCoolDown :: Int ,
        _redBoxes :: [RedBox]
    } | DeadState
makeLenses ''GameState

prosValForPos g = ( fromInteger (g `mod` ( floor $ gameSize/gameScale)) :: Float  ) - (gameSize/gameScale/2.0)

box x y = RedBox { _redBoxX = x , _redBoxY = y }

genBoxes _           0 = []
genBoxes (v1:v2:seq) n = (  box (prosValForPos v1) (prosValForPos v2)  ):( genBoxes seq (n-1)  )

initWorld = do
    g <- getStdGen
    return $ GameState {
        _player = [(0,0)] ,
        _grow = False ,
        _playerDirection = DLeft ,
        _movementCoolDown = 10 , 
        _redBoxes = genBoxes (randoms g :: [Integer]) 100
    }

isLeft (EventKey (SpecialKey KeyLeft) Down _ _) = True
isLeft _ = False
isRight (EventKey (SpecialKey KeyRight) Down _ _) = True
isRight _ = False
isUp (EventKey (SpecialKey KeyUp) Down _ _) = True
isUp _ = False
isDown (EventKey (SpecialKey KeyDown) Down _ _) = True
isDown _ = False

eventKey (EventKey (SpecialKey KeyDown) Down _ _) =  Just DDown
eventKey (EventKey (SpecialKey KeyUp) Down _ _)   =  Just DUp
eventKey (EventKey (SpecialKey KeyLeft) Down _ _) =  Just DLeft
eventKey (EventKey (SpecialKey KeyRight) Down _ _) = Just DRight
eventKey _ = Nothing

isf1 (EventKey (SpecialKey KeyF1) Down _ _) = True
isf1 _ = False

turnPlayer e world = case e of
     Just DDown   -> set playerDirection DDown  world
     Just DUp     -> set playerDirection DUp    world
     Just DRight  -> set playerDirection DRight world
     Just DLeft   -> set playerDirection DLeft  world
     Nothing        -> world

eventOnWorld event DeadState =
    if isf1 event then do
        w <- initWorld
        return w
    else
        return DeadState
eventOnWorld event world = do
    return $ if mCanTurn key $ _playerDirection world then
        turnPlayer key world
    else
        world
          where key = eventKey event

movePlayer :: (Float,Float) -> Direction -> (Float,Float)
movePlayer (px,py) DUp    = ( px   , py+1 )
movePlayer (px,py) DRight = ( px+1 , py   )
movePlayer (px,py) DLeft  = ( px-1 , py   )
movePlayer (px,py) DDown  = ( px   , py-1 )

boxOnPlayer :: ( Float , Float ) -> RedBox -> Bool
boxOnPlayer (px,py) box = not ( (px == (_redBoxX box)) && (py == (_redBoxY box)) )

updateWorldPlayerPos world =
    if _movementCoolDown world < 1 then
        noGrow $ (
          over player
              (\l ->
                if _grow world then
                    (movePlayer (head l) (_playerDirection world)):( l )
                else
                    (movePlayer (head l) (_playerDirection world)):( init' l )
              )
        ) .
        ( set movementCoolDown coolDown ) $ world
    else
        over movementCoolDown (pred) world

setGrow w1 w2 = if ( (length $ _redBoxes w1) == (length $ _redBoxes w2) ) then
        w2
    else
        w2 { _grow = True }

updateWorldRedBoxesOverlap world =
    (setGrow
        world
        ) .
    (over
        redBoxes
        ( filter (boxOnPlayer $ head $ _player world) )
        )
    $ world

playerOverlap (x:[]) = False
playerOverlap (x:xs) = if foldl ( || ) (False) $ map (==x) xs then
        True
    else
        playerOverlap xs

tryKill world = if playerOverlap $ _player world then
        DeadState
    else
        world

noGrow world = world { _grow = False }

updateWorld _ DeadState = return DeadState
updateWorld _ world = do
    return $
         tryKill .
         updateWorldPlayerPos .
         updateWorldRedBoxesOverlap
         $ world

main = do
    playIO (InWindow gameWindowTitle (floor gameSize,floor gameSize) (0, 0))
        black
        30
        DeadState
        (render)
        eventOnWorld
        updateWorld

drawPlayer (x,y) = Color (makeColor 1 1 1 1) $ Translate px py $ playerBody
    where px = x * gameScale
          py = y * gameScale

drawRedBox box = Color (makeColor 1 0 0 1) $ Translate ( _redBoxX box * gameScale ) ( _redBoxY box * gameScale ) $ playerBody

render DeadState = return $ Pictures [ drawPlayer (0,0) ]
render world = return $
    Pictures $ ( map (drawPlayer) (_player world) ) ++ ( map (drawRedBox) $ _redBoxes world )
