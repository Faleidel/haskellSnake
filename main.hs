{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import System.Random

import System.IO.Unsafe

data Direction = DUp | DRight | DDown | DLeft deriving (Eq,Enum,Show)

next :: Direction -> Direction
next DLeft = DUp
next a = succ a

data Box = Box
    {
        _boxX :: Float ,
        _boxY :: Float 
    }
    deriving Show
makeLenses ''Box

data GameState = GameState
    {
        --the player is a list of position
        _coolDown :: Int ,
        _player :: [(Float,Float)] ,
        _life :: Int ,
        _grow :: Bool , -- will the player grow a block longer
        _playerDirection :: Direction ,
        _turned :: Bool ,
        _movementCoolDown :: Int ,
        _redBoxes :: [Box] ,
        _greenBoxes :: [Box] ,
        _yellowBoxes :: [Box]
    }
    | DeadState -- main menu and game over state
    deriving Show
makeLenses ''GameState

init' :: [a] -> [a]
init' [] = []
init' x = init x

gameScale = 10.0
gameSize = 500.0
gameWindowTitle = "Snake"

canTurn :: Direction -> Direction -> Bool
canTurn a b = not $ (next $ next a) == b

mCanTurn :: Maybe Direction -> Direction -> Bool
mCanTurn (Just b) a = canTurn a b
mCanTurn _ _ = False

playerBody :: Picture
playerBody = Polygon
    [
        ( -gameScale/2 , -gameScale/2 ) ,
        ( -gameScale/2 ,  gameScale/2 ) ,
        (  gameScale/2 ,  gameScale/2 ) ,
        (  gameScale/2 , -gameScale/2 ) ,
        ( -gameScale/2 , -gameScale/2 )
    ]

-- get a random number and scale back to a position in stage
prosValForPos :: Integer -> Float
prosValForPos g = ( fromInteger (g `mod` ( floor $ gameSize/gameScale)) :: Float  ) - (gameSize/gameScale/2.0)

--The Integer list is an infinit list of random numbers
genBoxes :: [Integer] -> Int -> ([Box],[Integer])
genBoxes s           0 = ([],s)
genBoxes (v1:v2:seq) n = ((  Box (prosValForPos v1) (prosValForPos v2)  ):( nl  )  ,  ns  )
    where ns = snd g
          nl = fst g
          g = genBoxes seq (n-1)

boxesOverlap :: Box -> Box -> Bool
boxesOverlap b1 b2 = ( (_boxX b1) == (_boxX b2) ) && ( (_boxY b1) == (_boxY b2) )

--will remove boxes of bs1 if they overlap bs2
filterBoxesOverlap bs2 bs1 = filter (\x ->not $ foldl (||) False (map (boxesOverlap x) bs2) ) bs1

initWorld :: Int -> IO GameState
initWorld d = do
    g <- getStdGen
    let (rb,g2) = genBoxes (randoms g :: [Integer]) 50
    let (gb,g3) = genBoxes (g2) 50
    let (yb,g4) = genBoxes (g3) 10
    let gb2 = filterBoxesOverlap rb gb
    let yb2 = (filterBoxesOverlap rb) . (filterBoxesOverlap gb) $ yb
    return $ GameState {
        _coolDown = d ,
        _player = [(0,0)] ,
        _life = 10 ,
        _grow = False ,
        _playerDirection = DLeft ,
        _turned = False ,
        _movementCoolDown = 20 , 
        _redBoxes = rb ,
        _greenBoxes = gb2 ,
        _yellowBoxes = yb2
    }

eventKey :: Event -> Maybe Direction
eventKey (EventKey (SpecialKey KeyDown) Down _ _)  = Just DDown
eventKey (EventKey (SpecialKey KeyUp) Down _ _)    = Just DUp
eventKey (EventKey (SpecialKey KeyLeft) Down _ _)  = Just DLeft
eventKey (EventKey (SpecialKey KeyRight) Down _ _) = Just DRight
eventKey _ = Nothing

isf1 :: Event -> Bool
isf1 (EventKey (SpecialKey KeyF1) Down _ _) = True
isf1 _ = False

isf2 :: Event -> Bool
isf2 (EventKey (SpecialKey KeyF2) Down _ _) = True
isf2 _ = False

turnPlayer :: Maybe Direction -> GameState -> GameState
turnPlayer _ DeadState = DeadState
turnPlayer _ (w@(GameState { _turned = True })) = w
turnPlayer e w =
    let world = w { _turned = True } in
    case e of
        Just DDown   -> set playerDirection DDown  world
        Just DUp     -> set playerDirection DUp    world
        Just DRight  -> set playerDirection DRight world
        Just DLeft   -> set playerDirection DLeft  world
        Nothing      -> world

eventOnWorld :: Event -> GameState -> IO GameState
eventOnWorld event DeadState =
    if isf1 event then do
        w <- initWorld 1
        return w
    else
        if isf2 event then do
            w <- initWorld 2
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

updateWorldPlayerPos :: GameState -> GameState
updateWorldPlayerPos DeadState = DeadState
updateWorldPlayerPos world =
    if _movementCoolDown world < 1 then
        ( set turned (False) ) .
        updateWorldRedBoxesOverlap .
        updateWorldGreenBoxesOverlap .
        updateWorldYellowBoxesOverlap .
        noGrow .
        (
          over player
              (\l ->
                if _grow world then
                    (movePlayer (head l) (_playerDirection world)):( l )
                else
                    (movePlayer (head l) (_playerDirection world)):( init' l )
              )
        ) .
        ( set movementCoolDown (_coolDown world) ) $ world
    else
        over movementCoolDown (pred) world

--If the to listes are not of the same length, then you just eated some box and need to grow.
setGrow :: GameState -> GameState -> GameState
setGrow w1 w2 = w2 { _grow = not $ (length $ _greenBoxes w1) == (length $ _greenBoxes w2) }

--Will return True if the box is on the player position provided
boxOnPlayer :: ( Float , Float ) -> Box -> Bool
boxOnPlayer (px,py) box = not ( (px == (_boxX box)) && (py == (_boxY box)) )

updateWorldGreenBoxesOverlap :: GameState -> GameState
updateWorldGreenBoxesOverlap DeadState = DeadState
updateWorldGreenBoxesOverlap world =
    (setGrow world) .
    (over greenBoxes ( filter (boxOnPlayer $ head $ _player world) ))
    $ world

tryAddLife :: GameState -> GameState -> GameState
tryAddLife w1 w2 =
    if (length $ _yellowBoxes w1) == (length $ _yellowBoxes w2) then
        w2
    else
        over life (succ) w2

updateWorldYellowBoxesOverlap :: GameState -> GameState
updateWorldYellowBoxesOverlap DeadState = DeadState
updateWorldYellowBoxesOverlap world =
    (tryAddLife world) .
    (over yellowBoxes ( filter (boxOnPlayer $ head $ _player world) ))
    $ world

updateWorldRedBoxesOverlap :: GameState -> GameState
updateWorldRedBoxesOverlap DeadState = DeadState
updateWorldRedBoxesOverlap world =
    let pl = head $ (_player world) in
    if (foldl (||) False) . (map $ (not . (boxOnPlayer pl))) $ (_redBoxes world) then
        if (_life world == 0) || ( ( (length $ _player world) == 1 )) then
            DeadState
        else
            ( over player (tail) ) .
            ( over life (pred) )
            $ world
    else
        world

--Is the player folded on himself
playerOverlap :: [(Float,Float)] -> Bool
playerOverlap (x:[]) = False
playerOverlap (x:xs) = if foldl ( || ) (False) $ map (==x) xs then
        True
    else
        playerOverlap xs

tryKill :: GameState -> GameState
tryKill DeadState = DeadState
tryKill world = if playerOverlap $ _player world then
        DeadState
    else
        world

noGrow :: GameState -> GameState
noGrow DeadState = DeadState
noGrow world = world { _grow = False }

updateWorld :: Float -> GameState -> IO GameState
updateWorld _ DeadState = return DeadState
updateWorld _ world = do
    return $
         tryKill .
         updateWorldPlayerPos
         $ world

main :: IO ()
main = do
    playIO (InWindow gameWindowTitle (floor gameSize,floor gameSize) (0, 0))
        black
        30
        DeadState
        (render)
        eventOnWorld
        updateWorld

drawPlayer :: (Float,Float) -> Picture
drawPlayer (x,y) = Color (makeColor 1 1 1 1) $ Translate px py $ playerBody
    where px = x * gameScale
          py = y * gameScale

drawBox :: Box -> Color -> Picture
drawBox box color = Color color $ Translate ( _boxX box * gameScale ) ( _boxY box * gameScale ) $ playerBody

drawRedBox :: Box -> Picture
drawRedBox box = drawBox box (makeColor 1 0 0 1)

drawGreenBox :: Box -> Picture
drawGreenBox box = drawBox box (makeColor 0 1 0 1)

drawYellowBox :: Box -> Picture
drawYellowBox box = drawBox box (makeColor 0 1 1 1)

drawLife world = Scale (0.15) (0.15) $ Color ( makeColor 0.5 0.5 1 1 ) $ Translate ( -400 ) ( -400 ) $ Text ("life : " ++ (show $ _life world))

render :: GameState -> IO Picture
render DeadState = return $ Pictures [ Scale (0.2) (0.2) $ Color ( makeColor 1 0 0 1 ) $ Translate ( -400 ) ( 0 ) $ Text "Presse f1 to start" ]
render world = return $
    Pictures $
        ( drawLife world ) :
        (
            ( map (drawPlayer) (_player world) ) ++
            ( map (drawRedBox) $ _redBoxes world ) ++
            ( map (drawYellowBox) $ _yellowBoxes world ) ++
            ( map (drawGreenBox) $ _greenBoxes world )
        )
