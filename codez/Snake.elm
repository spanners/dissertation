import Window
import Keyboard
import Random

(width, height) = (600, 400)
(hWidth, hHeight) = (width/2, height/2)

speed : Float
speed = 5
spawnInterval = 5

size : Float
size = 10

vecLen : Vec -> Float
vecLen (x,y) = sqrt (x * x + y * y)

vecSub : Vec -> Vec -> Vec
vecSub (ax,ay) (bx,by) = (ax-bx, ay-by)

data Event = Tick (Time, (Int, Int)) | Spawn Apple

formString : Float -> Float -> String -> Form
formString y scl str = toText str |> Text.color black
                          |> text 
                          |> toForm
                          |> scale scl 
                          |> move (0, y)

delta : Signal Float
delta = (fps 30)

input : Signal (Float, (Int, Int))
input = (,) <~ lift inSeconds delta
             ~ sampleOn delta ((\{ x, y } -> (x, y)) <~ Keyboard.arrows)

type Vec = (Float, Float) 

data Heading = Up | Down | Left | Right

type Apple = { pos : Vec
             , col : Color }

defaultApple = { pos = (0,0) 
               , col = lightGreen }

type Snake = { head    : Vec
             , heading : Heading
             , body    : [Vec] }

defaultSnake : Snake
defaultSnake = { head = (0, 0)
               , heading = Right 
               , body = [ (0,0), 
                          (0,0), 
                          (0,0), 
                          (0,0), 
                          (0,0) ] }

type Game = { snake  : Snake
            , score  : Int 
            , apples : [Apple] }

defaultGame : Game
defaultGame = { snake = defaultSnake
              , score = 0 
              , apples = [{defaultApple | pos <- (90,90)}] }

newApple : Vec -> Apple
newApple pos = { defaultApple | pos <- pos }


updatePos : Heading -> Float ->  Vec -> Vec
updatePos h sp (x,y) = if | h == Left  -> (x - sp, y)
                          | h == Right -> (x + sp, y)
                          | h == Down  -> (x, y - sp) 
                          | h == Up    -> (x, y + sp)
                          | otherwise  -> (x,      y)

init : [Vec] -> [Vec]
init list = if | length list < 2 -> list
               | otherwise -> let (x::xs) = list in x :: (initHelper xs)

initHelper : [Vec] -> [Vec]
initHelper (x::xs) = if | xs == []  -> []
                        | otherwise -> x :: (initHelper xs)

updateBody : Vec -> Heading -> [Vec] -> [Vec]
updateBody head h body = map (updatePos h 0) (head :: (init body))

stepSnake : (Int, Int) -> Bool -> Snake -> Snake
stepSnake dir hit ({head, heading} as snake) = 
    let h = heading
        left  dir = (fst dir) < 0
        right dir = (fst dir) > 0
        down  dir = (snd dir) < 0
        up    dir = (snd dir) > 0
        getHeading dir = if | left  dir -> Left
                            | right dir -> Right
                            | down  dir -> Down
                            | up    dir -> Up
        update body = updateBody head h body
    in { snake | head <- updatePos h speed head
               , heading <- if | left dir || right dir -> if h == Left || h == Right 
                                                          then h 
                                                          else getHeading dir
                               | down dir || up dir -> if h == Up || h == Down 
                                                       then h 
                                                       else getHeading dir
                               | otherwise -> h
               , body <- if hit 
                         then update ((width+size,height+size) :: snake.body) 
                         else update snake.body }

stepGame : Event -> Game -> Game
stepGame event g = 
    case event of
        Tick (t, dir) -> let  hit apple = 
                                (vecLen <| vecSub g.snake.head apple.pos) < (size + size)
                              touched = filter hit g.apples
                              untouched = filter (not . hit) g.apples
                              hitApple = not <| isEmpty touched
                              g' = { g | snake <- stepSnake dir hitApple g.snake
                                       , apples <- untouched
                                       , score <- if hitApple then g.score + 1 else g.score }
                              out = let (x, y) = g.snake.head 
                                    in abs x > hWidth - size || 
                                         abs y > hHeight - size
                         in if out then defaultGame else g'
        Spawn a       -> { g | apples <- a :: g.apples }
        _             -> g

randFloat sig = (lift (\x -> x / 100)
                        (lift toFloat (Random.range 0 100 sig)))

rand fn sig = lift fn (randFloat sig)

randX = rand (\x -> (width * x) + -hWidth)

randY = rand (\y -> (height * y) + -hHeight)

interval = (every (second * spawnInterval))

event : Signal Event
event = merges [ lift Tick input 
               , lift (\pos -> Spawn (newApple pos)) (lift2 (,) (randX interval) (randY interval)) ]

render : (Int, Int) -> Game -> Element
render (w, h) g = 
    let formSnake head = circle size |> filled black |> move head
        txts  = [ (formString 100 2 (show g.score)) ]
        forms = txts ++ map formSnake (g.snake.head :: g.snake.body) ++ map (\a -> formSnake a.pos) g.apples 
    in color black <| container w h middle <| color white <| collage width height forms

main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame event
