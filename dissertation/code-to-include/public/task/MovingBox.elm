{- 

Try moving the square around with your keyboard's arrow keys

Click your mouse over there =====> 
Use arrows Up, Down, Left, Right

Whee!

Now modify the code to prevent the square from going outside 
the edge of the grey window.

-}

import Keyboard

areaSize = 400
squareSize = 40

main : Signal Element
main = lift display position

delta : Signal Float
delta = fps 30

input : Signal (Float, (Float,Float))
input =
    let vectors = lift toVector Keyboard.arrows
    in  sampleOn delta (lift2 (,) delta vectors)

toVector : { x:Int, y:Int } -> (Float,Float)
toVector {x,y} =
    if x /= 0 && y /= 0
      then (x / sqrt 2, y / sqrt 2)
      else (x,y)

position : Signal (Float,Float)
position = foldp update (0,0) input

update : (Float, (Float,Float)) -> (Float,Float) -> (Float,Float)
update (dt,(vx,vy)) (x,y) =
    (x + dt * vx / 2, y + dt * vy / 2)

display : (Float,Float) -> Element
display xy =
    collage (round areaSize) (round areaSize)
      [ rect areaSize areaSize
          |> filled grey
      , rect squareSize squareSize
          |> outlined (solid black)
          |> move xy
      ]
