module EmbedMe where

import Graphics.Input as Input
import JavaScript as JS
import JavaScript.Experimental as JEXP
import Http
import Json
import Mouse
import Dict
import Window


(~>) = flip lift
infixl 4 ~>

(~~>) = flip lift2

(<~~) = (<|)

type Stamp = { t: Float, x: Float, y: Float }

-- Incoming

port stamped : Signal { t: Float, x: Float, y: Float }

stamps : Signal [Stamp]
stamps = foldp (::) [] stamped

scene (w,h) locs =
  let drawPentagon {t,x,y} =
          ngon 5 20 |> filled (hsva y 1 1 0.7)
                    |> move (x - toFloat w / 2, toFloat h / 2 - y)
                    |> rotate x
  in  collage w h (map drawPentagon locs)

main = lift2 scene Window.dimensions stamps

-- Outgoing

user_id = "1"

firebaseRequest requestType requestData = Http.request requestType ("https://sweltering-fire-9141.firebaseio.com/dissertation/" ++ user_id ++ ".json") requestData []



serialize r = r |> JEXP.fromRecord |> Json.fromJSObject |> Json.toJSString " " |> JS.toString

toRequestData (t, (x, y)) = {t = t, x = x, y = y } |> serialize

clicks = timestamp (sampleOn Mouse.isDown Mouse.position) 

toRequest click = case click of
  (0, (0, 0)) -> firebaseRequest "get" ""
  (t, (x, y)) -> firebaseRequest "post" (click |> toRequestData)

requests = clicks ~> toRequest

sendRequests = Http.send requests
