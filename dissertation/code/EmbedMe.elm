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

type Stamp = { t: Float, x: Float, y: Float }

-- Incoming

port stamped : Signal { t: Float, x: Float, y: Float }

stamps : Signal [Stamp]
stamps = foldp (::) [] stamped

scene (w,h) locs =
  let drawCircle {t, x, y} =
          circle 5 |> filled (hsva y 1 1 0.7)
                   |> move (x - toFloat w / 2, toFloat h / 2 - y)
  in  collage w h (map drawCircle locs)

-- main = lift2 scene Window.dimensions stamps

-- Outgoing

-- Do not change user_id = "1" 
-- It gets replaced with the actual user_id when compiled to JS
user_id = "1"

firebaseRequest requestType requestData = 
  Http.request requestType 
               ("https://sweltering-fire-9141.firebaseio.com/"
	          ++ "dissertation/" 
		  ++ user_id 
                  ++ ".json")
               requestData 
               []

serialize r = r |> JEXP.fromRecord 
                |> Json.fromJSObject 
		|> Json.toJSString " " 
		|> JS.toString

toRequestData (t, (x, y)) = {t = t, x = x, y = y } |> serialize

clicks = timestamp (sampleOn Mouse.isDown Mouse.position) 

toRequest click = case click of
  (0, (0, 0)) -> firebaseRequest "get" ""
  _           -> firebaseRequest "post" (click |> toRequestData)

requests = clicks ~> toRequest

sendRequests = Http.send requests
