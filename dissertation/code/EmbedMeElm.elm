module EmbedMe where

import Mouse
import Window
import Keyboard
import JavaScript as JS
import JavaScript.Experimental as JEXP
import Http
import Json

(~>) = flip lift
infixl 4 ~>

clicks : Signal (Time, (Int,Int))
clicks = timestamp (sampleOn Mouse.isDown Mouse.position)

user_id = "1"

firebaseRequest requestType requestData = 
  Http.request requestType 
    ("https://sweltering-fire-9141.firebaseio.com/dissertation/elm/" ++ user_id 
                                                                     ++ ".json") 
    requestData []
 
serialize r = r |> JEXP.fromRecord 
                |> Json.fromJSObject 
                |> Json.toJSString " " 
                |> JS.toString
 
toRequestData (t, (x,y)) = {t = t, x = x, y = y} |> serialize
 
toRequest event = case event of 
  (t, (x,y)) -> firebaseRequest "post" (event |> toRequestData)
 
requests = clicks ~> toRequest
 
sendRequests = Http.send requests
