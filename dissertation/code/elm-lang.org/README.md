Welcome to the elm-lang.org README!
-----------------------------------


Installation
============

See INSTALL for installation instructions


How do I run user experiments?
==============================

After installing elm, elm-server, and building the executable, and
running ./run-elm-server, navigate to one of the following URLs to
see the user experiment interface in action:

* Elm task: http://0.0.0.0:8000/edit/task/MovingBox.elm
* JS task : http://0.0.0.0:8000/_edit/task/MovingBox.js

By default, clicks that occur in the code pane are saved to this 
json file:

`https://sweltering-fire-9141.firebaseio.com/dissertation.json`

*See the section 'How do I save to my own Firebase?' for 
customisation*

If you  write your own tasks, visiting `/edit/<path/to/task>` will 
populate the click database elm directory with click data as you use 
it, for your task.

Furthermore, appending `?p=<participant-id>` allows you to annotate
the data with the particular participant performing those clicks.

If you host the elm environment on a server on the Internet, you can
send the link to your tasks to participants with a separate participant ID.

Example:

`http://my.server.com:8000/edit/task/MyTask.elm?p=42`

Send that to participant 42 and the firebase database will save to

`https://sweltering-fire-9141-firebaseio.com/dissertation/elm/42.json`


How do I visualise the click data?
==================================

Navigate to the root directory where the elm-lang.org was installed

`$ cd elm-lang.org/`

then open `EmbedMe.elm` with your favourite text editor, and 
uncomment the line:

`-- main = lift2 scene Window.dimensions Mouse.position`

by removing the `-- ` (`--` is a comment in Elm)


How do I save to my own Firebase (or other Database)
====================================================

Currently, EmbedElm only supports databases with a 
RESTful API.

If you have a databse with a RESTful API, do as follows:

As in the section "How do I visualise the click data?", open 
`EmbedMe.elm` and change 
`https://sweltering-fire-9141-firebaseio.com/` to your own
personal database.

You must also modify the same URL in 
`elm-lang.org/server/Server.hs`. Go to the function `embedee` 
and where the variable `visualiser` is defined:

	visualiser = 
	  concat [ "var firebaseData = new Firebase('"
	         , "http://sweltering-fire-9141.firebaseio.com/"
		 , "dissertation/
		 , langStr
        ...

and change the URL to your own.


How do I add support for my own languages?
==========================================

Currently, the IDE only supports Elm and Javascript, but if you are
feeling adventurous, and would like to add support for more, you will 
need to modify at least the following code:

* The `data Lang` type constructor

* All functions with Lang in the type signature, e.g.

   `embedee :: Lang -> String -> String -> H.Html`

* The URL routing in `elm-lang.org/server/Server.hs`, e.g.

   ~~~~~~~~~~{.haskell}
   main = do
      ...
      <|> route [ ("try", serveHtml Editor.empty)
                , ("edit", edit Elm)
   	     , ("_edit", edit Javascript)
   	     , ("__edit", edit MyLanguage)
   	     , ("code", code Elm)
   	     , ("_code", code Javascript)
   	     , ("__code", code MyLanguage)
   	     , ("compile", compile Elm)
   	     , ("_compile", compile Javascript)
   	     , ("__compile", compile MyLanguage)
   ...
   ~~~~~~~~~~~~~~~~~~~~
