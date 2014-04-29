# Implementation \label{implementation}

In this section I will describe how I implemented the desired requirements for the IDE, detailing the decisions made and issues encountered.

## Establishing context

Initially, the implementation involved a lot of exploratory programming, in order to try and figure out how each component of the existing IDE fit together, trying to determine how to get
constantly updating user input signals to stream via a `port` from Elm to
JavaScript, so that I could store `Mouse.position` and `Keyboard.keysDown` in a
DB backend for Elm that for embedding into the code pane. I found an example by Dénes Harmath that was close to what I wanted to achieve and forked the
repository. It lives at http://github.com/spanners/elm-lib

It is nice and lightweight, and I applied these ideas to
mouse input. See the palindrome Elm example at http://elm-lang.org/edit/examples/Intermediate/TextReverse.elm for relevant input signals as the inspiration for click logging that I used.

*However*, there are two **disadvantages with this approach**:

1. elm-lib uses an old Elm Foreign Function Interface [FFI] -- instead of using `port`s it uses `foreign import jsevent`. 

     I looked at how Evan Czaplicki does this in his elm-js-and-html example,  forked
     his repository at http://github.com/spanners/elm-js-and-html, and managed to see how he ported it from 0.10 to 0.12 which gave me some idea about how to do the same for elm-lib.


2. The second disadvantage is that it uses Firebase (http://firebase.io) as a
   backend for data storage. 
   
     This is a proprietary application. I would much rather use something Open
     Source that I can host myself for two reasons: Firstly for data protection
     reasons. This shouldn't be a problem due to the fact that it is mouse `(x,y)`
     co-ordinates and timestamps. Secondly because it has a hard-limit on the
     amount of data you can store on the FREE plan: 100MB. Again, this shouldn't be a
     problem so long as I filter the input intelligently.


## Issues encountered

My initial implementation of click logging used `Elm.embed` and offset a `<div>` tag as the place to load the arbitrary Elm code. This meant that all co--ordinates were offset by the height of the code `<div>`, and I would have to compute the offset after gathering the data. I tried to use `Elm.worker(Main.Elm, div, {})` and make the `<div>` element encompass the
  entire CodeMirror window, but the `Elm.worker` function turns out to be only for computational code, not for interacting with the outside world (Input/Output).

I managed to get Evan's stamps example (http://github.com/evancz/elm-html-and-js) working with Firebase (on my own repository, here: http://github.com/spanners/elm-js-and-html). Now I can successfully store user mouse events persistently in a JSON file.

This required dealing with **Disadvantage 1.** which I did
successfully, using the new `port` FFI.

I convert Elm Records into JSON Strings to be stored in Firebase like so:

~~~~~~~~~ {.haskell .numberLines}
firebaseRequest requestType requestData = 
  Http.request requestType 
               "https://username.firebaseio-demo.com/dissertation.json" 
               requestData 
               []
 
serialize r = r |> JEXP.fromRecord 
                |> Json.fromJSObject 
                |> Json.toJSString " " 
                |> JS.toString
 
toRequestData (x,y) = {x = x, y = y} |> serialize
 
toRequest event = case event of 
  (x,y) -> firebaseRequest "post" (event |> toRequestData)
 
requests = clicks ~> toRequest

sendRequests = Http.send requests
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With the Elm-runtime 0.12 port I was working on, I got the bug `elm: bad ADT got to port generation code` and had no luck finding why this was occurring, so posted on the *elm-discuss* mailing list asking for help (https://groups.google.com/forum/#!searchin/elm-discuss/ADT/elm-discuss/aIUK_MiW3yo/FZ0oSx-a1wYJ) on March 30th 2014.

For a while I only knew how to get visualisation working in Elm-runtime-0.10, which meant that I lost a lot of the benefits of the latest version (currently 0.12). I had to use regular expressions to swap where I'm doing a POST to submit mouseclick data, with GET to visualise the mouse data, depending on whether I want to visualise or capture mouse data. In essense, I would enter a special "View participant mouse data" mode into the IDE
e.g. specify a url path in addition to the experiment and the participant ID,
and then it loaded elm-runtime-0.10.js instead and did GET instead of POST on the same data

2 days after I posted to elm-discuss, Evan Czaplicki himself, semi-ported my half--working version of elm-lib (http://github.com/spanners/elm-lib) `StampTogether/Main.elm` to Elm-runtime 0.12  which is very helpful -- I can now modify this to suit my needs for the Firebase upload of click data

20 days after my post, Dénes Harmath (the original creator of elm-lib) published a fully--working port for Elm-runtime 0.12, but by this time I had found a workaround. I eventually used his version as it was much cleaner.

I fixed the quirk of having to click an offset from the code -- it turns out that CodeMirror.js binds to `mouse.click` and it was stealing the click from Elm's `Mouse.click`. Using `Mouse.isDown` instead solves this. I also fixed the quirk of having to compute the offset -- using `Elm.fullscreen` was the eventual solution to encompassing the whole editor `<div>` with click logging.

From that point on, much of the remaining modifications involved fitting JavaScript as a supported language, into the existing IDE `Editor.hs`, `Server.hs` and `Generate.hs` code (See \ref{Editor.hs}, \ref{Server.hs} and \ref{Generate.hs} respectively). It is, in my opinion, the least elegant part of the implementation. In order to toggle whether we are using the JavaScript interpreter or the Elm compiler, I pass around a `type Lang = Elm | Javascript`. An example from `Editor.js` (See \ref{Editor.hs}) is given below:

~~~~~~~~~~~~{.haskell .numberLines}

ide :: Lang -> String -> String -> FilePath -> String -> Html
ide lang cols participant fileName code =
    case lang of
         Javascript -> buildIde ("JS Editor: ", "/_compile")
         Elm        -> buildIde ("Elm Editor: ", "/compile")
  where buildIde (editStr,compileStr) =
            ideBuilder lang
                       cols
                       participant
                       (editStr ++ FP.takeBaseName fileName)
                       fileName
                       (compileStr ++"?input=" ++ urlEncode code)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I have a case block `case lang of Javascript -> ... Elm -> ...` which passes a different title and compile URL route to `buildIde` depending on the type of `lang`

If further languages need support, these case blocks (there are 2 in the codebase) would grow linearly, which is awkward and violates the software engineering principle of *Don't Repeat Yourself*.

Refinements in later iterations of the codebase included:

1. Allowing Elm code to be read from a file into `Editor.hs`
2. Modifying the stamps example to be bare
3. Modifying the stamps example to define regions of code to be logged for
   input
4. Visualising the mouse data straight from Firebase in the Elm 0.12 version
5. Writing python scripts to interpret the captured mouse 

Which were all relatively straightforward by then.

## Designing a task in JavaScript and Elm

In order to minimise the effect of the length of Source Lines Of Code [SLOC] on task difficulty, and to incorporate the 2 by 2 regions --- (Hard, Easy) x (Task--relevant, Task--irrelevant) --- I had to be very careful about how the Elm and Javascript tasks were presented. Pixi.js (http://www.pixijs.com/)  allows for a relatively similar (in API difficulty and function call SLOC), but still imperative paradigm for manipulating objects on a canvas, so this was the library of choice for the JS version of the task. The Elm version of the moving box task pre-existed on Elm-lang.org, and needed some minor adjustments to approach similarity with the Javascript version. This is the *"task juggling"* I predicted would be necessary in order to reach an acceptable equilibrium between the Elm and Javascript tasks.

