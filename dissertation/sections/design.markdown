# Design

**This is the chapter in which you review your design decisions at various levels
and critique the design process.**

## Sun Mar 30 00:49:51 GMT 2014

###  What did I get done?

So today involved a lot of hacking around, trying to determine how to get
constantly updating user input signals to stream via a `port` from Elm to
JavaScript, so that I can store `Mouse.position` and `Keyboard.keysDown` it in a
DB backend.

I found an example that is close to what I want to achieve and forked the
repository. It lives at http://github.com/spanners/elm-lib

It is nice and lightweight, and I have been working on applying these ideas to
mouse input. See the [palindrome Elm example]() for input signals. [Source here]().

*However*, there are two disadvantages with this approach:

1. It uses an old Elm FFI -- instead of using `port`s it uses `foreign import
   jsevent`. 

     Look at how Evan does this in his elm-js-and-html example. I have forked
     this at http://github.com/spanners/elm-js-and-html

2. The second disadvantage is that it uses [Firebase][] as a
   backend for data storage. 
   
     This is a proprietary application. I would much rather use something Open
     Source that I can host myself for two reasons: Firstly for data protection
     reasons. This shouldn't be a problem due to the fact that it is mouse (x,y)
     co-ordinates and timestamps. Secondly because it has a hard-limit on the
     amount of data you can store on the FREE plan: 100MB. Again, shouldn't be a
     problem so long as I filter the input intelligently.

[palindrome Elm example]: http://elm-lang.org/edit/examples/Intermediate/TextReverse.elm
[Source here]: https://github.com/elm-lang/elm-lang.org/blob/e53b8b873d0e5840bf31af2051f5cc147728ae4f/public/examples/Intermediate/TextReverse.elm

### What do I hope to do tomorrow?

* Focus more on hacking on the actual Editor.hs generated HTML. 

    I will almost certainly need to have an output `port` from Elm
    `Mouse.position` into JS in order to give Firebase something to store.

* Use `Elm.worker(Main.Elm, div, {})` and make the div somehow encompass the
  entire CodeMirror window...


## Mon Mar 31 23:42:00 BST 2014

### What did I get done?

* This: http://github.com/spanners/elm-html-and-js !

I managed to get [Evan's stamps example]() example working with [Firebase]().
Now I can successfully store user mouse events persistently in a JSON file.

This required dealing with **Disadvantage 1.** ([See entry 1]()), which I did
successfully, using the new `port` FFI :)

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


### What do I hope to do tomorrow?

* Use this inside the Elm `Editor.hs` IDE.

    This should be very straightforward.

    1. Allow Elm code to be read from a file into `Editor.hs`
    2. Modify the stamps example to be bare
    3. Modify the stamps example to define regions of code to be logged for
       input

* Design a task in JavaScript

* Get the Elm IDE to optionally interpret JavaScript instead of compiling Elm
  (limit this so only I, the experimenter, can do this)

[Evan's stamps example]: http://github.com/evancz/elm-html-and-js
[Firebase]: http://firebase.io
[See entry]: #sun-mar-30-00:49:51-gmt-2014


## Fri Apr 18 03:09:18 BST 2014

It would be so nice if I could point the Elm IDE to mouse data .json
and view it in an overlay in the code. This is (theoretically) very easy.

* * *

So ok. I only know how to get this working in Elm-runtime-0.10
but basically, I can just swap where I'm doing a POST to submit mouseclick data, with GET, and magic.

Annoyingly I haven't been able to get this to work with the latest runtime (0.12)
but hax allows me to enter a special "View participant mouse data" mode into the IDE
e.g. specify some url path in addition to the experiment
and the participant ID
and then it loads elm-runtime-0.10.js instead
and does GET instead of POST on the same data

:)

More detail on what I will modify. How will I modify?

## Experimental Design

Discussed progress made and what hypotheses to form that may usefully model
cognitive load.

![Extensions to be made to the Elm IDE]

I have implemented full-screen mouse tracking that stores to a database a tuple:

    (t, (x, y))

for every mouse move, producing a list in JSON (so it's more like 
`{{uniq-userid: {125125, (67, 321)}}, {uniq-userid: {125126, (67, 322)}} ...}`)

I am ready to demo this (See Action 1.)

The only issue worth tweaking is that user activity data is captured separately
from the error output, so I will need to collate the data afterwards or find
some way to feed it into the same data store.

### Meeting Discussion

2 Hypotheses

1. Why the regions (*see green boxes in figure above*) I define in the code (to
   mouse-track e.g.) are meaningful

2. Frequency of semantically or syntactically incorrect errors made will differ
   as a function of the language under study

These need narrowing as they are too broad to test. Explode them into multiple,
tighter hypotheses.

They are *valid* because they are *well-founded* -- i.e. I have good reason to
believe that # of errors made is an indication of cognitive load. I have good
reason to believe that the selected regions will have more mouse activity (or
whatever activity I suspect indicates higher cognitive load) as they are harder
regions of code OR they pertain to achieving the set task.


## Actions

1. Refine Mouse logging 

    1. **DONE** Make it so that I can run arbitrary Elm code in the editor via a
       fileOpen operation 
    2. **DONE** Make an Elm file that logs mouse movements ready to be loaded
       into the editor (See \ref{editor.hs})
    3. **DONE** Load it into the editor and test it uploads to Firebase
    4. **DONE** Modify Generate.hs (See \ref{generate.hs})
    
    ~~~~~~~~~ {.haskell .numberLines}
    case (Elm.compile elmSrc) of 
        Left jsSrc -> ...
        Right _ -> error "blah"
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
       So that when we get an error, we timestamp and append it to a log file so
       this can later be collated with the Firebase to determine when errors were
       made

       I'll need to insert a layer between `compile :: Snap()` and `serveHtml ::
       MonadSnap m => H.Html -> m ()` that performs the logging. It will have
       type signature `TypedHtml -> H.Html`

       See the functions `compile` and `serveHtml` in Server.hs (See
       \ref{server.hs}). 


    5. Make it so I can define regions in the mouse tracking -- i.e. ONLY within a
       defined region is the mouse movement tracked e.g. `if mouse(x,y) in
       some2by2Square then Just mouse(x,y) else Nothing`

       See [https://github.com/spanners/laska/blob/master/Signals.elm]()

2. **DONE** Demo to supervisor
    1. Install on VPS (See \ref{install-elm.sh})
    2. Run these:      

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.bash .numberLines}
    git clone https://github.com/spanners/elm-lang.org
    cd elm-lang.org
    cabal install --bindir=.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

3. **DONE** Design a task in JS and Elm

4. Define regions to select for logging activity. Why? Because:
    * Complex logic in code, OR
    * Relevant to task
    * Captures Thrash (keep on going over the same thing, e.g.). Errors made
      also captures thrash!

5. **DONE** Determine what to do with mouse (for example) data.


What makes code difficult to understand and work with?

* Bit twiddling?
* Declaring and defining simultaneously?
* Compound if/then/else statements?

*[Programming is] manipulating symbols blindly* ~ Bret Victor

#### Actions

1. **DONE** Reorder divs so embedded div is on top of editor div.

    This turned out (I am fairly certain) to be due to codemirror.js binding
    mouse clicks. It was solved by using Elm's `Mouse.isDown`. Using
    `Mouse.isDown` has the added benefit of tracking mouse selects and drags,
    because it logs `(x,y)` when the mouse is down and `(x,y)` again when it is up.

2. **DONE** Create a task that features *Hard/Simple x Task/Not-task* (See
   \ref{tab:2x2study})

3. ~~Implement *Region filtering* functionality so mouse activity is only logged
   when the clicks occur within defined region(s)~~

    I have instead defined bounding boxes that pertain to the regions I want to
    track as a mouse-data filter -- that is, I capture all click data for the
    whole frame, and then filter it by comparing x,y co-ordinates with my
    bounding boxes. If it's in the box, keep it, otherwise discard.

4. **DONE** Integrate JS task into IDE

5. **DONE** Perform pilot study

6. **WIP** Visualise mouse data

[Extensions to be made to the Elm IDE]: images/ide-extensions.png
