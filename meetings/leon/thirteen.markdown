# Tue Apr  1 14:30:00 BST 2014

Discussed progress made and what hypotheses to form that may usefully model
cognitive load.

## Progress since last meeting

![Extensions made to the Elm IDE]

I have implemented full-screen mouse tracking that stores to a database a tuple:

    (t, (x, y))

for every mouse move, producing a list in JSON (so it's more like 
`{{uniq-userid: {125125, (67, 321)}}, {uniq-userid: {125126, (67, 322)}} ...}`)

I am ready to demo this (See Action 1.)

The only issue worth tweaking is that user activity data is captured separately
from the error output, so I will need to collate the data afterwards or find
some way to feed it into the same data store.

## Meeting Discussion

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


# Actions

1. Refine Mouse logging 

    1. *DONE* Make it so that I can run arbitrary Elm code in the editor via a fileOpen
       operation 
    2. *DONE* Make an Elm file that logs mouse movements ready to be loaded into the Editor
    3. *DONE* Load it into the editor and test it uploads to Firebase
    4. *DONE* Modify [Generator.hs](https://github.com/spanners/elm-lang.org/blob/master/server/Generator.hs)
    
            case (Elm.compile elmSrc) of 
                Left jsSrc -> ...
                Right _ -> error "blah"
    
       So that when we get an error, we timestamp and append it to a log file so
       this can later be collated with the Firebase to determine when errors were
       made

       I'll need to insert a layer between `compile :: Snap()` and `serveHtml ::
       MonadSnap m => H.Html -> m ()` that performs the logging. It will have
       type signature `TypedHtml -> 

       [compile](https://github.com/spanners/elm-lang.org/blob/master/server/Server.hs#L81)
       [serveHtml](https://github.com/spanners/elm-lang.org/blob/master/server/Server.hs#L69)


    5. Make it so I can define regions in the mouse tracking -- i.e. ONLY within a
       defined region is the mouse movement tracked e.g. `if mouse(x,y) in
       some2by2Square then Just mouse(x,y) else Nothing`

       See https://github.com/spanners/laska/blob/master/Signals.elm

2. Demo to Hilary Johnson
    1. Install on VPS (See [build script](../../install_elm.sh))
    2. Run these:      

            git clone https://github.com/spanners/elm-lang.org
            cd elm-lang.org
            cabal install --bindir=.

3. *DONE* Design a task in JS and Elm

4. Define regions to select for logging activity. Why? Because:
    * Complex logic in code, OR
    * Relevant to task
    * Captures Thrash (keep on going over the same thing, e.g.). Errors made
      also captures thrash!

5. *DONE* Determine what to do with mouse (for example) data.

[Extensions made to the Elm IDE]: ide-extensions.png

