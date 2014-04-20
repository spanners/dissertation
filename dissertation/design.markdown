# Design

**This is the chapter in which you review your design decisions at various levels
and critique the design process.**

More detail on what I will modify. How will I modify?

## Experimental Design

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

[Extensions made to the Elm IDE]: ide-extensions.png

What makes code difficult to understand and work with?

* Bit twiddling?
* Declaring and defining simultaneously?
* Compound if/then/else statements?

*[Programming is] manipulating symbols blindly* ~ Bret Victor

Do a 2×2 study, defining regions in the code monitoring mouse clicks. Regions
can either be simple/hard in complexity (exhibiting/not-exhibiting one of the
above 'difficult' properties). Or code can be task-oriented or not, that is *the
code does/does not need to be changed to achieve the completed task set for the
user*:


--------------- ------------------
**Elm**         -

Simple/Task     Hard/Task

Simple/Not-Task Hard/Not-Task

**JavaScript**  -

Simple/Task     Hard/Task

Simple/Not-Task Hard/Not-Task
--------------- ------------------

: 2 × 2 study between-subjects \label{tab:2x2study}

#### Study method

Look at total and/or mean time in each of these areas for comparison.

My study will be **between-subjects** instead of within-subjects.

That is, I will study *different users* for different languages. If a user has
completed the task in Elm, I can not have them complete the task in JavaScript,
and vice-versa.

I will necessarily make a compromise here:

Between-subjects:

* I lose the ability to keep programmer competence as constant, thus it is a
  confounding variable

* I gain the ability to ignore learned-experience in completing the task -- the
  participant is different every time so will not have done this task before,
  thus this is not a confounding variable.

Within-subjects is the converse of the above methodological properties

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

![Extensions made to the Elm IDE]
