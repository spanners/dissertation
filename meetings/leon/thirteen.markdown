# Tue Apr  1 14:30:00 BST 2014

Discussed progress made and what hypotheses to form that may usefully model
cognitive load.

## Progress since last meeting

![Extensions made to the Elm IDE]

I have implemented full-screen mouse tracking that stores to a database a tuple:

    (t, (x, y))

in JSON (so it's more like 
`{{uniq-userid: {125125, (67, 321)}}, {uniq-userid: {125126, (67, 322)}} ...}`)
and this is awesome :)

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

1. Demo to Hilary Johnson
    1. Install on VPS (See [build script](install_elm.sh))
    2. Run these:      

            git clone https://github.com/spanners/elm-lang.org` 
            cd elm-lang.org`
            cabal install --bindir=.`

2. Design a task in JS and Elm

3. Define regions to select for logging activity. Why? Because:
    * Complex logic in code, OR
    * Relevant to task
    * Captures Thrash (keep on going over the same thing, e.g.). Errors made
      also captures thrash!

4. Determine what to do with mouse (for example) data.

[Extensions made to the Elm IDE]: ide-extensions.png
