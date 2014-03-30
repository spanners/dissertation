# Sun Mar 30 00:49:51 GMT 2014

##  What did I get done?

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

2. The second disadvantage is that it uses [Firebase](http://firebase.io) as a
   backend for data storage. 
   
     This is a proprietary application. I would much rather use something Open
     Source that I can host myself for two reasons: Firstly for data protection
     reasons. This shouldn't be a problem due to the fact that it is mouse (x,y)
     co-ordinates and timestamps. Secondly because it has a hard-limit on the
     amount of data you can store on the FREE plan: 100MB. Again, shouldn't be a
     problem so long as I filter the input intelligently.

[palindrome Elm example]: http://elm-lang.org/edit/examples/Intermediate/TextReverse.elm
[Source here]: https://github.com/elm-lang/elm-lang.org/blob/e53b8b873d0e5840bf31af2051f5cc147728ae4f/public/examples/Intermediate/TextReverse.elm

## What do I hope to do tomorrow?

* Focus more on hacking on the actual Editor.hs generated HTML. 

    I will almost certainly need to have an output `port` from Elm
    `Mouse.position` into JS in order to give Firebase something to store.

* Use `Elm.worker(Main.Elm, div, {})` and make the div somehow encompass the
  entire CodeMirror window...
