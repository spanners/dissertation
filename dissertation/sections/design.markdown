# System Architecture \label{Design}

Before starting implementation, the design phase (more appropriately titled
System Architecture, here) was an important step in the creation of the
eventual IDE, as it set the direction in which the implementation began. In
designing the extensions I wanted to make to the pre--existing Elm IDE, I first
had to see if it was feasible. I devised a architectural diagram illustrating
the control flow of clicking the *Compile* button in the editor (See Figure
\ref{fig:compile-diagram}) to get an idea of where the extensions might fit. The
stages (1,2,3,4), labelled in the diagram, are described in more detail in the
list below.

1. On pressing the *Compile* button, `resources/misc/editor.js:compile()`
   submits the input form (the editor code pane), performing a `POST` of the
   source code in the editor code pane to the URL `/compile?input="<source
   code here>"` --- the source code is passed as a raw string in the `input` parameter

2. The Server running the IDE receives this POST event and looks up the function
   mapping associated with the `/compile` route. In this case it is the
   `compile` function, which in turn applies the function `serveHtml` to result
   of `Generate.getHtmlPage getParam "input"` --- more on this next.

3. Inside `Generate.hs`, `Elm.compile` is applied to the Elm source code string, which
   lexes, parses, and generates the Javascript source, returning it to the `getHtmlPage` function.

4. From then on the HTML page structure of the runtime is built back up again,
   resulting in the new runtime (in this case, the MovingBox program).

![Control flow of a compile in the Elm IDE\label{fig:compile-diagram}](images/compile_diagram.png)

My extensions that I planned to make at the design stage are loosely illustrated
in Figure \ref{fig:ide-extensions}.

![Extensions to be made to the IDE\label{fig:ide-extensions}](images/ide-extensions.png)

It became apparent that I would need to make several modifications at each stage
of the round--trip (1,2,3,4), and indeed heavily modify the code pane (left) in order to
incorporate the same compile loop *within* the code pane in order to embed
arbitrary Elm code. The embedded code would itself need to be created, and
hooked into a database back--end to perform the JSON click data storage functionality

* For Stage 1., I determined that I would probably need to send some extra
  information in the `POST` submission to `/compile`, perhaps using another
  parameter.

* For Stage 2., the `getHtmlPage` function in `Generate.hs` was predicted to need heavy
  modification, due to the fact that this is where the IDE builds the HTML page
  back up after compiling the Elm source. I proposed that I would need some way
  of differentiating between an *Embedded Elm* compile, and the *Elm IDE*
  (the pre--existing implementation) compile.

* I hoped that no modifications would be necessary at Stage 3., seeing as it is
  very difficult and time consuming to modify the compiler as, not only would
  several stages of the compilation process need modification, but the resulting
  generated Javascript and the Elm-runtime itself would need modifying. All
  I will be doing is compiling standard (although embedded) Elm code, so I
  suspected I could avoid this as long as I place my efforts on ensuring I write
  standard Elm.

I do note here that the design was not solely a sealed--off, Waterfall style
phase in the development process. Rather, I gathered an intuition for where a
feature might go, attempted to implement it, and possibly came back to rethink
the architecture. The next section describes the implementation details, issues
I came across in doing so, and a general commentary of the process.
