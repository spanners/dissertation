# System Architecture \label{Design}

**This is the chapter in which you review your design decisions at various levels
and critique the design process.**

Before starting implementation, the design phase (more appropriately titled
"System Architecture, here) was an important step in the creation of the
eventual IDE, as it set the direction in which the implementation began. In
designing the extensions I wanted to make to the pre--existing Elm IDE, I first
had to see if it was feasible. I devised a architectural diagram illustrating
the control flow of clicking the *Compile* button in the editor (See Figure
\ref{fig:compile-diagram}) to get an idea of where the extensions might fit.

1. On pressing the *Compile* button, `resources/misc/editor.js:compile()`
   submits the input form (the editor code pane), performing a `POST` of the
   source code in the editor code pane to the URL `/compile?input="<source
   code here>"` --- the source code is passed as a raw string in the `input` parameter

2. The Server running the IDE receives this POST event and looks up the function
   mapping associated with the `/compile` route. In this case it is the
   `compile` function, which in turn applies the function `serveHtml to result
   of `Generate.getHtmlPage getParam "input"` --- more on this next.

3. Inside `Generate.hs`, `Elm.compile` is applied to the Elm source code string, which
   lexes, parses, and generates the Javascript source, returning it to the `getHtmlPage` function.

4. From then on the Html page structure of the runtime is built back up again,
   resulting, in this case, in the MovingBox example task.

![Control flow of a compile in the Elm IDE\label{fig:compile-diagram}](images/compile_diagram.png)


![Extensions to be made to the IDE\label{fig:ide-extensions}](images/ide-extensions.png)
