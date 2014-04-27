# Literature Survey

## Introduction to the problem area

<!--
Spending half an hour making a mind-map, starting with the word "Elm", I
get the following terms:

-   Signals

    -   Impure

    -   Time

    -   Input/Output

    -   History – Past & Future

    -   Data model

-   "Natural" Separation of Data model/Logic/Layout

-   Less cognitive load?

-   "Easy"

    -   Professed to be "easy" by the creator!

    -   What does it mean to be "easy"?

    -   Operationalisation

    -   Self-reporting

-   Programming Language

    -   Reactive

        -   "Instant feedback"

        -   "I/O-sensitive"

        -   Thrashing?

    -   Functional

        -   Pure

        -   Testing

            -   properties

            -   QuickCheck

            -   Delta Debugging

    -   Haskell derivative

        -   Embedded Domain-specific Language

        -   See also: Idris

    -   Multiple back-ends

        -   Javascript

        -   Java

        -   C

-   Programming

    -   code-compile-run loop

        -   "Programming blind"

        -   "Slow feedback"

    -   Text editor

    -   IDE

    -   Paradigms

        -   Declarative

        -   I'll tell you WHAT I want you to do, you figure out HOW to
            do it

    -   Encoding ideas

        -   Mapping of natural language concepts into formal language

        -   Loss of information?

        -   Augmented reality?

    -   Abstraction

        -   What level?

        -   What generation? 3GL? 4GL?

        -   Metaprogramming e.g. AspectJ

        -   Black/White box

    -   Cognitive offloading

    -   What is it?

        -   Art?

        -   Engineering?

        -   Science?

        -   Language?

        -   Mathematics?

        -   Ephemeral

        -   Intangible

        -   Limitless

        -   Limited

-   IDE

    -   Split View: Code | Runtime

-   Feedback

    -   "Instant-update"

    -   On-the-fly
-->

The problem area of user-interface programming, and more generally, the
activity of programming in a context such as a software engineering
environment, encompasses certain realms of interest. Through my survey
of literature, my research has touched upon the above-mentioned terms,
and I have discovered some thought-provoking problems that exist in the
field of programming. The concept of ‘Programming' embodies other
concepts – art-forms, engineering processes, science, language, and
mathematics, among others. To me, programming is a creative endeavour
unlike any other – in which the programmer wields materials of no
substance – the code – by manipulating symbols on a screen, which
represent states in the machine being used. There are so many
programming languages, and all languages (all that are Turing-complete)
reduce to the same language – that of a Turing Machine. So, *why do we
have so many programming languages?*.

*Beware of the Turing tar-pit in which everything is possible but
nothing of interest is easy.* [@PerlisTuringTarpit]

Different languages lend themselves to different ways of thinking about
problems. They may place emphasis on one feature, for example list
manipulation and hide others such as types. The language or programming
environment may make explicit the effect of changes as they are encoded,
as opposed to queuing up a block of changes and the programmer having to
initiate an update manually.

I would like to draw your attention in particular to the terms
**Abstraction**, **Cognitive offloading**, **Feedback**, **Loss of
information?**/**Augmented reality?**, **Thrashing**, and **"Programming
blind"**. These, at current, are my topics of interest, and my
literature review has up to this point been inextricably and heavily
influenced by this.

## What does it mean to be 'easy to use?'

In the process of surveying relevant (and sometimes irrelevant)
literature to this dissertation, recurring conceptual patterns were
observed – one particular instance of this is that several authors seem
to lay victim to the trap of claiming their creation is "easy to use",
"better", "simpler than $x$" without providing any supportive evidence
of this.

Perhaps these are incidents of ‘experimenter bias' – where the evaluator
is naturally predisposed to a positive appraisal of their own findings.
One way to avoid this is to have one set of people perform the data
capture and another set perform the data analysis. Nevertheless, these
patterns emerge, and present numerous opportunities for experimentation
and subsequent evidence supporting or contradicting these claims.
Experiments may see if the same conclusions are reached as the
above-mentioned authors, accounting for the ‘evaluator effect'
[@Hertzum2001].

Whether this particular route is taken for experimentation hinges on
pilot studies that will be conducted concurrently to the Literature
Survey, each inextricably shaping the other's direction of investigation
and inquiry.

The catalyst to this whole dissertation was a talk about the concept of
a highly reactive development environment – where changes in the code
result in instantaneous updates to the runtime, ‘on-the-fly'. This was
presented in Bret Victor's "Inventing on Principle" [@Victor2012a].
In his presentation Bret makes several assertions about the
‘traditional' style of coding, one statement of which is that "most of
the developer's time is spent looking at the code, blindly without an
immediate connection to the thing they're making". He argues that "so
much of creation is discovery, and you can't discover anything if you
can't see what you're doing" – alluding to his earlier statement that
the compile-run-debug cycle is much like this.

Evan Czaplicki, in his thesis of which Elm is the product
[@Czaplicki2012elm], makes similar claims – "[Elm] makes it *quick
and easy* to create and combine text, images, and video into rich
multimedia displays." While the evaluation of Elm's usability is not the
focus of the thesis, rather, it is to establish a context for Functional
Reactive Programming and describe the implementation details, he makes
other usability claims without evidence – "[non-declarative frameworks
for graphical user interfaces] mire programmers in the many small,
nonessential details of handling user input and modifying the display.",
"FRP makes GUI programming much more manageable", and in a section
entitled *The Benefits of Functional GUIs*, "In Elm, divisions between
data code, display code, and user interaction code arise fairly
naturally, helping programmers write robust GUI code". If these claims
are true, there is all the more evidence that Elm should be a language
of choice for GUI programmers, but experiments must be done to determine
this.

And perhaps this rapid development cycle is not always suitable – in
their 2012 paper, Lopez et al. (an inspiring paper that provides foundational
research into the behaviour of software developers while programming) show that
novices tend to "thrash" about, trying out many ideas that may or may not be a
solution, and executing "poorly directed, ineffective problem solving …failing
to realise they are doing it in good time, and fail to break out of it", whereas
experts think much more about the problem at hand before proceeding with a
solution [@Lopez2012a].

## Running User Studies

Perhaps a further direction of investigation may be running an
experiment to spot whether or not Elm's auto-updating IDE lends to a
lack of critical thinking – some operationalization may be *pauses
reported as ‘thinking' made during development* – where a pause is
disambiguated as ‘thinking' by the experimenter asking the participant
why they did not perform any interaction with the computer for more than
10 seconds, and the participant reports that they were
planning/designing/other similar activity. Along this line of thinking,
a paper studying the relationship between speech pauses and cognitive
load [@Khawaja2008] found through studying 48 mixed gender
participants that there is statistically significant indicators of
cognitive load through analysing pauses in speech. Perhaps this concept
of pauses can be applied to the activity of programming. However, the
planned method of disambiguating pauses via self-reporting (previously
mentioned) would not be suitable according to these authors – "such
measures can be either physically or psychologically intrusive and
disrupt the normal flow of the interaction", although a paper cited by
[@Khawaja2008] itself claims that "although self-ratings may appear
questionable, it has been demonstrated that people are quite capable of
giving a numerical indication of their perceived mental burden
[@Gopher1984]". Indeed a pilot study by Fraser and Kölling
[@McKay2012] structures the self-reporting by getting the users to
evaluate an IDE as they use it using a set of subject-specific
heuristics that they have designed. They showed that this customised set
of heuristics helped guide the user more effectively than Nielsen's
heuristics in evaluating usability, so one could develop a custom set of
heuristics for evaluating the usability of Elm.

From the Elm thesis [@Czaplicki2012elm], the language syntax and
rapid feedback seem simple enough that it is conceivable (or at the very
least, possible and of experimental interest) to allow the user to
customise the UI layout to their liking. Letting the user shape the UI
in concert with a UI programmer is covered the study of the interface
development environment "Mobi-D" in millitary and medical applications
[@Puerta1997], with success in those fields. It may be worth
speculating how Elm would fit into the development cycle that Puerta's
paper outlines, as this may lend inspiration to potential user interface
enhancements to the Elm IDE for A/B testing. It must be noted that there
does not seem to be a re-emergence of Mobi-D since the paper was
written, however.

My goal is to answer these questions. By way of conducting user studies,
leveraging Elm with extensions to do A/B testing to illustrate it's
effectiveness (or ineffectiveness) at enhancing User Interface Design.

Central to this idea of iteration is my desired method of performing
user studies: I will first do what I have called a "Pilot" – a short and
shallow trial User Study that focuses not on the research I'm concerned
with, but instead the particular experimental design I would like to use
in my actual User Study. By employing a Pilot I can hopefully get an
idea of the nature of the experimental design – perhaps discovering any
variables I had not previously considered that will require me to
increase my sample size or simplify the experiment in order to mitigate
their effect on the dependent variable I wish to test for. These are all
problems discovered in [@Yates2012a] – including basic teething
problems in getting the experiment to flow smoothly. In an even less
detailed aspect, the pilot may allow me to look at what is out there. It
may help to not look for anything in particular initially, and see what
happens.

At this stage, with the help of discussion with my Project Supervisor, I
have some ideas about how to gather data in User Studies and these
pilots could prove to be a useful testbed for such tools. I have a
hypothesis that the novice developer "thrashing" [@Lopez2012a] can
be observed by shorter pauses between editing and experimentation, and I
could measure this by way of measuring the mouse position relative to
the IDE, clicks, and key-presses, using tools built-in to Elm and a bit
of extension to stream this over the Internet to my storage facilities
[@WhatFRP].

