% The potential of declarative programming languages to support user interface programming: the case of ELM
% Simon Buist
% March 2014

---
title:  'The potential of declarative programming languages to support user interface programming: the case of ELM'
author:
- name: Simon Buist
  affiliation: University of Bath
tags: [programming, psychology, HCI, user experience]
abstract: |
  Your abstract should appear here. An abstract is a short paragraph describing the aims of
  the project, what was achieved and what contributions it has made.

  It consists of two paragraphs.
...

[table]: #table

\newpage
This dissertation may be made available for consultation within the University
Library and may be photocopied or lent to other libraries for the purposes of
consultation.

Signed:
\newpage

**The potential of declarative programming languages to support user interface programming: the case of ELM**

Submitted by: Simon Buist

# COPYRIGHT {-} 

Attention is drawn to the fact that copyright of this dissertation rests with its author. The
Intellectual Property Rights of the products produced as part of the project belong to the
author unless otherwise specified below, in accordance with the University of Bath’s policy
on intellectual property (see http://www.bath.ac.uk/ordinances/22.pdf).
This copy of the dissertation has been supplied on condition that anyone who consults it
is understood to recognise that its copyright rests with its author and that no quotation
from the dissertation and no information derived from it may be published without the
prior written consent of the author.

# Declaration {-}

This dissertation is submitted to the University of Bath in accordance with the requirements
of the degree of Bachelor of Science in the Department of Computer Science. No portion of
the work in this dissertation has been submitted in support of an application for any other
degree or qualification of this or any other university or institution of learning. Except
where specifically acknowledged, it is the work of the author.

Signed:
\newpage

# Acknowledgements {-}

Add any acknowledgements here.
\newpage

# Introduction

This is the introductory chapter.

## Example Section

Like all chapters, it will have a number of sections

### Example Subsection

...and sub-sections

#### Example sub-subsection
...and sub-subsections.

#### Table

: An example table

-------- --------
Items    Values
-------- --------
Item 1   Value 1

Item 2   Value 2
-------- --------

## Another section

Another section, just for good measure. You can reference a table, figure or equation using
[table][]

# Literature Survey

## Introduction to the problem area

Spending half an hour making a mind-map, starting with the word “Elm”, I
get the following terms:

-   Signals

    -   Impure

    -   Time

    -   Input/Output

    -   History – Past & Future

    -   Data model

-   “Natural” Separation of Data model/Logic/Layout

-   Less cognitive load?

-   “Easy”

    -   Professed to be “easy” by the creator!

    -   What does it mean to be “easy”?

    -   Operationalisation

    -   Self-reporting

-   Programming Language

    -   Reactive

        -   “Instant feedback”

        -   “I/O-sensitive”

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

        -   “Programming blind”

        -   “Slow feedback”

    -   Text editor

    -   IDE

    -   Paradigms

        -   Declarative

        -   I’ll tell you WHAT I want you to do, you figure out HOW to
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


The problem area of user-interface programming, and more generally, the
activity of programming in a context such as a software engineering
environment, encompasses certain realms of interest. Through my survey
of literature, my research has touched upon the above-mentioned terms,
and I have discovered some thought-provoking problems that exist in the
field of programming. The concept of ‘Programming’ embodies other
concepts – art-forms, engineering processes, science, language, and
mathematics, among others. To me, programming is a creative endeavour
unlike any other – in which the programmer weilds materials of no
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
information?**/**Augmented reality?**, **Thrashing**, and **“Programming
blind”**. These, at current, are my topics of interest, and my
literature review has up to this point been inextricably and heavily
influenced by this.

Under the umbrella quote of "Don’t program blind" – tools that help the
programmer see what he’s doing:

From: [Don’t Learn to Code, Learn to Program – But come back in 10
years](http://johnkurkowski.com/posts/dont-learn-to-code-learn-to-program-but-come-back-in-10-years/):

Quite a sad title really. And sad content. BUT it does link to some wise
words from Jeff Atwood, Peter Norvig ("Learn to program in 10 years!"),
and has a handful of links to different projects and a nice quote:

* If builders built buildings the way programmers wrote programs, then
the first woodpecker that came along would destroy civilization. * –
Gerald M. Weinberg, The Psychology of Computer Programming (1971)

[Out in the open: These Hackers want to give
youSuperpowers](http://www.wired.com/wiredenterprise/2014/01/light-table/)

“We consider programming a modern-day superpower. You can create
something out of nothing, cure cancer, build billion-dollar companies,”
he says. “We’re looking at how we can give that super power to everyone
else.”

The problem with coding, he says, is that you can’t see the results of
your work until after you’re done. In that sense, programming is unlike
almost every other craft. “When a chef adds an ingredient, he can smell
it, he can taste it,” Granger says. “When an artist makes a stroke on a
canvas, he can see it. But programming isn’t that way.”

*‘We consider programming a modern-day superpower. You can create
something out of nothing, cure cancer, build billion-dollar companies.’*

— Chris Granger

Programmers may spend hours or days working on code before they can make
sure it actually works. “We have to play computer in our heads,” he
says. “We write each line, imagine what it will do. You have to act like
a computer. The problem with that is that we’re pretty crappy
computers.” But Light Table seeks to bridge that gap.

Light Table is an open source programming tool that lets programmers see
the results of their code as their write it. It’s not an entirely new
idea. In the mid-1960s, an educational tool called Logo gave programming
students immediate feedback. More recently, languages like the kid
friendly Scratch and artist friendly Processing have offered a kind of
visual feedback, giving coders more insight into their programs as
they’re written.

But applying those ideas to professional software — complex applications
with thousands or even millions of lines of code — is another matter.
Light Table tackles such software by not only by displaying the results
of the code you’re working on right now, but by showing how it relates
to other parts of your software and how data flow from one chunk of code
to another. It also weaves documentation throughout the code, while
offering new ways to organize and visualize the code in any application.

**Flow Based Programming**

* [NoFlo](http://noflojs.org/)
* [Flowhub.io](http://flowhub.io/)
* [J. Paul Morrison](http://www.jpaulmorrison.com/fbp/) – Flow Based Programming
* [Example of NoFlo implementation of Jekyll](https://github.com/the-grid/noflo-jekyll)

## What does it mean to be ‘easy to use?’

In the process of surveying relevant (and sometimes irrelevant)
literature to this dissertation, recurring conceptual patterns were
observed – one particular instance of this is that several authors seem
to lay victim to the trap of claiming their creation is “easy to use”,
“better”, “simpler than $x$” without providing any supportive evidence
of this.

Perhaps these are incidents of ‘experimenter bias’ – where the evaluator
is naturally predisposed to a positive appraisal of their own findings.
One way to avoid this is to have one set of people perform the data
capture and another set perform the data analysis. Nevertheless, these
patterns emerge, and present numerous opportunities for experimentation
and subsequent evidence supporting or contradicting these claims.
Experiments may see if the same conclusions are reached as the
above-mentioned authors, accounting for the ‘evaluator effect’
[@Hertzum2001].

Whether this particular route is taken for experimentation hinges on
pilot studies that will be conducted concurrently to the Literature
Survey, each inextricably shaping the other’s direction of investigation
and inquiry.

The catalyst to this whole dissertation was a talk about the concept of
a highly reactive development environment – where changes in the code
result in instantaneous updates to the runtime, ‘on-the-fly’. This was
presented in Bret Victor’s “Inventing on Principle” [@Victor2012a].
In his presentation Bret makes several assertions about the
‘traditional’ style of coding, one statement of which is that “most of
the developer’s time is spent looking at the code, blindly without an
immediate connection to the thing they’re making”. He argues that “so
much of creation is discovery, and you can’t discover anything if you
can’t see what you’re doing” – alluding to his earlier statement that
the compile-run-debug cycle is much like this.

Evan Czaplicki, in his thesis of which Elm is the product
[@Czaplicki2012elm], makes similar claims – "[Elm] makes it *quick
and easy* to create and combine text, images, and video into rich
multimedia displays." While the evaluation of Elm’s usability is not the
focus of the thesis, rather, it is to establish a context for Functional
Reactive Programming and describe the implementation details, he makes
other usability claims without evidence – “[non-declarative frameworks
for graphical user interfaces] mire programmers in the many small,
nonessential details of handling user input and modifying the display.”,
“FRP makes GUI programming much more manageable”, and in a section
entitled *The Benefits of Functional GUIs*, “In Elm, divisions between
data code, display code, and user interaction code arise fairly
naturally, helping programmers write robust GUI code”. If these claims
are true, there is all the more evidence that Elm should be a language
of choice for GUI programmers, but experiments must be done to determine
this.

And perhaps this rapid development cycle is not always suitable – in
their 2012 paper, Lopez et al. show that novices tend to “thrash” about,
trying out many ideas that may or may not be a solution, and executing
“poorly directed, ineffective problem solving …failing to realise they
are doing it in good time, and fail to break out of it”, whereas experts
think much more about the problem at hand before proceeding with a
solution [@Lopez2012a].

## Running User Studies

Perhaps a further direction of investigation may be running an
experiment to spot whether or not Elm’s auto-updating IDE lends to a
lack of critical thinking – some operationalization may be *pauses
reported as ‘thinking’ made during development* – where a pause is
disambiguated as ‘thinking’ by the experimenter asking the participant
why they did not perform any interaction with the computer for more than
10 seconds, and the participant reports that they were
planning/designing/other similar activity. Along this line of thinking,
a paper studying the relationship between speech pauses and cognitive
load [@Khawaja2008] found through studying 48 mixed gender
participants that there is statistically significant indicators of
cognitive load through analysing pauses in speech. Perhaps this concept
of pauses can be applied to the activity of programming. However, the
planned method of disambiguating pauses via self-reporting (previously
mentioned) would not be suitable according to these authors – “such
measures can be either physically or psychologically intrusive and
disrupt the normal flow of the interaction”, although a paper cited by
[@Khawaja2008] itself claims that “although self-ratings may appear
questionable, it has been demonstrated that people are quite capable of
giving a numerical indication of their perceived mental burden
[@Gopher1984]”. Indeed a pilot study by Fraser and Kölling
[@McKay2012] structures the self-reporting by getting the users to
evaluate an IDE as they use it using a set of subject-specific
heuristics that they have designed. They showed that this customised set
of heuristics helped guide the user more effectively than Nielsen’s
heuristics in evaluating usability, so one could develop a custom set of
heuristics for evaluating the usability of Elm.

From the Elm thesis [@Czaplicki2012elm], the language syntax and
rapid feedback seem simple enough that it is conceivable (or at the very
least, possible and of experimental interest) to allow the user to
customise the UI layout to their liking. Letting the user shape the UI
in concert with a UI programmer is covered the study of the interface
development environment “Mobi-D” in millitary and medical applications
[@Puerta1997], with success in those fields. It may be worth
speculating how Elm would fit into the development cycle that Puerta’s
paper outlines, as this may lend inspiration to potential user interface
enhancements to the Elm IDE for A/B testing. It must be noted that there
does not seem to be a re-emergence of Mobi-D since the paper was
written, however.

My goal is to answer these questions. By way of conducting user studies,
leveraging Elm with extensions to do A/B testing to illustrate it’s
effectiveness (or ineffectiveness) at enhancing User Interface Design.

Central to this idea of iteration is my desired method of performing
user studies: I will first do what I have called a “Pilot” – a short and
shallow trial User Study that focuses not on the research I’m concerned
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
hypothesis that the novice developer “thrashing” [@Lopez2012a] can
be observed by shorter pauses between editing and experimentation, and I
could measure this by way of measuring the mouse position relative to
the IDE, clicks, and key-presses, using tools built-in to Elm and a bit
of extension to stream this over the Internet to my storage facilities
[@WhatFRP].

# Requirements

If you are doing a primarily software development project, this is the chapter in which you
review the requirements decisions and critique the requirements process.

# Design

This is the chapter in which you review your design decisions at various levels and critique
the design process.


## Pilot study

In reflection, the task I chose was too difficult to capture the cognitive load incurred by
the language itself for a given task, due to the difficulty of the task itself creating noise.
I could improve this by simplifying the task, in a way that is ‘language agnostic’, i.e. that
is not idiomatic of Elm or JavaScript (the two languages that I am comparing).
Something like the following will never be that easy in JavaScript:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
-- | Inefficient quicksort in haskell.
qsort :: (Enum a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
               qsort (filter (>= x) xs) 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
main = lift asText Mouse.position
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Experimental design

Foo

# Implementation and Testing
This is the chapter in which you review the implementation and testing decisions and issues,
and critique these processes.
Code can be output inline using ````some code````. For example, this code is inline:
````public static int example = 0;```` (I have used the character | as a delimiter, but any
non-reserved character not in the code text can be used.)
Code snippets can be output using the environment with the code given in the environment. For example, consider listing 5.1, below.
Listing 5.1: Example code

Code listings are produced using the package “Listings”. This has many useful options, so
have a look at the package documentation for further ideas.

# Results

This is the chapter in which you review the outcomes, and critique the outcomes process.
You may include user evaluation here too.

# Conclusions

This is the chapter in which you review the major achievements in the light of your original
objectives, critique the process, critique your own learning and identify possible future
work.

# Bibliography {-}
