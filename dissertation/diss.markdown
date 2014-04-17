---
title:  'Analysing cognitive load by extending an IDE to support input device logging of users during the activity of user--interface programming'
author:
- name: Simon Buist
  affiliation: The University of Bath
tags: [programming, psychology, HCI, user experience]
abstract: |
  This dissertation provides a browser-based Integrated Development Environment
  [IDE] that logs input device data --- for the purpose of performing user
  participation studies --- whose implementation is described herein. The IDE is
  then used to conduct studies comparing the cognitive load
  experienced with two languages, Elm and JavaScript, during the completion of a
  basic task: modifying code to restrict the movement of a 2--D box from leaving
  the bounds of a frame. In order to do this comparison, the metric of `number
  of mouse clicks per code region` is selected, as an operationalisation
  of the concept of *thrashing* [@Lopez2012a], as being indicative of cognitive
  load during task completion.  **The study found...**
...

# Introduction

<!--

I am interested in the effect of Functional Reactive Programming [FRP]
on User Interface programming.

I first grew an interest in the field of Functional Reactive Programming
after seeing Bret Victor's "Inventing on Principle" @Victor2012a. His
talk claims that, in the traditional compile-run-debug cycle of coding,
"most of the developer's time is spent looking at the code, blindly
without an immediate connection to the thing they're making". He goes on
to show a side-by-side illustration of a new style of development – on
one side is the runtime preview, and on the other side is the code
pertaining to said runtime. Changes in the code update the runtime,
live. He argues that "so much of creation is discovery, and you can’t
discover anything if you can't see what you're doing" – alluding to his
earlier statement that the compile-run-debug cycle is much like this. I
would like to investigate the claims Bret Victor makes, and indeed Elm,
an instance of such a FRP, whose website also makes similar claims.

A counter-argument may be that this is much like giving a child a
chainsaw. Is it too powerful? Does this tight feedback loop cut out a
perhaps crucial pause for thought? Furthermore – is this appropriate for
all types of programming? Is it at least appropriate for User Interface
design? It has been shown that novices tend to “thrash” about, trying
out many ideas that may or may not be a solution, whereas experts think
much more about the problem at hand before proceeding with a solution
@Lopez2012a.

My goal is to answer these questions. By way of conducting user studies,
leveraging Elm with extensions to do A/B testing to illustrate it’s
effectiveness (or ineffectiveness) at enhancing User Interface Design.

As far as the scope of this project goes, I will be researching as much as is
necessary in order to meet the aims of the project listed. Should I complete
these aims, I may go on to do further user studies, or attempt to further
analyse, compare and contrast User Interface Design and Declarative/Functional
Reactive Programming languages against other methods, so as to make firmer
statements about the benefits of Elm.

-->


# Project Plan

I will now explain my current plan for the project. Notice that I say
current here – this may change throughout the course of the project: I
may narrow in on a topic of interest, or branch out to investigate
anomalous research findings.

I will be building the end product – the dissertation and software – via
a process of iterations, much like an iterative Software Lifecycle. The
Literature Survey is ongoing – throughout the whole project from
beginning to end – feeding into all parts of the dissertation, and
indeed this Proposal, as shown in the Gantt chart (Figure  
\ref{fig:gantt}). The literature I choose is sometimes chosen to support
points I wish to make, sometimes acting to guide my next area of
research, reinforce findings, compare or contrast with other research,
and probably many other things I have not yet thought of. Most
importantly, I will be looking at who the paper/article etc. is cited
by, preferring sources that are peer-reviewed.

As well as this literature research, I will also have an ongoing Product
Literature Survey – looking at existing software out there that is
related to my current area of interest.

Central to this idea of iteration is my desired method of performing
user studies: I will first do what I have called a “Pilot” – a short and
shallow trial User Study that focuses not on the research I’m concerned
with, but instead the particular experimental design I would like to use
in my actual User Study. By employing a Pilot I can hopefully get an
idea of the nature of the experimental design – perhaps discovering any
variables I had not previously considered that will require me to
increase my sample size or simplify the experiment in order to mitigate
their effect on the dependent variable I wish to test for. These are all
problems discovered in @Yates2012a – including basic teething problems
in getting the experiment to flow smoothly. In an even less detailed
aspect, the pilot may allow me to look at what is out there. It may help
to not look for anything in particular initially, and see what happens.

At this stage, with the help of discussion with my Project Supervisor, I
have some ideas about how to gather data in User Studies and these
pilots could prove to be a useful testbed for such tools. I have a
hypothesis that the novice developer “thrashing” @Lopez2012a can be
observed by shorter pauses between editing and experimentation, and I
could measure this by way of measuring the mouse position relative to
the IDE, clicks, and key-presses, using tools built-in to Elm and a bit
of extension to stream this over the Internet to my storage facilities
@WhatFRP.

As you will see in the Gantt chart (Figure \ref{fig:gantt}) I have
included Testing & Implementation under the same heading as I will be
doing Test Driven Development. My experience on Placement at PicoChip,
my job as a Software Engineer at Altran and readings have helped me
realise that this way of developing is time-saving and improves code
quality by enforcing modularity in order to test it @Martin2008a and
@Hunt2000a.

![Gantt Chart\label{fig:gantt}](gantt-chart.png)

## Required Resources

I will now talk about the resources I require for the completion of this
dissertation, including the availability of these resources.

I will require users for my user study. These users must be proficient
in at least one programming language (declarative programming languages
are niche in and of themselves, never mind the discipline of
programming, so some basic knowledge is required in order to see useful
patterns in User Interface Design). Suitable candidates are First and
Second Year Computer Science students from most Universities in the UK.
Their availability is limited – Christmas holidays and coursework
deadlines may mean that certain periods of the term are particularly
busy for them. At Bath, suitable periods are therefore November, January
to Mid February (inclusive), Mid-March to April (inclusive). It will be
useful to procure free periods for other nearby Universities to hedge my
bets, and to have a decent random assignment of users so I can make
equivalent groups in my experiments.

The ACM Digital library, accessible via the Bath portal either from
University or from home via Single-sign-on is a valuable resource for
research papers, articles and references. The Cited-By feature will
allow me to assert the popularity/ranking of each resource. Another
valuable resource is the Psychology of Programming Interest Group, a
“[group of] people from diverse communities to explore common interests
in the psychological aspects of programming and in the computational
aspects of psychology”, with peer reviewed papers on particularly
relevant topics to my area of research.

I will require regular access to the Internet, Emacs with haskell-mode
installed and Elm version 0.10 @Elm2013a. I will also need git for
software source control, and bitbucket.org for online, private backups
of my work. I require LaTeX to type up my dissertation, and have chosen
texlive on Ubuntu 12.04.3 as my development environment of choice. The
full development environment is installed at the house I am staying in,
in Bath, on my laptop. I am also able to replicate this environment to a
satisfactory level at Bath University on any computer with access via
Putty/SSH or similar to LCPU, as all the above software can be installed
and run on my Bath University account.

I am using Chromium Version 28.0.1500.71 Ubuntu 12.04
(28.0.1500.71-0ubuntu1.12.04.1) to run the Elm IDE, which is an
important dependency that may cause problems in getting Users in User
Studies to run a functionally equivalent browser. Only recent editions
of Chrome, Chromium, Firefox, Opera and Safari (not Internet Explorer)
support Elm web programs.

# Ethical considerations

In conducting User Studies, I will be interacting with people and
collecting data from them, so I must be considerate and mindful of those
I talk to and the information I handle.

An Ethical Checklist such as the one Bath University uses as it’s
template @Bath2013a may assist my research such that I treat each
participant with care and respect. I may learn from the discoveries made
by others – in my reading, I came across a paper (also mentioned
earlier) that highlighted concerns that participants under study had,
and the paper detailed ways to mitigate these concerns so as to make the
participant feel that are informed and safe @Yates2012a.

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
their 2012 paper, Lopez et al. show that novices tend to "thrash" about,
trying out many ideas that may or may not be a solution, and executing
"poorly directed, ineffective problem solving …failing to realise they
are doing it in good time, and fail to break out of it", whereas experts
think much more about the problem at hand before proceeding with a
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


# Experimental methodology

## AB Testing of the languages with the same IDE?

The primary direction I mentioned (as echoed in my Proposal) was doing AB
testing of Elm vs. another language (e.g. JavaScript) (i.e. the language is the
dependent variable) using the same Concurrent FRP IDE (the independent variable).

## Test just the paradigm?

He also suggested a potential experiment to test just the paradigm, eliminating
the IDE from the experiment above. Perhaps for a Pilot study.

### Experiment process

1. Study question (e.g. Is it easy?)
2. Measurement concept (e.g. "Easy")
3. Operationalisation -- taking a measurement concept and mapping it to
   something concrete (e.g. if completing a pre-defined task the user must
   complete takes $< 5$ steps, it is 'easy' -- we can then compare instances of
   these studies given our definition of easy). This is much like mapping a
   design to an implementation, and there is a risk of losing information, or
   ending up with a mismatched concrete instance that does not represent the
   concept we wish to convey.
4. Do another operationalisation of our measurement concept -- this allows us to
   get a different perspective of the same concept. (e.g. if total length of
   pauses during a 1 hour experiment is $< 10$ minutes, it is 'easy'). We do this
   to get 'coverage' of the measurement concept. It is a form of cross
   validation. If we see an overlap in the correlational results after analysis,
   we can make a stronger assertion that e.g. "language A is easier than
   language B.". The idea I am describing here is methodological decision-making.
5. Predict what will be the likely results of our experiments on the
   operationalised measurements. This is "feed forward validation".
6. Do the experiement.
7. Analyse the data. See if the data has patterns that correlate with the
   assertion I wish to make. I will be representing the raw data in some outcome
   measure -- that is
   turning the raw data into a set of (or a single) value for comparison. 
8. Does the data answer the study question I set out to ask? This is now "feed
   backwards validation".
9. Write-up including the 'nitty-gritty' of the user study, and a statement like
   "Given our definition of easy, our multiple operationalisations of the
   concept of easy show that this is in fact objectively true/false".

### Pilots

We also spoke about ideas for pilot studies -- asking "What might be surprising
insights into declarative programming languages for User Interface Design -- the
case of Elm?".

Speak-aloud protocols where you prompt/facilitate the user to say what is on
their mind when that e.g. pause for more than 10 seconds -- a measurement I set
out to look for during an experiment. 

I might ask 

* > I notice you have paused for at least 10 seconds -- why did you?
* >> I thought the code would do X, but it did Y.
* > Why did you think it would do X?
* >> ...

I must ask the participant questions designed in a way that they are not
leading.

Leon suggested I gather a rich data set, as it's difficult to take notes AND
prompt the user during an experiment. SO difficult. Perhaps record video.

### Actions for next meeting

Devise a Pilot study, answering these 3 questions:

1. What might I ask people to do?
2. How will I gather data?
3. How will I analyse the data?

Also see paper Leon will send me on "Thematic analysis & Psychology"




# Pilot Study 1

Using per-participant questionnaire (See \ref{questionnaire1}), I captured video
& audio data of participants while the completed the task of extending a mario
game to make mario fly

## Hypotheses

##

### Method

Using Thematic analysis [@Braun2006Thematic] to code the data...

## Results

### Observation 1

* Prompting *"What are you thinking about?"* etc. seemed to place additional
  cognitive load on the user as they spent longer resuming than when not
  prompted. This caused noise in assessing the actual cognitive load incurred
  during the completion of the **task**. Were the signs of struggling/undergoing
  difficulty due to simply not understanding the language, or were they due to
  the difficulty of the task?

* In particular, the majority of instances where the users paused turned out to
  be confusion as to the semantics & syntax of the language.

### Model Adjustment 1

* Add tooltips that appear as the user places the keyboard cursor to the right
  of a token in the language.

### Observation 2

* Sifting through 1-hour+ of video data capture for incidences of cognitive load
  is *HARD!*. Is there some programmatic way of narrowing the video data to
  points of interest?

### Model Adjustment 2

* Track the user mouse and keyboard movements in a 3-tuple: `(Time t, (Mouse.x,
  Mouse.y), Keypress k)`
  
* It doesn't have to be implemented this way. I could extend **Model Adjustment
  1** to define blocks of code as tokens in themselves, and capture how long the
  cursor is static on that particular token.

* Leon suggested a further refinement of this idea in order to further narrow
  the data (in fact, just capturing mouse & keyboard movements will result in an
  explosion of the volume of data -- countrary to what I intend to achieve). His
  refinement was to define regions of interest in the code pane, and *only when
  the mouse/key cursor is in the region, do I capture data*. 

* Use the `if cursor in region then log (Time t, (Mouse.x, Mouse.y), Keypress
  k)` functionality as a *lens* to focus on significant portions of video
  capture.

## Further discussion

We then discussed some questions that might lead my direction of study in the
next steps of my research:

* Is the mouse/cursor position a proxy for someone's attention as they carry out
  the task?

* Often when I'm coding I'll leave the cursor where it is but think about other
  regions of code. I don't necessarily move the keyboard/mouse cursor to the
  section of code I'm thinking about. Instead, I use it as a 'bookmark' to track
  what I'm currently implementing, and may scroll around to other parts.

### We also discussed...

The result of the dissertation will be a list of observed cognitive
easing/loading that each language produces for users, much like an
advantage/disadvantage comparison:

----------- ------------
Elm         JavaScript
----------- ------------
+ ...       + ...

+ ...       - ...

- ...       - ...

- ...       + ...

+ ...       _
----------- ------------


## Actions

1. Design a task in JavaScript to go inside this adjusted model
   (incorporating Model Adjustment 1 and 2).

     This will require a degree of *"implementation juggling"* in order to find a
     balance of code-length/difficulty over the same task in Elm in such a way
     that is not creating noise in the thing being studied: Cognitive load. 

     Keep the reactivity constant, compare the differences in ease between JS and
     Elm.

2. If time available, run another Pilot study on this task + adjusted model

### Modifications to be made to the experimental methodology

Needs to be more objective! Why? What will I modify?

# Requirements

I will now identify what the requirements are for the project.

## Functional Requirements

1.  Write software to assist the capture of objective data to inform me
    of the user’s activities as they use the Elm IDE.

    1.  The program must be able to work offline and later transfer
        collected data to me once a connection is resumed, collecting
        mouse and keyboard activity\
        **Priority: High**

2.  Perform Pilot and User Studies

    1.  I must perform Pilot and User Studies in an iterative fashion,
        each one learning and building upon discoveries made in prior
        ones, starting vague and getting more and more focused on a
        particular facet of User Interface Design and/or Declarative
        programming as an activity.\
        **Priority: High**

    2.  I must use these studies to inform experimental and software
        design to disambiguate and filter data collected in the
        experiment, and to exercise hypotheses.\
        **Priority: High**

## Non-Functional Requirements

1.  Source code

    1.  The software must be written clearly and simply.\
        **Priority: High**

    2.  The software must have suitable, concise comments which explain
        the programs intent, but only where the code alone is not
        enough.\
        **Priority: High**

2.  Activity recording

    1.  The program activity recording feature must not slow down the
        user’s use of the IDE more than 1ms difference than without it.\
        **Priority: High**

    2.  There should be software to visualise the usage data\
        **Priority: Medium**

# Design

**This is the chapter in which you review your design decisions at various levels
and critique the design process.**

More detail on what I will modify. How will I modify?

![Extensions made to the Elm IDE]

# Implementation

Describe how I extended the Elm IDE

# Pilot Study 2

Using the Elm IDE


## Observations

The task I chose for Pilot Study 1 was too difficult to capture the cognitive load
incurred by the language itself for a given task, due to the difficulty of the
task itself creating noise.  I could improve this by simplifying the task, in a
way that is 'language agnostic', i.e. that is not idiomatic of Elm or JavaScript
(the two languages that I am comparing).  Something like the following will
never be that easy in JavaScript:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
main = lift asText Mouse.position
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Saw some things in Pilot Study 1, also in the use of the Elm IDE I extended, I saw some
things before Pilot Study 2.**

### Hypotheses

1H. 

## Experiment

### Method

A 2×2×2 study, that is 2 Languages (Elm and JavaScript), 2 Region difficulties
(Hard and Simple) and 2 Region relevances (Relevant and Not relevant) will be
done to determine if the number of mouse clicks per region differ across
variables.

# Group meeting with Leon at East Building, 11:15 Friday 4th October 2013


**N.B. READ UP ON AND REMIND YOURSELF OF HCI STUFF (Year 2) AND SOFTWARE
ENGINEERING STUFF (Year 1)**

### Reading material

In email repsonse to request for FYP meeting, **Leon writes:**

*Please do a bit of reading around beforehand. Go to the ACM Digital Library and
search on 'user interface programming'.*

1. [ACM Conference on Human Factors in Computing
   Systems](http://libproxy.bath.ac.uk/login?qurl=http%3A%2F%2Fdl.acm.org%2Fevent.cfm%3Fid%3DRE151)
2. [ACM CSCW: Conference on Computer Supported Cooperative
   Work](http://libproxy.bath.ac.uk/login?qurl=http%3A%2F%2Fdl.acm.org%2Fevent.cfm%3Fid%3DRE169)
3. [ACM UIST: Symposium on User Interface Software and
   Technology](http://libproxy.bath.ac.uk/login?qurl=http%3A%2F%2Fdl.acm.org%2Fevent.cfm%3Fid%3DRE172)


In moodle project page, **Leon writes:**


*Your project must be related to contemporary developments in Human-Computer
Interaction, and preferably to the part of the HCI world that focuses on
interactive systems for collaboration*

1. ???
2. ???

Also In moodle project page, **Leon also writes:**

*It normally starts with some user-centred research (observations, interviews,
pilot experiment) to ground the problem, carried out concurrently with
literature research. 

The research problem is normally boiled down to something that can be addressed
through the production of alternative versions of an interactive system. 

This is closely followed by initial design work and the production of a rough
but working prototype leading up to Christmas. 

After the January exams, my students typically re-scope their research problem,
based in the outcome of their initial work, and solidify their implementation
ready for a full evaluation in March and April.*

Thus, my answers to the questions Leon posed should follow this structure in
terms of what I want to get out of it. I can use the above structure to identify
**concerns** of potential challenges in each step/combination of
steps/step-transitions (e.g. step dependencies, resource procurement)


Also in moodle product page, **Leon also writes:**

*Students should prepare for their projects by refreshing their memories about
Interaction from CM20216 activities. You should read about HCI in general, and
support for collaboration in particular. Look at any or all of the following
book chapters:*

* Sharp, Rogers and Preece (2007) Interaction Design. hapter 4: Designing to
  Support Communication and Collaboration.
* Dix, Finlay, Abowd and Beale (2004) Human-Computer Interaction. hapter 14:
  Communication and Collaboration Models.
* Shneiderman and Plaisant (2005) Designing the User Interface. hapter 10:
  Collaboration.

### Leon asked us to answer these questions and bring a notebook:

#### Q1. What I hope to get out of my FYP as an experience?

I hope to gain a deep and meaningful understanding of the programmer as a user,
as an individual and the context of that individual -- e.g. in a software team
inside a department, inside a management structure... inside a company.

I hope to use this understanding to determine processes/work-flows that
programmers experience in the endeavour of User Interface Design, both from the
individual perspective and as a team.

Within these work flows, I wish to identify, in detail, metrics to gauge
productivity, in order to measure this in experiments, perhaps doing A/B testing
with Elm and some other, perhaps procedural language. This is an example of an
objective measure. 

I would also like to gather self-reported, more "fuzzy" feedback on user's
perception of their productivity -- pain points, advantages, etc.  they
experience in using Language X to product a UI compared to Language Y
(Declarative languages like Elm, etc)

I wish to verify, empirically, the comparisons and claims made on the [http://elm-lang.org/learn/What-is-FRP.elm]() page of the elm-lang.org
website, and those claimed it's research paper (detailing the implementation of
Elm, **benefits**, etc.)

In email again, **Leon writes:**

*The Elm site makes **comparative statements.** That is encouraging because it
sets up opportunities for you to test some of the claims they make, and to ask
new questions about Elm that its proponents may not have considered.*

These are:

1. "most current frameworks for graphical user interfaces are not declarative.
   They mire programmers in the many small, nonessential details of handling
   user input and manually modifying the display."
2. "with FRP, many of the irrelevant details are left to the compiler, freeing
   the programmer to think about things that matter."
3. "Coding the examples on [http://elm-lang.org/Examples.elm]() in a traditional
   GUI framework such as HTML/CSS/JavaScript . would require significantly more
   work and headache."
4. "Not only is that painful to code, but it also requires broad and deep
   knowledge of inconsequential things."
5. "FRP makes tasks considerably easier by taking care of the messy .how. of
   events, display, and updates."

#### Q2. Where my Project Idea came from (what inspired me)?

* The pain of coding and writing GUIs in PyQt4 while at my last job at Altran
* The joys of coding in Haskell
* The pain of writing GUIs in Haskell
* The joys of coding and writing GUIs in Elm!

#### Q3. What are my concerns?

1. Difficulty procuring programmers (users) -- specifically those that meet my
   criteria of not having used a declarative programming language.
2. Difficulty procuring programmers working in a team
3. The complexity/scope of the project -- is it enough for a FYP; is it too
   much?
4. Looking at the production of User Interfaces using a programming language,
   there are many variables -- how will I devise an experiment to minimise this
   and isolate a variable so that I can make some causal/correlational
   conclusions?
5. Concern regarding the dependency of a subsequent part of the project on a
   previous step -- this is inherent of all projects, though.

## Experimental design




### Tue Apr  1 14:30:00 BST 2014

Discussed progress made and what hypotheses to form that may usefully model
cognitive load.

#### Progress since last meeting

![Extensions made to the Elm IDE]

I have implemented full-screen mouse tracking that stores to a database a tuple:

    (t, (x, y))

for every mouse move, producing a list in JSON (so it's more like 
`{{uniq-userid: {125125, (67, 321)}}, {uniq-userid: {125126, (67, 322)}} ...}`)

I am ready to demo this (See Action 1.)

The only issue worth tweaking is that user activity data is captured separately
from the error output, so I will need to collate the data afterwards or find
some way to feed it into the same data store.

#### Meeting Discussion

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


#### Actions

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

### Tuesday Apr 8 14:30:00 BST 2014

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

# Implementation and Testing

This is the chapter in which you review the implementation and testing decisions and issues,
and critique these processes.
Code can be output inline using ````some code````. For example, this code is inline:
````public static int example = 0;```` (I have used the character | as a delimiter, but any
non-reserved character not in the code text can be used.)
Code snippets can be output using the environment with the code given in the environment. For example, consider listing 5.1, below.
Listing 5.1: Example code

Code listings are produced using the package "Listings". This has many useful options, so
have a look at the package documentation for further ideas.

# Results

### Tue Apr 15 15:50:38 BST 2014

Operationalisation of thrash (the concept), i.e. cementing the concept by a
metric that models cognitive load (does it? we don't know -- further work after
the analysis of this may determine if it is a plausible indicator of cognitive
load)

Leon suggested an improvement over this experimental method is to take people
who are new, and train them up either in JS or Elm, and then run the same task.
That way, their level of ability is comparable. (New as in never having used JS
or Elm)

My current method creates quite a bit of noise in the data, because I rely on
self-reported level of expertise in JS/Functional languages. I don't know how to
modify the data to account for this. I could group the analyses into categories?
I.e those who reported being experts at JS, those who reported never having used
it, those who reported being experts in at least one FP language, and those who
reported being new.

Talk about "phases" in a programmer's activities during task-completion:

(Not necessarily distinct and in sequence --- more often interleaved)

1. Familiarisation -- Where is the bit I need to change?
2. Narrowing in on the task once discovered -- Oh I need to change `X`, but how?
3. Solved task
4. Playing (?)

#### Mention the ways in which the study is flawed:

1. Self-reported expertise
2. Self-reported task completion
3. No way to be sure which error log pertains to which compile
1. Unique participant ID per Surveymonkey
2. Surveymonkey has taken my data hostage 
3. window dimensions?! 
4. Syntax reference 404
6. I did not capture their code solution, so relied on trust

#### Results

1. Describe data collected
1. How it was analysed (I aggregated regions and looked at number of clicks per
   region (Hard/Task, Hard/Not-Task, Simple/Task, Simple/Not-Task)\*(Elm,
   JavaScript))
1. Presentation of data (summary means std dev.)
    1. $\chi^2$ frequency analyses
    1. 2 × 2 × 2 making 8 cells. My expected is an even distribution of clicks in
       each category, i.e. if I have 80 clicks in total across all groups, I
       expect to find 10 in each cell if there is no correlation.

----------- ----------
Time (min)  Clicks
----------- ----------
 38.717217        183

  8.034583        130

  7.878533         39

 23.672500         25

 29.754533        391

 14.993517         78

 48.960367        769

  6.354050         71

  7.878533         39

 29.698267        501

 40.302217        803

 12.319317         65

 17.106933         79

 12.958300        119
----------- ----------

: Session time and clicks per session for Elm task


Instead of $\chi^2$, consider just using multiple regression with dummy
variables (binary predictors) (See Table \ref{tab:multipleregression})

---------- ------ -------- ------ --------- --------- --------- ---------
Condition  $d_1$  $d_2$    $d_3$  $d_4$     $d_5$     $d_6$     $d_7$        
---------- ------ -------- ------ --------- --------- --------- ---------
relevant × 1      0        0      0         0         0         0         
hard ×                                                                    
Elm                                                                 

relevant × 0      1        0      0         0         0         0         
hard ×                                                                    
JS                                                                 

relevant × 0      0        1      0         0         0         0         
easy ×                                                                    
Elm

relevant × 0      0        0      1         0         0         0         
easy ×                                                                    
JS

irrelevant 0      0        0      0         1         0         0         
× hard ×                                                                  
Elm                                                                 

irrelevant 0      0        0      0         0         1         0         
× hard ×                                                                  
JS                                                                 

irrelevant 0      0        0      0         0         0         1         
× easy ×                                                                  
Elm

irrelevant 0      0        0      0         0         0         0         
× easy ×                                                                  
JS
---------- ------ -------- ------ --------- --------- --------- ---------

: Multiple regression with dummy variables (d1, d2..) (binary predictors)
\label{tab:multipleregression}

* The $d_n$ in the top row are the dummy variables, the values are the codes you assign them
* There are $n-1$ dummy variabes; one group is coded as all zeros -- that's your reference group
* Why $n-1$? If there are 8 dummy codes, there are the same number of variables as conditions. the model's fully saturated and there are no degrees of freedom
    * Similar to $x_1 = 2, x_2 = 4, x_3 = 1$, and $y=2$. then trying to solve for $y$


**This is the chapter in which you review the outcomes, and critique the
outcomes process.**

**You may include user evaluation here too.**

# Conclusions

This is the chapter in which you review the major achievements in the light of your original
objectives, critique the process, critique your own learning and identify possible future
work.

# Bibliography {-}
