# Pilot Study 1

Using per-participant questionnaire (See \ref{questionnaire1}), I captured video
& audio data of participants while the completed the task of extending a mario
game to make mario fly

## Hypotheses

* H1. ...
* H2. ...

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

# New Study! First, implement an IDE that logs input

## Requirements

I will now identify what the requirements are for the project.

### Functional Requirements

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

### Non-Functional Requirements

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

