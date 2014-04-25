# Requirements

## High--level goals

1. Augment the Elm IDE to support the logging of input device activities during
   a programming task

1. Identify metrics that accurately model cognitive load

1. Design a task in JavaScript to go inside this adjusted model
   (incorporating Model Adjustment 1 and 2).

     This will require a degree of *"implementation juggling"* in order to find a
     balance of code-length/difficulty over the same task in Elm in such a way
     that is not creating noise in the thing being studied: Cognitive load. 

     Keep the reactivity constant, compare the differences in ease between JS and
     Elm.

2. If time available, run another Pilot study on this task + adjusted model

I will now identify what the requirements are for the project in order to
achieve these High--level goals.

## Functional Requirements

1.  Write software to assist the capture of objective data to inform me
    of the user’s activities as they use the Elm IDE.

    1.  The program must be able to capture data on-the-fly collecting mouse and
        keyboard activity\
        **Priority: High**

    1. The program must be able to support both arbitrary Elm and Javascript
       tasks\ 
       **Priority: High**

    1. The program should only capture data relevant to the study at hand -- for
       example if we are interested in a rectangular region defined by
       co-ordinates (top left, bottom right): `(x:10, y:45), (x:50, y:90)`, the
       program must only capture data within that region.\
       **Priority: Medium**

    1. The user of the program must be able to define regions with which to
       filter the data set.\
       **Priority: High**

    1. The program should capture syntax and semantic errors made when the user
       attempts to compile erroneous code.\
       **Priority: Medium**

1. Link experiment to each user

    1. The experiment must be able to support remote data capture --- i.e.
       support sending a URL of the IDE and task to participants, captuing their
       interaction remotely.\
       **Priority: High**

    1. The experiment must link the questionnaire to the task and to the compile
       error output so that one can be certain of a 1--1 correspondence with
       each source of data to each respondent, for collating afterwards.

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
        the program intent, but only where the code alone is not
        enough.\
        **Priority: High**

2.  Activity recording

    1.  The program activity recording feature must not slow down the
        user's use of the IDE more than 1ms difference than without it.\
        **Priority: High**

    2.  There must be software to visualise the usage data\
        **Priority: High**
