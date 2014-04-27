# Pilot Study 1 \label{pilot1}

Using a per-participant questionnaire (See \ref{pre-questionnaire}), I captured video
& audio data of 2 participants while they completed the task of extending a
mario game to make mario fly. This initial pilot study was done to get a feel of
what behaviours may be worth investigating further while a user completes a
programming task.  I may then refine the methodology to enable said behaviours
to be isolated more effectively, varying some dependent variable to see if it
has any effect. I have one hypothesis based on my understanding of thrashing
from the literature review, [@Lopez2012a].

## Hypotheses

* $H_1$. Novice users (*defined as*: those that list themselves as being new to
  functional programming in the pre-questionnaire)
  will press compile at least once every 2 minutes during the programming task.
* $H_2$. Novice users will not pause (*defined as:* no mouse movement, no
  typing) for more than 2 minutes during the programming task.

### Method

1. Consent form is signed (See \ref{consent-form})
2. Pre-questionnaire is given out.
3. User is informed that they may ask for help, and that they will
   be prompted if they pause (at the moment they break the pause), to ask why they
   paused. They are also informed that they can end at any time, and the goal is
   to make mario fly.
4. User is shown the result of completed task (mario flying).
4. Programming task is begun.
5. User completes or ends the task.
6. Post-questionnaire is given out (See \ref{post-questionnaire})
7. De-breifing.
8. Study ends.

Using Thematic analysis [@Braun2006Thematic] to code the captured audio and
video data, I will transcribe the programming activity, to see what themes
arise. 

### Using Thematic Analysis in Studies

In doing Thematic Analysis [TA], as a researcher, one must make **explicit** the
particular variant of TA that you intend to carry out, and the whole analysis
must be framed from that point on by that experimental methodology.

One must make the statement along the lines of: "Amongst the number of different
branches that the paper talks about, *X* with *Y* themes and *Z* focus is the
flavour of thematic analysis that I am going to use" -- e.g. essentialism NOT
constructionism, one aspect NOT whole, theoretical NOT inductive...

As this is a Pilot Study I will simply be using the 6-phase analysis to gather
themes in the data (if there are any), as shown in Table \ref{tab:phases-of-ta}.

  ----------------------------------------------------------------------
  Phase                       Description of the process
  --------------------------- ------------------------------------------
  1. Familiarising yourself   Transcribing data (if necessary), reading
  with your data:             and re-reading the data, noting down
                              initial ideas

  2. Generating initial       Coding interesting features of the data in
  codes:                      a systematic fashion across the entire
                              data set, collating data relevant to each
                              code.

  3. Searching for themes:    Collating codes into potential themes,
                              gathering all data relevant to each
                              potential theme.

  4. Reviewing themes:        Checking in the themes work in relation to
                              the coded extracts (Level 1) and the
                              entire data set (Level 2), generating a
                              thematic 'map' of the analysis.

  5. Defining and naming      Ongoing analysis to refine the specifics
  themes:                     of each theme, and the overall story the
                              analysis tells; generating clear
                              definitions and names for each theme.

  6. Producing the report:    The final opportunity for analysis.
                              Selection of vivid, compelling extract
                              examples, final analysis of selected
                              extracts, relating back of the analysis to
                              the research question and literature,
                              producing a scholarly report of the
                              analysis.
  ----------------------------------------------------------------------

  : Phases of Thematic Analysis, from [@Braun2006Thematic]
  \label{tab:phases-of-ta}

## Results

The participant that listed themselves as being Experienced in Functional
Programming did in fact exhibit more thrash than the one that listed themselves
as being a Novice, contrary to $H_1$ and $H_2$ They were observed clicking
compile once every 1m3s on average, to the Novice's 2m3s, and pausing for an
average of 3m24s to the Novice's 1m45s. I **reject** hypotheses $H_1$ and $H_2$.
It is important to see that this is an extremely small sample size, not nearly
enough to achieve saturation, and so not much meaning can be derived from this
result, but it is interesting.

## Observations

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

* More of a meta-observation about the methodology than about the experiment
  findings itself: Sifting through 1--hour+ of video data capture for incidences
  of cognitive load is tedious and not particularly fruitful or objective. Is
  there some programmatic way of narrowing the
  video data to points of interest?

### Model Adjustment 2

* Scrap the Thematic Analysis methodology entirely. Instead, extend the IDE to
  allow for tracking the user mouse and keyboard movements in a 3-tuple: `(Time
  t, (Mouse.x, Mouse.y), Keypress k)`
  
    * It doesn't have to be implemented this way. I could extend **Model
      Adjustment 1** to define blocks of code as tokens in themselves, and
      capture how long the cursor is static on that particular token.
      
    * A further refinement of this idea is to filter the data (in
      fact, just capturing mouse & keyboard movements will result in an
      explosion of the volume of data -- countrary to what I intend to achieve):
      define regions of interest in the code pane, and *only when the mouse/key
      cursor is in the region, do I capture data*. 
      
    * Use the `if cursor in region then log (Time t, (Mouse.x, Mouse.y), Keypress
      k)` functionality as a *lens* to focus on significant portions of video
      capture.

## Discussion

Following the Pilot Study, I drafted some questions and thoughts that might lead
my direction of study in the next steps of my research:

* How can I capture a more objective measure of a user's interaction with the
  IDE?

* What behaviours are indicative of a user experiencing cognitive load?

* Is the mouse/cursor position a proxy for someone's attention as they carry out
  the task?

* Often when I'm coding I'll leave the cursor where it is but think about other
  regions of code. I don't necessarily move the keyboard/mouse cursor to the
  section of code I'm thinking about. Instead, I use it as a 'bookmark' to track
  what I'm currently implementing, and may scroll around to other parts.

At this point my goal of this dissertation is to obtain a list of observed
cognitive easing/loading that each language produces for users, much like an
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

I will now discuss Requirements that need to be met in order to realise this
goal.
