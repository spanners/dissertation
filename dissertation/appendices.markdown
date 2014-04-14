# Appendix A

## Design Diagrams

# Appendix B

## User Documentation

# Appendix C

## Raw results output

# Appendix D

## Code

### Server.hs

~~~~ {include="code/server/Server.hs"}
this will be replaced by contents of README
~~~~

### Editor.hs

~~~~ {include="code/server/Editor.hs"}
this will be replaced by contents of README
~~~~

### Generate.hs

~~~~ {include="code/server/Generate.hs"}
this will be replaced by contents of README
~~~~

### EmbedMeElm.elm

~~~~ {include="code/EmbedMeElm.elm"}
this will be replaced by contents of README
~~~~


### EmbedMeJS.elm

~~~~ {include="code/EmbedMeJS.elm"}
this will be replaced by contents of README
~~~~

### fullScreenEmbedMe.js

~~~~ {include="code/fullScreenEmbedMe.js"}
this will be replaced by contents of README
~~~~

### editor.js.diff

~~~~ {include="code/editor.js.diff"}
this will be replaced by contents of README
~~~~

### firebase-mouseclick-data.json

~~~~ {include="code/firebase-mouseclick-data.json"}
this will be replaced by contents of README
~~~~

### MovingBox.js

~~~~ {include="code/public/jsTask/MovingBox.js"}
this will be replaced by contents of README
~~~~

### MovingBox.elm

~~~~ {include="code/public/elmTask/MovingBox.elm"}
this will be replaced by contents of README
~~~~

# Appendix E


## Meeting minutes (sample)


### Fri Oct 4 11:15 GMT 2013 

Group meeting with supervisor Leon Watts 

**N.B. READ UP ON AND REMIND YOURSELF OF HCI STUFF (Year 2) AND SOFTWARE ENGINEERING STUFF (Year 1)**

* Reading material

In email repsonse to request for FYP meeting, **Leon writes:**

    Please do a bit of reading around beforehand. Go to the ACM Digital Library
    and search on 'user interface programming'.

1. [ACM Conference on Human Factors in Computing
   Systems](http://libproxy.bath.ac.uk/login?qurl=http%3A%2F%2Fdl.acm.org%2Fevent.cfm%3Fid%3DRE151)
2. [ACM CSCW: Conference on Computer Supported Cooperative
   Work](http://libproxy.bath.ac.uk/login?qurl=http%3A%2F%2Fdl.acm.org%2Fevent.cfm%3Fid%3DRE169)
3. [ACM UIST: Symposium on User Interface Software and
   Technology](http://libproxy.bath.ac.uk/login?qurl=http%3A%2F%2Fdl.acm.org%2Fevent.cfm%3Fid%3DRE172)


In moodle project page, **Leon writes:**


    Your project must be related to contemporary developments in Human-Computer
    Interaction, and preferably to the part of the HCI world that focuses on
    interactive systems for collaboration.

1. ???
2. ???

Also In moodle project page, **Leon also writes:**

    It normally starts with some user-centred research (observations,
    interviews, pilot experiment) to ground the problem, carried out
    concurrently with literature research.  The research problem is normally
    boiled down to something that can be addressed through the production of
    alternative versions of an interactive system. 
	
    This is closely followed by initial design work and the production of a
    rough but working prototype leading up to Christmas. 
	
    After the January exams, my students typically re-scope their research
    problem, based in the outcome of their initial work, and solidify their
    implementation ready for a full evaluation in March and April.

Thus, my answers to the questions Leon posed should follow this structure in
terms of what I want to get out of it. I can use the above structure to identify
**concerns** of potential challenges in each step/combination of
steps/step-transitions (e.g. step dependencies, resource procurement)


Also in moodle product page, **Leon also writes:**

    Students should prepare for their projects by refreshing their memories
    about Interaction from CM20216 activities. You should read about HCI in
    general, and support for collaboration in particular. Look at any or all of
    the following book chapters:

* Sharp, Rogers and Preece (2007) Interaction Design. hapter 4: Designing to
  Support Communication and Collaboration.
* Dix, Finlay, Abowd and Beale (2004) Human-Computer Interaction. hapter 14:
  Communication and Collaboration Models.
* Shneiderman and Plaisant (2005) Designing the User Interface. hapter 10:
  Collaboration.

#### Leon asked us to answer these questions and bring a notebook:

* Q1. What I hope to get out of my FYP as an experience?

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
experience in using Language X to produce a UI compared to Language Y
(Declarative languages like Elm, etc)

I wish to verify, empirically, the comparisons and claims made on the [What is
FRP?](http://elm-lang.org/learn/What-is-FRP.elm) page of the elm-lang.org
website, and those claimed it's research paper (detailing the implementation of
Elm, **benefits**, etc.)

In email again, **Leon writes:**

    The Elm site makes **comparative statements.** That is encouraging because
    it sets up opportunities for you to test some of the claims they make, and
    to ask new questions about Elm that its proponents may not have considered.

These are:

1. "most current frameworks for graphical user interfaces are not declarative.
   They mire programmers in the many small, nonessential details of handling
   user input and manually modifying the display."
2. "with FRP, many of the irrelevant details are left to the compiler, freeing
   the programmer to think about things that matter."
3. "Coding [these examples](http://elm-lang.org/Examples.elm) in a traditional
   GUI framework such as HTML/CSS/JavaScript . would require significantly more
   work and headache."
4. "Not only is that painful to code, but it also requires broad and deep
   knowledge of inconsequential things."
5. "FRP makes tasks considerably easier by taking care of the messy .how. of
   events, display, and updates."

* Q2. Where my Project Idea came from (what inspired me)?

* The pain of coding and writing GUIs in PyQt4 while at my last job at Altran
* The joys of coding in Haskell
* The pain of writing GUIs in Haskell
* The joys of coding and writing GUIs in Elm!

* Q3. What are my concerns?

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

---


### Tue Oct 30 09:15 GMT 2013 

Individual Meeting after Proposal hand-in

Our discussion centered around the direction I wish to take following my Project
Proposal.

#### AB Testing of the language with the same IDE

The primary direction I mentioned (as echoed in my Proposal) was doing AB
testing of Elm vs. another language (e.g. JavaScript) (i.e. the language is the
dependent variable) using the same Concurrent FRP IDE (the independent
variable).

#### Test just the paradigm

He also suggested a potential experiment to test just the paradigm, eliminating
the IDE from the experiment above. Perhaps for a Pilot study.

#### Experiment process

1. Study question (e.g. Is it easy?)
2. Measurement concept (e.g. "Easy")
3. Operationalisation -- taking a measurement concept and mapping it to
   something concrete (e.g. if completing a pre-defined task the user must
   complete takes < 5 steps, it is 'easy' -- we can then compare instances of
   these studies given our definition of easy). This is much like mapping a
   design to an implementation, and there is a risk of losing information, or
   ending up with a mismatched concrete instance that does not represent the
   concept we wish to convey.
4. Do another operationalisation of our measurement concept -- this allows us to
   get a different perspective of the same concept. (e.g. if total length of
   pauses during a 1 hour experiment is < 10 minutes, it is 'easy'). We do this
   to get 'coverage' of the measurement concept. It is a form of cross
   validation. If we see an overlap in the correlational results after analysis,
   we can make a stronger assertion that e.g. "language A is easier than
   language B.". The idea I am describing here is methodological
   decision-making.
5. Predict what will be the likely results of our experiments on the
   operationalised measurements. This is "feed forward validation".
6. Do the experiement.
7. Analyse the data. See if the data has patterns that correlate with the
   assertion I wish to make. I will be representing the raw data in some outcome
   measure -- that is turning the raw data into a set of (or a single) value for
   comparison. 
8. Does the data answer the study question I set out to ask? This is now "feed
   backwards validation".
9. Write-up including the 'nitty-gritty' of the user study, and a statement like
   "Given our definition of easy, our multiple operationalisations of the
   concept of easy show that this is infact objectively true/false".

#### Pilots

We also spoke about ideas for pilot studies -- asking "What might be surprising
insights into declarative programming languages for User Interface Design -- the
case of Elm?".

Speak-aloud protocols where you prompt/facilitate the user to say what is on
their mind when that e.g. pause for more than 10 seconds -- a measurement I set
out to look for during an experiment. 

I might ask 
> I notice you have paused for at least 10 seconds -- why did you?
>> I thought the code would do X, but it did Y.
> Why did you think it would do X?
>> ...

I must ask the participant questions designed in a way that they are not
leading.

Leon suggested I gather a rich data set, as it's difficult to take notes AND
prompt the user during an experiment. SO difficult. Perhaps record video.

#### Actions for next meeting

Devise a Pilot study, answering these 3 questions:

1. What might I ask people to do?
2. How will I gather data?
3. How will I analyse the data?

Also see paper Leon will send me on "Thematic analysis & Psychology"

---

### Wed Mar 25 14:30 GMT 2014

(Several meetings undocumented)

TODO: Refer to notes in Diary for previous entries.

#### Progress since last meeting

Discussed findings from analysis of pilot study

#### Observation 1

* Prompting *"What are you thinking about?"* etc. seemed to place additional
  cognitive load on the user as they spent longer resuming than when not
  prompted. This caused noise in assessing the actual cognitive load incurred
  during the completion of the **task**. Were the signs of struggling/undergoing
  difficulty due to simply not understanding the language, or were they due to
  the difficulty of the task?

* In particular, the majority of instances where the users paused turned out to
  be confusion as to the semantics & syntax of the language.

#### Model Adjustment 1

* Add tooltips that appear as the user places the keyboard cursor to the right
  of a token in the language.

#### Observation 2

* Sifting through 1-hour+ of video data capture for incidences of cognitive load
  is *HARD!*. Is there some programmatic way of narrowing the video data to
  points of interest?

#### Model Adjustment 2

* Track the user mouse and keyboard movements in a 3-tuple: (Time t, (Mouse.x,
  Mouse.y), Keypress k)
  
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

#### Further discussion

We then discussed some questions that might lead my direction of study in the
next steps of my research:

* Is the mouse/cursor position a proxy for someone's attention as they carry out
  the task?

* Often when I'm coding I'll leave the cursor where it is but think about other
  regions of code. I don't necessarily move the keyboard/mouse cursor to the
  section of code I'm thinking about. Instead, I use it as a 'bookmark' to track
  what I'm currently implementing, and may scroll around to other parts.


#### Actions

1. Design a task in JavaScript to go inside this adjusted model (incorporating
   Model Adjustment 1 and 2).

   This will require a degree of *"implementation juggling"* in order to find a
   balance of code-length/difficulty over the same task in Elm in such a way
   that is not creating noise in the thing being studied: Cognitive load. 

   Keep the reactivity constant, compare the differences in ease between JS and
   Elm.

2. If time available, run another Pilot study on this task + adjusted model
