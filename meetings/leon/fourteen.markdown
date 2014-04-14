# Tuesday Apr 8 14:30:00 BST 2014

What makes code difficult to understand and work with?

* Bit twiddling?
* Declaring and defining simultaneously?
* Compound if/then/else statements?

"[Programming is] manipulating symbols blindly" ~ Bret Victor

Do a 2x2 study, defining regions in the code monitoring mouse clicks. Regions
can either be simple/hard in complexity (exhibiting/not-exhibiting one of the
above 'difficult' properties). Or code can be task-oriented or not, that is *the
code does/does not need to be changed to achieve the completed task set for the
user*:

## 2x2 study between-subjects

--------------- ------------------
Elm             -
--------------- ------------------
Simple/Task     Hard/Task

Simple/Not-Task Hard/Not-Task
--------------- ------------------

--------------- ------------------
JavaScript      -
--------------- ------------------
Simple/Task     Hard/Task

Simple/Not-Task Hard/Not-Task
--------------- ------------------

## Study method

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

## Actions

1. **DONE** Reorder divs so embedded div is on top of editor div.

    This turned out (I am fairly certain) to be due to codemirror.js binding
    mouse clicks. It was solved by using Elm's `Mouse.isDown`. Using
    `Mouse.isDown` has the added benefit of tracking mouse selects and drags,
    because it logs `(x,y)` when the mouse is down and `(x,y)` again when it is up.

2. **DONE** Create a task that features *Hard/Simple x Task/Not-task* (See [table above])

3. ~~Implement *Region filtering* functionality so mouse activity is only logged
   when the clicks occur within defined region(s)~~

    I have instead defined bounding boxes that pertain to the regions I want to
    track as a mouse-data filter -- that is, I capture all click data for the
    whole frame, and then filter it by comparing x,y co-ordinates with my
    bounding boxes. If it's in the box, keep it, otherwise discard.

4. **DONE** Integrate JS task into IDE

5. **DONE** Perform pilot study

6. **WIP** Visualise mouse data

[table above]: #2x2-study-between-subjects
