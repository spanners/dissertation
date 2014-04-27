# Pilot Study 2 \label{pilot2}

Following from the outcomes of Pilot Study 1, including the modifications made
to the experimental model, and the feature--augmented Elm IDE, I
would like to conduct another Pilot Study to test the features and determine
whether it accurately models thrashing/cognitive load. This section describes
the Observations made both from Pilot Study 1, Hypotheses I form due to these
Observations, The experimental Method, Results, Analysis and Discussion.

## Observations

Observations and participant feedback from Pilot Study 1 (See
\ref{pilot1-participant1}) suggest that the task
I chose, and the way in which I carried out the experiment, was too taxing to
capture the cognitive load incurred by the language itself for a given task, due
to the difficulty of the task itself creating noise, and the experimental
methodology incurring cognitive load -- my prompting and questioning causing
pauses. I could improve this by simplifying the task, in a way that is 'language
agnostic', i.e. that is not idiomatic of Elm or JavaScript (the two languages
that I am comparing).  Something like the following will never be that easy in
JavaScript:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
main = lift asText Mouse.position
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Hypotheses

* $H_0$. There will be a uniform distribution of the total amount of clicks per
  region (Null hypothesis), for an $\alpha$ of 0.05

* $H_1$. There will not be a uniform distribution of the total amount of clicks per
  region (Alternative hypothesis), for an $\alpha$ of 0.05

## Experiment

### Method

Using the IDE I have augmented to gather click data, and a pre-questionnaire to
determine level of expertise (See Section \ref{pilot2-pre-questionnaire}), I
will two independent pilots of users completing the same task in either Elm or
JavaScript.

The task (See code listings \ref{MovingBox.elm} and \ref{MovingBox.js} for the
Elm and JS versions, respectively) is designed in such a way as to approximate the
property of being 'language agnostic' -- the versions of the task are reasonably
similar in length, have the same comments, and variables are named similarly.
The task is to make the moving box clamp to the grey window's edges when moved
with arrow keys, preventing it from disappearing. It must be clamped in such a
way that, upon attempting to move the box *out of the grey window*, it stops
half-way. A YouTube video (See https://www.youtube.com/watch?v=cUgK42N7kt8) is
to be given to the participants so that they can see what the completed task
looks like.

The experiment will be conducted remotely.  I will send the participants a link
to the pre-questionnaire and either the Elm or JS task, and the youtube video
link. The pre-questionnaire data will be stored on Survey Monkey, the click data
will be stored on Firebase in JSON via my EmbedMe.elm (See \ref{EmbedMe.elm})
mouse click tracking program, and I will log the time at which a click occurs
and it's `(x,y)` coordinates under a participant number unique to each
participant.

This will be a 2×2×2 study, using geometrically defined regions (See Figures
\ref{fig:regions-elm-labelled} and \ref{fig:regions-js-labelled}), also known as
bounding boxes, in the code, monitoring the count of mouse clicks per region as
an indicator of thrashing/cognitive load. Regions can either be easy/hard in
complexity (exhibiting/not--exhibiting some/all of the 'difficult' properties
identified in the Design (See Section \ref{Design})).
Or code can be task--relevant or task--irrelevant, that is *the code does/does
not need to be changed to achieve the completed task set for the user*:


------------------ -----------------
**Elm**          

Easy/Relevant      Hard/Relevant

Easy/Irrelevant    Hard/Irrelevant

**JavaScript**   

Easy/Relevant      Hard/Relevant

Easy/Irrelevant    Hard/Irrelevant
------------------ -----------------

: 2 × 2 × 2 study between-subjects \label{tab:2x2study}

I will look at total and/or mean time in each of these areas for comparison. The
study will be **between-subjects** instead of within-subjects. That is, I will
study *different users* for different languages. If a user has completed the
task in Elm, I can not have them complete the task in JavaScript, and
vice-versa.

I will necessarily make a compromise here:

Between-subjects:

* I lose the ability to keep programmer competence as constant, thus it is a
  confounding variable

* I gain the ability to ignore learned-experience in completing the task -- the
  participant is different every time so will not have done this task before,
  thus this is not a confounding variable.

Within-subjects is the converse of the above methodological properties

On the resulting raw data, I will perform a multiple regression --- on the 2
Languages (Elm, JavaScript) × 2 region Difficulties (Hard, Simple) × 2 region
Relevances (Relevant, Not
relevant) --- to determine if the number of mouse clicks per region
differs across regions.

## Results \label{pilot2-results}

The raw click data (See \ref{mouseclick-data}), was processed with Python
scripts (See \ref{ClicksPerCategory.py}, \ref{get_clicks_per_category.py},
\ref{DecodeMouseData.py}) to produce the following tables:

------------ ----------- -------
Participant  Time (min)  Clicks
------------ ----------- -------
1            38.717      183
 
2             8.034      130
 
3             7.878       39
            
4            23.672       25
            
5            29.754      391
            
6            14.993       78
            
7            48.960      769
            
8             6.354       71
            
9             7.878       39
            
10           29.698      501
            
11           40.302      803
            
12           12.319       65
            
13           17.106       79
            
14           12.958      119
------------ ----------- -------

: Session time and clicks per session for Elm task

------------ ----------- -------
Participant  Time (min)  Clicks
------------ ----------- -------
1             8.545        126

2             3.766         41

3            18.731         75

4             4.537        117
------------ ----------- -------

: Session time and clicks per session for JS task

----------------------------- ----------- --------- ----------
Category                      Expected %  Expected  Observed  
----------------------------- ----------- --------- ----------
relevant × hard × Elm         12.5 %      106.37    76        
                                                      
relevant × hard × JS          12.5 %      106.37    33        
                                                
relevant × easy × Elm         12.5 %      106.37    487       
                                                
relevant × easy × JS          12.5 %      106.37    12        
                                                
irrelevant × hard × Elm       12.5 %      106.37    105       
                                                
irrelevant × hard × JS        12.5 %      106.37    69        
                                                
irrelevant × easy × Elm       12.5 %      106.37    66        
                                                
irrelevant × easy × JS        12.5 %      106.37    3         
                                                            
**TOTAL**                     **100%**    **851**   **851**   
----------------------------- ----------- --------- ----------

: 2×2×2 comparison of clicks per category --- Expected and Observed
\label{tab:pre-chisquared}

Table \ref{tab:pre-chisquared}, in the Expected column, shows a normal
distribution of clicks per category -- if our null hypothesis $H_0$ holds, the
observed outcome be 5% either side of this.


![Participant 18, JS task (Overlaid with mouse
clicks)\label{fig:visualise-18-overlay}](images/visualise-18-overlay.png)

![Participant 15, Elm task (Illustrating the potential
offset)\label{fig:visualise-15-overlay}](images/visualise-15-overlay.png)

See Figure \ref{fig:visualise-18-overlay} for the visualisation of participant
18 completing the JavaScript version of the task. The augmented IDE can be used
to visualise the click data for any participant number. Uncomment *line 32* of
*EmbedMe.elm* (See code listing \ref{EmbedMe.elm}) and reload the task editor
with the desired participant ID as an input parameter `p` to the URL, e.g. to
visualise participant 18 click data:

````
http://0.0.0.0:8000/edit/task/MovingBox.elm?p=18
````

### Analysis \label{pilot2-analysis}

I will now talk about how I analysed the raw data that was captured.

I performed the multiple regression on the categories defined using the
statistics tool SPSS. See Section \ref{multiple-regression} in Appendix for SPSS multiple regression
output. (**N.B** languages (Lan) and the relevance (Rel) and difficulty (Diff)
are set to variables in the output: `Elm := 1.00`,  `JS := 2.00`, and later in
the *K-Way and Higher Order Effects* tables, `Rel := 1`, `Diff := 2`, `Lan :=
3`). The following Chi-square ($\chi^2$) results were obtained.


---------------------- -------------
$\chi^2$               1633.879

Degrees of freedom     7

$\rho$--value          0

Yates' $\chi^2$        1626.741

Yates' $\rho$--value   0
---------------------- -------------

: $\chi^2$ calculation of clicks per quadrant
\label{tab:post-chisquared}

\newpage

### T--statistic result between groups Elm and JS \label{t-stat}

Using SciPy's `stats.ttest` function (See \ref{ttest-scipy.py}), and assuming
no variance between groups, and the following clicks per language in each
category, 

* Elm: `sum([76, 487, 105, 66]) = 734` clicks
* JS: `sum([33, 12, 69, 3]) = 117` clicks

 we can obtain a T--test value and $\rho$--value.

--------------- -----
T--value:       1.50 

$\rho$--value:  0.23
--------------- -----

: T--value and $\rho$--value between languages


## Interpretation

Looking at the Pearson's lookup table of $\chi^2$ value *vs.* $\rho$--value
(probability) we can see that, for an $\alpha$ of 5%, that is a $\rho$--value of
0.05, and 7 degrees of freedom, a $\chi^2$--value of 12.59 is enough to show
significance, and we obtained a $\chi^2$--value of 1633.955 (approx.). This
means table \ref{tab:post-chisquared} shows that there is statistical
significance, and there less than 0.05 probability this significance arose out
of chance (in fact, it is so small that SPSS rounded to 0). We can therefore
**reject the null hypothesis $H_0$**, and **accept the alternative hypothesis
$H_1$**, assuming a rigorous study -- more on this in the Discussion section
that follows.

## Discussion \label{pilot2-discussion}

During the few (2 Elm tasks) Pilot Study 2 experiments that I did observe (not
included in the results), I observed what could be interpreted as "phases" in a
programmer activity during task--completion:


1. Familiarisation -- Where is the bit I need to change?
2. Narrowing in on the task once discovered -- Oh I need to change `X`, but how?
3. Solved task
4. Playing with the solved task

(Not necessarily distinct and in sequence --- more often interleaved)

Since I conducted Pilot Study 2 remotely by sending the participant a link to
the pre-questionnaire survey (See \ref{pilot2-pre-questionnaire}) and a link to
either the Elm or JS task (See \ref{MovingBox.elm} and \ref{MovingBox.js},
respectively) I can not assert with any certainty that this was occurring during
the experiments in Pilot Study 2.

The study, although found that there is a significant difference in the number
of clicks in regions between languages (as an operationalisation of cognitive
load experienced in completing the same task), unfortunately had a number of flaws which
confound it's rigor, and therefore no meaning can be derived from these results.

In summary, the flaws I have identified are as follows:

1. Much smaller respondent count for JS than Elm.
1. Small sample size -- this forced me to use a T-test rather than a Z-test,
   meaning I can not be as sure of the results.
1. Self-reported expertise -- [@hansson2001competency] found that "these
   judgements (self-reports) are well performed and accurate enough to be
   incorporated as a valuable tool", but "the findings suggest that taking into
   account the individual's perception of how important the specific competence
   is for performing a particular job (relative competence) might be a way to
   handle problems with the variation in the importance of different
   competencies.", and there is no job--critical scenario here in this user
   participation study. We rely on the user themselves trustfully reporting
   their expertise in the languages (See pre-questionnaire
   \ref{pilot2-pre-questionnaire}) without the incentive of a salary or the risk
   of job loss.
1. Self-reported task completion -- We rely on the user themselves reporting
   that they have completed the task according to the specification
1. No way to be sure which error log pertains to which compile -- A shortcoming
   that was discovered *after* Pilot Study 2 is that the error log that captures
   when the user tries to compile syntactically/semantically correct code *does
   not log the participant ID the error pertains to*
1. No unique participant ID per SurveyMonkey -- We can not be sure that,
   although the timestamps are similar, a person filling out the survey at time
   $t$ may not be the same person starting the task at time $t$.
1. Window dimensions not captured -- How can we be sure that a click in location
   `(x,y)` *on the user's screen and resolution* is the same `(x,y)` that I
   capture in the database? Participant 15 (See Figure
   \ref{fig:visualise-15-overlay}) very likely had a much shorter window height
   than I have used here. I suspect this is the case because of the cluster of
   mouse clicks in the same range of the $x$ axis as the Compile button, but
   much further up in the $y$ axis, but I have no way to be sure.
1. I did not capture window resizing -- Same problem as above
1. Mouse scrolling not captured -- Same problem as above. Although the task code was
   purposefully designed to fit in as small a screen space as possible, if the
   user has a smaller screen than the text's dimensions, they may scroll,
   therefore offsetting the captured clicks.
1. Syntax reference 404 links -- It was discovered *after* Pilot Study 2 that the
   Syntax reference links returned `Server Error 404`, due to me accidentally
   failing to include the documentation in the site where the task was hosted.
   Captured click data suggests that people did attempt to follow the Syntax
   reference links, and the server access error logs support this indication
   with multiple 404s to the Syntax Reference. 
