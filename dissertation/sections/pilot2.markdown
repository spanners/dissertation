# Pilot Study 2

**Using the Elm IDE blah blah

Also using questionnaire (See \ref{pilot2-pre-questionnaire}). blah blah**

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

**Saw some things in Pilot Study 1, also in the use of the Elm IDE I extended, I
saw some things before Pilot Study 2.**

### Hypotheses

* $H_0$. There will be a uniform distribution of the total amount of clicks per
  region (Null hypothesis), for an $\alpha$ of 0.05

* $H_1$. There will not be a uniform distribution of the total amount of clicks per
  region (Alternative hypothesis), for an $\alpha$ of 0.05

## Experiment

### Method

Use the IDE I have augmented to gather click data, I will run studies 

A 2×2×2 multiple regression, that is --- 2 Languages (Elm, JavaScript) × 2
region Difficulties (Hard, Simple) × 2 region Relevances (Relevant, Not
relevant) --- will be done to determine if the number of mouse clicks per region
differ across variables.

## Results

----------- ----------
Time (min)  Clicks
----------- ----------
 38.717     183

  8.034     130

  7.878      39

 23.672      25

 29.754     391

 14.993      78

 48.960     769

  6.354      71

  7.878      39

 29.698     501

 40.302     803

 12.319      65

 17.106      79

 12.958     119
----------- ----------

: Session time and clicks per session for Elm task

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

: Clicks per Category, Expected and Observed
\label{tab:pre-chisquared}


![Participant 18, JS task (Overlaid with mouse
clicks)\label{fig:visualise-18-overlay}](images/visualise-18-overlay.png)

![Participant 15, Elm task (Illustrating the potential
offset)\label{fig:visualise-15-overlay}](images/visualise-15-overlay.png)

See Figure \ref{fig:visualise-18-overlay} for the visualisation of participant 18
completing the JavaScript version of the task.

Operationalisation of thrash (the concept), i.e. cementing the concept by a
metric that models cognitive load (does it? we don't know -- further work after
the analysis of this may determine if it is a plausible indicator of cognitive
load)


### Analysis

1. **Describe data collected
1. How it was analysed (I aggregated regions and looked at number of clicks per
   region (Hard/Task, Hard/Not-Task, Simple/Task, Simple/Not-Task)\*(Elm,
   JavaScript))
1. Presentation of data (summary means std dev.)
    1. $\chi^2$ frequency analyses
    1. 2 × 2 × 2 making 8 cells. My expected is an even distribution of clicks in
       each category, i.e. if I have 80 clicks in total across all groups, I
       expect to find 10 in each cell if there is no correlation.**

See Section \ref{multiple-regression} in Appendix for SPSS multiple regression
output. (**N.B** languages (Lan) and the relevance (Rel) and difficulty (Diff)
are set to variables in the output: `Elm := 1.00`,  `JS := 2.00`, and later in
the *K-Way and Higher Order Effects* tables, `Rel := 1`, `Diff := 2`, `Lan :=
3`)


---------------------- -------------
$\chi^2$               1633.879

degrees of freedom     7

$\rho$--value          0

Yates' $\chi^2$        1626.741

Yates' $\rho$--value   0
---------------------- -------------

: $\chi^2$ calculation of clicks per quadrant
\label{tab:post-chisquared}

### T--statistic result between groups Elm and JS

Using SciPy's `stats.ttest` function (See \ref{ttest_scipy.py}), and assuming
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

Looking at the Pearson's table of $\chi^2$ value *vs.* $\rho$--value
(probability) we can see that, for an $\alpha$ of 5%, that is a $\rho$--value of
0.05, and 7 degrees of freedom, a $\chi^2$--value of 12.59 is enough to show
significance, and we obtained a $\chi^2$--value of 1633.955 (approx.). This
means table \ref{tab:post-chisquared} shows that there is statistical
significance, and there less than 0.05 probability this significance arose out
of chance (in fact, it is so small that SPSS rounded to 0). We can therefore
**reject the null hypothesis $H_0$**, and **accept the alternative hypothesis
$H_1$**.

## Discussion

Talk about "phases" in a programmer's activities during task-completion:

(Not necessarily distinct and in sequence --- more often interleaved)

1. Familiarisation -- Where is the bit I need to change?
2. Narrowing in on the task once discovered -- Oh I need to change `X`, but how?
3. Solved task
4. Playing (?)

The study, although found that there is a significant difference in the number
of clicks in regions between languages (as an operationalisation of cognitive
load experienced in completing the same task), unfortunately had a number of flaws which
confound it's rigor, and therefore no meaning can be derived from these results.

In summary, the flaws I have identified are as follows:

1. Much smaller respondent count for JS than Elm.
1. Small sample size -- this forced me to use a T-test rather than a Z-test,
   meaning I can not be as sure of the results.
1. Self-reported expertise -- We rely on the user themselves reporting their
   expertise in the languages (See pre-questionnaire
   \ref{pilot2-pre-questionnaire})
1. Self-reported task completion -- We rely on the user themselves reporting
   that they have completed the task according to the specification
1. No way to be sure which error log pertains to which compile -- A shortcoming
   that was discovered *after* Pilot Study 2 is that the error log that captures
   when the user tries to compile syntactically/semantically correct code *does
   not log the participant ID the error pertains to*
1. No unique participant ID per Surveymonkey -- We can not be sure that,
   although the timestamps are similar, a person filling out the survey at time
   $t$ may not be the same person starting the task at time $t$.
1. Window dimensions not captured -- How can we be sure that a click in location
   `(x,y)` *on the user's screen and resolution* is the same `(x,y)` that I
   capture in the database? Participant 15 (See Figure
   \ref{fig:visualise-15-overlay}) very likely had a much shorter window height
   than I have used here. I suspect this is the case because of the cluster of
   mouse clicks in the same range of the $x$ axis as the Compile button, but
   much futher up in the $y$ axis, but I have no way to be sure.
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


In addition to fixing these flaws/foibles, an improvement over this experimental
method is to take people who are new -- as in: never having used Elm or JS, and
train them up either in JS or Elm, and then run the same task.  That way, their
level of ability is much more comparable. 

My current method creates quite a bit of noise in the data, because I rely on
self-reported level of expertise in JS/Functional languages. I don't know how to
modify the data to account for this. I could group the analyses into categories
-- i.e those who reported being experts at JS, those who reported never having
used it, those who reported being experts in at least one FP language, and those
who reported being new, and make cross comparisons with groups of equal levels
of ability.
