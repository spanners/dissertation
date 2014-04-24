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

**Saw some things in Pilot Study 1, also in the use of the Elm IDE I extended, I
saw some things before Pilot Study 2.**

### Hypotheses

1H. 

## Experiment

### Method

A 2×2×2 study, that is 2 Languages (Elm and JavaScript), 2 Region difficulties
(Hard and Simple) and 2 Region relevances (Relevant and Not relevant) will be
done to determine if the number of mouse clicks per region differ across
variables.

## Results

![Participant 15, Elm task (Overlaid with mouse
clicks)\label{fig:visualise-15-overlay}](images/visualise-15-overlay.png)

![Participant 18, JS task (Overlaid with mouse
clicks)\label{fig:visualise-18-overlay}](images/visualise-18-overlay.png)

See Figure \ref{fig:visualise-15-overlay} for the visualisation of participant 15
completing the Elm version of the task.

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

### Analysis

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

---------- ---------- ---------
Category   Observed   Expected
---------- ---------- ---------
relevant × 76         106.37
hard ×                                                                    
Elm                                                                 

relevant × 33         106.37
hard ×                                                                    
JS                                                                 

relevant × 487        106.37
easy ×                                                                    
Elm

relevant × 12         106.37
easy ×                                                                    
JS

irrelevant 105        106.37
× hard ×                                                                  
Elm                                                                 

irrelevant 69         106.37
× hard ×                                                                  
JS                                                                 

irrelevant 66         106.37
× easy ×                                                                  
Elm

irrelevant 3          106.37
× easy ×                                                                  
JS

**TOTAL**  **851**    **851**
---------- ---------- ---------

: Clicks per quadrant
\label{tab:pre-chisquared}


* Elm tasks in total: 15
* JS tasks in total: 4


---------------------- -------------
$\chi^2$               1633.879

degrees of freedom     7

$\rho$--value          0

Yates' $\chi^2$        1626.741

Yates' $\rho$--value   0
---------------------- -------------

: $\chi^2$ calculation of clicks per quadrant
\label{tab:post-chisquared}


**This is the chapter in which you review the outcomes, and critique the
outcomes process.**

**You may include user evaluation here too.**

## Discussion

1. Self-reported expertise
1. Self-reported task completion
1. No way to be sure which error log pertains to which compile
1. Unique participant ID per Surveymonkey
1. Surveymonkey has taken my data hostage 
1. window dimensions?! 
1. Syntax reference 404
1. I did not capture window resizing

Not capturing window resizing is problematic -- participant 15 (See Figure
\ref{fig:visualise-15-overlay}) very likely had a much shorter window height
than I have used here. I suspect this is the case because of the cluster of
mouse clicks in the same range of the $x$ axis as the Compile button, but much
futher up in the $y$ axis, but I have no way to be sure as I did not log window
dimensions.
