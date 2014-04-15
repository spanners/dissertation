# Tue Apr 15 15:50:38 BST 2014

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

## Mention the ways in which the study is flawed:

1. Self-reported expertise
2. Self-reported task completion
3. No way to be sure which error log pertains to which compile
1. Unique participant ID per Surveymonkey
2. Surveymonkey has taken my data hostage 
3. window dimensions?! 
4. Syntax reference 404
6. I did not capture their code solution, so relied on trust

## Results

1. Describe data collected
1. How it was analysed (I aggregated regions and looked at number of clicks per
   region (Hard/Task, Hard/Not-Task, Simple/Task, Simple/Not-Task)\*(Elm,
   JavaScript))
1. Presentation of data (summary means std dev.)
    1. `x`^2 frequency analyses
    1. `2x2x2` making 8 cells. My expected is an even distribution of clicks in
       each category, i.e. if I have 80 clicks in total across all groups, I
       expect to find 10 in each cell if there is no correlation.

