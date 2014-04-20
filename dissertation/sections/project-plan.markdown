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

![Gantt Chart\label{fig:gantt}](images/gantt-chart.png)
