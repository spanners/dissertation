# Conclusions

In this dissertation I have managed to achieve the product of an extended IDE
that is capable performing user participation studies by logging input device
data (Section \ref{implementation}). I have illustrated it's utility as a way of
modelling cognitive load by way of conducting a user study (Section
\ref{pilot2}), whose findings have shown that there is statistical significance
in the amount of user activity depending on the difficulty of the task, language
being used, and the relevance of the code to the task at hand (Section
\ref{pilot2-analysis}).

In light of the original objectives, I have not been able to assert the claims
Evan made in his senior thesis that Elm is indeed easy -- in the Discussion of
the second Pilot Study (Section \ref{pilot2-discussion}) I list reasons that the
interpretation I can make from the results is a lot weaker than I had
anticipated, due to the flaws in the experimental methodology and the execution
of the study. The Further Work Section (See \ref{further-work}) suggests ways in
which one could pursue a more rigorous study using the existing tools I have
created.

A caveat I would suggest to others doing a similar project: I could not
find much in the way of background/related work that has done what
I have done. Thus, I took a risk in pursuing this research, and I feel I have
provided important groundwork in the study of cognitive load, as well as
providing a set of IDE extensions to do so, remotely.

I have learned a lot about experimental methodologies -- the importance of Pilot
studies, how to perform Thematic Analysis (and where it is suitable). I have
also learned about responding to user feedback and my own observations in these
pilot studies to produce tighter hypotheses and implement experiments to test
them. 

## Further work \label{further-work}

In addition to fixing the flaws/foibles identified (Section \ref{pilot2-discussion}), an improvement over this experimental
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

Furthermore, can I be sure that the operationalisation of thrash (the concept) I
have chosen, i.e. cementing the concept by a metric to model cognitive load, is
a positive indicator?

I would love to continue pursuing this research with a view to exploring these
questions, and am grateful for all the work Bret Victor, Evan Czaplicki and
Lopez et. al., that have provided the catalyst that started this dissertation.
