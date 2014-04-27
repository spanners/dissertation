# Experimental methodology

Here are some possible approaches I could take to analysing the paradigm of
declaritive versus imperative progamming.

1. AB Testing of the languages with the same IDE?

    The primary direction I mentioned (as echoed in my Proposal) was doing AB
    testing of Elm vs. another language (e.g. JavaScript) (i.e. the language is the
    dependent variable) using the same Concurrent FRP IDE (the independent variable).

2. Test just the paradigm?

    Test just the paradigm, eliminating
    the IDE from the experiment above. Perhaps for a Pilot study.

## Experiment process

1. Study question (e.g. Is it easy?)
2. Measurement concept (e.g. "Easy")
3. Operationalisation -- taking a measurement concept and mapping it to
   something concrete (e.g. if completing a pre-defined task the user must
   complete takes $< 5$ steps, it is 'easy' -- we can then compare instances of
   these studies given our definition of easy). This is much like mapping a
   design to an implementation, and there is a risk of losing information, or
   ending up with a mismatched concrete instance that does not represent the
   concept we wish to convey.
4. Do another operationalisation of our measurement concept -- this allows us to
   get a different perspective of the same concept. (e.g. if total length of
   pauses during a 1 hour experiment is $< 10$ minutes, it is 'easy'). We do this
   to get 'coverage' of the measurement concept. It is a form of cross
   validation. If we see an overlap in the correlational results after analysis,
   we can make a stronger assertion that e.g. "language A is easier than
   language B.". The idea I am describing here is methodological decision-making.
5. Predict what will be the likely results of our experiments on the
   operationalised measurements. This is "feed forward validation".
6. Do the experiement.
7. Analyse the data. See if the data has patterns that correlate with the
   assertion I wish to make. I will be representing the raw data in some outcome
   measure -- that is
   turning the raw data into a set of (or a single) value for comparison. 
8. Does the data answer the study question I set out to ask? This is now "feed
   backwards validation".
9. Write-up including the 'nitty-gritty' of the user study, and a statement like
   "Given our definition of easy, our multiple operationalisations of the
   concept of easy show that this is in fact objectively true/false".

## Pilot Studies

What might be surprising insights into declarative programming languages for
User Interface Design in the case of Elm? I may explore Speak-aloud protocols
where I prompt/facilitate the user to say what is on their mind when that e.g.
pause for more than 10 seconds -- a measurement I set out to look for during an
experiment. 

An example dialog with the user may begin (Me speaking first):

* > I notice you have paused for at least 10 seconds -- why did you?
* >> I thought the code would do X, but it did Y.
* > Why did you think it would do X?
* >> ...

It is important that I must ask the participant questions designed in a way that they are not
leading.

Motivating questions for these pilot studies are:

1. What might I ask people to do?
2. How will I gather data?
3. How will I analyse the data?
