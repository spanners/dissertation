# Introduction

Evan Czaplicki, in his senior thesis "Elm: Concurrent FRP for Functional GUIs",
presents a groundbreaking unification of various styles of Functional Reactive
Programming --- Arrowized FRP, Concurrent FRP and others --- resulting in his
implementation of Elm, a Functional Reactive programming language in an aim to
"simplify the complicated task of creating responsive and usable graphical user
interfaces." \ref{Czaplicki2012elm}. However, there is not, as of yet any
evidence to support this claim and others similar to this, and the thesis has
inspired me to build further on his work. 

In this dissertation, I am going to substantiate the following claims: 

* Recording mouse-click data of users completing a programming task models
  cognitive load (Section \ref{pilot2}). I have shown that there is statistical
  significance between the number of clicks in regions over languages Elm and
  Javascript.

* Users completing a task in Elm exhibit less thrash (*defined as:* number of
  clicks in task-regions over the duration of the task) than those that completed
  the same task in Javascript (Section \ref{t-stat}).

# Motivation

<!-- The main idea of this dissertation is...

1. Here is a problem
2. It's an interesting problem
3. It's an unsolved problem
4. **Here is my idea**
5. My idea works (details, data)
6. Here's how my idea compares to other people's approaches -->

I am interested in the effect of Functional Reactive Programming [FRP]
on User Interface programming.

I first grew an interest in the field of Functional Reactive Programming
after seeing Bret Victor's "Inventing on Principle" [@Victor2012a]. His
talk claims that, in the traditional compile-run-debug cycle of coding,
"most of the developer's time is spent looking at the code, blindly
without an immediate connection to the thing they're making". He goes on
to show a side-by-side illustration of a new style of development – on
one side is the runtime preview, and on the other side is the code
pertaining to said runtime. Changes in the code update the runtime,
live. He argues that "so much of creation is discovery, and you can’t
discover anything if you can't see what you're doing" – alluding to his
earlier statement that the compile-run-debug cycle is much like this. I
would like to investigate the claims Bret Victor makes, and indeed Elm,
an instance of such a FRP, whose website also makes similar claims.

A counter-argument may be that this is much like giving a child a
chainsaw. Is it too powerful? Does this tight feedback loop cut out a
perhaps crucial pause for thought? Furthermore – is this appropriate for
all types of programming? Is it at least appropriate for User Interface
design? It has been shown that novices tend to “thrash” about, trying
out many ideas that may or may not be a solution, whereas experts think
much more about the problem at hand before proceeding with a solution
@Lopez2012a.

My goal is to answer these questions. By way of conducting user studies,
leveraging Elm with extensions to do A/B testing to illustrate it’s
effectiveness (or ineffectiveness) at enhancing User Interface Design.

As far as the scope of this project goes, I will be researching as much as is
necessary in order to meet the aims of the project listed. Should I complete
these aims, I may go on to do further user studies, or attempt to further
analyse, compare and contrast User Interface Design and Declarative/Functional
Reactive Programming languages against other methods, so as to make firmer
statements about the benefits of Elm.
