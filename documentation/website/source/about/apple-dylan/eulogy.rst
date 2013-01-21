******************
Apple Dylan Eulogy
******************

Paul R Potts writes:

In 1994 I was a beta user of the Dylan Technology Release put out by
:doc:`apple-cambridge` R&D.

:doc:`index` was an amazing development environment: the most
sophisticated and highly leveraging environment that I've ever seen,
before or since, with the second place position belonging to Apple's
NewtonScript. (I'm told a Lisp Machine might be comparable, but I've
never seen one).

Unfortunately, it was ahead of its time: it required what was then a
considerable amount of memory (20 megabytes or more), was fairly
crash-prone and somewhat incomplete, and was unfortunately quite slow.
(On a 68040-based Quadra 800, or a 68030-based PowerBook Duo, more than
"quite" slow -- it was agonizing, and I consider myself to be a very
patient guy).

The IDE did very ambitious things, and did them well enough to prove the
concept, but using it for real work was very difficult. It is not quite
clear to me why it was so slow. I'm told that MCL is not slow, but
perhaps the demands that the Dylan TR placed on it were a little much
for it; I sometimes found myself staring at the "GC" cursor for many
minutes at a time.

I contributed what testing I could (within the limits of my patience)
and also submitted a small program to draw fractals based on
string-rewriting L-systems. Even with my poor knowledge of Lisp and poor
choice of algorithms, it performed decently, indicating to me that Dylan
could be compiled to reasonably efficient code, if not extremely
efficient.

Dylan was, in part, an attempt to build a bridge between static language
users and the dynamic language community. The Cambridge team wisely
understood that C programmers would be generally unwilling to convert to
a parenthesized, prefix syntax: for this reason, Dylan was given a
Pascal-like syntax. Programmers who have used Lisp generally come to
feel comfortable with it; I've used Scheme enough now to feel be
somewhat accustomed to the style, but I will probably always prefer a
non-Lispy syntax. Computer science types can say "that's just syntax; it
is irrelevant," but in fact many barriers to language adoption are
cultural and practical, not technical.

There have been, and continue to be, a lot of unspoken assumptions that
make it hard for the two communities to cross over. For example, I had
never used Scheme or Lisp lists, and did not realize that Dylan lists
were built using the Lisp "pair" idiom, and thus that in normal
operation, inserting items into a Dylan <list> would result in the items
appearing in reverse order.

This was not made clear in the documentation, because to those with a
Scheme or Lisp background it was obvious. But in C code, building a
linked list is generally done by walking to the end and hooking nodes to
the tail. Currying and closures were completely mysterious to me. But,
slowly, I've come to understand these things that are very basic in the
Lisp community and now use closures and currying in my Dylan code.

It seems to be possible, to this day, to graduate from a computer
science program having learned _either_ the idioms and scenery of the
Lisp/Scheme world, _or_ the rules and scenery or the C/C++/Java/C#
world, but not both. We just generally aren't willing to venture into
each other's alien landscape. The static language community continues to
make marginal improvements to its languages, not realizing the
improvements they are discovering are they are discovering are twenty or
more years old. But that is the subject for some ranting another day.

I have collected some screen shots to illustrate the amazing user
interface of the Apple Dylan technology release. These were captured on
my PowerBook G4/400. The Dylan TR will still run under MacOS 9.2.2, but
will not run at all in the "classic" emulation environment.

Since some of the images are already quite large, I have reduced the
color depth to an 8-bit web-safe palette. This has made some of the
icons look a little less attractive than they did in their original
color scheme, but the difference is not very noticeable.

- :doc:`screenshots/index`

Enjoy.

--------

RainerJoswig adds:

MCL as the implementation environment for Apple Dylan
=====================================================

MCL itself has an advanced GC implementation. It has one limitation
though, it uses a fixed amount of memory that can't be expanded at
runtime. So, to run the Dylan IDE without seeing too much GCs you might
need to give the MCL application, the Dylan IDE in this case, a lot of
memory (use the Info dialog in the Finder to set the memory size) - to
as much as you like - say, 50 MByte. A full GC of a 50MB MCL takes under
a second on a current Mac. Make sure that the memory is RAM and not
virtual memory on disk, which would slow things down. Another thing for
MCL experts is to turn on the EGC (Ephemeral Garbage Collection) by
calling (egc t) in an MCL listener. The EGC looks for garbage on memory
pages that have been changed recently - with support from the MMU. The
effect is that the EGC runs frequently and unnoticeably - so full GCs
will not happen that often.

Sk8
===

There are several cool IDEs implemented on top of MCL. Another one done
by Apple is SK8. SK8 has an even cooler environment than Apple Dylan and
has been used in dozens of projects many years ago. The programming
language of SK8 is a kind of object-oriented AppleScript (written in
MCL). Here is a `picture of Sk8 running on a PowerMac
<../../_static/images/sk8.jpg>`_. Sk8 has been released by Apple as source code.

Apple Dylan influences
======================

Apple Dylan's IDE is also a bit influenced by Smalltalk 80. In Smalltalk
you also develop code in a kind of database. Mac-based real cool
Interface Builders were done first in LeLisp (later ported to Objective
C at NeXT). Interesting stuff done with Apple Dylan were also a first
shot of the Newton OS, a study of the Finder in Dylan and a Dylan
version of MacApp (Apple's early framework for object-oriented
application development).

More radical dynamic development environments
=============================================

In this category I would think fall things like the Symbolics Genera
environment, Intellicorp's KEE, some Smalltalks and Xerox's InterLisp D.

New in the Apple Dylan IDE
==========================

What I think was relatively new are the remote debugging capabilities of
running Dylan applications from within the Apple Dylan IDE. The
NewtonScript environment does allow that only primitively. Also new was
the use of an Interface Builder from within a running Apple Dylan
application, without having the IDE inside the application.

New version of Apple Dylan?
===========================

Currently Digitool has rights to Apple Dylan. Maybe they would port it
to MCL 5.0 and MacOS X - if you throw tons of money at them, that is.
