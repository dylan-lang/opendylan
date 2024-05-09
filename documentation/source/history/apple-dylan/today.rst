*****************
Apple Dylan Today
*****************

Paul R Potts writes:

I frequently hear from people who are interested in trying out the
:doc:`technology-release` (TR) today, even knowing full well that it is
an old prototype with no support from Apple, running on an operating
system (MacOS 9.2.2) which has been declared dead.

The outlook is grim for the Technology Release. Apple Dylan was patched
to run on the PowerPC architecture, but that was many years ago. There
were many unfixed bugs, and they have remained unfixed. As system
updates have been released, the stability of the Apple Dylan TR has been
further degraded. (And, remember, it was not that solid to begin with,
even when used on a 68040-based system such as a Quadra 800).

There was a problem with the icon. I think the creator type "dyln" was
later re-used by the OS for generic shared libraries. Using ResEdit, I
changed the creator type to "diln" and removed some apparently
extraneous icons, and the TR has its icon back.

Right now, I am using the Apple Dylan TR on a G4 PowerBook (400 MHz)
with 512 megabytes of RAM. To get it to work properly, I had to make
sure that the PPC Exception Enabler extension was NOT in the extensions
folder, and turn off virtual memory.

The TR still runs with painful slowness, despite the ridiculous speed
difference between the G4 and 68040 processor family. It does not seem
to make much of a difference. You'll have to wait long minutes to build
and launch a project.

I gave the Dylan application a 125-megabyte partition. I was having
trouble launching even the simplest project, "Hello World," but
rebuilding the project with all sub-projects (which still takes a long,
long time) fixed the problem.

My applications will run and interactive debugging is possible. So, I
decided to try it out with some new Dylan code that I wanted to use on
three platforms: Functional Objects, Gwydion, and Apple Dylan. I was
hoping to use the browsers to better visualize my code. And there is the
LID interchange file format: this is supposed to make exchanging Dylan
with text-based tools easier.

After a few hours, several complete from-scratch rebuilds of the
project, many strange error messages, and one hard lock-up, I've decided
that the cool IDE just isn't worth it: I can't get any real work done.

- Importing source failed miserably, so I had to copy and paste from
  a text editor record-by-record.
- There are some language compatibility issues: Apple Dylan does not
  seem to provide "format-out" or "define function" or "type-union" or
  "limited." I no longer have the big set of paper documentation that
  came with the Technology Release, and it does not come with online
  versions of these books. So, hack workarounds were immediately needed
  to share source with Gwydion and Fun-o. So much for common source.
- Then, the weird problems began. Errors about being unable to compile
  a method, but the method in question was not indicated. Warnings that
  would not show up in the source records. Incomprehensible IDE errors
  popped up.
- After tossing out the project completely, rebooting, and starting over
  twice, my patience had reached its end. Interactive debugging is a
  wonderful thing, but only when it is my code that I'm debugging.

All this is quite a shame. All software goes to software heaven (or
hell) one day, but :doc:`index` never even reached adulthood. If you
have the chance, try it out. It is still inspiring to witness the
elegance and potential of this environment.

Meanwhile, please take a look at my :doc:`screenshots/index`. It's a bit
hard to capture such a dynamic environment in action this way, but maybe
something remains to inspire you.
