Harlequin Dylan Environment Training Materials
----------------------------------------------

The two half-sessions on the language were presented by Mark Tillotson.


* Contents

This directory contains the following files.

File				Description
----				-----------

readme.txt			This file
teaching-notes.txt		Notes for presentation and practical
exercises.txt			Notes for some exercises
glossary.txt			Brief (incomplete) glossary of terms
protocols.txt			Presentation note on "programming in the
				large in Dylan"

* Notes

The sources from the Dylan Programming book were used in the practical.
Projects for these in a form suitable for Harlequin Dylan 1.1 can be found
in HOPE under D-examples-dylan-programming.

These sources are also available for download from the Harlequin
website, at

<URL:http://www.harlequin.com/education/books/dylan-book/dylan-33.html#MARKER-9-1>

but these are in an inconvenient form.

  1) There are bugs in the code, so you'll get warnings (and errors?) if you
     compile it with Harlequin Dylan.
  2) Harlequin Dylan creates subdirectories for each project when building,
     so if all the projects are in one directory (as they are by default in
     that download), you may get clashes.  (This should be less of a problem
     from version 1.1, which names the directories "<project-name>-build",
     but better safe than sorry!)
  3) The download comes with LID files but no HDP files, so subprojects have
     to be located by hand and set to be DLLs.  I've already done this, so
     it will make your life easier!

