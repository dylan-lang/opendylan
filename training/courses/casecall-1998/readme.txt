CaseCall Training
=================

* Overview

This document gives an overview of the internal Dylan training course which
was presented over the working week of 6th-10th July 1998.

The course structure is given in terms of its timetable.  The key maps
abbreviations in the timetable to the full topic names, along with the
subdirectories in which you can find files related to that topic.  Each
directory contains more information on how the files were (intended to be)
used for each session, in a "readme.txt" file.  (Note that the "slides" and
presentation notes may not be sufficient to recreate the presentation
without some work!)

At the end are some miscellaneous sections: details of hardware and software
used; and where to find feedback info from the trainees (and trainers). 


* Credits :-)

The trainers, in order of appearance, were Mark Tillotson <markt>, Hugh
Greene <hughg>, Keith Playford <keith> and Jason Trenouth <jason>. The
attendees were Ian Chisholm <ianc>, Paul Miller <paulm>, Andrew Innes
<andrewi>, Rowan Dodd <rowand> and Stuart Croy <stuartc>. Many thanks to
Adrian Patterson <adrianp> for sysadmin support. 


* Timetable

This was the planned timetable, to which we mostly adhered.  (This is also
available as the Word document "Timetable.doc" in this directory, in case
anyone wants to replicate it neatly.)

	Mon	Tue	Wed	Thur	Fri
09:00	Overview DUIM1	C-FFI	ODBC	CORBA Overview
10:00	Lang	DUIM1 	C-FFI	ODBC	Client-side
11:00	---------------- Coffee break ----------------
11:30	Env	DUIM1	C-FFI	ODBC	Server-side
12:30	------------------- Lunch --------------------
13:30	LangEnv2	DUIM2	Effic.	OLEAut	Libraries Detail
14:30	LangEnv2	DUIM2	Effic.	OLEAut	Modify client
15:30	---------------- Coffee break ----------------
16:00	LangEnv2	DUIM2	Effic.	OLEAut	Review (of week)
17:00	------------------ Go Home! ------------------

Key:
	Abbrev	Topic				Directory
	------	-----				---------

	Lang	Language Intro			language
	Env	Environment Intro			environment
	LangEnv2	Language and Environment 2		language, environment
	DUIM1	Duim Intro			duim
	DUIM2	Advanced DUIM			duim
	C-FFI	C Foreign (Function) Interface	c-ffi
	Effic.	Efficient Dylan			efficiency
	-	ODBC				odbc
	OLEAut	OLE Automation			ole-automation
	-	CORBA Overview (etc.)		corba


* Hardware and Software Used

** Hardware

o  6 Pentium-Something 200Mhz (?) PCs with 128Mb of RAM
o  One nifty LCD projector
o  Copies of the DRM and the Dylan Programming book.

** Software

o  Harlequin Dylan 1.0 Internal Edition, plus patches for
   o  CORBA support (including sockets and winsock2)
   o  DUIM Win32 resource support
   o  Err, some other environement bugs, I think, but mostly we patched the
      environment because it depended on DUIM.
   (I'd suggest any attempts to re-run this course just use the latest
   external HD release, e.g., 1.1 Enterprise.)
o  Visual C++ 5.0 (for linker)
o  Viewers for PowerPoint and Word files (with partial success!).
o  Win95 Winsock2 installation (for CORBA).
   o  It took us a while to find this on the Web.  It should be available
      now as "\\DC1-ED\pub\win95\winsock\winsock2\W95ws2setup.exe"
   o  This turns out to require that the Win95 standard TCP/IP drivers be
      installed, which they weren't, because we were on a tiny training LAN).

** Courseware

The example notes and projects used in the training can be found in HOPE
compounds under D-training-courseware (and some projects are under D-
examples).  However, that compound hierarchy may be used to store new and/or
updated training material, so all the versions used in this course have been
checkpointed with the label

  D_training_casecall_1998

and so the whole lot can be checked out with the HOPE command

  co -missing-dir force comp D-training-courseware -a -comp D-examples -rec
    -ver D_training_casecall_1998

** Day-by-day software checklist

(This list assumes you're using a Dylan release like 1.1 Enterprise or
above, rather than the patched-to-death monstrosity we had!)

Mon
	PowerPoint and Word viewers
	Harlequin Dylan version 1.1 Enterprise Edition (or better)
	Example source for HD/Dylan Intro practical

Tue
	Visual C++ 5.0
	Set HD to use MS linker
	Modify VCVARS32.BAT so it doesn't assume the presence of the VC++
           CD (see comments in that file).
	Modify AUTOEXEC.BAT to run VCVARS32.BAT

Wed
	Add HD Bin directory to path (for console-debug)
	Emacs (to capture profiling output)
	Example source for C-FFI practical
	Example source for Efficient Dylan practical

Thu
	ODBC Drivers
	Northwind database (install and register)
	Register OLE Automation server

Fri
	Winsock2 update for Win95
	Restore AUTOEXEC.BAT and VCVARS32.BAT

** Feedback

The feedback from the Review session and from various notes taken during the
course is available
  in the Dylan Notebook in Spring (PROJECTS\DYLAN\ADYLAN.nsf)
  as "Support\Training\CaseCall Training Comments".

