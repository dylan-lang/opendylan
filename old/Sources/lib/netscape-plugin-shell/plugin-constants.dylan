module: netscape-plugin-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

/*
 * Values for mode passed to NPP_New:
 */
define constant $NP-EMBED = 1;
define constant $NP-FULL =  2;

/*
 * Values for stream type passed to NPP-NewStream:
 */
define constant $NP-NORMAL =	 1;
define constant $NP-SEEK =	 2;
define constant $NP-ASFILE =	 3;
define constant $NP-ASFILEONLY = 4;

// ??define constant $NP-MAXREADY = (((unsigned)(~0)<<1)>>1);





/*----------------------------------------------------------------------*/
/*                   Error and Reason Code definitions                  */
/*----------------------------------------------------------------------*/

/*
 *	Values of type NPError:
 */


define constant $NPERR-BASE =			    0;
define constant $NPERR-NO-ERROR =		    ($NPERR-BASE + 0);
define constant $NPERR-GENERIC-ERROR =		    ($NPERR-BASE + 1);
define constant $NPERR-INVALID-INSTANCE-ERROR =	    ($NPERR-BASE + 2);
define constant $NPERR-INVALID-FUNCTABLE-ERROR =    ($NPERR-BASE + 3);
define constant $NPERR-MODULE-LOAD-FAILED-ERROR =   ($NPERR-BASE + 4);
define constant $NPERR-OUT-OF-MEMORY-ERROR =	    ($NPERR-BASE + 5);
define constant $NPERR-INVALID-PLUGIN-ERROR =	    ($NPERR-BASE + 6);
define constant $NPERR-INVALID-PLUGIN-DIR-ERROR =   ($NPERR-BASE + 7);
define constant $NPERR-INCOMPATIBLE-VERSION-ERROR = ($NPERR-BASE + 8);
define constant $NPERR-INVALID-PARAM =		    ($NPERR-BASE + 9);
define constant $NPERR-INVALID-URL =		    ($NPERR-BASE + 10);
define constant $NPERR-FILE-NOT-FOUND =		    ($NPERR-BASE + 11);
define constant $NPERR-NO-DATA =		    ($NPERR-BASE + 12);
define constant $NPERR-STREAM-NOT-SEEKABLE =	    ($NPERR-BASE + 13);

/*
 *	Values of type NPReason:
 */
define constant $NPRES-BASE =	     0;
define constant $NPRES-NETWORK-ERR = ($NPRES-BASE + 0);
define constant $NPRES-USER-BREAK =  ($NPRES-BASE + 2);
define constant $NPRES-DONE =	     ($NPRES-BASE + 3);
