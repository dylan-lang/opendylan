/* $Id: version.c,v 1.1 2004/03/12 00:42:10 cgay Exp $ */

/*      Version identification string

	The prefix "@(#)" is searched for by the Unix "what" command.
	The characters following that will also be displayed by the 
	"-version" option or the "@version" function.

	If you make any changes to this program at all, please be
	sure to update this string also, so that your modified version
	can be distinguished from the original version.
 */

const char what_string [] = "@(#)gema 1.3 April 7, 1996";


/* this is just so it will appear in "strings" output: */
const char author [] = "David N. Gray <DGray@acm.org>";

