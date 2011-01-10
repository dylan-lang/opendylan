Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*---*** Not ready for prime-time yet!

//---*** This is defined by MFC, maybe we should put this somewhere else
define constant $RT-TOOLBAR = MAKEINTRESOURCE(241);

//---*** Adapted from the VC++ code bartool.cpp in the MFC distribution
define C-struct <CTOOLBARDATA>
  slot Version-value   :: <WORD>;
  slot Width-value     :: <WORD>;
  slot Height-value    :: <WORD>;
  slot ItemCount-value :: <WORD>;
  pointer-type-name: <LPDLGTEMPLATE>;
end C-struct <CTOOLBARDATA>;

struct CToolBarData
{
	WORD wVersion;
	WORD wWidth;
	WORD wHeight;
	WORD wItemCount;
	//WORD aItems[wItemCount]

	WORD* items()
		{ return (WORD*)(this+1); }
};

*grok-resource-table*[$RT-TOOLBAR] := grok-toolbar;

*/