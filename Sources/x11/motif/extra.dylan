Module:   Motif
Synopsis: Hand-generated definitions to supplement the automatic translation.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// pointers to scalars
define C-pointer-type <XmButtonType*> => <XmButtonType>;
define C-pointer-type <XmFontContext*> => <XmFontContext>;
define C-pointer-type <XmFontListEntry*> => <XmFontListEntry>;
define C-pointer-type <XmOffset*> => <XmOffset>;
define C-pointer-type <XmOffsetPtr*> => <XmOffsetPtr>;
define C-pointer-type <XmString*> => <XmString>;
define C-pointer-type <XmStringCharSet*> => <XmStringCharSet>;
define C-pointer-type <XmStringComponentType*> => <XmStringComponentType>;
define C-pointer-type <XmStringContext*> => <XmStringContext>;
define C-pointer-type <XmStringDirection*> => <XmStringDirection>;
define inline constant <XmTextPosition*> = <C-long*>;

// pointers to pointers
define C-pointer-type <XmSecondaryResourceData*>
  => <XmSecondaryResourceData>;
define C-pointer-type <XmSecondaryResourceData**>
  => <XmSecondaryResourceData*>;
define C-pointer-type <XmClipboardPendingList*> => <XmClipboardPendingList>;
define C-pointer-type <C-int**> => <C-int*>; // used only in "Xm/List.h"

// enumeration types
define inline constant <XmHighlightMode> = <C-int>;
define inline constant <XmTextDirection> = <C-int>;
