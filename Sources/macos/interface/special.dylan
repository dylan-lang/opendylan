Module:    macos-interface
Synopsis:  Manually coded additions to the automatic translation.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///---*** NOTE: Is this in the right order for a big-endian machine???
define macro FOUR_CHAR_CODE
  { FOUR_CHAR_CODE (?c1:expression, ?c2:expression, ?c3:expression, ?c4:expression) }
    => { %logior(%shift-left(as(<machine-word>, as(<integer>, ?c1)), 24),
		 %shift-left(as(<machine-word>, as(<integer>, ?c2)), 16),
		 %shift-left(as(<machine-word>, as(<integer>, ?c3)),  8),
		             as(<machine-word>, as(<integer>, ?c4))) }
end macro FOUR_CHAR_CODE;

/// Used by some patterns as a special hack ...
define inline constant $FFFFFFFF = as(<machine-word>, #xFFFFFFFF);

///---*** NOTE: THESE DEFINITIONS ARE TEMPORARY!
///---*** NOTE: We need proper definitions of these arrays and code to handle Pascal strings
define inline constant <Str255> = <UInt8>;
define inline constant <Str63>  = <UInt8>;
define inline constant <Str32>  = <UInt8>;
define inline constant <Str31>  = <UInt8>;
define inline constant <Str27>  = <UInt8>;
define inline constant <Str15>  = <UInt8>;
define inline constant <Str32Field> = <UInt8>;  //---*** EVEN MORE SPECIAL!

///---*** NOTE: THESE DEFINITION ARE TEMPORARY!
///---*** NOTE: We need proper definitions of these arrays
define inline constant <Bits16>      = <SInt16>;  // QuickDraw.h
define inline constant <FormatOrder> = <SInt16>;  // QuickdrawText.h
define inline constant <OffsetTable> = <SInt32>;  // QuickdrawText.h
define inline constant <KeyMap>      = <UInt32>;  // Events.h
define inline constant <CSpecArray>  = <UInt32>;  // QuickDraw.h
define inline constant <MCTable>     = <UInt32>;  // Menus.h

/// Pointer types that are referenced in parameters but never actually defined ...
define C-pointer-type <Handle*>            => <Handle>;
define C-pointer-type <TECInfoHandle*>     => <TECInfoHandle>;
define C-pointer-type <EvQElPtr*>          => <EvQElPtr>;
define C-pointer-type <GrafPtr*>           => <GrafPtr>;
define C-pointer-type <AEEventHandlerUPP*> => <AEEventHandlerUPP>;
