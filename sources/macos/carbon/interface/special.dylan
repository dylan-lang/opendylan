Module:    carbon-interface
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

///---*** NOTE: Maybe provide a way to specify a maximum size instead of a type?
define macro with-pascal-string
  { with-pascal-string (?var:name = ?string:expression)
     ?:body
    end }
    => { with-pascal-string (?var = ?string, type: <Str255*>)
	   ?body
	 end }
  { with-pascal-string (?var:name = ?string:expression, type: ?type:name)
     ?:body
    end }
    => { begin
	   let _strsize = size(?string);
	   with-stack-structure (?var :: ?type)
	     ?var[0] := _strsize;
	     for (_i :: <integer> from 0 below _strsize)
	       ?var[_i + 1] := as(<integer>, ?string[_i]);
	     end;
	     ?body
	   end
	 end }
end with-pascal-string;

/// Used by some patterns as a special hack ...
define inline constant $FFFFFFFF = as(<machine-word>, #xFFFFFFFF);

///---*** NOTE: Is this good enough for array?
define macro C-array-definer
  { define C-array "<" ## ?name:name ## ">"
        ?basetype:name [ ?size:expression ] }
    => { define C-struct "<" ## ?name ## ">"
           sealed inline-only array slot ?name ## "-data" :: ?basetype,
             length: ?size;
           pointer-type-name: "<" ## ?name ## "*>";
         end C-struct;
	 define sealed inline method element
             (x :: "<" ## ?name ## "*>", i :: <integer>, #key default) 
          => (value)
	   ?name ## "-data"(x, i)
         end method element;
	 define sealed inline method element-setter
             (value, x :: "<" ## ?name ## "*>", i :: <integer>) => (value)
	   ?name ## "-data"(x, i) := value
         end method element-setter; } 
end macro C-array-definer;

///---*** NOTE: We need proper definitions of these arrays and code to handle Pascal strings
define C-array <Str255> <C-unsigned-char>[256];
define C-array <Str63>  <C-unsigned-char>[64];
define C-array <Str32>  <C-unsigned-char>[33];
define C-array <Str31>  <C-unsigned-char>[32];
define C-array <Str27>  <C-unsigned-char>[28];
define C-array <Str15>  <C-unsigned-char>[16];
define C-array <Str32Field> <C-unsigned-char>[34];  //---*** EVEN MORE SPECIAL!

define C-array <Bits16>      <C-signed-short>[16];
define C-array <FormatOrder> <C-signed-short>[1];
define C-array <OffsetTable> <OffPair>[3];
define C-array <KeyMap>      <C-unsigned-long>[4];
define C-array <CSpecArray>  <ColorSpec>[1];
define C-array <MCTable>     <MCEntry>[1];
define C-array <ScrpSTTable> <ScrpSTElement>[1601];

/// Opaque structures which are, by definition, not defined in the header files ...
define inline constant <OpaqueCollection*>        = <C-void*>;
define inline constant <OpaqueDragReference*>     = <C-void*>;
define inline constant <OpaqueIconRef*>           = <C-void*>;
define inline constant <OpaqueCFragConnectionID*> = <C-void*>;
define inline constant <OpaqueCFragClosureID*>    = <C-void*>;
define inline constant <OpaqueCFragContainerID*>  = <C-void*>;
define inline constant <OpaqueCFragContextID*>    = <C-void*>;

/// Pointer types that are referenced in parameters but never actually defined ...
define C-pointer-type <Handle*>            => <Handle>;
define C-pointer-type <TECInfoHandle*>     => <TECInfoHandle>;
define C-pointer-type <EvQElPtr*>          => <EvQElPtr>;
define C-pointer-type <GDHandle*>          => <GDHandle>;
define C-pointer-type <PixMapHandle*>      => <PixMapHandle>;
define C-pointer-type <GrafPtr*>           => <GrafPtr>;
define C-pointer-type <CGrafPtr*>          => <CGrafPtr>;
define C-pointer-type <AEEventHandlerUPP*> => <AEEventHandlerUPP>;
define C-pointer-type <AliasHandle*>       => <AliasHandle>;
define C-pointer-type <AuxWinHandle*>      => <AuxWinHandle>;
define C-pointer-type <IconGetterUPP*>     => <IconGetterUPP>;
define C-pointer-type <IconFamilyHandle*>  => <IconFamilyHandle>;
define C-pointer-type <C-void**>           => <C-void*>;
define C-pointer-type <ControlHandle*>     => <ControlHandle>;
define C-pointer-type <AuxCtlHandle*>      => <AuxCtlHandle>;
define C-pointer-type <MenuHandle*>        => <MenuHandle>;
define C-pointer-type <ModalFilterUPP*>    => <ModalFilterUPP>;

///---*** NOTE: TEMPORARY: I don't feel like converting Sound.h today ...
define inline-only C-function SysBeep
  parameter duration :: <C-short>;
  c-name: "SysBeep";
  c-modifiers: "pascal";
end;

///---*** NOTE: Need to do a proper 64-bit implementation but for now ...
define inline constant <SInt64> = <Wide>;
define inline constant <UInt64> = <UnsignedWide>;
