Module:   Xlib
Synopsis: Hand-generated definitions to supplement the automatic translation.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline constant <Cardinal> = limited(<integer>, min: 0); // unsigned int
define inline constant <Dimension> =
  limited(<integer>, min: 0, max: #xFFFF); // unsigned short
define inline constant <Position> =
  limited(<integer>, min: -#x8000, max: #x7FFF); // signed short

define inline constant <Cardinal*> = <C-unsigned-int*>;
define inline constant <Dimension*> = <C-unsigned-short*>;
define inline constant <Position*> = <C-short*>;

define inline constant <unsigned-32-bits> =
  type-union(<integer>, <machine-word>);

define inline constant <Window> = <XID>;
define inline constant <Drawable> = <XID>;
define inline constant <Font> = <XID>;
define inline constant <Pixmap> = <XID>;
define inline constant <Cursor> = <XID>;
define inline constant <Colormap> = <XID>;
define inline constant <GContext> = <XID>;

// opaque pointer in "Xresource.h":
define C-subtype <XrmHashBucketRec*> ( <C-void*> ) end;

// pointers to scalars
define C-pointer-type <Atom*> => <C-Atom>;
define C-pointer-type <Colormap*> => <C-Colormap>;
define inline constant <KeyCode*> = <C-unsigned-char*>;
define C-pointer-type <KeySym*> => <C-KeySym>;
define inline constant <Modifiers*> = <C-unsigned-int*>;
define C-pointer-type <Pixmap*> => <C-Pixmap>;
define C-pointer-type <Window*> => <C-Window>;
define C-pointer-type <XEvent*> => <XEvent>;
define C-pointer-type <XPointer*> => <XPointer>;

// pointers to pointers
define C-pointer-type <Atom**> => <Atom*>;
define C-pointer-type <Dimension**> => <Dimension*>;
define C-pointer-type <KeyCode**> => <KeyCode*>;
define C-pointer-type <Window**> => <Window*>;
define C-pointer-type <XExtData**> => <XExtData*>;
define C-pointer-type <XFontStruct**> => <XFontStruct*>;
define C-pointer-type <XFontStruct***> => <XFontStruct**>;
define C-pointer-type <C-string**> => <C-string*>;
define C-pointer-type <C-unicode-string**> => <C-unicode-string*>;
define C-pointer-type <C-unsigned-char**> => <C-unsigned-char*>;

// pointer types referenced in the <Display> structure;
// the user doesn't need to know about these, so provide a dummy
// definition and do not export.
define C-subtype <opaque-struct-ptr> ( <C-void*> ) end;
define constant <XContextDB*>	  = <opaque-struct-ptr>;
define constant <XDisplayAtoms*>  = <opaque-struct-ptr>;
define constant <XExten*>	  = <opaque-struct-ptr>;
define constant <XFreeFuncs*>	  = <opaque-struct-ptr>;
define constant <XIMFilter*>	  = <opaque-struct-ptr>;
define constant <XInternalAsync*> = <opaque-struct-ptr>;
define constant <XKeytrans*>	  = <opaque-struct-ptr>;
define constant <XLockInfo*>	  = <opaque-struct-ptr>;
define constant <XLockPtrs*>	  = <opaque-struct-ptr>;
define constant <XSQEvent*>	  = <opaque-struct-ptr>;


define macro callback-definer
  { define callback ?new:name :: ?ftype:name = ?old:name } =>
	{ ?ftype ## "-callback-wrapper" (?new, ?old) }
end macro;


// The following slot accessor functions need to be declared as open generics
// because they have methods defined in more than one library. 

define macro open-accessor-definer
  { define open-accessor ?name:name } =>
    { define open inline-only generic ?name (struct) => (slot-value);
      define open inline-only generic ?name ## "-setter" 
          (new, struct) => (new); }
end macro;

define open-accessor background-value;
define open-accessor bits-per-rgb-value;
define open-accessor blue-mask-value;
define open-accessor border-width-value;
define open-accessor callback-value;
define open-accessor class-value;
define open-accessor client-data-value;
define open-accessor colormap-value;
define open-accessor data-value;
define open-accessor depth-value;
define open-accessor direction-value;
define open-accessor event-value;
define open-accessor flags-value;
define open-accessor foreground-value;
define open-accessor format-value;
define open-accessor green-mask-value;
define open-accessor height-value;
define open-accessor length-value;
define open-accessor name-value;
define open-accessor pixel-value;
define open-accessor red-mask-value;
define open-accessor res-class-value;
define open-accessor screen-value;
define open-accessor sibling-value;
define open-accessor stack-mode-value;
define open-accessor string-value;
define open-accessor target-value;
define open-accessor text-value;
define open-accessor value-value;
define open-accessor visual-value;
define open-accessor visualid-value;
define open-accessor width-value;
define open-accessor win-gravity-value;
define open-accessor window-value;
define open-accessor x-value;
define open-accessor y-value;

