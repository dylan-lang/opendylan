Module:    Win32-common
Synopsis:  Additional declarations to be loaded before the automatically
	   converted files.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// A commonly used special case:
define constant $FFFFFFFF :: <machine-word> =
  as(<machine-word>, -1); // #xFFFFFFFF

/*   // was used temporarily before `limited' worked:
// integer subranges used internally:
define constant <U8> = <integer>;
define constant <U16> = <integer>;
define constant <U32> = <abstract-integer>;
define constant <S16> = <integer>;
define constant <ambiguous-short> = <integer>;
*/

// integer subranges used internally:
define constant <U8> = limited(<integer>, min: 0, max: #xFF);
define constant <U16> = limited(<integer>, min: 0, max: #xFFFF);
//define constant <U32> = limited(<abstract-integer>, min: 0, max: 0xFFFFFFFF);
define constant <S16> = limited(<integer>, min: -#x8000, max: #x7FFF);
define constant <ambiguous-short> =	// 16 bits, signed or unsigned
  limited(<integer>, min: -#x8000, max: #xFFFF);


// Dylan types corresponding to the builtin C types -- Probably should be in C-FFI?
// Those type which are 32-bits wide also accept <machine-word> as the C-FFI will
// return <machine-word> if needed for those types ...

define constant <signed-char> = limited(<integer>, min: -#x80, max: #x7f);
define constant <unsigned-char> = limited(<integer>, min: 0, max: #xff);

define constant <signed-short> = limited(<integer>, min: -#x8000, max: #x7fff);
define constant <unsigned-short> = limited(<integer>, min: 0, max: #xffff);

define constant <signed-long> =
  type-union(<integer>, <machine-word>);
define constant <unsigned-long> = 
  type-union(limited(<integer>, min: 0), <machine-word>);

define constant <signed-int> = <signed-long>;
define constant <unsigned-int> = <unsigned-long>;

/* // Can't do this because of sealing
define sealed inline method c-type-cast 
    ( type == <signed-long>, value :: <object> ) => (value :: <signed-long>)
  c-type-cast(<C-both-signed-long>, value)
end method c-type-cast;

define sealed inline method c-type-cast 
    (type == <unsigned-long>, value :: <object>) => (value :: <unsigned-long>)
  c-type-cast(<C-both-unsigned-long>, value)
end method c-type-cast;
*/

// special case types from "windef.h":

define constant <FARPROC> = <C-function-pointer>;
define constant <PROC> = <C-function-pointer>;

define constant <C-BYTE*> = <C-unsigned-char*>;

// These types could hold either an integer or a pointer:
define constant <LPARAM>  = <C-both-long>;
define constant <LRESULT> = <C-both-long>;
define constant <WPARAM>  = <C-both-unsigned-int>;
// In menu functions such as AppendMenu, UINT parameters can be passed
// a handle address.
define constant <UINT>    = <C-both-unsigned-int>;
define constant <PUINT>   = <C-both-unsigned-int*>;
define constant <LPUINT>  = <PUINT>;

define constant <LPULONG>  = <PULONG>;

define C-pointer-type <C-void**> => <C-void*>; // maybe should be in C-FFI ?
define constant <LPLPVOID> = <C-void**>;


// The following correspond to macros in "windef.h":

define generic LOWORD ( n :: <object> ) => value :: <U16>;
define generic HIWORD ( n :: <object> ) => value :: <U16>;

define inline method LOWORD ( n :: <integer> ) => value :: <U16>;
    logand(n,#xFFFF)
end LOWORD;

define inline method HIWORD ( n :: <integer> ) => value :: <U16>;
    logand( ash(n,-16), #xFFFF)  
end HIWORD;

define inline method LOWORD ( n :: <machine-word> ) => value :: <U16>;
  as(<integer>, %logand(n, as(<machine-word>, #xFFFF)))
end LOWORD;

// Not inline because of Bug 2262.
define method HIWORD ( n :: <machine-word> ) => value :: <U16>;
  as(<integer>, u%shift-right(n,16))
end HIWORD;

define method LOWORD ( n :: <double-integer> ) => value :: <U16>;
  LOWORD(%double-integer-low(n))
end LOWORD;

define method HIWORD ( n :: <double-integer> ) => value :: <U16>;
  HIWORD(%double-integer-low(n))
end HIWORD;

define sealed method MAKELONG ( wLow :: <ambiguous-short>,
			        wHigh :: <ambiguous-short> )
			=> value :: <unsigned-long>;
  let low :: <integer> = logand(wLow, #xFFFF);
  if ( wHigh > #x0FFF | wHigh < 0 )
    %logior(low, u%shift-left(as(<machine-word>, logand(wHigh, #xFFFF)), 16))
  else
    logior(low, ash(wHigh,16))
  end if
end MAKELONG;

define sealed method MAKELONG ( wLow :: <boolean>, wHigh :: <object> )
	=> value :: <unsigned-long>;
  MAKELONG( (if(wLow) 1 else 0 end if), wHigh)
end MAKELONG;


// The following is a substitute for MAKEPOINT, MAKEPOINTS, and LONG2POINT.
// Unpacks a 32-bit value into two signed 16-bit numbers
define function LPARAM-TO-XY( lparam ) => ( x :: <S16> , y :: <S16> );

  local method extend-short( u :: <U16> ) => s :: <S16>;
	  // sign-extend a 16-bit number
	  if ( logand( u, #x8000 ) = 0 )
	    u
	  else
	    logior( u, lognot(#x7FFF) );
	  end if;
	end extend-short;

  values( extend-short(LOWORD(lparam)), extend-short(HIWORD(lparam)) )
end LPARAM-TO-XY;



define inline method LOBYTE ( n :: <integer> ) => value :: <U8>;
  logand(n,#xFF)
end LOBYTE;

define inline method HIBYTE ( n :: <integer> ) => value :: <U8>;
  logand(ash(n,-8), #xFF)  
end HIBYTE;

define method LOBYTE ( n :: <machine-word> ) => value :: <U8>;
  LOBYTE(LOWORD(n))
end LOBYTE;

define method HIBYTE ( n :: <machine-word> ) => value :: <U8>;
  HIBYTE(LOWORD(n))
end HIBYTE;

define sealed method MAKEWORD ( low-byte :: <integer>, high-byte :: <integer> )
 => value :: <U16>;
  logior(logand(low-byte, #xFF), ash(logand(high-byte, #xFF),8))
end MAKEWORD;


// Macros in "winnt.h":

define inline function MAKELANGID(p :: <U16>, s :: <U16>)
  => (value :: <integer>);
   logior( ash(s, 10), p)
end MAKELANGID;

define inline function PRIMARYLANGID(lgid :: <integer>) => (value :: <U16>);
  logand(lgid,#x3FF)
end PRIMARYLANGID;

define inline function SUBLANGID(lgid :: <integer>) => (value :: <U16>);
  ash( lgid, -10 )
end SUBLANGID;

define inline function MAKELCID(lgid :: <U16>, srtid :: <U16>)
	=> (value :: <integer>);
  MAKELONG(lgid, srtid)
end;

define inline function LANGIDFROMLCID(lcid) => (value :: <U16>);
  LOWORD(lcid)
end;

define inline function SORTIDFROMLCID(lcid) => (value :: <U16>);
  HIWORD( logand(lcid, $NLS-VALID-LOCALE-MASK) )
end;


// This is declared in "winnt.h" in a way that is too clumsy for automatic
// conversion, but we want to do it smarter anyway:


define C-struct <LARGE-INTEGER>
  sealed slot LowPart-value  :: <C-both-unsigned-long>;
  sealed slot HighPart-value :: <C-both-signed-long>;
  pointer-type-name: <PLARGE-INTEGER>;
  end;

// define constant <LPLARGE-INTEGER> = <PLARGE-INTEGER>;

define method pointer-value( pointer :: <PLARGE-INTEGER>, #key index = 0)
	=> value :: <abstract-integer>;
  unless ( index == 0 )
    pointer := pointer-value-address(pointer, index: index);
  end;
  let high = pointer.HighPart-value;
  let low  = pointer.LowPart-value;
  if ( instance?(low, <integer>) & (high = ash(low, -31)) )
    low
  else
    make(<double-integer>,
	 low: as(<machine-word>, low),
	 high: as(<machine-word>, high))
  end if
end;

define method pointer-value-setter( value :: <integer>,
				   destination :: <PLARGE-INTEGER>,
				   #key index = 0)
 => (value :: <integer>);
  unless ( index == 0 )
    destination := pointer-value-address(destination, index: index);
  end;
  destination.HighPart-value := ash(value, -31); // sign-extend
  destination.LowPart-value  := value
end;

define method pointer-value-setter( value :: <double-integer>,
				   destination :: <PLARGE-INTEGER>,
				   #key index = 0)
 => (value :: <double-integer>);
  unless ( index == 0 )
    destination := pointer-value-address(destination, index: index);
  end;
  destination.LowPart-value  := %double-integer-low(value);
  destination.HighPart-value := %double-integer-high(value);
  value
end;

define method pointer-value-setter( value :: <machine-word>,
				   destination :: <PLARGE-INTEGER>,
				   #key index = 0)
 => (value :: <machine-word>);
  unless ( index == 0 )
    destination := pointer-value-address(destination, index: index);
  end;
  destination.HighPart-value := 0; // assuming unsigned
  destination.LowPart-value  := value
end;

define C-struct <ULARGE-INTEGER>
  sealed slot LowPart-value  :: <C-both-unsigned-long>;
  sealed slot HighPart-value :: <C-both-unsigned-long>;
  pointer-type-name: <PULARGE-INTEGER>;
  end;

define method pointer-value( pointer :: <PULARGE-INTEGER>, #key index = 0)
	=> value :: <abstract-integer>;
  unless ( index == 0 )
    pointer := pointer-value-address(pointer, index: index);
  end;
  let low = pointer.LowPart-value;
  let high = pointer.HighPart-value;
  if ( zero?(high) & instance?(low, <integer>) & ~ negative?(low) )
    low
  else
    make(<double-integer>,
	 low: as(<machine-word>, low),
	 high: as(<machine-word>, high))
  end if
end;

define method pointer-value-setter( value :: <unsigned-long>,
				   destination :: <PULARGE-INTEGER>,
				   #key index = 0)
 => (value :: <unsigned-long>);
  unless ( index == 0 )
    destination := pointer-value-address(destination, index: index);
  end;
  destination.HighPart-value := 0;
  destination.LowPart-value  := value
end;

define method pointer-value-setter( value :: <double-integer>,
				   destination :: <PULARGE-INTEGER>,
				   #key index = 0)
 => (value :: <double-integer>);
  unless ( index == 0 )
    destination := pointer-value-address(destination, index: index);
  end;
  destination.LowPart-value  := %double-integer-low(value);
  destination.HighPart-value := %double-integer-high(value);
  value
end;

define method initialize (pointer :: <PULARGE-INTEGER>, #key value, #all-keys)
  next-method();
  if ( value )
    pointer-value(pointer) := value;
  end;
  values()
end method initialize;

define method initialize (pointer :: <PLARGE-INTEGER>, #key value, #all-keys)
  next-method();
  if ( value )
    pointer-value(pointer) := value;
  end;
  values()
end method initialize;



// This equivalent for the Win32 `TEXT' macro will need some more work
// if and when we support using the Unicode version of the API for NT.
// NOTE -- This implementation relies on the runtime padding strings with a null!
// WARNING -- This implementation can have problems with the garbage collector
//   as the GC won't know that C might have saved a pointer to the string!
//   But it should only be used on literals anyway.

define generic TEXT (string :: <string>) => (value :: <C-string>);

define inline method TEXT (string :: <byte-string>) => (value :: <C-string>)
  C-string-constant(string)
end method;

define inline method TEXT (string :: <C-string>) => (value :: <C-string>)
  string
end method;


// The following slot accessor functions need to be declared as open generics
// because they have methods defined in more than one library. 

define macro open-accessor-definer
  { define open-accessor ?name:name } =>
    { define open inline-only generic ?name (struct) => (slot-value);
      define open inline-only generic ?name ## "-setter" 
          (new, struct) => (new); }
end macro;

define open-accessor x-value;
define open-accessor y-value;
define open-accessor cx-value;
define open-accessor cy-value;
define open-accessor left-value;
define open-accessor top-value;
define open-accessor right-value;
define open-accessor bottom-value;


// This isn't used in this library, but methods are defined in both
// "win32-dialog" and "win32-controls": 

define open-accessor flags-value;

// This isn't used in this library, but methods are defined in both
// "win32-kernel" and "win32-user": 
define open-accessor offset-value;
define open-accessor lpData-value;

// Defined in win32-user, win32-rich-edit, and COM:
define open-accessor cbSize-value;

// defined here and in OLE-Dialogs:
define open-accessor Flink-value;

// defined in win32-kernel, Win32-DDE, and OLE-Controls:
define open-accessor cb-value;

// defined in win32-kernel, Win32-controls, OLE-Controls, and Ole-Dialogs:
define open-accessor dwFlags-value;

// defined in win32-kernel, win32-user, and win32-GDI:
define open-accessor cbData-value;

// defined in win32-kernel and win32-GDI:
define open-accessor dwSize-value;
define open-accessor wFlags-value;
define open-accessor u-value;
define open-accessor wSecond-value;

// defined in win32-GDI, COM, and winsock2:
define open-accessor Buffer-value;
