Module:    bulk-io-internal
Author:    Seth LaForge & Toby Weinberg
Synopsis:  bulk-io, we haul <byte>s...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// CreateFileMappingA is for bytes, W is for unicode characters.  In C
// you just #define CreateFileMapping to one or the other. Hiho...
define C-function CreateFileMapping
  parameter hFile :: <HANDLE>;
  parameter lpSecurityAttributes :: <LPSECURITY-ATTRIBUTES>;
  parameter fdwProtect :: <DWORD>;
  parameter dwMaximumSizeHigh :: <DWORD>;
  parameter dwMaximumSizeLow :: <DWORD>;
  parameter lpszMapName :: <C-string>;
  result value :: <HANDLE>;
  c-name: "CreateFileMappingA", c-modifiers: "__stdcall";
end;

define C-function MapViewOfFile
  parameter hFileMappingObject :: <HANDLE>;
  parameter dwDesiredAccess :: <DWORD>;
  parameter dwFileOffsetHigh :: <DWORD>;
  parameter dwFileOffsetLow :: <DWORD>;
  parameter dwNumberOfBytesToMap :: <DWORD>;
  result value :: <C-char*>; // originally LPVOID
  c-name: "MapViewOfFile", c-modifiers: "__stdcall";
end;


define C-function FlushViewOfFile
  parameter lpBaseAddress :: <C-char*>; // originally LPVOID
  parameter dwNumberOfBytesToFlush :: <DWORD>;
  result value :: <BOOL>; 
  c-name: "FlushViewOfFile", c-modifiers: "__stdcall";
end;

define C-function UnmapViewOfFile
  parameter lpBaseAddress :: <C-char*>; // originally LPVOID
  result value :: <BOOL>; 
  c-name: "UnmapViewOfFile", c-modifiers: "__stdcall";
end;

// Why isn't this in the Win32 interface?
define C-function SetEndOfFile
  parameter hFile :: <HANDLE>;
  result value :: <BOOL>;
  c-name: "SetEndOfFile", c-modifiers: "__stdcall";
end;

// The section stuff is in winnt.dylan
define constant $FILE-MAP-COPY  = $SECTION-QUERY; 
ignorable($FILE-MAP-COPY);
define constant $FILE-MAP-WRITE = $SECTION-MAP-WRITE;
ignorable($FILE-MAP-WRITE);
define constant $FILE-MAP-READ  = $SECTION-MAP-READ;
ignorable($FILE-MAP-READ);

define constant $PAGE-NOACCESS  = #x01;
ignorable($PAGE-NOACCESS);
define constant $PAGE-READONLY  = #x02;
ignorable($PAGE-READONLY);
define constant $PAGE-READWRITE = #x04;
ignorable($PAGE-READWRITE);


define abstract primary class <mapped-memory> (<vector>) end class;

define sealed primary class <memory-mapped-file> (<mapped-memory>)
  slot mapped-file-accessor, 
	  required-init-keyword: accessor:;
  slot mapped-file-mapping-handle :: <HANDLE>, 
	  required-init-keyword: mapping-handle:;
  slot mapped-file-direction :: one-of(#"input", #"input-output"), 
	  required-init-keyword: direction:;
  slot mapped-file-size :: <integer>, 
	  required-init-keyword: size:;
  slot mapped-file-expanding? :: <boolean>,
	  required-init-keyword: expanding?:;
  slot mapped-file-bytes :: <C-char*>,
	  required-init-keyword: bytes:;
end class;


define method make (class == <memory-mapped-file>, #key locator, 
		    size: requested-size = #"all", direction = #"input",
		    expanding? = #f, if-exists = #f, if-does-not-exist = #f) 
		=> (r :: <memory-mapped-file>)

  let the-accessor = new-accessor(#"file", locator: locator,
				  direction: direction, if-exists: if-exists,
				  if-does-not-exist: if-does-not-exist);

  let (map-mode, view-mode) = 
    if (direction == #"input") 
      values($PAGE-READONLY, $FILE-MAP-READ)
    elseif (direction == #"input-output") 
      values($PAGE-READWRITE, $FILE-MAP-WRITE)
    else 
      error("Unrecognized direction for memory mapped file: %=", direction);
    end if;

  let file-size = the-accessor.accessor-file-size;
  let map-size = 
    if (requested-size == #"all") file-size
    elseif (instance?(requested-size, <integer>))
      if ((requested-size > file-size) & (direction == #"input-output"))
	// CreateFileMapping will automagically grow the file.
	requested-size
      elseif (requested-size <= file-size)
	requested-size
      else
	 error("Size requested for memory mapped input larger than file size")
      end if
    else
      error("Requested size for memory mapped file must be #\"all\" or a "
	    "positive integer, not %=", requested-size)
    end if;

  if (expanding? & direction == #"input")
    error("Only input-output memory-mapped files may be expanding");
  elseif (expanding? & map-size < file-size)
    error("Expanding memory-mapped files must use the entire file");
  end;
  
  let mapping-handle = 
    CreateFileMapping(make(<HANDLE>, address: the-accessor.accessor-fd),
		      null-pointer(<LPSECURITY-ATTRIBUTES>), map-mode,
		      0 /* MaximumSizeHigh */, map-size,
		      null-pointer(<C-string>));

  if (mapping-handle = $NULL-HANDLE)
    error("Error in CreateFileMapping %=", win32-error-message(GetLastError()));
  end if;

  let bytes = MapViewOfFile(mapping-handle, view-mode, 
			    0 /* FileOffsetLow */, 0 /* FileOffsetLow */,
			    map-size);

  if (null-pointer?(bytes))
    error("Error in MapViewOfFile: %=", win32-error-message(GetLastError()));
  end if;

  next-method(class, accessor: the-accessor, mapping-handle: mapping-handle,
	      direction: direction, size: map-size, expanding?: expanding?,
	      bytes: bytes)

end method make;


define method close-mapped-memory (mapped-memory :: <memory-mapped-file>,
				   #key final-size = #f) => ()
  // Stuff gets lazily written out to disk.  Will resizing cause problems?
  UnmapViewOfFile(mapped-memory.mapped-file-bytes);
  CloseHandle(mapped-memory.mapped-file-mapping-handle);
  if (final-size & final-size ~= mapped-memory.size)
    let file-handle = make(<HANDLE>, 
		       address: mapped-memory.mapped-file-accessor.accessor-fd);
    SetFilePointer(file-handle, final-size, null-pointer(<PLONG>), $FILE-BEGIN);
    SetEndOfFile(file-handle);
  end if;
  accessor-close(mapped-memory.mapped-file-accessor);
end;
  

define function flush-mapped-memory
	(mapped-vector :: <memory-mapped-file>) => ()
  FlushViewOfFile(mapped-vector.mapped-file-bytes, 0);
end function;


define method type-for-copy (obj :: <memory-mapped-file>) => (class :: <type>)
  <byte-vector>
end method type-for-copy;


define inline method size (vector :: <memory-mapped-file>) => (r :: <integer>)
  vector.mapped-file-size
end method size;


// Resize the file.  Must close mapping, resize the file, and reopen 
// the mapping.  Not a lightweight operation...
define method size-setter (new-size :: <integer>, file :: <memory-mapped-file>)
		       => (size :: <integer>)
  if (file.mapped-file-direction ~= #"input-output")
    error("Attempt to resize an input-only memory mapped file");
  end if;

  UnmapViewOfFile(file.mapped-file-bytes);
  CloseHandle(file.mapped-file-mapping-handle);

  let (map-mode, view-mode) = values($PAGE-READWRITE, $FILE-MAP-WRITE);

  file.mapped-file-size := new-size;
  let mapping-handle = 
    CreateFileMapping(make(<HANDLE>, 
			   address: file.mapped-file-accessor.accessor-fd),
		      null-pointer(<LPSECURITY-ATTRIBUTES>), map-mode,
		      0 /* MaximumSizeHigh */, new-size,
		      null-pointer(<C-string>));

  if (mapping-handle = $NULL-HANDLE)
    error("Error in CreateFileMapping %=", win32-error-message(GetLastError()));
  end if;

  let bytes =
	  MapViewOfFile(mapping-handle, view-mode, 
	  		0 /* FileOffsetLow */, 0 /* FileOffsetLow */, new-size);

  if (null-pointer?(bytes))
    error("Error in MapViewOfFile: %=", win32-error-message(GetLastError()));
  end if;

  file.mapped-file-mapping-handle := mapping-handle;
  file.mapped-file-bytes := bytes;

  new-size
end method size-setter;

 
// methods for element and element-setter
define inline method element (vector :: <memory-mapped-file>, 
			      offset :: <integer>, 
			      #key default = unsupplied()) 
			  => (result :: <byte>)
  if (primitive-range-check(integer-as-raw(offset), integer-as-raw(0),
			    integer-as-raw(vector.mapped-file-size)))
    vector.mapped-file-bytes[offset]
  else
    if (unsupplied?(default))
      element-range-error(vector, offset);
    else
      default
    end if;
  end if;
end method element;


define inline method element-setter (value :: <byte>, 
				     vector :: <memory-mapped-file>, 
				     offset :: <integer>) 
				 => (result :: <byte>)
  index-check(vector, offset);
  vector.mapped-file-bytes[offset] := value;
end method element-setter;


// Note that offset must be a multiple of 4!
define inline method word-32-element (vector :: <memory-mapped-file>,
				      offset :: <integer>)
				  => (result :: <machine-word>)
  if (primitive-range-check(integer-as-raw(offset + 3), integer-as-raw(0),
			    integer-as-raw(vector.mapped-file-size)))
    C-long-at(vector.mapped-file-bytes, byte-index: offset)
  else
    element-range-error(vector, offset);
  end if;
end method word-32-element;


// Note that offset must be a multiple of 4!
define inline method word-32-element-setter 
		(value :: type-union(<machine-word>, <integer>),
		 vector :: <memory-mapped-file>,
		 offset :: <integer>)
	     => (result :: <machine-word>)
  index-check(vector, offset + 3);
  C-long-at(vector.mapped-file-bytes, byte-index: offset) := value
end method word-32-element-setter;


// Note that offset must be a multiple of 4!
define inline method word-64-element (vector :: <memory-mapped-file>,
				      offset :: <integer>)
				  => (result :: <machine-word>)
  if (primitive-range-check(integer-as-raw(offset + 7), integer-as-raw(0),
			    integer-as-raw(vector.mapped-file-size)))
    machine-word-logior(
      C-long-at(vector.mapped-file-bytes, byte-index: offset),
      machine-word-shift-left-with-overflow(
	C-long-at(vector.mapped-file-bytes, byte-index: offset + 4),
	32
      )
    )
  else
    element-range-error(vector, offset);
  end if;
end method word-64-element;


// Note that offset must be a multiple of 4!
define inline method word-64-element-setter 
		(value :: type-union(<machine-word>, <integer>),
		 vector :: <memory-mapped-file>,
		 offset :: <integer>)
	     => (result :: <machine-word>)
  index-check(vector, offset + 7);
  // Does C-long-at-setter do a range check on it's value?  I hope not.
  C-long-at(vector.mapped-file-bytes, byte-index: offset) := value;
  C-long-at(vector.mapped-file-bytes, byte-index: offset + 4) := 
    machine-word-shift-right(value, 32);
  value
end method word-64-element-setter;


define inline method index-check (vector :: <memory-mapped-file>, 
				  index :: <integer>) => ()
  unless (primitive-range-check(integer-as-raw(index), integer-as-raw(0),
				integer-as-raw(vector.mapped-file-size)))
    if (vector.mapped-file-expanding?)
      let new-size = vector.mapped-file-size;
      while (new-size <= index)
	new-size := new-size + ceiling/(new-size, 2) + 1;
      end while;
      vector.size := new-size;
    else 
      element-range-error(vector, index);
    end if;
  end unless;
end method index-check;


define inline method copy-bytes 
      (dst :: <memory-mapped-file>, dst-start :: <integer>,
       src :: type-union(<byte-vector>, <byte-string>), src-start :: <integer>,
       n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src))
    index-check(dst, dst-end);
    primitive-replace-bytes! 
	  (dst.mapped-file-bytes, integer-as-raw(0), integer-as-raw(dst-start),
	   src, primitive-repeated-slot-offset(src), integer-as-raw(src-start),
	   integer-as-raw(n));
  else
    element-range-error(src, src-end);
  end if;
end method copy-bytes;


define inline method copy-bytes 
      (dst :: type-union(<byte-vector>, <byte-string>), dst-start :: <integer>,
       src :: <memory-mapped-file>, src-start :: <integer>,
       n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & 
      src-end <= size(src) & dst-end <= size(dst))
    primitive-replace-bytes! 
	  (dst, primitive-repeated-slot-offset(dst), integer-as-raw(dst-start),
	   src.mapped-file-bytes, integer-as-raw(0), integer-as-raw(src-start),
	   integer-as-raw(n));
  else
    //copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if;
end method copy-bytes;


// Define some functions for setting and getting integers or machine
// words as four byte quantities.  Use the low level *-at regroutines
// from the ffi


// iteration:
// Could also have backward iteration protocol.  I haven't bothered yet.
define method forward-iteration-protocol 
	(memory-mapped-byte-vector :: <memory-mapped-file>)
     => (initial-state :: <integer>, limit :: <integer>, 
	 next-state :: <function>, finished-state? :: <function>, 
	 current-key :: <function>, current-element :: <function>,
	 current-element-setter :: <function>, copy-state :: <function>)
  values
    (0, // initial-state
     memory-mapped-byte-vector.size, // limit
     method  // next-state,
	 (s :: <memory-mapped-file>, i :: <integer>)
	 i + 1 end method, 
     method  // finished-state?,
	 (s :: <memory-mapped-file>, i :: <integer>, limit :: <integer>)
	 i = limit end method,
     method  // current-key 
	 (s :: <memory-mapped-file>, i :: <integer>)
	 i end method,
     element,  // current-element -- not reale fficient
     element-setter, // current-element-setter
     method  // copy-state
	 (s :: <memory-mapped-file>, i :: <integer>)
	 i end method)
end method;
