Module:	      system-internals
Author:       Gary Palter
Synopsis:     Mac OS Carbon implementation of the File System library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///---*** NOTE: Need to switch to new HFS+ APIs as soon as possible!

/// From MacTypes.h ...

/// From Errors.h ...
define constant $noErr    = 0;
define constant $nsvErr   = -35;
define constant $bdNamErr = -37;
define constant $fnfErr   = -43;

/// From Files.h ...
define constant $fsRtDirID       = 2;
define constant $FSSPEC-SIZE     = 70;
define constant $CINFOPBREC-SIZE = 108;

/// From Folders.h ...
define constant $kOnSystemDisk            = -32768;
define constant $kCreateFolder            = 1;
define constant $kTemporaryFolderType     = as(<machine-word>, #x74656d70); // 'temp'
define constant $kChewableItemsFolderType = as(<machine-word>, #x666c6e74); // 'flnt'


/// From ATSUnicode.h ...
define constant $smSystemScript = -1;


/// Convenience functions/macros to create buffers to hold various types of data,
/// pass them to Mac OS APIs, and convert the data to/from native Dylan types

define macro with-short-buffer
  { with-short-buffer (?:name = ?init:expression) ?:body end }
    => { begin
	   let ?name :: <byte-string>
	     = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
	   short-buffer-as-integer(?name) := ?init;
	   ?body
	 end }
end macro with-short-buffer;

define macro cast-short-buffer-as-pointer
  { cast-short-buffer-as-pointer(?buffer:name) }
    => { primitive-cast-raw-as-pointer(primitive-string-as-raw(?buffer)) }
end macro cast-short-buffer-as-pointer;

define inline-only function short-buffer-as-integer (buffer :: <byte-string>)
 => (value :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)),
	integer-as-raw(0), integer-as-raw(0)))
end function short-buffer-as-integer;

define inline-only function short-buffer-as-integer-setter
    (value :: <integer>, buffer :: <byte-string>) => (value :: <integer>)
  primitive-c-signed-short-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)),
       integer-as-raw(0), integer-as-raw(0))
    := integer-as-raw(value);
  value
end function short-buffer-as-integer-setter;


define macro with-long-buffer
  { with-long-buffer (?:name = ?init:expression) ?:body end }
    => { begin
	   let ?name :: <byte-string>
	     = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
	   long-buffer-as-machine-word(?name) := as(<machine-word>, ?init);
	   ?body
	 end }
end macro with-long-buffer;

define macro cast-long-buffer-as-pointer
  { cast-long-buffer-as-pointer(?buffer:name) }
    => { primitive-cast-raw-as-pointer(primitive-string-as-raw(?buffer)) }
end macro cast-long-buffer-as-pointer;

define inline-only function long-buffer-as-machine-word (buffer :: <byte-string>)
 => (value :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-signed-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)),
	integer-as-raw(0), integer-as-raw(0)))
end function long-buffer-as-machine-word;

define inline-only function long-buffer-as-machine-word-setter
    (value :: <machine-word>, buffer :: <byte-string>) => (value :: <machine-word>)
  primitive-c-signed-long-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)),
       integer-as-raw(0), integer-as-raw(0))
    := primitive-unwrap-machine-word(value);
  value
end function long-buffer-as-machine-word-setter;


define macro with-pascal-string
  { with-pascal-string (?:name = ?init:expression) ?:body end }
    => { begin
	   let ?name :: <byte-string> = make(<byte-string>, size: 256, fill: '\0');
	   let _value :: <byte-string> = as(<byte-string>, ?init);
	   pascal-string-as-string(?name) := _value;
	   ?body
	 end }
end macro with-pascal-string;

define macro cast-pascal-string-as-pointer
  { cast-pascal-string-as-pointer(?pascal-string:name) }
    => { primitive-cast-raw-as-pointer(primitive-string-as-raw(?pascal-string)) }
end macro cast-pascal-string-as-pointer;

define inline-only function pascal-string-as-string (pascal-string :: <byte-string>)
 => (value :: <byte-string>)
  copy-sequence(pascal-string, start: 1, end: as(<integer>, pascal-string[0]) + 1)
end function pascal-string-as-string;

define inline-only function pascal-string-as-string-setter 
    (value :: <byte-string>, pascal-string :: <byte-string>)
 => (value :: <byte-string>)
  let size :: <integer> = min(size(value), 255);
  pascal-string[0] := as(<byte-character>, size);
  for (i :: <integer> from 0 below size)
    pascal-string[i + 1] := as(<byte-character>, value[i])
  end;
  value
end function pascal-string-as-string-setter;


///---*** EXPLANATION?
define macro with-locator-as-fsspec
  { with-locator-as-fsspec (?fsspec:name = ?locator:expression, ?status:name) ?:body end }
    => { begin
	   do-with-locator-as-fsspec(?locator,
				     method (?fsspec :: <byte-string>, ?status :: <integer>)
				       ?body
				     end)
	 end }
end macro with-locator-as-fsspec;

define macro cast-fsspec-as-pointer
  { cast-fsspec-as-pointer(?fsspec:expression) }
    => { primitive-cast-raw-as-pointer(primitive-string-as-raw(?fsspec)) }
end macro cast-fsspec-as-pointer;

///---*** NOTE: TEMPORARY: Needs to be "smarter" when we have proper MacOS locators!
define function do-with-locator-as-fsspec (path :: <locator>, body :: <function>)
  let path :: <byte-string> = as(<byte-string>, %expand-pathname(path));
  let size :: <integer> = size(path);
  let fsspec :: <byte-string> = make(<byte-string>, size: $FSSPEC-SIZE, fill: '\0');
  let status :: <integer>
    = if (size > 255)
	$bdNamErr
      else
	with-pascal-string (fileName = path)
	  raw-as-integer
	    (%call-c-function ("FSMakeFSSpec", c-modifiers: "pascal")
		 (vRefNum :: <raw-c-signed-short>, dirID :: <raw-c-signed-long>,
		  fileName :: <raw-byte-string>, spec :: <raw-c-pointer>)
	      => (status :: <raw-c-signed-short>)
	       (integer-as-raw(0),
		integer-as-raw(0),
		cast-pascal-string-as-pointer(fileName),
		cast-fsspec-as-pointer(fsspec))
	     end)
	end
      end;
  body(fsspec, status)
end function do-with-locator-as-fsspec;

define inline-only function fsspec-vRefNum
    (fsspec :: <byte-string>) => (vRefNum :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(fsspec)),
	integer-as-raw(0), integer-as-raw(0)))
end function fsspec-vRefNum;

define inline-only function fsspec-dirID
    (fsspec :: <byte-string>) => (dirID :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-signed-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(fsspec)),
	integer-as-raw(0), integer-as-raw(2)))
end function fsspec-dirID;

define inline-only function fsspec-name
    (fsspec :: <byte-string>) => (name :: <byte-string>)
  pascal-string-as-string(copy-sequence(fsspec, start: 6))
end function fsspec-name;


/// EXPLANATION?
///---*** NOTE: TEMPORARY: Needs to be "smarter" (simpler) when we have proper MacOS locators!
define function fsspec-components-as-locator
    (vRefNum :: <integer>, dirID :: <machine-word>, filename :: false-or(<byte-string>))
 => (path :: <locator>)
  let path = make(<stretchy-vector>);
  let cinfopbrec :: <byte-string> = make(<byte-string>, size: $CINFOPBREC-SIZE, fill: '\0');
  with-pascal-string (dirname = "")
    primitive-c-pointer-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
         integer-as-raw(0), integer-as-raw(18))
      := cast-pascal-string-as-pointer(dirname);
    primitive-c-signed-short-at
	(primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
	 integer-as-raw(0), integer-as-raw(22))
      := integer-as-raw(vrefnum);
    primitive-c-signed-long-at
	(primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
	 integer-as-raw(0), integer-as-raw(100))
      := primitive-unwrap-machine-word(dirID);
    primitive-c-signed-short-at
	(primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
	 integer-as-raw(0), integer-as-raw(28))
      := integer-as-raw(-1);
    let done :: <boolean> = #f;
    while (~done)
      primitive-c-signed-long-at
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
	   integer-as-raw(0), integer-as-raw(48))
	:= primitive-c-signed-long-at
	     (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
	      integer-as-raw(0), integer-as-raw(100));
      let status :: <integer>
	= raw-as-integer
	    (%call-c-function ("PBGetCatInfoSync", c-modifiers: "pascal")
		 (paramBlock :: <raw-c-pointer>) => (status :: <raw-c-signed-short>)
	       (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)))
	     end);
      unless (status == $noErr)
	carbon-file-error(status, "convert", "(%d,%d) to a <locator>", vrefnum, dirid)
      end;
      add!(path, pascal-string-as-string(dirname));
      done := $fsRtDirID
	      = raw-as-integer
		  (primitive-c-signed-long-at
		     (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
		      integer-as-raw(0), integer-as-raw(48)));
    end;
  end;
  let directory = as(<directory-locator>,
		     reduce(rcurry(concatenate, ":"), "", reverse!(path)))
  if (filename)
    make(<file-locator>,
	 directory: directory,
	 name: filename)
  else
    directory
  end
end function fsspec-components-as-locator;


///---*** EXPLANATION
define macro with-file-catalog-info
  { with-file-catalog-info ((?fsspec:name, ?ci:name) = ?locator:expression) ?:body end }
    => { begin
	   do-with-file-catalog-info(?locator,
				     method (?fsspec :: <byte-string>, ?ci :: <byte-string>)
				       ?body
				     end)
	 end }
  { with-file-catalog-info (?ci:name = ?locator:expression) ?:body end }
    => { begin
	   do-with-file-catalog-info(?locator,
				     method (_fsspec_ :: <byte-string>, ?ci :: <byte-string>)
				       ?body
				     end)
	 end }
end macro with-file-catalog-info;

define function do-with-file-catalog-info (file :: <locator>, body :: <function>)
  with-locator-as-fsspec (fsspec = file, status)
    if (status == $noErr)
      let ci :: <byte-string> = make(<byte-string>, size: $CINFOPBREC-SIZE, fill: '\0');
      primitive-c-pointer-at			// ioNamePtr
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(ci)),
	   integer-as-raw(0), integer-as-raw(18))
	:= primitive-cast-raw-as-pointer
	     (primitive-machine-word-add
		(primitive-cast-pointer-as-raw(primitive-string-as-raw(fsspec)),
		 integer-as-raw(6)));
      primitive-c-signed-short-at               // ioVRefNum
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(ci)),
	   integer-as-raw(0), integer-as-raw(22))
	:= primitive-c-signed-short-at
	     (primitive-cast-raw-as-pointer(primitive-string-as-raw(fsspec)),
	      integer-as-raw(0), integer-as-raw(0));
      primitive-c-signed-short-at               // ioFDirIndex
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(ci)),
	   integer-as-raw(0), integer-as-raw(28))
	:= integer-as-raw(0);
      primitive-c-signed-long-at		// ioDirID
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(ci)),
	   integer-as-raw(0), integer-as-raw(48))
	:= primitive-c-signed-long-at
	     (primitive-cast-raw-as-pointer(primitive-string-as-raw(fsspec)),
	      integer-as-raw(0), integer-as-raw(2));
      let status :: <integer>
	= raw-as-integer
	    (%call-c-function ("PBGetCatInfoSync", c-modifiers: "pascal")
		 (paramBlock :: <raw-c-pointer>) => (status :: <raw-c-signed-short>)
	       (primitive-cast-raw-as-pointer(primitive-string-as-raw(ci)))
	     end);
      if (status == $noErr)
	body(fsspec, ci)
      else
	carbon-file-error(status, "get attributes of", "%s", file)
      end
    else
      carbon-file-error(status, "get attributes of", "%s", file)
    end
  end
end function do-with-file-catalog-info;

define inline-only function attributes (catalog-info :: <byte-string>)
 => (attributes :: <integer>)
  raw-as-integer
    (primitive-c-unsigned-char-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(catalog-info)),
	integer-as-raw(0), integer-as-raw(30)))
end function attributes;

define inline-only function locked? (attributes :: <integer>) => (locked? :: <boolean>)
  logbit?(0, attributes)
end function locked?;

define inline-only function directory? (attributes :: <integer>) => (directory? :: <boolean>)
  logbit?(4, attributes)
end function directory?;

define inline-only function file-data-fork-lEOF (catalog-info :: <byte-string>)
 => (size :: <integer>)
  raw-as-integer
    (primitive-c-signed-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(catalog-info)),
	integer-as-raw(0), integer-as-raw(54)))
end function file-data-fork-lEOF;

define inline-only function file-resource-fork-lEOF (catalog-info :: <byte-string>)
 => (size :: <integer>)
  raw-as-integer
    (primitive-c-signed-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(catalog-info)),
	integer-as-raw(0), integer-as-raw(64)))
end function file-resource-fork-lEOF;

define inline-only function file-creation-date (catalog-info :: <byte-string>)
 => (date :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-unsigned-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(catalog-info)),
	integer-as-raw(0), integer-as-raw(72)))
end function file-creation-date;

define inline-only function file-modification-date (catalog-info :: <byte-string>)
 => (date :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-unsigned-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(catalog-info)),
	integer-as-raw(0), integer-as-raw(76)))
end function file-modification-date;

define inline-only function finder-info (catalog-info :: <byte-string>)
 => (finder-info :: <byte-string>)
  concatenate(copy-sequence(catalog-info, start: 32, end: 48),
	      copy-sequence(catalog-info, start: 84, end: 100))
end function finder-info;

///---*** IS THIS RIGHT?  OR IS IT BYTE-SWAPPED?
define inline-only function creator (finder-info :: <byte-string>)
 => (creator :: <byte-string>)
  copy-sequence(finder-info, start: 4, end: 8)
end function creator;

///---*** IS THIS RIGHT?  OR IS IT BYTE-SWAPPED?
define inline-only function filetype (finder-info :: <byte-string>)
 => (filetype :: <byte-string>)
  copy-sequence(finder-info, start: 0, end: 4)
end function filetype;

///---*** IS THIS RIGHT?  OR IS IT BYTE-SWAPPED?
define inline-only function string-as-ostype (type :: <byte-string>)
 => (ostype :: <machine-word>)
  if (size(type) > 4)
    carbon-file-error($noErr, "convert", "%s to the OSType data format", type)
  else
    let padded-type :: <byte-string>
      = concatenate(type, copy-sequence("    ", start: size(type)));
    primitive-wrap-machine-word
      (primitive-c-unsigned-long-at
	 (primitive-cast-raw-as-pointer(primitive-string-as-raw(padded-type)),
	  integer-as-raw(0), integer-as-raw(0)))
  end
end function string-as-ostype;

define inline-only function alias? (finder-info :: <byte-string>)
 => (invisible? :: <boolean>)
  primitive-machine-word-logbit?
    (integer-as-raw(15),
     primitive-c-unsigned-short-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(finder-info)),
	integer-as-raw(0), integer-as-raw(8)))
end function alias?;

define inline-only function invisible? (finder-info :: <byte-string>)
 => (invisible? :: <boolean>)
  primitive-machine-word-logbit?
    (integer-as-raw(14),
     primitive-c-unsigned-short-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(finder-info)),
	integer-as-raw(0), integer-as-raw(8)))
end function invisible?;


///---*** NOTE: Should add text for some common error codes (e.g., FNF) ...
define function carbon-file-error
    (status :: <integer>, operation :: <string>,
     additional-information, #rest additional-information-args)
 => (death :: <bottom>)
  local method fix-args (args :: <list>) => (fixed-args :: <list>)
	  map(method (x)
		if (instance?(x, <locator>))
		  as(<string>, x)
		else
		  x
		end
	      end method,
	      args)
	end method;
  if (status == $noErr)
    if (additional-information)
      error(make(<file-system-error>,
		 format-string: concatenate("Can't %s ", additional-information),
		 format-arguments: concatenate(list(operation),
					       fix-args(additional-information-args))))
    else
      error(make(<file-system-error>,
		 format-string: "Can't %s",
		 format-arguments: list(operation)))
    end
  else
    if (additional-information)
      error(make(<file-system-error>,
		 format-string: concatenate("MacOS error #%d: Can't %s ", 
					    additional-information),
		 format-arguments: concatenate(list(status, operation),
					       fix-args(additional-information-args))))
    else
      error(make(<file-system-error>,
		 format-string: "MacOS error #%d: Can't %s",
		 format-arguments: list(status, operation)))
    end
  end
end function carbon-file-error;
