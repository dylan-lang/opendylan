Module:       system-internals
Synopsis:     Abstract modeling of locations
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
///---*** NEED PASCAL STRING, SHORT & LONG MACROS & INLINE-ONLY FUNCTIONS!
///---*** DO WE USE MoreFiles OR WRITE THE EQUIVALENT IN DYLAN?

/*
define macro c-integer-type-buffer-definer
  { define c-integer-type-buffer ?:name 
			         (?object:name) 
                                 (?accessor:name, ?wrapper:name, ?unwrapper:name} }
    => { define macro "with-" ## ?name ## "-buffer"
	   { "with-" ## ?name ## "-buffer" (??:name = ??init:expression) ??:body end }
	     => { begin
		    let ??name :: <byte-string>
		      = make(<byte-string>,
			     size: raw-as-integer(primitive-word-size()), fill: '\0');
		    ?accessor
		      (primitive-cast-raw-as-pointer(primitive-string-as-raw(??name)),
		       integer-as-raw(0), integer-as-raw(0))
		      := ?unwrapper(??init);
		    ??body
		  end }
	 end macro with-unsigned-long-buffer;
         define macro "cast-" ## ?name "-buffer-as-pointer"
	   { "cast-" ## ?name ## "-buffer-as-pointer"(??buffer:name) }
	     => { primitive-cast-raw-as-pointer(primitive-string-as-raw(??buffer)) }
	 end macro "cast-" ## ?name ## "-buffer-as-pointer";
         define inline-only function ?name ## "-buffer-as-" ?object
	     (buffer :: <byte-string>) => (value :: ?object)
	   ?wrapper
	     (?accessor
		(primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)),
		 integer-as-raw(0), integer-as-raw(0)))
	 end function unsigned-long-buffer-as-machine-word }
end macro c-integer-type-buffer-definer;
*/

define macro with-pascal-string
  { with-pascal-string (?:name = ?init:expression) ?:body end }
    => { begin
	   let ?name :: <byte-string> = make(<byte-string>, size: 256, fill: '\0');
	   let _value :: <byte-string> = ?init;
	   let _size :: <integer> = min(size(_value), 255);
	   primitive-c-unsigned-char-at
	       (primitive-cast-raw-as-pointer(primitive-string-as-raw(?name)),
		integer-as-raw(0), integer-as-raw(0))
	     := integer-as-raw(_size);
	   for (_i :: <integer> from 1 to _size)
	     primitive-c-unsigned-char-at
		 (primitive-cast-raw-as-pointer(primitive-string-as-raw(?name)),
		  integer-as-raw(0), integer-as-raw(_i))
	       := primitive-byte-character-as-raw(_value[_i - 1]);
	   end;
	   ?body
	 end }
end macro with-pascal-string;

define macro cast-pascal-string-as-pointer
  { cast-pascal-string-as-pointer(?string:name) }
    => { primitive-cast-raw-as-pointer(primitive-string-as-raw(?string)) }
end macro cast-pascal-string-as-pointer;

define inline-only function pascal-string-as-string (string :: <byte-string>)
 => (value :: <byte-string>)
  copy-sequence(string, start: 1, end: as(<integer>, string[0]) + 1)
end function pascal-string-as-string;

define macro with-short-buffer
  { with-short-buffer (?:name = ?init:expression) ?:body end }
    => { begin
	   let ?name :: <byte-string>
	     = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
	   primitive-c-signed-short-at
	       (primitive-cast-raw-as-pointer(primitive-string-as-raw(?name)),
		integer-as-raw(0), integer-as-raw(0))
	     := integer-as-raw(?init);
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

define macro with-long-buffer
  { with-long-buffer (?:name = ?init:expression) ?:body end }
    => { begin
	   let ?name :: <byte-string>
	     = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
	   primitive-c-signed-long-at
	       (primitive-cast-raw-as-pointer(primitive-string-as-raw(?name)),
		integer-as-raw(0), integer-as-raw(0))
	     := integer-as-raw(?init);
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


///---*** EXPLANATIONS?

define function volume-refnum&name
    (vrefnum :: false-or(<integer>), name :: false-or(<string>))
 => (vrefnum :: <integer>, name :: <byte-string>)
  if (vrefnum)
    with-pascal-string (name = "")
      with-short-buffer (vrefnum-buffer = 0)
	with-long-buffer (freeBytes-buffer = 0)
	  with-long-buffer (totalBytes-buffer = 0)
	    let status :: <raw-c-signed-short>
	      = %call-c-function ("HGetVolInfo", c-modifiers: "pascal")
		    (volReference :: <raw-c-signed-short>, volName :: <raw-byte-string>,
		     vRefNum :: <raw-c-pointer>, freeBytes :: <raw-c-pointer>,
		     totalBytes :: <raw-c-pointer>)
		 => (status :: <raw-c-signed-short>)
		  (integer-as-raw(vrefnum), 
		   cast-pascal-string-as-pointer(name),
		   cast-short-buffer-as-pointer(vrefnum-buffer), 
		   cast-long-buffer-as-pointer(freeBytes-buffer),
		   cast-long-buffer-as-pointer(totalBytes-buffer))
		end;
	    if (primitive-machine-word-equal?(status, integer-as-raw(0)))
	      values(short-buffer-as-integer(vrefnum-buffer),
		     pascal-string-as-string(name))
	    else
	      locator-error("Can't get volume name and vRefNum from volume reference %d:"
			      " status = %d",
			    vrefnum, raw-as-integer(status))
	    end
	  end
	end
      end
    end
  else
    with-pascal-string (pathname = concatenate-as(<byte-string>,
						  name,	
						  delimiter-to-string($macintosh-separator)))

      with-short-buffer (vrefnum-buffer = 0)
	let status :: <raw-c-signed-short>
	  = %call-c-function ("DetermineVRefNum", c-modifiers: "pascal")
	        (pathname :: <raw-byte-string>, vRefNum :: <raw-c-signed-short>, 
	         realVRefNum :: <raw-c-pointer>)
	     => (status :: <raw-c-signed-short>)
	      (cast-pascal-string-as-pointer(pathname),
	       integer-as-raw(0),
	       cast-short-buffer-as-pointer(vrefnum-buffer))
	    end;
	if (primitive-machine-word-equal?(status, integer-as-raw(0)))
	  values(short-buffer-as-integer(vrefnum-buffer), name)
	else
	  locator-error("Can't get volume vRefNum from volume name \"%s\": status = %d",
			name, raw-as-integer(status))
	end
      end
    end
  end
end function volume-refnum&name;


///---*** EXPLANATIONS?

define sealed class <native-macintosh-volume-locator> (<macintosh-volume-locator>)
  sealed constant slot locator-vrefnum :: <integer>,
    required-init-keyword: vrefnum:;
end class <native-macintosh-volume-locator>;

define sealed method make
    (class == <native-macintosh-volume-locator>,
     #key vrefnum :: false-or(<integer>) = #f,
          name :: false-or(<string>) = #f,
          volume :: false-or(<string>) = #f)
 => (locator :: <macintosh-volume-locator>)
  let (vrefnum, volume)
    = if (vrefnum)
	volume-refnum&name(vrefnum, #f)
      else
	volume-refnum&name(#f, volume | name)
      end;
  next-method(class, vrefnum: vrefnum, volume: name)
end method make;

define sealed method \=
    (locator1 :: <native-macintosh-volume-locator>,
     locator2 :: <native-macintosh-volume-locator>)
 => (equal? :: <boolean>)
  locator1.locator-vrefnm = locator2.locator-vrefnum
end method \=;


define sealed abstract class <native-macintosh-file-system-locator>
    (<macintosh-file-system-locator>)
  sealed slot %locator-macos-fsspec :: ?;
end class <native-macintosh-file-system-locator>;

define sealed method string-as-locator
    (class == <native-macintosh-file-system-locator>, string :: <string>)
 => (locator :: <native-macintosh-file-system-locator>)
  let pos = find-delimiter-from-end(string, $macintosh-separator);
  if (pos == string.size - 1)
    string-as-locator(<native-macintosh-directory-locator>, string)
  else
    string-as-locator(<native-macintosh-file-locator>, string)
  end
end method string-as-locator;


define constant <macos-dirid> = type-union(<integer>, <machine-word>);

define sealed class <native-macintosh-directory-locator> 
    (<macintosh-directory-locator>, <native-macintosh-file-system-locator>)
  sealed constant slot locator-dirid :: false-or(<macos-dirid>),
    required-init-keyword: dirid:;
end class <native-macintosh-directory-locator>;

define sealed method make
    (class == <native-macintosh-directory-locator>,
     #key server :: false-or(<macintosh-server-locator>) = #f,
          path :: false-or(<sequence>) = #f,
          relative? :: <boolean> = #f,
          directory :: false-or(<macintosh-directory-locator>) = #f,
          name :: false-or(<string> = #f,
          dirid :: false-or(<macos-dirid>) = #f)
 => (locator :: <native-macintosh-file-locator>)
  let path
    = if (name | directory)
	concatenate(if (directory) directory.locator-path else #[] end,
		    if (name) vector(name) else #[] end)
      else
	path
      end;
  let (dirid, path)
    = if (relative?)
	// Only absolute directory locators have a MacOS directory ID ...
	values(#f, path)
      else
	values(dirid, 
      end;
  next-method(class,
	      server:    server,
	      dirid:     dirid,
	      path:      path,
	      relative?: relative?)
end method make;

/// 

define constant <native-file-system-locator>  = <native-macintosh-file-system-locator>;
define constant <native-directory-locator> = <native-macintosh-directory-locator>;
define constant <native-file-locator>      = <native-macintosh-file-locator>;
*/

define constant <native-file-system-locator>  = <macintosh-file-system-locator>;
define constant <native-directory-locator> = <macintosh-directory-locator>;
define constant <native-file-locator>      = <macintosh-file-locator>;

define function file-system-separator
    () => (separator :: <character>)
  $macintosh-separator
end function file-system-separator;
