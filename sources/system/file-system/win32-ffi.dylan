Module:	      system-internals
Author:       Gary Palter
Synopsis:     Win32 implementation of the File System library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// From WINDOWS.H et al.
define constant $MAX_PATH = 260;
define constant $INVALID_HANDLE_VALUE         = -1;
define constant $FILE_ATTRIBUTE_READONLY      = #x00000001;
define constant $FILE_ATTRIBUTE_READONLY_BIT  = 0;	// #x00000001
define constant $FILE_ATTRIBUTE_DIRECTORY_BIT = 4;	// #x00000010
define constant $FORMAT_MESSAGE_FLAGS         = #x000011FF;
define constant $FORMAT_MESSAGE_LANGUAGE      = #x00000400;
//---*** NOTE: See the code for the #"executable?" property for an explanation
//---*** of why the next three values are presently unused...
// define constant $NO_ERROR                     = 0;
// define constant $ERROR_BAD_EXE_FORMAT         = 193;
// define constant $ERROR_ACCESS_DENIED          = 5;
define constant $ERROR_FILE_NOT_FOUND         = 2;
define constant $ERROR_PATH_NOT_FOUND         = 3;
define constant $ERROR_NOT_READY              = 21;
define constant $ERROR_NO_MORE_FILES          = 18;
define constant $ERROR_NOT_SUPPORTED          = 50;
define constant $SHGFI_EXETYPE                = #x00002000;

///

define macro with-stack-path
  { with-stack-path (?path:name) ?:body end }
  => { begin
         let ?path :: <byte-string> = make(<byte-string>, size: $MAX_PATH + 1, fill: '\0');
	 ?body
       end }
end macro with-stack-path;



///

/// Used instead of define C-struct to avoid relying on the C-FFI library ...

define constant $FILETIME_SIZE = 2 * raw-as-integer(primitive-word-size());

define constant $FIND_DATA_SIZE = 
  begin
    $DWORD_SIZE			// sizeof(dwFileAttributes)
      + $FILETIME_SIZE		// sizeof(ftCreationTime)
      + $FILETIME_SIZE		// sizeof(ftLastAccessTime)
      + $FILETIME_SIZE		// sizeof(ftLastWriteTime)
      + $DWORD_SIZE		// sizeof(nFileSizeHigh)
      + $DWORD_SIZE		// sizeof(nFileSizeLow)
      + $DWORD_SIZE		// sizeof(dwReserved0)
      + $DWORD_SIZE		// sizeof(dwReserved1)
      + $MAX_PATH		// sizeof(cFileName)
      + 14			// sizeof(cAlternateFileName)
  end;

define macro with-stack-win32-find-data
  { with-stack-win32-find-data (?wfd:name, ?directory:expression) ?:body end }
  => { begin
         let ?wfd = primitive-wrap-machine-word(integer-as-raw(0));
	 block ()
	   ?wfd := primitive-wrap-machine-word
		     (primitive-cast-pointer-as-raw
			(%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
			     (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
			  => (pointer :: <raw-c-pointer>)
			   (integer-as-raw(0), integer-as-raw($FIND_DATA_SIZE))
			 end));
	   if (primitive-machine-word-equal?(primitive-unwrap-machine-word(?wfd),
					     integer-as-raw(0)))
	     win32-file-system-error("get space for WIN32_FIND_DATA for", "%s", ?directory)
	   end;
	   ?body
	 cleanup
	   if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(?wfd),
						 integer-as-raw(0)))
	     %call-c-function ("LocalFree", c-modifiers: "__stdcall")
	         (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
	       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(?wfd)))
	     end
	   end
         end
       end }
end macro with-stack-win32-find-data;

define inline-only function win32-find-data-attributes
    (win32-find-data :: <machine-word>) => (attributes :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-unsigned-long-at(primitive-unwrap-machine-word(win32-find-data),
				  integer-as-raw(0),
				  integer-as-raw(0)))
end function win32-find-data-attributes;

define inline-only function win32-find-data-filename
    (win32-find-data :: <machine-word>) => (filename :: <byte-string>)
  primitive-raw-as-string
    (primitive-cast-raw-as-pointer
       (primitive-machine-word-add
          (primitive-unwrap-machine-word(win32-find-data),
	   integer-as-raw(0			// offset(dwFileAttributes)
			    + $DWORD_SIZE	// offset(ftCreationTime)
			    + $FILETIME_SIZE	// offset(ftLastAccessTime)
			    + $FILETIME_SIZE	// offset(ftLastWriteTime)
			    + $FILETIME_SIZE	// offset(nFileSizeHigh)
			    + $DWORD_SIZE	// offset(nFileSizeLow)
			    + $DWORD_SIZE	// offset(dwReserved0)
			    + $DWORD_SIZE	// offset(dwReserved1)
			    + $DWORD_SIZE))))	// offset(cFileName)
end function win32-find-data-filename;


///

define macro with-file-attributes
  { with-file-attributes (?file:expression, ?fa:name) ?:body end }
  => { begin
	 do-with-file-attributes(?file,
				 method (?fa :: <machine-word>)
				  ?body
				 end)
       end }
end macro with-file-attributes;

define function do-with-file-attributes (file :: <locator>, f :: <function>)
  let file = %expand-pathname(file);
  if (instance?(file, <directory-locator>))
    // FindFirstFile requires that there be a filename ...
    file := make(<file-locator>,
		 directory: file,
		 name: ".");
  end;
  let finder = primitive-wrap-machine-word(integer-as-raw($INVALID_HANDLE_VALUE));
  with-stack-win32-find-data (fa, file)
    block ()
      finder := primitive-wrap-machine-word
	          (primitive-cast-pointer-as-raw
		     (%call-c-function ("FindFirstFileA", c-modifiers: "__stdcall")
			  (lpFileName :: <raw-byte-string>,
			   lpFindFileData :: <raw-c-pointer>)
		       => (hFindFile :: <raw-c-pointer>)
			(primitive-string-as-raw(as(<byte-string>, file)),
			 primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(fa)))
		      end));
      if (primitive-machine-word-equal?
	    (primitive-unwrap-machine-word(finder),
	     integer-as-raw($INVALID_HANDLE_VALUE)))
	win32-file-system-error("get attributes of", "%s", file);
      end;
      f(fa)
    cleanup
      if (primitive-machine-word-not-equal?
	    (primitive-unwrap-machine-word(finder),
	     integer-as-raw($INVALID_HANDLE_VALUE)))
	%call-c-function ("FindClose", c-modifiers: "__stdcall")
	    (hFindFile :: <raw-c-pointer>) => (closed? :: <raw-c-signed-int>)
	  (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(finder)))
        end
      end
    end
  end
end function do-with-file-attributes;

define inline-only function fa-attributes (fa :: <machine-word>) => (attrs :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-unsigned-long-at(primitive-unwrap-machine-word(fa),
				  integer-as-raw(0),
				  integer-as-raw(0)))
end function fa-attributes;

define inline-only function fa-creation-time (fa :: <machine-word>)
 => (creation-time :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-machine-word-add
       (primitive-unwrap-machine-word(fa),
	integer-as-raw(0			// offset(dwFileAttributes)
                         + $DWORD_SIZE)))	// offset(ftCreationTime)
end function fa-creation-time;

define inline-only function fa-access-time (fa :: <machine-word>)
 => (access-time :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-machine-word-add
       (primitive-unwrap-machine-word(fa),
	integer-as-raw(0			// offset(dwFileAttributes)
			 + $DWORD_SIZE		// offset(ftCreationTime)
			 + $FILETIME_SIZE)))	// offset(ftLastAccessTime)
end function fa-access-time;

define inline-only function fa-write-time (fa :: <machine-word>)
 => (write-time :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-machine-word-add
       (primitive-unwrap-machine-word(fa),
	integer-as-raw(0			// offset(dwFileAttributes)
			 + $DWORD_SIZE		// offset(ftCreationTime)
			 + $FILETIME_SIZE	// offset(ftLastAccessTime)
			 + $FILETIME_SIZE)))	// offset(ftLastWriteTime)
end function fa-write-time;

define inline-only function fa-size-low (fa :: <machine-word>) => (size-low :: <integer>)
  raw-as-abstract-integer
    (primitive-c-unsigned-long-at(primitive-unwrap-machine-word(fa),
				  integer-as-raw(8),
				  integer-as-raw(0)))
end function fa-size-low;

/// High order 32-bits of the number of 100-nanosecond ticks since January 1, 1601
/// corresponding to an interval of 300 years (i.e., January 1, 1901)
define constant $300-years = 22042728;

define inline-only function filetime-valid? (ft :: <machine-word>) => (valid? :: <boolean>)
  primitive-machine-word-greater-than?
    (primitive-c-unsigned-long-at(primitive-unwrap-machine-word(ft),
				  integer-as-raw(1),
				  integer-as-raw(0)),
     integer-as-raw($300-years))
end function filetime-valid?;


/// Error handling

define function win32-last-error-message () => (message :: <string>)
  let status = primitive-wrap-machine-word
		 (%call-c-function ("GetLastError", c-modifiers: "__stdcall")
		      () => (status :: <raw-c-unsigned-long>)
		    ()
		  end);
  %call-c-function ("FormatMessageA", c-modifiers: "__stdcall")
      (flags :: <raw-c-unsigned-long>, lpSource :: <raw-c-pointer>,
       message-id :: <raw-c-unsigned-long>, language-id :: <raw-c-unsigned-long>,
       lpBuffer :: <raw-c-pointer>, bytes :: <raw-c-unsigned-long>,
       lpArguments :: <raw-c-pointer>)
   => (count :: <raw-c-unsigned-long>)
    (integer-as-raw($FORMAT_MESSAGE_FLAGS),
     primitive-cast-raw-as-pointer(integer-as-raw(0)),
     primitive-unwrap-machine-word(status),
     integer-as-raw($FORMAT_MESSAGE_LANGUAGE),
     primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(message-buffer-ptr)),
     integer-as-raw(0),
     primitive-cast-raw-as-pointer(integer-as-raw(0)))
  end;
  let message = primitive-raw-as-string
                  (primitive-c-pointer-at
                     (primitive-unwrap-machine-word(message-buffer-ptr),
                      integer-as-raw(0),
                      integer-as-raw(0)));
  %call-c-function ("LocalFree", c-modifiers: "__stdcall")
     (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
    (primitive-c-pointer-at(primitive-unwrap-machine-word(message-buffer-ptr),
                            integer-as-raw(0),
                            integer-as-raw(0)))
  end;
  message
end function win32-last-error-message;

/*---*** andrewa: old version
define function win32-last-error-message () => (message :: <string>)
  let status = primitive-wrap-machine-word
		 (%call-c-function ("GetLastError", c-modifiers: "__stdcall")
		      () => (status :: <raw-c-unsigned-long>)
		    ()
		  end);
  with-stack-dword (message-address)
    %call-c-function ("FormatMessageA", c-modifiers: "__stdcall")
        (flags :: <raw-c-unsigned-long>, lpSource :: <raw-c-pointer>,
	 message-id :: <raw-c-unsigned-long>, language-id :: <raw-c-unsigned-long>,
	 lpBuffer :: <raw-c-pointer>, bytes :: <raw-c-unsigned-long>,
	 lpArguments :: <raw-c-pointer>)
     => (count :: <raw-c-unsigned-long>)
      (integer-as-raw($FORMAT_MESSAGE_FLAGS),
       primitive-cast-raw-as-pointer(integer-as-raw(0)),
       primitive-unwrap-machine-word(status),
       integer-as-raw($FORMAT_MESSAGE_LANGUAGE),
       primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(message-address)),
       integer-as-raw(0),
       primitive-cast-raw-as-pointer(integer-as-raw(0)))
    end;
    let message = primitive-raw-as-string
                    (primitive-c-pointer-at(primitive-unwrap-machine-word(message-address),
					    integer-as-raw(0),
					    integer-as-raw(0)));
    %call-c-function ("LocalFree", c-modifiers: "__stdcall")
        (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
      (primitive-c-pointer-at(primitive-unwrap-machine-word(message-address),
			      integer-as-raw(0),
			      integer-as-raw(0)))
    end;
    message
  end
end function win32-last-error-message;
*/

// Should really signal a distint error class, perhaps a subclass of <file-error> ...
define function win32-file-system-error
    (operation :: <string>, additional-information, #rest additional-information-args) => (res :: <bottom>)
  let status-message = win32-last-error-message();
  if (additional-information)
    error(make(<file-system-error>,
	       format-string: concatenate("%s: Can't %s ", additional-information),
	       format-arguments: concatenate(list(status-message),
					     list(operation),
					     map(method (x)
						   if (instance?(x, <locator>))
						     as(<string>, x)
						   else
						     x
						   end
						 end method,
						 additional-information-args))))
  else
    error(make(<file-system-error>,
	       format-string: "%s: Can't %s",
	       format-arguments: list(status-message, operation)))
  end;
end function win32-file-system-error;
