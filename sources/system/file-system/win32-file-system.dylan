Module:	      system-internals
Author:       Gary Palter
Synopsis:     Win32 implementation of the File System library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
define function %expand-pathname
    (path :: <microsoft-file-system-locator>)
 => (expanded-path :: <microsoft-file-system-locator>)
  with-stack-path (path-buffer)
    with-stack-dword (unused-address)
      let path-length = raw-as-integer
			  (%call-c-function ("GetFullPathNameA", c-modifiers: "__stdcall")
			       (fileName :: <raw-c-pointer>, 
				bufferSize :: <raw-c-unsigned-long>,
				bufferPtr :: <raw-c-pointer>,
				filePartPtrPtr :: <raw-c-pointer>)
			    => (bufferUsed :: <raw-c-unsigned-long>)
			     (primitive-string-as-raw(as(<byte-string>, path)),
			      integer-as-raw($MAX_PATH),
			      primitive-string-as-raw(path-buffer),
			      primitive-cast-raw-as-pointer
				(primitive-unwrap-machine-word(unused-address)))
			   end);
      if (path-length > $MAX_PATH | path-length = 0)
	win32-file-system-error("expand", "%s", path)
      else
	as(object-class(path), copy-sequence(path-buffer, end: path-length))
      end
    end
  end
end function %expand-pathname;


///
define function %shorten-pathname 
    (path :: <microsoft-file-system-locator>)
 => (shortened-path :: <microsoft-file-system-locator>)
  with-stack-path (path-buffer)
    let path-length = raw-as-integer
			(%call-c-function ("GetShortPathNameA", c-modifiers: "__stdcall")
			   (fileName :: <raw-c-pointer>, 
			    bufferPtr :: <raw-c-pointer>,
			    bufferSize :: <raw-c-unsigned-long>)
			   => (bufferUsed :: <raw-c-unsigned-long>)
			   (primitive-string-as-raw(as(<byte-string>, path)),
			    primitive-string-as-raw(path-buffer),
			    integer-as-raw($MAX_PATH))
			end);
    if (path-length = 0)
      if (win32-raw-last-error() = $ERROR_NOT_SUPPORTED)
	path
      else
	win32-file-system-error("shorten", "%s", path)
      end
    elseif (path-length > $MAX_PATH)
      win32-file-system-error("shorten", "%s", path)
    else
      as(object-class(path), copy-sequence(path-buffer, end: path-length))
    end
  end
end function %shorten-pathname;


///
define function %file-exists? 
    (file :: <microsoft-file-system-locator>) => (exists? :: <boolean>)
  let file = %expand-pathname(file);
  if (primitive-machine-word-not-equal?
	(%call-c-function ("GetFileAttributesA", c-modifiers: "__stdcall")
	     (path :: <raw-byte-string>)
	  => (exists? :: <raw-c-unsigned-long>)
	   (primitive-string-as-raw(as(<byte-string>, file)))
	 end,
	 integer-as-raw($INVALID_HANDLE_VALUE)))
    #t
  elseif (begin
	    let status = win32-raw-last-error();
	    status = $ERROR_FILE_NOT_FOUND 
	      | status = $ERROR_PATH_NOT_FOUND
	      | status = $ERROR_NOT_READY
	  end)
    #f
  else
    #t
  end
end function %file-exists?;


///
define inline-only function attributes-to-file-type
    (attributes :: <machine-word>)
 => (file-type :: <file-type>)
  //---*** How do we determine if a file is a shortcut?
  if (primitive-machine-word-logbit?
	(integer-as-raw($FILE_ATTRIBUTE_DIRECTORY_BIT),
	 primitive-unwrap-machine-word(attributes)))
    #"directory"
  else
    #"file"
  end
end function attributes-to-file-type;

define function %file-type 
    (file :: <microsoft-file-system-locator>) => (file-type :: <file-type>)
  with-file-attributes (file, fa)
    attributes-to-file-type(fa-attributes(fa))
  end
end function %file-type;


///
define function %link-target
    (link :: <microsoft-file-system-locator>) => (target :: <microsoft-file-system-locator>)
  error(make(<file-system-error>,
	     format-string: "link-target is not available on this platform",
	     format-arguments: #()))
end function %link-target;


///
define function %delete-file
    (file :: <microsoft-file-system-locator>) => ()
  let file = %expand-pathname(file);
  // NOTE: Turn off the read-only flag or we won't be able to delete the file!
  %file-property(file, #"writeable?") := #t;
  unless (primitive-raw-as-boolean
            (%call-c-function ("DeleteFileA", c-modifiers: "__stdcall")
                 (path :: <raw-byte-string>)
              => (deleted? :: <raw-c-signed-int>)
               (primitive-string-as-raw(as(<byte-string>, file)))
             end))
    win32-file-system-error("delete", "%s", file)
  end
end function %delete-file;


///
define function %copy-file
    (source :: <microsoft-file-system-locator>, destination :: <microsoft-file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  let source = %expand-pathname(source);
  let destination = %expand-pathname(destination);
  // NOTE: Contrary to the documentation, CopyFile won't copy over
  // an existing read-only file so we need to delete it manually.
  if (if-exists == #"replace" & %file-exists?(destination))
    %delete-file(destination)
  end;
  unless (primitive-raw-as-boolean
            (%call-c-function ("CopyFileA", c-modifiers: "__stdcall")
                 (sourcePath :: <raw-byte-string>, destPath :: <raw-byte-string>,
                  failIfExists :: <raw-c-signed-int>)
              => (copied? :: <raw-c-signed-int>)
	       (primitive-string-as-raw(as(<byte-string>, source)),
		primitive-string-as-raw(as(<byte-string>, destination)),
                integer-as-raw
		  (select (if-exists)
		     #"signal" => -1;
		     #"replace" => 0;
		   end))
	     end))
    win32-file-system-error("copy", "%s to %s", source, destination)
  end
end function %copy-file;


///
define function %rename-file
    (source :: <microsoft-file-system-locator>, destination :: <microsoft-file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  let source = %expand-pathname(source);
  let destination = %expand-pathname(destination);
  // NOTE: We can't use MoveFileEx which provides options to control
  // the move if the target exists because MoveFileEx isn't implemented
  // in Windows 95.  (When this code was originally written, the 
  // documentation for MoveFileEx failed to mention that fact.  Sigh)
  if (if-exists == #"replace" & %file-exists?(destination))
    %delete-file(destination)
  end;
  unless (primitive-raw-as-boolean
	    (%call-c-function ("MoveFileA", c-modifiers: "__stdcall")
		 (sourcePath :: <raw-byte-string>, destPath :: <raw-byte-string>)
	      => (moved? :: <raw-c-signed-int>)
	       (primitive-string-as-raw(as(<byte-string>, source)),
		primitive-string-as-raw(as(<byte-string>, destination)))
	     end))
    win32-file-system-error("rename", "%s to %s", source, destination)
  end
end function %rename-file;


///
define function %file-properties 
    (file :: <microsoft-file-system-locator>)
 => (properties :: <explicit-key-collection>)
  let properties = make(<table>);
  with-file-attributes (file, fa)
    properties[#"size"] := fa-size-low(fa);
    properties[#"creation-date"] := date-if-valid(fa-creation-time(fa));
    properties[#"access-date"] := date-if-valid(fa-access-time(fa));
    properties[#"modification-date"] := date-if-valid(fa-write-time(fa));
    properties[#"readable?"] := #t;
    properties[#"writeable?"] := writeable?(fa-attributes(fa));
  end;
  properties[#"executable?"] := %file-property(file, #"executable?");
  properties
end function %file-properties;


/// "Standard" properties not implemented on this platform:
///    author

/// "Standard" properties not settable on this platform:
///    size, creation-date, access-date, modification-date, readable?, executable?

/// Even though it's not implemented, we'll provide a stub just to be "nice"...
define method %file-property
    (file :: <microsoft-file-system-locator>, key == #"author") => (author :: false-or(<string>))
  #f
end method %file-property;

define method %file-property 
    (file :: <microsoft-file-system-locator>, key == #"size")
 => (file-size :: <integer>)
  with-file-attributes (file, fa)
    fa-size-low(fa)
  end
end method %file-property;

define inline-only function date-if-valid 
    (native-clock :: <machine-word>) 
 => (date :: false-or(<date>))
  filetime-valid?(native-clock) & make(<date>, native-clock: native-clock)
end function date-if-valid;

define method %file-property
    (file :: <microsoft-file-system-locator>, key == #"creation-date") => (creation-date :: false-or(<date>))
  with-file-attributes (file, fa)
    date-if-valid(fa-creation-time(fa))
  end
end method %file-property;

define method %file-property
    (file :: <microsoft-file-system-locator>, key == #"access-date")
 => (access-date :: false-or(<date>))
  with-file-attributes (file, fa)
    date-if-valid(fa-access-time(fa))
  end
end method %file-property;

define method %file-property
    (file :: <microsoft-file-system-locator>, key == #"modification-date")
 => (modification-date :: false-or(<date>))
  with-file-attributes (file, fa)
    date-if-valid(fa-write-time(fa))
  end
end method %file-property;

define method %file-property
    (file :: <microsoft-file-system-locator>, key == #"readable?") => (readable? :: <boolean>)
  #t
end method %file-property;

define inline-only function writeable? 
    (attrs :: <machine-word>) => (writeable? :: <boolean>)
  ~ primitive-machine-word-logbit?
      (integer-as-raw($FILE_ATTRIBUTE_READONLY_BIT),
       primitive-unwrap-machine-word(attrs))
end function writeable?;

define method %file-property
    (file :: <microsoft-file-system-locator>, key == #"writeable?") => (writeable? :: <boolean>)
  let file = %expand-pathname(file);
  let attributes = primitive-wrap-machine-word
                     (%call-c-function ("GetFileAttributesA", c-modifiers: "__stdcall")
                          (path :: <raw-byte-string>)
                       => (attributes :: <raw-c-unsigned-long>)
                        (primitive-string-as-raw(as(<byte-string>, file)))
                      end);
  if (primitive-machine-word-not-equal?
	(primitive-unwrap-machine-word(attributes),
	 integer-as-raw($INVALID_HANDLE_VALUE)))
    writeable?(attributes)
  else
    win32-file-system-error("get attributes of", "%s", file)
  end
end method %file-property;

define method %file-property-setter
    (new-writeable? :: <boolean>, file :: <microsoft-file-system-locator>, key == #"writeable?")
 => (new-writeable? :: <boolean>)
  let file = %expand-pathname(file);
  let attributes = primitive-wrap-machine-word
                     (%call-c-function ("GetFileAttributesA", c-modifiers: "__stdcall")
                          (path :: <raw-byte-string>)
                       => (attributes :: <raw-c-unsigned-long>)
                        (primitive-string-as-raw(as(<byte-string>, file)))
                      end);
  if (primitive-machine-word-not-equal?
	(primitive-unwrap-machine-word(attributes),
	 integer-as-raw($INVALID_HANDLE_VALUE)))
    let old-writeable? = writeable?(attributes);
    if (new-writeable? ~= old-writeable?)
      unless (primitive-raw-as-boolean
                (%call-c-function ("SetFileAttributesA", c-modifiers: "__stdcall")
		     (path :: <raw-byte-string>, attributes :: <raw-c-unsigned-long>)
		  => (success? :: <raw-c-signed-int>)
		   (primitive-string-as-raw(as(<byte-string>, file)),
                    primitive-machine-word-logxor
                      (primitive-unwrap-machine-word(attributes),
                       integer-as-raw($FILE_ATTRIBUTE_READONLY)))
		 end))
        win32-file-system-error("set attributes of", "%s", file)
      end
    end
  else
    win32-file-system-error("get attributes of", "%s", file)
  end;
  new-writeable?
end method %file-property-setter;

define method %file-property
    (file :: <microsoft-file-system-locator>, key == #"executable?")
 => (executable? :: <boolean>)
  let file = %expand-pathname(file);
  let executable? = primitive-raw-as-boolean
                      (%call-c-function ("SHGetFileInfoA", c-modifiers: "__stdcall")
			   (pszPath :: <raw-byte-string>,
			    dwFileAttributes :: <raw-c-unsigned-long>,
			    psfi :: <raw-c-pointer>,
			    cbFileInfo :: <raw-c-unsigned-int>,
			    uFlags :: <raw-c-unsigned-int>)
			=> (result :: <raw-c-unsigned-long>)
			 (primitive-string-as-raw(as(<byte-string>, file)),
			  integer-as-raw(0),
			  primitive-cast-raw-as-pointer(integer-as-raw(0)),
			  integer-as-raw(0),
			  integer-as-raw($SHGFI_EXETYPE))
		      end);
  if (executable?)
    #t
  elseif (begin
	    //---*** NOTE: SHGetFileInfoA doesn't reset GetLastError to $NO_ERROR
	    //---*** if the file exists but isn't an executable.  Consequently, we
	    //---*** can't check to see if an error actually occurred and signal it
	    //---*** appropriately; instead, we'll just always claim the file isn't
	    //---*** executable.  (Sigh)
	    // let status = win32-raw-last-error();
	    // status = $ERROR_BAD_EXE_FORMAT
	    //   | status = $ERROR_ACCESS_DENIED
	    //   | status = $NO_ERROR
	    #t
	  end)
    #f
  else
    win32-file-system-error("get attributes of", "%s", file)
  end
end method %file-property;


///
define function %do-directory
    (f :: <function>, directory :: <microsoft-directory-locator>) => ()
  let directory = %expand-pathname(directory);
  let wild-file = make(<microsoft-file-locator>, directory: directory, name: "*.*");
  let find-handle = primitive-wrap-machine-word(integer-as-raw($INVALID_HANDLE_VALUE));
  with-stack-win32-find-data (wfd, directory)
    block ()
      find-handle := primitive-wrap-machine-word
		       (primitive-cast-pointer-as-raw
			  (%call-c-function ("FindFirstFileA", c-modifiers: "__stdcall")
			       (lpFileName :: <raw-byte-string>,
				lpFindFileData :: <raw-c-pointer>)
			    => (hFindFile :: <raw-c-pointer>)
			     (primitive-string-as-raw(as(<byte-string>, wild-file)),
			      primitive-cast-raw-as-pointer
				(primitive-unwrap-machine-word(wfd)))
			   end));
      if (primitive-machine-word-equal?
	    (primitive-unwrap-machine-word(find-handle),
	     integer-as-raw($INVALID_HANDLE_VALUE)))
	win32-file-system-error("start listing of", "%s", directory)
      end;
      let have-file? :: <boolean> = #t;
      while (have-file?)
	let attributes :: <machine-word> = win32-find-data-attributes(wfd);
	let filename :: <byte-string> = win32-find-data-filename(wfd);
	let type :: <file-type> = attributes-to-file-type(attributes);
	unless (type == #"directory" & (filename = "." | filename = ".."))
	  f(directory,
	    filename,
	    type)
	end;
	unless (primitive-raw-as-boolean
		  (%call-c-function ("FindNextFileA", c-modifiers: "__stdcall")
		       (hFindFile :: <raw-c-pointer>, lpFindFileData :: <raw-c-pointer>)
		    => (closed? :: <raw-c-signed-int>)
		     (primitive-cast-raw-as-pointer
			(primitive-unwrap-machine-word(find-handle)),
		      primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(wfd)))
		   end))
	  if (win32-raw-last-error() = $ERROR_NO_MORE_FILES)
	    have-file? := #f
	  else
	    win32-file-system-error("continue listing of", "%s", directory)
	  end
	end
      end;
    cleanup
      if (primitive-machine-word-not-equal?
	    (primitive-unwrap-machine-word(find-handle),
	     integer-as-raw($INVALID_HANDLE_VALUE)))
	%call-c-function ("FindClose", c-modifiers: "__stdcall")
	    (hFindFile :: <raw-c-pointer>) => (closed? :: <raw-c-signed-int>)
	  (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(find-handle)))
        end
      end
    end
  end
end function %do-directory;


///
define function %create-directory 
    (directory :: <microsoft-directory-locator>)
 => (directory :: <microsoft-directory-locator>)
  let directory = %expand-pathname(directory);
  if (primitive-raw-as-boolean
        (%call-c-function ("CreateDirectoryA", c-modifiers: "__stdcall")
	     (dirPathname :: <raw-byte-string>, securityAttributes :: <raw-c-pointer>)
          => (created? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, directory)),
            primitive-cast-raw-as-pointer(integer-as-raw(0)))
         end))
    directory                           // Return the fully expanded pathname
  else
    win32-file-system-error("create the directory", "%s", directory)
  end
end function %create-directory;


///
define function %delete-directory
    (directory :: <microsoft-directory-locator>) => ()
  let directory = %expand-pathname(directory);
  unless (primitive-raw-as-boolean
	    (%call-c-function ("RemoveDirectoryA", c-modifiers: "__stdcall")
		 (dirPathname :: <raw-byte-string>)
	      => (deleted? :: <raw-c-signed-int>)
	       (primitive-string-as-raw(as(<byte-string>, directory)))
	     end))
    win32-file-system-error("delete", "%s", directory)
  end
end function %delete-directory;


///---*** Is there an easier way?  (Look into it ...)
define function %directory-empty?
    (directory :: <microsoft-directory-locator>) => (empty? :: <boolean>)
  ~%file-exists?(directory)
    | block (return)
	%do-directory
	  (method (directory :: <microsoft-directory-locator>, name :: <string>, type :: <file-type>)
	     ignore(directory); ignore(name); ignore(type);
	     return(#f)
	   end,
	   directory);
	#t
      end
end function %directory-empty?;


///
define function %home-directory
    () => (home-directory :: false-or(<microsoft-directory-locator>))
  let drive = environment-variable("HOMEDRIVE");
  let path = environment-variable("HOMEPATH");
  drive & path
    & as(<microsoft-directory-locator>, concatenate-as(<string>, drive, path))
end function %home-directory;


///
define function %working-directory
    () => (working-directory :: false-or(<microsoft-directory-locator>))
  let cdb-size :: <integer> = 1024;
  let curdir-buffer :: <byte-string> = make(<byte-string>, size: cdb-size, fill: '\0');
  let curdir-size :: <integer>
    = raw-as-integer(%call-c-function ("GetCurrentDirectoryA", c-modifiers: "__stdcall")
		         (nBufferLength :: <raw-c-unsigned-long>,
			  lpBuffer :: <raw-byte-string>)
		      => (nCurDirsize :: <raw-c-unsigned-long>)
		       (integer-as-raw(cdb-size),
			primitive-string-as-raw(curdir-buffer))
		     end);
  if (curdir-size > cdb-size)
    // Value was too large to fit in our initial buffer but GetCurrentDirectoryA
    // tells us how long it actually is so we can just make a buffer large enough
    let cdb-size :: <integer> = curdir-size + 1;
    curdir-buffer := make(<byte-string>, size: cdb-size, fill: '\0');
    curdir-size :=
      raw-as-integer(%call-c-function ("GetCurrentDirectoryA", c-modifiers: "__stdcall")
		         (nBufferLength :: <raw-c-unsigned-long>,
			  lpBuffer :: <raw-byte-string>)
		      => (nCurDirsize :: <raw-c-unsigned-long>)
		       (integer-as-raw(cdb-size),
			primitive-string-as-raw(curdir-buffer))
		     end)
  end;
  if (curdir-size > 0)
    as(<microsoft-directory-locator>, copy-sequence(curdir-buffer, end: curdir-size))
  else
    win32-file-system-error("get the current directory", #f)
  end
end function %working-directory;


///
define function %working-directory-setter
    (new-working-directory :: <microsoft-directory-locator>)
 => (new-working-directory :: <microsoft-directory-locator>)
  let directory = %expand-pathname(new-working-directory);
  unless (primitive-raw-as-boolean
	    (%call-c-function ("SetCurrentDirectoryA", c-modifiers: "__stdcall")
		 (lpPathName :: <raw-byte-string>)
	      => (currentDirectorySet? :: <raw-c-signed-int>)
	       (primitive-string-as-raw(as(<byte-string>, directory)))
	     end))
    win32-file-system-error("set the current directory", "to %s", directory)
  end;
  directory
end function %working-directory-setter;


///
define function %temp-directory
    () => (temp-directory :: <microsoft-directory-locator>)
  with-stack-path (path-buffer)
    let path-size = raw-as-integer
                      (%call-c-function ("GetTempPathA", c-modifiers: "__stdcall")
			   (bufferSize :: <raw-c-unsigned-long>,
			    bufferPtr :: <raw-c-pointer>)
			=> (bufferUsed :: <raw-c-unsigned-long>)
			 (integer-as-raw($MAX_PATH),
			  primitive-string-as-raw(path-buffer))
		       end);
    if (path-size ~= 0)
      as(<microsoft-directory-locator>, copy-sequence(path-buffer, end: path-size))
    else
      win32-file-system-error("find temporary directory", #f)
    end
  end
end function %temp-directory;


///
define function %root-directories
    () => (roots :: <sequence>)
  with-stack-path (path-buffer)
    let path-size = raw-as-integer
		      (%call-c-function ("GetLogicalDriveStringsA", c-modifiers: "__stdcall")
			   (bufferSize :: <raw-c-unsigned-long>,
			    bufferPtr :: <raw-c-pointer>)
			=> (bufferUsed :: <raw-c-unsigned-long>)
			 (integer-as-raw($MAX_PATH),
			  primitive-string-as-raw(path-buffer))
		       end);
    if (path-size ~= 0)
      let roots = list();
      let start = 0;
      block (return)
	while (start < path-size)
	  let fini = start;
	  until (path-buffer[fini] = '\0')
	    fini := fini + 1
	  end;
	  if (start = fini)
	    return()
	  else
	    roots := add!(roots, copy-sequence(path-buffer, start: start, end: fini));
	    start := fini + 1
	  end
	end
      end;
      map(curry(as, <microsoft-directory-locator>), reverse(roots))
    else
      win32-file-system-error("find root directories", #f)
    end
  end
end function %root-directories;
