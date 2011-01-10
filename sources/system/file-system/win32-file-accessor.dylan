Module:       system-internals
Synopsis:     Win32 stream accessors
Author:       Eliot Miranda, Scott McKay, Marc Ferguson, Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Should really signal a subclass of <file-error> ...
define function win32-file-error
    (accessor :: <native-file-accessor>, operation :: <string>,
     additional-information, #rest additional-information-args)
  let status-message = win32-last-error-message();
  if (additional-information)
    apply(error,
          concatenate("%s: Can't %s ", additional-information),
          status-message, operation, additional-information-args)
  else
    error("%s: Can't %s", status-message, operation)
  end;
  #f
end function win32-file-error;

// Legal values for direction are #"input", #"output", #"input-output"
// Legal values for if-exists are #"new-version", #"overwrite", #"replace",
//                                #"truncate", #"signal", #"append"
// NB #"append" does _not_ imply unix open(2) append semantics, _only_
// that writing is likely to continue from the end.  So its merely a hint
// as to where to go first.
// Legal values for if-does-not-exist are #"signal", #"create"
define sideways method accessor-open
    (accessor :: <native-file-accessor>, locator :: <pathname>,
     #rest keywords,
     #key direction = #"input", if-exists, if-does-not-exist,
       file-descriptor: initial-file-handle = #f, // :: false-or(<machine-word>)
       file-position: initial-file-position = #f, // :: false-or(<integer>)?
       file-size: initial-file-size = #f, // :: false-or(<integer>)?
       overlapped? :: <boolean> = #f,
       share? :: <boolean> = #t, // only shared access allowed in the past
       share-mode :: one-of(#"default", #"exclusive", #"share-read",
			    #"share-write", #"share-read-write") = #"default",
     #all-keys) => ();
  block (return)
    if (initial-file-position | initial-file-size)
      error("Cannot create a file accessor which specifies either"
	      "file-position: or file-size: keywords but does not specify"
	      "file-handle:");
    end if;
    select (direction)
      #"input" =>
	if-exists := #"overwrite";
	if-does-not-exist := if-does-not-exist | #"signal";
      #"output" =>
	if-exists := if-exists | #"new-version";
	if-does-not-exist := if-does-not-exist | #"create";
      #"input-output" =>
	if-exists := if-exists | #"overwrite";
	if-does-not-exist := if-does-not-exist | #"create";
    end;
    let fdwAccess
      = select (direction)
	  #"input"        => $GENERIC_READ;
	  #"output"       => $GENERIC_WRITE;
	  #"input-output" => logior($GENERIC_READ, $GENERIC_WRITE);
	end;
    // Actually the #"default" share-mode doesn't really make a sense
    // at all it's here for backward compatibility only.  The default
    // translates as: 
    // 
    // If it's input, allow others to read and nobody else to write.
    // That isn't senseless but but isn't consistent with the behavior
    // of input-output.
    // If it's output, allow others to write but nobody to read.  Why
    // on earth allow others to write?
    // If it's input-output allow others to read or write.  That
    // makes no sense.  The logic of the others should have the access
    // be exclusive for this case.
    // I expect the default was intended to be share read and write,
    // but somebody thought  you couldn't have the access different
    // from the direction somehow?
    if (share-mode = #"default" & (~share?))
      share-mode := #"exclusive";
    end if;
    let fdwShareMode
      =  select (share-mode by \==)
	   #"default" =>
	     select (direction)
	       #"input"        => $FILE_SHARE_READ;
	       #"output"       => $FILE_SHARE_WRITE;
	       #"input-output" => 
		 logior($FILE_SHARE_READ, $FILE_SHARE_WRITE);
	     end select;
	   #"exclusive" => 0;
	   #"share-read" => $FILE_SHARE_READ;
	   #"share-write" => $FILE_SHARE_WRITE;
	   #"share-read-write" => 
	     logior($FILE_SHARE_READ, $FILE_SHARE_WRITE);
	 end select;
    let path = as(<string>, locator);
    let exists :: <boolean> = win32-file-exists?(path);
    let fdwCreate = 0;
    if (exists)
      select (if-exists)
	#"signal" =>
	  return(signal(make(<file-exists-error>, locator: locator)));
	#"new-version", #"replace" =>
	  fdwCreate := $CREATE_ALWAYS;
	#"overwrite", #"append" =>
	  fdwCreate := $OPEN_EXISTING;
	#"truncate" =>
	  fdwCreate := $TRUNCATE_EXISTING;
      end
    else
      select (if-does-not-exist)
	#"signal" =>
	  return(signal(make(<file-does-not-exist-error>, locator: locator)));
	#"create" =>
	  fdwCreate := $CREATE_NEW;
      end
    end;
    let handle = 
      win32-open/create(path, fdwAccess, fdwShareMode, fdwCreate,
			overlapped?: overlapped?);
    if (handle)
      apply(accessor-open, accessor, handle, file-descriptor: handle, keywords);
      if (if-exists == #"append")
        accessor.accessor-position := accessor.accessor-size;
      end;
    else
      if (win32-access-error?())
	return(signal(make(<invalid-file-permissions-error>, locator: locator)))
      else
	return(win32-file-error(accessor, "open/create", #f))
      end if
    end if;
  end block;
end method accessor-open;
