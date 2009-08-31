Module:       system-internals
Synopsis:     Unix stream accessors (assuming ~ System V release 5.3 semantics)
Author:       Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $file_create_permissions
  = logior($S_IRUSR, $S_IWUSR, $S_IRGRP, $S_IWGRP, $S_IROTH, $S_IWOTH);

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
       file-position: initial-file-position = #f, // :: false-or(<integer>)?
       file-size: initial-file-size = #f, // :: false-or(<integer>)?
     #all-keys) => ()
  block (return)
    let locator = expand-pathname(locator);
    let pathstring = as(<byte-string>, locator);
    let exists = unix-file-exists?(pathstring);
    let (mode-code, if-exists, if-does-not-exist)
      = select (direction)
	  #"input" =>
	    values($O_RDONLY, 
		   #"overwrite",
		   (if-does-not-exist | #"signal"));
	  #"output" =>
	    values(logior($O_WRONLY, $O_SYNC),
		   (if-exists | #"new-version"),
		   (if-does-not-exist | #"create"));
	  #"input-output" =>
	    values(logior($O_RDWR, $O_SYNC),
		   (if-exists | #"overwrite"),
		   (if-does-not-exist | #"create"));
	end;
    let mode-code 
      = if (exists)
	  select (if-exists)
	    #"signal" =>
	      return(signal(make(<file-exists-error>,
				 locator: as(<posix-file-locator>, locator))));
	    #"new-version", #"replace" =>
	      if (unix-delete-file(pathstring))
		logior(mode-code, $O_CREAT);
	      else
		let errno = unix-errno();
		if (errno = $e_access)
		  return(signal(make(<invalid-file-permissions-error>,
				     locator: locator)));
		else
		  unix-file-error("unlink", "%s", locator);
		end;
	      end;
	    #"overwrite", #"append" => 
	      mode-code;
	    #"truncate" =>
	      logior(mode-code, $O_TRUNC);
	  end
	else
	  select (if-does-not-exist)
	    #"signal" =>
	      return(signal(make(<file-does-not-exist-error>,
                                 locator: as(<posix-file-locator>, locator))));
	    #"create" =>
	      logior(mode-code, $O_CREAT);
	  end
	end;
    let fd = unix-open(pathstring, mode-code, $file_create_permissions);
    if (fd < 0)
      let errno = unix-errno();
      if (errno = $e_access)
	return(signal(make(<invalid-file-permissions-error>,
			   locator: as(<posix-file-locator>, locator))));
      else
        unix-file-error("open", "%s", locator);
      end
    else
      apply(accessor-open, accessor, fd, file-descriptor: fd, keywords);
      if (if-exists == #"append")
        accessor.accessor-position := accessor.accessor-size;
      end;
      // IMPORTANT!!
      // Once the file has been created the required reopen behaviour is
      // overwrite.  E.g., if an if-exists: #"truncate" file-stream is
      // reopened after close we don't want it truncated again.
      // accessor.exists-behaviour = #"overwrite";
      // By the same token, if the underlying file has been removed by the
      // time a reopen occurs a signal is appropriate.
      // accessor.not-exists-behaviour = #"signal";
    end
  end
end method accessor-open;
