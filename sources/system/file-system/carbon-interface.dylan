Module:       system-internals
Synopsis:     An interface to the Macintosh file system primitives
Author:       Gary Palter, Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// From Files.h ...
define constant $FSSPEC-SIZE  = 70;
define constant $IOPARAM-SIZE = 50;

/// FSpOpenDF opening modes
define constant $fsCurPerm    = 0;
define constant $fsRdPerm     = 1;
define constant $fsWrPerm     = 2;
define constant $fsRdWrPerm   = 3;
define constant $fsRdWrShPerm = 4;

ignore($fsCurPerm, $fsRdWrShPerm);

/// SetFPos positioning modes
define constant $fsAtMark    = 0;
define constant $fsFromStart = 1;
define constant $fsFromLEOF  = 2;
define constant $fsFromMark  = 3;

ignore($fsAtMark, $fsFromLEOF, $fsFromMark);

/// Error codes which indicate incorrect access
define constant $permErr         = -54;
define constant $afpAccessDenied = -5000;

/// Other error codes
define constant $noErr    = 0;
define constant $bdNamErr = -37;
define constant $eofErr   = -39;
define constant $fnfErr   = -43;

/// From ATSUnicode.h ...
define constant $smSystemScript = -1;

/// <OSType> encoding of 'TEXT' ...
define constant $textSignature = as(<machine-word>, #x54455854);


/// EXPLANATION?
///---*** NOTE: TEMPORARY: Needs to be "smarter" when we have proper MacOS locators!
define macro with-locator-as-fsspec
  { with-locator-as-fsspec (?fsspec:name = ?locator:expression, ?status:name) ?:body end }
    => { begin
	   let _path = as(<string>, ?locator);
	   let _size :: <integer> = size(_path);
	   let ?fsspec :: <byte-string> = make(<byte-string>, size: $FSSPEC-SIZE, fill: '\0');
	   let ?status :: <integer>
	     = if (_size > 255)
		 $bdNamErr
	       else
		 let fileName :: <byte-string>
		   = make(<byte-string>, size: _size + 1, fill: '\0'); 
		 primitive-c-unsigned-char-at
		     (primitive-cast-raw-as-pointer(primitive-string-as-raw(fileName)),
		      integer-as-raw(0), integer-as-raw(0))
		   := integer-as-raw(_size);
		 for (_i :: <integer> from 1 to _size)
		   primitive-c-unsigned-char-at
		       (primitive-cast-raw-as-pointer(primitive-string-as-raw(fileName)),
			integer-as-raw(0), integer-as-raw(_i))
		     := primitive-byte-character-as-raw(_path[_i - 1]);
		 end;
		 raw-as-integer
		   (%call-c-function ("FSMakeFSSpec", c-modifiers: "pascal")
			(vRefNum :: <raw-c-signed-short>, dirID :: <raw-c-signed-long>,
			 fileName :: <raw-byte-string>, spec :: <raw-c-pointer>)
		     => (status :: <raw-c-signed-short>)
		      (integer-as-raw(0),
		       integer-as-raw(0),
		       primitive-string-as-raw(fileName),
		       primitive-cast-raw-as-pointer(primitive-string-as-raw(?fsspec)))
		    end)
	       end;
	   ?body
	 end }
end macro with-locator-as-fsspec;

define macro cast-fsspec-as-pointer
  { cast-fsspec-as-pointer(?fsspec:expression) }
    => { primitive-cast-raw-as-pointer(primitive-string-as-raw(?fsspec)) }
end macro cast-fsspec-as-pointer;


/// See Processes.h for the definition of ProcessSerialNumber and ProcessInfoRec ...

define constant $kCurrentProcess =  2;
define constant $PSN-SIZE        =  8;
define constant $PROCINFO-SIZE   = 60;

define variable *current-process-signature* :: false-or(<machine-word>) = #f;

define function ensure-current-process-signature () => ()
  unless (*current-process-signature*)
    let info :: <byte-string> = make(<byte-string>, size: $PROCINFO-SIZE, fill: '\0');
    let psn :: <byte-string> = make(<byte-string>, size: $PSN-SIZE, fill: '\0');
    primitive-c-unsigned-long-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(info)),
	 integer-as-raw(0), integer-as-raw(0))
      := integer-as-raw($PROCINFO-SIZE);
    primitive-c-pointer-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(info)),
	 integer-as-raw(0), integer-as-raw(4))
      := primitive-cast-raw-as-pointer(integer-as-raw(0));
    primitive-c-pointer-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(info)),
	 integer-as-raw(0), integer-as-raw(56))
      := primitive-cast-raw-as-pointer(integer-as-raw(0));
    primitive-c-unsigned-long-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(psn)),
	 integer-as-raw(0), integer-as-raw(4))
      := integer-as-raw($kCurrentProcess);
    let status
      = raw-as-integer
         (%call-c-function ("GetProcessInformation", c-modifiers: "pascal")
	      (PSN :: <raw-c-pointer>, info :: <raw-c-pointer>)
	   => (status :: <raw-c-signed-short>)
	    (primitive-cast-raw-as-pointer(primitive-string-as-raw(psn)),
	     primitive-cast-raw-as-pointer(primitive-string-as-raw(info)))
	  end);
    if (zero?(status))
      *current-process-signature*
	:= primitive-wrap-machine-word
	     (primitive-c-unsigned-long-at
		(primitive-cast-raw-as-pointer(primitive-string-as-raw(info)),
		 integer-as-raw(0), integer-as-raw(20)));
    else
      // Couldn't get the process signature; use '????' instead...
      *current-process-signature* := as(<machine-word>, #x3F3F3F3F)
    end
  end
end function ensure-current-process-signature;


/// Now the actual interfaces ...

define function carbon-file-exists? (path :: <locator>) => (exists? :: <boolean>)
  with-locator-as-fsspec (fsspec = path, status)
    status == $noErr
  end
end function carbon-file-exists?;

define function carbon-create-file (path :: <locator>) => (status :: <integer>)
  ensure-current-process-signature();
  with-locator-as-fsspec (fsspec = path, status)
    raw-as-integer
      (%call-c-function ("FSpCreate", c-modifiers: "pascal")
	   (spec :: <raw-c-pointer>, creator :: <raw-c-unsigned-long>,
	    fileType :: <raw-c-unsigned-long>, scriptTag :: <raw-c-signed-short>)
	=> (status :: <raw-c-signed-short>)
	 (cast-fsspec-as-pointer(fsspec),
	  primitive-unwrap-machine-word(*current-process-signature*),
	  primitive-unwrap-machine-word($textSignature),
	  integer-as-raw($smSystemScript))
       end)
  end
end function carbon-create-file;

define function carbon-delete-file (path :: <locator>) => (status :: <integer>)
  with-locator-as-fsspec (fsspec = path, status)
    if (status == $fnfErr)
      $noErr
    else
      raw-as-integer
	(%call-c-function ("FSpDelete", c-modifiers: "pascal")
	     (spec :: <raw-c-pointer>)
	  => (status :: <raw-c-signed-short>)
	   (cast-fsspec-as-pointer(fsspec))
	 end)
    end
  end
end function carbon-delete-file;

define function carbon-open (path :: <locator>, mode :: <integer>)
 => (refnum :: <integer>, status :: <integer>)
  let refnum-buffer :: <byte-string>
    = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
  with-locator-as-fsspec (fsspec = path, status)
    let status :: <integer>
      = raw-as-integer
          (%call-c-function ("FSpOpenDF", c-modifiers: "pascal")
	       (spec :: <raw-c-pointer>, permission :: <raw-c-signed-char>,
		refNum :: <raw-c-pointer>)
	    => (status :: <raw-c-signed-short>)
	     (cast-fsspec-as-pointer(fsspec),
	      integer-as-raw(mode),
	      primitive-cast-raw-as-pointer(primitive-string-as-raw(refnum-buffer)))
	   end);
    values(raw-as-integer
	     (primitive-c-signed-short-at
		(primitive-cast-raw-as-pointer(primitive-string-as-raw(refnum-buffer)),
		 integer-as-raw(0), integer-as-raw(0))),
	   status)
  end
end function carbon-open;

define function carbon-close (refnum :: <integer>, path :: <locator>) => (status :: <integer>)
  let status :: <integer>
    = raw-as-integer
        (%call-c-function ("FSClose")
	     (refnum :: <raw-c-signed-short>) => (status :: <raw-c-signed-short>)
	   (integer-as-raw(refnum))
	 end);
  if (status == $noErr)
    with-locator-as-fsspec (fsspec = path, status-for-flush)
      %call-c-function ("FlushVol", c-modifiers: "pascal")
	  (volName :: <raw-byte-string>, vRefNum :: <raw-c-signed-short>)
       => (status :: <raw-c-signed-short>)
	(primitive-cast-raw-as-pointer(integer-as-raw(0)),
	 primitive-c-signed-short-at
	   (cast-fsspec-as-pointer(fsspec), integer-as-raw(0), integer-as-raw(0)))
      end
    end
  end;
  status
end function carbon-close;

define function carbon-file-size (refnum :: <integer>)
 => (fsize :: <integer>, status :: <integer>)
  let fsize-buffer :: <byte-string>
    = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
  let status :: <integer>
    = raw-as-integer
       (%call-c-function ("GetEOF", c-modifiers: "pascal")
	    (refNum :: <raw-c-signed-short>, logEOF :: <raw-c-pointer>)
	 => (status :: <raw-c-signed-short>)
	  (integer-as-raw(refnum),
	   primitive-cast-raw-as-pointer(primitive-string-as-raw(fsize-buffer)))
	end);
  values(raw-as-integer
	   (primitive-c-signed-long-at
	      (primitive-cast-raw-as-pointer(primitive-string-as-raw(fsize-buffer)),
	       integer-as-raw(0), integer-as-raw(0))),
	 status)
end function carbon-file-size;

define function carbon-truncate (refnum :: <integer>) => (status :: <integer>)
  raw-as-integer
    (%call-c-function ("SetEOF", c-modifiers: "pascal")
	 (refNum :: <raw-c-signed-short>, logEOF :: <raw-c-signed-long>)
      => (status :: <raw-c-signed-short>)
       (integer-as-raw(refnum), integer-as-raw(0))
     end)
end function carbon-truncate;

define function carbon-set-file-position 
    (refnum :: <integer>, position :: <integer>, mode :: <integer>)
 => (status :: <integer>)
  let status :: <integer>
    = raw-as-integer
        (%call-c-function ("SetFPos", c-modifiers: "pascal")
	     (refNum :: <raw-c-signed-short>, posMode :: <raw-c-signed-short>,
	      posOff :: <raw-c-signed-long>)
	  => (status :: <raw-c-signed-short>)
	   (integer-as-raw(refnum), integer-as-raw($fsFromStart), integer-as-raw(position))
	 end);
  if (status == $eofErr & (mode ~= $fsRdPerm))
    // The upper layers of the Streams library expect set-file-position to extend files
    // open for output if asked to move beyond the current end-of-file...
    %call-c-function ("SetEOF", c-modifiers: "pascal")
	(refNum :: <raw-c-signed-short>, logEOF :: <raw-c-signed-long>)
     => (status :: <raw-c-signed-short>)
      (integer-as-raw(refnum), integer-as-raw(position))
    end;
    raw-as-integer
      (%call-c-function ("SetFPos", c-modifiers: "pascal")
	   (refNum :: <raw-c-signed-short>, posMode :: <raw-c-signed-short>,
	    posOff :: <raw-c-signed-long>)
	=> (status :: <raw-c-signed-short>)
	 (integer-as-raw(refnum), integer-as-raw($fsFromStart), integer-as-raw(position))
       end)
  else
    status
  end
end function carbon-set-file-position;

define function carbon-read
    (refnum :: <integer>, count-buffer :: <byte-string>,
     data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nread :: <integer>, status :: <integer>)
  primitive-c-signed-long-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(count-buffer)),
       integer-as-raw(0), integer-as-raw(0))
    := integer-as-raw(count);
  let status :: <integer>
    = raw-as-integer
        (%call-c-function ("FSRead", c-modifiers: "pascal")
	     (refNum :: <raw-c-signed-short>, count :: <raw-c-pointer>,
	      buffPtr :: <raw-c-pointer>)
	  => (status :: <raw-c-signed-short>)
	   (integer-as-raw(refnum),
	    primitive-cast-raw-as-pointer(primitive-string-as-raw(count-buffer)),
	    primitive-cast-raw-as-pointer
	      (primitive-machine-word-add
		 (primitive-cast-pointer-as-raw
		    (primitive-repeated-slot-as-raw(data,
						    primitive-repeated-slot-offset(data))),
		  integer-as-raw(offset))))
	 end);
  values(raw-as-integer
	   (primitive-c-signed-long-at
	      (primitive-cast-raw-as-pointer(primitive-string-as-raw(count-buffer)),
	       integer-as-raw(0), integer-as-raw(0))),
	 status)
end function carbon-read;

define function carbon-write
    (refnum :: <integer>, count-buffer :: <byte-string>, 
     data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nwritten :: <integer>, status :: <integer>)
  primitive-c-signed-long-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(count-buffer)),
       integer-as-raw(0), integer-as-raw(0))
    := integer-as-raw(count);
  let status :: <integer>
    = raw-as-integer
        (%call-c-function ("FSWrite", c-modifiers: "pascal")
	     (refNum :: <raw-c-signed-short>, count :: <raw-c-pointer>,
	      buffPtr :: <raw-c-pointer>)
	  => (status :: <raw-c-signed-short>)
	   (integer-as-raw(refnum),
	    primitive-cast-raw-as-pointer(primitive-string-as-raw(count-buffer)),
	    primitive-cast-raw-as-pointer
	      (primitive-machine-word-add
		 (primitive-cast-pointer-as-raw
		    (primitive-repeated-slot-as-raw(data,
						    primitive-repeated-slot-offset(data))),
		  integer-as-raw(offset))))
	 end);
  values(raw-as-integer
	   (primitive-c-signed-long-at
	      (primitive-cast-raw-as-pointer(primitive-string-as-raw(count-buffer)),
	       integer-as-raw(0), integer-as-raw(0))),
	 status)
end function carbon-write;

define function carbon-force-output
    (refnum :: <integer>, ioparam-block :: <byte-string>, path :: <locator>)
 => (status :: <integer>)
  primitive-c-unsigned-long-at
      (primitive-cast-pointer-as-raw(primitive-string-as-raw(ioparam-block)),
       integer-as-raw(0), integer-as-raw(12))
    := integer-as-raw(0);
  primitive-c-signed-short-at
      (primitive-cast-pointer-as-raw(primitive-string-as-raw(ioparam-block)),
       integer-as-raw(0), integer-as-raw(24))
    := integer-as-raw(refnum);
  let status :: <integer>
    = raw-as-integer
        (%call-c-function ("PBFlushFileSync")
	     (paramBlock :: <raw-c-pointer>) => (status :: <raw-c-signed-short>)
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(ioparam-block)))
	 end);
  if (status == $noErr)
    with-locator-as-fsspec (fsspec = path, status-for-flush)
      %call-c-function ("FlushVol", c-modifiers: "pascal")
	  (volName :: <raw-byte-string>, vRefNum :: <raw-c-signed-short>)
       => (status :: <raw-c-signed-short>)
	(primitive-cast-raw-as-pointer(integer-as-raw(0)),
	 primitive-c-signed-short-at
	   (cast-fsspec-as-pointer(fsspec), integer-as-raw(0), integer-as-raw(0)))
      end
    end
  end;
  status
end function carbon-force-output;
