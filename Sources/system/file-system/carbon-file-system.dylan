Module:	      system-internals
Author:       Gary Palter
Synopsis:     Mac OS Carbon implementation of the File System library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///---*** NOTE: Need to switch to new HFS+ APIs as soon as possible!

define function %expand-pathname
    (path :: <string>) => (expanded-path :: <macintosh-file-system-locator>)
  path
end function %expand-pathname;

define function %shorten-pathname
    (path :: <string>) => (shortened-path :: <macintosh-file-system-locator>)
  path
end function %shorten-pathname;


///
define function %file-exists?
    (file :: <macintosh-file-system-locator>) => (exists? :: <boolean>)
  with-locator-as-fsspec (fsspec = file, status)
    status == $noErr
  end
end function %file-exists?;


///
define function %file-type
    (file :: <macintosh-file-system-locator>) => (file-type :: <file-type>)
  with-file-catalog-info (catalog-data = file)
    if (catalog-data.attributes.directory?)
      #"directory"
    elseif (catalog-data.finder-info.alias?)
      #"link"
    else
      #"file"
    end
  end
end function %file-type;


///
define function %link-target 
    (link :: <macintosh-file-system-locator>) => (target :: <macintosh-file-system-locator>)
  error(make(<file-system-error>,
	     format-string: "link-target is not yet implemented",
	     format-arguments: #()))
end function %link-target;


///
define function %delete-file 
    (file :: <macintosh-file-system-locator>) => ()
  with-locator-as-fsspec (fsspec = file, status)
    if (status == $fnfErr)
      $noErr
    else
      let status :: <integer>
	= raw-as-integer
	    (%call-c-function ("FSpDelete", c-modifiers: "pascal")
		 (spec :: <raw-c-pointer>)
	      => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(fsspec))
	     end);
      unless (status == $noErr)
	carbon-file-error(status, "delete", "%s", file)
      end
    end
  end
end function %delete-file;


///
define function %copy-file
    (source :: <macintosh-file-system-locator>, destination :: <macintosh-file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  if (if-exists == #"replace" & %file-exists?(destination))
    %delete-file(destination)
  end;
  with-locator-as-fsspec (source-fsspec = source, status)
    // The Mac OS file copy API is a bit "curious".  Rather than passing it the
    // destination directly, you must pass it the parent directory of the destination
    // and the filename of destination iff it's not the same as the source filename.
    let destination = %expand-pathname(destination);
    let (destination-parent, destination-name)
      = if (instance?(destination, <macintosh-directory-locator>))
	  values(destination, #f)
	else
	  let destination-name = locator-name(destination);
	  values(locator-directory(destination),
		 (locator-name(source) ~= destination-name) & destination-name)
	end;
    with-locator-as-fsspec (destination-parent-fsspec = destination-parent, status)
      let status :: <integer>
	= raw-as-integer
	    (%call-c-function ("FSpFileCopy", c-modifiers: "pascal")
		 (srcSpec :: <raw-c-pointer>, dstSpec :: <raw-c-pointer>,
		  copyName :: <raw-c-pointer>, copyBufferPtr :: <raw-c-pointer>,
		  copyBufferSize :: <raw-c-signed-long>, preflight :: <raw-c-unsigned-char>)
	      => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(source-fsspec),
		cast-fsspec-as-pointer(destination-parent-fsspec),
		if (destination-name)
		  with-pascal-string (destination-name = destination-name)
		    cast-pascal-string-as-pointer(destination-name)
		  end
		else
		  primitive-cast-raw-as-pointer(integer-as-raw(0))
		end,
		primitive-cast-raw-as-pointer(integer-as-raw(0)),
		integer-as-raw(0),
		integer-as-raw(1))
	     end);
      unless (status == $noErr)
	carbon-file-error(status, "copy", "%s to %s", source, destination)
      end
    end
  end
end function %copy-file;


///
define function %rename-file
    (source :: <macintosh-file-system-locator>, destination :: <macintosh-file-system-locator>,
     #Key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  if (if-exists == #"replace" & %file-exists?(destination))
    %delete-file(destination)
  end;
  with-locator-as-fsspec (source-fsspec = source, status)
    // The Mac OS rename/move API is a bit "curious".  Rather than passing it the
    // destination directly, you must pass it the parent directory of the destination
    // and the filename of destination iff it's not the same as the source filename.
    let destination = %expand-pathname(destination);
    let (destination-parent, destination-name)
      = if (instance?(destination, <macintosh-directory-locator>))
	  values(destination, #f)
	else
	  let destination-name = locator-name(destination);
	  values(locator-directory(destination),
		 (locator-name(source) ~= destination-name) & destination-name)
	end;
    with-locator-as-fsspec (destination-parent-fsspec = destination-parent, status)
      let status :: <integer>
	= raw-as-integer
	    (%call-c-function ("FSpMoveRenameCompat", c-modifiers: "pascal")
		 (srcSpec :: <raw-c-pointer>, dstSpec :: <raw-c-pointer>,
		  copyName :: <raw-c-pointer>)
	      => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(source-fsspec),
		cast-fsspec-as-pointer(destination-parent-fsspec),
		if (destination-name)
		  with-pascal-string (destination-name = destination-name)
		    cast-pascal-string-as-pointer(destination-name)
		  end
		else
		  primitive-cast-raw-as-pointer(integer-as-raw(0))
		end)
	     end);
      unless (status == $noErr)
	carbon-file-error(status, "rename", "%s to %s", source, destination)
      end
    end
  end
end function %rename-file;


///
define function %file-properties 
    (file :: <macintosh-file-system-locator>)
 => (properties :: <explicit-key-collection>)
  let properties = make(<table>);
  with-file-catalog-info (catalog-data = file)
    let finder-info :: <byte-string> = catalog-data.finder-info;
    properties[#"mac-creator"] := creator(finder-info);
    properties[#"mac-filetype"] := filetype(finder-info);
    properties[#"size"]
      := file-data-fork-lEOF(catalog-data) + file-resource-fork-lEOF(catalog-data);
    properties[#"data-fork-size"] := file-data-fork-lEOF(catalog-data);
    properties[#"resource-fork-size"] := file-resource-fork-lEOF(catalog-data);
    properties[#"creation-date"] := date-if-valid(file-creation-date(catalog-data));
    properties[#"modification-date"] := date-if-valid(file-modification-date(catalog-data));
    properties[#"readable?"] := #t;
    properties[#"writeable?"] := ~catalog-data.attributes.locked?;
    properties[#"executable?"] := #f;		//---*** ????
    properties[#"invisible?"] := finder-info.invisible?;
  end;
  properties
end function %file-properties;


/// "Standard" properties not implemented on this platform:
///    author, access-date

/// "Standard" properties not settable on this platform:
///    size, creation-date, modification-date, readable?, executable?

/// Extra properties implemented on this platform:
///    mac-creator (S), mac-filetype (S), invisible? (S), data-fork-size, resource-fork-size
/// [Properties marked with "(S)" are settable]

/// Even though it's not implemented, we'll provide a stub just to be "nice"...
define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"author")
 => (author :: false-or(<string>))
  #f
end method %file-property;

define method %file-property 
    (file :: <macintosh-file-system-locator>, key == #"size")
 => (file-size :: <integer>)
  with-file-catalog-info (catalog-data = file)
    file-data-fork-lEOF(catalog-data) + file-resource-fork-lEOF(catalog-data)
  end
end method %file-property;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"data-fork-size")
 => (fork-size :: <integer>)
  with-file-catalog-info (catalog-data = file)
    file-data-fork-lEOF(catalog-data)
  end
end method %file-property;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"resource-fork-size")
 => (fork-size :: <integer>)
  with-file-catalog-info (catalog-data = file)
    file-resource-fork-lEOF(catalog-data)
  end
end method %file-property;

define inline function date-if-valid 
    (native-date :: <machine-word>) => (date :: false-or(<date>))
  unless (zero?(native-date))
    make(<date>, native-clock: native-date)
  end
end function date-if-valid;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"creation-date")
 => (creation-date :: false-or(<date>))
  with-file-catalog-info (catalog-data = file)
    date-if-valid(file-creation-date(catalog-data))
  end
end method %file-property;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"modification-date")
 => (modification-date :: false-or(<date>))
  with-file-catalog-info (catalog-data = file)
    date-if-valid(file-modification-date(catalog-data));
  end
end method %file-property;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"readable?")
 => (readable? :: <boolean>)
  #t
end method %file-property;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"writeable?")
 => (writeable? :: <boolean>)
  with-file-catalog-info (catalog-data = file)
    ~catalog-data.attributes.locked?
  end
end method %file-property;

define method %file-property-setter 
    (new-writeable? :: <boolean>, file :: <macintosh-file-system-locator>, 
     key == #"writeable?")
 => (new-writeable? :: <boolean>)
  with-locator-as-fsspec (fsspec = file, status)
    let status :: <integer>
      = if (new-writeable?)
	  raw-as-integer
	    (%call-c-function ("FSpRstFLock", c-modifiers: "pascal")
		 (spec :: <raw-c-pointer>) => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(fsspec))
	     end)
	else
	  raw-as-integer
	    (%call-c-function ("FSpSetFLock", c-modifiers: "pascal")
		 (spec :: <raw-c-pointer>) => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(fsspec))
	     end)
	end;
    unless (status == $noErr)
      carbon-file-error(status, if (new-writeable?) "unlock" else "lock" end, "%s", file)
    end
  end;
  new-writeable?
end method %file-property-setter;

///---*** SHOULDN'T THERE BE A WAY?  (e.g., Check for common types?)
define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"executable?")
 => (executable? :: <boolean>)
  #f
end method %file-property;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"mac-creator")
 => (mac-creator :: <byte-string>)
  with-file-catalog-info (catalog-data = file)
    catalog-data.finder-info.creator
  end
end method %file-property;

define method %file-property-setter 
    (new-mac-creator :: <byte-string>, file :: <macintosh-file-system-locator>,
     key == #"mac-creator")
 => (new-mac-creator :: <byte-string>)
  with-locator-as-fsspec (fsspec = file, status)
    let status :: <integer>
      = raw-as-integer
	  (%call-c-function ("FSpChangeCreatorType", c-modifiers: "pascal")
	       (spec :: <raw-c-pointer>, creator :: <raw-c-unsigned-long>,
		fileType :: <raw-c-unsigned-long>)
	    => (status :: <raw-c-signed-short>)
	     (cast-fsspec-as-pointer(fsspec),
	      primitive-unwrap-machine-word(string-as-ostype(new-mac-creator)),
	      integer-as-raw(0))
	   end);
    unless (status == $noErr)
      carbon-file-error(status, "set creator of", "%s to %s", file, new-mac-creator)
    end
  end;
  new-mac-creator
end method %file-property-setter;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"mac-filetype")
 => (mac-filetype :: <byte-string>)
  with-file-catalog-info (catalog-data = file)
    catalog-data.finder-info.filetype
  end
end method %file-property;

define method %file-property-setter 
    (new-mac-filetype :: <byte-string>, file :: <macintosh-file-system-locator>, 
     key == #"mac-filetype")
 => (new-mac-filetype :: <byte-string>)
  with-locator-as-fsspec (fsspec = file, status)
    let status :: <integer>
      = raw-as-integer
	  (%call-c-function ("FSpChangeCreatorType", c-modifiers: "pascal")
	       (spec :: <raw-c-pointer>, creator :: <raw-c-unsigned-long>,
		fileType :: <raw-c-unsigned-long>)
	    => (status :: <raw-c-signed-short>)
	     (cast-fsspec-as-pointer(fsspec),
	      integer-as-raw(0),
	      primitive-unwrap-machine-word(string-as-ostype(new-mac-filetype)))
	   end);
    unless (status == $noErr)
      carbon-file-error(status, "set filetype of", "%s to %s", file, new-mac-filetype)
    end
  end;
  new-mac-filetype
end method %file-property-setter;

define method %file-property
    (file :: <macintosh-file-system-locator>, key == #"invisible?")
 => (invisible? :: <boolean>)
  with-file-catalog-info (catalog-data = file)
    catalog-data.finder-info.invisible?
  end
end method %file-property;

define method %file-property-setter 
    (new-invisible? :: <boolean>, file :: <macintosh-file-system-locator>, 
     key == #"invisible?")
 => (new-invisible? :: <boolean>)
  with-locator-as-fsspec (fsspec = file, status)
    let status :: <integer>
      = if (new-invisible?)
	  raw-as-integer
	    (%call-c-function ("FSpSetIsInvisible", c-modifiers: "pascal")
		 (spec :: <raw-c-pointer>) => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(fsspec))
	     end)
	else
	  raw-as-integer
	    (%call-c-function ("FSpClearIsInvisible", c-modifiers: "pascal")
		 (spec :: <raw-c-pointer>) => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(fsspec))
	     end)
	end;
    unless (status == $noErr)
      carbon-file-error(status, "make", "%s %s", file, 
			if (new-invisible?) "invisible" else "visible" end)
    end
  end;
  new-invisible?
end method %file-property-setter;


///
define function %do-directory
    (f :: <function>, directory :: <macintosh-directory-locator>) => ()
  let directory = %expand-pathname(directory);
  with-locator-as-fsspec (parent-fsspec = directory, status)
    let parent-vRefNum :: <integer> = fsspec-vRefNum(parent-fsspec);
    let parent-dirID :: <machine-word> = fsspec-dirID(parent-fsspec);
    with-pascal-string (parent-name = fsspec-name(parent-fsspec))
      let fsspecs :: <byte-string>
	= make(<byte-string>, size: 128 * $FSSPEC-SIZE, fill: '\0');
      with-short-buffer (item-index-buffer = 1)
	with-short-buffer (item-count-buffer = 0)
	  let status :: <integer> = $noErr;
	  while (status == $noErr)
	    short-buffer-as-integer(item-count-buffer) := 0;
	    status
	      := raw-as-integer
	           (%call-c-function ("GetDirItems", c-modifiers: "pascal")
			(vRefNum :: <raw-c-signed-short>, dirID :: <raw-c-signed-long>,
			 name :: <raw-byte-string>, getFiles :: <raw-c-unsigned-char>,
			 getDirectories :: <raw-c-unsigned-char>, items :: <raw-c-pointer>,
			 reqItemCount :: <raw-c-signed-short>,
			 actItemCount :: <raw-c-pointer>, itemIndex ::  <raw-c-pointer>)
		     => (status :: <raw-c-signed-short>)
		      (integer-as-raw(parent-vRefNum),
		       primitive-unwrap-machine-word(parent-dirID),
		       cast-pascal-string-as-pointer(parent-name),
		       integer-as-raw(1),
		       integer-as-raw(1),
		       primitive-cast-raw-as-pointer(primitive-string-as-raw(fsspecs)),
		       integer-as-raw(128),
		       cast-short-buffer-as-pointer(item-count-buffer),
		       cast-short-buffer-as-pointer(item-index-buffer))
		    end);
	    let item-count :: <integer> = short-buffer-as-integer(item-count-buffer);
	    for (i :: <integer> from 0 below item-count)
	      //---*** NOTE: Should be much simpler once we've got real MacOS locators...
	      let start = i * $FSSPEC-SIZE;
	      let fsspec :: <byte-string>
		= copy-sequence(fsspecs, start: start, end: start + $FSSPEC-SIZE);
	      let filename :: <byte-string> = fsspec-name(fsspec);
	      f(directory,
		filename,
		%file-type(make(<macintosh-file-locator>, name: filename, directory: directory)))
	    end
	  end;
	  unless (status == $fnfErr)
	    carbon-file-error(status, "list contents of", "%s", directory)
	  end
	end
      end
    end
  end
end function %do-directory;


///
define function %create-directory 
    (directory :: <macintosh-directory-locator>)
 => (directory :: <macintosh-directory-locator>)
  with-locator-as-fsspec (fsspec = directory, status)
    with-long-buffer (createdDirID-buffer = 0)
      let status :: <integer>
	= raw-as-integer
	    (%call-c-function ("FSpDirCreate", c-modifiers: "pascal")
		 (spec :: <raw-c-pointer>,
		  scriptTag :: <raw-c-signed-short>, 
		  createDirID :: <raw-c-pointer>)
	      => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(fsspec),
		integer-as-raw($smSystemScript),
		cast-long-buffer-as-pointer(createdDirID-buffer))
	     end);
      unless (status == $noErr)
	carbon-file-error(status, "create the directory", "%s", file)
      end
    end
  end;
  directory
end function %create-directory;


define function %delete-directory 
    (directory :: <macintosh-directory-locator>) => ()
  with-locator-as-fsspec (fsspec = directory, status)
    if (status == $fnfErr)
      $noErr
    else
      let status :: <integer>
	= raw-as-integer
	    (%call-c-function ("FSpDelete", c-modifiers: "pascal")
		 (spec :: <raw-c-pointer>)
	      => (status :: <raw-c-signed-short>)
	       (cast-fsspec-as-pointer(fsspec))
	     end);
      unless (status == $noErr)
	carbon-file-error(status, "delete", "%s", directory)
      end
    end
  end
end function %delete-directory;


///---*** Is there an easier way?  (Look into it ...)
define function %directory-empty? 
    (directory :: <macintosh-directory-locator>) => (empty? :: <boolean>)
  ~%file-exists?(directory)
    | block (return)
	%do-directory
	  (method (directory :: <macintosh-directory-locator>, name :: <string>, type :: <file-type>)
	     ignore(directory); ignore(name); ignore(type);
	     return(#f)
	   end,
	   directory);
	#t
      end
end function %directory-empty?;


///---*** NOTE: Can't we come up with something useful here?
define function %home-directory 
    () => (home-directory :: false-or(<macintosh-directory-locator>))
  #f
end function %home-directory;


///
define function %working-directory
    () => (working-directory :: false-or(<macintosh-directory-locator>))
  with-pascal-string (volName = "")
    with-short-buffer (vrefnum-buffer = 0)
      with-long-buffer (dirid-buffer = 0)
	let status :: <integer>
	  = raw-as-integer
	      (%call-c-function ("HGetVol", c-modifiers: "pascal")
		   (volName :: <raw-c-pointer>, vRefNum :: <raw-c-pointer>,
		    dirID :: <raw-c-pointer>)
		=> (status :: <raw-c-signed-short>)
		 (cast-pascal-string-as-pointer(volName),
		  cast-short-buffer-as-pointer(vrefnum-buffer),
		  cast-long-buffer-as-pointer(dirid-buffer))
	       end);
	if (status == $noErr)
	  fsspec-components-as-locator(short-buffer-as-integer(vrefnum-buffer),
				       long-buffer-as-machine-word(dirid-buffer),
				       #f)
	else
	  carbon-file-error(status, "determine working directory", #f)
	end
      end
    end
  end
end function %working-directory;


///
define function %working-directory-setter
    (new-working-directory :: <macintosh-directory-locator>)
 => (new-working-directory :: <macintosh-directory-locator>)
  with-locator-as-fsspec (fsspec = new-working-directory, status)
    with-long-buffer (dirid-buffer = 0)
      with-short-buffer (ignored-buffer = 0)
	let status :: <integer>
	  = raw-as-integer
	      (%call-c-function ("FSpGetDirectoryID", c-modifiers: "pascal")
		   (spec :: <raw-c-pointer>, theDirectoryID :: <raw-c-pointer>,
		    isDirectory :: <raw-c-pointer>)
		=> (status :: <raw-c-signed-short>)
		 (cast-fsspec-as-pointer(fsspec),
		  cast-long-buffer-as-pointer(dirid-buffer),
		  cast-short-buffer-as-pointer(ignored-buffer))
	       end);
	if (status == $noErr)
	  let status :: <integer>
	    = raw-as-integer
		(%call-c-function ("HSetVol", c-modifiers: "pascal")
		     (volName :: <raw-c-pointer>, vRefNum :: <raw-c-signed-short>,
		      dirID :: <raw-c-signed-long>)
		  => (status :: <raw-c-signed-short>)
		   (primitive-cast-raw-as-pointer(integer-as-raw(0)),
		    integer-as-raw(fsspec-vRefNum(fsspec)),
		    primitive-unwrap-machine-word(long-buffer-as-machine-word(dirid-buffer)))
		 end);
	  unless (status == $noErr)
	    carbon-file-error(status, "set working directory to", "%s", new-working-directory)
	  end
	else
	  carbon-file-error(status, "get directory ID for", "%s", new-working-directory)
	end
      end
    end
  end;
  new-working-directory
end function %working-directory-setter;


///
define function %temp-directory
    () => (temp-directory :: false-or(<macintosh-directory-locator>))
  block (return)
    with-short-buffer (vrefnum = 0)
      with-long-buffer (dirid = 0)
	let status :: <integer>
	  = raw-as-integer
	      (%call-c-function ("FindFolder", c-modifiers: "pascal")
		   (vRefNum :: <raw-c-signed-short>, folderType :: <raw-c-unsigned-long>,
		    createFolder :: <raw-c-unsigned-char>, foundVRefNum :: <raw-c-pointer>,
		    foundDirID :: <raw-c-pointer>)
		=> (status :: <raw-c-signed-short>)
		 (integer-as-raw($kOnSystemDisk), 
		  primitive-unwrap-machine-word($kChewableItemsFolderType),
		  integer-as-raw($kCreateFolder),
		  cast-short-buffer-as-pointer(vrefnum),
		  cast-long-buffer-as-pointer(dirid))
	       end);
	unless (status == $noErr)
	  let status :: <integer>
	    = raw-as-integer
		(%call-c-function ("FindFolder", c-modifiers: "pascal")
		     (vRefNum :: <raw-c-signed-short>, folderType :: <raw-c-unsigned-long>,
		      createFolder :: <raw-c-unsigned-char>, foundVRefNum :: <raw-c-pointer>,
		      foundDirID :: <raw-c-pointer>)
		  => (status :: <raw-c-signed-short>)
		   (integer-as-raw($kOnSystemDisk), 
		    primitive-unwrap-machine-word($kTemporaryFolderType),
		    integer-as-raw($kCreateFolder),
		    cast-short-buffer-as-pointer(vrefnum),
		    cast-long-buffer-as-pointer(dirid))
		 end);
	  unless (status == $noErr)
	    return(#f)
	  end
	end;
	fsspec-components-as-locator(short-buffer-as-integer(vrefnum),
				     long-buffer-as-machine-word(dirid),
				     #f)
      end
    end
  end
end function %temp-directory;


///
define function %root-directories
    () => (roots :: <sequence>)
  let roots :: <stretchy-vector> = make(<stretchy-vector>);
  let fsspecs :: <byte-string> = make(<byte-string>, size: 16 * $FSSPEC-SIZE, fill: '\0');
  with-short-buffer (volume-index-buffer = 1)
    with-short-buffer (volume-count-buffer = 0)
      let status :: <integer> = $noErr;
      while (status == $noErr)
	short-buffer-as-integer(volume-count-buffer) := 0;
	status
	  := raw-as-integer
	       (%call-c-function ("OnLine", c-modifiers: "pascal")
		    (volumes :: <raw-c-pointer>, reqVolCount :: <raw-c-signed-short>,
		     actVolCount :: <raw-c-pointer>, volIndex :: <raw-c-pointer>)
		 => (status :: <raw-c-signed-short>)
		  (primitive-cast-raw-as-pointer(primitive-string-as-raw(fsspecs)),
		   integer-as-raw(16),
		   cast-short-buffer-as-pointer(volume-count-buffer),
		   cast-short-buffer-as-pointer(volume-index-buffer))
		end);
	let volume-count :: <integer> = short-buffer-as-integer(volume-count-buffer);
	for (i :: <integer> from 0 below volume-count)
	  let start = i * $FSSPEC-SIZE;
	  //---*** NOTE: Should use the vRefNum instead once we've got real MacOS locators...
	  let volume-name :: <byte-string>
	    = fsspec-name(copy-sequence(fsspecs, start: start, end: start + $FSSPEC-SIZE));
	  add!(roots,
	       make(<macintosh-directory-locator>,
		    server: make(<macintosh-volume-locator>, name: volume-name)));
	end;
      end;
      unless (status == $nsvErr)
	carbon-file-error(status, "get root volumes", #f)
      end
    end
  end;
  roots
end function %root-directories;
