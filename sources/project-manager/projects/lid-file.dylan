Module:  lid-projects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function read-file-library-description
    (location :: <file-locator>) => (library-name, file-names, properties)
  let properties = read-file-library-description-internal(location);

  let library-entry = element(properties, #"library", default: #f);
  let files-entry = element(properties, #"files", default: #f);
  // Sanity checks.
  unless (library-entry)
    signal(make(<badly-formed-file-header>, 
		format-string: "The library description file %= does not contain "
		  "the entry for library, which is mandatory.",
		format-arguments: vector(location)))
  end;
  unless (library-entry.size = 1)
    signal(make(<badly-formed-file-header>, 
		format-string: "The library description file %= contains multiple "
		  "entries for the library: option.",
		format-arguments: vector(location)))
  end;
  values(as(<symbol>, library-entry.first),
	 files-entry,
	 properties)
end function;

define function read-file-library-description-internal
    (location :: <file-locator>) => (properties)
  iterate do-it ()
    block ()
      let properties = read-file-header(location);
      let lid-entry = element(properties, #"lid", default: #f);
      local method merge-properties(p1, p2)
	      do(method(key) p2[key] := p1[key] end,
		 key-sequence(p1));
	      p2
	    end;
		
      if(lid-entry)
	let original-file
	  = make(<file-locator>, 
		 directory: location.locator-directory,
		 name:      lid-entry.first);
	let original-properties = read-file-library-description-internal(original-file);
	merge-properties(properties, original-properties)
      else
	properties
      end;
						   
    exception (r :: <simple-restart>)
	do-it();
    end
  end;
end function;
