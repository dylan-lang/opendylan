Module: dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method path-parser 
    (string :: <byte-string>) => (loc :: <file-system-locator>)
  as(<file-system-locator>, string);
end method;

define inline method p-parser 
    (string :: <byte-string>) => (loc :: <file-system-locator>)
  path-parser(string);
end method;

define sideways method contents 
    (file :: <file-locator>, #key) => (contents :: <sequence>)
  with-open-file (stream = file, direction: #"input")
    read-to-end(stream);    
  end;
end method;

define sideways method contents-setter
    (contents :: <byte-string>, file :: <file-locator>, #key) 
 => (contents :: <byte-string>)
  with-open-file 
      (stream = file, direction: #"output", element-type: <byte-character>)
    write-text(stream, contents);
    contents
  end;
end method;

define sideways method contents-setter
    (contents :: <byte-vector>, file :: <file-locator>, #key) 
 => (contents :: <byte-vector>)
  with-open-file 
      (stream = file, direction: #"output", element-type: <byte>)
    write(stream, contents);
    contents
  end;
end method;

define sideways method contents 
    (locator :: <directory-locator>, #key directories? = #f, files? = #t)
 => (contents :: <sequence>)
  collecting ()
    local method process-file (directory, filename, type) => ()
	    let locator
	      = select (type)
		  #"directory" =>
		    if (directories? & filename ~= "." & filename ~= "..")
		      subdirectory-locator(locator, filename)
		    end;
		  #"file", #"link" =>
		    if (files?)
		      make(<file-locator>,
			   directory: locator,
			   name:      filename)
		    end;
		end;
	    locator & collect(locator);
	  end method;
    do-directory(process-file, locator);
  end;
end method;

// eof
