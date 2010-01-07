Module: dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method contents 
    (url :: <file-url>, #key) => (contents)
  start-sockets();
  let directory = locator-directory(url);
  let server = locator-server(directory);
  let port = locator-port(server);
  let host = locator-host(server);
  //--- This is unfortunately tricky, we need to lose the server...
  let file
    = as(<byte-string>, 
	 make(<file-url>,
	      base:      url.locator-base,
	      extension: url.locator-extension,
	      directory: make(<directory-url>,
			      path:      directory.locator-path,
			      relative?: directory.locator-relative?)));
  with-http-stream (stream to host, port: port)
    format-out("Connected: %=\n", file);
    write-http-get
      (stream, host, file,
       User-Agent: "Mozilla/4.0 (compatible; Functional Developer-script; "
                   "Windows NT)");
    // Discard the response header.
    read-http-response-header(stream);
    // Read the content.
    read-to-end(stream);
  end;
end method;

define sideways method as 
    (type == <html-document>, string :: <string>) => (doc :: <html-document>)
  parse-html-from-stream(make(<string-stream>, contents: string))  
end method;
