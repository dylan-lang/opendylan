Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <build-error>(<condition>)
end class;

define open class <linker>(<object>)
end class;

define generic build() => ();

define generic build-system(build-targets :: <sequence>,
			   #key toplevel?, directory) => ();


/// Fake linker defaulting

define variable *default-linker* :: <symbol> = #"microsoft";

define function default-linker
    () => (linker :: <symbol>)
  *default-linker*
end function default-linker;

define function default-linker-setter
    (linker :: <symbol>) => (linker :: <symbol>)
  *default-linker* := linker
end function default-linker-setter;

// Build Error Signaling

define method build-error(format-string :: <string>, #rest args) => ()
  format-out("Build Error: ");
  apply(format-out, format-string, args);
  force-output(*standard-output*);
end method;

// convenient macro to continuously read and process stream input until eof

define macro with-stream-input
  { with-stream-input () ?:body end }
    => { 
	block()
	  while (#t)
	    ?body
	  end while;
	exception(<end-of-stream-error>)
	end block;
       }
end macro;
