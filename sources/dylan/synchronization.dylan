module:    threads-internal
Synopsis:  The implementation of the <synchronization> class
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define open abstract class <synchronization> (<object>)

  constant slot synchronization-name :: <optional-name>,
    init-value: #f, init-keyword: name:;
  
end class;


define open generic synchronization-name 
   (object :: <synchronization>) => (name :: <optional-name>);

define open generic wait-for
   (object :: <synchronization>, #key timeout = #f) => (success?);

define open generic release
   (object :: <synchronization>, #key) => ();


define generic millisecs (secs :: <real>) => (millisecs :: <integer>) ;

define method millisecs (secs :: <real>) => (millisecs :: <integer>)
  round(secs * 1000);
end method;

define method millisecs (secs :: <integer>) => (millisecs :: <integer>)
  secs * 1000;
end method;



define function check-synchronization-creation 
   (sync :: <synchronization>, res :: <integer>) => ()
  unless (res == $success)
    error(make(<synchronization-creation-error>, synchronization: sync));
  end unless;
end;



define function check-synchronization-finalization
   (sync :: <synchronization>, res :: <integer>) => ()
  unless (res == $success)
    error(make(<synchronization-finalization-error>, synchronization: sync));
  end unless;
end;
