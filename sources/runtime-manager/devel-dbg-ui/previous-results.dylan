module:       devel-dbg-ui
synopsis:     A history table for the tty debugger interface
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *warned-about-registration?* = #f;

define abstract class <history-variable> (<object>)
end class;

define class <tracked-history-variable> (<history-variable>)

  constant slot history-variable-tracked-object :: <remote-object>,
    required-init-keyword: tracked-object:;

end class;

define class <untracked-history-variable> (<history-variable>)

  constant slot history-variable-value :: <remote-value>,
    required-init-keyword: value:;

end class;


define variable *history-table* = make (<stretchy-vector>, size: 0);

define method add-history-value (v :: <remote-value>) 
  => (indx :: <integer>)
  let hv = #f;
  if (*open-application*)
    block ()
//    if (dylan-object?(*open-application*, v))
//      let robj = register-remote-object (*open-application*, v);
//      hv := make(<tracked-history-variable>, tracked-object: robj);
//    else
        hv := make(<untracked-history-variable>, value: v);
//    end if;
    exception (cannot-register :: <object-registration-error>)
      hv := make(<untracked-history-variable>, value: v);
      unless (*warned-about-registration?*)
        *warned-about-registration?* := #t;
        debugger-message("WARNING: Could not track history object!");
        debugger-message("         History variables may become out of date.");
      end unless;
    end block;
  else
    hv := make(<untracked-history-variable>, 
               value: as-remote-value(0));    // This just won't happen!
  end if;
  add! (*history-table*, hv);
  size (*history-table*);
end method;

define method obtain-value-from-history
    (application :: <application>, history :: <tracked-history-variable>)
         => (v :: <remote-value>)
  let v = remote-object-value(application, 
                              history.history-variable-tracked-object);
  if (v)
    v
  else
    as-remote-value(0)
  end if
end method;

define method obtain-value-from-history
    (application :: <application>, history :: <untracked-history-variable>)
         => (v :: <remote-value>)
  history.history-variable-value;
end method;

define method retrieve-history-value (indx :: <integer>) 
  => (v :: <remote-value>)

  if ((indx < 1) | (indx > size (*history-table*)))
     // This is an error. Return NULL
     as-remote-value (0);
  else
     obtain-value-from-history(*open-application*, *history-table* [indx - 1]);
  end if;
end method;
