Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define C-subtype <C-character> (<C-char>)
  map: <character>
end class;

define method import-value
    (dylan-class == <character>, c-class == <C-character>, value :: <integer>)
  as(<character>, value)
end method;

define method export-value
    (c-class == <C-character>, value :: <character>)
  as(<integer>, value)
end method;

define C-subtype <C-string> (<C-character>.pointer-type, <string>)
end;

define constant $null-character = as(<character>, 0);

define method make 
    (class :: subclass(<C-string>), 
       #rest args, #key pointer = #f, size = 0, fill = ' ')
  if (pointer)
    next-method()
  else
    let c-string = apply(next-method, class, element-count: size + 1, args);
    for (i from 0 below size)
      c-string[i] := fill
    end;
    c-string[size] := $null-character;
    c-string
  end
end method;

//// Iteration:

// Should really just be pushing a pointer along but I'm not sure the
// compiler can deal with that yet.

define method C-string-next-state 
    (s :: <C-string>, i :: <integer>)
  i + 1
end method;

define method C-string-finished-state? 
    (s :: <C-string>, i :: <integer>, limit)
  s[i] == $null-character
end method;

define method C-string-current-key 
    (s :: <C-string>, i :: <integer>)
  i
end method;

define method C-string-current-element
    (s :: <C-string>, i :: <integer>)
  s[i]
end method;

define method C-string-current-element-setter
    (val :: <character>, s :: <C-string>, i :: <integer>)
  s[i] := val
end method;

define method C-string-copy-state
    (s :: <C-string>, i :: <integer>)
  i
end method;

define method forward-iteration-protocol (s :: <C-string>)
  values
    (0,
     #f,
     C-string-next-state,
     C-string-finished-state?,
     C-string-current-key,
     C-string-current-element,
     C-string-current-element-setter,
     C-string-copy-state)
end method;

// eof
