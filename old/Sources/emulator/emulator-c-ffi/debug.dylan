Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method print (c :: subclass(<C-pointer>), #key stream)
  format(stream, "{class ~s pointer}", 
         referenced-type(c));
end method;

define method print (ptr :: <C-typed-pointer>, #key stream)
  format(stream, "{pointer to ~s ~x}", 
         referenced-type(object-class(ptr)),
         pointer-pointer(ptr));
end method;

define method print (ptr :: <C-string>, #key stream)
  let error = #f;
  let dylan-string = 
    block ()
      as(<string>, ptr)
    exception (c :: <error>)
      error := c;
      #f
    end;
  if (dylan-string)
    format(stream, "{<C-string> ~s ~x}",
           as(<string>, ptr),
           pointer-pointer(ptr))
  else
    format(stream, "{<C-string> ** ~a - not a valid C string ** ~x}",
           error,
           pointer-pointer(ptr))
  end;
end method;

// eof
