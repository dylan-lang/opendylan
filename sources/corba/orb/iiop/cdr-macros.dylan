Module: orb-iiop
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TYPECODED VALUE INDIRECTIONS

define thread variable *typecoded-value-indirections* :: <deque> = make(<deque>);

define function typecode-from-nesting (nesting :: <integer>)
 => (tc :: <typecode>)
  element(*typecoded-value-indirections*, nesting);
end function;

define macro with-typecoded-value-indirection
  { with-typecoded-value-indirection (?typecode:expression) ?body:body end }
    => 
    { 
     invoke-typecode-value-indirection(?typecode, method () ?body end method)
     }
end macro;

define method invoke-typecode-value-indirection (typecode :: <typecode>, function :: <function>)
  block ()
    push(*typecoded-value-indirections*, typecode);
    function();
  cleanup
    pop(*typecoded-value-indirections*);
  end block;
end method;
