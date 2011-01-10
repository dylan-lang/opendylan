Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <function> ... end;

define inline function maybe-copy-sig-types 
  (v :: <simple-object-vector>, n :: <integer>) => (res :: <simple-object-vector>)
  if (n = size(v)) v else copy-sequence(v, end: n) end if
end function;

define method function-specializers (f :: <function>)
  let sig = function-signature(f);
  maybe-copy-sig-types(signature-required(sig), signature-number-required(sig))
end method;

define method function-specializers (f :: <repeated-getter-method>)
  vector(slot-owner(method-slot-descriptor(f)), <integer>)
end method;

define method function-specializers (f :: <repeated-setter-method>)
  let slotd = method-slot-descriptor(f);
  vector(slot-type(slotd), slot-owner(slotd), <integer>)
end method;

define method function-specializers (f :: <getter-method>)
  vector(slot-owner(method-slot-descriptor(f)))
end method;

define method function-specializers (f :: <setter-method>)
  let slotd = method-slot-descriptor(f);
  vector(slot-type(slotd), slot-owner(slotd))
end method;

define method function-arguments (function :: <function>)
 => (required :: <integer>, rest? :: <boolean>, key?)
  let sig = function-signature(function);
  values(signature-number-required(sig), 
	 signature-rest?(sig), 
	 if (signature-all-keys?(sig)) 
	   #"all" 
	 else 
	   signature-key?(sig) & signature-keys(sig)
	 end if)
end method;

define method function-arguments (function :: <accessor-method>)
 => (required :: <integer>, rest? :: <boolean>, key?)
  values(function-number-required(function), #f, #f)
end method;

define method function-return-values (function :: <function>)
 => (return-value-types :: <sequence>, rest-return-value :: false-or(<type>))
  let sig = function.function-signature;
  values(maybe-copy-sig-types(sig.signature-values, sig.signature-number-values), 
	 sig.signature-rest-value)
end method;

define method function-return-values (function :: <accessor-method>)
 => (return-value-types :: <sequence>, rest-return-value :: false-or(<type>))
  values(vector(slot-type(method-slot-descriptor(function))), #f)
end method;
