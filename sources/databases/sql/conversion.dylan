Module: result-set-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql!conversion.dylan(trunk.5) $


define open generic default-conversion(value :: <object>) 
 => (converted-value :: <object>);

define method default-conversion(value :: <C-int*>) 
 => (converted-value :: <integer>)
  pointer-value(value);
end method;


define method default-conversion(value :: <C-string>) 
 => (converted-value :: <byte-string>)
  as(<byte-string>, value)
end method;


define method default-conversion(value :: <C-double*>) 
 => (converted-value :: <double-float>)
  pointer-value(value);
end method;


define method default-conversion(value :: <C-float*>)
 => (converted-value :: <single-float>)
  pointer-value(value);
end method;

define method conversion-helper (x :: <integer>, #key signed? :: <boolean> = #t)
 => (new-x :: <integer>)
  x
end method conversion-helper;

define method conversion-helper (x :: <machine-word>, #key signed? :: <boolean> = #t)
 => (new-x :: big/<integer>)
  if (signed?)
    if (machine-word-less-than?(x, coerce-integer-to-machine-word(0)))
      make(<double-integer>, low: x, high:
coerce-integer-to-machine-word(-1))
    else
      make(<double-integer>, low: x, high: coerce-integer-to-machine-word(0))
    end
  else
    make(<double-integer>, low: x, high: coerce-integer-to-machine-word(0))
  end
end method conversion-helper;

define method default-conversion(value :: <C-signed-long*>) 
 => (converted-value :: type-union(big/<integer>, <integer>))
  conversion-helper(pointer-value(value));
end method;


define sideways method as(type == <integer>, value :: <C-signed-long*>) 
 => (as-value :: <integer>)
  pointer-value(value);
end method;

define method default-conversion(value :: <C-signed-short*>)
 => (converted-value :: <integer>);
  pointer-value(value);
end method;

define sideways method as(type == <integer>, value :: <C-signed-short*>)
 => (as-value :: <integer>); 
  pointer-value(value);
end method;

define method default-conversion(value :: <C-char*>)
 => (converted-value :: <byte-string>)
  // This is really dumb but I don't know how to do it otherwise.
  as(<byte-string>, make(<C-string>, address: value.pointer-address));
end method;

define sideways method as(type == <byte-string>, value :: <c-char*>)
 => (as-value :: <byte-string>)
  as(<byte-string>, make(<C-string>, address: value.pointer-address));
end method;

define constant $default-coercion = #"default-coercion";
define constant $no-coercion = #"no-coercion";

define constant <coercion-policy> 
  = type-union(singleton($default-coercion),singleton($no-coercion),
	       <sequence>, <object>);


define generic convert-value(coercion-policy :: <coercion-policy>, 
			     value :: <object>, key :: <integer>)
 => (converted-value :: <object>);


define method convert-value(coercion-policy == $default-coercion, 
			    value :: <object>, key :: <integer>)
 => (converted-value :: <object>)
  default-conversion(value)
end method;


define method convert-value(coercion-policy :: <sequence>, 
			    value :: <object>, key :: <integer>)
 => (converted-value :: <object>)
  let not-found = make(<pair>);
  let conversion-function = element(coercion-policy, key, default: not-found);
  if (conversion-function ~== not-found)
    if (instance?(conversion-function, <function>) = #f)
      error("Coercion-policy sequence contains "
	      "an item that is not a function.");
    end if;
    conversion-function(value);
  else
    //++ signal a warning?
    convert-value(#"default-coercion", value, key)
  end if;
end method;


define generic acquire-null-value(indicator :: <object>,
				  index :: <integer>)
 => (null-value :: <object>);


define method acquire-null-value(indicator :: <object>,
				 index :: <integer>)
 => (null-value :: <object>);
  indicator;
end method;


define method acquire-null-value(indicator == $no-indicator,
				 index :: <integer>)
 => (null-value :: <object>);
    error("no output indicator provided.\n");  //+++ throw proper condition
end method; 


define method acquire-null-value(indicator :: <sequence>,
				 index :: <integer>)
 => (null-value :: <object>);
  let not-found = make(<pair>);
  let null-value = element(indicator, index, default: not-found);
  if (null-value == not-found)
    error("no output indicator provided.\n");  //+++ throw proper condition
  else
    null-value
  end if;
end method;


define sideways method as(type == <integer>, 
                 c-float-value :: type-union(<c-float*>, <c-double*>))
  => (as-value :: <integer>)
  let dylan-float = pointer-value(c-float-value);
  truncate(dylan-float);
end method;
