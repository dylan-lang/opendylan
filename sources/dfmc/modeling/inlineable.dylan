module: dfmc-modeling
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic inlineable? (object) => (inlineable? :: <boolean>, value);

define method inlineable?
    (object :: <object>) => (inlineable? :: <boolean>, value)
  values(#f, #f)
end method;

define method inlineable?
    (object :: <integer>) => (inlineable? :: <boolean>, value)
  values(#t, object)
end method;

define method inlineable?
    (object :: <byte-character>) => (inlineable? :: <boolean>, value)
  values(#t, object)
end method;

define method inlineable?
    (object :: <symbol>) => (inlineable? :: <boolean>, value)
  values(#t, object)
end method;

define method inlineable?
    (object :: <empty-list>) => (inlineable? :: <boolean>, value)
  values(#t, object)
end method;

define method inlineable?
    (object :: <simple-object-vector>) => (inlineable? :: <boolean>, value)
  if (empty?(object))
    values(#t, #[])
  else
    values(#f, #f)
  end if
end method;

define method inlineable?
    (object :: <byte-string>) => (inlineable? :: <boolean>, value)
  if (empty?(object))
    values(#t, "")
  else
    values(#f, #f)
  end if
end method;

define method inlineable? 
    (object :: <mapped-unbound>) => (inlineable? :: <boolean>, value)
  values(#t, object)
end method;

define method inlineable?
    (object :: <boolean>) => (inlineable? :: <boolean>, value)
  values(#t, object)
end method;

define class <value-model-copier> (<copier>) end class;

// define dont-copy-slots  <dood-mapped-object>           using <value-model-copier> =
//   { dood-pointer => #f };

define dont-copy-slots  <dood-mapped-and-owned-object> using <value-model-copier> =
  { object-dood  => #f };

define dont-copy-slots <model-properties> using <value-model-copier> =
  { private-model-definition => #f,
    private-model-creator    => (*current-dependent* |
      error("Attempt to copy a model outside of proper compilation-context")) };

define method deep-copy 
    (copier :: <value-model-copier>, object :: <dood-slot-value-proxy>)
 => (value)
  deep-copy(copier, dood-force-slot-value-proxy(object))
end method;

define function current-value-model-copier ()
  copier-reset
    (library-description-value-model-copier(dylan-library-description())
       | (library-description-value-model-copier(dylan-library-description()) 
            := make(<value-model-copier>)));
end function;

define function shared-inlineable? 
    (object, by-value?) => (inlineable? :: <boolean>, value)
  if (by-value?)
    let inline-object 
      = if (model-creator(object) == *current-dependent*)
	  object
	else
	  deep-copy(current-value-model-copier(), object);
	end if;
    values(#t, inline-object)
  elseif (model-has-definition?(object))
    values(#t, object)
  else 
    values(#f, #f)
  end if
end function;

define method inlineable? 
    (object :: <&object>) => (inlineable? :: <boolean>, value)
  shared-inlineable?
    (object, ^instance?(object, dylan-value(#"<value-object>")));
end method;

define method inlineable? 
    (object :: <&raw-object>) => (inlineable? :: <boolean>, value)
  shared-inlineable?(object, #t);
end method;

// eof
