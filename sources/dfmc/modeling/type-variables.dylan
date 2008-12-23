module: dfmc-modeling


define primary abstract &class <type-variable> (<object>)
  constant &slot type-variable-name :: <symbol>,
    required-init-keyword: name:;
  constant &slot type-variable-kind :: <symbol>,
    init-value: #"<type>",
    init-keyword: kind:;
  &slot type-variable-constraints :: <list>,
    init-value: #();
end;

define primary &class <simple-type-variable> (<type-variable>)
end;

/*
define primary &class <variable-arity-same-type-variable> (<type-variable>)
end;

define primary &class <variable-arity-different-type-variable> (<type-variable>)
  &slot type-variables :: <list>, init-value: #();
end;
*/

define method ^make (class == <&type-variable>, #rest all-keys, #key)
 => (res :: <&type-variable>)
  apply(^make, <&simple-type-variable>,
	all-keys);
end;
	
	