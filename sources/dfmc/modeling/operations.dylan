Module:   dfmc-modeling
Synopsis: Model object operations.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro generic-&override-definer
  { define generic-&override ?:name (?args:*) end } =>
    { define generic "^" ## ?name (?args);
      define &override-function "^" ## ?name end;
      define &override-function-methods ?name (?args) (?args) end }
end macro;

define macro &override-function-methods-definer
  { define &override-function-methods ?:name (?parms:*) (?args:*) end } =>
    { define method "^" ## ?name (?parms)
        let (#rest vals) = ?name(?args);
        if (size(vals) = 1)
          make-compile-time-literal(vals[0])
        else
          apply(values, map(make-compile-time-literal, vals))
        end if
      end }
args:
  { ?arg:name, ... } => { make-run-time-literal(?arg), ... }
  { ?arg:name } => { make-run-time-literal(?arg) }
  { } => { }
end macro;

define method make-run-time-literal (object)
  object
end method;

define method make-run-time-literal (object :: <&single-float>)
  ^raw-object-value(^%single-float-data(object))
end method;

define method make-run-time-literal (object :: <&double-float>)
  ^raw-object-value(^%double-float-data(object))
end method;

define method make-run-time-literal (object :: <&machine-word>)
  ^raw-object-value(^%machine-word-data(object))
end method;

define method make-run-time-literal (object :: <&double-integer>)
  make(<double-integer>, 
       low:  ^raw-object-value(^%%double-integer-low(object)),
       high: ^raw-object-value(^%%double-integer-high(object)))
end method;

define &override-function ^concatenate end;

define method ^concatenate (x :: <simple-object-vector>, #rest others)
  if (every?(rcurry(instance?, <simple-object-vector>), others))
    apply(concatenate, x, others);
  else 
    error("NO APPLICABLE concatenate METHOD");
  end if;    
end method;

///---*** NOTE: What's the right make-run-time-literal for <&machine-word>?

define macro arithmetic-&override-definer
  { define arithmetic-&override ?name-and-args:* end } =>
    { define generic-&override ?name-and-args end }
end;

define arithmetic-&override \+ (object1, object2) end;
define arithmetic-&override \- (object1, object2) end;
define arithmetic-&override \* (object1, object2) end;
define arithmetic-&override \/ (object1, object2) end;
define arithmetic-&override negative (object1) end;
define arithmetic-&override floor (object1) end;
define arithmetic-&override ceiling (object1) end;
define arithmetic-&override round (object1) end;
define arithmetic-&override truncate (object1) end;
define arithmetic-&override floor/ (object1, object2) end;
define arithmetic-&override ceiling/ (object1, object2) end;
define arithmetic-&override round/ (object1, object2) end;
define arithmetic-&override truncate/ (object1, object2) end;
define arithmetic-&override modulo (object1, object2) end;
define arithmetic-&override remainder (object1, object2) end;
define arithmetic-&override abs (object1) end;
define arithmetic-&override ash (integer1, count) end;

define generic-&override \< (x, y) end;
define generic-&override \= (x, y) end;

///---*** What about non-<integer> constants?
define &override-function ^integer-as-raw (x :: <integer>) => (raw-x :: <&raw-integer>)
  make-raw-literal(x)
end;

/// NOTE: If the value is too big to be an <integer>, this code will
/// signal an error causing the optimizer to give up on this attempt ...
define &override-function ^raw-as-integer (raw-x :: <&raw-machine-word>)
 => (x :: <integer>)
  as(<integer>, ^raw-object-value(raw-x))
end;

///---*** What about non-<integer> constants?
define &override-function ^coerce-integer-to-machine-word (x :: <integer>)
 => (mwx)
  make(<&machine-word>, data: make(<&raw-machine-word>, value: x))
end;


/// HACK: TEMPORARY until we have proper model class coercions
///       in constant folding

/// HACK: EMULATOR BEFORE INTEGER BECAUSE DI = I

define method ^as (type, object :: <double-integer>) => (object)
  if (^instance?(object, type))
    object
  elseif (type == dylan-value(#"<machine-word>"))
    make(<&machine-word>, 
         data: make(<&raw-machine-word>, value: %double-integer-low(object)))
  else
    error("NO APPLICABLE AS METHOD");
  end if;
end method;

define method ^as (type, object :: <integer>) => (object)
  if (^instance?(object, type))
    object
  elseif (type == dylan-value(#"<machine-word>"))
    make(<&machine-word>, data: make(<&raw-machine-word>, value: object))
  else
    error("NO APPLICABLE AS METHOD");
  end if;
end method;

define method ^as (type, object :: <byte-character>) => (object)
  if (^instance?(object, type))
    object
  elseif (type == dylan-value(#"<integer>"))
    as(<integer>, object)
  else
    error("NO APPLICABLE AS METHOD")
  end
end method;

define &override-function ^immutable-vector (#rest values)
  => (vector :: <simple-object-vector>)
  let vector = as(<simple-object-vector>, values);
  immutable-model(vector)
end;

define &override-function ^make-<signature>
    (next?,
     required :: <simple-object-vector>,
     values :: <simple-object-vector>,
     rest-value /* false-or(<type>) */,
     signature-properties :: <integer>)
 => (signature :: <&signature>)
  ^make(<&signature>,
	required:   as-sig-types(required),
	values:     as-sig-types(values),
	rest-value: rest-value,
	next?:      next?,
	properties: signature-properties)
end;

define &override-function ^make-<keyword-signature>
    (next?, 
     required :: <simple-object-vector>,
     values :: <simple-object-vector>,
     rest-value /* ^false-or(<&type>) */,
     signature-properties :: <integer>,
     keys :: <simple-object-vector>,
     key-types :: <simple-object-vector>)
 => (signature :: <&keyword-signature>)
  ^make(<&signature>,
        key?:       #t, // the make method works out which class
	required:   as-sig-types(required),
	keys:       mapped-model(keys),
	key-types:  as-sig-types(key-types),
	values:     as-sig-types(values),
	rest-value: rest-value,
	next?:      next?,
	properties: signature-properties)
end;

define &override-function ^%copy-method-using-signature
    (function :: <&lambda>, new-signature :: <&signature>)
  // don't have to copy the function, if we're constant-folding
  let new-function :: <&lambda> = function;
  new-function.^function-signature := new-signature;
  new-function
end;

define &override-function ^object-class end;
define &override-function ^instance? end;
define &override-function ^subtype? end;

// eof

