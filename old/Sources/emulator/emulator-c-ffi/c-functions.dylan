Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// All throw-away for LW

define constant generate-arg-name
  = begin
      let count = -1;
      method ()
        count := count + 1;
        as(<symbol>, format(#f, "anonymous-arg-~a", count))
      end
    end;

define class <arg-descriptor> (<object>)
  slot name,
    init-function: generate-arg-name,
    init-keyword:  name:;
  slot type,
    init-keyword: type:;
  slot type-expression,
    init-keyword: type-expression:;
  slot by-reference,
    init-value:   #f,
    init-keyword: by-reference:;
end class;

define class <result-descriptor> (<object>)
  slot type,
    init-keyword: type:;
  slot type-expression,
    init-keyword: type-expression:;
  slot by-reference,
    init-value:   #f,
    init-keyword: by-reference:;
end class;

define method generate-C-call-out (name, spec)
  let in-out = copy-sequence(spec, end: size(spec) - 1);
  let args = choose(rcurry(instance?, <arg-descriptor>), in-out);
  let results = choose(rcurry(instance?, <result-descriptor>), in-out);
  let result = if (empty?(results)) #f else first(results) end;
  let options = last(spec);
  form := generate-lw-dff(name, second(options), args, result);
end method;

define method generate-C-call-in (name, fn-name, spec)
  let in-out = copy-sequence(spec, end: size(spec) - 1);
  let args = choose(rcurry(instance?, <arg-descriptor>), in-out);
  let results = choose(rcurry(instance?, <result-descriptor>), in-out);
  let result = if (empty?(results)) #f else first(results) end;
  let options = last(spec);
  form := generate-lw-fc(name, second(options), args, result);
end method;

// eof
