module:    idvm-harp
Synopsis:  Support for DOSS dumping of IDVM code
Author:    Tony Mann & Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Here we define the dump policy class and some put-specially methods
/// 


define class <idvm-code-dumping-policy> (<doss-policy>)
end class;

define class <resolution-function-proxy> (<object>)
  slot function-name :: <symbol>, required-init-keyword: name:;
end class;

define constant lookup-mangled
  = make(<resolution-function-proxy>, name: #"lookup-mangled");

define constant lookup-variable-reader
  = make(<resolution-function-proxy>, name: #"lookup-variable-reader");

define constant lookup-variable-writer
  = make(<resolution-function-proxy>, name: #"lookup-variable-writer");

define constant lookup-variable-definer
  = make(<resolution-function-proxy>, name: #"lookup-variable-definer");

define constant lookup-constant-definer
  = make(<resolution-function-proxy>, name: #"lookup-constant-definer");


define variable *debug-idvm-doss* = #t;
define variable *idvm-debug-stream* = *standard-output*;


define method doss-message (string :: <string>, #rest args) => ()
  if (*debug-idvm-doss*)
    apply(format, *idvm-debug-stream*, string, args);
  end if;
end method;

define method demangle-const-reference(ref :: <constant-reference>)
  let name = ref.cr-refers-to;
  let pos1 = subsequence-position(name, "X");
  let pos2 = subsequence-position(name, "X", count: 2);

  vector(copy-sequence(name, start: pos2 + 1, end: name.size - 1),
         as(<symbol>, copy-sequence(name, start: pos1 + 1, end: pos2)),
         as(<symbol>, copy-sequence(name, end: pos1)));
  
end method demangle-const-reference;

define method demangle-var-reference(ref :: <constant-reference>)
  let name = ref.cr-refers-to;
  let pos1 = subsequence-position(name, "X");
  let pos2 = subsequence-position(name, "X", count: 2);

  vector(copy-sequence(name, start: pos2 + 1),
         as(<symbol>, copy-sequence(name, start: pos1 + 1, end: pos2)),
         as(<symbol>, copy-sequence(name, end: pos1)));
  
end method demangle-var-reference;

define method put-specially 
    (const-ref :: <constant-reference>,
     policy :: <idvm-code-dumping-policy>, dd :: <doss-dumper>)
    => (object-dumped? :: <boolean>)

  doss-message("constant-reference via lookup-mangled: %s\n", 
               const-ref.cr-refers-to);

  apply(put-apply, const-ref, dd, lookup-mangled, const-ref.demangle-const-reference);
  #t
end method;


define method put-specially 
    (const-ref :: <local-constant-reference>,
     policy :: <idvm-code-dumping-policy>, dd :: <doss-dumper>)
    => (object-dumped? :: <boolean>)

  doss-message("local-constant-reference: %s\n", const-ref.cr-refers-to);

  put-object(const-ref.constant-reference-value, dd);
  #t
end method;



define method put-specially 
    (const-ref :: <variable-reader>,
     policy :: <idvm-code-dumping-policy>, dd :: <doss-dumper>)
    => (object-dumped? :: <boolean>)

  doss-message("variable-reader via lookup-variable-reader: %s\n", const-ref.cr-refers-to);

  apply(put-apply, const-ref, dd, lookup-variable-reader, 
        const-ref.constant-reference-definer, const-ref.demangle-var-reference);
  #t
end method;


define method put-specially 
    (const-ref :: <variable-writer>,
     policy :: <idvm-code-dumping-policy>, dd :: <doss-dumper>)
    => (object-dumped? :: <boolean>)

  doss-message("variable-writer via lookup-variable-writer: %s\n", const-ref.cr-refers-to);

  apply(put-apply, const-ref, dd, lookup-variable-writer, 
        const-ref.constant-reference-definer, const-ref.demangle-var-reference);
  #t
end method;


define method put-specially 
    (definer :: <variable-definition>,
     policy :: <idvm-code-dumping-policy>, dd :: <doss-dumper>)
    => (object-dumped? :: <boolean>)

  doss-message("variable-definition via lookup-variable-definer: %s %s\n",
         definer.cr-refers-to, definer.constant-reference-value);

  apply(put-apply, definer, dd, lookup-variable-definer, definer.constant-reference-value, definer.demangle-var-reference);
  #t
end method;


define method put-specially 
    (definer :: <constant-definition>,
     policy :: <idvm-code-dumping-policy>, dd :: <doss-dumper>)
    => (object-dumped? :: <boolean>)

  doss-message("constant-definition via lookup-constant-definer: %s %s\n",
         definer.cr-refers-to, definer.constant-reference-value);

  apply(put-apply, definer, dd, lookup-constant-definer, definer.constant-reference-value, definer.demangle-const-reference);
  #t
end method;

define method put-specially 
    (const-ref :: <idvm-opcode>,
     policy :: <idvm-code-dumping-policy>, dd :: <doss-dumper>)
    => (object-dumped? :: <boolean>)

  put-variable(const-ref, dd,
               const-ref.opcode-name,
               #"IDVM",
               #"IDVM");
  #t
end method;


define method put-specially 
    (resolution-function-proxy :: <resolution-function-proxy>,
     policy :: <idvm-code-dumping-policy>, dd :: <doss-dumper>)
    => (object-dumped? :: <boolean>)

  put-variable(resolution-function-proxy, dd,
               resolution-function-proxy.function-name, #"IDVM", #"IDVM");
  #t
end method;


