module: dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

/// definitions for the basic designator classes in the ffi

// still need implementations of the <machine-word> versions,
// <integer>, and <abstract-integer> versions.  I suspect we can just
// have the <machine-word versions here, and the others defined by a
// normal library.


define abstract &class <C-value> (<object>)
  metaclass <designator-class>;
end &class;

define abstract &class <C-void> (<C-value>)
  metaclass <designator-class>;
end &class;

define open abstract &class <C-struct> (<C-value>)
  metaclass <designator-class>;
end &class;

define open abstract &class <C-union> (<C-value>)
  metaclass <designator-class>;
end &class;

// We have a split between structs and non-structs
define abstract &class <C-non-struct> (<C-value>)
  metaclass <designator-class>;
end;


define abstract open &class <C-pointer> (<C-non-struct>)
  metaclass <designator-class>,
    raw-type-name: #"<raw-c-pointer>",
    boxer-function-name: #"primitive-wrap-c-pointer",
    unboxer-function-name: #"primitive-unwrap-c-pointer",
    raw-dereferencer: #"primitive-c-pointer-at",
    low-level-type: <C-pointer>,
    self: <C-pointer>;
  runtime-constant &slot raw-pointer-address :: <raw-pointer>,
    required-init-keyword: raw-pointer-address:;
end &class;

define abstract &class <C-untyped-pointer> (<C-pointer>)
  metaclass <designator-class>,
    low-level-type: <C-untyped-pointer>,
    self: <C-untyped-pointer>;
end &class;

// is this necessary??
define abstract open &class <C-void*> (<C-pointer>)
  metaclass <designator-class>,
   referenced-type: <C-void>,
   low-level-type: <C-void*>,
   self: <C-void*>;
end &class;

define concrete &class <instantiation-of-c-void*> (<C-void*>)
  metaclass <c-automatic-pointer-designator-class>,
    low-level-type: <instantiation-of-c-void*>,
    self: <instantiation-of-c-void*>,
    abstract-super: <C-void*>;
end &class;

define abstract open &class <C-statically-typed-pointer>
    (<C-void*>, <mutable-object-with-elements>)
  metaclass <designator-class>,
    low-level-type: <C-statically-typed-pointer>,
    self: <C-statically-typed-pointer>;
end &class;

define abstract open &class <C-pointer-to-pointer> (<C-statically-typed-pointer>)
  metaclass <designator-class>,
    low-level-type: <C-pointer-to-pointer>,
    self: <C-pointer-to-pointer>;
end &class;


define open abstract &class <C-function-pointer> (<C-statically-typed-pointer>)
  metaclass <designator-class>,
    low-level-type: <C-function-pointer>,
    self: <C-function-pointer>;
end &class;

define concrete &class <C-function-pointer-instantiation>
    (<C-function-pointer>)
  metaclass <c-automatic-pointer-designator-class>,
    low-level-type: <C-function-pointer-instantiation>,
    self: <C-function-pointer-instantiation>,
    abstract-super: <C-function-pointer>;
end &class;

// need to use a new metaclass here to hold the static type
define abstract open &class <C-statically-typed-function-pointer> (<C-function-pointer>)
  metaclass <designator-class>,
    low-level-type: <C-statically-typed-function-pointer>,
    self: <C-statically-typed-function-pointer>;
end &class;


define abstract &class <C-number> (<C-non-struct>)
  metaclass <designator-class>;
end &class;

define abstract &class <C-integer> (<C-number>)
  metaclass <designator-class>, low-level-type: <machine-word>;
end &class;

define abstract &class <C-abstract-float> (<C-number>)
  metaclass <designator-class>;
end;

define macro integer-designator-class-definer
  { define integer-designator-class "<C-raw-" ## ?designator-class-name:name ## ">"
                                    "<" ## ?super-class-name:name ## ">"
                                    ?bitfield-dereferencer:name
                                    ?boxer-name:name
                                    end }
  => { define integer-designator-class-aux "<C-raw-" ## ?designator-class-name ## ">"
                                           "<C-raw-" ## ?designator-class-name ## "*>"
                                           "<" ## ?super-class-name ## ">"
                                           "<raw-c-" ## ?designator-class-name ## ">"
                                           "primitive-c-" ## ?designator-class-name ## "-at"
                                           ?bitfield-dereferencer ?boxer-name
                                           end }
  { define integer-designator-class "<C-raw-" ## ?designator-class-name:name ## ">"
                                    "<" ## ?super-class-name:name ## ">"
                                    ?bitfield-dereferencer:name
                                    ?boxer-name:name
                                    ?raw-type:name
                                    end }
  => { define integer-designator-class-aux "<C-raw-" ## ?designator-class-name ## ">"
                                           "<C-raw-" ## ?designator-class-name ## "*>"
                                           "<" ## ?super-class-name ## ">"
                                           ?raw-type
                                           "primitive-c-" ## ?designator-class-name ## "-at"
                                           ?bitfield-dereferencer ?boxer-name
                                           end }
options:
    { } => { }
 { #rest ?keys:* /* , #key, #all-keys */ }
    =>
 { ?keys }
bitfield-dereferencer:
    { signed } => { primitive-c-signed-field }
    { unsigned } => { primitive-c-unsigned-field }
    { int } => { primitive-c-int-field }
end macro;

define macro integer-designator-class-aux-definer
  { define integer-designator-class-aux ?model-class-name:name
                                        "<" ## ?pointer-class-name:name ## ">"
                                        ?superclass-name:name
                                        ?raw-type:name
                                        ?raw-dereferencer:name
                                        ?bitfield-dereferencer:name
                                        ?boxer-name:name
                                        end }
  => { define open abstract &class ?model-class-name (?superclass-name)
         metaclass <designator-class>, raw-type-name: ?#"raw-type",
           raw-dereferencer: ?#"raw-dereferencer",
           bitfield-dereferencer: ?#"bitfield-dereferencer",
           boxer-function-name: ?#"boxer-name",
           unboxer-function-name: #"primitive-unwrap-machine-word",
           pointer-type-name: "<" ## ?pointer-class-name ## ">";
       end;
       define open abstract &class "<" ## ?pointer-class-name ## ">"
           (<C-statically-typed-pointer>)
         metaclass <c-automatic-pointer-designator-class>,
           referenced-type: ?model-class-name,
           low-level-type: "<" ## ?pointer-class-name ## ">",
           self: "<" ## ?pointer-class-name ## ">",
           concrete-class-name:
             "<instantiation-of-" ## ?pointer-class-name ## ">",
           pointer-type-name: "<" ## ?pointer-class-name ## "*>";
       end;
       define sealed concrete &class
             "<instantiation-of-" ## ?pointer-class-name ## ">"
           ("<" ## ?pointer-class-name ## ">")
         metaclass <c-automatic-pointer-designator-class>,
           abstract-super: "<" ## ?pointer-class-name ## ">",
           low-level-type: "<instantiation-of-" ## ?pointer-class-name ## ">",
           self: "<instantiation-of-" ## ?pointer-class-name ## ">";
       end;
       define open abstract &class "<" ## ?pointer-class-name ## "*>"
           (<C-pointer-to-pointer>)
         metaclass <c-automatic-pointer-designator-class>,
           referenced-type: "<" ## ?pointer-class-name ## ">",
           low-level-type: "<" ## ?pointer-class-name ## "*>",
           self: "<" ## ?pointer-class-name ## "*>",
           concrete-class-name:
             "<instantiation-of-" ## ?pointer-class-name ## "*>";
       end;
       define sealed concrete &class
             "<instantiation-of-" ## ?pointer-class-name ## "*>"
           ("<" ## ?pointer-class-name ## "*>")
         metaclass <c-automatic-pointer-designator-class>,
           abstract-super: "<" ## ?pointer-class-name ## "*>",
           low-level-type:
             "<instantiation-of-" ## ?pointer-class-name ## "*>",
           self: "<instantiation-of-" ## ?pointer-class-name ## "*>";
       end
     }
end macro;

define integer-designator-class <C-raw-unsigned-char> <C-integer> unsigned box-c-unsigned-char end;
define integer-designator-class <C-raw-signed-char> <C-integer> signed box-c-signed-char end;
define integer-designator-class <C-raw-unsigned-short> <C-integer> unsigned box-c-unsigned-short end;
define integer-designator-class <C-raw-signed-short> <C-integer> signed box-c-signed-short end;
define integer-designator-class <C-raw-unsigned-long> <C-integer> unsigned primitive-wrap-machine-word end;
define integer-designator-class <C-raw-signed-long> <C-integer> signed primitive-wrap-machine-word end;
define integer-designator-class <C-raw-unsigned-int> <C-integer> unsigned primitive-wrap-machine-word end;
define integer-designator-class <C-raw-signed-int> <C-integer> signed primitive-wrap-machine-word end;
define integer-designator-class <C-raw-int> <C-integer> int primitive-wrap-machine-word <raw-c-unsigned-int> end;

define macro float-designator-class-definer
  { define float-designator-class "<C-" ## ?C-class:name ## ">"
                                  "<" ## ?dylan-class:name ## ">"
                                  end }
  => { define float-designator-class-aux "<C-" ## ?C-class ## ">"
                                         "<C-" ## ?C-class ## "*>"
                                         "<instantiation-of-c-" ## ?C-class ## "*>"
                                         "<raw-c-" ## ?C-class ## ">"
                                                "<" ## ?dylan-class ## ">"
                                         "primitive-c-" ## ?C-class ## "-at"
                                         "primitive-raw-as-" ## ?dylan-class
                                                "primitive-" ## ?dylan-class ## "-as-raw"
       end }
end macro float-designator-class-definer;

define macro float-designator-class-aux-definer
  { define float-designator-class-aux ?C-class:name
                                      ?C-pointer-class:name
                                      ?C-pointer-instantiation-class:name
                                      ?raw-type:name
                                      ?dylan-class:name
                                      ?dereferencer:name
                                      ?boxer:name
                                      ?unboxer:name
                                      end }
  => { define open abstract &class ?C-class (<C-abstract-float>)
         metaclass <designator-class>,
           raw-type-name: ?#"raw-type",
           raw-dereferencer: ?#"dereferencer",
           boxer-function-name: ?#"boxer",
           unboxer-function-name: ?#"unboxer",
           low-level-type: ?dylan-class;
       end;
       define open abstract &class ?C-pointer-class (<C-statically-typed-pointer>)
         metaclass <c-automatic-pointer-designator-class>,
           referenced-type: ?C-class,
           low-level-type: ?C-pointer-class,
           self: ?C-pointer-class;
       end;
       define sealed concrete &class ?C-pointer-instantiation-class (?C-pointer-class)
         metaclass <c-automatic-pointer-designator-class>,
           abstract-super: ?C-pointer-class,
           low-level-type: ?C-pointer-instantiation-class,
           self: ?C-pointer-instantiation-class;
       end }
end macro float-designator-class-aux-definer;

define float-designator-class <C-float> <single-float> end;
define float-designator-class <C-double> <double-float> end;

///---*** NOTE: Since we don't implement <extended-float>, it would be difficult
///---*** to provide <C-long-double> as we don't have the needed primitives
/// define float-designator-class <C-long-double> <extended-float> end;
