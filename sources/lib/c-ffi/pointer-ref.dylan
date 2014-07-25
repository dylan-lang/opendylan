Module:    c-ffi-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro pointer-ref-method-definer
  { define pointer-ref-method ?function-name:name
                              ?pointer-type:name ?raw-type:name
                              ?boxed-class:name ?getter:name ?boxer:name
                              ?unboxer:name
                              end }
  =>
  { define inline-only function ?function-name
           (c-pointer :: <C-pointer>,
            #key byte-index :: <integer> = 0,
                 scaled-index :: <integer> = 0)
        => (value :: ?boxed-class)
         ?boxer(?getter(primitive-unwrap-c-pointer(c-pointer),
                        integer-as-raw(scaled-index),
                        integer-as-raw(byte-index)))
       end function ?function-name;
       define inline-only function ?function-name ## "-setter"
           (new-value :: ?boxed-class,
            c-pointer :: <C-pointer>,
            #key byte-index :: <integer> = 0,
                 scaled-index :: <integer> = 0)
        => (res :: ?boxed-class)
         ?getter ## "-setter"
           (?unboxer(new-value),
            primitive-unwrap-c-pointer(c-pointer),
            integer-as-raw(scaled-index),
            integer-as-raw(byte-index));
         new-value
       end function ?function-name ## "-setter";
     }
end macro;

define pointer-ref-method C-signed-char-at
  <C-raw-signed-char*> raw-c-signed-char
  <machine-word> primitive-c-signed-char-at
  primitive-wrap-machine-word primitive-unwrap-machine-word
  end;

define pointer-ref-method C-unsigned-char-at
  <C-raw-unsigned-char*> raw-c-unsigned-char <machine-word>
  primitive-c-unsigned-char-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method C-signed-short-at
  <C-raw-signed-short*> raw-c-signed-short <machine-word>
  primitive-c-signed-short-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method
  C-unsigned-short-at
  <C-raw-unsigned-short*> raw-c-unsigned-short <machine-word>
  primitive-c-unsigned-short-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method C-signed-long-at
  <C-raw-signed-long*> raw-c-signed-long <machine-word>
  primitive-c-signed-long-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method C-unsigned-long-at
  <C-raw-unsigned-long*> raw-c-unsigned-long <machine-word>
  primitive-c-unsigned-long-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method C-size-t-at
  <C-raw-size-t*> raw-c-size-t <machine-word>
  primitive-c-size-t-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method C-ssize-t-at
  <C-raw-ssize-t*> raw-c-ssize-t <machine-word>
  primitive-c-ssize-t-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method C-signed-int-at
  <C-raw-signed-int*> raw-c-signed-int <machine-word>
  primitive-c-signed-int-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method C-unsigned-int-at
  <C-raw-unsigned-int*> raw-c-unsigned-int <machine-word>
  primitive-c-unsigned-int-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define constant c-long-at = c-signed-long-at;
define constant c-long-at-setter = c-signed-long-at-setter;

define constant c-short-at = c-signed-short-at;
define constant c-short-at-setter = c-signed-short-at-setter;

define constant c-char-at = c-signed-char-at;
define constant c-char-at-setter = c-signed-char-at-setter;

define constant c-int-at = c-signed-int-at;
define constant c-int-at-setter = c-signed-int-at-setter;

/*
define pointer-ref-method C-signed-long-long-at
  <C-raw-signed-long-long*> raw-c-signed-long-long <machine-word> primitive-c-signed-long-long-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;

define pointer-ref-method C-unsigned-long-long-at
  <C-raw-unsigned-long-long*> raw-c-unsigned-long-long <machine-word>
  primitive-c-unsigned-long-long-at
  primitive-wrap-machine-word
  primitive-unwrap-machine-word
  end;
*/


define pointer-ref-method C-float-at
  <C-float*> raw-single-float <single-float>
  primitive-c-float-at
  primitive-raw-as-single-float
  primitive-single-float-as-raw
end;

define pointer-ref-method C-double-at
  <C-double*> raw-double-float <double-float>
  primitive-c-double-at
  primitive-raw-as-double-float
  primitive-double-float-as-raw
  end;

/*
define pointer-ref-method C-long-double-at
  <C-long-double*> raw-extended-float <extended-float>
  primitive-c-long-double-at
  primitive-raw-as-extended-float
  primitive-extended-float-as-raw
  end;
*/


define inline function C-pointer-at (class :: subclass(<C-pointer>),
                                     pointer :: <C-pointer>,
                                     #key byte-index :: <integer> = 0,
                                          scaled-index :: <integer> = 0)
 => (value :: <C-pointer>);
  make(class,
       address: primitive-wrap-machine-word
                  (primitive-cast-pointer-as-raw
                    (primitive-c-pointer-at
                      (primitive-unwrap-c-pointer(pointer),
                       integer-as-raw(scaled-index),
                       integer-as-raw(byte-index)))))
end function;

define inline function C-pointer-at-setter (new-value :: <C-pointer>,
                                            pointer :: <C-pointer>,
                                            #key byte-index :: <integer> = 0,
                                                 scaled-index :: <integer> = 0)
 => (new-value :: <C-pointer>);
  primitive-c-pointer-at(primitive-unwrap-c-pointer(pointer),
                         integer-as-raw(scaled-index),
                         integer-as-raw(byte-index))
    := primitive-unwrap-c-pointer(new-value);
  new-value
end function;


/*
// might be useful someday

define function pointer-add (result-class :: <designator-class>,
                             object :: <c-pointer>,
                             byte-offset :: <integer>)
 => (new-pointer :: <c-pointer>);
  //
  make(result-class, address: pointer-address(object) + byte-offset);
end;
*/
