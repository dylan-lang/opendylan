module:    powerpc-harp
Synopsis:  PowerPC Generation of <compiled-lambda> objects
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method assemble-compiled-lambda
    (backend :: <powerpc-back-end>, print-info, public?, export?, source-locator) 
    => (code :: <compiled-lambda>)

  let vars = backend.variables;
  let externals = as(<simple-object-vector>, vars.external-references);
  let referenced-data =
    compiled-lambda-referenced-data(backend, vars.referenced-data-words);
  let (code-size, labels-size, debug-info-size) = compiled-lambda-size(backend);
  let name = vars.function-name;
  let code = make(<simple-integer-vector>, size: code-size); 
  let labels = make(<simple-object-vector>, size: labels-size);
  let debug-info = make(<simple-object-vector>, size: debug-info-size);
  // debug-info is a vector of any <debug-info-constant>s
  // NB it doesn't go directly into the compiled lambda like this
  fill-compiled-lambda-code-and-labels(backend, code, labels, debug-info);
  let (debug-scopes :: <debug-scopes>,
       all-debug-scopes :: <simple-object-vector>,
       all-debug-names :: <simple-object-vector>) =
    compiled-lambda-debug-scopes(backend, debug-info);
  let (location, all-locators, selected-locators)
    = strip-locators-from-debug-info(backend, debug-info, source-locator);
  let state = vars.vreg-state;
  make(<compiled-lambda>, code: code, labels: labels, 
       public: public?, export: export?,
       scopes: debug-scopes, all-scopes: all-debug-scopes, all-names: all-debug-names,
       location: location, object-location: source-locator,
       referenced-data: referenced-data,
       all-locators: all-locators, selected-locators: selected-locators,
       externals: externals, name: name, model: name, print-info: print-info,
       frame-g-size: state.next-gc-spill, frame-n-size: state.raw-size);
end method;


define method fill-compiled-lambda-code-and-labels
    (backend :: <powerpc-back-end>, 
     code :: <simple-integer-vector>, labels :: <simple-object-vector>,
     debug-info :: <simple-object-vector>)
    => ();
  fill-code-and-labels-from-code-sequence
    (backend, code, 0, labels, 0, debug-info, 0, backend.variables.code-vector);
end method;


define method fill-code-and-labels-from-code-sequence
    (backend :: <powerpc-back-end>, 
     code :: <simple-integer-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>,
     vec :: <sequence>)
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>);
  for (item in vec)
    if (instance?(item, <integer>))
      // Separate out integers within the loop, because there are expected
      // to be a lot of them, and this will probably be an efficiency win
      code[code-index] := item;
      inc!(code-index, 1);
    else
      let (new-c, new-l, new-o) 
	= fill-code-for-item(backend, item, 
			     code, code-index, 
			     labels, labels-index, 
			     debug-info, debug-info-index);
      code-index := new-c;
      labels-index := new-l;
      debug-info-index := new-o;
    end if;
  end for;
  values(code-index, labels-index, debug-info-index);
end method;

define method fill-code-for-item
    (backend :: <powerpc-back-end>, item :: <integer>, 
     code :: <simple-integer-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>) 
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>)
  code[code-index] := item;
  values(code-index + 1, labels-index, debug-info-index);
end method;


define method fill-code-for-item
    (backend :: <powerpc-back-end>, item :: <new-sdi>, 
     code :: <simple-integer-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>) 
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>)
  fill-code-and-labels-from-code-sequence
    (backend, code, code-index, labels, labels-index, debug-info, debug-info-index,
     item.new-sdi-code-holder)
end method;


define inline method labelled-constant-code-increment
    (backend :: <powerpc-back-end>, item :: <labelled-constant>)
 => (increment :: <integer>)
  truncate/(item.labelled-constant-size, backend.code-item-increment)
end;

define method fill-code-for-item
    (backend :: <powerpc-back-end>, item :: <debug-info-constant>, 
     code :: <simple-integer-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>) 
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>)
  // set this to help the outputters
  item.labelled-constant-index := code-index;
  debug-info[debug-info-index] := item;
  values(code-index + labelled-constant-code-increment(backend, item),
	 labels-index, debug-info-index + 1)
end method;


define method fill-code-for-item
    (backend :: <powerpc-back-end>, item :: <relative-address-constant>, 
     code :: <simple-integer-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>) 
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>)
  // Fill in the code with the offset from start of lambda 
  // to permit block blatting in the linker.
  fill-code-with-item
    (backend,
     code-index * backend.code-item-increment + item.relative-offset,
     code,
     code-index);
  item.labelled-constant-index := code-index;  // set this to help the outputters
  labels[labels-index] := item;
  values(code-index + labelled-constant-code-increment(backend, item),
	 labels-index + 1, debug-info-index)
end method;


define method fill-code-for-item
    (backend :: <powerpc-back-end>, item :: <explicit-labelled-constant>, 
     code :: <simple-integer-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>) 
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>)
  // Fill in the code with the offset to permit block blatting in the linker.
  fill-code-with-item
    (backend, item.labelled-constant-reference.cr-const-offset, code, code-index);
  item.labelled-constant-index := code-index;  // set this to help the outputters
  labels[labels-index] := item;
  values(code-index + labelled-constant-code-increment(backend, item),
	 labels-index + 1, debug-info-index)
end method;


define method fill-code-for-item
    (backend :: <powerpc-back-end>, item :: <labelled-constant-with-opcode>, 
     code :: <simple-integer-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>) 
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>)
  // Fill in the code with the offset to permit block blatting in the linker.
  code[code-index] := item.opcode-value;
  fill-code-with-item
    (backend, item.labelled-constant-reference.cr-const-offset, code, code-index + 1);
  item.labelled-constant-index := code-index;  // set this to help the outputters
  labels[labels-index] := item;
  values(code-index + labelled-constant-code-increment(backend, item),
	 labels-index + 1, debug-info-index)
end method;


define method fill-code-with-item
    (backend :: <powerpc-back-end>, val :: <integer>, 
     code :: <simple-integer-vector>, index :: <integer>)
  code[index]     := val;
end method;

define method fill-code-with-word
    (backend :: <powerpc-back-end>, val :: <abstract-integer>, 
     code :: <simple-integer-vector>, index :: <integer>)
  // This default method assumes LittleEndian machines
  let (b0, b1) = split-word-into-halves(val, #f);
  code[index]     := b0;
  code[index + 1] := b1;
end method;

define method fill-code-with-double-word
    (backend :: <powerpc-back-end>, 
     low :: <abstract-integer>, high :: <abstract-integer>, 
     code :: <simple-integer-vector>, index :: <integer>)
  // This default method assumes LittleEndian machines
  fill-code-with-word(backend, low, code, index);
  fill-code-with-word(backend, high, code, index + 2);
end method;

define generic split-word-into-halves 
    (word :: <abstract-integer>, big-endian?)
    => (b0 :: <integer>, b1 :: <integer>);

define method split-word-into-halves 
    (word :: <integer>, big-endian?)
    => (b0 :: <integer>, b1 :: <integer>)
  if (word.zero?)
    values(0, 0)
  else
    let b0 :: <integer> = logand(word, #xffff);
    let b1 :: <integer> = logand(ash(word, -16), #xffff);
    if (big-endian?)
      values(b1, b0);
    else
      values(b0, b1);
    end if;
  end if;
end method;

define method split-word-into-halves 
    (word :: <abstract-integer>, big-endian?)
    => (b0 :: <integer>, b1 :: <integer>)
  if (word.zero?)
    values(0, 0)
  else
    let b0 :: <integer> = generic-logand(word, #xffff);
    let b1 :: <integer> = generic-logand(generic-ash(word, -16), #xffff);
    if (big-endian?)
      values(b1, b0);
    else
      values(b0, b1);
    end if;
  end if;
end method;


define method compiled-lambda-referenced-data
    (backend :: <powerpc-back-end>, data :: <list>)
 => (res :: false-or(<simple-integer-vector>))
  let len :: <integer> = data.size;
  if (len.zero?)
    #f; // No referenced data
  else
    let datavec = make(<simple-integer-vector>, size: (len * 2));
    iterate process-data (index :: <integer> = 0, todo :: <list> = data)
      unless (todo.empty?)
        let word = todo.head;
        if (word)
          // It's a simple single-worder
          fill-code-with-word(backend, word, datavec, index);
          process-data(index + 2, todo.tail);
        else
          // It's a double-worder with the double word in the next pair
          let next-todo :: <list> = todo.tail;
          let data-pair = next-todo.head;
          let low = data-pair.head;
          let high = data-pair.tail;
          fill-code-with-double-word(backend, low, high, datavec, index);
          process-data(index + 4, next-todo.tail);
        end if;
      end unless;
    end iterate;
    datavec;
  end if;
end method;


define method compiled-lambda-size (backend :: <powerpc-back-end>) 
    => (code-size :: <integer>, labels-size :: <integer>, debug-info-size :: <integer>)
  code-vector-code-and-label-size(backend, backend.variables.code-vector);
end method;


define method code-vector-code-and-label-size 
    (backend :: <powerpc-back-end>, vec :: <sequence>) 
    => (code-size :: <integer>, labels-size :: <integer>, debug-info-size :: <integer>)
  let code-size :: <integer> = 0;
  let labels-size :: <integer> = 0;
  let debug-info-size :: <integer> = 0;
  let code-inc = 1;
  for (item in vec)
    if (instance?(item, <integer>))
      // Separate out integers within the loop, because there are expected
      // to be a lot of them, and this will probably be an efficiency win
        inc!(code-size, code-inc);
    else
      let (c-inc, l-inc) = code-item-code-and-label-size(backend, item);
      inc!(code-size, c-inc);
      if (l-inc > 0)
	if (instance?(item, <debug-info-constant>))
	  inc!(debug-info-size, l-inc);
	else
	  inc!(labels-size, l-inc);
	end if;
      end if;
    end if;
  end for;
  values(code-size, labels-size, debug-info-size);
end method;



define method code-item-code-and-label-size 
    (backend :: <powerpc-back-end>, item :: <integer>)
    =>  (code-size :: <integer>, labels-size :: <integer>)
  values(1, 0);
end method;


define method code-item-code-and-label-size 
    (backend :: <powerpc-back-end>, item :: <new-sdi>)
    =>  (code-size :: <integer>, labels-size :: <integer>)
  code-vector-code-and-label-size(backend, item.new-sdi-code-holder);
end method;


define method code-item-code-and-label-size 
    (backend :: <powerpc-back-end>, item :: <labelled-constant>)
    =>  (code-size :: <integer>, labels-size :: <integer>)
  values(labelled-constant-code-increment(backend, item), 1);
end method;
