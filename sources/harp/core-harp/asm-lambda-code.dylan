module:    main-harp
Synopsis:  Generation of <compiled-lambda> objects
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Support for generating <compiled-lambda> objects - which correspond
/// to the results of compiling a lambda - and may be stored for later
/// code emission without reference to the original back end.


define method assemble-compiled-lambda
    (backend :: <harp-back-end>, print-info, public?, export?, source-locator) 
    => (code :: <compiled-lambda>)

  let vars = backend.variables;
  let externals = as(<simple-object-vector>, vars.external-references);
  let referenced-data =
    compiled-lambda-referenced-data(backend, vars.referenced-data-words);
  let (code-size, labels-size, debug-info-size) = compiled-lambda-size(backend);
  let name = vars.function-name;
  let code = make(code-vector-class(backend), size: code-size); 
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


define open generic code-vector-class(backend :: <harp-back-end>)
 => (class :: subclass(<vector>));

define method code-vector-class(backend :: <harp-back-end>)
 => (class == <byte-vector>)
  <byte-vector>
end method;

define method compiled-lambda-debug-scopes
    (backend :: <harp-back-end>, debug-info :: <simple-object-vector>)
 => (scopes :: <debug-scopes>, all-scopes :: <simple-object-vector>,
     all-names :: <simple-object-vector>)
  if (debug-info.empty?)
    values(make-zero-debug-scopes(), #[], #[])
  else
    let limit = debug-info.size;
    iterate process-scopes (scopes = #(), index :: <integer> = 0, locator :: <labelled-constant> = debug-info[0])
      if (index < limit - 1)
        let next = index + 1;
        let next-locator :: <labelled-constant> = debug-info[next];
        let min :: <integer> = locator.labelled-constant-index;
        let max :: <integer> = next-locator.labelled-constant-index - 1;
        if (max < min) max := min end;
        let new-scope 
          = update-scope-from-debug-info(backend, scopes, locator, min, max);
        process-scopes(new-scope, next, next-locator);
      else
        if (instance?(locator, <code-locator-constant>))
          let pos = locator.labelled-constant-index;
          pack-debug-scopes(backend, update-scope-from-debug-info(backend, scopes, locator, pos, pos));
        else
          // don't bother to process a stack state marker if it's
          // the last piece of debug info - because the empty scope would 
          // be assumed anyway.
         pack-debug-scopes(backend, scopes);
        end if;
      end;
    end iterate;
  end if;
end method;


// SCL gets told about any named registers which are found to be live
// However, it is possible (but rare) that the live register won't have 
// even been coulourd due to elimination, if the SCL instruction
// is just after a MOVE. Filter that out here
define method coloured-subset
    (regs :: <simple-object-vector>)
     => (coloured-regs :: <simple-object-vector>)
  let uncoloured :: <integer> = 0;
  for (reg :: <virtual-register> in regs)
    unless (reg.virtual-register-colour)
      uncoloured := uncoloured + 1;
    end unless;
  end for;
  if (uncoloured == 0)
    regs;
  else
    let new = make(<simple-object-vector>, size: (regs.size - uncoloured));
    let index :: <integer> = 0;
    for (reg :: <virtual-register> in regs)
      if (reg.virtual-register-colour)
        new[index] := reg;
        index := index + 1;
      end if;
    end for;
    new;
  end if;
end method;


define method update-scope-from-debug-info
    (backend :: <harp-back-end>, scopes :: <list>, 
     locator :: <code-locator-constant>, 
     min-pos :: <integer>, max-pos :: <integer>)
    => (scopes :: <list>)
  let vars = locator.locator-live-variables.coloured-subset;
  let with-stack = locator.locator-with-stack?;
  // Tidy up the debug-info, now that we have calculated the debug scopes
  locator.locator-live-variables := #[];
  add-debug-scope(backend, scopes, with-stack, min-pos, max-pos, vars);
end method;


define method update-scope-from-debug-info
    (backend :: <harp-back-end>, scopes :: <list>, 
     locator :: <start-frame-marker>, 
     min-pos :: <integer>, max-pos :: <integer>)
    => (scopes :: <list>)
  add-debug-scope(backend, scopes, #t, min-pos, max-pos, #[]);
end method;


define method update-scope-from-debug-info
    (backend :: <harp-back-end>, scopes :: <list>, 
     locator :: <end-frame-marker>, 
     min-pos :: <integer>, max-pos :: <integer>)
    => (scopes :: <list>)
  add-debug-scope(backend, scopes, #f, min-pos, max-pos, #[]);
end method;


define method strip-locators-from-debug-info
    (backend :: <harp-back-end>, debug-info :: <simple-object-vector>, source-locator) 
    => (abs-location :: false-or(<absolute-source-position>),
        all-locators :: <simple-object-vector>,
        selected-locators)
  if (source-locator)
    let abs-location :: <absolute-source-position> =
      source-locator.locator-as-absolute-source-position;
    let all-locators :: <simple-object-vector>
      = choose-code-locators(abs-location, debug-info);
    /*
    format-out("\n### all-locators: %=\n",
	       reduce
		 (method (string :: <string>, loc)
		    concatenate(string, ",",
				integer-to-string(function-relative-line-number(loc)),
				".",
				integer-to-string(function-relative-code-position(loc)));
		  end,
		  "",
		  all-locators));
    */
    let selected-locators = choose-interesting-locators(backend, all-locators);
    values(abs-location, all-locators, selected-locators);
  else values(#f, #[], 0);
  end if;
end method;

define method choose-code-locators
    (abs-location :: <absolute-source-position>,
     debug-info :: <simple-object-vector>)
 => (code-locators :: <simple-object-vector>)
  for (i :: <integer> from debug-info.size - 1 to 0 by -1,
       result :: <list> = #() then
	 begin
	   let item = element-no-bounds-check(debug-info, i);
	   if (instance?(item, <code-locator-constant>))
	     let locator = item.locator-data;
	     let code-pos = item.labelled-constant-index;
	     pair(make-relative-source-position(abs-location, locator, code-pos),
		  result);
	   else result
	   end if
	 end)
  finally 
    as(<simple-object-vector>, result);
  end for;
end method;


// Choose those locators which are interesting enough to be selected
// for the source locators in a line-only debug format. We pick the 
// first one we encounter for a particular line (for now).
// Encode selected-locators as bitsets representing availability wrt all-locators

define method choose-interesting-locators 
    (backend :: <harp-back-end>, all-locators :: <simple-object-vector>)
    => (selected-locators)
  let found :: <list> = #();
  let bit-set :: <vector-32bit> =
    make(<vector-32bit>,
	 size: bit-set-offset(all-locators.size) + 1);
  for (loc :: <relative-source-position> in all-locators,
       position :: <integer> from 0)
    let line = loc.function-relative-line-number;
    unless (member?(line, found))
      found := pair(line, found);
      set-bit-in-set(bit-set, position);
    end unless;
  end for;
  pack-bitset(bit-set)
end method;


/// fill-compiled-lambda-code-and-labels fills in the code vector, labels
/// vector and debug-info vector for a compiled lambda.
/// This method is endian-ness independent. The mapping of bytes onto words
/// is deferred until the linker / outputter stage.


define method fill-compiled-lambda-code-and-labels
    (backend :: <harp-back-end>, 
     code :: <vector>, labels :: <simple-object-vector>, debug-info :: <simple-object-vector>)
    => ();
  fill-code-and-labels-from-code-sequence
    (backend, code, 0, labels, 0, debug-info, 0, backend.variables.code-vector);
end method;


define generic fill-code-and-labels-from-code-sequence
    (backend :: <harp-back-end>, 
     code :: <vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>,
     vec :: <sequence>)
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>);

/// For back ends with BYTE-ALIGNED code.

define method fill-code-and-labels-from-code-sequence
    (backend :: <harp-back-end>, 
     code :: <byte-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>,
     vec :: <sequence>)
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>);
  for (item in vec)
    if (instance?(item, <byte>))
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

/// For back ends with WORD-ALIGNED code, implement with short-alignment.
/// Use simple-integer-vectors (code-increment=2) to avoid integer overflows.

define method fill-code-and-labels-from-code-sequence
    (backend :: <harp-back-end>, 
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


/// fill-code-for-item fills the code, labels and debug-infovectors, given
/// a complex code item (integers are assumed to have been taken care of 
/// before invoking this function - for efficiency's sake).
/// Default methods are provided for all known types of complex code item for
/// byte and word aligned back ends.
/// For completeness, theres also an <integer> method - although integers should
/// have been handled at a higher level.


define generic fill-code-for-item
    (backend :: <harp-back-end>, item, 
     code :: <vector>, code-index :: <integer>,
     labels :: <vector>, labels-index :: <integer>,
     debug-info :: <vector>, debug-info-index :: <integer>) 
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>);



define method fill-code-for-item
    (backend :: <harp-back-end>, item :: <byte>, 
     code :: <byte-vector>, code-index :: <integer>,
     labels :: <simple-object-vector>, labels-index :: <integer>,
     debug-info :: <simple-object-vector>, debug-info-index :: <integer>) 
    => (new-code-index :: <integer>, 
        new-labels-index :: <integer>, 
        new-debug-info-index :: <integer>)
  code[code-index] := item;
  values(code-index + 1, labels-index, debug-info-index);
end method;


define method fill-code-for-item
    (backend :: <harp-back-end>, item :: <new-sdi>, 
     code :: <byte-vector>, code-index :: <integer>,
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
    (backend :: <harp-back-end>, item :: <labelled-constant>)
 => (increment :: <integer>)
  truncate/(item.labelled-constant-size, backend.code-item-increment)
end;

define method fill-code-for-item
    (backend :: <harp-back-end>, item :: <debug-info-constant>, 
     code :: <byte-vector>, code-index :: <integer>,
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
    (backend :: <harp-back-end>, item :: <relative-address-constant>, 
     code :: <byte-vector>, code-index :: <integer>,
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
    (backend :: <harp-back-end>, item :: <explicit-labelled-constant>, 
     code :: <byte-vector>, code-index :: <integer>,
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
    (backend :: <harp-back-end>, item :: <labelled-constant-with-opcode>, 
     code :: <byte-vector>, code-index :: <integer>,
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

define method fill-code-for-item
    (backend :: <harp-back-end>, item :: <integer>, 
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
    (backend :: <harp-back-end>, item :: <new-sdi>, 
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


define method fill-code-for-item
    (backend :: <harp-back-end>, item :: <debug-info-constant>, 
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
    (backend :: <harp-back-end>, item :: <relative-address-constant>, 
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
    (backend :: <harp-back-end>, item :: <explicit-labelled-constant>, 
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
    (backend :: <harp-back-end>, item :: <labelled-constant-with-opcode>, 
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


define generic fill-code-with-item
    (backend :: <harp-back-end>, val :: <integer>, 
     code :: <vector>, index :: <integer>) => ();

// Strictly for byte-aligned backends with constant increment 4.
// Add a new method otherwise.

define inline method fill-code-with-item
    (backend :: <harp-back-end>, val :: <integer>, 
     code :: <byte-vector>, index :: <integer>) => ()
  fill-code-with-word(backend, val, code, index);
end method;

// Strictly for short-aligned backends with constant increment 2.
// Add a new method otherwise.

define inline method fill-code-with-item
    (backend :: <harp-back-end>, val :: <integer>, 
     code :: <simple-integer-vector>, index :: <integer>) => ()
  code[index]     := val;
end method;

define generic fill-code-with-word
    (backend :: <harp-back-end>, val :: <abstract-integer>, 
     code :: <vector>, index :: <integer>) => ();

define method fill-code-with-word
    (backend :: <harp-back-end>, val :: <abstract-integer>, 
     code :: <byte-vector>, index :: <integer>) => ()
  let (b0, b1, b2, b3) = split-word-into-bytes(val, backend.big-endian?);
  code[index]     := b0;
  code[index + 1] := b1;
  code[index + 2] := b2;
  code[index + 3] := b3;
end method;

define method fill-code-with-word
    (backend :: <harp-back-end>, val :: <abstract-integer>, 
     code :: <simple-integer-vector>, index :: <integer>) => ()
  let (b0, b1) = split-word-into-halves(val, backend.big-endian?);
  code[index]     := b0;
  code[index + 1] := b1;
end method;


define generic fill-code-with-double-word
    (backend :: <harp-back-end>, 
     low :: <abstract-integer>, high :: <abstract-integer>, 
     code :: <vector>, index :: <integer>) => ();

define method fill-code-with-double-word
    (backend :: <harp-back-end>, 
     low :: <abstract-integer>, high :: <abstract-integer>, 
     code :: <byte-vector>, index :: <integer>) => ()
  if (backend.big-endian?)
    fill-code-with-word(backend, high, code, index);
    fill-code-with-word(backend, low, code, index + 4);
  else
    fill-code-with-word(backend, low, code, index);
    fill-code-with-word(backend, high, code, index + 4);
  end if;
end method;

define method fill-code-with-double-word
    (backend :: <harp-back-end>, 
     low :: <abstract-integer>, high :: <abstract-integer>, 
     code :: <simple-integer-vector>, index :: <integer>) => ()
  if (backend.big-endian?)
    fill-code-with-word(backend, high, code, index);
    fill-code-with-word(backend, low, code, index + 2);
  else
    fill-code-with-word(backend, low, code, index);
    fill-code-with-word(backend, high, code, index + 2);
  end if;
end method;


define generic split-word-into-bytes 
    (word :: <abstract-integer>, big-endian?)
    => (b0 :: <byte>, b1 :: <byte>, b2 :: <byte>, b3 :: <byte>);

define method split-word-into-bytes 
    (word :: <integer>, big-endian?)
    => (b0 :: <byte>, b1 :: <byte>, b2 :: <byte>, b3 :: <byte>)
  if (word.zero?)
    values(0, 0, 0, 0)
  else
    let b0 :: <byte> = logand(word, #xff);
    let b1 :: <byte> = logand(ash(word, -8), #xff);
    let b2 :: <byte> = logand(ash(word, -16), #xff);
    let b3 :: <byte> = logand(ash(word, -24), #xff);
    if (big-endian?)
      values(b3, b2, b1, b0);
    else
      values(b0, b1, b2, b3);
    end if;
  end if;
end method;

define method split-word-into-bytes 
    (word :: <abstract-integer>, big-endian?)
    => (b0 :: <byte>, b1 :: <byte>, b2 :: <byte>, b3 :: <byte>)
  if (word.zero?)
    values(0, 0, 0, 0)
  else
    let b0 :: <byte> = generic-logand(word, #xff);
    let b1 :: <byte> = generic-logand(generic-ash(word, -8), #xff);
    let b2 :: <byte> = generic-logand(generic-ash(word, -16), #xff);
    let b3 :: <byte> = generic-logand(generic-ash(word, -24), #xff);
    if (big-endian?)
      values(b3, b2, b1, b0);
    else
      values(b0, b1, b2, b3);
    end if;
  end if;
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



/// compiled-lambda-size determines how much code there is for a lambda, 
/// and also how many labels are associated with the lambda.

define open generic compiled-lambda-size (backend :: <harp-back-end>) 
    => (code-size :: <integer>, labels-size :: <integer>, debug-info-size :: <integer>);


/// The default method for compiled-lambda-size calculates the size in hardcoded
/// increments of 1. This suffices both for true byte-increment backends like the
/// pentium (bytes in byte-vectors) and also for split-instruction backends like
/// the PowerPC (shorts in simple-integer-vectors). Add a new method only if there
/// is a discrepancy between code generation granularity and code-vector medium.

define method compiled-lambda-size (backend :: <harp-back-end>) 
    => (code-size :: <integer>, labels-size :: <integer>, debug-info-size :: <integer>)
  code-vector-code-and-label-size(backend, backend.variables.code-vector);
end method;


define method code-vector-code-and-label-size 
    (backend :: <harp-back-end>, vec :: <sequence>) 
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


/// code-item-code-and-label-size is an extensible protocol for determining the
/// size of code items. There are default methods for SDIs and <labelled-constant>s.
/// For completeness, theres also an <integer> method - although integers should
/// have been handled at a higher level.

define open generic code-item-code-and-label-size 
    (backend :: <harp-back-end>, item) 
    => (code-size :: <integer>, labels-size :: <integer>);



define method code-item-code-and-label-size 
    (backend :: <harp-back-end>, item :: <integer>)
    =>  (code-size :: <integer>, labels-size :: <integer>)
  values(1, 0);
end method;


define method code-item-code-and-label-size 
    (backend :: <harp-back-end>, item :: <new-sdi>)
    =>  (code-size :: <integer>, labels-size :: <integer>)
  code-vector-code-and-label-size(backend, item.new-sdi-code-holder);
end method;


define method code-item-code-and-label-size 
    (backend :: <harp-back-end>, item :: <labelled-constant>)
    =>  (code-size :: <integer>, labels-size :: <integer>)
  values(labelled-constant-code-increment(backend, item), 1);
end method;



define open generic compiled-lambda-referenced-data
      (backend :: <harp-back-end>, data :: <list>) => (res :: false-or(<vector>));

define macro compiled-lambda-referenced-data-method-definer
  { define compiled-lambda-referenced-data-method
      (?backend-class:name, ?vector-class:name, ?word-size:expression, ?double-size:expression) }
    =>
  { define method compiled-lambda-referenced-data
      (backend :: ?backend-class, data :: <list>) => (res :: false-or(?vector-class))
      let len :: <integer> = data.size;
      if (len.zero?)
        #f; // No referenced data
      else
        let datavec = make(?vector-class, size: (len * ?word-size));
        iterate process-data (index :: <integer> = 0, todo :: <list> = data)
          unless (todo.empty?)
            let word = todo.head;
            if (word)
              // It's a simple single-worder
              fill-code-with-word(backend, word, datavec, index);
              process-data(index + ?word-size, todo.tail);
            else
              // It's a double-worder with the double word in the next pair
              let next-todo :: <list> = todo.tail;
              let data-pair = next-todo.head;
              let low = data-pair.head;
              let high = data-pair.tail;
              fill-code-with-double-word(backend, low, high, datavec, index);
              process-data(index + ?double-size, next-todo.tail);
            end if;
          end unless;
        end iterate;
        datavec;
      end if;
    end method }
end macro;

define compiled-lambda-referenced-data-method
  (<harp-back-end>, <byte-vector>, 4, 8);


// PRINT-LINEARISED-HARP
//

define open generic print-linearised-harp
     (backend :: <harp-back-end>, 
      stream :: <stream>, 
      blk-vector :: <simple-basic-block-vector>, 
      blk-num :: <integer>) => ();
