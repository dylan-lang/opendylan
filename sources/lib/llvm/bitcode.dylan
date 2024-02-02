Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2018 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// See http://llvm.org/docs/BitCodeFormat.html

// Built-in abbrev IDs
define constant $ABBREV-END-BLOCK :: <integer> = 0;
define constant $ABBREV-ENTER-SUBBLOCK :: <integer> = 1;
define constant $ABBREV-DEFINE-ABBREV  :: <integer> = 2;
define constant $ABBREV-UNABBREV-RECORD :: <integer> = 3;

define constant $ABBREV-APPLICATION-BASE :: <integer> = 4;

define class <bitcode-stream> (<wrapper-stream>)
  slot bitcode-accumulator :: <machine-word> = as(<machine-word>, 0);
  slot bitcode-accumulator-size :: <integer> = 0;
  slot bitcode-abbrev-bits :: <integer> = 2;
  slot bitcode-abbrev-definitions :: <object-table> = make(<object-table>);
  constant slot bitcode-blockinfo :: <object-table> = make(<object-table>);
  slot bitcode-records :: <object-table> = make(<object-table>);
end class;

define method bitcode-flush
    (stream :: <bitcode-stream>)
 => ();
  let accumulator = stream.bitcode-accumulator;
  let byte-mask = coerce-integer-to-machine-word(#xFF);
  write-4-aligned-bytes
    (inner-stream(stream),
     coerce-machine-word-to-integer
       (machine-word-logand(accumulator, byte-mask)),
     coerce-machine-word-to-integer
       (machine-word-logand(machine-word-shift-right(accumulator, 8),
                            byte-mask)),
     coerce-machine-word-to-integer
       (machine-word-logand(machine-word-shift-right(accumulator, 16),
                            byte-mask)),
     coerce-machine-word-to-integer
       (machine-word-logand(machine-word-shift-right(accumulator, 24),
                            byte-mask)));
end method;

define inline method write-fixed
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <machine-word>)
 => ();
  let accumulator-size = stream.bitcode-accumulator-size;
  stream.bitcode-accumulator
    := machine-word-logior
         (stream.bitcode-accumulator,
          machine-word-unsigned-shift-left(value, accumulator-size));
  let slack = 32 - accumulator-size;
  stream.bitcode-accumulator-size := accumulator-size + bits;
  if (stream.bitcode-accumulator-size >= 32)
    bitcode-flush(stream);
    stream.bitcode-accumulator-size := stream.bitcode-accumulator-size - 32;
    if (slack < 32)
      stream.bitcode-accumulator
        := machine-word-unsigned-shift-right(value, slack);
    else
      stream.bitcode-accumulator := coerce-integer-to-machine-word(0);
    end if;
  end if;
end method;

define method write-fixed
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <integer>)
 => ();
  write-fixed(stream, bits, as(<machine-word>, value));
end method;

define method write-fixed
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <character>)
 => ();
  write-fixed(stream, bits, as(<machine-word>, as(<integer>, value)));
end method;

define method write-abbrev-id
    (stream :: <bitcode-stream>, id :: <integer>)
 => ();
  write-fixed(stream, stream.bitcode-abbrev-bits, id);
end method;

define inline method write-vbr
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <machine-word>)
 => ();
  let limit = u%shift-left(1, bits - 1);
  let mask = \%-(limit, 1);
  iterate loop (value :: <machine-word> = value)
    if (u%<(value, limit))
      write-fixed(stream, bits, value);
    else
      write-fixed(stream, bits,
                  machine-word-logior(limit,
                                      machine-word-logand(value, mask)));
      loop(machine-word-unsigned-shift-right(value, bits - 1));
    end if;
  end iterate;
end method;

define method write-vbr
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <integer>)
 => ();
  write-vbr(stream, bits, as(<machine-word>, value));
end method;

define method write-vbr
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <character>)
 => ();
  write-vbr(stream, bits, as(<machine-word>, as(<integer>, value)));
end method;

define method write-vbr
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <double-integer>)
 => ();
  write-vbr-aux(stream, bits,
                %double-integer-low(value),
                %double-integer-high(value));
end method;

define method write-vbr
    (stream :: <bitcode-stream>, bits :: <integer>,
     value :: <double-machine-word>)
 => ();
  write-vbr-aux(stream, bits,
                double-machine-word-low(value),
                double-machine-word-high(value));
end method;

define method write-vbr-aux
    (stream :: <bitcode-stream>, bits :: <integer>,
     value-low :: <machine-word>, value-high :: <machine-word>)
 => ();
  let limit = u%shift-left(1, bits - 1);
  let mask = \%-(limit, 1);
  iterate loop (value-low :: <machine-word> = value-low,
                value-high :: <machine-word> = value-high)
    if (zero?(value-high) & u%<(value-low, limit))
      write-fixed(stream, bits, value-low);
    else
      write-fixed(stream, bits,
                  machine-word-logior(limit,
                                      machine-word-logand(value-low, mask)));
      let (low :: <machine-word>, high :: <machine-word>)
        = ud%shift-right(value-low, value-high, bits - 1);
      loop(low, high);
    end if;
  end iterate;
end method;

define method as-signed-vbr (value :: <integer>) => (result :: <machine-word>);
  if (negative?(value))
    machine-word-logior(%shift-left(%negative(value), 1),
                        coerce-integer-to-machine-word(1))
  else
    %shift-left(value, 1)
  end if
end method;

define method as-signed-vbr
    (value :: <double-integer>)
 => (result :: <double-machine-word>)
  if (negative?(value))
    let (low-neg :: <machine-word>, carry :: <machine-word>)
      = u%+(%lognot(%double-integer-low(value)), 1);
    let high-neg :: <machine-word>
      = u%+(%lognot(%double-integer-high(value)), carry);
    let (low :: <machine-word>, high :: <machine-word>)
        = ud%shift-left(low-neg, high-neg, 1);
    make(<double-machine-word>, low: %logior(low, 1), high: high)
  else
    let (low :: <machine-word>, high :: <machine-word>)
        = ud%shift-left(%double-integer-low(value),
                        %double-integer-high(value),
                        1);
    make(<double-machine-word>, low: low, high: high)
  end if
end method;

define method write-alignword
    (stream :: <bitcode-stream>)
 => ();
  unless (zero?(stream.bitcode-accumulator-size))
    bitcode-flush(stream);
    stream.bitcode-accumulator-size := 0;
    stream.bitcode-accumulator := as(<machine-word>, 0);
  end;
end method;

define constant $char6-charset :: <string>
  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._";

define method write-char6
    (stream :: <bitcode-stream>, char :: <character>)
 => ();
  let code = position($char6-charset, char)
    | error("character '%c' cannot be represented as a char6", char);
  write-fixed(stream, 6, code);
end method;

define method write-blob
    (stream :: <bitcode-stream>, blob :: <byte-vector>)
 => ();
  // Output the blob length
  write-vbr(stream, 6, blob.size);

  // Flush bitcode
  write-alignword(stream);

  // Write the blob
  write(stream, blob);

  // Align to 32-bit boundary
  while (modulo(stream-position(stream), 4) ~= 0)
    write-element(stream, 0);
  end while;
end method;

define method close
    (stream :: <bitcode-stream>, #rest keys,
     #key wait?, synchronize?, abort?) => ()
  ignore(wait?, synchronize?, abort?);
  unless (zero?(stream.bitcode-accumulator-size))
    bitcode-flush(stream);
  end unless;
  next-method();
end method close;


/// Blocks

define class <bitcode-block-definition> (<object>)
  //constant slot block-name :: <string>,
  //  required-init-keyword: name:;
  constant slot block-id :: <integer>,
    required-init-keyword: block-id:;
  constant slot block-records :: <object-table> = make(<object-table>);
end class;

define class <bitcode-record-definition> (<object>)
  constant slot record-name :: <symbol>,
    required-init-keyword: name:;
  constant slot record-id :: <integer>,
    required-init-keyword: record-id:;
end class;

define sealed method initialize
    (defn :: <bitcode-block-definition>, #key records :: <sequence>, #all-keys)
 => ();
  for (record-info in records)
    defn.block-records[record-info.record-name] := record-info;
  end for;
end method;

define macro bitcode-block-definer
  { define bitcode-block ?:name = ?block-id:expression
      ?clauses
    end }
    => { define constant ?name
           = make(<bitcode-block-definition>,
                  name: ?"name", block-id: ?block-id,
                  records: vector(?clauses)) }

clauses:
  { } => { }
  { ?clause; ... } => { ?clause, ... }

clause:
    { record ?:name = ?record-id:expression }
      => { make(<bitcode-record-definition>,
                name: ?#"name", record-id: ?record-id) }

end macro;

define macro with-block-output
  { with-block-output (?stream:expression,
                       ?block-info:expression,
                       ?abbrev-bits:expression)
      ?:body
    end }
    => { let stream :: <bitcode-stream> = ?stream;
         let block-definition :: <bitcode-block-definition> = ?block-info;

         // Output the block start code
         write-abbrev-id(stream, $ABBREV-ENTER-SUBBLOCK);

         // ID of this block
         write-vbr(stream, 8, block-definition.block-id);

         // Save the current abbreviation bit width and abbreviation
         // definitions, and set the new ones
         let outer-abbrev-bits = stream.bitcode-abbrev-bits;
         stream.bitcode-abbrev-bits := ?abbrev-bits;
         write-vbr(stream, 4, stream.bitcode-abbrev-bits);

         // If there are global blockinfo definitions for this block type,
         // incorporate them
         let outer-abbrev-definitions = stream.bitcode-abbrev-definitions;
         let blockinfo-definitions
           = element(stream.bitcode-blockinfo, block-definition, default: #f);
         stream.bitcode-abbrev-definitions
           := if (blockinfo-definitions)
                shallow-copy(blockinfo-definitions)
              else
                make(<object-table>)
              end if;

         // Save the current record info and set
         let outer-records = stream.bitcode-records;
         stream.bitcode-records := block-definition.block-records;

         // Write out a dummy word count field
         write-alignword(stream);
         let backpatch-position = stream-position(stream.inner-stream);
         write-fixed(stream, 32, 0); // Will be backpatched below

         ?body;

         // Output the block end code
         write-abbrev-id(stream, $ABBREV-END-BLOCK);
         write-alignword(stream);

         // Backpatch the word count field
         let end-position = stream-position(stream.inner-stream);
         stream-position(stream.inner-stream) := backpatch-position;
         write-fixed(stream, 32,
                     truncate/(end-position - backpatch-position - 4, 4));
         stream-position(stream.inner-stream) := end-position;

         // Restore the abbreviation state and record info
         stream.bitcode-abbrev-bits := outer-abbrev-bits;
         stream.bitcode-abbrev-definitions := outer-abbrev-definitions;
         stream.bitcode-records := outer-records;
        }
end macro;

define bitcode-block $BLOCKINFO_BLOCK = 0
  record SETBID = 1;
  record BLOCKNAME = 2;
  record SETRECORDNAME = 3;
end bitcode-block;

define method stream-record-id
    (stream :: <bitcode-stream>, record :: <symbol>)
 => (id :: <integer>)
  let record-definition = element(stream.bitcode-records, record, default: #f);
  if (~record-definition)
    error("record %= not defined for this block type", record);
  end if;
  record-definition.record-id
end method;


/// Abbreviations

define class <abbrev-op> (<object>)
  constant slot op-kind :: <symbol>,
    required-init-keyword: kind:;
  constant slot op-data :: false-or(<integer>),
    init-value: #f, init-keyword: data:;
end class;

define function op-fixed (width :: <integer>) => (op :: <abbrev-op>)
  make(<abbrev-op>, kind: #"fixed", data: width)
end function;

define function op-vbr (width :: <integer>) => (op :: <abbrev-op>)
  make(<abbrev-op>, kind: #"vbr", data: width)
end function;

define function op-array () => (op :: <abbrev-op>)
  make(<abbrev-op>, kind: #"array")
end function;

define function op-char6 () => (op :: <abbrev-op>)
  make(<abbrev-op>, kind: #"char6")
end function;

define function op-blob() => (op :: <abbrev-op>)
  make(<abbrev-op>, kind: #"blob")
end function;

define method encode-abbrev-op
    (stream :: <bitcode-stream>, op :: <integer>)
 => ();
  write-fixed(stream, 1, 1);
  write-fixed(stream, 8, op);
end method;

define method encode-abbrev-op
    (stream :: <bitcode-stream>, op :: <abbrev-op>)
 => ();
  write-fixed(stream, 1, 0);
  let code
    = select (op.op-kind)
        #"fixed" => 1;
        #"vbr"   => 2;
        #"array" => 3;
        #"char6" => 4;
        #"blob"  => 5;
      end select;
  write-fixed(stream, 3, code);
  if (op.op-data)
    write-vbr(stream, 5, op.op-data);
  end if;
end method;

define class <bitcode-abbrev-definition> (<object>)
  constant slot abbrev-id :: <integer>,
    required-init-keyword: id:;
  constant slot abbrev-ops :: <vector>,
    required-init-keyword: ops:;
end class;

define method write-abbrev-definition
   (stream :: <bitcode-stream>, name :: <symbol>, #rest ops)
 =>();
  let id = $ABBREV-APPLICATION-BASE + stream.bitcode-abbrev-definitions.size;
  stream.bitcode-abbrev-definitions[name]
    := make(<bitcode-abbrev-definition>, id: id,
            ops: choose(rcurry(instance?, <abbrev-op>), ops));

  write-abbrev-id(stream, $ABBREV-DEFINE-ABBREV);
  write-vbr(stream, 5, ops.size);
  map(curry(encode-abbrev-op, stream), ops);
end method;

// NB: Assumes SETBID has already been done
define method write-blockinfo-abbrev-definition
    (stream :: <bitcode-stream>,
     block-definition :: <bitcode-block-definition>,
     name :: <symbol>, #rest ops)
 =>();
  let blockinfo-definitions
    = element(stream.bitcode-blockinfo, block-definition, default: #f)
    | (stream.bitcode-blockinfo[block-definition] := make(<object-table>));

  let id = $ABBREV-APPLICATION-BASE + blockinfo-definitions.size;
  blockinfo-definitions[name]
    := make(<bitcode-abbrev-definition>, id: id,
            ops: choose(rcurry(instance?, <abbrev-op>), ops));

  write-abbrev-id(stream, $ABBREV-DEFINE-ABBREV);
  write-vbr(stream, 5, ops.size);
  map(curry(encode-abbrev-op, stream), ops);
end method;


/// Records

define method write-record
    (stream :: <bitcode-stream>, record :: <symbol>, #rest operands)
 => ();
  let operand-count :: <integer>
    = operands.size
    + if (~empty?(operands) & instance?(operands.last, <sequence>))
        operands.last.size - 1
      else
        0
      end if;
  write-record-head(stream, record, operand-count);

  // Output the arguments
  for (operand in operands)
    if (instance?(operand, <sequence>))
      for (item in operand)
        write-vbr(stream, 6, item);
      end for;
    else
      write-vbr(stream, 6, operand)
    end if;
  end for;
end method;

define method write-record-head
    (stream :: <bitcode-stream>, record :: <symbol>, count :: <integer>)
 => ();
  // Output the unabbreviated record code
  write-abbrev-id(stream, $ABBREV-UNABBREV-RECORD);

  // Output the record code
  write-vbr(stream, 6, stream-record-id(stream, record));

  // Output the total number of arguments
  write-vbr(stream, 6, count);
end method;

define method write-abbrev-record
  (stream :: <bitcode-stream>, name :: <symbol>, #rest operands)
 => ();
  let definition = stream.bitcode-abbrev-definitions[name];

  // Output the abbreviation id
  write-abbrev-id(stream, definition.abbrev-id);

  // Output the operands according to the abbreviation ops
  let ops = definition.abbrev-ops;
  for (value in operands,
       op-index = 0
         then begin
                let op = ops[op-index];
                select (op.op-kind)
                  #"fixed" =>
                    write-fixed(stream, op.op-data, value);
                    op-index + 1;
                  #"vbr"   =>
                    write-vbr(stream, op.op-data, value);
                    op-index + 1;
                  #"array" =>
                    write-vbr(stream, 6, value.size);
                    let aop = ops[op-index + 1];
                    select (aop.op-kind)
                      #"fixed" =>
                        do(curry(write-fixed, stream, aop.op-data), value);
                      #"vbr" =>
                        do(curry(write-vbr, stream, aop.op-data), value);
                      #"char6" =>
                        do(curry(write-char6, stream), value);
                    end select;
                    op-index + 2;
                  #"char6" =>
                    write-char6(stream, value);
                    op-index + 1;
                  #"blob" =>
                    write-blob(stream, value);
                    op-index + 1;
                end select;
              end)
  end for;
end method;

define macro write-record*
  { write-record*(?stream:expression, ?record:expression, ?operands:*) }
    => { let stream = ?stream;
         write-record-head(stream, ?record, %count-operands(?operands));
         %write-operands(stream, ?operands); }
end macro;

define macro %count-operands
  { %count-operands() }
    => { 0 }
  { %count-operands(?operand:*, ?rest:*) }
    => { 1 + %count-operands(?rest) }
end macro;

define macro %write-operands
  { %write-operands(?stream:expression) }
    => { }
  { %write-operands(?stream:expression, ?operand:expression, ?rest:*) }
    => { write-vbr(?stream, 6, ?operand);
         %write-operands(?stream, ?rest); }
end macro;

