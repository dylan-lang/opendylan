Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// See http://llvm.org/docs/BitCodeFormat.html

// Built-in abbrev IDs
define constant $ABBREV-END-BLOCK :: <integer> = 0;
define constant $ABBREV-ENTER-SUBBLOCK :: <integer> = 1;
define constant $ABBREV-DEFINE-ABBREV  :: <integer> = 2;
define constant $ABBREV-UNABBREV-RECORD :: <integer> = 3;

define class <bitcode-stream> (<wrapper-stream>)
  slot bitcode-accumulator :: <machine-word> = as(<machine-word>, 0);
  slot bitcode-accumulator-size :: <integer> = 0;
  slot bitcode-abbrev-bits :: <integer> = 2;
  slot bitcode-records :: <object-table> = make(<object-table>);
end class;

define method bitcode-flush
    (stream :: <bitcode-stream>)
 => ();
  write-4-aligned-bytes
    (inner-stream(stream),
     as(<integer>,
        %logand(stream.bitcode-accumulator,                   #xFF)),
     as(<integer>,
        %logand(%shift-right(stream.bitcode-accumulator, 8),  #xFF)),
     as(<integer>,
        %logand(%shift-right(stream.bitcode-accumulator, 16), #xFF)),
     as(<integer>,
        %logand(%shift-right(stream.bitcode-accumulator, 24), #xFF)));
end method;

define method write-fixed
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <machine-word>)
 => ();
  stream.bitcode-accumulator
    := %logior(stream.bitcode-accumulator,
               %shift-left(value, stream.bitcode-accumulator-size));
  let slack = 32 - stream.bitcode-accumulator-size;
  stream.bitcode-accumulator-size := stream.bitcode-accumulator-size + bits;
  if (stream.bitcode-accumulator-size >= 32)
    bitcode-flush(stream);
    stream.bitcode-accumulator-size := stream.bitcode-accumulator-size - 32;
    if (slack < 32)
      stream.bitcode-accumulator := u%shift-right(value, slack);
    else
      stream.bitcode-accumulator := as(<machine-word>, 0);
    end if;
  end if;
end method;

define method write-fixed
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <integer>)
 => ();
  write-fixed(stream, bits, as(<machine-word>, value));
end method;

define method write-abbrev-id
    (stream :: <bitcode-stream>, id :: <integer>)
 => ();
  write-fixed(stream, stream.bitcode-abbrev-bits, id);
end method;

define method write-vbr
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <machine-word>)
 => ();
  let limit = u%shift-left(1, bits - 1);
  let mask = \%-(limit, 1);
  iterate loop (value :: <machine-word> = value)
    if (u%<(value, limit))
      write-fixed(stream, bits, value);
    else
      write-fixed(stream, bits, %logior(limit, %logand(value, mask)));
      loop(u%shift-right(value, bits - 1));
    end if;
  end iterate;
end method;

define method write-vbr
    (stream :: <bitcode-stream>, bits :: <integer>, value :: <integer>)
 => ();
  write-vbr(stream, bits, as(<machine-word>, value));
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
    | error("character '%c' cannot be represented as a char6");
  write-fixed(stream, 6, code);
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
  constant slot block-name :: <string>,
    required-init-keyword: name:;
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

         // Save the current abbreviation bit width and set 
         let outer-abbrev-bits = stream.bitcode-abbrev-bits;
         stream.bitcode-abbrev-bits := ?abbrev-bits;
         write-vbr(stream, 4, stream.bitcode-abbrev-bits);

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

         // Restore the abbreviation width state and record info
         stream.bitcode-abbrev-bits := outer-abbrev-bits;
         stream.bitcode-records := outer-records;
        }
end macro;

define bitcode-block $BLOCKINFO_BLOCK = 0
  record SETBID = 1;
  record BLOCKNAME = 2;
  record SETRECORDNAME = 3;
end bitcode-block;


/// Records

define method write-record
    (stream :: <bitcode-stream>, record :: <symbol>, #rest operands)
 => ();
  let record-definition = element(stream.bitcode-records, record, default: #f);
  if (~record-definition)
    error("record %= not defined for this block type", record);
  end if;

  // Output the unabbreviated record code
  write-abbrev-id(stream, $ABBREV-UNABBREV-RECORD);

  // Output the record code
  write-vbr(stream, 6, record-definition.record-id);

  // Output the total number of arguments
  let operand-count
    = operands.size
    + if (~empty?(operands) & instance?(operands.last, <sequence>))
        operands.last.size - 1
      else
        0
      end if;
  write-vbr(stream, 6, operand-count);

  // Output the arguments
  for (operand in operands)
    if (instance?(operand, <sequence>))
      for (item in operand)
        write-vbr(stream, 6, as(<integer>, item));
      end for;
    else
      write-vbr(stream, 6, operand)
    end if;
  end for;
end method;