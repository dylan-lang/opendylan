module: disasm
author: Jon Thackray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A disassembler for i386, i486 and i586 code

define method arg-pair-to-string(arg1 :: <byte-string>, arg2 :: <byte-string>) => (str :: <byte-string>)
  concatenate(arg1, ", ", arg2);
end method arg-pair-to-string;

define method hex-digit-to-byte(x :: <integer>) => (ch :: <character>)
  if (x < 0 | x > 15)
    '?'
  else
    as (<character>,
        if (x < 10)
          as(<integer>, '0') + x
        else
          as(<integer>, 'a') + x - 10
        end)
  end
end method hex-digit-to-byte;

define method hex-byte-to-string(x :: <integer>) => (str :: <byte-string>)
  if (x < 0 | x > 255)
    make(<byte-string>, size: 2, fill: '?')
  else
    let str = make(<byte-string>, size: 2);
    let (hi, lo) = truncate/(x, 16);
    str[0] := hex-digit-to-byte(hi);
    str[1] := hex-digit-to-byte(lo);
    str
  end
end method hex-byte-to-string;

define method hex-byte-to-string(x :: <byte-character>)
  => (str :: <byte-string>)
  hex-byte-to-string(as(<integer>, x));
end method hex-byte-to-string;

define method byte-sequence-to-string(code-vector :: <byte-vector>, from :: <integer>, to :: <integer>)
  => (str :: <byte-string>)
  let size = if (to <= from) 0 else to - from end;
  let out-string = make(<byte-string>, size: size * 2);
  for (i from 0 below size)
    let str = hex-byte-to-string(code-vector[from + i]);
    let two-i = 2 * i;
    out-string[two-i] := str[0];
    out-string[two-i + 1] := str[1];
  end for;
  out-string
end method byte-sequence-to-string;

define method immediate-value-to-string(imm :: <byte-immediate-value>) => (str :: <byte-string>)
//  format-out("Entering immediate-value-to-string for byte-immediate-value 0x%x\n", imm.byte-immediate-value);
  let imm = imm.byte-immediate-value;
  let imm1 = if (imm < #x80) imm else #x100 - imm end;
  let hex = hex-byte-to-string(imm1);
  if (imm < #x80) hex else concatenate("-", hex) end
end method immediate-value-to-string;

define method immediate-value-to-string(imm :: <short-immediate-value>) => (str :: <byte-string>)
//  format-out("Entering immediate-value-to-string for short-immediate-value 0x%x\n", imm.short-immediate-value);
  let (hi, lo) = truncate/(imm.short-immediate-value, #x100);
  concatenate(hex-byte-to-string(hi), hex-byte-to-string(lo));
end method immediate-value-to-string;

define method word-imm-to-string(imm :: <four-byte-integer>) => (str :: <byte-string>)
  let lo = logand(imm, #xffff);
  let hi = ash(imm, -16);
  concatenate(immediate-value-to-string(make(<short-immediate-value>, short-immediate-value: hi)),
    immediate-value-to-string(make(<short-immediate-value>, short-immediate-value: lo)));
end method word-imm-to-string;

define method relocated-imm-to-string(ext :: <no-external>, i :: <four-byte-integer>) => (res :: <byte-string>)
  word-imm-to-string(i);
end method;

define method relocated-word-imm-to-string(ext :: <some-external>, imm :: <four-byte-integer>) => (str :: <byte-string>)
  let label-str = ext.label-name;
  if (imm ~== 0)
    let val-str = word-imm-to-string(imm);
    concatenate(label-str, "+", val-str)
  else
    label-str
  end
end method relocated-word-imm-to-string;

define method relocated-imm-to-string(ext :: <labelled-external>, imm :: <four-byte-integer>) => (res :: <byte-string>)
  relocated-word-imm-to-string(ext, imm);
end method;

define method relocated-imm-to-string(ext :: <relative-external>, imm :: <four-byte-integer>) => (res :: <byte-string>)
  let val = imm + ext.offset;
  relocated-word-imm-to-string(ext, val);
end method;

define method immediate-value-to-string(imm :: <word-immediate-value>) => (str :: <byte-string>)
//  format-out("Entering immediate-value-to-string for word-immediate-value 0x%x\n", imm.word-immediate-value);
  let external = imm.word-relocation;
  let val = imm.word-immediate-value;
  relocated-imm-to-string(external, val);
end method immediate-value-to-string;

define method immediate-value-to-string-plus-sign(imm :: <byte-immediate-value>)
  => (str :: <byte-string>)
  let str = immediate-value-to-string(imm);
  if (str[0] == '-') str else concatenate("+", str) end
end method immediate-value-to-string-plus-sign;

define method immediate-value-to-string-plus-sign(imm :: <immediate-value>)
  => (str :: <byte-string>)
  concatenate("+", immediate-value-to-string(imm))
end method immediate-value-to-string-plus-sign;

define method register-to-string(register :: <register>) => (str :: <byte-string>)
  register.register-name;
end method register-to-string;

define method fp-register-to-string(fp-register :: <fp-register>) => (str :: <byte-string>)
  let reg = fp-register.fp-register-pos;
  if (reg == 0) "ST" else format-to-string("ST(%d)", reg) end
end method fp-register-to-string;

define method add-offset-for-immediate(imm :: <word-immediate-value>, str :: <byte-string>) =>
  (res :: <byte-string>)
  select (imm.word-relocation by instance?)
    <no-external> => str;
      otherwise concatenate("offset ", str)
  end select;
end method;

define method add-offset-for-immediate(imm :: <immediate-value>, str :: <byte-string>) =>
  (res :: <byte-string>)
  str
end method;

define method arg-to-string(arg :: <immediate-arg>) => (str :: <byte-string>)
//  format-out("Entering arg-to-string for immediate-arg\n");
  let imm = arg.arg-immediate-value;
  let foo = immediate-value-to-string(imm);
//  format-out("returning '%s'\n", foo);
  add-offset-for-immediate(arg.arg-immediate-value, foo);
end method arg-to-string;

define method arg-to-string(arg :: <register-arg>) => (str :: <byte-string>)
//  format-out("Entering arg-to-string for register-arg\n");
  let foo = register-to-string(arg.register-arg);
//  format-out("returning '%s'\n", foo);
  foo;
end method arg-to-string;

define method arg-to-string(arg :: <fp-register-arg>) => (str :: <byte-string>)
//  format-out("Entering arg-to-string for fp-register-arg\n");
  let foo = fp-register-to-string(arg.fp-register-arg);
//  format-out("returning '%s'\n", foo);
  foo;
end method arg-to-string;

define method scale-to-string(scale :: <integer>) => (str :: <byte-string>)
  select(scale)
    1 => "2";
    2 => "4";
    3 => "8";
    otherwise error ("Invalid scale %d in scale-to-string\n", scale);
  end
end method scale-to-string;

define method memory-index-to-string(index :: <no-memory-index>)
  => (str :: <byte-string>)
  error("memory-index-to-string called with no-memory-index\n");
end method memory-index-to-string;

define method memory-index-to-string(index :: <scaled-indexed-memory-index>)
  => (str :: <byte-string>)
  let reg = register-to-string(index.indexed-memory-index-reg);
  if (index.indexed-memory-index-scale == 0)
    reg
  else
    concatenate(reg, "*", scale-to-string(index.indexed-memory-index-scale));
  end
end method memory-index-to-string;

define method disp-base-index-to-string(disp :: <no-memory-displacement>, base :: <no-memory-base>, index :: <no-memory-index>) => (str :: <byte-string>)
  error("memory-arg without base, displacement or index\n");
end method disp-base-index-to-string;

define method disp-base-index-to-string(disp :: <no-memory-displacement>, base :: <no-memory-base>, index :: <scaled-indexed-memory-index>) => (str :: <byte-string>)
  error("memory-arg with index and no base or displacement\n");
end method disp-base-index-to-string;

define method disp-base-index-to-string(disp :: <no-memory-displacement>, base :: <some-memory-base>, index :: <scaled-indexed-memory-index>) => (str :: <byte-string>)
  concatenate("[", register-to-string(base.memory-base-reg), "+", memory-index-to-string(index), "]");
end method disp-base-index-to-string;

define method disp-base-index-to-string( disp :: <no-memory-displacement>, base :: <some-memory-base>, index :: <no-memory-index>) => (str :: <byte-string>)
  concatenate("[", register-to-string(base.memory-base-reg), "]");
end method disp-base-index-to-string;

define method disp-base-index-to-string(disp :: <some-memory-displacement>, base :: <no-memory-base>, index :: <no-memory-index>) => (str :: <byte-string>)
  concatenate("[", immediate-value-to-string(disp.memory-displacement), "]");
end method disp-base-index-to-string;

define method disp-base-index-to-string(disp :: <some-memory-displacement>, base :: <some-memory-base>, index :: <no-memory-index>) => (str :: <byte-string>)
  concatenate("[", register-to-string(base.memory-base-reg), immediate-value-to-string-plus-sign(disp.memory-displacement), "]");
end method disp-base-index-to-string;

define method disp-base-index-to-string(disp :: <some-memory-displacement>, base :: <no-memory-base>, index :: <scaled-indexed-memory-index>) => (str :: <byte-string>)
  concatenate("[", memory-index-to-string(index), immediate-value-to-string-plus-sign(disp.memory-displacement), "]");
end method disp-base-index-to-string;

define method disp-base-index-to-string(disp :: <some-memory-displacement>, base :: <some-memory-base>, index :: <scaled-indexed-memory-index>) => (str :: <byte-string>)
  concatenate("[", memory-index-to-string(index), "+", register-to-string(base.memory-base-reg), immediate-value-to-string-plus-sign(disp.memory-displacement), "]");
end method disp-base-index-to-string;

define method absolute-address(disp :: <some-memory-displacement>, base :: <no-memory-base>, index :: <no-memory-index>, seg :: <no-segment-override>)
  => (bool :: <boolean>)
  #t
end method absolute-address;

define method absolute-address(disp :: <memory-displacement>, base :: <memory-base>, index :: <memory-index>, seg :: <segment-override>)
  => (bool :: <boolean>)
  #f
end method absolute-address;

define method imm-to-integer(imm :: <byte-immediate-value>) => (int :: <integer>)
  imm.byte-immediate-value
end method imm-to-integer;

define method imm-to-integer(imm :: <short-immediate-value>) => (int :: <integer>)
  imm.short-immediate-value
end method imm-to-integer;

define method imm-to-integer(imm :: <word-immediate-value>) => (int :: <integer>)
  imm.word-immediate-value
end method imm-to-integer;

define method disp-to-integer(disp :: <no-memory-displacement>) => (int :: <integer>)
  0
end method disp-to-integer;

define method disp-to-integer(disp :: <some-memory-displacement>) => (int :: <integer>)
  imm-to-integer(disp.memory-displacement)
end method disp-to-integer;

define method address-to-string(address :: <integer>) => (str :: <byte-string>)
  let hex1 = hex-byte-to-string(logand(address, #xff));
  let hex2 = hex-byte-to-string(logand(ash(address, -8), #xff));
  let hex3 = hex-byte-to-string(logand(ash(address, -16), #xff));
  let hex4 = hex-byte-to-string(logand(ash(address, -24), #xff));
  concatenate(hex4, hex3, hex2, hex1)
end method address-to-string;

define method memory-arg-size-to-string(arg :: <memory-arg-size-byte>) => (res :: <byte-string>)
  "byte ptr"
end method memory-arg-size-to-string;

define method memory-arg-size-to-string(arg :: <memory-arg-size-short>) => (res :: <byte-string>)
  "word ptr"
end method memory-arg-size-to-string;

define method memory-arg-size-to-string(arg :: <memory-arg-size-word>) => (res :: <byte-string>)
  "dword ptr"
end method memory-arg-size-to-string;

define method memory-arg-size-to-string(arg :: <memory-arg-size-double-word>) => (res :: <byte-string>)
  "qword ptr"
end method memory-arg-size-to-string;

define method memory-arg-size-to-string(arg :: <memory-arg-size-word-real>) => (res :: <byte-string>)
  "dword ptr"
end method memory-arg-size-to-string;

define method memory-arg-size-to-string(arg :: <memory-arg-size-double-word-real>) => (res :: <byte-string>)
  "qword ptr"
end method memory-arg-size-to-string;

define method memory-arg-size-to-string(arg :: <memory-arg-size-extended-real>) => (res :: <byte-string>)
  "tbyte ptr"
end method memory-arg-size-to-string;

define method memory-arg-to-string(arg :: <memory-arg>, seg :: <segment-override>, table-lookup :: <function>)
  => (str :: <byte-string>)
//  format-out("Entering arg-to-string for memory-arg\n");
  let disp = arg.memory-arg-disp;
  let base = arg.memory-arg-base;
  let index = arg.memory-arg-index;
  let arg-szie = arg.memory-arg-size;
  let addr =
    if (absolute-address(disp, base, index, seg) & table-lookup(disp-to-integer(disp)) ~== #f)
      let (str, offset) = table-lookup(disp-to-integer(disp));
      concatenate(str, "+", address-to-string(offset))
    else
      let addr = disp-base-index-to-string(disp, base, index);
      select (seg by instance?)
	<no-segment-override> => addr;
	otherwise concatenate(seg.segment-register.segment-register-name, ":", addr);
      end select;
    end;
  concatenate(memory-arg-size-to-string(arg.memory-arg-size), " ", addr);      
end method memory-arg-to-string;

define method get-offset-as-integer(offset :: <byte-offset>) => (int :: <integer>, external :: <external>)
  let int = offset.byte-offset;
  values(if (int < #x80) int else int - #x100 end, $no-external)
end method get-offset-as-integer;

define method get-offset-as-integer(offset :: <short-offset>) => (int :: <integer>, external :: <external>)
  let int = offset.short-offset;
  values(if (int < #x8000) int else int - #x10000 end, $no-external)
end method get-offset-as-integer;

define method get-offset-as-integer(offset :: <word-offset>) => (int :: <integer>, external :: <external>)
  let int = offset.word-offset;
  let max-neg = #x8000 * #x10000;
  let max = 2 * max-neg;
  values(if (int < max-neg) int else int - max end, offset.word-offset-relocation)
end method get-offset-as-integer;

define method arg-plus-override-to-string(arg :: <opcode-argument>, seg :: <segment-override>, table-lookup :: <function>)
  => (str :: <byte-string>)
//  format-out("Entering arg-plus-override-to-string\n");
  select (arg by instance?)
    <memory-arg> => memory-arg-to-string(arg, seg, table-lookup);
    otherwise arg-to-string(arg);
  end select
end method arg-plus-override-to-string;

define method arg-plus-override-plus-table-plus-address-to-string(arg :: <opcode-argument>, seg :: <segment-override>, table :: <table>, end-index :: <integer>, address :: <integer>, table-lookup :: <function>)
  => (str :: <byte-string>)
//  format-out("Entering arg-plus-override-to-string\n");
  select (arg by instance?)
    <memory-arg> => memory-arg-to-string(arg, seg, table-lookup);
    <offset-arg> =>
      let (offset, external) = get-offset-as-integer(arg.offset-arg);
      select (external by instance?)
	<no-external> =>
	  let offset-in-chunk = end-index + offset; /* The real destination address */
	  let res = element(table, offset-in-chunk, default: #f);
	  if (res == #f)
	    let full-address = address + offset-in-chunk;
	    let (str, offset) = table-lookup(full-address);
	    if (str == #f)
	      address-to-string(full-address)
	    else
	      concatenate(str, "+", address-to-string(offset))
	    end
	  else
	    res
	  end;
	<some-external> =>
	  let name = external.label-name;
	  if (offset == 0)
	    name
	  else
	    format-to-string("%s+%s", name, address-to-string(offset))
	  end;
	otherwise =>
	  error("arg-to-string got funny external\n");
      end select;
    otherwise arg-to-string(arg);
  end select
end method arg-plus-override-plus-table-plus-address-to-string;

define method opcode-to-string(table :: <table>, opcode :: <proper-opcode>, end-index :: <integer>, address :: <integer>, table-lookup :: <function>)
  => (str :: <byte-string>)
  local
    method args-to-string(args :: <argument-vector>, seg :: <segment-override>)
    => (str :: <byte-string>)
//      format-out("Entering args-to-string with %d args\n", args.size);
      select (args.size)
        1 =>
	  arg-plus-override-plus-table-plus-address-to-string(args[0], seg, table, end-index, address, table-lookup);
        2 =>
          arg-pair-to-string(arg-plus-override-to-string(args[0], seg, table-lookup), arg-plus-override-to-string(args[1], seg, table-lookup));
	3 =>
	  concatenate(arg-pair-to-string(arg-plus-override-to-string(args[0], seg, table-lookup), arg-plus-override-to-string(args[1], seg, table-lookup)), ", ", arg-plus-override-to-string(args[2], seg, table-lookup));
        otherwise error("decoded opcode has no or incorrect arguments (%d)\n", args.size);
      end select;
    end method args-to-string;
//  format-out("opcode-to-string getting name\n");
  let name = opcode.proper-opcode-name;
//  format-out("opcode-to-string getting args\n");
  let args = opcode.proper-opcode-args;
//  format-out("opcode-to-string getting seg\n");
  let seg = opcode.proper-opcode-seg;
//  format-out("opcode-to-string calling args-to-string if necessary\n");
  if (args.size == 0) name else concatenate(name, " ", args-to-string(args, seg)) end;
end method opcode-to-string;

define method opcode-to-string(table :: <table>, opcode :: <unspecified-not-an-opcode>, end-index :: <integer>, address :: <integer>, table-lookup :: <function>)
  => (str :: <byte-string>)
  "not an opcode (source unknown)";
end method opcode-to-string;

define method opcode-to-string(table :: <table>, opcode :: <not-an-opcode>, end-index :: <integer>, address :: <integer>, table-lookup :: <function>)
  => (str :: <byte-string>)
  concatenate("The following byte sequence is not an opcode '", byte-sequence-to-string(opcode.bytes-read, 0, opcode.bytes-read.size), "'")
end method opcode-to-string;

define method pad(int :: <integer>) => (str :: <byte-string>)
  select (int)
    1 => " ";
    2 => "  ";
    3 => "   ";
    4 => "    ";
    5 => "     ";
    otherwise "      ";
  end select;
end method pad;

define method opcode-and-index-to-string(table :: <table>, opc :: <general-opcode-and-offsets>, address :: <integer>, table-lookup :: <function>)
  => (str :: <byte-string>)
  let index = opc.general-opcode-offset;
  let end-index = opc.general-opcode-end-offset;
  let res = element(table, index, default: #f);
  let prefix =
    if (res == #f)
      "        "
    else
      let len = res.size;
      if (len < 7) concatenate(res, ":", pad(7 - len)) else concatenate(res, ":") end;
    end;
  concatenate(prefix, " ", opcode-to-string(table, opc.general-opcode-opcode, end-index, address, table-lookup))
end method opcode-and-index-to-string;

define method add-label-at-offset(table :: <table>, index :: <integer>, label-number :: <integer>)
  => (table :: <table>, label-number :: <integer>)
  let label = format-to-string("L%d", label-number);
  table[index] := label;
  values(table, label-number + 1)
end method add-label-at-offset;

define method add-labels(list :: <list>, opcode-list :: <list>, max-index :: <integer>)
  => (list :: <list>)
  if (empty?(opcode-list))
    list
  else
    let opcode = head(opcode-list);
    let general-opcode = opcode.general-opcode-opcode;
    let list = select (general-opcode by instance?)
      <proper-opcode> =>
	begin
	  let args = general-opcode.proper-opcode-args;
	  if (args.size == 1)
	    let arg = args[0];
	    select (arg by instance?)
	      <offset-arg> =>
		begin
		  let (offset, external) = get-offset-as-integer(arg.offset-arg);
		  let end-opcode = opcode.general-opcode-end-offset;
		  let index = end-opcode + offset;
		  select (external by instance?)
		    <no-external> =>
		      if (index >= 0 & index < max-index)
			pair(index, list)
		      else
			list
		      end;
		    otherwise list
		  end select
		end;
	      otherwise list
	    end select
	  else
	    list
	  end
	end;
      otherwise list
    end;
    add-labels(list, tail(opcode-list), max-index)
  end
end method add-labels;

define method compute-max-index(index :: <integer>, opcode-list :: <list>)
  => (index :: <integer>)
  if (empty?(opcode-list))
    index
  else
    let opcode = head(opcode-list);
    compute-max-index(opcode.general-opcode-end-offset, tail(opcode-list))
  end
end method compute-max-index;

define method make-label-table(table :: <table>, label-number :: <integer>, indexes :: <list>)
  => (table :: <table>)
  if (empty?(indexes))
    table
  else
    let (table, new-label-number) = add-label-at-offset(table, head(indexes), label-number);
    make-label-table(table, new-label-number, tail(indexes))
  end
end method make-label-table;

define method opcs-to-string(label-table :: <table>, opcode-list :: <list>, address :: <integer>, table-lookup :: <function>, results :: <list>)
   => (res :: <list>)
  if (empty?(opcode-list))
    reverse(results)
  else
    let opcode = head(opcode-list);
    let opc-string = opcode-and-index-to-string(label-table, opcode, address, table-lookup);
    let index = opcode.general-opcode-offset;
    opcs-to-string(label-table, tail(opcode-list), address, table-lookup, pair(opc-string, results))
  end
end method opcs-to-string;

define method opcodes-to-string(opcode-list :: <list>, address :: <integer>, table-lookup :: <function>) =>
  (string-list :: <list>)
  let table = make(<table>, size: 1);
// First generate the necessary labels
  let max-index = compute-max-index(0, opcode-list);
  let index-list = sort(remove-duplicates(add-labels(#(), opcode-list, max-index)));
  let label-table = make-label-table(table, 0, index-list);
  opcs-to-string(table, opcode-list, address, table-lookup, #())
end method opcodes-to-string;
