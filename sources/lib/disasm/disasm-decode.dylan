module: disasm
author: Jon Thackray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A disassembler for i386, i486 and i586 code

define method find-external(external-table :: <simple-object-vector>, index :: <integer>)
  => (ext :: <external>)
// Look for an external at this address
  let size = external-table.size;
  let i = 0;
  let j = size - 1;
  if (j >= i + 1)
    let first = external-table[i];
    let last = external-table[j];
    let first-addr = first.position-in-code-vector;
    let last-addr = last.position-in-code-vector;
    let ext = $no-external;
    while (j >= i + 1)
      if (index == first-addr)
	ext := first;
	j := i
      else
	if (index == last-addr)
	  ext := last;
	  j := i
	else
	  if (j == i + 1)
	    j := i
	  else
	    let k = truncate/(j + i, 2);
	    let mid = external-table[k];
	    let mid-addr = mid.position-in-code-vector;
	    if (mid-addr < index)
	      i := k;
	      first := mid;
	      first-addr := mid-addr
	    else
	      j := k;
	      last := mid;
	      last-addr := mid-addr
	    end
	  end
	end
      end
    end while;
    ext
  elseif (j = 0)
    let ext = external-table[0];
    if (ext.position-in-code-vector == index)
      ext
    else
      $no-external
    end
  else
    $no-external
  end
end method find-external;

define method integer-to-segment-override(int :: <integer>)
  let name = select(int)
    #x2E => "cs";
    #x36 => "ss";
    #x3E => "ds";
    #x26 => "es";
    #x64 => "fs";
    #x65 => "gs";
    otherwise error("to-segment-register unexpected argument 0x%x\n", int);
  end select;
  make(<some-segment-override>, segment-register: make(<segment-register>, segment-register-name: name));
end method integer-to-segment-override;

// define method immediate-size(imm :: <byte-immediate-value>) => (int :: <integer>)
//   1
// end method immediate-size;
// 
// define method immediate-size(imm :: <short-immediate-value>) => (int :: <integer>)
//   2
// end method immediate-size;
// 
// define method immediate-size(imm :: <word-immediate-value>) => (int :: <integer>)
//   4
// end method immediate-size;

define method integer-to-register-name(int :: <integer>, val :: <byte-immediate-value>)
  => (str :: <byte-string>)
  select (int)
    0 => "al";
    1 => "cl";
    2 => "dl";
    3 => "bl";
    4 => "ah";
    5 => "ch";
    6 => "dh";
    7 => "bh";
    otherwise error("Register number %d out of range\n", int);
  end select;
end method integer-to-register-name;

define method integer-to-register-name(int :: <integer>, val :: <short-immediate-value>)
  => (str :: <byte-string>)
  select (int)
    0 => "ax";
    1 => "cx";
    2 => "dx";
    3 => "bx";
    4 => "sp";
    5 => "bp";
    6 => "si";
    7 => "di";
    otherwise error("Register number %d out of range\n", int);
  end select;
end method integer-to-register-name;

define method integer-to-register-name(int :: <integer>, val :: <word-immediate-value>)
  => (str :: <byte-string>)
  select (int)
    0 => "eax";
    1 => "ecx";
    2 => "edx";
    3 => "ebx";
    4 => "esp";
    5 => "ebp";
    6 => "esi";
    7 => "edi";
    otherwise error("Register number %d out of range\n", int);
  end select;
end method integer-to-register-name;

define method integer-to-register(int :: <integer>, size :: <immediate-value>)
  => (reg :: <register>)
  let name = integer-to-register-name(int, size);
  make (<register>, register-integer-rep: int, register-name: name);
end method integer-to-register;

define method integer-to-register(int :: <integer>, size :: <immediate-arg>)
  => (reg :: <register>)
  integer-to-register(int, size.arg-immediate-value);
end method integer-to-register;

define method integer-to-register(int :: <integer>, size :: <integer>)
  => (reg :: <register>)
  let imm-value = select(size)
    1 => $zero-byte-immediate-value;
    2 => $zero-short-immediate-value;
    4 => $zero-word-immediate-value;
    otherwise error("integer-to-register passed bad size %d\n", size);
  end select;
  let name = integer-to-register-name(int, imm-value);
  make (<register>, register-integer-rep: int, register-name: name);
end method integer-to-register;

define method integer-to-fp-register(int :: <integer>)
  => (reg :: <fp-register>)
  make(<fp-register>, fp-register-pos: int)
end method integer-to-fp-register;

define method register-to-arg(reg :: <register>) => (arg :: <register-arg>)
  make(<register-arg>, register-arg: reg);
end method register-to-arg;

define method fp-register-to-arg(reg :: <fp-register>) => (arg :: <fp-register-arg>)
  make(<fp-register-arg>, fp-register-arg: reg);
end method fp-register-to-arg;

define method integer-to-fp-register-arg(int :: <integer>) => (arg :: <fp-register-arg>)
  fp-register-to-arg(integer-to-fp-register(int));
end method integer-to-fp-register-arg;

define constant $al-register = integer-to-register(0, 1);

define constant $cl-register = integer-to-register(1, 1);

define constant $ax-register = integer-to-register(0, 2);

define constant $eax-register = integer-to-register(0, 4);

define constant $al-register-arg = register-to-arg($al-register);

define constant $cl-register-arg = register-to-arg($cl-register);

define constant $ax-register-arg = register-to-arg($ax-register);

define constant $eax-register-arg = register-to-arg($eax-register);

define constant $stack-top = integer-to-fp-register-arg(0);

define method offset-to-arg(offs :: <offset>)
  => (arg :: <offset-arg>)
  make(<offset-arg>, offset-arg: offs);
end method offset-to-arg;

define method base-is-ebp(base :: <memory-base>) => (bool :: <boolean>)
  select (base by instance?)
    <no-memory-base> =>
      begin
        #f;
      end;
    <some-memory-base> =>
      begin
        (base.memory-base-reg.register-integer-rep == 5);
      end;
    otherwise error("base-is-ebp called on crap\n");
  end select
end method base-is-ebp;

define method get-byte-offset(code-vector :: <byte-vector>, index :: <integer>, bound :: <integer>)
  => (off :: <offset>, new-index :: <integer>)
  if (index < bound)
    let byte = code-vector[index];
      values (make(<byte-offset>, byte-offset: byte), index + 1)
  else
    error(make(<disassembly-no-more-bytes-error>, position: index));
  end
end method get-byte-offset;

define method get-short-offset(code-vector :: <byte-vector>, index :: <integer>, bound :: <integer>)
  => (off :: <offset>, new-index :: <integer>)
  if (index + 1 < bound)
    let short = code-vector[index + 1] * #x100 + code-vector[index];
    values (make(<short-offset>, short-offset: short), index + 2)
  else
    error(make(<disassembly-no-more-bytes-error>, position: index));
  end
end method get-short-offset;

define method get-word-offset(code-vector :: <byte-vector>, index :: <integer>, bound :: <integer>, external-table :: <simple-object-vector>)
  => (off :: <offset>, new-index :: <integer>)
  let external = find-external(external-table, index);
  if (index + 3 < bound)
    let word = (((code-vector[index + 3]) * #x100 + code-vector[index + 2]) * #x100 + code-vector[index + 1]) * #x100 + code-vector[index];
    values (make(<word-offset>, word-offset: word, word-offset-relocation: external), index + 4)
  else
    error(make(<disassembly-no-more-bytes-error>, position: index));
  end
end method get-word-offset;

define method imm-to-imm-arg(imm :: <immediate-value>) => (imm-arg :: <immediate-arg>)
  make(<immediate-arg>, arg-immediate-value: imm);
end method imm-to-imm-arg;

define method integer-to-imm(int :: <integer>) => (arg :: <immediate-arg>)
  imm-to-imm-arg(make(<byte-immediate-value>, byte-immediate-value: int))
end method integer-to-imm;

define method get-byte-immediate(code-vector :: <byte-vector>, index :: <integer>, bound :: <integer>)
  => (imm :: <byte-immediate-value>, int :: <integer>)
  if (index < bound)
    values(make(<byte-immediate-value>, byte-immediate-value: code-vector[index]), index + 1)
  else
    error(make(<disassembly-no-more-bytes-error>, position: index));
  end
end method get-byte-immediate;

define method get-short-immediate(code-vector :: <byte-vector>, index :: <integer>, bound :: <integer>)
  => (imm :: <short-immediate-value>, int :: <integer>)
  if (index + 1 < bound)
    values(make(<short-immediate-value>, short-immediate-value: code-vector[index + 1] * #x100 + code-vector[index]), index + 2)
  else
    error(make(<disassembly-no-more-bytes-error>, position: index));
  end
end method get-short-immediate;

define method get-word-immediate(code-vector :: <byte-vector>, index :: <integer>, bound :: <integer>, external-table :: <simple-object-vector>)
  => (imm :: <word-immediate-value>, int :: <integer>)
  let external = find-external(external-table, index);
  if (index + 3 < bound)
    values(make(<word-immediate-value>, word-immediate-value: ((code-vector[index + 3] * #x100 + code-vector[index + 2]) * #x100 + code-vector[index + 1]) * #x100 + code-vector[index], word-relocation: external), index + 4)
  else
    error(make(<disassembly-no-more-bytes-error>, position: index));
  end
end method get-word-immediate;

define method fp-fun-name1(int :: <integer>) => (str :: <byte-string>)
  select (logand(int, 7))
    0 => "fadd";
    1 => "fmul";
    2 => "fcom";
    3 => "fcomp";
    4 => "fsub";
    5 => "fsub";
    6 => "fdiv";
    7 => "fdivr";
  end select
end method fp-fun-name1;

define method fp-fun-name2(int :: <integer>) => (str :: <byte-string>)
  select (logand(int, 7))
    0 => "fiadd";
    1 => "fimul";
    2 => "ficom";
    3 => "ficomp";
    4 => "fisub";
    5 => "fisubr";
    6 => "fidiv";
    7 => "fidivr";
  end select
end method fp-fun-name2;

define method fp-fun-name3(int :: <integer>, index :: <integer>) => (str :: <byte-string>)
  select (logand(int, 7))
    0 => "fadd";
    1 => "fmul";
    2, 3 => error(make(<disassembly-unexpected-byte-error>, position: index));
    4 => "fsubr";
    5 => "fsub";
    6 => "fdivr";
    7 => "fdiv";
  end select
end method fp-fun-name3;

define method fp-fun-name4(int :: <integer>, index :: <integer>) => (str :: <byte-string>)
  select (logand(int, 7))
    0 => "faddp";
    1 => "fmulp";
    2,3 => error(make(<disassembly-unexpected-byte-error>, position: index));
    4 => "fsubp";
    5 => "fsubrp";
    6 => "fdivrp";
    7 => "fdivp";
  end select
end method fp-fun-name4;

define method decode-sib(index :: <integer>, mod :: <integer>, code-vector :: <byte-vector>, end-index :: <integer>)
  => (base :: <memory-base>, scale :: <memory-index>, new-index :: <integer>)
  if (index < end-index)
    let byte = code-vector[index];
    let (rest, base-reg-int) = truncate/(byte, 8);
    let (indexer, index-reg-int) = truncate/(rest, 8);
    let index-reg = integer-to-register(index-reg-int, 4);
    let index-and-scale =
      if (index-reg-int == 4)
        $no-memory-index; // This case is to allow esp as a base
      else
        make(<scaled-indexed-memory-index>, indexed-memory-index-reg: index-reg, indexed-memory-index-scale: indexer);
      end;
    let memory-base =
      make(<some-memory-base>, memory-base-reg: integer-to-register(base-reg-int, 4));
    values(memory-base, index-and-scale, index + 1)
  else
    error(make(<disassembly-no-more-bytes-error>, position: index))
  end;
end method decode-sib;

define method full-decode-rm-arg-to-integer(index :: <integer>, eight-bit :: <boolean>, code-vector :: <byte-vector>, is-16-bit-operands :: <is-16-bit-operands>, is-16-bit-addressing :: <is-16-bit-addressing>, end-index :: <integer>, external-table :: <simple-object-vector>, memory-arg-size :: <memory-arg-size>)
  => (int :: <integer>, arg :: <opcode-argument>, new-index :: <integer>)
  if (is-16-bit-addressing.is-16-bit)
    format-out("decode-rm-arg-to-integer doesn't understand 16 bit addressing yet\n");
    error(make(<disassembly-unexpected-byte-error>, position: index))
  else
    if (index < end-index)
      let size = if (eight-bit) 1 else if (is-16-bit-operands.is-16-bit) 2 else 4 end end;
      let byte = code-vector[index];
      let (rest, reg) = truncate/(byte, 8); // The register
      let (mod, reg-opc) = truncate/(rest, 8); // The register/opcode and modifier
      let (mem, new-index) = select (mod)
        0 => // [reg] or disp32 or [SIB]
          if (reg == 4)
            let (memory-base, scaled-index, new-index) = decode-sib(index + 1, mod, code-vector, end-index);
            if (mod == 0 & base-is-ebp(memory-base))
              let (disp, new-new-index) = get-word-immediate(code-vector, new-index, end-index, external-table);
              values(
              make(<memory-arg>,
                   memory-arg-disp: make(<some-memory-displacement>, memory-displacement: disp),
                   memory-arg-base: $no-memory-base,
                   memory-arg-index: scaled-index,
		   memory-arg-size: memory-arg-size), new-new-index)
            else
              values(
              make(<memory-arg>,
                   memory-arg-disp: $no-memory-displacement,
                   memory-arg-base: memory-base,
                   memory-arg-index: scaled-index,
		   memory-arg-size: memory-arg-size), new-index)
            end
          elseif (reg == 5) // disp32 without base or index
            let (disp, new-index) = get-word-immediate(code-vector, index + 1, end-index, external-table);
            values(
            make(<memory-arg>,
                 memory-arg-disp: make(<some-memory-displacement>, memory-displacement: disp),
                 memory-arg-base: $no-memory-base,
                 memory-arg-index: $no-memory-index,
		 memory-arg-size: memory-arg-size), new-index);
          else
            let reg = integer-to-register(reg, $zero-word-immediate-value);
            values(
            make(<memory-arg>,
                 memory-arg-disp: $no-memory-displacement,
                 memory-arg-base: make(<some-memory-base>, memory-base-reg: reg),
                 memory-arg-index: $no-memory-index,
		 memory-arg-size: memory-arg-size), index + 1);
          end;
        1 => // disp8[reg] or disp8[SIB]
          begin
            let (memory-base, scaled-index, new-index) =
              if (reg == 4)
                decode-sib(index + 1, mod, code-vector, end-index);
              else
                values(make(<some-memory-base>, memory-base-reg: integer-to-register(reg, $zero-word-immediate-value)),
                       $no-memory-index,
                       index + 1);
              end;
            let (disp, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
            values(
            make(<memory-arg>,
                   memory-arg-disp: make(<some-memory-displacement>, memory-displacement: disp),
                   memory-arg-base: memory-base,
                   memory-arg-index: scaled-index,
		   memory-arg-size: memory-arg-size), new-new-index);
          end;
        2 => // disp32[reg] or disp32[SIB] or disp16[reg]
          begin
            let (memory-base, scaled-index, new-index) =
              if (reg == 4)
                decode-sib(index + 1, mod, code-vector, end-index);
              else
                values(make(<some-memory-base>, memory-base-reg: integer-to-register(reg, $zero-word-immediate-value)),
                       $no-memory-index,
                       index + 1);
              end;
            let (disp, new-new-index) = get-word-immediate(code-vector, new-index, end-index, external-table);
            values(
            make(<memory-arg>,
                   memory-arg-disp: make(<some-memory-displacement>, memory-displacement: disp),
                   memory-arg-base: memory-base,
                   memory-arg-index: scaled-index,
		   memory-arg-size: memory-arg-size), new-new-index);
          end;
        3 => values(register-to-arg(integer-to-register(reg, size)), index + 1);
        otherwise error("decode-rm-arg-to-integer mod %d out of range\n", mod);
      end select;
      values(reg-opc, mem, new-index);
    else
      error(make(<disassembly-no-more-bytes-error>, position: index));
    end
  end
end method full-decode-rm-arg-to-integer;

define method decode-opcode-without-prefixes(code-vector :: <byte-vector>, index :: <integer>, end-index :: <integer>, external-table :: <simple-object-vector>, segment-override :: <segment-override>, is-16-bit-operands :: <is-16-bit-operands>, is-16-bit-addressing :: <is-16-bit-addressing>, repeater :: <repeater>)
  => (opc :: <general-opcode>, int :: false-or(<integer>))
  local
    method make-proper-opcode(name :: <byte-string>, args :: <argument-vector>)
      => (opc :: <proper-opcode>)
      make(<proper-opcode>, proper-opcode-name: name, proper-opcode-args: args, proper-opcode-seg: segment-override)
    end method make-proper-opcode,
    method decode-rm-arg-to-integer(index :: <integer>, eight-bit :: <boolean>, memory-arg-size :: <memory-arg-size>)
      => (int :: <integer>, arg :: <opcode-argument>, new-index :: <integer>)
      full-decode-rm-arg-to-integer(index, eight-bit, code-vector, is-16-bit-operands, is-16-bit-addressing, end-index, external-table, memory-arg-size)
    end method decode-rm-arg-to-integer,
    method decode-rm-arg-to-reg(index :: <integer>, eight-bit :: <boolean>, memory-arg-size :: <memory-arg-size>)
      => (reg :: <register>, arg :: <opcode-argument>, new-index :: <integer>)
      let (reg, arg, new-index) = decode-rm-arg-to-integer(index, eight-bit, memory-arg-size);
      values(integer-to-register(reg, if (eight-bit) 1 else if (is-16-bit-operands.is-16-bit) 2 else 4 end end), arg, new-index);
    end method decode-rm-arg-to-reg,
    method decode-rm-arg-to-vector(index :: <integer>, eight-bit :: <boolean>, memory-arg-size :: <memory-arg-size>)
      => (vec :: <vector>, new-index :: <integer>)
      let (reg, arg, new-index) = decode-rm-arg-to-reg(index, eight-bit, memory-arg-size);
      values(vector(register-to-arg(reg), arg), new-index);
    end method decode-rm-arg-to-vector,
    method decode-rm-arg-to-rev-vector(index :: <integer>, eight-bit :: <boolean>, memory-arg-size :: <memory-arg-size>)
      => (vec :: <vector>, new-index :: <integer>)
      let (reg, arg, new-index) = decode-rm-arg-to-reg(index, eight-bit, memory-arg-size);
      values(vector(arg, register-to-arg(reg)), new-index);
    end method decode-rm-arg-to-rev-vector,
    method decode-simple-add-type-opcode(index :: <integer>, initial-opcode :: <integer>, name :: <byte-string>)
      => (opc :: <general-opcode>, int :: <integer>)
      let (args, new-index) = select (initial-opcode)
        #x04, #x0c, #x14, #x1c, #x24, #x2c, #x34, #x3c =>
// add al, imm8
          begin
            let (arg, new-index) = get-byte-immediate(code-vector, index, end-index);
            values(vector($al-register-arg, imm-to-imm-arg(arg)), new-index);
          end;
        #x05, #x0d, #x15, #x1d, #x25, #x2d, #x35, #x3d =>
// This one can take a short or a word argument, depending on is-16-bit-operands
// add ax, imm16 or add eax imm32
          begin
	    let (implicit-arg, arg, new-index) =
	      if (is-16-bit-operands.is-16-bit)
		let (arg, new-index) = get-short-immediate(code-vector, index, end-index);
		values($ax-register-arg, arg, new-index);
	      else
		let (arg, new-index) = get-word-immediate(code-vector, index, end-index, external-table);
		values($eax-register-arg, arg, new-index);
              end;
            values(vector(implicit-arg, imm-to-imm-arg(arg)), new-index)
          end;
        #x00, #x08, #x10, #x18, #x20, #x28, #x30, #x38 =>
// add r/m8, r8
          begin
            let (vec, new-index) = decode-rm-arg-to-rev-vector(index, #t, $byte-arg-size);
            values(vec, new-index);
          end;
        #x01, #x09, #x11, #x19, #x21, #x29, #x31, #x39 =>
// add r/m16, r16
// add r/m32, r32
          begin
            let (vec, new-index) = decode-rm-arg-to-rev-vector(index, #f, $word-arg-size);
            values(vec, new-index);
          end;
        #x02, #x0a, #x12, #x1a, #x22, #x2a, #x32, #x3a =>
// add r8, r/m8
          begin
            let (vec, new-index) = decode-rm-arg-to-vector(index, #t, $byte-arg-size);
            values(vec, new-index);
          end;
        #x03, #x0b, #x13, #x1b, #x23, #x2b, #x33, #x3b =>
// add r16, r/m16
// add r32, r/m32
          begin
            let (vec, new-index) = decode-rm-arg-to-vector(index, #f, $word-arg-size);
            values(vec, new-index);
          end;
        otherwise error("Bad initial-opcode %d passed to decode-simple-add-type-opcode\n", initial-opcode);
      end select;
      values(make-proper-opcode(name, args), new-index);
    end method decode-simple-add-type-opcode,
    method decode-complex-add-type-opcode(index :: <integer>, initial-opcode :: <integer>)
      => (opc :: <general-opcode>, int :: <integer>)
      let (args, new-index, opcode-extension) = select (initial-opcode)
        #x80 =>
// add r/m8, imm8
          begin
            let (opcode-extension, arg1, new-index) = decode-rm-arg-to-integer(index, #t, $byte-arg-size);
            let (arg2, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
            values(vector(arg1, imm-to-imm-arg(arg2)), new-new-index, opcode-extension);
          end;
        #x81 =>
// add r/m16, imm16
// add r/m32, imm32
          begin
            let (opcode-extension, arg1, new-index) = decode-rm-arg-to-integer(index, #f, $word-arg-size);
            let (arg2, new-new-index) =
              if (is-16-bit-operands.is-16-bit)
                get-short-immediate(code-vector, new-index, end-index)
              else
                get-word-immediate(code-vector, new-index, end-index, external-table)
              end;
            values(vector(arg1, imm-to-imm-arg(arg2)), new-new-index, opcode-extension);
          end;
        #x83 =>
// add r/m16, imm8
// add r/m32, imm8
          begin
            let (opcode-extension, arg1, new-index) = decode-rm-arg-to-integer(index, #f, $word-arg-size);
            let (arg2, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
            values(vector(arg1, imm-to-imm-arg(arg2)), new-new-index, opcode-extension);
          end;
        otherwise error("Bad initial-opcode %d passed to decode-complex-add-opcode\n", initial-opcode);
      end select;
      let name = select(opcode-extension)
        0 => "add";
        1 => "or";
        2 => "adc";
        3 => "sbb";
        4 => "and";
        5 => "sub";
        6 => "xor";
        7 => "cmp";
        otherwise error("opcode-extension %d in decode-complex-add-opcode out of range\n", opcode-extension);
      end select;
      values(make-proper-opcode(name, args), new-index);
    end method decode-complex-add-type-opcode;
  block ()
    if (index < end-index)
      let first-byte = code-vector[index];
      let new-index = index + 1;
      local method simple-opcode(name :: <byte-string>)
        => (opc :: <proper-opcode>, int :: <integer>)
        values(make-proper-opcode(name, $empty-vector), new-index)
      end method simple-opcode,
      method simple-two-byte-opcode(name :: <byte-string>, byte2 :: <integer>)
        => (opc :: <proper-opcode>, int :: <integer>)
	if (new-index < end-index & code-vector[new-index] = byte2)
	  values(make-proper-opcode(name, $empty-vector), new-index + 1)
	else
	  error(make(<disassembly-unexpected-byte-error>, position: new-index))
	end
      end method simple-two-byte-opcode,
      method simple-opcode-with-index(name :: <byte-string>, index :: <integer>)
	=> (opc :: <proper-opcode>, int :: <integer>)
        values(make-proper-opcode(name, $empty-vector), index)
      end method simple-opcode-with-index,
      method decode-simple-inc-type(name :: <byte-string>)
        => (opc :: <proper-opcode>, int :: <integer>)
	let reg-int = logand(first-byte, #x07);
	let reg-arg = register-to-arg(integer-to-register(reg-int, if (is-16-bit-operands.is-16-bit) 2 else 4 end));
	values(make-proper-opcode(name, vector(reg-arg)), new-index)
      end method decode-simple-inc-type,
      method decode-condition(byte :: <integer>)
	=> (str :: <byte-string>)
	select(logand(byte, #x0f))
	  0 => "o";
	  1 => "no";
	  2 => "b";
	  3 => "nb";
	  4 => "z";
	  5 => "nz";
	  6 => "be";
	  7 => "nbe";
	  8 => "s";
	  9 => "ns";
	  10 => "p";
	  11 => "np";
	  12 => "l";
	  13 => "nl";
	  14 => "le";
	  15 => "nle";
	end select
      end method decode-condition,
      method decode-0f-type(index :: <integer>)
        => (opc :: <proper-opcode>, int :: <integer>)
        if (index < end-index)
	  let byte = code-vector[index];
          select (byte)
	    #x00 =>
	      begin
		let (int, arg, new-index) = decode-rm-arg-to-vector(index + 1, #f, $word-arg-size);
		let name = select(int)
		  0 => "sldt";
		  1 => "str";
		  2 => "lldt";
		  3 => "ltr";
		  4 => "verr";
		  5 => "verw";
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index));
		end select;
		values(make-proper-opcode(name, vector(arg)), new-index);
	      end;
	    #x01 =>
	      begin
		let (int, arg, new-index) = decode-rm-arg-to-vector(index + 1, #f, $word-arg-size);
		let name = select(int)
		  0 => "sgdt";
		  1 => "sidt";
		  2 => "lgdt";
		  3 => "lidt";
		  4 => "smsw";
		  6 => "lmsw";
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index));
		end select;
		values(make-proper-opcode(name, vector(arg)), new-index);
	      end;
	    #x02 => // lar
	      begin
		let (vec, new-index) = decode-rm-arg-to-vector(index + 1, #f, $word-arg-size);
		values(make-proper-opcode("lar", vec), new-index);
	      end;
	    #x03 => // lsl
	      begin
		let (vec, new-index) = decode-rm-arg-to-vector(index + 1, #f, $word-arg-size);
		values(make-proper-opcode("lsl", vec), new-index);
	      end;
	    #x80, #x81, #x82, #x83, #x84, #x85, #x86, #x87, #x88, #x89, #x8a, #x8b, #x8c, #x8d, #x8e, #x8f => // jcc
	      begin
		let name = concatenate("j", decode-condition(byte));
		let (offset, new-index) =
		  if (is-16-bit-operands.is-16-bit)
		    get-short-offset(code-vector, index + 1, end-index)
		  else
		    get-word-offset(code-vector, index + 1, end-index, external-table)
		  end;
		values(make-proper-opcode(name, vector(offset-to-arg(offset))), new-index);
	      end;
	    #x90, #x91, #x92, #x93, #x94, #x95, #x96, #x97, #x98, #x99, #x9a, #x9b, #x9c, #x9d, #x9e, #x9f => // setcc
	      begin
		let name = concatenate("set", decode-condition(byte));
		values(make-proper-opcode(name, $empty-vector), new-index);
	      end;
	    #xa0 => simple-two-byte-opcode("push fs", #xa1);
	    #xa1 => simple-two-byte-opcode("pop fs", #xa1);
	    #xa3, #xab, #xb3, #xbb, #xbc, #xbd =>
	      begin
		let name = select(byte)
		  #xa3 => "bt";
		  #xab => "bts";
		  #xb3 => "btr";
		  #xbb => "btc";
		  #xbc => "bsf";
		  #xbd => "bsr";
		end select;
		let (vec, new-index) = decode-rm-arg-to-rev-vector(index + 1, #f, $word-arg-size);
		values(make-proper-opcode(name, vec), new-index);
	      end;
	    #xa4, #xa5, #xac, #xad =>
	      begin
		let name = if (byte == #xa4 | byte == #xa5) "shld" else "shrd" end;
		let (reg, arg, new-index) = decode-rm-arg-to-reg(index + 1, #f, $word-arg-size);
		let (arg3, new-new-index) =
		  if (byte == #xa4 | byte == #xac)
		    let (imm, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
		    values(imm-to-imm-arg(imm), new-new-index)
		  else
		    values($cl-register-arg, new-index)
		  end;
		values(make-proper-opcode(name, vector(arg, register-to-arg(reg), arg3)), new-new-index)
	      end;
	    #xa8 => simple-two-byte-opcode("push gs", #xa9);
	    #xa9 => simple-two-byte-opcode("pop gs", #xa9);
	    #xaf => // imul
	      begin
		let (vec, new-index) = decode-rm-arg-to-vector(index + 1, #f, $word-arg-size);
		values(make-proper-opcode("imul", vec), new-index);
	      end;
	    #xb0, #xb1 =>
	      begin
		let (is-8-bit, arg-size) =
		  if (byte == #xb0) values(#t, $byte-arg-size)
		  else
		    values(#f, $word-arg-size)
		  end;
		let (vec, new-index) = decode-rm-arg-to-rev-vector(index + 1, is-8-bit, arg-size);
		values(make-proper-opcode("cmpxchg", vec), new-index);
	      end;
	    #xb2, #xb4, #xb5 =>
	      begin
		let name = select(byte)
		  #xb2 => "lss";
		  #xb4 => "lfs";
		  #xb5 => "lgs";
		  otherwise error("Unknown byte 0x%x for lgs type opcode\n", byte);
		end select;
		let (vec, new-index) = decode-rm-arg-to-vector(index + 1, #f, $word-arg-size);
		values(make-proper-opcode(name, vec), new-index);
	      end;
	    #xb6, #xb7, #xbe, #xbf => // movzx/movsx
	      begin
		let name = if (byte == #xb6 | byte == #xb7) "movzx" else "movsx" end;
		let arg-size =
		  if (byte == #xb6 | byte == #xbe) $byte-arg-size else $short-arg-size end;
		let (vec, new-index) = decode-rm-arg-to-vector(index + 1, #f, arg-size);
		values(make-proper-opcode(name, vec), new-index);
	      end;
	    #xba => // bt/btc/btr/bts
	      begin
		let (int, arg, new-index) = decode-rm-arg-to-integer(index + 1, #f, $word-arg-size);
		let name = select(int)
		  4 => "bt";
		  5 => "bts";
		  6 => "btr";
		  7 => "btc";
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index));
		end select;
		let (imm, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
		values(make-proper-opcode(name, vector(arg, imm-to-imm-arg(imm))), new-new-index);
	      end;
	    #xc0, #xc1 =>
	      begin
		let (is-8-bit, arg-size) =
		  if (byte == #xc0) values(#t, $byte-arg-size)
		  else
		    values(#f, $word-arg-size)
		  end;
		let (vec, new-index) = decode-rm-arg-to-rev-vector(index + 1, is-8-bit, arg-size);
		values(make-proper-opcode("xadd", vec), new-index);
	      end;
	    otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index));
	  end select
        else
          error(make(<disassembly-no-more-bytes-error>, position: index))
        end
      end method decode-0f-type;
      select (first-byte)
        #x00, #x01, #x02, #x03, #x04, #x05
          => decode-simple-add-type-opcode(new-index, first-byte, "add");
	#x06 => simple-opcode("push es");
	#x07 => simple-opcode("pop ds");
        #x08, #x09, #x0a, #x0b, #x0c, #x0d
          => decode-simple-add-type-opcode(new-index, first-byte, "or");
	#x0e => simple-opcode("push cs");
	#x0f => decode-0f-type(new-index);
        #x10, #x11, #x12, #x13, #x14, #x15
          => decode-simple-add-type-opcode(new-index, first-byte, "adc");
	#x16 => simple-opcode("push ss");
	#x17 => simple-opcode("pop es");
        #x18, #x19, #x1a, #x1b, #x1c, #x1d
          => decode-simple-add-type-opcode(new-index, first-byte, "sbb");
	#x1e => simple-opcode("push ds");
	#x1f => simple-opcode("pop ss");
        #x20, #x21, #x22, #x23, #x24, #x25
          => decode-simple-add-type-opcode(new-index, first-byte, "and");
        #x27 => simple-opcode("daa");
        #x28, #x29, #x2a, #x2b, #x2c, #x2d
          => decode-simple-add-type-opcode(new-index, first-byte, "sub");
        #x2f => simple-opcode("das");
        #x30, #x31, #x32, #x33, #x34, #x35
          => decode-simple-add-type-opcode(new-index, first-byte, "xor");
        #x37 => simple-opcode("aaa");
        #x38, #x39, #x3a, #x3b, #x3c, #x3d
          => decode-simple-add-type-opcode(new-index, first-byte, "cmp");
        #x3f => simple-opcode("aas");
	#x40, #x41, #x42, #x43, #x44, #x45, #x46, #x47 => decode-simple-inc-type("inc");
	#x48, #x49, #x4a, #x4b, #x4c, #x4d, #x4e, #x4f => decode-simple-inc-type("dec");
	#x50, #x51, #x52, #x53, #x54, #x55, #x56, #x57 => decode-simple-inc-type("push");
	#x58, #x59, #x5a, #x5b, #x5c, #x5d, #x5e, #x5f => decode-simple-inc-type("pop");
        #x60 => simple-opcode(if (is-16-bit-operands.is-16-bit) "pusha" else "pushad" end);
        #x61 => simple-opcode(if (is-16-bit-operands.is-16-bit) "popa" else "popad" end);
	#x62 =>
	  begin
	    let (vec, new-new-index) = decode-rm-arg-to-vector(new-index, #f, $word-arg-size);
	    values(make-proper-opcode("bound", vec), new-new-index)
	  end;
	#x63 =>
	  begin
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, #f, $word-arg-size);
	    let reg = integer-to-register(int, 2); // Always 16 bit here
	    values(make-proper-opcode("arpl", vector(arg, register-to-arg(reg))), new-new-index)
	  end;
	#x68 =>
	  begin
	    let (imm, new-new-index) =
	      if (is-16-bit-operands.is-16-bit)
		get-short-immediate(code-vector, new-index, end-index)
	      else
		get-word-immediate(code-vector, new-index, end-index, external-table)
	      end;
	    values(make-proper-opcode("push", vector(imm-to-imm-arg(imm))), new-new-index);
	  end;
	#x69 =>
	  begin
	    let (reg, arg, new-new-index) = decode-rm-arg-to-reg(new-index, #f, $word-arg-size);
	    let (imm, new-new-new-index) =
	      if (is-16-bit-operands.is-16-bit)
		get-short-immediate(code-vector, new-new-index, end-index)
	      else
		get-word-immediate(code-vector, new-new-index, end-index, external-table)
	      end;
	    values(make-proper-opcode("imul", vector(register-to-arg(reg), arg, imm-to-imm-arg(imm))), new-new-new-index)
	  end;
	#x6a =>
	  begin
	    let (imm, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
	    values(make-proper-opcode("push", vector(imm-to-imm-arg(imm))), new-new-index);
	  end;
	#x6b =>
	  begin
	    let (reg, arg, new-new-index) = decode-rm-arg-to-reg(new-index, #f, $word-arg-size);
	    let (imm, new-new-new-index) = get-byte-immediate(code-vector, new-new-index, end-index);
	    values(make-proper-opcode("imul", vector(register-to-arg(reg), arg, imm-to-imm-arg(imm))), new-new-new-index)
	  end;
	#x6c => simple-opcode("insb");
	#x6d => simple-opcode(if (is-16-bit-operands.is-16-bit) "insw" else "insd" end);
	#x6e => simple-opcode("outsb");
	#x6f => simple-opcode(if (is-16-bit-operands.is-16-bit) "outsw" else "outsd" end);
	#x70, #x71, #x72, #x73, #x74, #x75, #x76, #x77, #x78, #x79, #x7a, #x7b, #x7c, #x7d, #x7e, #x7f =>
	  begin
	    let name = concatenate("j", decode-condition(first-byte));
	    let (offset, new-new-index) = get-byte-offset(code-vector, new-index, end-index);
	    values(make-proper-opcode(name, vector(offset-to-arg(offset))), new-new-index);
	  end;
        #x80, #x81, #x83
          => decode-complex-add-type-opcode(new-index, first-byte);
	#x82 => // ???
	  error(make(<disassembly-unexpected-byte-error>, position: new-index));
	#x84, #x85 =>
	  begin
	    let (is-8-bit, arg-size) =
	      if (first-byte == #x84) values(#t, $byte-arg-size)
	      else
		values(#f, $word-arg-size)
	      end;
	    let (vec, new-new-index) = decode-rm-arg-to-rev-vector(new-index, is-8-bit, arg-size);
	    values(make-proper-opcode("test", vec), new-new-index)
	  end;
	#x86, #x87 =>
	  begin
	    let (is-8-bit, arg-size) =
	      if (first-byte == #x86) values(#t, $byte-arg-size)
	      else
		values(#f, $word-arg-size)
	      end;
	    let (vec, new-new-index) = decode-rm-arg-to-vector(new-index, is-8-bit, arg-size);
	    values(make-proper-opcode("xchg", vec), new-new-index)
	  end;
	#x88, #x89, #x8a, #x8b =>
	  begin
	    let (is-8-bit, arg-size) =
	      if (first-byte == #x88 | first-byte = #x8a) values(#t, $byte-arg-size)
	      else
		values(#f, $word-arg-size)
	      end;
	    let (vec, new-new-index) =
	      if (first-byte == #x8a | first-byte == #x8b)
		decode-rm-arg-to-vector(new-index, is-8-bit, arg-size)
	      else
		decode-rm-arg-to-rev-vector(new-index, is-8-bit, arg-size)
	      end;
	    values(make-proper-opcode("mov", vec), new-new-index)
	  end;
	#x8d =>
	  begin
	    let (vec, new-new-index) = decode-rm-arg-to-vector(new-index, #f, $word-arg-size);
	    values(make-proper-opcode("lea", vec), new-new-index)
	  end;
	#x8f =>
	  begin
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, #f, $word-arg-size);
	    if (int == 0)
	      values(make-proper-opcode("pop", vector(arg)), new-new-index)
	    else
	      error(make(<disassembly-unexpected-byte-error>, position: new-index))
	    end
	  end;
        #x90 => values($nop, new-index);
	#x91, #x92, #x93, #x94, #x95, #x96, #x97 =>
	  begin
	    let implicit-arg = if (is-16-bit-operands.is-16-bit) $ax-register-arg else $eax-register-arg end;
	    let reg-arg = register-to-arg(integer-to-register(logand(first-byte, 7), if (is-16-bit-operands.is-16-bit) 2 else 4 end));
	    values(make-proper-opcode("xchg", vector(implicit-arg, reg-arg)), new-index)
	  end;
        #x98 => simple-opcode(if (is-16-bit-operands.is-16-bit) "cbw" else "cwde" end);
        #x99 => simple-opcode(if (is-16-bit-operands.is-16-bit) "cwd" else "cdq" end);
        #x9b => simple-opcode("wait");
        #x9c => simple-opcode(if (is-16-bit-operands.is-16-bit) "pushf" else "pushfd" end);
        #x9d => simple-opcode(if (is-16-bit-operands.is-16-bit) "popf" else "popfd" end);
        #x9e => simple-opcode("sahf");
        #x9f => simple-opcode("lahf");
        #xa4 => simple-opcode("movsb");
        #xa5 => simple-opcode(if (is-16-bit-operands.is-16-bit) "movsw" else "movsd" end);
	#xa6 => simple-opcode("cmpsb");
	#xa7 => simple-opcode(if (is-16-bit-operands.is-16-bit) "cmpsw" else "cmpsd" end);
	#xa8 =>
	  begin
	    let (imm, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
	    values(make-proper-opcode("test", vector($al-register-arg, imm-to-imm-arg(imm))), new-new-index);
	  end;
	#xa9 =>
	  begin
	    let (arg-reg, imm, new-new-index) =
	      if (is-16-bit-operands.is-16-bit)
		let (imm, new-new-index) = get-short-immediate(code-vector, new-index, end-index);
		values($ax-register-arg, imm, new-new-index)
	      else
		let (imm, new-new-index) = get-word-immediate(code-vector, new-index, end-index, external-table);
		values($eax-register-arg, imm, new-new-index)
	      end;
	    values(make-proper-opcode("test", vector(arg-reg, imm-to-imm-arg(imm))), new-new-index);
	  end;
        #xaa => simple-opcode("stosb");
        #xab => simple-opcode(if (is-16-bit-operands.is-16-bit) "stosw" else "stosd" end);
	#xac => simple-opcode("lodsb");
        #xad => simple-opcode(if (is-16-bit-operands.is-16-bit) "lodsw" else "lodsd" end);
        #xae => simple-opcode("scasb");
        #xaf => simple-opcode(if (is-16-bit-operands.is-16-bit) "scasw" else "scasd" end);
	#xb0, #xb1, #xb2, #xb3, #xb4, #xb5, #xb6, #xb7 =>
	  begin
	    let reg-arg = register-to-arg(integer-to-register(logand(first-byte, 7), 1));
	    let (imm, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
	    values(make-proper-opcode("mov", vector(reg-arg, imm-to-imm-arg(imm))), new-new-index);
	  end;
	#xb8, #xb9, #xba, #xbb, #xbc, #xbd, #xbe, #xbf =>
	  begin
	    let size = if (is-16-bit-operands.is-16-bit) 2 else 4 end;
	    let reg-arg = register-to-arg(integer-to-register(logand(first-byte, 7), size));
	    let (imm, new-new-index) =
	      if (is-16-bit-operands.is-16-bit)
		get-short-immediate(code-vector, new-index, end-index)
	      else
		get-word-immediate(code-vector, new-index, end-index, external-table)
	      end;
	    values(make-proper-opcode("mov", vector(reg-arg, imm-to-imm-arg(imm))), new-new-index);
	    
	  end;
	#xc0, #xc1 =>
	  begin
	    let (is-8-bit, arg-size) =
	      if (first-byte == #xc0) values(#t, $byte-arg-size)
	      else
		values(#f, $word-arg-size)
	      end;
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, is-8-bit, arg-size);
	    let (imm, new-new-new-index) = get-byte-immediate(code-vector, new-new-index, end-index);
	    let name = select(int)
	      0 => "rol";
	      1 => "ror";
	      2 => "rcl";
	      3 => "rcr";
	      4 => "sal";
	      5 => "shr";
	      6 => "sll";
	      7 => "sar";
	    end select;
	    values(make-proper-opcode(name, vector(arg, imm-to-imm-arg(imm))), new-new-new-index);
	  end;
	#xc2, #xca =>
	  begin
	    let (imm, new-new-index) = get-short-immediate(code-vector, new-index, end-index);
	    values(make-proper-opcode("ret", vector(imm-to-imm-arg(imm))), new-new-index);
	  end;
        #xc3, #xcb => simple-opcode("ret");
	#xc4, #xc5 =>
	  begin
	    let name = select(first-byte)
	      #xc4 => "les";
	      #xc5 => "lds";
	      otherwise error("Unknown byte 0x%x for lgs type opcode\n", first-byte);
	    end select;
	    let (vec, new-index) = decode-rm-arg-to-vector(index + 1, #f, $word-arg-size);
	    values(make-proper-opcode(name, vec), new-index);
	  end;
	#xc6 =>
	  begin
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, #t, $byte-arg-size);
	    if (int ~== 0)
	      error(make(<disassembly-unexpected-byte-error>, position: new-new-index));
	    else
	      let (imm-byte, new-new-new-index) = get-byte-immediate(code-vector, new-new-index, end-index);
	      values(make-proper-opcode("mov", vector(arg, imm-to-imm-arg(imm-byte))), new-new-new-index)
	    end
	  end;
	#xc7 =>
	  begin
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, #f, $word-arg-size);
	    if (int ~== 0)
	      error(make(<disassembly-unexpected-byte-error>, position: new-new-index));
	    else
	      let (imm, new-new-new-index) =
		if (is-16-bit-operands.is-16-bit)
		  get-short-immediate(code-vector, new-new-index, end-index)
		else
		  get-word-immediate(code-vector, new-new-index, end-index, external-table)
		end;
	      values(make-proper-opcode("mov", vector(arg, imm-to-imm-arg(imm))), new-new-new-index)
	    end
	  end;
	#xc8 =>
	  begin
	    let (imm-short, new-new-index) = get-short-immediate(code-vector, new-index, end-index);
	    let (imm-byte, new-new-new-index) = get-byte-immediate(code-vector, new-new-index, end-index);
	    values(make-proper-opcode("enter", vector(imm-to-imm-arg(imm-short), imm-to-imm-arg(imm-byte))), new-new-new-index)
	  end;
        #xc9 => simple-opcode("leave");
	#xcc => simple-opcode("int 3");
	#xcd =>
	  begin
	    let (imm, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
	    values(make-proper-opcode("int", vector(imm-to-imm-arg(imm))), new-new-index)
	  end;
	#xce => simple-opcode("into");
	#xcf => simple-opcode("iret");
	#xd0, #xd1, #xd2, #xd3 =>
	  begin
	    let (is-8-bit, arg-size) =
	      if (first-byte == #xd0 | first-byte = #xd2) values(#t, $byte-arg-size)
	      else
		values(#f, $word-arg-size)
	      end;
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, is-8-bit, arg-size);
	    let name = select(int)
	      0 => "rol";
	      1 => "ror";
	      2 => "rcl";
	      3 => "rcr";
	      4 => "sal";
	      5 => "shr";
	      6 => "sll";
	      7 => "sar";
	    end select;
	    let implicit-arg =
	      if (first-byte == #xd0 | first-byte == #xd1)
		integer-to-imm(1)
	      else
		$cl-register-arg
	      end;
	    values(make-proper-opcode(name, vector(arg, implicit-arg)), new-new-index)
	  end;
	#xd4 => simple-two-byte-opcode("aam", #x0a);
	#xd5 => simple-two-byte-opcode("aad", #x0a);
	#xd6 => // ???
	  error(make(<disassembly-unexpected-byte-error>, position: new-index));
        #xd7 => simple-opcode("xlatb");
	#xd8 =>
	  begin
	    if (new-index < end-index)
	      let byte = code-vector[new-index];
	      let slash-value = logand(ash(byte, -3), 7);
	      let mod-value = logand(ash(byte, -6), 3);
	      if (mod-value == 3)
		let rem = logand(byte, #x3f);
		let stack-index = logand(rem, 7);
		values(make-proper-opcode(fp-fun-name1(ash(rem, -3)), vector($stack-top, integer-to-fp-register-arg(stack-index))), new-index + 1)
	      else
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-real-arg-size);
		values(make-proper-opcode(fp-fun-name1(int), vector(arg)), new-new-index)
	      end;
	    else
	      error(make(<disassembly-no-more-bytes-error>, position: new-index))
	    end
	  end;
	#xd9 =>
	  begin
	    if (new-index < end-index)
	      let byte = code-vector[new-index];
	      let slash-value = logand(ash(byte, -3), 7);
	      let mod-value = logand(ash(byte, -6), 3);
	      if (mod-value == 3)
		select(byte)
		  #xc0, #xc1, #xc2, #xc3, #xc4, #xc5, #xc6, #xc7 =>
		    values(make-proper-opcode("fld", vector(integer-to-fp-register-arg(logand(byte, 7)))), new-index + 1);
		  #xc8, #xc9, #xca, #xcb, #xcc, #xcd, #xce, #xcf =>
		    values(make-proper-opcode("fxch", vector(integer-to-fp-register-arg(logand(byte, 7)))), new-index + 1);
		  #xd0 => simple-opcode-with-index("fnop", new-index + 1);
		  #xe0 => simple-opcode-with-index("fchs", new-index + 1);
		  #xe4 => simple-opcode-with-index("ftst", new-index + 1);
		  #xe5 => simple-opcode-with-index("fxam", new-index + 1);
		  #xe1 => simple-opcode-with-index("fabs", new-index + 1);
		  #xe8 => simple-opcode-with-index("fld1", new-index + 1);
		  #xe9 => simple-opcode-with-index("fldl2t", new-index + 1);
		  #xea => simple-opcode-with-index("fldl2e", new-index + 1);
		  #xeb => simple-opcode-with-index("fldpi", new-index + 1);
		  #xec => simple-opcode-with-index("fldlg2", new-index + 1);
		  #xed => simple-opcode-with-index("fldln2", new-index + 1);
		  #xee => simple-opcode-with-index("fldz", new-index + 1);
		  #xf0 => simple-opcode-with-index("f2xm1", new-index + 1);
		  #xf1 => simple-opcode-with-index("fyl2x", new-index + 1);
		  #xf2 => simple-opcode-with-index("fptan", new-index + 1);
		  #xf3 => simple-opcode-with-index("fpatan", new-index + 1);
		  #xf4 => simple-opcode-with-index("fxtract", new-index + 1);
		  #xf5 => simple-opcode-with-index("fprem1", new-index + 1);
		  #xf6 => simple-opcode-with-index("fdecstp", new-index + 1);
		  #xf7 => simple-opcode-with-index("fincstp", new-index + 1);
		  #xf8 => simple-opcode-with-index("fprem", new-index + 1);
		  #xf9 => simple-opcode-with-index("fyl2xp1", new-index + 1);
		  #xfa => simple-opcode-with-index("fsqrt", new-index + 1);
		  #xfb => simple-opcode-with-index("fsincos", new-index + 1);
		  #xfc => simple-opcode-with-index("frndint", new-index + 1);
		  #xfd => simple-opcode-with-index("fscale", new-index + 1);
		  #xfe => simple-opcode-with-index("fsin", new-index + 1);
		  #xff => simple-opcode-with-index("fcos", new-index + 1);
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index));
		end select
	      elseif (slash-value == 0)
		// FLD m32real
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-real-arg-size);
		if (int == 0)
		  values(make-proper-opcode("fld", vector(arg)), new-new-index)
		else
		  error("bad byte %d gives %d for fld\n", byte, int)
		end
	      elseif (slash-value == 2)
		// FST m32real
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-real-arg-size);
		if (int == 2)
		  values(make-proper-opcode("fst", vector(arg)), new-new-index)
		else
		  error("bad byte %d gives %d for fstp\n", byte, int)
		end
	      elseif (slash-value == 3)
		// FSTP m32real
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-real-arg-size);
		if (int == 3)
		  values(make-proper-opcode("fstp", vector(arg)), new-new-index)
		else
		  error("bad byte %d gives %d for fstp\n", byte, int)
		end
	      elseif (slash-value == 4)
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-real-arg-size);
		if (int == 4)
		  values(make-proper-opcode("fldenv", vector(arg)), new-new-index)
		else
		  error("bad byte %d gives %d for fldenv\n", byte, int)
		end
	      elseif (slash-value == 5)
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-real-arg-size);
		if (int == 5)
		  values(make-proper-opcode("fnldcw", vector(arg)), new-new-index)
		else
		  error("bad byte %d gives %d for fnldcw\n", byte, int)
		end
	      elseif (slash-value == 6)
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-real-arg-size);
		if (int == 6)
		  values(make-proper-opcode("fnstcw", vector(arg)), new-new-index)
		else
		  error("bad byte %d gives %d for fnstcw\n", byte, int)
		end
	      elseif (slash-value == 7)
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-real-arg-size);
		if (int == 7)
		  values(make-proper-opcode("fnstenv", vector(arg)), new-new-index)
		else
		  error("bad byte %d gives %d for fnstenv\n", byte, int)
		end
	      else
		error(make(<disassembly-unexpected-byte-error>, position: new-index + 1))
	      end
	    else
	      error(make(<disassembly-no-more-bytes-error>, position: new-index))
	    end
	  end;
	#xda =>
	  begin
	    if (new-index < end-index)
	      let byte = code-vector[new-index];
	      let slash-value = logand(ash(byte, -3), 7);
	      let mod-value = logand(ash(byte, -6), 3);
	      if (mod-value == 3)
		let rem = logand(byte, #x3f);
		if (byte == #xe9)
		  values(make-proper-opcode("fucompp", $empty-vector), new-index + 1)
		else
		  error(make(<disassembly-unexpected-byte-error>, position: new-index))
		end
	      else
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $word-arg-size);
		values(make-proper-opcode(fp-fun-name2(int), vector(arg)), new-new-index)
	      end;
	    else
	      error(make(<disassembly-no-more-bytes-error>, position: new-index))
	    end
	  end;
	#xdb =>
	  begin
	    if (new-index < end-index)
	      let byte = code-vector[new-index];
	      let slash-value = logand(ash(byte, -3), 7);
	      let mod-value = logand(ash(byte, -6), 3);
	      if (mod-value == 3)
		select (byte)
		  #xe2 => simple-opcode-with-index("fnclex", new-index + 1);
		  #xe3 => simple-opcode-with-index("fninit", new-index + 1);
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index))
		end select;
	      else
		let (name, arg-size) = select(slash-value)
		  0 => values("fild", $word-arg-size);
		  2 => values("fist", $word-arg-size);
		  3 => values("fistp", $word-arg-size);
		  5 => values("fld", $extended-real-arg-size);
		  7 => values("fstp", $extended-real-arg-size);
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index))
		end select;
		let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, #f, arg-size);
		values(make-proper-opcode(name, vector(arg)), new-new-index)
	      end
	    else
	      error(make(<disassembly-no-more-bytes-error>, position: new-index))
	    end
	  end;
	#xdc =>
	  begin
	    if (new-index < end-index)
	      let byte = code-vector[new-index];
	      let slash-value = logand(ash(byte, -3), 7);
	      let mod-value = logand(ash(byte, -6), 3);
	      if (mod-value == 3)
		let rem = logand(byte, #x3f);
		let stack-index = logand(rem, 7);
		values(make-proper-opcode(fp-fun-name3(ash(rem, -3), new-index + 1), vector(integer-to-fp-register-arg(stack-index), $stack-top)), new-index + 1)
	      else
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $double-word-real-arg-size);
		values(make-proper-opcode(fp-fun-name1(int), vector(arg)), new-new-index)
	      end;
	    else
	      error(make(<disassembly-no-more-bytes-error>, position: new-index))
	    end
	  end;
	#xdd =>
	  begin
	    if (new-index < end-index)
	      let byte = code-vector[new-index];
	      let slash-value = logand(ash(byte, -3), 7);
	      let mod-value = logand(ash(byte, -6), 3);
	      if (mod-value == 3)
		let stack-index = logand(byte, 7);
		let name = select(slash-value)
		  0 => "ffree";
		  2 => "fst";
		  3 => "fstp";
		  4 => "fucom";
		  5 => "fucomp";
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index))
		end select;
		values(make-proper-opcode(name, vector(integer-to-fp-register-arg(stack-index))), new-index + 1);
	      else
		let (int, arg, new-new-index) =
		  decode-rm-arg-to-integer(new-index, #f, $double-word-real-arg-size);
		let name = select(int)
		  0 => "fld";
		  2 => "fst";
		  3 => "fstp";
		  4 => "frstor";
		  6 => "fnsave";
		  7 => "fnstsw";
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index))
		end select;
		values(make-proper-opcode(name, vector(arg)), new-new-index)
	      end
	    else
	      error(make(<disassembly-no-more-bytes-error>, position: new-index))
	    end
	  end;
	#xde =>
	  begin
	    if (new-index < end-index)
	      let byte = code-vector[new-index];
	      if (byte == #xd9)
		simple-opcode-with-index("fcompp", new-index + 1)
	      else
	        let slash-value = logand(ash(byte, -3), 7);
	        let mod-value = logand(ash(byte, -6), 3);
	        if (mod-value == 3)
		  let rem = logand(byte, #x3f);
		  let stack-index = logand(rem, 7);
		  values(make-proper-opcode(fp-fun-name4(ash(rem, -3), new-index + 1), vector(integer-to-fp-register-arg(stack-index), $stack-top)), new-index + 1)
	        else
		  let (int, arg, new-new-index) =
		    decode-rm-arg-to-integer(new-index, #f, $short-arg-size);
		  values(make-proper-opcode(fp-fun-name2(int), vector(arg)), new-new-index)
	        end
	      end
	    else
	      error(make(<disassembly-no-more-bytes-error>, position: new-index))
	    end
	  end;
	#xdf =>
	  begin
	    if (new-index < end-index)
	      let byte = code-vector[new-index];
	      let slash-value = logand(ash(byte, -3), 7);
	      let mod-value = logand(ash(byte, -6), 3);
	      if (mod-value == 3)
		if (byte == #xe0)
		  values(make-proper-opcode("fnstsw", vector($ax-register-arg)), new-index + 1)
		else
		  error(make(<disassembly-unexpected-byte-error>, position: new-index));
		end
	      else
		let (name, arg-size) = select(slash-value)
		  0 => values("fild", $short-arg-size);
		  2 => values("fist", $short-arg-size);
		  3 => values("fistp", $short-arg-size);
		  4 => values("fbld", $extended-real-arg-size);
		  5 => values("fild", $double-word-arg-size);
		  6 => values("fbstp", $extended-real-arg-size);
		  7 => values("fistp", $double-word-arg-size);
		  otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index))
		end select;
		let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, #f, arg-size);
		values(make-proper-opcode(name, vector(arg)), new-new-index, arg-size)
	      end
	    else
	      error(make(<disassembly-no-more-bytes-error>, position: new-index))
	    end
	  end;
	#xe0, #xe1, #xe2 =>
	  begin
	    let name = select(first-byte)
	      #xe0 => "loop";
	      #xe1 => "loopz";
	      #xe2 => "loopnz";
	      otherwise error("Unknown first byte 0x%x in loop opcode\n", first-byte);
	    end select;
	    let (offset, new-new-index) = get-byte-offset(code-vector, new-index, end-index);
	    values(make-proper-opcode(name, vector(offset-to-arg(offset))), new-new-index);
	  end;
	#xe3 =>
	  begin
	    let (offset, new-new-index) = get-byte-offset(code-vector, new-index, end-index);
	    values(make-proper-opcode("jcxz", vector(offset-to-arg(offset))), new-new-index)
	  end;
	#xe4, #xe5, #xe6, #xe7 => 
	  begin
	    let (imm-byte, new-new-index) = get-byte-immediate(code-vector, new-index, end-index);
	    let imm-byte-arg = imm-to-imm-arg(imm-byte);
	    let (name, is-in) =
	      if (first-byte == #xe4 | first-byte == #xe5)
		values("in", #t)
	      else
		values("out", #f)
	      end;
	    let implicit-reg-arg =
	      if (first-byte == #xe4 | first-byte == #xe6)
		$al-register-arg
	      elseif (is-16-bit-operands.is-16-bit)
		$ax-register-arg
	      else
		$eax-register-arg
	      end;
	    let (arg1, arg2) =
	      if (is-in)
		values(implicit-reg-arg, imm-byte-arg)
	      else
		values(imm-byte-arg, implicit-reg-arg)
	      end;
	    values(make-proper-opcode(name, vector(arg1, arg2)), new-new-index)
	  end;
	#xe8 =>
	  begin
	    let (offset, new-new-index) =
	      if (is-16-bit-operands.is-16-bit)
		get-short-offset(code-vector, new-index, end-index)
	      else
		get-word-offset(code-vector, new-index, end-index, external-table)
	      end;
	    values(make-proper-opcode("call", vector(offset-to-arg(offset))), new-new-index);
	  end;
	#xe9 =>
	  begin
	    let (offset, new-new-index) =
	      if (is-16-bit-operands.is-16-bit)
		get-short-offset(code-vector, new-index, end-index)
	      else
		get-word-offset(code-vector, new-index, end-index, external-table)
	      end;
	    values(make-proper-opcode("jmp", vector(offset-to-arg(offset))), new-new-index);
	  end;
	#xeb =>
	  begin
	    let (offset, new-new-index) = get-byte-offset(code-vector, new-index, end-index);
	    values(make-proper-opcode("jmp", vector(offset-to-arg(offset))), new-new-index);
	  end;
	#xec, #xed, #xee, #xef =>
	  begin
	    let (name, is-in) =
	      if (first-byte == #xe4 | first-byte == #xe5)
		values("in", #t)
	      else
		values("out", #f)
	      end;
	    let implicit-reg-arg1 =
	      if (first-byte == #xec | first-byte == #xee)
		$al-register-arg
	      elseif (is-16-bit-operands.is-16-bit)
		$ax-register-arg
	      else
		$eax-register-arg
	      end;
	    let implicit-reg-arg2 = register-to-arg(integer-to-register(2, 2));
	    let (arg1, arg2) =
	      if (is-in)
		values(implicit-reg-arg1, implicit-reg-arg2)
	      else
		values(implicit-reg-arg2, implicit-reg-arg1)
	      end;
	    values(make-proper-opcode("in", vector(arg1, arg2)), new-index)
	  end;
        #xf0 => simple-opcode("lock");
	#xf1 => // ???
	  error(make(<disassembly-unexpected-byte-error>, position: new-index));
        #xf4 => simple-opcode("hlt");
        #xf5 => simple-opcode("cmc");
	#xf6, #xf7 => // div group
	  begin
	    let (is-8-bit, arg-size) =
	      if (first-byte == #xf6) values(#t, $byte-arg-size)
	      else
		values(#f, $word-arg-size)
	      end;
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, is-8-bit, arg-size);
	    let (name, is-div-type, is-test) = select(int)
	      0 => values("test", #f, #t);
	      2 => values("not", #f, #f);
	      3 => values("neg", #f, #f);
	      4 => values("mul", #t, #f);
	      5 => values("imul", #t, #f);
	      6 => values("div", #t, #f);
	      7 => values("idiv", #t, #f);
	      otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index));
	    end select;
	    let (vec, new-new-new-index) =
	      if (is-div-type)
		let implicit-arg =
		  if (is-8-bit)
		    $al-register-arg
		  elseif (is-16-bit-operands.is-16-bit)
		    $ax-register-arg
		  else
		    $eax-register-arg
		  end;
		values(vector(implicit-arg, arg), new-new-index)
	      elseif (is-test)
		let (imm, new-new-new-index) =
		  if (is-8-bit)
		    get-byte-immediate(code-vector, new-new-index, end-index)
		  elseif (is-16-bit-operands.is-16-bit)
		    get-short-immediate(code-vector, new-new-index, end-index)
		  else
		    get-word-immediate(code-vector, new-new-index, end-index, external-table)
		  end;
		values(vector(arg, imm-to-imm-arg(imm)), new-new-new-index)
	      else
		values(vector(arg), new-new-index)
	      end;
	    values(make-proper-opcode(name, vec), new-new-index);
	  end;
        #xf8 => simple-opcode("clc");
        #xf9 => simple-opcode("stc");
        #xfa => simple-opcode("cli");
        #xfb => simple-opcode("sti");
        #xfc => simple-opcode("cld");
        #xfd => simple-opcode("std");
	#xfe => // inc/dec r/m8
	  begin
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, #t, $byte-arg-size);
	    let name = select(int)
	      0 => "inc";
	      1 => "dec";
	      otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index));
	    end select;
	    values(make-proper-opcode(name, vector(arg)), new-new-index);
	  end;
	#xff => // inc/dec r/m32
	  begin
	    let (int, arg, new-new-index) = decode-rm-arg-to-integer(new-index, #f, $word-arg-size);
	    let name = select(int)
	      0 => "inc";
	      1 => "dec";
	      2 => "call";
	      4 => "jmp";
	      6 => "push";
	      otherwise error(make(<disassembly-unexpected-byte-error>, position: new-index));
	    end select;
	    values(make-proper-opcode(name, vector(arg)), new-new-index);
	  end;
//	#xf2, #xf3, #x26, #x2e, #x36, #x3e, #x64, #x65, #x66, #x67, #x8c, #x8e, #x9a, #xa0, #xa1, #xa2, #xa3, #xea
        otherwise values($unknown, index + 1);
      end select;
    else
      values($unknown, index);
    end
  exception (disassembly-failure :: <disassembly-failure>)
    select(disassembly-failure by instance?)
      <disassembly-unexpected-byte-error> => values($unknown, index + 1);
      otherwise
	values($unknown, disassembly-failure.position-in-code-vector);
    end select
  end block;
end method decode-opcode-without-prefixes;

define method decode-opcode-without-repeater-and-address-size-and-operand-size(code-vector :: <byte-vector>, index :: <integer>, end-index :: <integer>, external-table :: <simple-object-vector>, is-16-bit-operands :: <is-16-bit-operands>, is-16-bit-addressing :: <is-16-bit-addressing>, repeater :: <repeater>)
  => (opc :: <general-opcode>, int :: <integer>)
  if (index < end-index)
    let byte = code-vector[index];
    if (byte == #x2E | byte == #x36 | byte == #x3E | byte == #x26 | byte == #x64 | byte == #x65)
      decode-opcode-without-prefixes(code-vector, index + 1, end-index, external-table, integer-to-segment-override(byte), is-16-bit-operands, is-16-bit-addressing, repeater);
    else
      decode-opcode-without-prefixes(code-vector, index, end-index, external-table, make(<no-segment-override>), is-16-bit-operands, is-16-bit-addressing, repeater);
    end
  else
    values($unspecified-not-an-opcode, index);
  end
end method decode-opcode-without-repeater-and-address-size-and-operand-size;

define method decode-opcode-without-repeater-and-address-size(code-vector :: <byte-vector>, index :: <integer>, end-index :: <integer>, external-table :: <simple-object-vector>, is-16-bit-addressing :: <is-16-bit-addressing>, repeater :: <repeater>)
  => (opc :: <general-opcode>, int :: <integer>)
  if (index < end-index)
    let byte = code-vector[index];
    let operand-size = make(<is-16-bit-operands>);
    if (byte == #x66)
      operand-size.is-16-bit := #t;
      decode-opcode-without-repeater-and-address-size-and-operand-size(code-vector, index + 1, end-index, external-table, operand-size, is-16-bit-addressing, repeater);
    else
      operand-size.is-16-bit := #f;
      decode-opcode-without-repeater-and-address-size-and-operand-size(code-vector, index, end-index, external-table, operand-size, is-16-bit-addressing, repeater);
    end
  else
    values($unspecified-not-an-opcode, index);
  end
end method decode-opcode-without-repeater-and-address-size;

define method decode-opcode-without-repeater(code-vector :: <byte-vector>, index :: <integer>, end-index :: <integer>, external-table :: <simple-object-vector>, repeater :: <repeater>)
  => (opc :: <general-opcode>, int :: <integer>)
  if (index < end-index)
    let byte = code-vector[index];
    let addressing = make(<is-16-bit-addressing>);
    if (byte == #x67)
      addressing.is-16-bit := #t;
      decode-opcode-without-repeater-and-address-size(code-vector, index + 1, end-index, external-table, addressing, repeater);
    else
      addressing.is-16-bit := #f;
      decode-opcode-without-repeater-and-address-size(code-vector, index, end-index, external-table, addressing, repeater);
    end
  else
    values($unspecified-not-an-opcode, index);
  end
end method decode-opcode-without-repeater;

define method decode-opcode(code-vector :: <byte-vector>, index :: <integer>, end-index :: <integer>, external-table :: <simple-object-vector>)
  => (opc :: <general-opcode>, int :: <integer>)
  if (index < end-index)
    let byte = code-vector[index];
    if (byte == #xF2 | byte == #xF3)
      decode-opcode-without-repeater(code-vector, index + 1, end-index, external-table, make(<some-repeater>, repeater-value: byte))
    else
      decode-opcode-without-repeater(code-vector, index, end-index, external-table, make(<no-repeater>))
    end
  else
    values($unspecified-not-an-opcode, index)
  end
end method decode-opcode;

define method decode-to-opcode-and-offsets(code-vector :: <byte-vector>, index :: <integer>, end-index :: <integer>, external-table :: <simple-object-vector>)
  => (opc :: <general-opcode-and-offsets>, new-index :: <integer>)
  let (opc, new-index) = decode-opcode(code-vector, index, end-index, external-table);
  new-index := if (new-index == #f) index + 1 else new-index end; /* Compiler bug workaround */
  values(make(<general-opcode-and-offsets>, general-opcode-opcode: opc, general-opcode-offset: index, general-opcode-end-offset: new-index),
	 new-index);
end method decode-to-opcode-and-offsets;

define method decode-opcodes(code-vector :: <byte-vector>, start-index :: <integer>, end-index :: <integer>, #key external-table = $empty-vector)
  => (opcode-list :: <list>, new-index :: <integer>)
  let opcode-list = #();
  let new-index = start-index;
  let opcode = $nop-and-offset;
  let end-index = if (end-index < code-vector.size) end-index else code-vector.size end;
  while (new-index < end-index)
    let (new-opcode, new-new-index) = decode-to-opcode-and-offsets(code-vector, new-index, end-index, external-table);
    new-index := new-new-index;
    opcode := new-opcode;
    opcode-list := pair(opcode, opcode-list);
  end while;
  select (opcode.general-opcode-opcode by instance?)
    <proper-opcode> =>
      if (new-index < end-index)
	error("decode-opcodes failed with proper opcode and new-index %d and end-index %d\n",
	      new-index, end-index)
      else
	values(reverse(opcode-list), end-index)
      end;
    otherwise values(reverse(tail(opcode-list)), end-index);
  end select
end method decode-opcodes;
