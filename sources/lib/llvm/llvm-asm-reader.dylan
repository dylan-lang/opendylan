Module:       llvm-asm-parser-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define table $llvm-keywords :: <string-table>
  = {
     "begin" => $%BEGIN-token,
     "end" => $%END-token,
     "true" => $%TRUE-token,
     "false" => $%FALSE-token,
     "declare" => $%DECLARE-token,
     "define" => $%DEFINE-token,
     "global" => $%GLOBAL-token,
     "constant" => $%CONSTANT-token,

     "private" => $%PRIVATE-token,
     "linker_private" => $%LINKER_PRIVATE-token,
     "linker_private_weak" => $%LINKER_PRIVATE_WEAK-token,
     "linker_private_weak_def_auto" => $%LINKER_PRIVATE_WEAK_DEF_AUTO-token,
     "internal" => $%INTERNAL-token,
     "available_externally" => $%AVAILABLE_EXTERNALLY-token,
     "linkonce" => $%LINKONCE-token,
     "linkonce_odr" => $%LINKONCE_ODR-token,
     "weak" => $%WEAK-token,
     "weak_odr" => $%WEAK_ODR-token,
     "appending" => $%APPENDING-token,
     "dllimport" => $%DLLIMPORT-token,
     "dllexport" => $%DLLEXPORT-token,
     "common" => $%COMMON-token,
     "default" => $%DEFAULT-token,
     "hidden" => $%HIDDEN-token,
     "protected" => $%PROTECTED-token,
     "extern_weak" => $%EXTERN_WEAK-token,
     "external" => $%EXTERNAL-token,
     "thread_local" => $%THREAD_LOCAL-token,
     "unnamed_addr" => $%UNNAMED_ADDR-token,
     "zeroinitializer" => $%ZEROINITIALIZER-token,
     "undef" => $%UNDEF-token,
     "null" => $%NULL-token,
     "to" => $%TO-token,
     "tail" => $%TAIL-token,
     "target" => $%TARGET-token,
     "triple" => $%TRIPLE-token,
     "deplibs" => $%DEPLIBS-token,
     "datalayout" => $%DATALAYOUT-token,

     "volatile" => $%VOLATILE-token,
     "atomic" => $%ATOMIC-token,
     "unordered" => $%UNORDERED-token,
     "monotonic" => $%MONOTONIC-token,
     "acquire" => $%ACQUIRE-token,
     "release" => $%RELEASE-token,
     "acq_rel" => $%ACQ_REL-token,
     "seq_cst" => $%SEQ_CST-token,
     "singlethread" => $%SINGLETHREAD-token,

     "nuw" => $%NUW-token,
     "nsw" => $%NSW-token,
     "exact" => $%EXACT-token,
     "inbounds" => $%INBOUNDS-token,
     "align" => $%ALIGN-token,
     "addrspace" => $%ADDRSPACE-token,
     "section" => $%SECTION-token,
     "alias" => $%ALIAS-token,
     "module" => $%MODULE-token,
     "asm" => $%ASM-token,
     "sideeffect" => $%SIDEEFFECT-token,
     "alignstack" => $%ALIGNSTACK-token,
     "gc" => $%GC-token,
     
     "ccc" => $%CCC-token,
     "fastcc" => $%FASTCC-token,
     "coldcc" => $%COLDCC-token,
     "x86_stdcallcc" => $%X86_stdcallcc-token,
     "x86_fastcallcc" => $%X86_fastcallcc-token,
     "arm_apcscc" => $%ARM_apcscc-token,
     "arm_aapcscc" => $%ARM_aapcscc-token,
     "arm_aapcs_vfpcc" => $%ARM_aapcs_vfpcc-token,
     "msp430_intrcc" => $%MSP430_INTRCC-token,
     "x86_thiscallcc" => $%X86_THISCALLCC-token,
     "ptx_kernel" => $%PTX_KERNELCC-token,
     "ptx_device" => $%PTX_DEVICECC-token,
     
     "cc" => $%CC-token,
     "c" => $%C-token,
     
     "signext" => $%SIGNEXT-token,
     "zeroext" => $%ZEROEXT-token,
     "inreg" => $%INREG-token,
     "sret" => $%SRET-token,
     "nounwind" => $%NOUNWIND-token,
     "noreturn" => $%NORETURN-token,
     "noalias" => $%NOALIAS-token,
     "nocapture" => $%NOCAPTURE-token,
     "byval" => $%BYVAL-token,
     "nest" => $%NEST-token,
     "readnone" => $%READNONE-token,
     "readonly" => $%READONLY-token,
     
     "inlinehint" => $%INLINEHINT-token,
     "noinline" => $%NOINLINE-token,
     "alwaysinline" => $%ALWAYSINLINE-token,
     "optsize" => $%OPTSIZE-token,
     "ssp" => $%SSP-token,
     "sspreq" => $%SSPREQ-token,
     "noredzone" => $%NOREDZONE-token,
     "noimplicitfloat" => $%NOIMPLICITFLOAT-token,
     "naked" => $%NAKED-token,
     "uwtable" => $%UWTABLE-token,
     "returns_twice" => $%RETURNS_TWICE-token,
     "nonlazybind" => $%NONLAZYBIND-token,
     
     "type" => $%TYPE-token,
     "opaque" => $%OPAQUE-token,
     
     "eq" => $%EQ-token,
     "ne" => $%NE-token,
     "slt" => $%SLT-token,
     "sgt" => $%SGT-token,
     "sle" => $%SLE-token,
     "sge" => $%SGE-token,
     "ult" => $%ULT-token,
     "ugt" => $%UGT-token,
     "ule" => $%ULE-token,
     "uge" => $%UGE-token,
     "oeq" => $%OEQ-token,
     "one" => $%ONE-token,
     "olt" => $%OLT-token,
     "ogt" => $%OGT-token,
     "ole" => $%OLE-token,
     "oge" => $%OGE-token,
     "ord" => $%ORD-token,
     "uno" => $%UNO-token,
     "ueq" => $%UEQ-token,
     "une" => $%UNE-token,

     "xchg" => $%XCHG-token,
     "nand" => $%NAND-token,
     "max" => $%MAX-token,
     "min" => $%MIN-TOKEN,
     "umax" => $%UMAX-token,
     "umin" => $%UMIN-token,
     
     "x" => $%X-token,
     "blockaddress" => $%BLOCKADDRESS-token,

     "personality" => $%PERSONALITY-token,
     "cleanup" => $%CLEANUP-token,
     "catch" => $%CATCH-token,
     "filter" => $%FILTER-token,

     "void" => $%VOID-token,
     "float" => $%FLOAT-token,
     "double" => $%DOUBLE-token,
     "x86_fp80" => $%X86_FP80-token,
     "fp128" => $%FP128-token,
     "ppc_fp128" => $%PPC_FP128-token,
     "label" => $%LABEL-token,
     "metadata" => $%METADATA-token,
     "x86_mmx" => $%X86_MMX-token,

     "add" => $%ADD-token,
     "fadd" => $%FADD-token,
     "sub" => $%SUB-token,
     "fsub" => $%FSUB-token,
     "mul" => $%MUL-token,
     "fmul" => $%FMUL-token,
     "udiv" => $%UDIV-token,
     "sdiv" => $%SDIV-token,
     "fdiv" => $%FDIV-token,
     "urem" => $%UREM-token,
     "srem" => $%SREM-token,
     "frem" => $%FREM-token,
     "shl" => $%SHL-token,
     "lshr" => $%LSHR-token,
     "ashr" => $%ASHR-token,
     "and" => $%AND-token,
     "or" => $%OR-token,
     "xor" => $%XOR-token,
     "icmp" => $%ICMP-token,
     "fcmp" => $%FCMP-token,

     "phi" => $%PHI-token,
     "call" => $%CALL-token,
     "trunc" => $%TRUNC-token,
     "zext" => $%ZEXT-token,
     "sext" => $%SEXT-token,
     "fptrunc" => $%FPTRUNC-token,
     "fpext" => $%FPEXT-token,
     "uitofp" => $%UITOFP-token,
     "sitofp" => $%SITOFP-token,
     "fptoui" => $%FPTOUI-token,
     "fptosi" => $%FPTOSI-token,
     "inttoptr" => $%INTTOPTR-token,
     "ptrtoint" => $%PTRTOINT-token,
     "bitcast" => $%BITCAST-token,
     "select" => $%SELECT-token,
     "va_arg" => $%VAARG-token,
     "ret" => $%RET-token,
     "br" => $%BR-token,
     "switch" => $%SWITCH-token,
     "invoke" => $%INVOKE-token,
     "indirectbr" => $%INDIRECTBR-token,
     "unreachable" => $%UNREACHABLE-token,

     "alloca" => $%ALLOCA-token,
     "load" => $%LOAD-token,
     "store" => $%STORE-token,
     "cmpxchg" => $%CMPXCHG-token,
     "atomicrmw" => $%ATOMICRMW-token,
     "fence" => $%FENCE-token,
     "getelementptr" => $%GETELEMENTPTR-token,

     "extractelement" => $%EXTRACTELEMENT-token,
     "insertelement" => $%INSERTELEMENT-token,
     "shufflevector" => $%SHUFFLEVECTOR-token,
     "getresult" => $%GETRESULT-token,
     "extractvalue" => $%EXTRACTVALUE-token,
     "insertvalue" => $%INSERTVALUE-token,
     "landingpad" => $%LANDINGPAD-token,
     "resume" => $%RESUME-token,
    };

define function llvm-identifier-character?
    (ch :: <character>) => (result? :: <boolean>)
  ('a' <= ch & ch <= 'z')
  | ('A' <= ch & ch <= 'Z')
  | ('0' <= ch & ch <= '9')
  | ch == '_'
  | ch == '-'
  | ch == '$'
  | ch == '.'  
end function;

define function llvm-asm-parse
    (module :: <llvm-module>, stream :: <stream>)
 => ();
  local
    method lexer() => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      select (ch)
        #f =>
          values($EOF-token, #f);
          
        ' ', '\t', '\r', '\n' =>
          lexer();

        'i' =>
          let characters = make(<stretchy-object-vector>);
          add!(characters, ch);
          lexer-i(characters);
          
        's', 'u' =>
          let characters = make(<stretchy-object-vector>);
          add!(characters, ch);
          lexer-s(characters);

        '0' =>
          let characters = make(<stretchy-object-vector>);
          add!(characters, ch);
          lexer-0(characters);
        
        '+' =>
          let characters = make(<stretchy-object-vector>);
          add!(characters, ch);
          lexer-+(characters);
          
        '@' =>
          lexer-@();
          
        '%' =>
          lexer-%();

        '"' =>
          let characters = make(<stretchy-object-vector>);
          lexer-quote(characters, $%STRINGCONSTANT-token);

        '.' =>
          lexer-dot();

        '$' =>
          let characters = make(<stretchy-object-vector>);
          add!(characters, ch);
          lexer-$(characters);
          
        ';' =>
          let line = read-line(stream, on-end-of-stream: #f);
          if (line)
            lexer()
          else
            $EOF-token
          end if;

        '!' =>
          lexer-metadata();

        '-' =>
          let characters = make(<stretchy-object-vector>);
          add!(characters, ch);
          lexer-negative(characters);
          
        '=' =>
          values($%EQUALS-token, #f);

        '[' =>
          values($%LBRACK-token, #f);
        ']' =>
          values($%RBRACK-token, #f);
        '(' =>
          values($%LPAREN-token, #f);
        ')' =>
          values($%RPAREN-token, #f);
        '{' =>
          values($%LBRACE-token, #f);
        '}' =>
          values($%RBRACE-token, #f);
        '<' =>
          values($%LANGLE-token, #f);
        '>' =>
          values($%RANGLE-token, #f);
          
        ',' =>
          values($%COMMA-token, #f);
        '*' =>
          values($%STAR-token, #f);

        otherwise =>
          if ('0' <= ch & ch <= '9')
            let characters = make(<stretchy-object-vector>);
            add!(characters, ch);
            lexer-digit(characters);
          elseif (llvm-identifier-character?(ch))
            let characters = make(<stretchy-object-vector>);
            add!(characters, ch);
            lexer-identifier(characters);
          else
            error("Unrecognized character '%c'", ch);
          end if;
      end select;
    end method,

    method lexer-i
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        add!(characters, read-element(stream));
        lexer-i(characters)
      elseif (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-identifier(characters)
      elseif (ch == ':')
        read-element(stream);
        values($%LABELSTR-token, as(<string>, characters))
      else
        let bits-string = as(<string>, characters);
        values($%INTTYPE-token, string-to-integer(bits-string, start: 1))
      end if;
    end method,

    method lexer-s
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch == '0')
        add!(characters, read-element(stream));
        lexer-0(characters)
      else
        lexer-identifier(characters)
      end if
    end method,

    method lexer-identifier
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-identifier(characters);
      elseif (ch == ':')
        read-element(stream);
        values($%LABELSTR-token, as(<string>, characters))
      else
        let identifier = as(<string>, characters);
        let token = element($llvm-keywords, identifier, default: #f);
        if (token)
          values(token, identifier)
        else
          error("Unrecognized keyword '%s'", identifier);
        end if
      end if
    end method,

    method lexer-0
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch == 'x')
        add!(characters, read-element(stream));
        lexer-0x(characters);
      else
        lexer-digit(characters)
      end;
    end method,

    method lexer-0x
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch == 'K' | ch == 'L' | ch == 'M')
        add!(characters, read-element(stream));
        lexer-0x-digit(characters, ch);
      elseif (('0' <= ch & ch <= '9')
                | ('a' <= ch & ch <= 'f')
                | ('A' <= ch & ch <= 'F'))
        add!(characters, read-element(stream));
        if (characters[0] = 's' | characters[0] = 'u')
          lexer-0x-digit(characters, 's');
        else
          lexer-0x-digit(characters, 'J');
        end if;
      elseif (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-identifier(characters);
      elseif (ch == ':')
        read-element(stream);
        values($%LABELSTR-token, as(<string>, characters))
      else
        error("Invalid 0x");
      end if;
    end method,

    method lexer-0x-digit
        (characters :: <stretchy-object-vector>, class :: <character>)
     => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (('0' <= ch & ch <= '9')
            | ('a' <= ch & ch <= 'f')
            | ('A' <= ch & ch <= 'F'))
        add!(characters, read-element(stream));
        lexer-0x-digit(characters, class);
      elseif (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-identifier(characters);
      elseif (ch == ':')
        read-element(stream);
        values($%LABELSTR-token, as(<string>, characters))
      else
        let number = as(<string>, characters);
        select (class)
          's' =>
            let start = position(number, 'x') + 1;
            let token = if (number[0] == 's')
                          $%ESINT64VAL-token
                        else
                          $%EUINT64VAL-token
                        end if;
            let value
              = generic-string-to-integer(number, start: start, base: 16);
            values(token,
                   if (number[0] == '-')
                     generic-negative(value)
                   else
                     value
                   end);
            
          'J' =>
            let start = position(number, 'x') + 1;
            let high
              = string-to-machine-word(number, start: start, end: start + 8);
            let low
              = string-to-machine-word(number, start: start + 8);
            values($%FPVAL-token, encode-double-float(low, high));
              
          otherwise =>
            error("Not handling 0x%c", class);
        end select;
      end if;
    end method,

    method lexer-digit
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        add!(characters, read-element(stream));
        lexer-digit(characters);
      elseif (ch == '.')
        add!(characters, read-element(stream));
        lexer-fractional(characters);
      elseif (llvm-identifier-character?(ch) & ch ~== 'x')
        add!(characters, read-element(stream));
        lexer-identifier(characters);
      elseif (ch == ':')
        read-element(stream);
        values($%LABELSTR-token, as(<string>, characters))
      else
        let number = as(<string>, characters);
        values($%EUINT64VAL-token, generic-string-to-integer(number))
      end if;
    end method,

    method lexer-fractional
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        add!(characters, read-element(stream));
        lexer-fractional(characters);
      elseif (ch == 'e')
        add!(characters, read-element(stream));
        lexer-exponent(characters);
      elseif (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-identifier(characters);
      elseif (ch == ':')
        read-element(stream);
        values($%LABELSTR-token, as(<string>, characters))
      else
        values($%FPVAL-token,
               atof(as(<string>, characters)));
      end if;
    end method,

    method lexer-exponent
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch == '+' | ch == '-')
        add!(characters, read-element(stream));
        lexer-exponent-sign(characters);
      elseif (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-identifier(characters);
      elseif (ch == ':')
        read-element(stream);
        values($%LABELSTR-token, as(<string>, characters));
      else
        error("incomplete floating-point number");
      end if;
    end method,

    method lexer-exponent-sign
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        add!(characters, read-element(stream));
        lexer-exponent-sign(characters);
      else
        values($%FPVAL-token,
               atof(as(<string>, characters)));
      end if;
    end method,

    method lexer-+
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      error("+-token");
    end method,
    
    method lexer-@
        () => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        let characters = make(<stretchy-object-vector>);
        add!(characters, ch);
        lexer-@-digit(characters);
      elseif (llvm-identifier-character?(ch))
        let characters = make(<stretchy-object-vector>);
        add!(characters, ch);
        lexer-@-identifier(characters);
      elseif (ch == '"')
        let characters = make(<stretchy-object-vector>);
        lexer-quote(characters, $%GLOBALVAR-token);
      else
        error("Illegal @-name at '%c'", ch);
      end if;
    end method,

    method lexer-@-digit
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        add!(characters, read-element(stream));
        lexer-@-digit(characters);
      else
        let number = as(<string>, characters);
        values($%GLOBALVAL_ID-token, string-to-integer(number));
      end if;
    end method,

    method lexer-@-identifier
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-@-identifier(characters);
      else
        values($%GLOBALVAR-token, as(<string>, characters))
      end if;
    end method,

    method lexer-%
        () => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        let characters = make(<stretchy-object-vector>);
        add!(characters, ch);
        lexer-%-digit(characters);
      elseif (llvm-identifier-character?(ch))
        let characters = make(<stretchy-object-vector>);
        add!(characters, ch);
        lexer-%-identifier(characters);
      elseif (ch == '"')
        let characters = make(<stretchy-object-vector>);
        lexer-quote(characters, $%LOCALVAR-token);
      else
        error("Illegal %%-name at '%c'", ch);
      end if;
    end method,

    method lexer-%-digit
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if ('0' <= ch & ch <= '9')
        add!(characters, read-element(stream));
        lexer-%-digit(characters);
      else
        let number = as(<string>, characters);
        values($%LOCALVAL_ID-token, string-to-integer(number));
      end if;
    end method,

    method lexer-%-identifier
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-%-identifier(characters);
      else
        values($%LOCALVAR-token, as(<string>, characters))
      end if;
    end method,

    method lexer-quote
        (characters :: <stretchy-object-vector>, token-class)
     => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if (~ch)
        error("End of file within quoted string");
      elseif (ch == '"')
        if (token-class == $%STRINGCONSTANT-token
              & peek(stream, on-end-of-stream: #f) == ':')
          read-element(stream);
          values($%LABELSTR-token, as(<string>, characters))
        else
          values(token-class, as(<string>, characters))
        end if;
      elseif (ch == '\\')
        lexer-quote-backslash(characters, token-class)
      else
        add!(characters, ch);
        lexer-quote(characters, token-class)
      end if;
    end method,
    
    method lexer-quote-backslash
        (characters :: <stretchy-object-vector>, token-class)
     => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if (ch == '\\')
        add!(characters, ch);
        lexer-quote(characters, token-class);
      elseif (('0' <= ch & ch <= '9')
                | ('a' <= ch & ch <= 'f')
                | ('A' <= ch & ch <= 'F'))
        lexer-quote-backslash-hex(characters, token-class, ch);
      else
        error("bad \\xx in quoted string");  
      end if;
    end method,
    
    method hex-digit-value (ch :: <character>) => (v :: <integer>);
      select (ch)
        '0'      => 0;
        '1'      => 1;
        '2'      => 2;
        '3'      => 3;
        '4'      => 4;
        '5'      => 5;
        '6'      => 6;
        '7'      => 7;
        '8'      => 8;
        '9'      => 9;
        'A', 'a' => 10;
        'B', 'b' => 11;
        'C', 'c' => 12;
        'D', 'd' => 13;
        'E', 'e' => 14;
        'F', 'f' => 15;
      end select
    end method,
    
    method lexer-quote-backslash-hex
        (characters :: <stretchy-object-vector>, token-class, first-digit)
     => (token-class, token-value);
      let ch = read-element(stream, on-end-of-stream: #f);
      if (('0' <= ch & ch <= '9')
            | ('a' <= ch & ch <= 'f')
            | ('A' <= ch & ch <= 'F'))
        let value = hex-digit-value(first-digit) * 16 + hex-digit-value(ch);
        add!(characters, as(<character>, value));
        lexer-quote(characters, token-class);
      else
        error("bad \\xx in quoted string");  
      end if;
    end method,
    
    method lexer-dot
        () => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch == '.')
        read-element(stream);
        lexer-dotdot();
      else
        let characters = make(<stretchy-object-vector>);
        add!(characters, '.');
        lexer-identifier(characters);
      end if;
    end method,

    method lexer-dotdot
        () => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch == '.')
        read-element(stream);
        values($%DOTDOTDOT-token, #f);
      else
        let characters = make(<stretchy-object-vector>);
        add!(characters, '.');
        add!(characters, '.');
        lexer-identifier(characters);
      end if;
    end method,

    method lexer-$
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      error("Not handling $-token");
    end method,

    method lexer-negative
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (ch == '0')
        add!(characters, read-element(stream));
        lexer-0(characters);
      elseif ('1' <= ch & ch <= '9')
        add!(characters, read-element(stream));
        lexer-digit(characters);
      else
        error("Invalid -token");
      end if;
    end method,

    method lexer-metadata
        () => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (('a' <= ch & ch <= 'z') | ('A' <= ch & ch <= 'Z'))
        let characters = make(<stretchy-object-vector>);
        add!(characters, read-element(stream));
        lexer-metadata-var(characters)
      else
        values($%EXCLAIM-token, #f)
      end if
    end method,

    method lexer-metadata-var
        (characters :: <stretchy-object-vector>) => (token-class, token-value);
      let ch = peek(stream, on-end-of-stream: #f);
      if (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-metadata-var(characters)
      else
        values($%METADATAVAR-token, as(<string>, characters))
      end if
    end method,

    method on-error (symbol, value, history) => ()
      error("syntax error at %= (%=)", symbol, value);
    end method;

  dynamic-bind (*llvm-parse-state* = make(<llvm-parse-state>, module: module))
    run-parser(#f, llvm-parser, lexer, on-error: on-error);
  end dynamic-bind;
end function;

define function generic-string-to-integer
    (string :: <string>,
     #key base :: <integer> = 10, 
          start :: <integer> = 0, 
          end: _end :: <integer> = size(string),
          default = $unsupplied)
 => (result :: <abstract-integer>, next-key :: <integer>);
  // Set initial state
  let valid? :: <boolean> = #f;
  let negative? :: <boolean> = #f;
  let integer :: <abstract-integer> = 0;
  
  block (return)
    for (i :: <integer> from start below _end)
      let char :: <character> = string[i];
      let digit :: false-or(<integer>)
        = select (char)
            '-' =>
              if (i = start)
                negative? := #t;
              elseif (valid?)
                return(if (negative?)
                         generic-negative(integer)
                       else
                         integer
                       end, i);
              elseif (supplied?(default))
                return(default, i);
              else
                error("not a valid integer");
              end if;
              #f;
            '+' =>
              if (i = start)
                negative? := #f;
              elseif (valid?)
                return(if (negative?) - integer else integer end, i);
              elseif (supplied?(default))
                return(default, i);
              else
                error("not a valid integer");
              end if;
              #f;
            '0'      => 0;
            '1'      => 1;
            '2'      => 2;
            '3'      => 3;
            '4'      => 4;
            '5'      => 5;
            '6'      => 6;
            '7'      => 7;
            '8'      => 8;
            '9'      => 9;
            'A', 'a' => 10;
            'B', 'b' => 11;
            'C', 'c' => 12;
            'D', 'd' => 13;
            'E', 'e' => 14;
            'F', 'f' => 15;
            'G', 'g' => 16;
            'H', 'h' => 17;
            'I', 'i' => 18;
            'J', 'j' => 19;
            'K', 'k' => 20;
            'L', 'l' => 21;
            'M', 'm' => 22;
            'N', 'n' => 23;
            'O', 'o' => 24;
            'P', 'p' => 25;
            'Q', 'q' => 26;
            'R', 'r' => 27;
            'S', 's' => 28;
            'T', 't' => 29;
            'U', 'u' => 30;
            'V', 'v' => 31;
            'W', 'w' => 32;
            'X', 'x' => 33;
            'Y', 'y' => 34;
            'Z', 'z' => 35;
            otherwise =>
              if (valid?)
                return(if (negative?)
                         generic-negative(integer)
                       else
                         integer
                       end, i);
              elseif (supplied?(default))
                return(default, i);
              else
                error("not a valid integer");
              end if;
            end select;
      if (digit)
        if (digit < base)
          integer := generic-+(generic-*(integer, base), digit);
          valid? := #t;
        elseif (valid?)
          return(if (negative?) generic-negative(integer) else integer end, i);
        elseif (supplied?(default))
          return(default, i);
        else
          error("not a valid integer");
        end if;
      end if;
    end for;

    if (valid?)
      values(if (negative?) generic-negative(integer) else integer end, _end);
    elseif (supplied?(default))
      return(default, _end);
    else
      error("not a valid integer");
    end if;
  end block;
end function generic-string-to-integer;

// This lame implementation is "borrowed" from dfmc-reader; it does a
// poor job of preserving precision, but at least it doesn't require
// bignums.
define constant $max-mantissa-digits = 18;

define method atof
    (string :: <byte-string>,
     #key start :: <integer> = 0,
          end: finish :: <integer> = string.size)
 => (value :: <float>);
  let class = #f;
  let posn = start;
  let sign = 1;
  let mantissa = 0;
  let scale = #f;
  let exponent-sign = 1;
  let exponent = 0;
  let exponent-shift = 0;
  let digits = 0;

  // Parse the optional sign.
  if (posn < finish)
    let char = string[posn];
    if (char == '-')
      posn := posn + 1;
      sign := -1;
    elseif (char == '+')
      posn := posn + 1;
    end if;
  end if;

  block (return)
    block (parse-exponent)
      // Parse the mantissa.
      while (posn < finish)
        let char = string[posn];
        posn := posn + 1;
        if (char >= '0' & char <= '9')
          if (digits < $max-mantissa-digits)
            let digit = as(<integer>, char) - as(<integer>, '0');
            mantissa := generic-+(generic-*(mantissa, 10), digit);
            if (scale)
              scale := generic-*(scale, 10);
            end if;
          else
            // If we're after the decimal point, we can just ignore
            // the digit. If before, we have to remember that we've
            // been multiplied.
            if (~scale)
              exponent-shift := generic-+(exponent-shift, 1);
            end;
          end;
          digits := digits + 1;
        elseif (char == '.')
          if (scale)
            error("bogus float.");
          end if;
          scale := 1;
        elseif (char == 'e' | char == 'E')
          parse-exponent();
        elseif (char == 'd' | char == 'D')
          class := #"double";
          parse-exponent();
        elseif (char == 's' | char == 'S')
          class := #"single";
          parse-exponent();
        elseif (char == 'x' | char == 'X')
          class := #"extended";
          parse-exponent();
        else
          error("bogus float.");
        end if;
      end while;
      return();
    end block;

    // Parse the exponent.
    if (posn < finish)
      let char = string[posn];
      if (char == '-')
        exponent-sign := -1;
        posn := posn + 1;
      elseif (char == '+')
        posn := posn + 1;
      end if;

      while (posn < finish)
        let char = string[posn];
        if (char >= '0' & char <= '9')
          let digit = as(<integer>, char) - as(<integer>, '0');
          exponent := generic-+(generic-*(exponent, 10), digit);
        else
          error("bogus float");
        end if;
        posn := posn + 1;
      end while;
    end if;
  end block;

  exponent := generic-+(exponent, exponent-shift);

  // TODO: CORRECTNESS: Decide how to maintain precision and representation,
  // since we lose it here. (CMU used a ratio representation).
  // TODO: CORRECTNESS: Handle overflows reasonably gracefully.
  // TODO: CORRECTNESS: Note that we don't have extended floats.

  let (mantissa, base, scale)
    = select (class)
        #f          => values(as(<double-float>, mantissa),
                              as(<double-float>, 10),
                              as(<double-float>, scale | 1));
        #"single"   => values(as(<single-float>, mantissa),
                              as(<single-float>, 10),
                              as(<single-float>, scale | 1));
        #"double"   => values(as(<double-float>, mantissa),
                              as(<double-float>, 10),
                              as(<double-float>, scale | 1));
        #"extended" => values(as(<extended-float>, mantissa),
                              as(<extended-float>, 10),
                              as(<extended-float>, scale | 1));
      end;

  let result
    = if (exponent = 0)
        generic-/(mantissa, scale)
      else
        let scaled-mantissa = generic-/(mantissa, scale);
        // NOTE: Floating point exponentiation loses precision for some
        // suprisingly small exponents so we'll use successive multiplications.
        //---*** NOTE: Revisit this as it may be costly w.r.t. consing and
        //---*** there must be a better way (rationals?).
        local
          method power-of-10 () => (power :: <float>);
            let iterate?
              = select (base by instance?)
                  <single-float> => exponent > 15;
                  // Yes, <double-float> exponentation is never accurate!
                  <double-float> => #t;
                  //---*** NOTE: We don't have <extended-float>s yet ...
                  <extended-float> => #t;
                end;
            if (iterate?)
              for (i from 1 below exponent)
                base := generic-*(base, 10.0)
              end;
              base
            else
              generic-^(base, exponent)
            end
          end method power-of-10;
        if (exponent-sign = 1)
          generic-*(scaled-mantissa, power-of-10())
        else
          generic-/(scaled-mantissa, power-of-10())
        end
      end;
  if (sign = -1)
    -result
  else
    result
  end if
end method atof;
