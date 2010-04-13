Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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
     
     "type" => $%TYPE-token,
     "opaque" => $%OPAQUE-token,
     "union" => $%UNION-token,
     
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
     
     "x" => $%X-token,
     "blockaddress" => $%BLOCKADDRESS-token,

     "void" => $%VOID-token,
     "float" => $%FLOAT-token,
     "double" => $%DOUBLE-token,
     "x86_fp80" => $%X86_FP80-token,
     "fp128" => $%FP128-token,
     "ppc_fp128" => $%PPC_FP128-token,
     "label" => $%LABEL-token,
     "metadata" => $%METADATA-token,

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
     "unwind" => $%UNWIND-token,
     "unreachable" => $%UNREACHABLE-token,

     "alloca" => $%ALLOCA-token,
     "malloc" => $%MALLOC-token,
     "free" => $%FREE-token,
     "load" => $%LOAD-token,
     "store" => $%STORE-token,
     "getelementptr" => $%GETELEMENTPTR-token,

     "extractelement" => $%EXTRACTELEMENT-token,
     "insertelement" => $%INSERTELEMENT-token,
     "shufflevector" => $%SHUFFLEVECTOR-token,
     "getresult" => $%GETRESULT-token,
     "extractvalue" => $%EXTRACTVALUE-token,
     "insertvalue" => $%INSERTVALUE-token,
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
        '\\' =>
          values($%UPVAL-token, #f);

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
        lexer-0x-digit(characters, 'J');
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
          'J' =>
            let start = position(number, 'x') + 1;
            let token = if (number[0] == 's')
                          $%ESINT64VAL-token
                        else
                          $%EUINT64VAL-token
                        end if;
            let value = string-to-integer(number, start: start, base: 16);
            values(token, if (number[0] == '-') -value else value end);
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
      elseif (llvm-identifier-character?(ch))
        add!(characters, read-element(stream));
        lexer-identifier(characters);
      elseif (ch == ':')
        read-element(stream);
        values($%LABELSTR-token, as(<string>, characters))
      else
        let number = as(<string>, characters);
        values($%EUINT64VAL-token, string-to-integer(number))
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
        values($%FPVAL-token, 0.0d0); // FIXME
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
        values($%FPVAL-token, 0.0d0); // FIXME
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
      //elseif (ch == '\\')
      //  lexer-quote-backslash(characters, token-class)
      else
        add!(characters, ch);
        lexer-quote(characters, token-class)
      end if;
    end method,

    // method lexer-quote-backslash
    //     (characters :: <stretchy-object-vector>, token-class)
    //  => (token-class, token-value);
    //   let ch = read-element(stream, on-end-of-stream: #f);
    //   if (ch == '\\')
    //     add!(characters, ch);
    //     lexer-quote(characters, token-class);
    //   else
    //     error("No hex escapes yet please");
    //   end if;
    // end method;

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
      error("Not handling !");
    end method,

    method on-error (symbol, value, history) => ()
      error("syntax error at %= (%=)", symbol, value);
    end method;

  dynamic-bind (*llvm-parse-state* = make(<llvm-parse-state>, module: module))
    run-parser(#f, llvm-parser, lexer, on-error: on-error);
  end dynamic-bind;
end function;
