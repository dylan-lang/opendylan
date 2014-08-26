Module:       llvm-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Test support

define method test-value-function
    (builder :: <llvm-builder>, value :: <llvm-value>)
 => (result :: <llvm-value>);
  value
end method;

define method test-value-function
    (builder :: <llvm-builder>, value :: <integer>)
 => (result :: <llvm-value>);
  make(<llvm-integer-constant>, type: $llvm-i32-type, integer: value)
end method;

define method test-value-function
    (builder :: <llvm-builder>, value :: <single-float>)
 => (result :: <llvm-value>);
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  make(<llvm-float-constant>, type: float-type, float: value)
end method;

define method test-value-function
    (builder :: <llvm-builder>, value :: <double-float>)
 => (result :: <llvm-value>);
  let double-type = make(<llvm-primitive-type>, kind: #"DOUBLE");
  make(<llvm-float-constant>, type: double-type, float: value)
end method;

define sideways method make-test-instance
    (class :: subclass(<llvm-builder>))
 => (builder :: <llvm-builder>)
  make(<llvm-builder>, value-function: test-value-function)
end method make-test-instance;

define function make-builder-with-test-function
    (#key entry-block? = #t,
          return-type = $llvm-void-type,
          arg-type = #f,
          varargs? = #f)
 => (builder :: <llvm-builder>)
  let builder = make-test-instance(<llvm-builder>);
  let module = make-test-instance(<llvm-module>);
  builder.llvm-builder-module := module;

  let parameter-types
    = if (arg-type) vector(arg-type) else #[] end if;
  let arguments
    = map(method (arg-type, index) 
            make(<llvm-argument>,
                 type: arg-type,
                 name: format-to-string("arg%d", index),
                 index: index)
          end,
          parameter-types,
          range(below: parameter-types.size));
  let test-function-type
    = make(<llvm-function-type>,
           return-type: return-type,
           parameter-types: parameter-types,
           varargs?: varargs?);
  let test-function-pointer-type
    = make(<llvm-pointer-type>, pointee: test-function-type);
  let test-function
    = make(<llvm-function>,
           name: "test",
           type: test-function-pointer-type,
           arguments: arguments,
           linkage: #"external");
  llvm-builder-define-global(builder, "test", test-function);
  builder.llvm-builder-function := test-function;

  for (argument in arguments)
    ins--local(builder, argument.llvm-argument-name, argument);
  end for;

  if (entry-block?)
    ins--block(builder, make(<llvm-basic-block>, name: "entry"));
  end if;

  builder
end function;

define function builder-test-function-disassembly
    (builder :: <llvm-builder>)
 => (lines :: <sequence>);
  let bc-pathname = "out.bc";   // FIXME
  block ()
    llvm-save-bitcode-file(builder.llvm-builder-module, bc-pathname);
    with-application-output (stream = "llvm-dis", input: bc-pathname)
      iterate loop (lines = #())
        let line = read-line(stream, on-end-of-stream: #f);
        if (line)
          let disassembly = recognize-line(line);
          if (disassembly)
            loop(add(lines, disassembly))
          else
            loop(lines)
          end;
        else
          reverse!(lines)
        end if
      end iterate
    end with-application-output
  cleanup
    delete-file(bc-pathname);
  end block
end function;

define function recognize-line
    (line :: <string>) => (disassembly :: false-or(<string>));
  local
    method rec-0 () => (disassembly :: false-or(<string>));
      select (line[0])
        ' ' => rec-1();
        otherwise => rec-label(1);
      end select
    end,
    method rec-1 () => (disassembly :: false-or(<string>));
      select (line[1])
        ' ' => rec-disassembly(2);
        otherwise => #f;
      end select
    end,
    method rec-disassembly
        (index :: <integer>) => (disassembly :: false-or(<string>));
      if (index < line.size)
        select (line[index])
          ' ' => rec-disassembly-space(index, index + 1);
          ';' => copy-sequence(line, start: 2, end: index);
          otherwise => rec-disassembly(index + 1);
        end select
      else
        copy-sequence(line, start: 2)
      end if
    end,
    method rec-disassembly-space
        (dend :: <integer>, index :: <integer>)
     => (disassembly :: false-or(<string>));
      if (index < line.size)
        select (line[index])
          ' ' => rec-disassembly-space(dend, index + 1);
          ';' => copy-sequence(line, start: 2, end: dend);
          otherwise => rec-disassembly(index + 1);
        end select
      else
        copy-sequence(line, start: 2, end: dend)
      end if
    end,
    method rec-label
        (index :: <integer>) => (disassembly :: false-or(<string>));
      if (index < line.size)
        select (line[index])
          ' ' => #f;
          ':' => copy-sequence(line, end: index + 1);
          otherwise => rec-label(index + 1);
        end select
      else
        #f
      end if
    end;
  ~empty?(line) & rec-0();
end function;


/// llvm-builder tests

define llvm-builder class-test <llvm-builder> ()
  //---*** Fill this in...
end class-test <llvm-builder>;

define llvm-builder function-test llvm-builder-basic-block ()
  //---*** Fill this in...
end function-test llvm-builder-basic-block;

define llvm-builder function-test llvm-builder-basic-block-setter ()
  //---*** Fill this in...
end function-test llvm-builder-basic-block-setter;

define llvm-builder function-test llvm-builder-function ()
  //---*** Fill this in...
end function-test llvm-builder-function;

define llvm-builder function-test llvm-builder-function-setter ()
  //---*** Fill this in...
end function-test llvm-builder-function-setter;

define llvm-builder function-test llvm-builder-module ()
  //---*** Fill this in...
end function-test llvm-builder-module;

define llvm-builder function-test llvm-builder-module-setter ()
  //---*** Fill this in...
end function-test llvm-builder-module-setter;

define llvm-builder function-test llvm-builder-define-global ()
  //---*** Fill this in...
end function-test llvm-builder-define-global;

define llvm-builder function-test llvm-builder-declare-global ()
  //---*** Fill this in...
end function-test llvm-builder-declare-global;

define llvm-builder function-test llvm-builder-global ()
  //---*** Fill this in...
end function-test llvm-builder-global;

define llvm-builder constant-test <llvm-local-value> ()
  //---*** Fill this in...
end constant-test <llvm-local-value>;

define llvm-builder function-test ins--local ()
  //---*** Fill this in...
end function-test ins--local;

define llvm-builder function-test llvm-builder-local ()
  //---*** Fill this in...
end function-test llvm-builder-local;

define llvm-builder function-test ins--block ()
  //---*** Fill this in...
end function-test ins--block;

define llvm-builder function-test ins--dbg ()
  let builder = make-builder-with-test-function();

  let dbg-compile-unit
    = llvm-make-dbg-compile-unit($DW-LANG-C99, "test.c", ".", "test");
  let dbg-file
    = llvm-make-dbg-file("test.c", ".");
  let dbg-function-type
    = llvm-make-dbg-function-type(dbg-file, #f, #[]);
  let dbg-function
    = llvm-make-dbg-function(dbg-file,
                             builder.llvm-builder-function.llvm-global-name,
                             builder.llvm-builder-function.llvm-global-name,
                             dbg-file,
                             12,
                             dbg-function-type,
                             definition?: #t,
                             function: builder.llvm-builder-function,
                             module: builder.llvm-builder-module);
  let dbg-lexical-block
    = llvm-make-dbg-lexical-block(dbg-function, dbg-file, 14, 0);
  ins--dbg(builder, 14, 23, dbg-lexical-block, #f);
  ins--add(builder, 1111, 2222);
  ins--ret(builder);
  check-equal("ins--dbg disassembly with metadata",
              #("entry:",
                "%0 = add i32 1111, 2222, !dbg !5",
                "ret void, !dbg !5"),
              builder-test-function-disassembly(builder));
end function-test ins--dbg;

define llvm-builder function-test ins--add ()
  let builder = make-builder-with-test-function();
  ins--add(builder, 1111, 2222);
  ins--ret(builder);
  check-equal("ins--add disassembly",
              #("entry:", "%0 = add i32 1111, 2222", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--add;

define llvm-builder function-test ins--fadd ()
  let builder = make-builder-with-test-function();
  ins--fadd(builder, 1.0s0, 2.0s0);
  ins--ret(builder);
  check-equal("ins--fadd disassembly",
              #("entry:",
                "%0 = fadd float 1.000000e+00, 2.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fadd;

define llvm-builder function-test ins--sub ()
  let builder = make-builder-with-test-function();
  ins--sub(builder, 1111, 2222);
  ins--ret(builder);
  check-equal("ins--sub disassembly",
              #("entry:", "%0 = sub i32 1111, 2222", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--sub;

define llvm-builder function-test ins--fsub ()
  let builder = make-builder-with-test-function();
  ins--fsub(builder, 1.0s0, 2.0s0);
  ins--ret(builder);
  check-equal("ins--fsub disassembly",
              #("entry:",
                "%0 = fsub float 1.000000e+00, 2.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fsub;

define llvm-builder function-test ins--mul ()
  let builder = make-builder-with-test-function();
  ins--mul(builder, 1111, 2222);
  ins--ret(builder);
  check-equal("ins--mul disassembly",
              #("entry:", "%0 = mul i32 1111, 2222", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--mul;

define llvm-builder function-test ins--fmul ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  ins--fmul(builder,
            1.0s0,
            2.0s0);
  ins--ret(builder);
  check-equal("ins--fmul disassembly",
              #("entry:",
                "%0 = fmul float 1.000000e+00, 2.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fmul;

define llvm-builder function-test ins--udiv ()
  let builder = make-builder-with-test-function();
  ins--udiv(builder,
            222,
            111);
  ins--ret(builder);
  check-equal("ins--udiv disassembly",
              #("entry:", "%0 = udiv i32 222, 111", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--udiv;

define llvm-builder function-test ins--sdiv ()
  let builder = make-builder-with-test-function();
  ins--sdiv(builder, 222, 111);
  ins--ret(builder);
  check-equal("ins--sdiv disassembly",
              #("entry:", "%0 = sdiv i32 222, 111", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--sdiv;

define llvm-builder function-test ins--fdiv ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  ins--fdiv(builder, 1.0s0, 2.0s0);
  ins--ret(builder);
  check-equal("ins--fdiv disassembly",
              #("entry:",
                "%0 = fdiv float 1.000000e+00, 2.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fdiv;

define llvm-builder function-test ins--urem ()
  let builder = make-builder-with-test-function();
  ins--urem(builder, 222, 111);
  ins--ret(builder);
  check-equal("ins--urem disassembly",
              #("entry:", "%0 = urem i32 222, 111", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--urem;

define llvm-builder function-test ins--srem ()
  let builder = make-builder-with-test-function();
  ins--srem(builder, 222, 111);
  ins--ret(builder);
  check-equal("ins--srem disassembly",
              #("entry:", "%0 = srem i32 222, 111", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--srem;

define llvm-builder function-test ins--frem ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  ins--frem(builder, 1.0s0, 2.0s0);
  ins--ret(builder);
  check-equal("ins--frem disassembly",
              #("entry:",
                "%0 = frem float 1.000000e+00, 2.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--frem;

define llvm-builder function-test ins--shl ()
  let builder = make-builder-with-test-function();
  ins--shl(builder, 111, 2);
  ins--ret(builder);
  check-equal("ins--shl disassembly",
              #("entry:", "%0 = shl i32 111, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--shl;

define llvm-builder function-test ins--lshr ()
  let builder = make-builder-with-test-function();
  ins--lshr(builder, 111, 2);
  ins--ret(builder);
  check-equal("ins--lshr disassembly",
              #("entry:", "%0 = lshr i32 111, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--lshr;

define llvm-builder function-test ins--ashr ()
  let builder = make-builder-with-test-function();
  ins--ashr(builder, 111, 2);
  ins--ret(builder);
  check-equal("ins--ashr disassembly",
              #("entry:", "%0 = ashr i32 111, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--ashr;

define llvm-builder function-test ins--and ()
  let builder = make-builder-with-test-function();
  ins--and(builder, 111, 2);
  ins--ret(builder);
  check-equal("ins--and disassembly",
              #("entry:", "%0 = and i32 111, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--and;

define llvm-builder function-test ins--or ()
  let builder = make-builder-with-test-function();
  ins--or(builder, 111, 2);
  ins--ret(builder);
  check-equal("ins--or disassembly",
              #("entry:", "%0 = or i32 111, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--or;

define llvm-builder function-test ins--xor ()
  let builder = make-builder-with-test-function();
  ins--xor(builder, 111, 2);
  ins--ret(builder);
  check-equal("ins--xor disassembly",
              #("entry:", "%0 = xor i32 111, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--xor;

define llvm-builder function-test ins--trunc ()
  let builder = make-builder-with-test-function();
  ins--trunc(builder, 111, $llvm-i8-type);
  ins--ret(builder);
  check-equal("ins--trunc disassembly",
              #("entry:", "%0 = trunc i32 111 to i8", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--trunc;

define llvm-builder function-test ins--zext ()
  let builder = make-builder-with-test-function();
  ins--zext(builder,
            make(<llvm-integer-constant>, type: $llvm-i8-type, integer: 111),
            $llvm-i32-type);
  ins--ret(builder);
  check-equal("ins--zext disassembly",
              #("entry:", "%0 = zext i8 111 to i32", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--zext;

define llvm-builder function-test ins--sext ()
  let builder = make-builder-with-test-function();
  ins--sext(builder,
            make(<llvm-integer-constant>, type: $llvm-i8-type, integer: 111),
            $llvm-i32-type);
  ins--ret(builder);
  check-equal("ins--sext disassembly",
              #("entry:", "%0 = sext i8 111 to i32", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--sext;

define llvm-builder function-test ins--fptoui ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  ins--fptoui(builder, 1.0s0, $llvm-i32-type);
  ins--ret(builder);
  check-equal("ins--fptoui disassembly",
              #("entry:",
                "%0 = fptoui float 1.000000e+00 to i32",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fptoui;

define llvm-builder function-test ins--fptosi ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  ins--fptosi(builder, 1.0s0, $llvm-i32-type);
  ins--ret(builder);
  check-equal("ins--fptosi disassembly",
              #("entry:",
                "%0 = fptosi float 1.000000e+00 to i32",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fptosi;

define llvm-builder function-test ins--uitofp ()
  let builder = make-builder-with-test-function();
  ins--uitofp(builder, 111, make(<llvm-primitive-type>, kind: #"FLOAT"));
  ins--ret(builder);
  check-equal("ins--uitofp disassembly",
              #("entry:", "%0 = uitofp i32 111 to float", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--uitofp;

define llvm-builder function-test ins--sitofp ()
  let builder = make-builder-with-test-function();
  ins--sitofp(builder, 111, make(<llvm-primitive-type>, kind: #"FLOAT"));
  ins--ret(builder);
  check-equal("ins--sitofp disassembly",
              #("entry:", "%0 = sitofp i32 111 to float", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--sitofp;

define llvm-builder function-test ins--fptrunc ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let double-type = make(<llvm-primitive-type>, kind: #"DOUBLE");
  ins--fptrunc(builder, 1.0d0, float-type);
  ins--ret(builder);
  check-equal("ins--fptrunc disassembly",
              #("entry:",
                "%0 = fptrunc double 1.000000e+00 to float",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fptrunc;

define llvm-builder function-test ins--fpext ()
  let builder = make-builder-with-test-function();
  let double-type = make(<llvm-primitive-type>, kind: #"DOUBLE");
  ins--fpext(builder, 1.0s0, double-type);
  ins--ret(builder);
  check-equal("ins--fpext disassembly",
              #("entry:",
                "%0 = fpext float 1.000000e+00 to double",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fpext;

define llvm-builder function-test ins--ptrtoint ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let ptr = ins--alloca(builder, float-type, 1);
  ins--ptrtoint(builder, ptr, $llvm-i32-type);
  ins--ret(builder);
  check-equal("ins--ptrtoint disassembly",
              #("entry:",
                "%0 = alloca float",
                "%1 = ptrtoint float* %0 to i32",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--ptrtoint;

define llvm-builder function-test ins--inttoptr ()
  let builder = make-builder-with-test-function();
  ins--inttoptr(builder, 0, $llvm-i8*-type);
  ins--ret(builder);
  check-equal("ins--inttoptr disassembly",
              #("entry:",
                "%0 = inttoptr i32 0 to i8*",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--inttoptr;

define llvm-builder function-test ins--bitcast ()
  let builder = make-builder-with-test-function();
  ins--bitcast(builder,
             make(<llvm-integer-constant>, type: $llvm-i8-type, integer: 255),
             $llvm-i8-type);
  ins--ret(builder);
  check-equal("ins--bitcast disassembly",
              #("entry:", "%0 = bitcast i8 -1 to i8", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--bitcast;

define llvm-builder function-test ins--icmp-eq ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-eq(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-eq disassembly",
              #("entry:", "%0 = icmp eq i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-eq;

define llvm-builder function-test ins--icmp-ne ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-ne(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-ne disassembly",
              #("entry:", "%0 = icmp ne i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-ne;

define llvm-builder function-test ins--icmp-slt ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-slt(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-slt disassembly",
              #("entry:", "%0 = icmp slt i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-slt;

define llvm-builder function-test ins--icmp-sgt ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-sgt(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-sgt disassembly",
              #("entry:", "%0 = icmp sgt i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-sgt;

define llvm-builder function-test ins--icmp-sle ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-sle(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-sle disassembly",
              #("entry:", "%0 = icmp sle i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-sle;

define llvm-builder function-test ins--icmp-sge ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-sge(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-sge disassembly",
              #("entry:", "%0 = icmp sge i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-sge;

define llvm-builder function-test ins--icmp-ult ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-ult(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-ult disassembly",
              #("entry:", "%0 = icmp ult i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-ult;

define llvm-builder function-test ins--icmp-ugt ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-ugt(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-ugt disassembly",
              #("entry:", "%0 = icmp ugt i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-ugt;

define llvm-builder function-test ins--icmp-ule ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-ule(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-ule disassembly",
              #("entry:", "%0 = icmp ule i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-ule;

define llvm-builder function-test ins--icmp-uge ()
  let builder = make-builder-with-test-function();
  let cmp = ins--icmp-uge(builder, 1, 2);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--icmp-uge disassembly",
              #("entry:", "%0 = icmp uge i32 1, 2", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--icmp-uge;

define llvm-builder function-test ins--fcmp-oeq ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-oeq(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-oeq disassembly",
              #("entry:",
                "%0 = fcmp oeq float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-oeq;

define llvm-builder function-test ins--fcmp-one ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-one(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-one disassembly",
              #("entry:",
                "%0 = fcmp one float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-one;

define llvm-builder function-test ins--fcmp-olt ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-olt(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-olt disassembly",
              #("entry:",
                "%0 = fcmp olt float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-olt;

define llvm-builder function-test ins--fcmp-ogt ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-ogt(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-ogt disassembly",
              #("entry:",
                "%0 = fcmp ogt float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-ogt;

define llvm-builder function-test ins--fcmp-ole ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-ole(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-ole disassembly",
              #("entry:",
                "%0 = fcmp ole float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-ole;

define llvm-builder function-test ins--fcmp-oge ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-oge(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-oge disassembly",
              #("entry:",
                "%0 = fcmp oge float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-oge;

define llvm-builder function-test ins--fcmp-ord ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-ord(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-ord disassembly",
              #("entry:",
                "%0 = fcmp ord float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-ord;

define llvm-builder function-test ins--fcmp-uno ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-uno(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-uno disassembly",
              #("entry:",
                "%0 = fcmp uno float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-uno;

define llvm-builder function-test ins--fcmp-ueq ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-ueq(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-ueq disassembly",
              #("entry:",
                "%0 = fcmp ueq float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-ueq;

define llvm-builder function-test ins--fcmp-une ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-une(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-une disassembly",
              #("entry:",
                "%0 = fcmp une float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-une;

define llvm-builder function-test ins--fcmp-ult ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-ult(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-ult disassembly",
              #("entry:",
                "%0 = fcmp ult float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-ult;

define llvm-builder function-test ins--fcmp-ugt ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-ugt(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-ugt disassembly",
              #("entry:",
                "%0 = fcmp ugt float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-ugt;

define llvm-builder function-test ins--fcmp-ule ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-ule(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-ule disassembly",
              #("entry:",
                "%0 = fcmp ule float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-ule;

define llvm-builder function-test ins--fcmp-uge ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-ule(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-ule disassembly",
              #("entry:",
                "%0 = fcmp ule float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-uge;

define llvm-builder function-test ins--fcmp-true ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-true(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-true disassembly",
              #("entry:",
                "%0 = fcmp true float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-true;

define llvm-builder function-test ins--fcmp-false ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let cmp = ins--fcmp-false(builder, 4.0s0, 5.0s0);
  llvm-constrain-type(cmp.llvm-value-type, $llvm-i1-type);
  ins--ret(builder);
  check-equal("ins--fcmp-false disassembly",
              #("entry:",
                "%0 = fcmp false float 4.000000e+00, 5.000000e+00",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fcmp-false;

define function make-test-struct-type
    (name :: false-or(<string>)) => (type :: <llvm-type>);
  let rt-type
    = make(<llvm-struct-type>,
           elements:
             vector($llvm-i8-type,
                    make(<llvm-array-type>, size: 10,
                         element-type:
                           make(<llvm-array-type>, size: 20,
                                element-type: $llvm-i32-type)),
                    $llvm-i8-type));
  let st-type
    = make(<llvm-struct-type>,
           name: name,
           elements:
             vector($llvm-i32-type,
                    make(<llvm-primitive-type>, kind: #"DOUBLE"),
                    rt-type));
  st-type
end function;

define llvm-builder function-test ins--gep ()
  let st-type = make-test-struct-type("ST");
  let st*-type = make(<llvm-pointer-type>, pointee: st-type);
  let i32*-type = make(<llvm-pointer-type>, pointee: $llvm-i32-type);
  let builder
    = make-builder-with-test-function(arg-type: st*-type,
                                      return-type: i32*-type);

  let reg = ins--gep(builder, llvm-builder-local(builder, "arg0"),
                     1, 2, 1, 5, 13);
  ins--ret(builder, reg);
  check-equal("ins--gep disassembly",
              #("entry:",
                "%0 = getelementptr %ST* %arg0,"
                  " i32 1, i32 2, i32 1, i32 5, i32 13",
                "ret i32* %0"),
              builder-test-function-disassembly(builder));
end function-test ins--gep;

define llvm-builder function-test ins--gep-inbounds ()
  let st-type = make-test-struct-type("ST");
  let st*-type = make(<llvm-pointer-type>, pointee: st-type);
  let i32*-type = make(<llvm-pointer-type>, pointee: $llvm-i32-type);
  let builder
    = make-builder-with-test-function(arg-type: st*-type,
                                      return-type: i32*-type);

  let reg
    = ins--gep-inbounds(builder, llvm-builder-local(builder, "arg0"),
                        1, 2, 1, 5, 13);
  ins--ret(builder, reg);
  check-equal("ins--gep disassembly",
              #("entry:",
                "%0 = getelementptr inbounds %ST* %arg0,"
                  " i32 1, i32 2, i32 1, i32 5, i32 13",
                "ret i32* %0"),
              builder-test-function-disassembly(builder));
end function-test ins--gep-inbounds;

define llvm-builder function-test ins--select ()
  let builder = make-builder-with-test-function();
  ins--select(builder,
              make(<llvm-integer-constant>, type: $llvm-i1-type, integer: 1),
              make(<llvm-integer-constant>, type: $llvm-i8-type, integer: 17),
              make(<llvm-integer-constant>, type: $llvm-i8-type, integer: 42));
  ins--ret(builder);
  check-equal("ins--select disassembly",
              #("entry:", "%0 = select i1 true, i8 17, i8 42", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--select;

define llvm-builder function-test ins--va-arg ()
  //---*** Fill this in...
end function-test ins--va-arg;

define function make-test-vector () => (value :: <llvm-value>)
  let vector-type
    = make(<llvm-vector-type>, size: 4, element-type: $llvm-i32-type);
  let vector-contents
    = map(method(x)
            make(<llvm-integer-constant>, type: $llvm-i32-type, integer: x)
          end,
          range(below: 4));
  make(<llvm-aggregate-constant>,
       type: vector-type,
       aggregate-values: vector-contents)
end function;

define llvm-builder function-test ins--extractelement ()
  let builder = make-builder-with-test-function();
  let ee = ins--extractelement(builder,
                               make-test-vector(),
                               make(<llvm-integer-constant>,
                                    type: $llvm-i32-type,
                                    integer: 2));
  llvm-constrain-type(ee.llvm-value-type, $llvm-i32-type);
  ins--ret(builder);
  check-equal("ins--extractelement disassembly",
              #("entry:",
                "%0 = extractelement <4 x i32> <i32 0, i32 1, i32 2, i32 3>,"
                  " i32 2",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--extractelement;

define llvm-builder function-test ins--insertelement ()
  let builder = make-builder-with-test-function();
  ins--insertelement(builder,
                     make-test-vector(),
                     make(<llvm-integer-constant>,
                          type: $llvm-i32-type,
                          integer: 8),
                     make(<llvm-integer-constant>,
                          type: $llvm-i32-type,
                          integer: 2));
  ins--ret(builder);
  check-equal("ins--insertelement disassembly",
              #("entry:",
                "%0 = insertelement <4 x i32> <i32 0, i32 1, i32 2, i32 3>,"
                  " i32 8, i32 2",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--insertelement;

define llvm-builder function-test ins--shufflevector ()
  let builder = make-builder-with-test-function();
  let test-vector = make-test-vector();
  let sv = ins--shufflevector(builder,
                              test-vector,
                              test-vector,
                              test-vector);
  llvm-constrain-type(sv.llvm-value-type, test-vector.llvm-value-type);
  ins--ret(builder);
  check-equal("ins--shufflevector disassembly",
              #("entry:",
                "%0 = shufflevector <4 x i32> <i32 0, i32 1, i32 2, i32 3>,"
                  " <4 x i32> <i32 0, i32 1, i32 2, i32 3>,"
                  " <4 x i32> <i32 0, i32 1, i32 2, i32 3>",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--shufflevector;

define llvm-builder function-test ins--phi ()
  //---*** Fill this in...
end function-test ins--phi;

define llvm-builder function-test ins--phi* ()
  with-test-unit ("ins--phi valid")
    let builder = make-builder-with-test-function();
    let entry = builder.llvm-builder-basic-block;
    let loop = make(<llvm-basic-block>, name: "Loop");
    ins--br(builder, loop);

    ins--block(builder, loop);
    let indvar0
      = ins--phi*(builder,
		  0, entry,
		  llvm-builder-local(builder, "nextindvar0"), loop);
    let indvar1
      = ins--phi*(builder,
		  40, entry,
		  llvm-builder-local(builder, "nextindvar1"), loop);
    ins--local(builder, "nextindvar0", ins--add(builder, indvar0, 1));
    ins--local(builder, "nextindvar1", ins--add(builder, indvar1, 1));
    ins--br(builder, loop);
    check-equal("ins--phi disassembly",
                #("entry:",
                  "br label %Loop",
                  "Loop:",
                  "%0 = phi i32 [ 0, %entry ], [ %nextindvar0, %Loop ]",
                  "%1 = phi i32 [ 40, %entry ], [ %nextindvar1, %Loop ]",
                  "%nextindvar0 = add i32 %0, 1",
                  "%nextindvar1 = add i32 %1, 1",
                  "br label %Loop"),
                builder-test-function-disassembly(builder));
  end;

  with-test-unit ("ins--phi* invalid")
    check-condition
      ("ins--phi must be grouped at basic block beginning",
       <error>,
       begin
         let builder = make-builder-with-test-function();
         let entry = builder.llvm-builder-basic-block;
         let loop = make(<llvm-basic-block>, name: "Loop");
         ins--br(builder, loop);
         ins--block(builder, loop);
         let indvar0
           = ins--phi*(builder,
		       0, entry,
		       llvm-builder-local(builder, "nextindvar0"), loop);
         ins--local(builder, "nextindvar0", ins--add(builder, indvar0, 1));
         let indvar1
           = ins--phi*(builder,
		       40, entry,
		       llvm-builder-local(builder, "nextindvar1"), loop);
         ins--local(builder, "nextindvar1", ins--add(builder, indvar1, 1));
         ins--br(builder, loop);
       end);
  end;
end function-test ins--phi*;

define llvm-builder function-test ins--call ()
  //---*** Fill this in...
end function-test ins--call;

define llvm-builder function-test ins--tail-call ()
  //---*** Fill this in...
end function-test ins--tail-call;

define llvm-builder function-test ins--call-intrinsic ()
  with-test-unit ("ins--call-intrinsic @llvm.prefetch")
    let builder = make-builder-with-test-function();
    let ptr = ins--alloca(builder, $llvm-float-type, 1);
    let address = ins--bitcast(builder, ptr, $llvm-i8*-type);
    ins--call-intrinsic(builder, "llvm.prefetch", vector(address, 0, 3, 1));
    ins--load(builder, ptr);
    ins--ret(builder);
    check-equal("ins--call-intrinsic @llvm.prefetch disassembly",
                #("entry:",
                  "%0 = alloca float",
                  "%1 = bitcast float* %0 to i8*",
                  "call void @llvm.prefetch(i8* nocapture %1, i32 0, i32 3, i32 1)"
                    " nounwind",
                  "%2 = load float* %0",
                  "ret void"),
                builder-test-function-disassembly(builder));
  end;
  with-test-unit ("ins--call-intrinsic float @llvm.sqrt")
    let builder = make-builder-with-test-function();
    ins--call-intrinsic(builder, "llvm.sqrt", vector(2.0s0));
    ins--ret(builder);
    check-equal("ins--call-intrinsic @llvm.sqrt disassembly",
                #("entry:",
                  "%0 = call float @llvm.sqrt.f32(float 2.000000e+00)"
                    " nounwind readonly",
                  "ret void"),
                builder-test-function-disassembly(builder));
  end;
  with-test-unit ("ins--call-intrinsic i8 @llvm.memcpy")
    let builder = make-builder-with-test-function();
    let ptr1 = ins--alloca(builder, $llvm-i8-type, 20);
    let ptr2 = ins--alloca(builder, $llvm-i8-type, 20);
    let ptr3 = ins--alloca(builder, $llvm-i8-type, 20);
    ins--call-intrinsic(builder, "llvm.memcpy",
                        vector(ptr1, ptr2, 20, 0, $llvm-false));
    ins--call-intrinsic(builder, "llvm.memcpy",
                        vector(ptr2, ptr3, 20, 0, $llvm-false));
    ins--ret(builder);

    check-equal("ins--call-intrinsic @llvm.memcpy disassembly",
                #("entry:",
                  "%0 = alloca i8, i32 20",
                  "%1 = alloca i8, i32 20",
                  "%2 = alloca i8, i32 20",
                  "call void @llvm.memcpy.p0i8.p0i8.i32"
                    "(i8* nocapture %0, i8* nocapture %1, i32 20, i32 0,"
                    " i1 false) nounwind",
                  "call void @llvm.memcpy.p0i8.p0i8.i32"
                    "(i8* nocapture %1, i8* nocapture %2, i32 20, i32 0,"
                    " i1 false) nounwind",
                  "ret void"),
                builder-test-function-disassembly(builder));
  end;
end function-test ins--call-intrinsic;

define llvm-builder function-test ins--alloca ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  ins--alloca(builder, float-type, 1);
  ins--alloca(builder, float-type, 16);
  ins--alloca(builder, float-type, 16, alignment: 4);
  ins--ret(builder);
  check-equal("ins--alloca disassembly",
              #("entry:",
                "%0 = alloca float",
                "%1 = alloca float, i32 16",
                "%2 = alloca float, i32 16, align 4",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--alloca;

define llvm-builder function-test ins--load ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-float-type, 1);
  ins--load(builder, ptr);
  ins--ret(builder);
  check-equal("ins--load disassembly",
              #("entry:",
                "%0 = alloca float",
                "%1 = load float* %0",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--load;

define llvm-builder function-test ins--store ()
  let builder = make-builder-with-test-function();
  let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
  let ptr = ins--alloca(builder, float-type, 1, alignment: 4);
  ins--store(builder, 1.0s0, ptr, alignment: 4);
  ins--ret(builder);
  check-equal("ins--store disassembly",
              #("entry:",
                "%0 = alloca float, align 4",
                "store float 1.000000e+00, float* %0, align 4",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--store;

define llvm-builder function-test ins--cmpxchg ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--cmpxchg(builder, ptr, 1, 0, scope: #"single-thread", ordering: #"monotonic");
  ins--cmpxchg(builder, ptr, 0, 1, volatile?: #t, ordering: #"acquire-release");
  ins--ret(builder);
  check-equal("ins--cmpxchg disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = cmpxchg i32* %0, i32 1, i32 0 singlethread monotonic",
                "%2 = cmpxchg volatile i32* %0, i32 0, i32 1 acq_rel",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--cmpxchg;

define llvm-builder function-test ins--atomicrmw-xchg ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-xchg(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-xchg disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile xchg i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-xchg;

define llvm-builder function-test ins--atomicrmw-add ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-add(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-add disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile add i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-add;

define llvm-builder function-test ins--atomicrmw-sub ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-sub(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-sub disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile sub i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-sub;

define llvm-builder function-test ins--atomicrmw-and ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-and(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-and disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile and i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-and;

define llvm-builder function-test ins--atomicrmw-nand ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-nand(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-nand disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile nand i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-nand;

define llvm-builder function-test ins--atomicrmw-or ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-or(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-or disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile or i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-or;

define llvm-builder function-test ins--atomicrmw-xor ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-xor(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-xor disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile xor i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-xor;

define llvm-builder function-test ins--atomicrmw-max ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-max(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-max disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile max i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-max;

define llvm-builder function-test ins--atomicrmw-min ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-min(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-min disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile min i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-min;

define llvm-builder function-test ins--atomicrmw-umax ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-umax(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-umax disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile umax i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-umax;

define llvm-builder function-test ins--atomicrmw-umin ()
  let builder = make-builder-with-test-function();
  let ptr = ins--alloca(builder, $llvm-i32-type, 1, alignment: 4);
  ins--atomicrmw-umin(builder, ptr, 10, volatile?: #t, ordering: #"monotonic");
  ins--ret(builder);
  check-equal("ins--atomicrmw-umin disassembly",
              #("entry:",
                "%0 = alloca i32, align 4",
                "%1 = atomicrmw volatile umin i32* %0, i32 10 monotonic",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--atomicrmw-umin;

define llvm-builder function-test ins--fence ()
  let builder = make-builder-with-test-function();
  ins--fence(builder, scope: #"single-thread", ordering: #"release");
  ins--ret(builder);
  check-equal("ins--fence disassembly",
              #("entry:",
                "fence singlethread release",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--fence;

define llvm-builder function-test ins--insertvalue ()
  let builder = make-builder-with-test-function();
  let struct-type
    = make(<llvm-struct-type>,
           name: "ST",
           elements: vector($llvm-i32-type,
                            make(<llvm-primitive-type>, kind: #"FLOAT")));
  let agg1
    = ins--insertvalue(builder,
                       make(<llvm-undef-constant>, type: struct-type), 1, 0);
  ins--insertvalue(builder, agg1, 1.0s0, 1);
  ins--ret(builder);
  check-equal("ins--insertvalue disassembly",
              #("entry:",
                "%0 = insertvalue %ST undef, i32 1, 0",
                "%1 = insertvalue %ST %0, float 1.000000e+00, 1",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--insertvalue;

define llvm-builder function-test ins--extractvalue ()
  let struct-type
    = make(<llvm-struct-type>,
           name: "ST",
           elements: vector($llvm-i32-type,
                            make(<llvm-primitive-type>, kind: #"FLOAT")));
  let builder = make-builder-with-test-function(arg-type: struct-type);
  let ev = ins--extractvalue(builder, llvm-builder-local(builder, "arg0"), 0);
  ins--ret(builder);
  check-equal("ins--extractvalue disassembly",
              #("entry:",
                "%0 = extractvalue %ST %arg0, 0",
                "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--extractvalue;

define llvm-builder function-test ins--ret ()
  let builder = make-builder-with-test-function();
  ins--ret(builder);
  ins--block(builder, make(<llvm-basic-block>, name: "bb.1"));
  ins--ret(builder, 17);
  check-equal("ins--ret disassembly",
              #("entry:", "ret void", "bb.1:", "ret i32 17"),
              builder-test-function-disassembly(builder));
end function-test ins--ret;

define llvm-builder function-test ins--br ()
  let builder = make-builder-with-test-function();
  let bb1 = make(<llvm-basic-block>, name: "bb.1");
  let bb2 = make(<llvm-basic-block>, name: "bb.2");
  ins--br(builder,
          make(<llvm-integer-constant>, type: $llvm-i1-type, integer: 1),
          bb1, bb2);
  ins--block(builder, bb1);
  ins--br(builder, bb2);
  ins--block(builder, bb2);
  ins--br(builder, bb1);
  check-equal("ins--br disassembly",
              #("entry:", "br i1 true, label %bb.1, label %bb.2",
                "bb.1:", "br label %bb.2",
                "bb.2:", "br label %bb.1"),
              builder-test-function-disassembly(builder));
end function-test ins--br;

define llvm-builder function-test ins--switch ()
  //---*** Fill this in...
end function-test ins--switch;

define llvm-builder function-test ins--switch* ()
  let builder = make-builder-with-test-function();
  let bb1 = make(<llvm-basic-block>, name: "bb.1");
  let bb2 = make(<llvm-basic-block>, name: "bb.2");
  let bb3 = make(<llvm-basic-block>, name: "bb.3");
  ins--switch*(builder, 17, bb3,
	       1, bb1,
	       2, bb2);
  ins--block(builder, bb1);
  ins--br(builder, bb2);
  ins--block(builder, bb2);
  ins--br(builder, bb1);
  ins--block(builder, bb3);
  ins--ret(builder);

  check-equal("ins--switch* disassembly",
              #("entry:",
                "switch i32 17, label %bb.3 [",
                "  i32 1, label %bb.1",
                "  i32 2, label %bb.2",
                "]",
                "bb.1:", "br label %bb.2",
                "bb.2:", "br label %bb.1",
                "bb.3:", "ret void"),
              builder-test-function-disassembly(builder));
end function-test ins--switch*;

define llvm-builder function-test ins--invoke ()
  //---*** Fill this in...
end function-test ins--invoke;

define llvm-builder function-test ins--resume ()
  let builder = make-builder-with-test-function();
  let struct-type
    = make(<llvm-struct-type>,
           elements: vector($llvm-i8*-type, $llvm-i32-type));
  ins--resume(builder, make(<llvm-undef-constant>, type: struct-type));
  check-equal("ins--resume disassembly",
              #("entry:", "resume { i8*, i32 } undef"),
              builder-test-function-disassembly(builder));
end function-test ins--resume;

define llvm-builder function-test ins--unreachable ()
  let builder = make-builder-with-test-function();
  ins--unreachable(builder);
  check-equal("ins--unreachable disassembly",
              #("entry:", "unreachable"),
              builder-test-function-disassembly(builder));
end function-test ins--unreachable;

define llvm-builder function-test ins--landingpad ()
  let builder = make-builder-with-test-function();

  let personality-function-type
    = make(<llvm-function-type>,
           return-type: $llvm-i32-type,
           parameter-types: #(),
           varargs?: #t);
  let personality-function
    = make(<llvm-function>,
           name: "__gxx_personality_v0",
           type: make(<llvm-pointer-type>, pointee: personality-function-type),
           arguments: #(),
           linkage: #"external");
  llvm-builder-define-global(builder, personality-function.llvm-global-name,
                             personality-function);

  let next = make(<llvm-basic-block>);
  ins--invoke(builder, next, next, builder.llvm-builder-function, #(),
              type: $llvm-void-type);
  ins--local(builder, "Next", ins--block(builder, next));
  let struct-type
    = make(<llvm-struct-type>,
           elements: vector($llvm-i8*-type, $llvm-i32-type));
  ins--landingpad(builder, struct-type, personality-function, #(),
                  cleanup?: #t);
  check-equal("ins--landingpad disassembly",
              #("entry:",
                "invoke void @test()",
                "        to label %Next unwind label %Next",
                "Next:",
                "%0 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0",
                "        cleanup"),
              builder-test-function-disassembly(builder));
end function-test ins--landingpad;

define llvm-builder macro-test ins--if-test ()
  with-test-unit ("ins--if, no else, no value")
    let builder = make-builder-with-test-function();

    let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
    let ptr = ins--alloca(builder, float-type, 1, alignment: 4);

    let cmp = ins--icmp-uge(builder, 1, 2);
    ins--if(builder, cmp)
      ins--store(builder, 1.0s0, ptr, alignment: 4);
    end ins--if;

    check-equal("ins--if disassembly",
                #("entry:",
                  "%0 = alloca float, align 4",
                  "%1 = icmp uge i32 1, 2",
                  "br i1 %1, label %2, label %3",
                  "store float 1.000000e+00, float* %0, align 4",
                  "br label %3"),
                builder-test-function-disassembly(builder));
  end;

  with-test-unit ("ins--if with else, no value")
    let builder = make-builder-with-test-function();

    let float-type = make(<llvm-primitive-type>, kind: #"FLOAT");
    let ptr = ins--alloca(builder, float-type, 1, alignment: 4);

    let cmp = ins--icmp-uge(builder, 1, 2);
    ins--if(builder, cmp)
      ins--store(builder, 1.0s0, ptr, alignment: 4);
    ins--else
      ins--store(builder, 2.0s0, ptr, alignment: 4);
    end ins--if;

    check-equal("ins--if disassembly",
                #("entry:",
                  "%0 = alloca float, align 4",
                  "%1 = icmp uge i32 1, 2",
                  "br i1 %1, label %2, label %3",
                  "store float 1.000000e+00, float* %0, align 4",
                  "br label %4",
                  "store float 2.000000e+00, float* %0, align 4",
                  "br label %4"),
                builder-test-function-disassembly(builder));
  end;

  with-test-unit ("ins--if, no else, with value")
    let builder = make-builder-with-test-function();

    let cmp = ins--icmp-uge(builder, 1, 2);
    ins--if(builder, cmp)
      ins--add(builder, 1111, 2222);
    end ins--if;

    check-equal("ins--if disassembly",
                #("entry:",
                  "%0 = icmp uge i32 1, 2",
                  "br i1 %0, label %1, label %3",
                  "%2 = add i32 1111, 2222",
                  "br label %3",
                  "%4 = phi i32 [ %2, %1 ], [ undef, %entry ]"),
                builder-test-function-disassembly(builder));
  end;

  with-test-unit ("ins--if with else, with value")
    let builder = make-builder-with-test-function();

    let cmp = ins--icmp-uge(builder, 1, 2);
    ins--if(builder, cmp)
      ins--add(builder, 1111, 2222);
    ins--else
      ins--add(builder, 3333, 4444);
    end ins--if;

    check-equal("ins--if disassembly",
                #("entry:",
                  "%0 = icmp uge i32 1, 2",
                  "br i1 %0, label %1, label %3",
                  "%2 = add i32 1111, 2222",
                  "br label %5",
                  "%4 = add i32 3333, 4444",
                  "br label %5",
                  "%6 = phi i32 [ %2, %1 ], [ %4, %3 ]"),
                builder-test-function-disassembly(builder));
  end;

  with-test-unit ("ins--if with else, no fall-through")
    let builder = make-builder-with-test-function();

    let cmp = ins--icmp-uge(builder, 1, 2);
    ins--if(builder, cmp)
      ins--add(builder, 1111, 2222);
    ins--else
      ins--call-intrinsic(builder, "llvm.trap", vector());
      ins--unreachable(builder);
    end ins--if;

    check-equal("ins--if disassembly",
                #("entry:",
                  "%0 = icmp uge i32 1, 2",
                  "br i1 %0, label %1, label %3",
                  "%2 = add i32 1111, 2222",
                  "br label %4",
                  "call void @llvm.trap() #1",
                  "unreachable"),
                builder-test-function-disassembly(builder));
  end;

  with-test-unit ("ins--if with else, no successor block")
    let builder = make-builder-with-test-function();

    let cmp = ins--icmp-uge(builder, 1, 2);
    ins--if(builder, cmp)
      ins--call-intrinsic(builder, "llvm.trap", vector());
      ins--unreachable(builder);
    ins--else
      ins--call-intrinsic(builder, "llvm.trap", vector());
      ins--unreachable(builder);
    end ins--if;
    check-false("no active block", builder.llvm-builder-basic-block);

    check-equal("ins--if disassembly",
                #("entry:",
                  "%0 = icmp uge i32 1, 2",
                  "br i1 %0, label %1, label %2",
                  "call void @llvm.trap() #1",
                  "unreachable",
                  "call void @llvm.trap() #1",
                  "unreachable"),
                builder-test-function-disassembly(builder));
  end;
end macro-test;

define llvm-builder macro-test ins--iterate-test ()
  with-test-unit ("ins--iterate with no loop variables")
    let builder = make-builder-with-test-function();

    ins--iterate loop (builder)
      ins--call-intrinsic(builder, "llvm.trap", vector());
      loop();
    end ins--iterate;

    check-equal("ins--iterate disassembly",
                #("entry:",
                  "br label %0",
                  "call void @llvm.trap() #1",
                  "br label %0"),
                builder-test-function-disassembly(builder));
  end;

  with-test-unit ("ins--iterate with one loop variable")
    let builder = make-builder-with-test-function();

    ins--iterate loop (builder, a = 0)
      loop(ins--add(builder, a, 1));
    end ins--iterate;

    check-equal("ins--iterate disassembly",
                #("entry:",
                  "br label %0",
                  "%1 = phi i32 [ 0, %entry ], [ %2, %0 ]",
                  "%2 = add i32 %1, 1",
                  "br label %0"),
                builder-test-function-disassembly(builder));
  end;

  with-test-unit ("ins--iterate with two loop variable and complex control flow")
    let builder = make-builder-with-test-function();

    ins--iterate loop (builder, sum = 0, i = 0)
      let cmp = ins--icmp-ult(builder, i, 14);
      ins--if (builder, cmp)
        let sum = ins--add(builder, sum, i);
        let inc = ins--add(builder, i, 1);
        loop(sum, inc)
      ins--else
        sum
      end ins--if
    end ins--iterate;

    check-equal("ins--iterate disassembly",
                #("entry:",
                  "br label %0",
                  "%1 = phi i32 [ 0, %entry ], [ %5, %4 ]",
                  "%2 = phi i32 [ 0, %entry ], [ %6, %4 ]",
                  "%3 = icmp ult i32 %2, 14",
                  "br i1 %3, label %4, label %7",
                  "%5 = add i32 %1, %2",
                  "%6 = add i32 %2, 1",
                  "br label %0",
                  "br label %8"),
                builder-test-function-disassembly(builder));
  end;

end macro-test;
