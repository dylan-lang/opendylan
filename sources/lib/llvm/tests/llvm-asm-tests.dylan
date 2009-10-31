Module:       llvm-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro llvm-asm-suite-definer
  { define llvm-asm-suite ?suite-name:name (?keyword-args:*) ?options end }
    => { define variable ?suite-name
           = make-suite(?"suite-name", 
		        method ()
		          llvm-asm-suite-components(?"suite-name", ?options)
		        end,
                        ?keyword-args) }
options:
    { } => { }
    { ?option; ... } => { ?option, ... }
    
option:
    { directory ?:expression } => { ?expression }
end macro llvm-asm-suite-definer;

define llvm-asm-suite llvm-asm-suite ()
  directory "Assembler";
  directory "Bitcode";
  directory "Feature";
  directory "Analysis/Andersens";
  directory "Analysis/BasicAA";
  directory "Analysis/CallGraph";
  directory "Analysis/Dominators";
  directory "Analysis/GlobalsModRef";
  directory "Analysis/LoopDependenceAnalysis";
  directory "Analysis/LoopInfo";
  directory "Analysis/PointerTracking";
  directory "Analysis/PostDominators";
  directory "Analysis/Profiling";
  directory "Analysis/ScalarEvolution";
  directory "Archive";
  directory "BugPoint";
  directory "CodeGen/ARM";
  directory "CodeGen/Alpha";
  directory "CodeGen/Blackfin";
  directory "CodeGen/CBackend";
  directory "CodeGen/CPP";
  directory "CodeGen/CellSPU";
  directory "CodeGen/Generic";
  directory "CodeGen/Generic/GC";
  directory "CodeGen/MSP430";
  directory "CodeGen/Mips";
  directory "CodeGen/PIC16";
  directory "CodeGen/PowerPC";
  directory "CodeGen/SPARC";
  directory "CodeGen/SystemZ";
  directory "CodeGen/Thumb";
  directory "CodeGen/Thumb2";
  directory "CodeGen/X86";
  directory "CodeGen/XCore";
  directory "DebugInfo";
  directory "ExecutionEngine";
  directory "FrontendC";
  directory "Integer";
  directory "Linker";
  directory "Other";
  directory "Transforms/ADCE";
  directory "Transforms/ArgumentPromotion";
  directory "Transforms/BlockPlacement";
  directory "Transforms/BranchFolding";
  directory "Transforms/CodeExtractor";
  directory "Transforms/CodeGenPrepare";
  directory "Transforms/CondProp";
  directory "Transforms/ConstProp";
  directory "Transforms/ConstantMerge";
  directory "Transforms/DeadArgElim";
  directory "Transforms/DeadStoreElimination";
  directory "Transforms/FunctionAttrs";
  directory "Transforms/GVN";
  directory "Transforms/GlobalDCE";
  directory "Transforms/GlobalOpt";
  directory "Transforms/IPConstantProp";
  directory "Transforms/IndVarSimplify";
  directory "Transforms/Inline";
  directory "Transforms/InstCombine";
  directory "Transforms/Internalize";
  directory "Transforms/JumpThreading";
  directory "Transforms/LCSSA";
  directory "Transforms/LICM";
  directory "Transforms/LoopDeletion";
  directory "Transforms/LoopIndexSplit";
  directory "Transforms/LoopRotate";
  directory "Transforms/LoopSimplify";
  directory "Transforms/LoopStrengthReduce";
  directory "Transforms/LoopUnroll";
  directory "Transforms/LoopUnswitch";
  directory "Transforms/LowerInvoke";
  directory "Transforms/LowerSetJmp";
  directory "Transforms/LowerSwitch";
  directory "Transforms/Mem2Reg";
  directory "Transforms/MemCpyOpt";
  directory "Transforms/MergeFunc";
  directory "Transforms/PruneEH";
  directory "Transforms/Reassociate";
  directory "Transforms/SCCP";
  directory "Transforms/SRETPromotion";
  directory "Transforms/SSI";
  directory "Transforms/ScalarRepl";
  directory "Transforms/SimplifyCFG";
  directory "Transforms/SimplifyLibCalls";
  directory "Transforms/StripSymbols";
  directory "Transforms/TailCallElim";
  directory "Transforms/TailDup";
  directory "Verifier";
end llvm-asm-suite;

define function llvm-asm-suite-components
    (name :: <string>, #rest directories)
 => (components :: <sequence>);
  block (return)
    let llvm-tests-path :: false-or(<string>)
      = environment-variable("LLVM_TEST");
    if (~llvm-tests-path)
      signal("The LLVM_TEST environment variable is not set");
      return(#[]);
    end if;

    let llvm-tests-directory :: <directory-locator>
      = as(<directory-locator>, llvm-tests-path);

    let components = make(<stretchy-object-vector>);
    for (directory in directories)
      let suite-directory
        = merge-locators(as(<directory-locator>, directory),
                         llvm-tests-directory);

      let tests = make(<stretchy-object-vector>);
      do-directory(method (directory :: <directory-locator>, name, type)
                     if (type == #"file")
                       let file-locator = as(<file-locator>, name);
                       if (file-locator.locator-extension = "ll")
                         let test-function
                           = make-llvm-test-function(directory, file-locator);
                         if (test-function)
                           let test
                             = make(<test>,
                                    name: name,
                                    function: test-function);
                           add!(tests, test);
                         end if;
                         
                       end if;
                     end if;
                   end method,
                   suite-directory);

      let suite = make-suite(concatenate("llvm-", directory, "-suite"),
                             always(tests));
      add!(components, suite);
    end for;

    components
  end block
end function;

define function make-llvm-test-function
    (directory :: <directory-locator>, file-locator :: <file-locator>)
 => (test-function :: false-or(<function>));
  let merged-file-locator = merge-locators(file-locator, directory);

  let marked-not?
    = with-open-file(stream = merged-file-locator)
        peek(stream, on-end-of-stream: #f) == ';'
          & subsequence-position(read-line(stream) | "", "; RUN: not ")
      end with-open-file;

  if (marked-not?)
    method ()
      let module
        = make(<llvm-module>, name: as(<string>, merged-file-locator));
      with-open-file (stream = merged-file-locator)
        check-condition(format-to-string("Parse %s detects errors",
                                         file-locator),
                        <error>, llvm-asm-parse(module, stream));
      end with-open-file;
    end method
  else
    method ()
      let module
        = make(<llvm-module>, name: as(<string>, merged-file-locator));
      with-open-file (stream = merged-file-locator)
        check-no-errors(format-to-string("Parse %s", file-locator),
                        llvm-asm-parse(module, stream));
      end with-open-file;
    end method
  end if
end function;