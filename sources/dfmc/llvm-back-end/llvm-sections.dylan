Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The following sections have special meanings:
// #"data"             - ambiguously traced data section
// #"ambiguous-data"   - synonym for the above
// #"variables"        - section traced as roots by the GC
// #"objects"          - section traced as a heap by the GC
// #"untraced-objects" - heap section, untraced by the GC
// #"untraced-data"    - untraced data section
// #"code"             - the code or text section
// #"init-code"        - separate initialization part of the code section

define method llvm-section-name
    (back-end :: <llvm-back-end>, section :: <symbol>)
 => (name :: false-or(<string>));
  select (section)
    #"code"                    => #f;         // Code (usually ".text")
    #"init-code"               => ".text$i";  // Initialization code
    #"data", #"ambiguous-data" => ".dydat$m"; // Ambiguously traced data
    #"objects"                 => ".dyobj$m"; // Dylan objects (a static heap)
    #"variables"               => ".dyvar$m"; // Dylan roots (variables)
    #"untraced-objects"        => ".dyutr$m"; // Untraced Dylan objects
    #"untraced-data"           => ".dyutr$r"; // Untraced random data
  end select
end method;

// Darwin is different (cf. Mac OS X ABI Mach-O File Format Reference)
// LLVM section names for Mach-O are "segment, section" (comma-separated)
define method llvm-section-name
    (back-end :: <llvm-darwin-back-end>, section :: <symbol>)
 => (name :: false-or(<string>));
  select (section)
    #"code"                    => #f; // Code (usually "__TEXT,__text")
    #"init-code"               => "__TEXT,__init";  // Initialization code
    #"data", #"ambiguous-data" => "__DATA,__dydat"; // Ambiguously traced data
    #"objects"                 => "__DATA,__dyobj"; // Dylan objects
    #"variables"               => "__DATA,__dyvar"; // Dylan roots (variables)
    #"untraced-objects",                            // Untraced Dylan objects
    #"untraced-data"           => "__DATA,__dyutr"; // Untraced random data
  end select
end method;
