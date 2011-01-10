Synopsis: define compilation-pass macro
Module:   dfmc-common
Author:   Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TODO: optimization and visit need to be fixed when macros support
// auxiliary rules for pattern variables

define method optimization-default-hack (#rest x)
  if (empty?(x))
    $optimization-default
  else
    x[0]
  end if
end method optimization-default-hack;

define method visit-default-hack (#rest x)
  if (empty?(x))
    #"top-level-forms"
  else
    x[0]
  end if
end method visit-default-hack;

define macro compilation-pass-definer
  { define compilation-pass ?pass:name,
      #key ??optimization:name,
           ??visit:name,
           ?mandatory?:expression = #f, 
           ?disabled?:expression = #f,
           ?print?:expression = #f, 
           ?print-before?:expression = #f, 
           ?print-after?:expression = #f,
           ?check?:expression = #f, 
           ?check-before?:expression = #f, 
           ?check-after?:expression = #f,
           ??after:name, ??before:name, ??trigger:name, ??triggered-by:name,
           ??back-end:name, ??exclude-back-end:name }
  => { define-compilation-pass!
         (?#"pass",
          function: method (code) ?pass(code) end, // delayed evaluation hack
	  mandatory?: ?mandatory?, // overrides optimization:
          optimization-level: optimization-default-hack(??optimization, ...),
	  visit: visit-default-hack(??visit, ...),
          disabled?: ?disabled?,
          print?: ?print?,
          print-before?: ?print-before?,
          print-after?: ?print-after?,
          check?: ?check?,
          check-before?: ?check-before?,
          check-after?: ?check-after?,
          before: #[ ??before , ... ],
          after: #[ ??after , ...],
          trigger: #[ ??trigger, ... ],
          triggered-by: #[ ??triggered-by , ... ],
          back-end: #[ ??back-end , ... ],
          exclude-back-end: #[ ??exclude-back-end , ... ]) }
 optimization:
  { ?:name } => { "$optimization-" ## ?name }
 visit:
  { ?:name } => { ?#"name" } 
 after:
  { ?:name } => { ?#"name" }
 before:
  { ?:name } => { ?#"name" }
 trigger:
  { ?:name } => { ?#"name" }
 triggered-by:
  { ?:name } => { ?#"name" }
 back-end:
  { ?:name } => { ?#"name" }
 exclude-back-end:
  { ?:name } => { ?#"name" }
end macro;
