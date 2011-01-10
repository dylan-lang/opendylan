Module:    dylan-user
Author:    Jason Trenouth, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library scepter
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use scepter-core;
  use scepter-dump-back-end;
  use scepter-dylan-back-end;
  use scepter-ir-back-end;
  use scepter-file-front-end;

  export scepter;
end library;

define module scepter
  use generic-arithmetic-functional-dylan, exclude: {<union>};

  use scepter-error,
    export: { <idl-condition>,
              idl-condition-title,
              idl-condition-body,
              idl-condition-source };
              

  use scepter-driver,
    export: { <scepter>,
              scepter-invoke,
              scepter-output,
              scepter-case-sensitive-reserved-words?-setter,
              scepter-failure-mode-setter,
              scepter-failure-token-setter,
              scepter-front-end, scepter-front-end-setter,
              scepter-back-ends, scepter-back-ends-setter,
              scepter-back-end-class,
              scepter-informative?-setter,
              scepter-sources-setter,
              scepter-initialize-options,
              scepter-process-options,
              scepter-default-back-end-class,
              scepter-default-front-end-class,
              scepter-source,
              scepter-parse-options,
              get-scepter,
              set-scepter,
	      scepter-format-progress-start,
	      scepter-format-progress-end,
              scepter-report-condition,
              \with-progress };

  use scepter-back-end,
    export: { <scepter-back-end>,
              scepter-back-end-class
              };

  use scepter-front-end,
    export: { <scepter-front-end>,
              scepter-front-end-class };

  use scepter-dylan-back-end,
    export: { <dylan-back-end-result>,
              dylan-back-end-result-success?,
              dylan-back-end-result-projects,
              dylan-back-end-result-modified-projects };

  use scepter-file-front-end,
    export: { make-token-stream,
	      \with-token-stream,
	      idl-parser-tag,
	      idl-lexer-string };

end module;

