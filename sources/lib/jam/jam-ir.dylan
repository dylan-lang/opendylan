Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <jam-arg> = type-union(<byte-string>, <jam-statement>);

define abstract class <jam-statement> (<object>)
  // source-location info?
end class;

define class <jam-block-statement> (<jam-statement>)
  constant slot block-local-vars :: <sequence>,
    init-value: #[], init-keyword: local-vars:;
  constant slot block-local-values :: <sequence>,
    init-value: #[], init-keyword: local-values:;
  constant slot block-statements :: <sequence>,
    required-init-keyword: statements:;
end class;

define class <jam-include-statement> (<jam-statement>)
  constant slot include-list :: <sequence>,
    required-init-keyword: list:;
end class;

define class <jam-invocation-statement> (<jam-statement>)
  constant slot invocation-rulename :: <jam-arg>,
    required-init-keyword: rulename:;
  constant slot invocation-fields :: <sequence>,
    required-init-keyword: fields:;
end class;

define class <jam-assignment-statement> (<jam-statement>)
  constant slot assignment-variable :: <jam-arg>,
    required-init-keyword: variable:;
  constant slot assignment-values :: <sequence>,
    required-init-keyword: values:;
  constant slot assignment-kind :: one-of(#"=", #"+=", #"?="),
    required-init-keyword: kind:;
end class;

define class <jam-on-assignment-statement> (<jam-assignment-statement>)
  constant slot assignment-targets :: <sequence>,
    required-init-keyword: targets:;
end class;

define class <jam-break-statement> (<jam-statement>)
  constant slot break-values :: <sequence>,
    required-init-keyword: values:;
end class;

define class <jam-continue-statement> (<jam-statement>)
  constant slot continue-values :: <sequence>,
    required-init-keyword: values:;
end class;

define class <jam-return-statement> (<jam-statement>)
  constant slot return-values :: <sequence>,
    required-init-keyword: values:;
end class;

define class <jam-for-statement> (<jam-statement>)
  constant slot for-var :: <byte-string>,
    required-init-keyword: var:;
  constant slot for-values :: <sequence>,
    required-init-keyword: values:;
  constant slot for-statements :: <sequence>,
    required-init-keyword: statements:;
end class;

define class <jam-switch-statement> (<jam-statement>)
  constant slot switch-values :: <sequence>,
    required-init-keyword: values:;
  constant slot switch-cases :: <sequence>,
    required-init-keyword: cases:;
end;

define class <jam-if-statement> (<jam-statement>)
  constant slot if-condition :: <jam-expression>,
    required-init-keyword: condition:;
  constant slot if-statements :: <sequence>,
    required-init-keyword: statements:;
  constant slot else-statement :: false-or(<jam-statement>),
    init-value: #f, init-keyword: else:;
end class;

define class <jam-while-statement> (<jam-statement>)
  constant slot while-condition :: <jam-expression>,
    required-init-keyword: condition:;
  constant slot while-statements :: <sequence>,
    required-init-keyword: statements:;
end class;

define class <jam-on-statement> (<jam-statement>)
  constant slot on-targets :: <jam-arg>,
    required-init-keyword: targets:;
  constant slot on-statement :: <jam-statement>,
    required-init-keyword: statement:;
end class;

define class <jam-ruledef-statement> (<jam-statement>)
  constant slot ruledef-name :: <byte-string>,
    required-init-keyword: name:;
  constant slot ruledef-params :: <sequence>,
    required-init-keyword: params:;
  constant slot ruledef-statements :: <sequence>,
    required-init-keyword: statements:;
end class;

define class <jam-actiondef-statement> (<jam-statement>)
  constant slot actiondef-name :: <byte-string>,
    required-init-keyword: name:;
  constant slot actiondef-flags :: <sequence>,
    required-init-keyword: flags:;
  constant slot actiondef-bindlist :: false-or(<sequence>),
    required-init-keyword: bindlist:;
  constant slot actiondef-commands :: <byte-string>,
    required-init-keyword: commands:;
end class;

define class <jam-case> (<object>)
  //constant slot case-pattern :: <byte-string>,
  //  required-init-keyword: pattern:;
  constant slot case-match-function :: <function>,
    required-init-keyword: match-function:;
  constant slot case-statements :: <sequence>,
    required-init-keyword: statements:;
end class;



define abstract class <jam-expression> (<object>)
  // no slots
end class;

define class <jam-leaf-expression> (<jam-expression>)
  constant slot leaf-argument :: <jam-arg>,
    required-init-keyword: argument:;
  constant slot leaf-list :: false-or(<sequence>),
    init-value: #f, init-keyword: list:;
end class;

define abstract class <jam-composite-expression> (<jam-expression>)
  constant slot composite-left :: <jam-expression>,
    required-init-keyword: left:;
  constant slot composite-right :: false-or(<jam-expression>),
    init-value: #f, init-keyword: right:;
end class;

define class <jam-not-expression> (<jam-composite-expression>) end;

define class <jam-lt-expression> (<jam-composite-expression>) end;
define class <jam-le-expression> (<jam-composite-expression>) end;
define class <jam-gt-expression> (<jam-composite-expression>) end;
define class <jam-ge-expression> (<jam-composite-expression>) end;

define class <jam-eq-expression> (<jam-composite-expression>) end;
define class <jam-ne-expression> (<jam-composite-expression>) end;

define class <jam-and-expression> (<jam-composite-expression>) end;
define class <jam-or-expression> (<jam-composite-expression>) end;
