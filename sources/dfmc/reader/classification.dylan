Module:    dfmc-reader
Synopsis:  Name classification
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// What you get out of a syntax table is actually an abstract
// property rather than just a name I guess.

define class <name-properties> (<object>)
  constant slot class,
    required-init-keyword: class:;
end class;

define method install-syntax (table, #rest tokens) => ()
  for (i from 0 below tokens.size by 2)
    table[as(<symbol>, tokens[i])]
      := make(<name-properties>, class: tokens[i + 1]);
  end;
end method;

define macro token-classes-definer
  { define token-classes ?table:expression ?tokens:* end }
    => { install-syntax(?table, ?tokens) }
tokens:
  { token ?:name; ... }
    => { ?#"name", "$" ## ?name ## "-token", ... };
  { token ?:name => ?class:expression; ... }
    => { ?#"name", ?class, ... }
  { token ?name:expression => ?class:expression; ... }
    => { ?name, ?class, ... }
  { }
    => { }
end macro;

define constant $core-syntax-table :: <object-table> = make(<object-table>);

define token-classes $core-syntax-table
  token \define;
  token \end;
  // We do "let", "let handler", and "local" as macros.
  // token \macro;
  token \otherwise;
end token-classes;

define token-classes $core-syntax-table
  token "="         => $equal-token;
  token "=="        => $equal-equal-token;
  token "=>"        => $equal-greater-token;
end token-classes;

// TODO: CORRECTNESS: Not thread safe.

define constant *classification-cache* :: <object-table>
  = make(<object-table>);

define macro with-classification-cache
  { with-classification-cache ?:body end }
    => { remove-all-keys!(*classification-cache*); ?body }
end macro;

define inline function syntax-for-name (table, name)
  let cached-class = element(*classification-cache*, name, default: #f);
  cached-class
    | (element(*classification-cache*, name)
         := begin
              let props = element($core-syntax-table, name, default: #f);
              if (props)
                let props :: <name-properties> = props;
                props.class
              else
                classify-word-in(*fragment-context*, name)
                  | $unreserved-name-token;
              end
            end)
end function;

define function classify-dylan-name (name)
  let props = element($core-syntax-table, name, default: #f);
  if (props)
    let props :: <name-properties> = props;
    props.class
  else
    classify-word-in(#f, name)
      | $unreserved-name-token;
  end;
end function;

define function classify-expansion-word-in (module, name)
  let props = element($core-syntax-table, name, default: #f);
  if (props)
    let props :: <name-properties> = props;
    props.class
  else
    classify-word-in(module, name)
      | $unreserved-name-token;
  end;
end function;

define method definer-token-class? (class) => (well? :: <boolean>)
  class == $define-body-word-only-token
    | class == $define-list-word-only-token
    | class == $define-macro-body-word-only-token
end;

define method definer-or-merged-token-class? (class) => (well? :: <boolean>)
  definer-token-class?(class)
    | class == $function-and-define-body-word-token
    | class == $function-and-define-list-word-token
    | class == $begin-and-define-body-word-token
    | class == $begin-and-define-list-word-token
    | class == $macro-case-begin-and-define-macro-body-word-token
end;

//// Token merging.

// TODO: CORRECTNESS: Fill out token merging.

define macro merge-method-definer
  { define merge-method ?kind1:name, ?kind2:name => ?kind3:name; }
    => { define method merge-token-classes
             (kind1 == "$" ## ?kind1 ## "-token",
              kind2 == "$" ## ?kind2 ## "-token",
              word) => (result)
           "$" ## ?kind3 ## "-token"
         end method;
         define method merge-token-classes
             (kind2 == "$" ## ?kind2 ## "-token",
              kind1 == "$" ## ?kind1 ## "-token",
              word) => (result)
           "$" ## ?kind3 ## "-token"
         end }
end macro;

define merge-method begin-word-only, define-body-word-only
  => begin-and-define-body-word;

define merge-method begin-word-only, define-list-word-only
  => begin-and-define-list-word;

define merge-method function-word-only, define-body-word-only
  => function-and-define-body-word;

define merge-method function-word-only, define-list-word-only
  => function-and-define-list-word;

define merge-method macro-case-begin-word-only, define-macro-body-word-only
  => macro-case-begin-and-define-macro-body-word;

// TODO: Just for bootstrapping.
define merge-method begin-word-only, define-macro-body-word-only
  => macro-case-begin-and-define-macro-body-word;
