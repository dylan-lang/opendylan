Module: result-set-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//++ place this into dwsql.dylan?
define open abstract class <database-collection> (<sequence>)
end class;

define method type-for-copy(collection :: <database-collection>)
 => (type :: <class>)
  <deque>;
end method;


define open abstract class <result-set> (<database-collection>)
  slot liaison :: <function> = identity,
    init-keyword: liaison:;
end class;

define method size-setter(new-size :: <object>, result-set :: <result-set>)
 => (new-size :: <object>)
  error(make(<result-set-mutation-error>));
end method;


define open class <empty-result-set> (<result-set>)
  keyword liaison:, init-value: identity;
end class;


define open abstract class <forward-only-result-set> (<result-set>)
end class;

define open abstract class <scrollable-result-set> (<result-set>)
end class;


define open class <result-set-policy> (<object>)
  constant slot rowset-size :: type-union(<integer>, singleton(#"all")) = 10,
    init-keyword: rowset-size:;

  constant slot scrollable? :: <boolean> = #f,
    init-keyword: scrollable:;

  constant slot scroll-window :: <integer> = 10,
    init-keyword: scroll-window:;

  constant slot asynchronous :: <boolean> = #f,
    init-keyword: asynchronous:;
end class <result-set-policy>;

define constant $default-result-set-policy = make(<result-set-policy>);
define constant $scrollable-result-set-policy = make(<result-set-policy>,
						     scrollable: #t);


define method replace-subsequence!(target :: <result-set>, 
				   insert-sequence :: <sequence>, 
				   #key start :: <integer> = 0, 
				   end: last = unsupplied()) 
 => (result-sequence :: <sequence>)
  error(make(<result-set-mutation-error>));
end method;

define open generic record-available?(result-set :: <result-set>,
                                      key :: <integer>)
 => (availability :: <boolean>);

define method record-available?(result-set :: <result-set>,
                                      key :: <integer>)
 => (availability :: <boolean>)
  //++++ This method should be removed when we handle asynchronous query
  //     execution, and replaced by per-dbms methods.
  #t;
end method;
