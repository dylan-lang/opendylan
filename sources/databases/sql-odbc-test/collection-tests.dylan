Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: D-databases-sql-odbc-test!collection-tests.dylan(trunk.6) $


define constant $a-value = as(<integer>, 'a');
define constant $z-value = as(<integer>, 'z');
define constant $a-thru-z-list-size = $z-value - $a-value + 1;
define constant $a-thru-z-list = map(method(num)
					 make(<string>, 
					      size: 1, 
					      fill: as(<character>, num))
				     end method,
				     make(<range>, 
					  from: $a-value, 
					  to: $z-value));
define constant $a-thru-z-list-list = map(list, $a-thru-z-list);

define generic rs-name(rs :: <result-set>) => (name :: <string>);

define method rs-name(rs :: <forward-only-result-set>) => (name :: <string>)
  "<forward-only-result-set>"
end method;

define method rs-name(rs :: <scrollable-result-set>) => (name :: <string>)
  "<scrollable-result-set>"
end method;


define method check-result-set-identity(result-set :: <result-set>,
					coercion-policy,
					scrollable?)
 => (identity-good :: <boolean>)
  let correct-class = if (scrollable? = #t)
			<scrollable-result-set>
		      else
			<forward-only-result-set>
		      end if;
  let correct-class? = instance?(result-set, correct-class);
  
  check-true("Result-set identity test", correct-class?);
  correct-class?;
end method;

define method test-body-aux(sql-text :: <string>,
                            coercion-policy :: <coercion-policy>, 
			    is-scrollable? :: <boolean>) 
 => (result-set :: <result-set>, field-accessor :: <function>)
  let query = make(<sql-statement>, text: sql-text, coercion-policy: coercion-policy);
  let field-accessor = if (coercion-policy = $no-coercion)
		         pointer-value
		       else 
		         identity
		       end if;
  let policy = make(<result-set-policy>, scrollable: is-scrollable?, rowset-size: 1);
  let result-set = execute(query, 
                           result-set-policy: make(<result-set-policy>, 
                                                   scrollable: is-scrollable?, 
                                                   rowset-size: 10));
  check-result-set-identity(result-set, coercion-policy, is-scrollable?);
  values(result-set, field-accessor);
end method;

define macro result-set-test-definer
  { define result-set-test ?test-name:name
        ( ?sql-text:expression )
     ?test-body:body end }
 => 
  {
    define test ?test-name()
      with-connection(*collection-connection*)
        with-transaction()
          let options = vector(vector($no-coercion, #f), 
                               vector($default-coercion, #f),
                               vector($default-coercion, #t));
          do(method(option-instance)
               let (?=coercion-policy, ?=is-scrollable?) = apply(values, option-instance);
               let (?=result-set, ?=field-accessor) 
                 = test-body-aux(?sql-text, ?=coercion-policy, ?=is-scrollable?);
               ?test-body; 
             end method,
             options);
        end with-transaction;
      end with-connection;
    end test; }
end macro;


define result-set-test size-1("select * from dwsql "
				"where col_2 = 0 and col_2 = 1")
  check-equal(format-to-string("Size 1 check on %s", 
			       rs-name(result-set)),
	      size(result-set), 0);
end;



define result-set-test size-2("select * from dwsql")
  let result-set-size = result-set.size;
  check-equal(format-to-string("Size 2 check on %s", 
			       rs-name(result-set)),
	      result-set-size, $a-thru-z-list-size);
end;


define result-set-test size-3("select col_1 from dwsql order by col_1")
  if (is-scrollable?)
    check-equal("Size 3 test", result-set[result-set.size - 1][0], "z")
  end if;
end result-set-test;


define result-set-test size-setter-1("select * from dwsql")
  check-condition(format-to-string("Size 1 setter test on %s",
				   rs-name(result-set)),
		  <result-set-mutation-error>,
		  result-set.size := 3);
end;


define result-set-test empty?-1("select * from dwsql")
  check-true(format-to-string("Empty? test 1 on %s", 
			      rs-name(result-set)),
	     ~empty?(result-set));
end;


define result-set-test empty?-2("select * from dwsql "
				  "where col_2 = 0 and col_2 = 1")
  let is-empty? = result-set.empty?;
  check-true(format-to-string("Empty? test 2 on %s", 
			      rs-name(result-set)),
	     is-empty?); //empty?(result-set));
end;


define result-set-test element-1("select col_2 from dwsql order by col_2")
  let record-1 = element(result-set, 0);
  let field-1 = field-accessor(element(record-1, 0));
  check-equal(format-to-string("Element test on %s: record 1", 
			       rs-name(result-set)),
	      field-1, as(<integer>, 'a'));

  let record-2 = element(result-set, 1);
  let field-2 = field-accessor(element(record-2, 0));
  check-equal(format-to-string("Element test on %s: record 2", rs-name(result-set)),
	      field-2, as(<integer>, 'b'));

  let record-3 = element(result-set, 2);
  let field-3 = field-accessor(element(record-3, 0));
  check-equal(format-to-string("Element test on %s: record 3", rs-name(result-set)),
	      field-3, as(<integer>, 'c'));
end;

define result-set-test element-2("select col_2 from dwsql order by col_2")
  let record-3 = element(result-set, 2);
  let field-1 = field-accessor(element(record-3, 0));

  check-equal(format-to-string("Element test-2 on %s: record 3",
			       rs-name(result-set)),
	      field-1, as(<integer>, 'c'));

  if (is-scrollable? = #t)
    check-true(format-to-string("Element test-2 on %s: revisiting",
				rs-name(result-set)),
	       block ()
		 let record-2 = element(result-set, 1);
		 let field = field-accessor(element(record-2, 0));
		 field = as(<integer>, 'b') 
	       end);
  else
    let not-found = make(<pair>);
    let field = element(result-set, 1, default: not-found);
    check-true(format-to-string("Element test-2 on %s: invalid revisiting", 
				rs-name(result-set)), field = not-found);
  end if;
end;

define result-set-test element-3("select col_2 from dwsql order by col_2")
  check-equal("Element with default test",
	      element(result-set, 30, default: #"ack"),
	      #"ack");
end;

define result-set-test map-1("select col_2 from dwsql where col_2 < 100")
  let result = map(compose(field-accessor, rcurry(element, 0)), 
		      result-set);
  let answer = list(as(<integer>, 'a'), 
		   as(<integer>, 'b'), 
		   as(<integer>, 'c'));
  check-equal("Map-1 test", result, answer);
end;

define result-set-test map-2("select col_2 from dwsql "
			       "where col_2 = 0 and col_2 = 1")
  check-equal("Map-2 test",
	      map(rcurry(element, 0), result-set),
	      #());
end;

define result-set-test map-3("select col_2 from dwsql")
  check-true("Map-3 test", 
	     instance?(map(rcurry(element, 0), result-set),
		       type-for-copy(result-set)));
end;


define result-set-test map-4("select col_2 from dwsql")
  let answer = make(<range>, from: as(<integer>, 'a'), to: as(<integer>, 'z'));
  check-false("Map-4 test", \==(result-set, answer));
end;



define result-set-test map-as-1("select col_2 from dwsql where col_2 < 100")
  let answer = list(as(<integer>, 'a'), 
		    as(<integer>, 'b'), 
		    as(<integer>, 'c'));
  let result = map-as(<list>, 
		      compose(field-accessor, rcurry(element, 0)), 
		      result-set);
  check-true("Map-as test 1 - identity check", instance?(result, <list>));
  check-equal("Map-as test 1", result, answer);
end;

define result-set-test map-as-2("select col_2 from dwsql where col_2 < 100")
  let answer = list(as(<integer>, 'a'), 
		    as(<integer>, 'b'), 
		    as(<integer>, 'c'));
  check-true("Map-as test 2", 
	     \~==(map-as(<list>,
			 compose(field-accessor, rcurry(element, 0)),
			 result-set),
		  answer));
end;


define result-set-test map-into-1("select col_2 from dwsql where col_2 < 100")
  let collection = list('a', 'b', 'c');
  check-equal("Map-into test 1", 
	      map-into(collection, 
		       compose(even?, truncate, field-accessor, 
			       rcurry(element, 0)), 
		       result-set),
	      map-as(<list>, compose(even?, curry(as, <integer>)),
		     #('a', 'b', 'c')));
end;


define result-set-test any?-1("select col_2 from dwsql where col_2 < 100")
  check-true("Any?-1 test", 
	     any?(compose(curry(\=, as(<integer>, 'b')), 
			  field-accessor,
			  rcurry(element, 0)),
		  result-set));
end;

define result-set-test any?-2("select col_2 from dwsql "
				"where col_2 = 1 and col_2 = 0")
  check-false("Any?-2 test", 
	      any?(compose(curry(\=, as(<integer>, 'b')), 
			   field-accessor, rcurry(element, 0)),
		   result-set));
end;


define result-set-test every?-1("select col_2 from dwsql where col_2 < 100")
  check-true("Every?-1 test", 
	     every?(compose(curry(\>, 100), 
			    field-accessor,
			    rcurry(element, 0)),
		    result-set));
end;

define result-set-test every?-2("select * from dwsql "
				  "where col_2 = 1 and col_2 = 0")
  check-true("Every?-2 test",
	     every?(compose(curry(\>, 100),
			    field-accessor,
			    rcurry(element, 0)),
		    result-set));
end;


define result-set-test reduce-1("select col_2 from dwsql")
  let answer = reduce(\+, 0, make(<range>, 
				  from: as(<integer>, 'a'),
				  to: as(<integer>, 'z')));
  check-equal("Reduce-1 test",
	      reduce(method(x, record)
			 x + field-accessor(element(record, 0))
		     end method,
		     0,
		     result-set),
	      answer);
end;

define result-set-test reduce-2("select * from dwsql "
				  "where col_2 = 1 and col_2 = 0")
  check-equal("Reduce-2 test",
	      reduce(method(x, record)
			 x + field-accessor(element(record, 0))
		     end method,
		     0,
		     result-set),
	      0);
end;

define result-set-test member?-1("select col_2 from dwsql")
  check-true("Member?-1 test", 
	     member?(as(<integer>, 's'), result-set,
		     test: method(value, collection-element)
			       value = field-accessor(collection-element[0])
			   end method));
end;

define result-set-test member?-2("select col_2 from dwsql")
  check-false("Member?-2 test",
	      member?(1, result-set,
		      test: method(value, collection-element)
				value = field-accessor(collection-element[0])
			    end method));
end;

define result-set-test member?-3("select * from dwsql "
				   "where col_2 = 1 and col_2 = 0")
  check-false("Member?-3 test", 
	      member?(1, result-set,
		      test: method(value, collection-element)
				value = field-accessor(collection-element[0])
			    end method));
end;

define result-set-test find-key-1("select col_2 from dwsql order by col_2")
  let s-value = as(<integer>, 's');
  let s-key = s-value - as(<integer>, 'a');
  let result = find-key(result-set, method(record)
                                      let field = field-accessor(element(record, 0));
                                      field = s-value;
                                    end method);
  check-equal("Find-key-1 test", result, s-key);
end;

define result-set-test find-key-2("select * from dwsql "
				    "where col_2 = 1 and col_2 = 0")
  check-false("Find-key-2 test", find-key(result-set, empty?));
end;

define result-set-test find-key-3("select col_2 from dwsql order by col_2")
  check-equal("Find-key-3 test",
	      find-key(result-set,
		       method(record)
			   field-accessor(element(record, 0)) = 
			     as(<integer>, 'a')
		       end method),
	      0);
end;


// Since result-sets are immutable (not derived from <mutable-collection>),
// the following methods are not defined on it:
//    replace-elements! 
//    fill!
//    element-setter
//    add!
//    remove  (double check this one)
//    remove!


define result-set-test key-sequence-1("select col_2 from dwsql "
					"where col_2 < 100 order by col_2")
  check-true("Key sequence test 1",
	     key-sequence(result-set) = #(0, 1, 2));
end;


// Can't use the following method on a result-set since they access each
// element more than once:
//    add
//    add-new


define result-set-test choose-1("select col_2 from dwsql order by col_2")
  // choose will not work correctly on instances of <forward-only-result-set>.
  // Calling choose on such a result-set will result in a collection of the
  // proper size but each element of the collection will be the same (ie,
  // the last record retrieved from the database).

  // field elements are truncated in case floating-point values are returned
  // instead of integers (ODBC does this)

  let element-access = compose(truncate, field-accessor, rcurry(element, 0));
  let choose-result = choose(compose(even?, element-access), result-set);
  let result = map-as(<deque>, element-access, choose-result);

  let answer = if (coercion-policy == $no-coercion)
		 make(<deque>, size: 13, fill: $z-value);
	       else
		 choose(even?, make(<range>, from: $a-value, to: $z-value));
	       end if;
  check-equal("Choose test 1", result, answer);
end;


define result-set-test choose-2("select col_2 from dwsql "
				  "where col_2 = 1 and col_2 = 0")
  check-equal("Choose test 2",
	      choose(compose(even?, rcurry(element, 0)), result-set),
	      #());
end;


define result-set-test choose-by-1("select col_2 from dwsql order by col_2")
  let result = choose-by(even?, range(from: 1, to: 26), result-set);
  let answer = if (coercion-policy == $no-coercion)
		 make(<deque>, size: 13, fill: $z-value)
	       else
		 choose-by(even?, 
			   range(from: 1, to: 26),
			   range(from: $a-value, to: $z-value))
	       end if;

  check-true("Choose by test 1", 
	     every?(method(result-record, answer-item)
			let element-access = compose(truncate, 
						     field-accessor, 
						     rcurry(element, 0));
			element-access(result-record) = answer-item;
		    end method,
		    result,
		    answer));
end;

define result-set-test choose-by-2("select col_2 from dwsql "
				     "where col_2 = 1 and col_2 = 0")
  check-equal("Choose by test 2",
	      choose-by(even?, #(), result-set),
	      #());
end;

define result-set-test choose-by-3("select col_2 from dwsql "
				     "where col_2 = 1 and col_2 = 0")
  check-equal("Choose by test 3",
	      choose-by(even?, result-set, #()),
	      #());
end;


define result-set-test intersection-1("select col_2 from dwsql order by col_2")
  let element-access = compose(truncate, field-accessor, rcurry(element, 0));
  let answer = if (coercion-policy = $no-coercion)
		 make(<list>, size: 3, fill: $z-value)
	       else
		 list(as(<integer>, 'm'),
		      as(<integer>, 'n'),
		      as(<integer>, 'o'))
	       end if;
  let new-set = intersection(result-set, answer, 
			     test: method (record, item)
				     element-access(record) = item
				   end method);

  check-true("Intersection test 1",
	     every?(method (new-set-record)
		      let field = element-access(new-set-record);
		      member?(field, answer)
		    end method,
		    new-set));
end;

define result-set-test intersection-2("select col_2 from dwsql "
					"where col_2 = 1 and col_2 = 0")
  check-true("Intersection test 2",
	     empty?(intersection(result-set, #())));
end;


define result-set-test union-1("select col_1 from dwsql "
				 "where col_2 < 99")
  let answer = #("a", "b", "c", "x", "y", "z");
  let new-set = union(result-set, #(#("x"), #("y"), #("z")));

  check-true("Union test 1",
	     every?(method (new-set-record)
		      member?(new-set-record[0], answer, test: \=)
		    end method,
		    new-set));
end;

/* need test for 
     remove-duplicates
     remove-duplicates!
     copy-sequence
*/


define result-set-test concatenate-1("select col_1 from dwsql "
				       "where col_1 < 'd'")
  let ending = #(#("x"), #("y"), #("z"));
  let heading = #(#("a"), #("b"), #("c"));
  if (is-scrollable?)
    let result = concatenate(result-set, ending);
    check-equal("Concatenate 1 test", 
		result, 
		concatenate(heading, ending));
  else
    check-condition("Concatenate 1 test",
		    <data-not-available>,
		    concatenate(result-set, ending));
  end if;
end;


define result-set-test concatenate-as-1("select col_1 from dwsql "
					  "where col_1 < 'd'")
  if (is-scrollable?)
  let answer = #("a", "b", "c", "x", "y", "z");

  let result = concatenate-as(<list>, 
			      result-set, 
			      #(#("x"), #("y"), #("z")));

  check-true("Concatenate-as 1 test",
	     instance?(result, <list>) &
	       every?(method(record)
			  let field = record[0];
			  member?(field, answer, test: \=)
		      end method,
		      result));
  else
      check-condition("Concatenate-as 1 test",
		      <data-not-available>,
		      concatenate-as(<list>, 
				     result-set, 
				     #(#("x"), #("y"), #("z"))));
  end if;
end;


define result-set-test replace-subsequence!-1 ("select col_1 from dwsql")
  check-condition("replace-subsequence! 1 test",
		  <result-set-mutation-error>, 
		  replace-subsequence!(result-set, #("x", "y", "z"), 
				       end: 1));
end;


define result-set-test reverse-1("select col_1 from dwsql order by col_1")
  if (coercion-policy ~= $no-coercion)
    check-equal("reverse 1 test", 
		reverse(result-set), 
		reverse($a-thru-z-list-list))
  end if;
end;


define result-set-test sort-1("select col_1 from dwsql order by col_1 desc")
  // sorting will only work on scrolling result-sets.
  if (is-scrollable? = #t)
    let result = sort(result-set,
		      test: method(a, b) 
				a[0] < b[0] 
			    end method);

    check-equal("Sort test 1", result, $a-thru-z-list-list);
  end if;
end;


define result-set-test first-1("select col_1 from dwsql order by col_1")
  let answer = if(coercion-policy = $no-coercion) 'a' else "a" end if;
  let first-record = first(result-set);
  check-equal("First test 1", 
	      field-accessor(element(first-record, 0)), 
	      answer);
end;

define result-set-test last-1("select col_1 from dwsql order by col_1")
  if (is-scrollable?)
    let last-record = last(result-set);
    check-equal("Last test 1", last-record[0], "z");
  end if;
end;

define result-set-test last-2("select * from dwsql "
				"where col_2 = 0 and col_2 = 1")
  check-true("Last test 2", last(result-set, default: #"ack") = #"ack");
end;


define result-set-test subsequence-position-1("select col_2 from dwsql "
						"order by col_2")
//  check-false("subsequence-position test not implemented! "
//		"Need a damn debugger!!", #t);
/*  if (is-scrollable?)
    check-equal("Subsequence-position test 1",
		subsequence-position(result-set, #("l", "m", "n"),
				     test: method (record, pattern) 
					     record[0] = pattern 
					   end method),
		as(<integer>, 'l') - as(<integer>, 'a'));
  end if;*/
end;



define variable *collection-connection* = #f;

define method create-collection-test-table()
  with-connection(*collection-connection*)
    let statement = make(<sql-statement>,
			 text: "create table dwsql (col_1 varchar(1), "
			   "col_2 number, col_3 number)",
			 input-indicator: $null-value);
    execute(statement);

    statement.text := "insert into dwsql(col_1, col_2, col_3) values(?, ?, ?)";
    for (i from as(<integer>, 'a') to as(<integer>, 'z'))
      execute(statement,
	      parameters: vector(as(<character>, i), i, 
				 if (even?(i)) i else $null-value end if));
    end for;
  end with-connection;
end method;

define method collection-test-setup()
  with-dbms(*the-dbms*)
    let database = make(<database>, datasource-name: *datasource-name*);
    let user = make(<user>, user-name: *user-name*, password: *user-password*);
    *collection-connection* := connect(database, user);
    create-collection-test-table();
  end with-dbms;
end method;

define method collection-test-cleanup()
  with-connection(*collection-connection*)
    execute("drop table dwsql");
  end with-connection;
  disconnect(*collection-connection*);
  *collection-connection* := #f;
end method;

define suite collection-test-suite(setup-function: collection-test-setup,
				   cleanup-function: collection-test-cleanup)
  test size-1;
  test size-2;
  test size-3;
  test size-setter-1;

  test empty?-1;
  test empty?-2;

  test element-1;
  test element-2;
  test element-3;

  test map-1;
  test map-2;
  test map-3;
  test map-4;

  test map-as-1;
  test map-as-2;

  test map-into-1;

  test any?-1;
  test any?-2;

  test every?-1;
  test every?-2;

  test reduce-1;
  test reduce-2;

  test member?-1;
  test member?-2;
  test member?-3;

  test find-key-1;
  test find-key-2;
  test find-key-3;

  test key-sequence-1;

  test choose-1;

  test choose-2;

  test choose-by-1;
  test choose-by-2;
  test choose-by-3;

  test intersection-1; 
  test intersection-2;

  test union-1;

  test concatenate-1;

  test concatenate-as-1;

  test replace-subsequence!-1; 

  test reverse-1;

  test sort-1;

  test first-1;

  test last-1;
  test last-2;

  test subsequence-position-1;
end suite;

