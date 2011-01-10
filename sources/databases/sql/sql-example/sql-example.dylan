Module:    sql-example
Author:    eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $database-name :: <string> = "northwind";
define constant $user-name :: <string> = "";
define constant $user-password :: <string>  = "";

//---------------  Example 1  ---------------
// This example performs a simple query for the names of companies in
// the USA. The default-coercion policy and forward-only result-set 
// policy are used since these policies are not specified and the liaison
// function will be the copy-sequence generic function.
// - Default coercion: the 'C objects' are converted to the appropriate
//   Dylan object. For this particular query, instances of <C-String> are
//   converted to <byte-string>.
// - Forward-only result-sets: these result-sets (collections) can only be
//   traversed once and must be done in increasing order. 
// - Liaison: the copy-sequence generic function will be used as the liaison
//   function since it isn't specified and the coercion policy is not
//   $no-coercion. For this particular example, results of the call to
//   copy-sequence will be instance of <simple-object-vector> since the
//   coercion-policy is not $no-coercion.

define function display-customers()
  with-database(make(<database>, datasource-name: $database-name),
                make(<user>, user-name: $user-name, password: $user-password))
    let query = make(<sql-statement>, 
                     text: "select CompanyName from customers "
                           "where country = 'USA'");
    let customers-result-set = execute(query);

    format-out("*** USA Companies ***\n");
    do(curry(format-out,"%=\n"), customers-result-set);
    format-out("\n");
  end with-database;
end function;


//----------  Example 2  ----------
// This examples constructs the employee hierarchy by quering for employees
// who do not report to anyone (the reportsTo field is null) and, or each 
// of these 'boss' objects created, query for the subordinates recursively.
// For each employee record retrieved from the database, the liaison function
// employee-maker will create an instance of <employee> using the information
// in the record. The coercion policy is the default-coercion policy since it
// is not specified and the result-set-policy is specified as scrollable.
// The method used by this example to construct the employee hierarchy is not 
// efficient but it does demostrate multiple queries
// - Default coercion: the 'C objects' are converted to the appropriate
//   Dylan object. For this particular query, instances of <C-String> 
//   (lastName and firstName) are converted to <byte-string> and instances of
//   <C-int*> (employeeId) are converted to <integer>. (<C-int*> may be wrong)
// - Scrollable result-sets: these result-sets (collections) may be traversed
//   in any direction any number of times.
// - Liaison: the employee-maker function will be used as the liaison.


define class <employee> (<object>)
  slot id :: <integer>,
    required-init-keyword: id:;

  slot name :: <string>,
    required-init-keyword: name:;

  slot boss :: false-or(<employee>),
    required-init-keyword: boss:;

  slot subordinates :: <deque> = make(<deque>);
end class;

define function construct-employee-hierarchy()
  local method employee-query(boss :: false-or(<employee>))
         => (employee-result-set :: <result-set>)
          local method employee-maker(record :: <record>)
                 => (employee :: <employee>)
                  let (id, last-name, first-name) = apply(values, record);
                  let name = concatenate(last-name, ", ", first-name);
                  let employee = make(<employee>,
                                      id: id,
                                      name: name,
                                      boss: boss);
                  if (boss ~= #f)
                    add!(boss.subordinates, employee)
                  end if;
                  employee
                end method;

          let employee-result-set
            = if (boss = #f)
                execute("select employeeId, lastName, firstName "
                        "from employees "
                        "where reportsTo is null",
                        liaison: employee-maker,
                        result-set-policy: $scrollable-result-set-policy);
              else
                execute("select employeeId, lastName, firstName "
                        "from employees "
                        "where reportsTo = ?",
                        parameters: vector(boss.id),
                        liaison: employee-maker,
                        result-set-policy: $scrollable-result-set-policy);
              end if;
        
          // For each employee in employee-result-set, query for subordinates.
          do(employee-query, employee-result-set);
          employee-result-set;
        end method;

  with-database(make(<database>, datasource-name: $database-name),
                make(<user>, user-name: $user-name, password: $user-password))
    local method print-employee(employee :: <employee>, #key indent = 0)
            let space = make(<string>, size: indent, fill: ' ');
            format-out("%sEmployee's Id:   %=\n", space, employee.id);
            format-out("%sEmployee's Name: %=\n", space, employee.name);
            format-out("%sEmployee's Boss: %=\n\n", space,
                       if (employee.boss ~= #f)
                         employee.boss.name
                       else
                         "None"
                       end if);

            // Recursively call print-employee on each subordinate of employee
            // while increasing the indent by 4.
            do(rcurry(print-employee, indent:, indent + 4), 
               employee.subordinates);
          end method;

    let employee-result-set = employee-query(#f);

    format-out("*** Employees ***\n");
    do(print-employee, employee-result-set);
  end with-database;
end function;

define function sql-example()
  let the-dbms = make(<odbc-dbms>);

  with-dbms(the-dbms)
    display-customers();
    construct-employee-hierarchy();
  end with-dbms;
end function;

block ()
  sql-example();
exception (condition :: <condition>)
  format-out("%=\n", condition);
  signal(condition);
end block;