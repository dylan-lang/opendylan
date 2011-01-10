Module:   employee-explorer
Synopsis: The sql-odbc back-end for the Employee Explorer
Author:   Keith Playford after Ed Cessna
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The liason function to convert result records into corresponding instances 
// of <employee>.

define method employee-maker 
    (boss :: false-or(<employee>), record :: <record>)
 => (employee :: <employee>)
  let (id, last-name, first-name, extension) = apply(values, record);
  let name = concatenate(first-name, " ", last-name);
  let employee = make(<employee>,
                      id: id,
                      first-name: first-name,
                      last-name:  last-name,
                      full-name:  name,
                      extension:  extension,
                      boss: boss);
  employee
end method;

// Called by the Explorer to compute the root set of the employee 
// hierarchy.

define method compute-bosses () => (bosses :: <sequence>)
  let result-set
    = execute("select employeeId, lastName, firstName, Extension "
              "from employees "
              "where reportsTo is null",
              liaison: curry(employee-maker, #f),
              result-set-policy: $scrollable-result-set-policy);
  as(<simple-object-vector>, result-set);
end method;

// Called by the Explorer to compute the subordinates of the given employee.
// The calling employee framework installs the resulting sequence as the
// subordinates value of the employee given.

define method compute-subordinates (boss :: <employee>) => (subs :: <sequence>)
  // format-out("Computing subordinates of: %s\n", last-name(boss));
  let result-set
    = execute("select employeeId, lastName, firstName, Extension "
              "from employees "
              "where reportsTo = ?",
              parameters: vector(boss.id),
              liaison: curry(employee-maker, boss),
              result-set-policy: $scrollable-result-set-policy);
  as(<simple-object-vector>, result-set);
end method;

define method do-with-open-database 
    (datasource :: <byte-string>, 
       user-name :: <byte-string>, user-password :: <byte-string>,
       f :: <function>) 
  with-dbms (make(<odbc-dbms>))
    with-database(make(<database>, datasource-name: datasource),
                  make(<user>, user-name: user-name, password: user-password))
      f();
    end with-database;
  end;
end method;

/*
// A... stubbery!!

define method compute-bosses () => (bosses :: <sequence>)
  map(method (id) 
        make(<employee>, id: id, 
             first-name: "Boss",
             last-name:  format-to-string("Number %d", id),
             boss:       #f)
      end,
      range(from: 1, to: 10));
end method;

define method compute-subordinates (e :: <employee>) => (sub :: <sequence>)
  map(method (id) 
        make(<employee>, id: id, 
             first-name: format-to-string
                           ("Subo of %s %s", first-name(e), last-name(e)),
             last-name:  format-to-string("Number %d", id),
             boss:       e)
      end,
      range(from: 1, to: 5));
end method;
*/
