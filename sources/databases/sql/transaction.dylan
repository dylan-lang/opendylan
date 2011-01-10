Module: sql-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql!transaction.dylan(trunk.6) $


define open generic default-transaction-mode(connection :: <connection>)
 => (mode :: <transaction-mode>);

define open generic default-isolation-level(connection :: <connection>)
  => (level :: <isolation-level>);

define open generic default-diagnostics-size(connection :: <connection>) 
 => (diagnostics-size :: <integer>);


define open generic start-transaction(connection :: <connection>, 
				      transaction-mode :: <transaction-mode>, 
				      isolation-level :: <isolation-level>, 
				      diagnostics-size :: <integer>)
 => (transaction :: <transaction>);

define open generic end-transaction(transaction :: <transaction>)
 => ();

define open generic rollback-transaction(transaction :: <transaction>)
 => ();

define open generic commit-transaction(transaction :: <transaction>)
 => ();



define macro with-transaction
  { with-transaction(#key ?rollback:name = ignore-rollback, 
		     ?commit:name = ignore-commit,
		     ?autocommit:expression = #t,  // if #f, rollback
		     ?transaction-mode:name = not-supplied,
		     ?isolation-level:name = not-supplied,
		     ?diagnostics-size:expression = $unfound,
		     ?connection:expression = $unfound)
       ?transaction-body:body
     rollback ?rollback-body:body
    end }
  => { 
       let connection = if (?connection == $unfound)
			  default-connection()
			else
			  ?connection
			end if;
       let diagnostics-size = if (?diagnostics-size == $unfound)
				default-diagnostics-size(connection)
			      else
				?diagnostics-size
			      end if;
       let transaction-mode = if (?transaction-mode == $unfound)
				default-transaction-mode(connection)
			      else
				?transaction-mode
			      end if;
       let isolation-level = if (?isolation-level == $unfound)
			       default-isolation-level(connection)
			     else
			       ?isolation-level
			       end if;
       let transaction = #f;

       with-connection(connection)
         block (exit)
	   transaction := start-transaction(connection, transaction-mode, 
					    isolation-level, diagnostics-size);
	   let ?rollback = compose(exit, 
				   curry(rollback-transaction, transaction));
	   let ?commit = compose(exit, 
				 curry(commit-transaction, transaction));

	   ?transaction-body

         afterwards
	   if (?autocommit)
	     commit-transaction(transaction);
	     exit();
	   else 
	     rollback-transaction(transaction);
	     exit();
	   end if;

         cleanup 
	   if (transaction ~= #f)
	     end-transaction(transaction);
	   end if;

         exception (condition :: <transaction-rollback>)
           ?rollback-body
         end block
       end; }

  { with-transaction(#key ?rollback:name = ignore-rollback, 
		     ?commit:name = ignore-commit,
		     ?autocommit:expression = #t,  // if #f, rollback
		     ?transaction-mode:name = not-supplied,
		     ?isolation-level:name = not-supplied,
		     ?diagnostics-size:expression = $unfound,
		     ?connection:expression = $unfound)
       ?transaction-body:body 
    end }
  => { 
       let connection = if (?connection == $unfound)
			  default-connection()
			else
			  ?connection
			end if;
       let diagnostics-size = if (?diagnostics-size == $unfound)
				default-diagnostics-size(connection)
			      else
				?diagnostics-size
			      end if;
       let transaction-mode = if (?transaction-mode == $unfound)
				default-transaction-mode(connection)
			      else
				?transaction-mode
			      end if;
       let isolation-level = if (?isolation-level == $unfound)
			       default-isolation-level(connection)
			     else
			       ?isolation-level
			       end if;
       let transaction = #f;

       with-connection(connection)
         block (exit)
	   transaction := start-transaction(connection, transaction-mode, 
					    isolation-level, diagnostics-size);
	   let ?rollback = compose(exit, 
				   curry(rollback-transaction, transaction));
	   let ?commit = compose(exit, 
				 curry(commit-transaction, transaction));

	   ?transaction-body

         afterwards
	   if (?autocommit)
	     commit-transaction(transaction);
	   else 
	     rollback-transaction(transaction);
	   end if;

         cleanup 
	   if (transaction ~= #f)
	     end-transaction(transaction);
	   end if;
         end block
       end; }


  transaction-mode:
    { read-only } => { $read-only }
    { read-write } => { $read-write }
    { not-supplied } => { $unfound }

  isolation-level:
    { read-uncommitted } => { $read-uncommitted }
    { read-committed } => { $read-committed }
    { repeatable-read } => { $repeatable-read }
    { serializable } => { $serializable }
    { not-supplied } => { $unfound }
end macro;


