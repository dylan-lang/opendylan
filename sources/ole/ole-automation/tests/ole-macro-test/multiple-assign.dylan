Module: ole-macro-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro multiple-assign
  { 
    multiple-assign((?vars:*) := ?:expression)
  } => {
    multiple-assign-help-1((), %%-temp, (?vars), (?vars) := ?expression)
  }
end macro multiple-assign;

define macro multiple-assign-help-1
  { multiple-assign-help-1((?temps:*), ?:name, (), (?vars:*) := ?:expression)
  } => {
    let (?temps) = ?expression;
    multiple-assign-help-2((?vars) := (?temps)) 
  }

  { multiple-assign-help-1((), ?:name, (?var:expression, ?rest:*), 
			   (?vars:*) := ?:expression)
  } => {
    multiple-assign-help-1((?name), "%" ## ?name, (?rest), 
			   (?vars) := ?expression) 
  }

  { multiple-assign-help-1((?temps:*), ?:name, (?var:expression, ?rest:*), 
			   (?vars:*) := ?:expression)
  } => {
    multiple-assign-help-1((?temps, ?name), "%" ## ?name, (?rest), 
			   (?vars) := ?expression) 
  }
end macro multiple-assign-help-1;

define macro multiple-assign-help-2
  { multiple-assign-help-2(() := ())
  } => { }

  { multiple-assign-help-2((?var:*, ?vars:*) := (?temp:*, ?temps:*))
  } => { 
    ?var := ?temp;
    multiple-assign-help-2((?vars) := (?temps))
  }
end macro multiple-assign-help-2;

