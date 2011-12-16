Module:    dfmc-environment-database
Synopsis:  DFM compiler slot information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Slot objects

// Return the name of a slot in a given namespace
define method environment-object-name
    (server :: <dfmc-database>, slot :: <slot-object>,
     namespace :: <namespace-object>)
 => (name :: false-or(<binding-name-object>))
  let variable = source-form-variable(slot.compiler-object-proxy);
  if (variable)
    make-environment-object(<binding-name-object>,
			    project: server.server-project,
			    compiler-object-proxy: variable)
  end
end method environment-object-name;

// Return the class to which a slot belongs
define sealed method slot-class
    (server :: <dfmc-database>, slot :: <slot-object>)
 => (class :: <class-object>)
  let slot-definition = compiler-object-proxy(slot);
  let class-definition = source-form-parent-form(slot-definition);
  //---*** TESTING: can we ever have a slot without a class definition?
  /**/ debug-assert(class-definition, "Class unavailable for slot %=", slot-definition);
  make-environment-object(<class-object>,
                          project: server.server-project,
                          compiler-object-proxy: class-definition)
end method slot-class;

// Return the getter method of a slot
define sealed method slot-getter
    (server :: <dfmc-database>, slot :: <slot-object>)
 => (getter :: false-or(<function-object>))
  let slot-definition = compiler-object-proxy(slot);
  let getter-method = slot-definition-getter(slot-definition);
  getter-method
    & make-environment-object(<method-object>,
                              project: server.server-project,
                              compiler-object-proxy: getter-method)
end method slot-getter;

// Return the setter method of a slot
define sealed method slot-setter 
    (server :: <dfmc-database>, slot :: <slot-object>)
 => (setter :: false-or(<function-object>))
  let slot-definition = compiler-object-proxy(slot);
  let setter-method = slot-definition-setter(slot-definition);
  setter-method
   & make-environment-object(<method-object>,
                             project: server.server-project,
                             compiler-object-proxy: setter-method)
end method slot-setter;

define sealed method slot-type 
    (server :: <dfmc-database>, slot :: <slot-object>)
 => (type :: <environment-object>)
  let slot-definition = compiler-object-proxy(slot);
  let type-expression = slot-definition-type(slot-definition);
  //---*** cpage: 1997.12.18 Testing code you can use to see when the
  //              database uses #f vs. <object>.
  /*
  if (~type-expression)
    debug-out(#"dfmc-environment-database",
              "slot-type: type unavailable for slot %=", slot-definition)
  end;
  */
  make-environment-object-for-type-expression(server, type-expression)
end method slot-type;

define sealed method slot-init-kind
    (server :: <dfmc-database>, slot :: <slot-object>)
 => (kind :: false-or(<symbol>))
  ignore(server);
  slot-definition-init-kind(slot.compiler-object-proxy)
end method slot-init-kind;

define sealed method slot-init-keyword 
    (server :: <dfmc-database>, slot :: <slot-object>)
 => (keyword :: false-or(<symbol>), required? :: <boolean>)
  let (keyword, required?) = slot-definition-keyword(slot.compiler-object-proxy);
  values(keyword, required?)
end method slot-init-keyword;

define sealed method slot-allocation
    (server :: <dfmc-database>, slot :: <slot-object>)
 => (keywords :: <sequence>)
  //---*** cpage: Currently, we merge the results of adjectives and allocation;
  //              perhaps we should have a slot-adjectives protocol, or a
  //              more general foo-adjectives protocol?
  let slot-definition = slot.compiler-object-proxy;
  let adjectives = source-form-adjectives(slot-definition);
  let allocation = slot-definition-allocation(slot-definition);
  add(adjectives, allocation)
end method slot-allocation;
