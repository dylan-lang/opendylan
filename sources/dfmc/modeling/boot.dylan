Module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The boot framework.

define abstract class <&expander> (<object>)
  constant slot expander-definition,
    required-init-keyword: definition:;
  constant slot expander-macro-object,
    required-init-keyword: macro-object:;
end class;

define class <&macro> (<&expander>) 
end class;

define compiler-sideways method compute-form-model-object
    (definition :: <&macro-definition>, variable :: <variable-name-fragment>)
       => (model :: <&macro>)
  make(<&macro>, 
       definition:   definition,
       macro-object: form-macro-object(definition))
end method;

define class <&converter> (<&expander>) 
end class;

define compiler-sideways method compute-form-model-object
    (definition :: <&converter-definition>, 
       variable :: <variable-name-fragment>) 
       => (model :: <&converter>)
  make(<&converter>, 
       definition:   definition,
       macro-object: form-macro-object(definition));
end method;

define class <&definer> (<&expander>) end;

define compiler-sideways method compute-form-model-object
    (definition :: <&definition-definition>,
       variable :: <variable-name-fragment>) 
       => (model :: <&definer>)
  make(<&definer>, 
       definition:   definition,
       macro-object: form-macro-object(definition))
end method;

/// PROXIES FOR MODELS

define class <dood-binding-proxy> (<dood-proxy>)
  constant slot dood-proxy-binding :: <module-binding>, 
    required-init-keyword: binding:;
end class;

define sealed domain make (subclass(<dood-binding-proxy>));
define sealed domain initialize (<dood-binding-proxy>);

define class <dood-binding-value-proxy> (<dood-binding-proxy>)
end class;

define method model-definition (x :: <&expander>) => (res)
  expander-definition(x)
end method;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object) => (proxy)
  make(<dood-binding-value-proxy>, binding: model-variable-binding(object))
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-binding-value-proxy>) => (object)
  with-dood-context (dood-root(dood))
    untracked-binding-model-object-if-computed(dood-proxy-binding(proxy))
  end with-dood-context;
end method;

// define method dood-disk-object 
//     (dood :: <dood>, object :: <&expander>)
//  => (proxy :: type-union(<dood-binding-value-proxy>, <&expander>))
//   dood-as-proxy(dood, object, dood-make-binding-value-proxy)
// end method;

define class <dood-expander-proxy> (<dood-binding-proxy>)
end class;

define method dood-make-expander-proxy
    (dood :: <dood>, object) => (proxy)
  make(<dood-expander-proxy>, binding: model-variable-binding(object))
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-expander-proxy>) => (object)
  with-dood-context (dood-root(dood))
    let binding = dood-proxy-binding(proxy);
    let form    = untracked-binding-definition(binding);
  with-dependent ($top-level-processing of form)
    compute-and-install-form-model-objects(form);
    untracked-binding-model-object-if-computed(binding);
  end with-dependent;
  end with-dood-context;
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <&expander>)
 => (proxy :: type-union(<dood-expander-proxy>, <&expander>))
  dood-as-proxy(dood, object, dood-make-expander-proxy)
end method;

/*
/// BINDING DEFINITIONS

define class <dood-binding-definition-proxy> (<dood-binding-proxy>)
end class;

define method dood-make-binding-definition-proxy
    (dood :: <dood>, object) => (proxy)
  make(<dood-binding-definition-proxy>,
       binding: form-variable-binding(object))
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-binding-definition-proxy>) => (object)
  untracked-binding-definition(dood-proxy-binding(proxy))
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <primitive-definition>)
 => (proxy :: type-union(<dood-binding-definition-proxy>, 
                         <primitive-definition>))
  dood-as-proxy(dood, object, dood-make-binding-definition-proxy)
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <raw-type-definition>)
 => (proxy :: type-union(<dood-binding-definition-proxy>, 
                         <raw-type-definition>))
  dood-as-proxy(dood, object, dood-make-binding-definition-proxy)
end method;
*/
