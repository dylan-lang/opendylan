module: dfmc-flow-graph
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// DFM COPYING POLICY

define compiler-open class <dfm-copier> (<copier>) end class;

define dont-copy-object <source-location>       using <dfm-copier>;
define dont-copy-object <fragment>              using <dfm-copier>;
define dont-copy-object <&top>                  using <dfm-copier>;
define dont-copy-object <&primitive>            using <dfm-copier>;
define dont-copy-object <&definition>           using <dfm-copier>;
define dont-copy-object <top-level-form>        using <dfm-copier>;
define dont-copy-object <top-level-environment> using <dfm-copier>;
define dont-copy-object <module-binding>        using <dfm-copier>;
define dont-copy-object <mapped-unbound>        using <dfm-copier>;
define dont-copy-object <&namespace>            using <dfm-copier>;
define dont-copy-object <&mm-wrapper>           using <dfm-copier>;
define dont-copy-object <&singular-terminal-engine-node> using <dfm-copier>;

// define dont-copy-slots  <dood-mapped-object>           using <dfm-copier> =
//   { dood-pointer  => #f };

define dont-copy-slots  <dood-mapped-and-owned-object> using <dfm-copier> =
  { object-dood  => #f };

define dont-copy-slots  <queueable-item-mixin>       using <dfm-copier> =
  { item-status => $queueable-item-absent };

define dont-copy-slots  <computation>                using <dfm-copier> =
  { computation-source-location => parent-source-location(),
    computation-type => #f,
    %node-id => #f };

define dont-copy-slots  <bind-exit>                  using <dfm-copier> =
  { %label => #f };

define dont-copy-slots  <loop>                       using <dfm-copier> =
  { %label => #f };

define dont-copy-slots  <model-properties>           using <dfm-copier> =
  { private-model-creator => (*current-dependent* |
     error("Attempt to copy a model outside of proper compilation-context")) };

define dont-copy-slots  <emitted-object>             using <dfm-copier> =
  { emitted-name      => #f,
    emitted-type-name => #f };

define dont-copy-slots  <&lambda>                    using <dfm-copier> =
  { lambda-heap            => #f,
    // private-signature-spec => #f,
    // private-body-spec      => #f
   };

define dont-copy-slots  <&iep>                       using <dfm-copier> =
  { code              => #f };

define dont-copy-slots  <temporary>                  using <dfm-copier> =
  { %node-id     => #f };
define dont-copy-slots  <object-reference>           using <dfm-copier> =
  { %node-id     => #f };
// If a call checked incompatible out of line, it might still become
// compatible inline (and so amenable to upgrading and inlining), so
// we reset its state in the inline copy.

define method do-deep-copy
    (copier :: <dfm-copier>, object :: <queueable-item-mixin>) => (value)
  let copy = next-method();
  copy.item-status := $queueable-item-absent;
  copy
end method;

define method do-deep-copy
    (copier :: <dfm-copier>, object :: <call>) => (value)
  let copy = next-method();
  if (copy.compatibility-state == $compatibility-checked-incompatible)
    copy.compatibility-state := $compatibility-unchecked;
  end;
  copy
end method;

define method do-deep-copy
    (copier :: <dfm-copier>, object :: <simple-call>) => (value)
  let copy = next-method();
  copy.call-inlining-depth := *inlining-depth*;
  copy
end method;

define method do-deep-copy
    (copier :: <dfm-copier>, object :: <module-binding-reference>) => (value)
  let copy = next-method();
  referenced-binding(copy)
    := local-binding-in-requesting-library(referenced-binding(copy));
  copy
end method;

define method do-deep-copy
    (copier :: <dfm-copier>, object :: <lambda-lexical-environment>) => (value)
  let copy = next-method();
  remove-closure!(copy);
  copy
end method;

define method do-deep-copy
    (copier :: <dfm-copier>, object :: <&profiling-call-site-cache-header-engine-node>) => (value)
  let copy = next-method();
  let ld   = model-library(copy);
  ^profiling-call-site-cache-header-engine-node-id(copy)
    := library-generate-call-site-id(ld);
  ^profiling-call-site-cache-header-engine-node-library(copy)
    := namespace-model(language-definition(ld));
  copy
end method;


define method deep-copy
    (copier :: <dfm-copier>, object :: <dood-slot-value-proxy>) => (value)
  deep-copy(copier, dood-force-slot-value-proxy(object))
end method;

define method deep-copy
    (copier :: <dfm-copier>, object :: <&object>) => (value)
  if (model-has-definition?(object))
    object // reference by name
  else
    maybe-do-deep-copy(copier, object)
  end if
end method;

define method deep-copy
    (copier :: <dfm-copier>, object :: <&lambda>) => (value)
  let m = next-method();
  if (m == object) // maybe in process of being inline-only copied?
    element(walker-walked(copier), object, default: #f) | m
  else
    m
  end if
end method;

define thread variable *dfm-copier-environment-context* = #f;

define macro with-dfm-copier-environment
  { with-dfm-copier-environment (?:expression) ?:body end }
    => { dynamic-bind (*dfm-copier-environment-context* = ?expression)
           ?body
         end dynamic-bind }
end macro;

define method do-deep-copy
    (copier :: <dfm-copier>, object :: <exit>) => (value)
  let copy = next-method();
  if (*dfm-copier-environment-context*)
    let state = entry-state(object);
    exits(state) := add-new!(exits(state), copy);
    dynamic-bind (*computation-tracer* = #f)
      add-user!(state, copy);
    end
  end if;
  copy
end method;

define method deep-copy
    (copier :: <dfm-copier>, object :: <lexical-environment>) => (value)
  let copying-environment = *dfm-copier-environment-context*;
  if (~copying-environment
        | (copying-environment &
            inner-environment?(object, copying-environment)))
    maybe-do-deep-copy(copier, object);
  else
    object
  end if;
end method;

define method deep-copy
    (copier :: <dfm-copier>, object :: <computation>) => (value)
  let copying-environment = *dfm-copier-environment-context*;
  if (~copying-environment
        | (copying-environment &
            inner-environment?(environment(object), copying-environment)))
    maybe-do-deep-copy(copier, object);
  else
    object
  end if;
end method;

define method deep-copy
    (copier :: <dfm-copier>, object :: <temporary>) => (value)
  let copying-environment = *dfm-copier-environment-context*;
  if (~copying-environment
        | (copying-environment &
            inner-environment?(environment(object), copying-environment)))
    maybe-do-deep-copy(copier, object);
  else
    object
  end if;
end method;

// TODO: This is very bad because of the number of non-modeled vectors
// in the DFM representation.
/*
define method deep-copy
    (copier :: <dfm-copier>, object :: <simple-object-vector>)
 => (copy :: <simple-object-vector>)
  if (model-has-definition?(object))
    object
  else
    let copy = next-method();
    copy
  end;
end method;
*/

// TODO: We have to do this for real at some point, on all model vectors.
// For now, this handles manually mapping the type vectors in the
// signatures of non-key methods (enough for the FFI).
define method do-deep-copy
    (copier :: <dfm-copier>, object :: <&signature>)
 => (value :: <&signature>)
  let copy = next-method();
  let copy-required = ^signature-required(copy);
  if (copy-required ~== ^signature-required(object))
    mapped-model(copy-required);
  end;
  let copy-values = ^signature-values(copy);
  if (copy-values ~== ^signature-values(object))
    mapped-model(copy-values);
  end;
  copy
end method;

define method do-deep-copy
    (copier :: <dfm-copier>, object :: <&keyword-signature>)
 => (value :: <&signature>)
  let copy = next-method();
  let copy-keys = ^signature-keys(copy);
  if (copy-keys ~== ^signature-keys(object))
    mapped-model(copy-keys);
  end;
  let copy-key-types = ^signature-key-types(copy);
  if (copy-key-types ~== ^signature-key-types(object))
    mapped-model(copy-key-types);
  end;
  copy
end method;

// Unlike primitves, of which they're a slightly suspect subclass,
// raw c-function objects don't have definitions of their own and
// must be copied.
define method deep-copy
    (copier :: <dfm-copier>, object :: <&c-function>) => (value)
  maybe-do-deep-copy(copier, object)
end method;

define method deep-copy
    (copier :: <dfm-copier>, object :: <&raw-object>) => (value)
  maybe-do-deep-copy(copier, object)
end method;

define method deep-copy
    (copier :: <dfm-copier>, object :: <&code>) => (value)
  if (model-has-definition?(function(object)))
    object // reference by name
  else
    maybe-do-deep-copy(copier, object)
  end if
end method;

define method number-temporaries (f :: <&lambda>) => (res :: <integer>)
  number-temporaries(environment(f))
end method;

define function estimated-copier-table-size (f :: <&lambda>) => (res :: <integer>)
  number-temporaries(f) * 15
end function;

define function current-dfm-copier (capacity :: <integer>) => (res :: <dfm-copier>)
  copier-reset
    (library-description-dfm-copier(dylan-library-description())
       | (library-description-dfm-copier(dylan-library-description())
            := make(<dfm-copier>)),
     capacity: capacity);
end function;
