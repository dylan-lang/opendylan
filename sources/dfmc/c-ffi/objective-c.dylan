Module:    dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function c-ffi-default-selector-inline-policy () => (policy)
  #{ inline-only }
end function;

define &macro objc-selector-definer
  { define ?mods:* objc-selector ?dylan-name:name ?spec:* end }
    =>
  begin
    let (arg-specs, result-spec, selector, options)
      = parse-c-function-spec(dylan-name, spec, name-symbol: #"selector");
    let (arg-fragments, result-fragment, parameter-list-fragment,
         return-values-fragment, define-gf?, parameter-names-fragment)
      = parse-early-options(arg-specs, result-spec, options, dylan-name);
    let inline-policy = mods;
    let body = #{ begin
                    if (?=$trace-ffi-calls)
                      // we depend on folding one of the branches for
                      // performance reasons, so $trace-ffi-calls better be
                      // a constant.  apply(values, ...)
                      // isn't optimized, that's why the whole objc-msgsend-body
                      // code segment is duplicated here.
                      ?=log-entry(?selector, ?parameter-names-fragment);
                      let (#rest results) = objc-msgsend-body
                                               ?dylan-name
                                               (selector ?selector),
                                               (options ??options, ...),
                                               ?result-fragment,
                                               ??arg-fragments, ...
                                             end;
                      apply(?=log-exit, ?selector, results);
                      apply(values, results)
                    else
                      objc-msgsend-body
                        ?dylan-name
                        (selector ?selector),
                        (options ??options, ...),
                        ?result-fragment,
                        ??arg-fragments, ...
                      end
                    end
                  end
                };
    if (define-gf?)
      #{ define constant ?dylan-name = ?=objc/register-selector(?selector);
         define ?inline-policy method "%send-" ## ?dylan-name ?parameter-list-fragment
          => ?return-values-fragment;
           ?body
         end }
    else
      #{ define constant ?dylan-name = ?=objc/register-selector(?selector);
         define ?inline-policy function "%send-" ## ?dylan-name ?parameter-list-fragment
          => ?return-values-fragment;
           ?body
         end }
    end if;
  end

mods:
    { } => c-ffi-default-selector-inline-policy();
    { ?other:* } => #{ ?other };

spec:
    { } => #();
    { ?stuff:*; ... } => pair(stuff, ...);
end &macro;

define &macro objc-msgsend-body
  { objc-msgsend-body ?function-name:name
     (selector ?selector:expression),
     (options ?key-options:*),
     ?result-spec:*,
     ?args:*
  end }
  => expand-objc-msgsend-body(form, function-name, result-spec, args,
                              key-options);
args:
  { } => #();
  { ?arg:*, ...} => pair(arg, ...);
arg:
  { (parameter ?arg-name:name :: ?type:expression,
     call-discipline: ?discipline:expression,  ?key-options:*) }
  => apply(make,
           <c-ffi-argument-descriptor>,
           name: arg-name,
           designator-name: type,
           // !@#$ fragment-value bogosity
           //      should probably be able to to as(<symbol>, discipline)
           call-discipline: as(<symbol>, fragment-value(discipline)),
           key-options);
result-spec:
  { (result void) }
  => make(<c-ffi-result-descriptor>,
          name: gensym(),
          void?: #t);
  { (result ?result-name:name :: ?type:expression, ?key-options:*) }
  => apply(make,
           <c-ffi-result-descriptor>,
           name: result-name,
           designator-name: type,
           key-options);
key-options:
   { } => #()
   { ?key:expression, ?value:expression, ... }
           // !@#$ fragment-value bogosity
           //      should probably be able to to as(<symbol>, discipline)
    => pair(as(<symbol>, fragment-value(key)), pair(value, ...));
end;

define method expand-objc-msgsend-body
    (form :: <fragment>,
     dylan-name :: <variable-name-fragment>,
     result-desc :: <c-ffi-result-descriptor>,
     arg-specs :: <sequence>,
     options :: <sequence>)
 => (expansion);

  let result-designator
    = ~void?(result-desc) & ^eval-designator(result-desc.designator-name);
  unless (void?(result-desc)
          | designator-class?(result-designator))
    generate-unresolved-designator-error(result-desc.designator-name,
      dylan-name, #{ c-function-result }, #());
    result-designator := #f;
  end unless;

  do(method (desc)
       desc.model-type := ^eval-designator(desc.designator-name);
       unless (designator-class?(desc.model-type))
         generate-unresolved-designator-error(desc.designator-name,
           dylan-name, #{ c-function-parameter }, #());
         desc.model-type := ^eval-designator(#{ <C-void*> });
       end unless;
     end,
     arg-specs);

  let (dylan-function-parameter-list,
       dylan-function-extra-returns-list,
       stack-allocation-heads,
       in-out-arg-set-forms,
       c-function-parameter-list,
       c-function-arguments,
       extra-return-values)
    = c-function-parse-input-output-parameters(dylan-name, arg-specs);

  let selector-expr = get-property(options, #"selector");
  let selector = #f;
  if (selector-expr)
    selector := ^top-level-eval(selector-expr);
    unless (instance?(selector, <string>))
      note(<invalid-selector-value>,
           source-location: fragment-source-location(selector-expr),
           definition-name: dylan-name,
           selector-expression: selector-expr);
      selector := #f;
    end unless;
  end if;

  unless (selector)
    note(<missing-selector>,
         source-location: fragment-source-location(form),
         definition-name: dylan-name);
    selector := "dummy_selector";
  end unless;

  let result-boxer
    = result-designator & result-designator.^boxer-function-name;
  let result-name = if (result-designator)
                      result-desc.name;
                    else
                      #{ tmp }
                    end if;
  let result-dylan-type
    = result-designator & result-designator.^mapped-import-type;
  let result-raw-type = if (result-designator)
                          result-designator.^raw-type-name;
                        else
                          #{ <raw-c-void> }
                        end if;
  let result-low-type = if (result-designator)
                          result-designator.^low-level-type
                        else
                          #f
                        end if;
  let import-function
    = result-designator
      & (result-designator.^import-function
           | #{ identity });
  let return-values =
    if (result-designator)
      pair( #{ ?result-name }, as(<list>, extra-return-values));
    else
      extra-return-values
    end;

  let modifiers-expr = get-property(options, #"c-modifiers", default: #f);
  // this allows it to be a named constant or macro
  let modifiers
    = if (modifiers-expr)
        let val = ^top-level-eval(modifiers-expr);
        if (~instance?(val, <string>))
          note(<invalid-c-modifiers-value>,
               source-location: fragment-source-location(modifiers-expr),
               definition-name: dylan-name,
               modifiers-expression: modifiers-expr);
          "";
        else
         val;
       end if;
      else
        ""
      end;

  // We extract the first argument from these vectors. The first argument
  // is passed as the target object.
  let (target, c-function-parameter-list, c-function-arguments)
    = if (~empty?(c-function-arguments))
        values(c-function-arguments.first,
               copy-sequence(c-function-parameter-list, start: 1),
               copy-sequence(c-function-arguments, start: 1))
      else
        note(<missing-selector-target>,
             source-location: fragment-source-location(form),
             definition-name: dylan-name);
        values(#f, c-function-parameter-list, c-function-arguments)
      end if;

  let foreign-call
    = #{ %objc-msgsend (?target, ?=primitive-unwrap-c-pointer(?dylan-name), c-modifiers: ?modifiers)
            (??c-function-parameter-list, ...)
         => (?result-name :: ?result-raw-type)
          (??c-function-arguments, ...)
        end };
  let result-binding
    = if (result-designator)
        #{ let ?result-name :: ?result-dylan-type
            = ?import-function (boxer-for-designator
                                  (?result-low-type,
                                   ?foreign-call,
                                   ?result-boxer)) };
      else
        #{ ?foreign-call }
      end if;
  let stack-allocation-size = size(stack-allocation-heads);
  local method build-with-stack-body (i ::<integer>, body)
          if (i >= stack-allocation-size)
            body
          else
            let stack-head = stack-allocation-heads[i];
            build-with-stack-body
              (i + 1,
               #{ with-stack-block ?stack-head
                   ?body
                  end })
          end if;
        end method;
  let inner-body
    = build-with-stack-body(0,
                            #{ ??in-out-arg-set-forms  ...
                                ?result-binding;
                              values(??return-values, ...) });
  inner-body;
end;
