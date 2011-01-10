Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// constants
define constant $zero-date = make(<date>, year: 1900, month: 1, day: 1);

define constant <build-status> = limited(<integer>, min: 0, max: 11);

define constant $build-status-init    :: <build-status> = 0;
define constant $build-status-making  :: <build-status> = 1;
define constant $build-status-stable  :: <build-status> = 2;
define constant $build-status-newer   :: <build-status> = 3;
define constant $build-status-temp    :: <build-status> = 4;
define constant $build-status-touched :: <build-status> = 5;
define constant $build-status-missing :: <build-status> = 6;
define constant $build-status-needtmp :: <build-status> = 7;
define constant $build-status-old     :: <build-status> = 8;
define constant $build-status-update  :: <build-status> = 9;
define constant $build-status-nofind  :: <build-status> = 10;
define constant $build-status-nomake  :: <build-status> = 11;

/*
define constant $build-status-names
  = #["init", "making", "stable", "newer", "temp", "touched",
      "missing", "needtmp", "old", "update", "nofind", "nomake"];
*/

define constant <build-progress>
  = one-of(#"init", #"onstack", #"active", #"running", #"done");



// <jam-target> - internal class
//
// Stores information about each known target.
//
define class <jam-target> (<object>)
  constant slot target-name :: <string>,
    required-init-keyword: name:;
  
  // bound target location
  slot target-bound-locator :: false-or(<physical-locator>),
    init-value: #f, init-keyword: bound-locator:;

  // modification date for files
  slot target-modification-date :: false-or(<date>),
    init-value: #f;

  // modification date for leaf sources
  slot target-leaf-date :: <date>,
    init-value: $zero-date;

  // build status
  slot target-build-status :: <build-status>,
    init-value: $build-status-init;

  // build progress
  slot target-build-progress :: <build-progress>,
    init-value: #"init";

  // build command execution result
  slot target-build-execution-result :: one-of(#"ok", #"fail", #"interrupt"),
    init-value: #"ok";
  
  // target-specific (on) variables
  constant slot target-variables :: <string-table> = make(<string-table>);

  // target dependencies
  constant slot target-depends :: <stretchy-vector> = make(<stretchy-vector>);

  // include dependency pseudo-target
  slot target-includes-target :: false-or(<jam-target>), init-value: #f;
  constant slot target-internal? :: <boolean>,
    init-value: #f, init-keyword: internal?:;
  
  // build action invocations
  constant slot target-action-invocations :: <stretchy-vector>
    = make(<stretchy-vector>);

  // target flags
  slot target-always-build? :: <boolean>, init-value: #f;
  slot target-leaf-only? :: <boolean>, init-value: #f;
  slot target-allow-nonexistent? :: <boolean>, init-value: #f;
  slot target-file? :: <boolean>, init-value: #t;
  slot target-check-timestamps? :: <boolean>, init-value: #t;
  slot target-temporary? :: <boolean>, init-value: #f;
end class;

define method jam-target-copy
    (jam :: <jam-state>, new-jam :: <jam-state>, target :: <jam-target>)
 => (new-target :: <jam-target>);
  let new-target
    = element(new-jam.%jam-targets, target.target-name, default: #f);
  if (new-target)
    new-target
  else
    let new-target = make(<jam-target>, name: target.target-name);
    new-jam.%jam-targets[target.target-name] := new-target;
    
    for (value keyed-by variable-name in target.target-variables)
      new-target.target-variables[variable-name] := value;
    end for;
    
    map-into(new-target.target-depends, curry(jam-target-copy, jam, new-jam),
             target.target-depends);
    
    if (target.target-includes-target)
      new-target.target-includes-target
        := jam-target-copy(jam, new-jam, target.target-includes-target);
    end if;
    
    for (invocation in target.target-action-invocations)
      add!(new-target.target-action-invocations,
           make(<jam-action-invocation>,
                action: invocation.action-invocation-action,
                targets: map(curry(jam-target-copy, jam, new-jam),
                             invocation.action-invocation-targets),
                sources: map(curry(jam-target-copy, jam, new-jam),
                             invocation.action-invocation-sources)));
    end for;

    new-target.target-always-build? := target.target-always-build?;
    new-target.target-leaf-only? := target.target-leaf-only?;
    new-target.target-allow-nonexistent? := target.target-allow-nonexistent?;
    new-target.target-file? := target.target-file?;
    new-target.target-check-timestamps? := target.target-check-timestamps?;
    new-target.target-temporary? := target.target-temporary?;

    new-target
  end if
end method;

define method jam-target
    (jam :: <jam-state>, target-name :: <string>)
 => (target :: <jam-target>);
  element(jam.%jam-targets, target-name, default: #f)
    | (jam.%jam-targets[target-name] := make(<jam-target>, name: target-name))
end method;

define method jam-target-variable
    (jam :: <jam-state>, target-name :: <string>, name :: <string>,
     #key default :: false-or(<sequence>) = #[])
 => (value :: false-or(<sequence>));
  let target = jam-target(jam, target-name);
  element(target.target-variables, name, default: default)
end method;

define method jam-target-variable-setter
    (value :: false-or(<sequence>), jam :: <jam-state>,
     target-name :: <string>, name :: <string>)
 => (value :: false-or(<sequence>));
  let target = jam-target(jam, target-name);
  if (value)
    target.target-variables[name] := value;
  else
    remove-key!(target.target-variables, name);
    #f
  end if
end method;

define macro with-jam-target
  { with-jam-target (?jam:expression, ?target:expression) ?:body end }
     => { do-with-jam-target(?jam, ?target, method() ?body end) }
end macro;

define method do-with-jam-target
    (jam :: <jam-state>, target :: <jam-target>, thunk :: <function>)
  let vars = key-sequence(target.target-variables);
  let outer-values
    = map(method (v :: <string>) jam-variable(jam, v, default: #f) end, vars);
  for (val keyed-by var in target.target-variables)
    jam-variable(jam, var) := val;
  end for;
  block ()
    thunk()
  cleanup
    for (var in vars, outer-value in outer-values)
      target.target-variables[var] := jam-variable(jam, var);
      jam-variable(jam, var) := outer-value;
    end for;
  end block;  
end method;


// Builtins for dependency graph building

define function jam-builtin-depends
    (jam :: <jam-state>,
     target-names :: <sequence>,
     source-names :: <sequence>)
 => (result :: <sequence>);
  let sources = map(curry(jam-target, jam), source-names);
  for (name in target-names)
    let target = jam-target(jam, name);
    for (source in sources)
      add-new!(target.target-depends, source);
    end for;
  end for;
  #[]
end function;

define function jam-builtin-includes
    (jam :: <jam-state>,
     target-names :: <sequence>,
     source-names :: <sequence>)
 => (result :: <sequence>);
  let sources = map(curry(jam-target, jam), source-names);
  for (name in target-names)
    let target = jam-target(jam, name);
    unless (target.target-includes-target)
      target.target-includes-target
        := make(<jam-target>,
                name: target.target-name,
                bound-locator: target.target-bound-locator,
                internal?: #t,
                file?: #f);
    end unless;
    for (source in sources)
      add-new!(target.target-includes-target.target-depends, source);
    end for;
  end for;
  #[]
end function;

// Builtins for modifying update determination

define function jam-builtin-always
    (jam :: <jam-state>, target-names :: <sequence>)
 => (result :: <sequence>);
  for (target-name in target-names)
    let target = jam-target(jam, target-name);
    target.target-always-build? := #t;
  end for;
  #[]
end function;

define function jam-builtin-leaves
    (jam :: <jam-state>, target-names :: <sequence>)
 => (result :: <sequence>);
  for (target-name in target-names)
    let target = jam-target(jam, target-name);
    target.target-leaf-only? := #t;
  end for;
  #[]
end function;

define function jam-builtin-nocare
    (jam :: <jam-state>, target-names :: <sequence>)
 => (result :: <sequence>);
  for (target-name in target-names)
    let target = jam-target(jam, target-name);
    target.target-allow-nonexistent? := #t;
  end for;
  #[]
end function;

define function jam-builtin-notfile
    (jam :: <jam-state>, target-names :: <sequence>)
 => (result :: <sequence>);
  for (target-name in target-names)
    let target = jam-target(jam, target-name);
    target.target-file? := #f;
  end for;
  #[]
end function;

define function jam-builtin-noupdate
    (jam :: <jam-state>, target-names :: <sequence>)
 => (result :: <sequence>);
  for (target-name in target-names)
    let target = jam-target(jam, target-name);
    target.target-check-timestamps? := #f;
  end for;
  #[]
end function;

define function jam-builtin-temporary
    (jam :: <jam-state>, target-names :: <sequence>)
 => (result :: <sequence>);
  for (target-name in target-names)
    let target = jam-target(jam, target-name);
    target.target-temporary? := #t;
  end for;
  #[]
end function;


// Binding targets

define method jam-target-bind
    (jam :: <jam-state>, target-name :: <string>)
 => (locator :: <physical-locator>, target :: <jam-target>);
  let target = jam-target(jam, target-name);
  let locator = jam-target-bind-aux(jam, target-name, target);
  values(locator, target)
end method;

define method jam-target-bind-aux
    (jam :: <jam-state>, target-name :: <string>, target :: <jam-target>)
 => (locator :: <locator>);
  if(target.target-bound-locator)
    target.target-bound-locator
  else
    let locator = as(<file-system-locator>, strip-grist(target-name));
    if (locator.locator-relative?)
      let locate
        = element(target.target-variables, "LOCATE", default: #f)
        | jam-variable(jam, "LOCATE");
      let search
        = element(target.target-variables, "SEARCH", default: #f)
        | jam-variable(jam, "SEARCH");
      
      if (~empty?(locate))
        let merged
          = merge-locators(locator, as(<directory-locator>, first(locate)));
        target.target-modification-date := file-modification-date(merged);
        target.target-bound-locator := merged
      elseif (~empty?(search))
        block(done)
          for (dir in search)
            let merged
              = merge-locators(locator, as(<directory-locator>, dir));
            let modification-date = file-modification-date(merged);
            if (modification-date) // file-exists?(merged)
              target.target-bound-locator := merged;
              target.target-modification-date := modification-date;
              done(merged);
            end if;
          finally
            target.target-bound-locator := locator
          end for;
        end block
      else
        target.target-modification-date := file-modification-date(locator);
        target.target-bound-locator := locator
      end if
    else
      target.target-modification-date := file-modification-date(locator);
      target.target-bound-locator := locator
    end if
  end if
end method;

define method file-modification-date
    (locator :: <file-system-locator>)
 => (date :: false-or(<date>));
  block (return)
    file-property(locator, #"modification-date")
  exception (e :: <file-system-error>)
    return(#f)
  end block
end method;


/// Action definitions and invocations

define class <jam-action> (<object>)
  constant slot action-name :: <string>,
    required-init-keyword: name:;
  constant slot action-commands :: <string>,
    required-init-keyword: commands:;
  constant slot action-bindlist :: <sequence>,
    required-init-keyword: bindlist:;

  // Flags
  constant slot action-updated? :: <boolean>,
    init-value: #f, init-keyword: updated?:;
  constant slot action-together? :: <boolean>,
    init-value: #f, init-keyword: together?:;
  constant slot action-ignore? :: <boolean>,
    init-value: #f, init-keyword: ignore?:;
  constant slot action-quietly? :: <boolean>,
    init-value: #f, init-keyword: quietly?:;
  constant slot action-existing? :: <boolean>,
    init-value: #f, init-keyword: existing?:;
/*
  constant slot action-piecemeal? :: <boolean>,
    init-value: #f, init-keyword: piecemeal?:;
  constant slot action-maxline :: false-or(<string>),
    init-value: #f, init-keyword: maxline:;
*/
end class;

define class <jam-action-invocation> (<object>)
  constant slot action-invocation-action :: <jam-action>,
    required-init-keyword: action:;
  constant slot action-invocation-targets :: <sequence>,
    required-init-keyword: targets:;
  constant slot action-invocation-sources :: <sequence>,
    required-init-keyword: sources:;
  slot action-invocation-subsumed? :: <boolean>,
    init-value: #f;
end;

define method jam-invoke-action
    (jam :: <jam-state>, action :: <jam-action>, #rest lol)
 => ();
  let targets = map(curry(jam-target, jam), element(lol, 0, default: #()));
  let sources = map(curry(jam-target, jam), element(lol, 1, default: #()));

  unless (targets.empty?)
    let invocation
      = make(<jam-action-invocation>,
             action: action, targets: targets, sources: sources);
    for(target in targets)
      add!(target.target-action-invocations, invocation);
    end for;
  end unless;
end method;
