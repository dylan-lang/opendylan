Module:       testworks
Summary:      Testworks harness
Author:       James Krisch, Shri Amit, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Suites

define class <suite> (<component>)
  constant slot %components :: false-or(type-union(<sequence>, <function>)) = #f,
    init-keyword: components:;
  constant slot suite-setup-function :: <function> = method () end, 
    init-keyword: setup-function:;
  constant slot suite-cleanup-function :: <function> = method () end, 
    init-keyword: cleanup-function:;
end class <suite>;

define variable *all-suites*
  = make(<suite>, 
         name: "All Defined Suites",
         components: make(<stretchy-vector>));

define method root-suite () => (suite :: <suite>)
  *all-suites*
end method root-suite;

define method ensure-suite-components
    (components :: <sequence>, suite :: <suite>) 
 => (components :: <sequence>)
  map(method (component)
        select (component by instance?)
          <component> =>
            component;
          <function>  =>
            find-test-object(component)
              | error("Non-test function %= in suite %s", 
                      component, component-name(suite));
          otherwise   =>
            error("Invalid object %= in suite %s", component, component-name(suite))
        end
      end,
      components)
end method ensure-suite-components;

define method suite-components
    (suite :: <suite>) => (components :: <sequence>)
  let components = suite.%components;
  select (components by instance?)
    <sequence> => components;
    <function> => ensure-suite-components(components(), suite)
  end
end method suite-components;

define method make-suite
    (name :: <string>, components, #rest keyword-args)
 => (suite :: <suite>)
  let suite
    = apply(make, <suite>,
            name: name,
            components: components,
            keyword-args);
  let all-suites = root-suite().suite-components;
  let position
     = find-key(all-suites,
                method (suite)
                  suite.component-name = name
                end);
  if (position)
    all-suites[position] := suite
  else
    add!(all-suites, suite)
  end;
  suite
end method make-suite;

define macro suite-definer
  { define suite ?suite-name:name (?keyword-args:*) ?components end } =>
    {define variable ?suite-name
       = make-suite(?"suite-name", 
		    method ()
		      list(?components)
		    end,
		    ?keyword-args) }

  components:
    { } => { }
    { test ?:name; ... }
      => { ?name, ... }
    { suite ?:name; ... }
      => { ?name, ... }
end macro suite-definer;

define method find-suite
    (name :: <string>, #key search-suite = root-suite()) 
 => (suite :: false-or(<suite>))
  let lowercase-name = as-lowercase(name);
  local method do-find-suite (suite :: <suite>)
	  if (as-lowercase(component-name(suite)) = lowercase-name)
	    suite
	  else
	    block (return)
	      for (object in suite-components(suite))
		if (instance?(object, <suite>))
		  let subsuite = do-find-suite(object);
		  if (subsuite) return(subsuite) end;
		end
	      end
	    end
	  end
	end;
  do-find-suite(search-suite);
end method find-suite;

define method find-test
    (name :: <string>, #key search-suite = root-suite()) 
 => (test :: false-or(<test>))
  let lowercase-name = as-lowercase(name);
  local method do-find-test (suite :: <suite>)
          block (return)
	    for (object in suite-components(suite))
	      select (object by instance?)
		<test> =>
		  if (as-lowercase(component-name(object)) = lowercase-name)
		    return(object)
		  end if;
		<suite> =>
		  let test = do-find-test(object);
		  if (test) return(test) end;
	      end
	    end
	  end
	end;
  do-find-test(search-suite);
end method find-test;

define method perform-suite  
    (suite :: <suite>, 
     #key tags                     = $all,
          announce-function        = #f,
          announce-checks?         = *announce-checks?*,
          report-format-function   = *format-function*,
          progress-format-function = *format-function*,
          report-function          = *default-report-function*,
          progress-function        = *default-progress-function*,
          debug?                   = *debug?*)
 => (result :: <component-result>)
  perform-component
    (suite,
     make(<perform-options>,
	  tags:                     tags,
	  announce-function:        announce-function,
	  announce-checks?:         announce-checks?,
	  progress-format-function: progress-format-function,
	  progress-function:        progress-function | null-progress-function,
	  debug?:                   debug?),
     report-function:        report-function | null-report-function,
     report-format-function: report-format-function)
end method perform-suite;

define method execute-component
    (suite :: <suite>, options :: <perform-options>)
 => (subresults :: <sequence>, status :: <result-status>)
  let subresults :: <stretchy-vector> = make(<stretchy-vector>);
  let status
    = block ()
	suite.suite-setup-function();
	for (component in suite.suite-components)
	  let subresult = maybe-execute-component(component, options);
	  add!(subresults, subresult)
	end;
	case
	  every?(method (subresult)
		   let status = subresult.result-status;
		   status = #"passed" | status = #"not-executed"
		 end,
		 subresults) =>
	    #"passed";
	  otherwise =>
	    #"failed"
	end
      cleanup
	suite.suite-cleanup-function();
      end;
  values(subresults, status)
end method execute-component;
