module: match
author: Hal Abelson, et. al.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TRY-RULES tries each of a list of rules on a given datum. 
/// If any rule is applicable, the success continuation, SUCCEED, is
/// called with a value and a failure continuation.  If no rule is 
/// applicable, FAIL is called with no arguments.

/// A rule is an association of a pattern with a procedure to
/// be executed if the expression matches the pattern.  The rule
/// procedure gets a dictionary as its argument.  The dictionary
/// represents the map of pattern variables to the matched values.
/// The rule procedure may itself either succeed or fail. 

define method try-rules (datum, the-rules, fail, succeed)
  // fail = method () ...
  // succeed = method (value fail) ...
  local method scan (rules)
	  if (null?(rules))
	    fail()
	  else
	    match(rule-pattern(head(rules)), datum, make-empty-dictionary(),
		  method ()
		    scan(tail(rules))
		    end method,
		  method (dictionary, fail)
		    succeed(rule-procedure(head(rules))(dictionary), fail)
		    end method)
	  end if;
	end method;
  scan(the-rules);
end method;

/// The matcher takes a pattern, data, dictionary, 
///  and success and failure continutation procedures.
///    DICTIONARY is a list of triples: #(var, value, info)
///    FAIL is method () ...
///    SUCCEED is method (dict, fail) ....
/// The idea is that the succeed continuation passes along the place
///  to fail back to, if we need to backtrack later.

define method match (pat, dat, dict, fail, succeed)
  case 
    pat == dat
      => succeed(dict, fail);
    arbitrary-element?(pat)
      => element-match(pat, dat, dict, fail, succeed);
    constant?(pat)
      => if (same-constant?(pat, dat))
	   succeed(dict, fail)
	 else
	   fail()
	 end if;
    start-arbitrary-segment?(pat)
      => segment-match(head(pat), dat, dict,
		       fail,
		       method (rest, dict, fail)
			 match(tail(pat), rest, dict,
			       fail, succeed)
		         end method);
    pair?(dat)
      => match(head(pat), head(dat), dict,
	       fail,
	       method (dict, fail)
		 match(tail(pat), tail(dat), dict,
		       fail, succeed)
	         end method);
    otherwise
      => fail();
  end case;
end method;

/// An arbitrary element pattern may have a restriction -- a procedure
///  that determines whether or not the datum should be allowed
///  to match the pattern.  The restriction may either succeed or fail.
///  If it succeeds, the success continuation is called with a value, 
///  which is added to the dictionary in the info field.

define method element-match (var, dat, dict, fail, succeed)
  let vname = var-name(var);
  let e = lookup(vname, dict);
  if (e)				// If already bound
    let val = matcher-entry-data(e);	// current value must
    if (val = dat)			// be same as expression.
      succeed(dict, fail)
    else
      fail()
    end if;
  else
    var-restriction(var)(dat,
			 fail,
			 method (result, fail)
			   succeed(extend-dictionary(vname, dat, result, dict),
				   fail)
			   end method)
  end if;
end method;

/// Try to match some initial segment of the data.  
/// The success continuation for this is of the form
///   method (rest-of-data, dictionary, fail) ...
/// Thus, if the match succeeds, we go on to try to match the rest of
/// the data against the rest of the pattern, while keeping the
/// failure point as a place to backtrack to.

define method segment-match (seg-var, dat, dict, fail, succeed-with-rest)
  let vname = var-name(seg-var);
  let p = var-restriction(seg-var);
  local method try-segment (rest)
	  p(segment->list(make-segment(dat, rest)),
	    method ()			//Try a longer segment, if possible.
	      if (null?(rest))
		fail()
	      else
		try-segment(tail(rest))
	      end if;
	      end method,
	    method (result, fail)	//Proposed segment is acceptable
	      succeed-with-rest(rest,
				extend-dictionary(vname, make-segment(dat, rest), 
						  result, dict),
				fail)
	      end method)
	end method;
  let e = lookup(vname, dict);
  if (e)				// If the segment variable is already 
    // bound, its value must be the segment at the head of the data
    // and the match must proceed with the rest of the data.
    compare-segment-to-list-head(matcher-entry-data(e), dat,
				 fail,
				 method (rest-list, fail)
				   succeed-with-rest(rest-list, dict, fail)
				   end method)
  else
    // Otherwise, try matching the segment against successively
    // longer initial segments of the data.  TRY-SEGMENT tries an
    // initial segment.  If it fails, it tries a longer segment.
    // If it succeeds, it calls SUCCEED-WITH-REST, but with a
    // failure continuation that will try a longer segment if we
    // need to backtrack to this point.
    try-segment(dat)
  end if;
end method;

/// Compare a segment with an initial segment of data.  If they are equal,
/// succeed with the rest of the data.  Otherwise fail.

define method compare-segment-to-list-head (segment, lst, fail, succeed)
  let endseg = segment-end(segment);
  local method scan (seg-ptr, rest-lst)
	  case 
	    seg-ptr == endseg
	      => succeed(rest-lst, fail);
	    null?(rest-lst)
	      => fail();
	    head(seg-ptr) = head(rest-lst)
	      => scan(tail(seg-ptr), tail(rest-lst));
	    otherwise
	      => fail();
	  end case;
	end method;
  scan(segment-start(segment), lst);
end method;

/// Syntax of the data being manipulated:

define method constant? (exp)
  ~pair?(exp)
end method;

define method same-constant? (x, y)
  x = y
end method;

/// Rule syntax

define class <rule> (<object>)
  slot rule-pattern, required-init-keyword: pattern:;
  slot rule-procedure, required-init-keyword: procedure:;
end class;

define method make-rule (pat, proc) 
  make(<rule>, pattern: pat,
               procedure: proc)
end method;

/// Pattern syntax.

define method arbitrary-element? (pat)
  pair?(pat)
  & head(pat) == #"?"
end method;

define method start-arbitrary-segment? (pat)
  pair?(pat)
  & pair?(head(pat))
  & head(head(pat)) == #"??"
end method;

define method var-name (pat)
  second(pat)
end method;

define method var-restriction (pat)
  if (size(pat) >= 3)
    third(pat)
  else 
    always-succeed
  end if;
end method;

define method always-succeed (dat, fail, succeed)
  succeed(dat, fail)
end method;


/// Segments

define class <segment> (<object>)
  slot segment-start, required-init-keyword: start:;
  slot segment-end, required-init-keyword: end:;
end class;

define method make-segment (start, seg-end)
  make(<segment>, start: start, end: seg-end)
end method;

define method segment? (x)
  instance?(x, <segment>)
end method;

define method segment->list (segment)
  let seg-end = segment-end(segment); // was end = 
    local method collect-segment (pointer)
	    if (pointer == seg-end)
	      #()
	    else
	      pair(head(pointer),
		   collect-segment(tail(pointer)))
	    end if;
	  end method;
  collect-segment(segment-start(segment))
end method;

define method restrict-segment (proc, seg)
  proc(segment->list(seg))
end method;
  

define method convert-matcher-entry (val)
  if (segment? (val))
      segment->list(val)
  else
    val
  end if;
end method;


/// Dictionaries

define method make-empty-dictionary ()
  #()
end method;

define class <matcher-entry> (<object>)
  slot matcher-entry-name, required-init-keyword: name:;
  slot matcher-entry-data, required-init-keyword: data:;
  slot matcher-entry-info, required-init-keyword: info:;
end class;

define method make-matcher-entry (name, data, info)
  make(<matcher-entry>, name: name, data: data, info: info)
end method;

define method lookup (name, dictionary)
  block (return)
    for (entry in dictionary)
      if (name == matcher-entry-name(entry))
	return(entry)
      end if;
    end for;
  end block;
end method;

define method extend-dictionary (name, data, info, dict)
  pair(make-matcher-entry(name, data, info), dict)
end method;

define method dictionary-info (name, dict)
  let e = lookup(name, dict);
  if (e)
    matcher-entry-info(e)
  else
    error("name not in dictionary: ~s", name) // change ~s to %=
  end if;
end method;

define method matched-expression (name, dict)
  let e = lookup(name, dict);
  if (e)
    matcher-entry-data(e)
  else
    error("name not in dictionary: ~s", name) // change ~s to %=
  end if;
end method;


define method value (patvar, dict)
  convert-matcher-entry(dictionary-info(patvar, dict))
end method;

/// Dylan-specific (shouldn't these be in the language?) Already Done?

define method pair? (x)
  instance?(x, <pair>)
end method;

/*
define method null? (x)
  instance?(x, <empty-list>)
end method;
*/

/// Test function:  
///    Tries one pattern and returns pretty dict on success, #f on fail

define method try-pattern (pattern, datum)
  block (return)
    match(pattern,
	  datum,
	  make-empty-dictionary(),
	  method ()
	    return(#f)
	    end method,
	  method (dict, fail)
	    let vars = map(matcher-entry-name, dict);
	    return(map(list, vars,
		             map(rcurry(value, dict), vars)));
	    end method);
  end block;
end method;

define method match-tests ()
  try-pattern(#(rasberry:), #(rasberry:))
    = #()
		&
  try-pattern(#(rasberry:), #(strawberry:))
    = #f
		&
  try-pattern(#(foo:, #(#"??", x:), bar:, baz:, #(#"??", y:)),
	      #(foo:, x:, bar:, y:, bar:, baz:))
    = #(#(y:, #()), #(x:, #(x:, bar:, y:)))
		&
  try-pattern(#(foo:, #(#"?", x:), bar:),
	      #(foo:, x:, bar:))
    = #(#(x:, x:))
		&
  try-pattern(#(foo:, #(#"?", x:), bar:),
	      #(foo:, x:, wrong:))
    = #f
		&
  try-pattern(#(#(#"?", bread:), #(#"??", filling:), #(#"?", bread:)),
	      #(rye:, corn-beef:, lettuce:, mayo:, rye:))
    = #(#(filling:, #(corn-beef:, lettuce:, mayo:)), #(bread:, rye:))
    		&
  try-pattern(#(#(#"?", bread:), #(#"??", filling:), #(#"?", bread:)),
	      #(corn-beef:, lettuce:, mayo:, rye:))
    = #f
end method match-test;
