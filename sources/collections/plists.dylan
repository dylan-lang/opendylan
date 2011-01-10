Module:       collections-internals
Synopsis:     Simple property-list management library
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Property lists

// Internal, for use only on <list>s
define macro pop!
  { pop! (?list:expression) }
    => { begin
           let _result = head(?list);
           ?list := tail(?list);
           _result
	 end }
end macro pop!;


define generic get-property
    (plist :: <sequence>, indicator, #key default) => (property :: <object>);

define method get-property
    (plist :: <list>, indicator, #key default) => (property :: <object>)
  block (return)
    until (empty?(plist))
      when (pop!(plist) == indicator)
        return(head(plist))
      end;
      plist := tail(plist)
    end;
    default
  end
end method get-property;

define method get-property
    (plist :: <vector>, indicator, #key default) => (property :: <object>)
  block (return)
    let length :: <integer> = size(plist);
    assert(even?(length),
	   "This plist does not have even length");
    without-bounds-checks
      for (i :: <integer> from 0 below length by 2)
	when (plist[i] == indicator)
	  return(plist[i + 1])
	end;
      finally
	return(default);
      end
    end
  end
end method get-property;

// Modifies PLIST
define generic do-put-property!
    (plist :: <sequence>, indicator, value) => (plist :: <sequence>);

define macro put-property!
  { put-property! (?plist:expression, ?indicator:expression, ?value:expression) }
    => { ?plist := do-put-property!(?plist, ?indicator, ?value); }
end macro put-property!;

define method do-put-property!
    (plist :: <list>, indicator, value) => (plist :: <list>)
  block (return)
    let pl = plist;
    until (empty?(pl))
      when (pop!(pl) == indicator & pl)
        head(pl) := value;
        return(plist)
      end;
      pl := tail(pl)
    end;
    concatenate!(plist, list(indicator, value))
  end
end method do-put-property!;

define method do-put-property!
    (plist :: <vector>, indicator, value) => (plist :: <vector>)
  block (return)
    let length :: <integer> = size(plist);
    assert(even?(length),
	   "This plist does not have even length");
    without-bounds-checks
      for (i :: <integer> from 0 below length by 2)
	when (plist[i] == indicator)
	  plist[i + 1] := value;
	  return(plist)
	end;
      finally
	return(concatenate!(plist, vector(indicator, value)));
      end
    end
  end
end method do-put-property!;

define generic keyword-sequence
    (plist :: <sequence>) => (keywords :: <sequence>);

define generic value-sequence
    (plist :: <sequence>) => (values :: <sequence>);

define method keyword-sequence
    (plist :: <vector>) => (keywords :: <vector>)
  let length :: <integer> = size(plist);
  assert(even?(length),
	 "This plist does not have even length");
  let count :: <integer> = ash(length, -1);
  without-bounds-checks
    let keywords = make(<vector>, size: count);
    for (i :: <integer> from 0 below count)
      keywords[i] := plist[i + i];
    end;
    keywords
  end
end method keyword-sequence;

define method value-sequence
    (plist :: <vector>) => (values :: <vector>)
  let length :: <integer> = size(plist);
  assert(even?(length),
	 "This plist does not have even length");
  let count :: <integer> = ash(length, -1);
  without-bounds-checks
    let values = make(<vector>, size: count);
    for (i :: <integer> from 0 below count)
      values[i] := plist[i + i + 1];
    end;
    values
  end
end method value-sequence;

define method keyword-sequence
    (plist :: <list>) => (keywords :: <list>)
  local method loop (plist :: <list>, keywords)
	  if (plist == #())
	    reverse!(keywords)
	  else
	    loop(plist.tail.tail, pair(plist.head, keywords))
	  end
	end method;
  loop(plist, #());
end method keyword-sequence;

define method value-sequence
    (plist :: <list>) => (values :: <list>)
  local method loop (plist :: <list>, values)
	  if (plist == #())
	    reverse!(values)
	  else
	    let next :: <pair> = plist.tail;
	    loop(next.tail, pair(next.head, values))
	  end
	end method;
  loop(plist, #());
end method value-sequence;


// Modifies PLIST
define generic do-remove-property!
    (plist :: <sequence>, indicator) => (value, plist :: <sequence>);

define macro remove-property!
  { remove-property! (?plist:expression, ?indicator:expression) }
    => { begin
           let (_value, _new-plist) = do-remove-property!(?plist, ?indicator);
           ?plist := _new-plist;
           _value;
         end; }
end macro remove-property!;

define method do-remove-property!
    (plist :: <list>, indicator) => (value, plist :: <list>)
  block (return)
    let result-plist = plist;
    let pl = plist;
    let ppl = #f;
    until (empty?(pl))
      when (first(pl) == indicator)
        let result = second(pl);
        if (pl == plist)
          result-plist := tail(tail(result-plist))
        else
          tail(ppl) := tail(tail(pl))
        end;
        return(result, result-plist)
      end;
      if (pl == plist)
        ppl := tail(pl)
      else
        ppl := tail(tail(ppl))
      end;
      pl := tail(tail(pl))
    end;
    return(#f, result-plist)
  end
end method do-remove-property!;

define method do-remove-property!
    (plist :: <stretchy-vector>, indicator) => (value, plist :: <vector>)
  let j = 0;
  let value = #f;
  let length :: <integer> = size(plist);
  assert(even?(length),
	 "This plist does not have even length");
  without-bounds-checks
    for (i :: <integer> from 0 below length by 2)
      if (plist[i] == indicator)
	value := plist[i + 1];
      else
	plist[j]     := plist[i];
	plist[j + 1] := plist[i + 1];
	j := j + 2
      end
    end
  end;
  size(plist) := j;
  values(value, plist)
end method do-remove-property!;

define method do-remove-property!
    (plist :: <simple-object-vector>, indicator) => (value, plist :: <vector>)
  let j = 0;
  let value = #f;
  let length :: <integer> = size(plist);
  assert(even?(length),
	 "This plist does not have even length");
  let count :: <integer> = 0;
  without-bounds-checks
    for (i :: <integer> from 0 below length by 2)
      when (plist[i] == indicator) count := count + 2 end;
    end;
  end;
  if (count == 0)
    values(value, plist)
  else
    let new = make(<simple-object-vector>, size: length - count);
    without-bounds-checks
      for (i :: <integer> from 0 below length by 2)
	if (plist[i] == indicator)
	  value := plist[i + 1];
	else
	  new[j]     := plist[i];
	  new[j + 1] := plist[i + 1];
	  j := j + 2
	end
      end
    end;
    values(value, new)
  end;
end method do-remove-property!;


/// #rest argument surgery

define generic remove-keywords
    (plist :: <sequence>, keywords :: <sequence>) => (plist :: <sequence>);

define method remove-keywords
    (plist :: <list>, keywords :: <sequence>) => (plist :: <vector>)
  case
    empty?(plist)    => as(<vector>, plist);
    empty?(keywords) => as(<vector>, plist);
    otherwise =>
      let length :: <integer> = size(plist);
      assert(even?(length),
	     "This plist does not have even length");
      let new-plist :: <stretchy-object-vector>
	= make(<stretchy-vector>, size: length);
      let j :: <integer> = 0;
      if (size(keywords) = 1)	// speed bum when only one keyword
        let keyword = keywords[0];
	block (break)
	  without-bounds-checks
	    while (#t)
	      let indicator = pop!(plist);
	      let value     = pop!(plist);
	      unless (keyword == indicator)
		new-plist[j]     := indicator;
		new-plist[j + 1] := value;
		j := j + 2
	      end;
	      when (empty?(plist))
		break()
	      end
	    end
	  end
        end
      else
        block (break)
          without-bounds-checks
	    while (#t)
	      let indicator = pop!(plist);
	      let value     = pop!(plist);
	      unless (member?(indicator, keywords))
		new-plist[j]     := indicator;
		new-plist[j + 1] := value;
		j := j + 2
	      end;
	      when (empty?(plist))
		break()
	      end
	    end
	  end
        end
      end;
      size(new-plist) := j;
      new-plist
  end
end method remove-keywords;

define method remove-keywords
    (plist :: <vector>, keywords :: <sequence>) => (plist :: <vector>)
  case
    empty?(plist)    => plist;
    empty?(keywords) => plist;
    otherwise =>
      let length :: <integer> = size(plist);
      assert(even?(length),
	     "This plist does not have even length");
      let new-plist :: <stretchy-object-vector>
	= make(<stretchy-vector>, size: length);
      let j :: <integer> = 0;
      if (size(keywords) = 1)	// speed bum when only one keyword
        let keyword = keywords[0];
	without-bounds-checks
	  for (i :: <integer> from 0 below length by 2)
	    let indicator = plist[i];
	    let value     = plist[i + 1];
	    unless (keyword == indicator)
	      new-plist[j]     := indicator;
	      new-plist[j + 1] := value;
	      j := j + 2
	    end
	  end
        end
      else
        without-bounds-checks
	  for (i :: <integer> from 0 below length by 2)
	    let indicator = plist[i];
	    let value     = plist[i + 1];
	    unless (member?(indicator, keywords))
	      new-plist[j]     := indicator;
	      new-plist[j + 1] := value;
	      j := j + 2
	    end
	  end
        end
      end;
      size(new-plist) := j;
      new-plist
  end
end method remove-keywords;

define macro with-keywords-removed
  { with-keywords-removed (?new-plist:variable = ?plist:expression, ?keywords:expression)
      ?:body
    end}
    => { begin
           let ?new-plist = remove-keywords(?plist, ?keywords);
           ?body
         end }
end macro with-keywords-removed;
