Module:    CL-internals
Author:    Scott McKay
Synopsis:  Simple property list management a la Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic get-property
    (plist :: <sequence>, indicator, #key default) => (property :: <object>);

define method get-property
    (plist :: <list>, indicator, #key default) => (property :: <object>)
  block (return)
    while (#t)
      if (empty?(plist))
        return(default)
      end;
      if (pop!(plist) == indicator)
        return(head(plist))
      end;
      plist := tail(plist)
    end
  end
end method get-property;

define method get-property
    (plist :: <vector>, indicator, #key default) => (property :: <object>)
  block (return)
    for (i from 0 below size(plist) by 2)
      if (plist[i] == indicator)
        return(plist[i + 1])
      end;
    finally
      return(default);
    end
  end
end method get-property;


// Modifies PLIST
define generic do-put-property!
    (plist :: <sequence>, indicator, value) => (plist :: <sequence>);

define method do-put-property! 
    (plist :: <list>, indicator, value) => (plist :: <list>)
  block (return)
    let pl = plist;
    while (#t)
      if (empty?(pl))
        return(concatenate!(plist, list(indicator, value)))
      end;
      if (pop!(pl) == indicator & pl)
        head(pl) := value;
        return(plist)
      end;
      pl := tail(pl)
    end
  end
end method do-put-property!;

define method do-put-property!
    (plist :: <vector>, indicator, value) => (plist :: <vector>)
  block (return)
    for (i from 0 below size(plist) by 2)
      if (plist[i] == indicator)
        plist[i + 1] := value;
        return(plist)
      end;
    finally
      return(concatenate!(plist, vector(indicator, value)));
    end
  end
end method do-put-property!;

define macro put-property!
  { put-property! (?plist:expression, ?indicator:expression, ?value:expression) }
    => { ?plist := do-put-property!(?plist, ?indicator, ?value); }
end macro;


// Modifies PLIST
define generic do-remove-property!
    (plist :: <sequence>, indicator) => (plist :: <sequence>);

define method do-remove-property!
    (plist :: <list>, indicator) => (plist :: <list>)
  block (return)
    let result-plist = plist;
    let pl = plist;
    let ppl = #f;
    while (#t)
      if (empty?(pl))
        return(#f, result-plist)
      end;
      if (first(pl) == indicator)
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
    end
  end
end method do-remove-property!;

define method do-remove-property!
    (plist :: <vector>, indicator) => (plist :: <vector>)
  let j = 0;
  let value = #f;
  for (i from 0 below size(plist) by 2)
    unless (plist[i] == indicator)
      plist[j] := plist[i];
      plist[j + 1] := plist[i + 1];
      value := plist[i + 1];
      j := j + 2;
    end;
  end;
  size(plist) := j;
  values(value, plist)
end method do-remove-property!;

define macro remove-property!
  { remove-property! (?plist:expression, ?indicator:expression) }
    => { begin
           let (_value, _new-plist) = do-remove-property!(?plist, ?indicator);
           ?plist := _new-plist;
           _value;
         end; }
end macro;


define generic remove-keywords
    (plist :: <sequence>, keywords :: <sequence>) => (plist :: <sequence>);

define method remove-keywords
    (plist :: <list>, keywords :: <sequence>) => (plist :: <sequence>)
  case
    empty?(plist)    => plist;
    empty?(keywords) => plist;
    otherwise =>
      let new-plist = make(<stretchy-vector>, size: size(plist));
      let j = 0;
      if (size(keywords) = 1)	// speed bum when only one keyword
        let keyword = first(keywords);
        block (return)
          while (#t)
            begin
              let indicator = pop!(plist);
              let value = pop!(plist);
              unless (keyword == indicator)
                new-plist[j] := indicator;
                new-plist[j + 1] := value;
                j := j + 2;
              end
            end;
            if (empty?(plist))
              return()
            end
          end
        end
      else
        block (return)
          while (#t)
            begin
              let indicator = pop!(plist);
              let value = pop!(plist);
              unless (cl-find(keywords, indicator))
                new-plist[j] := indicator;
                new-plist[j + 1] := value;
                j := j + 2;
              end
            end;
            if (empty?(plist))
              return()
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
      let length = size(plist);
      let new-plist = make(<stretchy-vector>, size: length);
      let j = 0;
      if (size(keywords) = 1)	// speed bum when only one keyword
        let keyword = first(keywords);
        for (i from 0 below length by 2)
          let indicator = plist[i];
          let value = plist[i + 1];
          unless (keyword == indicator)
            new-plist[j] := indicator;
            new-plist[j + 1] := value;
            j := j + 2;
          end;
        end
      else
        for (i from 0 below length by 2)
          let indicator = plist[i];
          let value = plist[i + 1];
          unless (cl-find(keywords, indicator))
            new-plist[j] := indicator;
            new-plist[j + 1] := value;
            j := j + 2;
          end;
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
         end; }
end macro;
