Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function extract-grist
    (name :: <byte-string>)
 => (grist :: <byte-string>);
  if (name.size > 1 & name[0] == '<')
    let grist-end = find-key(name, curry(\==, '>'));
    if (grist-end)
      copy-sequence(name, end: grist-end + 1)
    else
      ""
    end if
  else
    ""
  end if
end function;

define function strip-grist
    (name :: <byte-string>)
 => (result :: <byte-string>);
  if (name.size > 1 & name[0] == '<')
    let grist-end = find-key(name, curry(\==, '>'));
    if (grist-end)
      copy-sequence(name, start: grist-end + 1)
    else
      name
    end if
  else
    name
  end if
end function;

define function add-grist
    (old :: <byte-string>, new :: <byte-string>)
 => (result :: <byte-string>);
  let old-grist = extract-grist(old);
  if (old-grist.empty?)
    new
  else
    concatenate(old-grist, new)
  end if
end function;
