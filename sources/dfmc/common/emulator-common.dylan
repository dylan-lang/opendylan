module: dfmc-common
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline method lookup-keyword-value (keys, key)
  iterate loop (keys = keys)
    unless (empty?(keys))
      let k = head(keys);
      if (k == key)
	head(tail(keys))
      else
	loop(tail(tail(keys)))
      end if;
    end unless;
  end iterate;
end method;
