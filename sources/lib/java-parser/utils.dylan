Module: java-parser
Author: Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <grammar-object> (<object>) end;
define sealed domain initialize(<grammar-object>);
define sealed domain make(subclass(<grammar-object>));

define macro grammar-sequence-definer
  { define grammar-sequence "<" ## ?:name ## ">" (?of:expression) }
  => { define constant "<" ## ?name ## ">" = limited(<vector>, of: ?of);
       define inline function "rev-" ## ?name (rev-decls :: <list>)
	=> (seq :: "<" ## ?name ## ">")
	 let n = size(rev-decls);
         if (n == 0)
           make("<" ## ?name ## ">", size: 0)
         else
           let v :: "<" ## ?name ## ">"
             = make("<" ## ?name ## ">", size: n, fill: rev-decls[0]);
	   for (i from n - 1 by -1, decl :: ?of in rev-decls) v[i] := decl end;
	   v
         end;
       end }
end macro;
