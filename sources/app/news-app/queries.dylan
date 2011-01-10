language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define variable *queries* = #();

define class <query> (<object>)
  slot news-groups, init-keyword: groups:;
  slot last-articles-read, init-keyword: articles-read:;
  slot search-keys, init-keyword: search-keys:;
end;
*/

define method scan-text (text :: <string>, key :: <string>, start)
  ignore(start);
  subsequence-position(text, key, test: char=);
end;

define method scan-text (text :: <string>, keys :: <list>, start)
  let test-method = method(key)
                      scan-text(text, key, start);
                    end;
  select (head(keys))
    #"and" => every?(test-method, tail(keys));
    #"or" => any?(test-method, tail(keys));
    #"not" => ~scan-text(text, tail(keys), start);
  end select;
end;

/*
define method query-header (header-text, query)
  scan-text(header-text, query.search-keys, 0);
end;

define method query-body (body-text, query,
                          #key start = 0)
  scan-text(body-text, query.search-keys, start);
end;

define method all-query-groups ()
  let group-union = method(a, b)
                      union(a.news-groups, b.news-groups, test: \=);
                    end;
  reduce(group-union, *queries*.head.news-groups, *queries*.tail);
end;

define method all-queries-for-group (group)
  let query-on-group? = method(a)
                          member?(group, a.news-groups, test: \=);
                        end;
  choose(query-on-group?, *queries*);
end;

define method first-article-for-queries(q-list, group)
  let lowest = #f;
  for (q in q-list)
    let group-index = find-key(q.news-groups, method(x)
                                                x = group;
                                              end);
    let articles-read-list = q.last-articles-read;
    if (group-index & articles-read-list & (group-index < size(articles-read-list)))
      if (lowest) 
        lowest := min(lowest, articles-read-list[group-index]);
      else
        lowest := articles-read-list[group-index];
      end;
    end;
  end for;
  lowest;
end;

define method add-new-query(groups, search-keys)
  *queries* := add!(*queries*, make(<query>, 
                                    groups: groups,
                                    search-keys: search-keys,
                                    articles-read: make(<vector>, size: size(groups))));
end;

/* initial query: */

add-new-query(#("comp.sys.mac.oop.macapp3",
                "comp.sys.mac.programmer",
                "comp.lang.dylan"),
	      #(#"or", "Dylan", #(#"and", "oop", "framework"), "AppleEvents"));
*/
