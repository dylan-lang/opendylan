language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *news-host*
  // = "services.cambridge.apple.com";
  = "newshost";

define variable *news* = #f;

define constant parse-header =
     method (header :: <string>, type :: <string>,
             #key strip-keyword = #t, strip-new-line = #t)
       // Given list of strings, find one which contains type
       // (such as "subject: ") as a substring. Returns remainder of that string.
       let pos = if (every?(char=, type, header))
                   0;
                 else
                   subsequence-position(header, type, test: char=);
                 end;
       if (pos)
         let end-line = find-char(header, '\n', start: pos + 1) |
                           size(header);
         copy-sequence(header, start: if (strip-keyword)
                                        pos + size(type);
                                      else
                                        pos;
                                      end,
                       end: if (strip-new-line)
                              end-line;
                            else
                              min(end-line + 1, size(header));
                            end);
         
       end;
     end;

define variable *interesting-headers* :: <list> =
     #("Subject: ", "From: ", "Organization: ", "Newsgroups: ", "Date: ");

define method filter-headers (text)
  let headers-list = map(method(header)
                           parse-header(text, header, strip-keyword: #f, strip-new-line: #f)
                         end,
                         *interesting-headers*);
  apply(concatenate-as, <byte-string>, remove(headers-list, #f, count: size(headers-list)));
end;

define method startup-news()
  *news* := start-news(host: *news-host*);
  unless (*news*)
    tell-user-dialog("Connecting to Server",
                     concatenate("Problem connecting to netnews server \"", 
                                 *news-host*,
                                 "\"\n Try another server or try later."));
  end;
end;

define constant cleanup-news =
     method()
       if (*news*)
         stop-news(*news*);
         *news* := #f;
       end;
     end;

define method read-one-news (group, article-number, #key include-header?)
  select-group(*news*, group);
  let (status, article-text) = article(*news*, article-number);
  ignore(status);
  let subject = parse-header(article-text, "Subject: ") |
                   concatenate("No subject #", article-number);
  let title = replace-elements!(subject, curry(id?, ':'), always('-')); // colons make bad file names.
  let body-start = if (include-header?)
                     -1;
                   else
                     subsequence-position(article-text, "\n\n"); // find blank line that seperates header from body.
                   end;
  if (body-start)
    let text-str = copy-sequence(article-text, start: body-start + 1);
    values(title, concatenate-as(<byte-string>, 
                                 filter-headers(article-text), text-str));
  else
    title;
  end;
end;

/*
define variable *status-window* = #f;

define constant make-status-window =
     method()
       let main-rect = *main-screen-bounds-rect*;
       let loc = point(main-rect.left + 2, main-rect.bottom - 50);
       let sz = point(main-rect.right - 60 - 2, 40);
       let window = make(<window>, title: "Status", 
                         next-handler: #f,
                         main-window: #t,
                         location: loc,
                         extent: sz,
                         closable: #t, zoomable: #t, resizable: #t);
       let view = make(<static-text>, 
                       identifier: #"status",
                       determiners: list(make(<has-super-view-bounds>)),
                       location: point(2, 0), 
                       extent: sz,
                       text-style: $geneva9,
                       text: "Unconnected");
       add-determiner(view, make(<super-view-relative-determiner>));
       add-sub-view(window.root-view, view);
       
       open(window);
       window;
     end;

define method status-callback (str :: <string>)
  poll-event(*dispatcher*);
  let view = find-sub-view(*status-window*, #"status");
  if (view)
    view.text := str;
    redraw(view, local-bounds(view));
  else
    signal(str);
  end;
end;

*status-callback* := status-callback;
*/
