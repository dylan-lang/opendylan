language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define variable *scanning-buffer* = make(<stretchy-byte-string>, size: 1024 * 80);

define variable *scanner-window* = #f;

define method scan-message(stream, id, queries)
  => (subject, message-number);
  let (status, article-text, number) = article(stream, id);
                                               // , read-into: *scanning-buffer*
  if (number < 300)
    let message-number = if ((id = "") | (id[0] = '<'))
                           nth-word(status, 1);
                         else
                           id;
                         end;
    let subject = parse-header(article-text, "Subject: ");
    if (subject & 
        for (q in queries,
             yes? = #f then query-header(subject, q),
             until (yes?))
        finally yes?;
        end)
      values(subject, message-number);
    else
      let body-start = subsequence-position(article-text, "\n\n"); // find blank line that seperates header from body.
      if (body-start &
          for (q in queries,
               yes? = #f then query-body(article-text, q, start: body-start),
               until (yes?))
          finally yes?;
          end)
        values(subject, message-number);
      else
        values(#f, message-number);
      end;
    end;
  end;
end;

define method do-message(stream, group, id, queries) => message-number;
  let (subject, message-number) = scan-message(stream, id, queries);
  if (subject)
    add-article-to-scanner(group, message-number, subject);
  end;
  message-number;
end;

define method scan-group(stream, group)
  let status = select-group(stream, group);
  add-comment-to-scanner(concatenate("Searching ", 
                                     nth-word(status, 1),
                                     " articles in ", 
                                     group));
  let first-news = nth-word(status, 2);
  let queries = all-queries-for-group(group);
  let last-article-read = first-article-for-queries(queries, group);
  let start-news = if (last-article-read & (last-article-read > first-news)) // string > ok.
                     let last-news = nth-word(status, 3);
                     unless (last-article-read = last-news) 
                       last-article-read;
                     end;
                   else
                     first-news;
                   end;
  for(id-str = start-news then begin
                                 let str = do-message(stream, group, id-str, queries) |
                                              (next-msg(stream) &
                                               do-message(stream, group, "", queries));
                                 if (str)
                                   int-to-string(string-to-int(str) + 1);
                                 end;
                               end,
      while (id-str))
  end for;
end;

define method do-scan(stream)
  SetCursor(as(<Cursor>, pointer-at(as(<machine-pointer>, *watch-cursor*))));
  
  for(group in all-query-groups())
    scan-group(stream, group);
  end;
  add-comment-to-scanner("Done.");

 SetCursor($arrow-cursor);
end;

define method open-article (article-object)
  SetCursor(as(<Cursor>, pointer-at(as(<machine-pointer>, *watch-cursor*))));
  add-comment-to-scanner("Reading article...");
  
  let subject = element(article-object, 1);
  let group = element(article-object, 2);
  let id = element(article-object, 3);
  open(make(<news-document>, title: subject, article-number: id, group: group));
  
  add-comment-to-scanner("Done.");
  SetCursor($arrow-cursor);
end;

define method scanner-draw(view, row, column, rect)
  ignore(view, column);
  inset-region(rect, 1, 1);
  let seq = view.sequence;
  let item = element(seq, row);
  select(first(item))
    #"article" => draw-just-string(second(item), rect, #"left");
    #"comment" => begin
                    let style = view.text-style;
                    style.italic? := #t;
                    set-port-style(style);
                    view.text-style := style;
                    draw-just-string(second(item), rect, #"left");
                    style.italic? := #f;
                    set-port-style(style);
                  end;
  end select;
end;

define method scanner-click(view, row, column, #key double-click?)
  ignore(view, column);
  if (double-click?)
    if (row < 0)
      beep();
    else
      let seq = view.sequence;
      let item = element(seq, row);
      select(first(item))
        #"article" => open-article(item);
        #"comment" => beep();
      end select;
    end;
  end;
end;

define method add-to-scanner (item)
  if (*scanner-window*)
    let grid-view = find-sub-view(*scanner-window*, #"scanner-text");
    sequence-setter(concatenate(grid-view.sequence, pair(item, #())), grid-view,
                    invalidate?: #f);
    invalidate-cell(grid-view, size(grid-view.sequence) - 1, 0);
  else
    beep();
  end;
end;

define method add-article-to-scanner (group, article-number, subject, #key text)
  add-to-scanner(list(#"article", subject, group, article-number, text));
end;

define method add-comment-to-scanner (text)
  let window = *scanner-window*;
  if (window & window.visible?)
    let status-view = find-sub-view(window, #"status-static-text");
    text(status-view) := concatenate("Status: ", text);
    redraw(status-view, local-bounds(status-view));
  end if; // text-setter
end;

define class <run-behavior> (<behavior>)
end;

define method behavior-event (b :: <run-behavior>,
                              n :: <list>,
                              h :: <event-handler>,
                              event :: <button-event>,
                              id :: <object>)
  ignore(b, id, n, event);
  let window = get-window(h);
  
  read-query-window(window);
  add-comment-to-scanner("Starting Search...");
  block ()
    SetCursor(as(<Cursor>, pointer-at(as(<machine-pointer>, *watch-cursor*))));
    startup-news();
    do-scan(*news*); // should make this a command!
  cleanup
    cleanup-news();
    SetCursor($arrow-cursor);
  end block;
end method;

define method setup-query-window (window, query)
  // let status-view = find-sub-view(window, #"status-static-text");
  let groups-view = find-sub-view(window, #"groups-edit-text");
  let query-view = find-sub-view(window, #"query-edit-text");
  
  set-text(groups-view, query-groups-string(query, add-new-lines?: #t));
  set-text(query-view, query-search-string(query));
end;

define method read-query-window (window)
  let search-text = get-text(find-sub-view(window, #"query-edit-text"));
  let group-text = get-text(find-sub-view(window, #"groups-edit-text"));
  
  let search = parse-query(search-text, 0);
  let groups = parse-query-groups(group-text);
  if (~search)
    tell-user-dialog("Invalid Query",
                     Concatenate(search-text, " is a not a valid query string."));
  elseif (~groups)
    tell-user-dialog("Invalid Group List",
                     Concatenate(group-text, " is a not a valid list of news groups."));
  else
    *queries* := #();
    add-new-query(groups, search)
  end;
end;

define constant make-scanner-window =
     method(#key window-title = "Default Query")
       local replace-view (old, new)
               let super = old.super-view;
               add-sub-view(super, new, after: old);
               remove-sub-view(super, old);
             end;
       let window = new-template-window(0, open?: #f);
       window.title := window-title;
       
       let dummy-view = find-sub-view(window, #"my-list-view");
       
       let grid = make(<sequence-view>, 
                       location: dummy-view.location,
                       identifier: #"scanner-text",
                       default-row-height: 18,
                       default-column-width: 800,
                       draw-function: scanner-draw,
                       click-function: scanner-click,
                       sequence: #[],
                       text-style: $geneva12);
       add-behavior(grid, make(<grid-select-behavior>));
       replace-view(dummy-view, grid);
       setup-query-window(window, first(*queries*));
       
       let run-button = find-sub-view(window, #"run-button");
       add-behavior(run-button, make(<run-behavior>));
       
       /*let window = make-scrolling-window(list(grid), title: "Scanner", 
                                          target-view: grid,
                                          next-handler: #f,
                                          main-window: #t); */
       open(window);
       window;
     end;
