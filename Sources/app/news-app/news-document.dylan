language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <news-document> (<document>)
  slot text-handle, init-value: $null-machine-pointer;
  slot article-number, init-keyword: article-number:, init-value: #f;
  slot group, init-keyword: group:, init-value: #f;
end class;

define method main-file-type (doc :: <news-document>)
  ignore(doc);
  ostype("TEXT");
end method;

define method make-windows (document :: <news-document>)
  let read-new-news? = nil?(document.text-handle);
  let text-view = make(<text-view>, 
                       location: $zero-point, 
                       extent: point(400 - 16, 400),
                       text-style: $geneva9);
  text-view.location := top-left(text-view.inset);
  add-determiner(text-view, make(<super-view-relative-determiner>));
  
  let window = make-scrolling-window(list(text-view), title: document.title, 
                                     target-view: text-view,
                                     next-handler: document,
                                     main-window: #t,
                                     location: point(100, 100),
                                     extent: point(400, 400),
                                     closable: #t, zoomable: #t, resizable: #t);
  SetCursor(as(<Cursor>, pointer-at(as(<machine-pointer>, *watch-cursor*))));
  if (read-new-news?)
    block ()
      startup-news();
      let (title-str, text) = read-one-news(document.group, document.article-number);
      document.title := title-str;
      window.title := title-str;
      open(window);
      if (text)
        set-text(text-view, text);
      end;
      document.text-handle := get-text-handle(text-view);
    cleanup
      cleanup-news();
    end block;
  else
    set-text(text-view, document.text-handle);
    open(window);
  end if;
  
  SetCursor($arrow-cursor);
end method;

define method read-from-file (document :: <news-document>,
                              file :: <file>)  
  let length = get-end-of-file(file);
  document.text-handle := NewHandle(length);
  fail-nil(document.text-handle);
  
  block ()
    HLock(document.text-handle);
    let ptr = pointer-at(document.text-handle);
    
    read-data(file, ptr, length);
  cleanup
    HUnlock(document.text-handle);
  end block;
end method;

define method write-to-file (document :: <news-document>,
                             file :: <file>,
                             making-copy? :: <boolean>)
  ignore(making-copy?);
  set-end-of-file(file, 0);
  
  block ()
    HLock(document.text-handle);
    let ptr = pointer-at(document.text-handle);
    let length = GetHandleSize(document.text-handle);
    
    write-data(file, ptr, length);
  cleanup
    HUnlock(document.text-handle);
  end block;
end method;

/* framework hacks */

define method set-text (view :: <text-view>, str :: <string>, 
                        #key style-handle = as(<StScrpHandle>, 0) ) => ()
  // workaround, framework doesn't handle stretchy strings.
  set-text(view,  as(<byte-string>, str), style-handle: style-handle);
end method;

define method request-file-name (default-name :: <string>, file-type)
    // workaround, framework doesn't handle stretchy strings.
  request-file-name(as(<byte-string>, default-name), file-type);
end;