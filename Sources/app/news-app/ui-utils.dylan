language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method center-screen-location (size-h, size-v)
  let center-h = floor/(width(*main-screen-bounds-rect*), 2);
  let center-v = floor/(height(*main-screen-bounds-rect*), 2);
  let offset-h = floor/(size-h, 2);
  let offset-v = floor/(size-v, 2);
  point(center-v - offset-v, center-h - offset-h);
end;

define method ask-user-dialog (title :: <string>, prompt :: <string>, initial-text :: <string>,
                               #key size-h = 350, size-v = 150)
  => (ok?, user-string)
  let the-window = make(<window>, 
                        location: center-screen-location(size-h, size-v),
                        extent: point(size-h, size-v),
                        closable: #f, 
                        modal: #t,
                        title: title);
      
  let ok-button = make(<ctl-mgr-button>, 
                       location: point(size-h - 100, size-v - 40),
                       extent: point(80, 20),
                       title: "OK", dismisses-dialog: #t,
                       default: #t);
  
  add-sub-view(the-window.root-view, ok-button);

  let cancel-button = make(<ctl-mgr-button>, 
                           location: point(16, size-v - 40),
                       extent: point(80, 20),
                          title: "Cancel", dismisses-dialog: #t);
  
  add-sub-view(the-window.root-view, cancel-button);
  
  let prompt-text = make(<static-text>, text: prompt,
                         location: point(16, 16),
                         extent: point(size-h - 2, 20));
    
  add-sub-view(the-window.root-view, prompt-text);
      
  let edit-text = make(<edit-text>, 
                       location: point(16, 60),
                       extent: point(size-h - 20, 20),
                       text: initial-text);
  add-adorner(edit-text, make(<frame-adorner>));
  
  add-sub-view(the-window.root-view, edit-text);
  the-window.target-view := edit-text;
  
  open(the-window);
  
  let dismisser = pose-modally(the-window, default: ok-button, cancel: cancel-button);
  
  let text = get-text(edit-text);
  
  close(the-window);
  
  // return #t if the user clicked OK
  if (dismisser = ok-button)
    values(#t, text);
  else
    values(#f, #f);
  end if;
end method;

define method tell-user-dialog (title :: <string>, message :: <string>,
                                #key size-h = 350,
                                     size-v = 150,
                                     ok-text = "OK",
                                     cancel-text = "Cancel"
                               )
  => (ok? :: <boolean>);
  // a better version of framework's tell-dialog.
  
  let the-window = make(<window>, 
                        location: center-screen-location(size-h, size-v),
                        extent: point(size-h, size-v),
                        closable: #f, 
                        modal: #t,
                        title: title);
                                                      
  let ok-button = if (ok-text)
                    let ok-button = make(<ctl-mgr-button>, 
                                         location: point(size-h - 100, size-v - 40),
                                         extent: point(80, 20),
                                         title: ok-text, dismisses-dialog: #t,
                                         default: #t);
                    add-sub-view(the-window.root-view, ok-button);
                  end;
  
  let cancel-button = if (cancel-text)
                        let cancel-button = make(<ctl-mgr-button>, 
                                                 location: point(16, size-v - 40),
                                                 extent: point(80, 20),
                                                 title: cancel-text, dismisses-dialog: #t);
                        add-sub-view(the-window.root-view, cancel-button);
                      end;
  
  let prompt-text = make(<static-text>, text: message,
                         location: point(16, 16),
                         extent: point(size-h - 12, size-v - 65));
  add-sub-view(the-window.root-view, prompt-text);
  
  open(the-window);
  
  let dismisser = pose-modally(the-window, default: ok-button, cancel: cancel-button);
    
  close(the-window);
  
  // return #t if the user clicked OK
  if (dismisser = ok-button)
    #t;
  else
    #f;
  end if;
end method;

define method new-template-window (id :: <integer>, #key open? = #t) => (new-window)
  let resHandle = get-resource(ostype("dwin"), id);
  fail-nil(resHandle);
  
  let stream = make(<handle-object-stream>, handle: resHandle);
  let aWindow = read-object(stream);
  
  // ¥ Open the window so that we can see it
  if (open?)
    open(aWindow);
  end;
  aWindow;
end method;

define variable *watch-cursor* = GetCursor($watchCursor);