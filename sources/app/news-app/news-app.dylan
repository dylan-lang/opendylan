language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <news-app-behavior> (<behavior>)
end class;

define method behavior-setup-menus (b :: <news-app-behavior>,
                                    n :: <list>,
                                    h :: <main-handler>)
  next-method();
  
  enable-item(#"new");
  
  enable-item(#"Scan");
  enable-item(#"Server");
  enable-item(#"Query-dialog");
end method;

define method behavior-event (b :: <news-app-behavior>,
                              n :: <list>,
                              h :: <main-handler>,
                              event :: <menu-event>,
                              id == #"new")
  ignore(b, n, h, event, id);
  
  // open(make(<news-document>));
  tell-user-dialog("New Query",
                   "Makeing new query documents is not yet implemented.");
 end method;

define method behavior-event (b :: <news-app-behavior>,
                              n :: <list>,
                              h :: <main-handler>,
                              event :: <menu-event>,
                              id == #"server")
  ignore(b, h, n, event, id);
  let (doit?, host) = ask-user-dialog("News Server", "Enter news server address:", *news-host*);
  if (doit? & (host ~= *news-host*))
    *news-host* := host;
  end;
end method;

define method behavior-event (b :: <news-app-behavior>,
                              n :: <list>,
                              h :: <main-handler>,
                              event :: <menu-event>,
                              id == #"about-box")
  ignore(b, h, n, event, id);
  tell-user-dialog("About News App",
                   "News App is a program that can run queries on various news groups.  "
                   "MacTCP must be installed and this machine must have access to a news server.\n"
                   "By Derek White.  © Copyright Apple Computer, Inc. 1994-1995." ,
                   size-h: 450,
                   size-v: 160,
                   cancel-text: #f);
  #t;
end method;

define method behavior-close (behavior :: <news-app-behavior>, 
                              event-handler :: <event-handler>) => ()
  next-method();
  news-app-cleanup();
end method

define method news-app-cleanup ()
  close-resolver();
end;

define method make-my-menus ()
   // Set up the menus.
  
  make(<menu>,
       title: "File",
       install: #t,
       items: list(
                    make(<menu-item>,
                         title: "New Query",
                         identifier: #"new",
                         command-key: 'N'),
                    make(<menu-item>,
                         title: "Open…",
                         identifier: #"open",
                         command-key: 'O'),
                    make(<separator-item>),
                      make(<menu-item>,
                           title: "Close",
                           identifier: #"close",
                           command-key: 'W'),
                    make(<menu-item>,
                         title: "Save",
                         identifier: #"save",
                         command-key: 'S'),
                    make(<menu-item>,
                         title: "Save As…",
                         identifier: #"save-as"),
                    make(<menu-item>,
                         title: "Save a Copy…",
                         identifier: #"save-copy"),
                    make(<menu-item>,
                         title: "Revert…",
                         identifier: #"revert"),
                    make(<separator-item>),
                    make(<menu-item>,
                           title: "Page Setup…",
                           identifier: #"page-setup"),
                    make(<menu-item>,
                         title: "Print…",
                         identifier: #"print",
                         command-key: 'P'),
                    make(<separator-item>),
                    make(<menu-item>,
                           title: "Quit",
                           identifier: #"quit",
                           command-key: 'Q')));
  
  make-edit-menu(extra-items: list(make(<menu-item>,
                                        title: "Select All",
                                        identifier: #"select-all",
                                        command-key: 'A')));
  
  make(<menu>,
       title: "News",
       install: #t,
       items: list(make(<menu-item>,
                        title: "Set Server",
                        identifier: #"Server")));
  
  // if (*has-oce-toolbox*)
  //  init-standard-mailer();
  //  make-mail-menu();
  // end if;
  
  make-debug-menu();
end;

define variable *ran-once* = #f;

define method init-app () => ()  
  unless(*ran-once*)
    make-my-menus();
    
    add-behavior(*main-handler*, make(<news-app-behavior>));
    
    add-document-type(*main-handler*, ostype("TEXT"), <news-document>);
    *ran-once* := #t;
    
    *status-window* := make-status-window();
    *scanner-window* := make-scanner-window();
  end;
end method;

define method reset-app () => ()  
  if (*status-window*) close(*status-window*); *status-window* := #f; end;
  if (*scanner-window*) close(*scanner-window*); *scanner-window* := #f; end;
end method;

set-library-init-function(init-app);

set-library-reset-function(reset-app); // this should get called at shutdown too.