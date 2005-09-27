Module:    environment-tools
Synopsis:  Environment Tools
Author:    Andy Armstrong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constants

define constant $bullet-character     = '\<95>';
define constant $copyright-character  = '\<a9>';
define constant $registered-character = '\<ae>';
define constant $trademark-character  = '\<99>';

//--- This should probably be in the release-info library

define variable *help-file*
  = release-file("Documentation/opendylan.chm");
define variable *help-file-string*
  = as(<string>, *help-file*);

define constant $beta-release-text
  = #["This is a beta release of Functional Developer. Please submit bugs",
      "to support@functionalobjects.com.",
      "",
      "Note that distribution of software created with any beta version",
      "of Functional Developer is prohibited."];

define constant $extra-features-text
  = #["Upgrade to Functional Developer(tm)",
      "Professional or Enterprise Edition",
      "for these additional features:"];
//  = #["If you like Functional Developer Personal Edition,",
//      "upgrade to Professional or Enterprise",
//      "for these additional features:"];

define constant $extra-features-info
  = #["COM/OLE/ActiveX Support",
      "ODBC Connectivity",
      "C Library Interoperability",
      "CORBA Distributed Objects (Enterprise Edition)",
      "Cross-Network Debugging (Enterprise Edition)",
      "60-Day Free Getting Started Support",
      "Printed Documentation"];

define constant $purchasing-text
  = #["Shop on-line at the Function Developer Website or call our",
      "24-hour sales line now!"];

define constant $telesales-numbers
  = #["1 888 884 8871 (US: toll free)",
      "1 617 374 2521 (US: Boston)",
      "44 (0)1223 873883 (UK: Cambridge)"];

define constant $license-font
  = make(<text-style>, 
	 family: #"fix");
  
define constant $about-box-font
  = make(<text-style>, 
	 family: #"sans-serif", 
	 size:   #"large");

define constant $about-box-title-font
  = make(<text-style>, 
	 family: #"sans-serif",
	 size:   #"huge",
	 weight: #"bold");

define constant $about-box-copyright-font
  = make(<text-style>, 
	 family: #"sans-serif", 
	 size:   #"small");


/// License agreement

define constant $license-text-width = 72;

define frame <license-agreement-box> (<dialog-frame>)
  pane splash-screen-pane (frame)
    make(<label>, label: $splash-screen-bitmap);
  pane license-agreement-copyright-pane (frame)
    make(<text-editor>,
	 text: license-agreement-text(),
	 text-style: $license-font,
	 lines: 30, columns: $license-text-width + 5,
	 read-only?: #t, tab-stop?: #f);
  layout (frame)
    frame.license-agreement-copyright-pane;
  keyword title: = format-to-string("%s License Agreement", release-name());
  keyword cancel-callback: = #f;
  keyword center?: = #t;
end frame <license-agreement-box>;

define function split-line-at-word-break
    (line :: <string>, column :: <integer>)
 => (first-line :: <string>, rest :: false-or(<string>))
  let line-size = line.size;
  if (column >= line-size)
    values(line, #f)
  else
    let column
      = begin
	  let i :: <integer> = column;
	  while (i > 0 & line[i] ~= ' ')
	    i := i - 1
	  end;
	  if (i = 0)
	    while (i < line-size & line[i] ~= ' ')
	      i := i + 1
	    end;
	  end;
	  i
	end;
    let next-start
      = begin
	  let i :: <integer> = column;
	  while (i < line-size & line[i] == ' ')
	    i := i + 1
	  end;
	  if (i < line-size - 1) i end
	end;
    values(copy-sequence(line, end: column),
	   next-start & copy-sequence(line, start: next-start))
  end
end function split-line-at-word-break;

define function file-contents
    (filename :: <file-locator>) => (contents :: <string>)
  with-output-to-string (stream)
    block (return)
      with-open-file (file-stream = filename, direction: #"input")
	while (#t)
	  let line = read-line(file-stream, on-end-of-stream: #f);
	  unless (line) return() end;
	  while (line)
	    let (first-line, rest)
	      = split-line-at-word-break(line, $license-text-width);
	    write-line(stream, first-line);
	    line := rest
	  end
	end
      end
    end
  end
end function file-contents;

define function license-agreement-text
    () => (text :: <string>)
  file-contents(release-license-agreement-location())
end function license-agreement-text;


/// About Box

define function split-string
    (string :: <byte-string>, character :: <byte-character>)
 => (strings :: <sequence>)
  let results = make(<stretchy-vector>);
  let old-position :: <integer> = 0;
  let new-position :: <integer> = 0;
  let string-size :: <integer> = string.size;
  while (new-position < string-size)
    when (string[new-position] = character)
      add!(results, copy-sequence(string, start: old-position, end: new-position));
      while (new-position < string-size - 1 & string[new-position + 1] = ' ')
        new-position := new-position + 1
      end;
      old-position := new-position + 1;
    end;
    new-position := new-position + 1;
  end;
  if (old-position < string-size - 1)
    add!(results, copy-sequence(string, start: old-position))
  end;
  results
end function split-string;

define function use-copyright-symbols
    (string :: <byte-string>) => (new-string :: <byte-string>)
  let string-size = string.size;
  let new-string = make(<byte-string>, size: string-size);
  let i :: <integer> = 0;
  let pos :: <integer> = 0;
  while (i < string-size)
    let char = string[i];
    let (new-char, new-pos)
      = if (char == '(' & i < string-size - 2)
          select (as-uppercase(string[i + 1]))
            'C' =>
              if (string[i + 2] == ')')
                values($copyright-character, i + 3)
              end;
            'R' =>
              if (string[i + 2] == ')')
                values($registered-character, i + 3)
              end;
            'T' =>
              if (i < string-size - 3
                    & as-uppercase(string[i + 2]) == 'M'
                    & string[i + 3] == ')')
                values($trademark-character, i + 4)
              end;
            otherwise =>
              #f;
          end
        end;  
    if (new-char)
      new-string[pos] := new-char;
      i := new-pos;
    else
      new-string[pos] := string[i];
      i := i + 1;
    end;
    pos := pos + 1;
  end;
  copy-sequence(new-string, end: pos)
end function use-copyright-symbols;

define function edition-info-text
    () => (text :: <sequence>)
  let name = use-copyright-symbols(release-name());
  let version = use-copyright-symbols(release-version());
  let copyright
    = map(use-copyright-symbols, split-string(release-copyright(), '.'));
  let entries = concatenate(vector(name, version), copyright);
  let user-info-entries = user-info-text();
  concatenate(remove(entries, #f),
	      if (empty?(user-info-entries))
		#[]
	      else
		vector("")
	      end,
	      user-info-entries)
end function edition-info-text;

define function about-box-info-text
    () => (text :: <sequence>)
  let version = use-copyright-symbols(release-version());
  concatenate(vector(version), 
	      user-info-text())
end function about-box-info-text;

define function user-info-text
    () => (text :: <sequence>)
  let (serial-number, evaluation?, expiration, user, company) = license-info();
  ignore(expiration);
  let entries
    = vector(unless (release-free-edition?() | evaluation?)
	       format-to-string("Serial #%s", serial-number)
	     end,
	     user,
	     company);
  remove(entries, #f)
end function user-info-text;

define frame <about-box> (<dialog-frame>)
  pane splash-screen-pane (frame)
    make(<label>, label: $splash-screen-bitmap);
  pane user-info (frame)
    make-labels-layout
      (about-box-info-text(),
       text-style: $about-box-copyright-font);
  pane register-button (frame)
    make(<button>, 
	 label: "&Register",
	 activate-callback: method (button)
			      frame-register-developer(sheet-frame(button))
			    end);
  pane license-agreement-button (frame)
    make(<button>, 
	 label: "&License Terms",
	 activate-callback: 
	   method (button)
	     let dialog 
	       = make(<license-agreement-box>, 
		      owner: sheet-frame(button));
	     start-dialog(dialog)
	   end);
  pane ok-button (frame)
    make(<button>, 
	 label: "OK",
	 activate-callback: exit-dialog);
  pane exit-buttons (frame)
    horizontally (x-spacing: 8, equalize-widths?: #t)
      frame.register-button;
      frame.license-agreement-button;
      frame.ok-button
    end;
  layout (frame)
    vertically (spacing: 4, x-alignment: #"center")
      with-border (type: #"raised")
        frame.splash-screen-pane
      end;
      horizontally (x-spacing: 8, max-width: $fill)
        frame.user-info;
        make(<null-pane>, max-width: $fill, height: 1, max-height: $fill);
        horizontally (y-alignment: #"bottom", max-height: $fill)
          frame.exit-buttons
        end
      end
    end;
  keyword title: = format-to-string("About %s %s",
				    release-product-name(),
				    release-edition());
  keyword exit-buttons?: = #f;
  keyword center?: = #t;
  //--- This would be a good idea if DUIM didn't screw it up!
  // keyword fixed-width?:  = #t;
  // keyword fixed-height?: = #t;
end frame <about-box>;

define method initialize
    (frame :: <about-box>, #key) => ()
  next-method();
  frame-default-button(frame) := frame.ok-button
end method initialize;

define method frame-show-about-box
    (frame :: <environment-frame>) => ()
  with-frame-manager (frame-manager(frame))
    let about-box = make(<about-box>, owner: frame);
    start-dialog(about-box);
    if (show-upgrade-box?())
      frame-show-upgrade-box(frame)
    end
  end
end method frame-show-about-box;

define method do-execute-command
    (frame :: <environment-frame>, command :: <help-on-version>) => ()
  frame-show-about-box(frame)
end method do-execute-command;

/// NB This filename is agreed with the doc team and will be found in the OS Help directory.
define help-source open-dylan
  *help-file-string*
end help-source;

define method frame-help-source
    (frame :: <environment-frame>, command :: <help-command>)
 => (source :: <symbol>)
  #"open-dylan"
end method frame-help-source;


/// Beta-Release box

define frame <beta-release-box> (<dialog-frame>)
  pane beta-release-name (frame)
    make(<label>,
	 label: use-copyright-symbols(release-name()),
	 text-style: $about-box-title-font);
  pane beta-release-text (frame)
    make-labels-layout(map(use-copyright-symbols, $beta-release-text),
		       text-style: $about-box-font);
  pane beta-release-box-image-pane (frame)
    make(<label>, label: $about-box-bitmap, text-style: $about-box-font);
  pane ok-button (frame)
    make(<button>, 
	 label: "OK",
	 width: 70, min-width: 70, fixed-width?: #t,
	 activate-callback: exit-dialog,
	 default?: #t);
  pane exit-buttons (frame)
    horizontally (x-spacing: 8, equalize-widths?: #t)
      frame.ok-button
    end;
  layout (frame)
    horizontally (x-spacing: 8)
      frame.beta-release-box-image-pane;
      vertically (y-spacing: 8)
        frame.beta-release-name;
        frame.beta-release-text;
        make(<null-pane>, max-height: $fill, height: 1);
        horizontally (max-width: $fill)
          make(<null-pane>, max-width: $fill, height: 1);
          frame.exit-buttons
        end
      end
    end;
  input-focus (frame)
    frame.ok-button;
  keyword title: = "Functional Developer Beta";
  keyword exit-buttons?: = #f;
  keyword center?: = #t;
  //--- This would be a good idea if DUIM didn't screw it up!
  // keyword fixed-width?:  = #t;
  // keyword fixed-height?: = #t;
end frame <beta-release-box>;

define method initialize
    (frame :: <beta-release-box>, #key) => ()
  next-method();
  frame-default-button(frame) := frame.ok-button
end method initialize;

define method frame-show-beta-release-box
    (frame :: <environment-frame>) => ()
  with-frame-manager (frame-manager(frame))
    let frame = make(<beta-release-box>, owner: frame);
    start-dialog(frame)
  end
end method frame-show-beta-release-box;


/// Upgrade box

define frame <upgrade-box> (<dialog-frame>)
  pane edition-info (frame)
    make-labels-layout
      (edition-info-text(),
       text-style: $about-box-copyright-font);
  pane extra-features-text (frame)
    make-labels-layout(map(use-copyright-symbols, $extra-features-text),
		       text-style: $about-box-title-font);
  pane extra-features-info (frame)
    make-labels-layout(map(use-copyright-symbols, $extra-features-info),
		       prefix: format-to-string("%c ", $bullet-character),
		       text-style: $about-box-font);
  pane dylan-web-site-link (frame)
    make(<active-label>, 
	 foreground: $blue,
	 text-style: $about-box-font,
	 underline?: #t,
	 label: "Functional Developer Web Site",
	 activate-callback: method (label)
			      frame-open-dylan-web-page(sheet-frame(label))
			    end);
  pane purchasing-text (frame)
    make-labels-layout($purchasing-text, text-style: $about-box-font);
  pane telesales-info (frame)
    make-labels-layout($telesales-numbers, 
		       text-style: $about-box-font);
  pane upgrade-box-image-pane (frame)
    make(<label>, label: $about-box-bitmap, text-style: $about-box-font);
  pane purchase-button (frame)
    make(<button>,
	 label: "&Purchase",
	 activate-callback: method (button)
			      let dialog = sheet-frame(button);
			      frame-purchase-dylan(dialog)
			    end);
  pane ok-button (frame)
    make(<button>, 
	 label: "OK",
	 activate-callback: exit-dialog,
	 default?: #t);
  pane exit-buttons (frame)
    horizontally (x-spacing: 8, equalize-widths?: #t)
      frame.purchase-button;
      frame.ok-button
    end;
  pane basic-text-layout (frame)
    vertically (y-spacing: 16)
      vertically (y-spacing: 4)
	frame.extra-features-text;
        horizontally ()
          make(<null-pane>, width: 32, height: 1);
          frame.extra-features-info
        end;
	frame.purchasing-text;
        horizontally ()
          make(<null-pane>, width: 32, height: 1);
          vertically (y-spacing: 4)
            frame.dylan-web-site-link;
	    frame.telesales-info
          end
	end
      end
    end;
  layout (frame)
    horizontally (x-spacing: 8)
      vertically (y-spacing: 8)
        frame.upgrade-box-image-pane;
        horizontally (y-alignment: #"bottom", max-height: $fill)
          frame.edition-info
        end
      end;
      vertically (y-spacing: 8, x-alignment: #"right", max-width: $fill)
        frame.basic-text-layout;
        make(<null-pane>, max-height: $fill, height: 1);
        frame.exit-buttons
      end
    end;
  input-focus (frame)
    frame.ok-button;
  keyword title: = "Upgrade Functional Developer";
  keyword exit-buttons?: = #f;
  keyword center?: = #t;
  //--- This would be a good idea if DUIM didn't screw it up!
  // keyword fixed-width?:  = #t;
  // keyword fixed-height?: = #t;
end frame <upgrade-box>;

define method initialize
    (frame :: <upgrade-box>, #key) => ()
  next-method();
  frame-default-button(frame) := frame.ok-button
end method initialize;

define method frame-show-upgrade-box
    (frame :: <environment-frame>) => ()
  with-frame-manager (frame-manager(frame))
    let frame = make(<upgrade-box>, owner: frame);
    start-dialog(frame)
  end
end method frame-show-upgrade-box;


/// HELP-CREDITS

/*---*** andrewa: this isn't currently used
define constant $functional-dylan-quotes
  = vector("\"This is not the end. It is not even the beginning of the end. "
           "But it is, perhaps, the end of the beginning.\"\n"
           " -- Winston Churchill, 1942",

	   "\"What we call the beginning is often the end, "
	   "And to make an end is to make a beginning. "
	   "The end is where we start from.\"\n"
	   " -- T. S. Eliot, \"Little Gidding\", 5.",

	   "\"A hard beginning maketh a good ending.\"\n"
	   " -- John Heywood, \"Proverbes\". Part i. Chap. iv."
	   );

define variable *quote-index* :: <integer> = 0;

define frame <help-quote> (<dialog-frame>)
  keyword title: = format-to-string("%s Beginnings", release-product-name());
  keyword cancel-callback: = #f;
  pane help-credits-text-pane (help-credits)
    make(<text-editor>,
	 read-only?: #t, tab-stop?: #f,
	 value: format-to-string($functional-dylan-quotes[*quote-index*]),
	 scroll-bars: #"none",
	 lines: 4, columns: 40);
  layout (help-credits)
    with-border (type: #"raised")
      with-spacing (spacing: 16)
        help-credits.help-credits-text-pane
      end
    end;
  keyword center?: = #t;
end frame; 

define method help-credits (frame :: <about-box>)
  => ()
  with-frame-manager (frame-manager(frame))
    let frame = make(<help-quote>, owner: frame, width: 400, height: 200);
    start-dialog(frame)
  end;
  *quote-index* := floor(modulo(*quote-index* + 1, size($functional-dylan-quotes)));
end method help-credits;
*/


/// Web site command tables

define constant $purchase-web-page
  = format-to-string("%s/%s", release-web-address(), "index.phtml");

define constant $download-doc-page
  = format-to-string("%s/%s", release-web-address(), "documentation.phtml");

define function frame-open-dylan-web-page
    (frame :: <frame>, #key page = release-web-address()) => ()
  let location = as(<url>, page);
  frame-open-object(frame, location)
end function frame-open-dylan-web-page;

define function frame-purchase-dylan
    (frame :: <frame>) => ()
  frame-open-dylan-web-page(frame, page: $purchase-web-page)
end function frame-purchase-dylan;


/// Registration

define function frame-register-developer
    (frame :: false-or(<frame>),
     #key title = "Register Functional Developer",
          owner = frame)
 => (success? :: <boolean>)
  let products = unregistered-products();
  if (empty?(products))
    if (environment-question
	  (format-to-string
	     ("All %s packages on this system have been registered.\n"
		"Do you wish to purchase additional products from our web site?",
	      release-product-name()),
	   owner: owner,
	   exit-style: #"yes-no"))
      //---*** TODO: Should ensure that frame isn't #f here somehow!
      frame-purchase-dylan(owner | current-frame())
    end;
    #t
  else
    let products
      = map(method(product)
		let name = select (product)
			     #"IDE" =>
			       format-to-string("%s %s", release-product-name(),
						         release-edition());
			     #"console-tools" =>
			       "Console-Based Developer Tools";
			     otherwise =>
			       library-pack-full-name(product);
			   end;
		vector(name, product)
	    end,
	    products);
    do-register-developer-dialog(frame, products, title: title, owner: owner)
  end
end function frame-register-developer;

define function do-register-developer-dialog
    (frame :: false-or(<frame>),
     products :: <sequence>,
     #key title = "Register Functional Developer",
          owner = frame)
 => (success? :: <boolean>)
  let framem = if (owner)
                 frame-manager(owner)
               else
                 find-frame-manager()
	       end;
  with-frame-manager (framem)
    let link-button
      = make(<push-button>,
             label: "Purchase",
             activate-callback:
               method (gadget)
		 frame-purchase-dylan(sheet-frame(gadget))
	       end method);
    let products-list
      = make(<option-box>,
             items: products,
	     label-key: first,
	     value-key: second);
    let serial-number
      = make(<text-field>, min-width: 250, max-width: 250);
    let license-number
      = make(<text-field>, min-width: 250, max-width: 250);
    let group-box-1
      = grouping ("Purchase Licenses", max-width: $fill)
          horizontally (spacing: 4, y-alignment: #"center")
            vertically ()
              make(<label>, 
                   label: "If you need to purchase licenses for Functional Objects"
                            " products, please press this button.",
                   multi-line?: #t,
                   min-width: 250,
                   max-width: 250);
            end;
            link-button;
          end
        end;
    let group-box-2
      = grouping ("Register Products", max-width: $fill)
          vertically (spacing: 8)
            vertically (spacing: -2)
              make(<label>,
		   label: "To register a product, enter the serial number and license key");
              make(<label>,
		   label: "exactly as given to you by Functional Objects.  Use the 'Apply'");
              make(<label>,
		   label: "button to register multiple products.");
            end;
            make(<table-layout>,
                 columns: 2,
                 x-alignment: #(#"right", #"left"),
  	         y-spacing: 4,
                 y-alignment: #"center",
                 children:
                   vector(make(<label>, label: "Select Product:"),
                          products-list,
                          make(<label>, label: "Serial number:"),
                          serial-number,
                          make(<label>, label: "License key:"),
                          license-number))
            end
        end;
    let exit-button
      = make(<push-button>,
             label: "OK",
             activate-callback:
               method (gadget)
                 let dialog = sheet-frame(gadget);
                 if (do-register-product(dialog, products-list, serial-number, license-number))
                   exit-dialog(dialog)
                 end;
               end);
    let cancel-button
      = make(<push-button>,
             label: "Cancel",
             activate-callback: cancel-dialog);
    let apply-button
      = make(<push-button>,
             label: "Apply",
             activate-callback:
               method (gadget)
                 let dialog = sheet-frame(gadget);
                 do-register-product(dialog, products-list, serial-number, license-number);
                 if (empty?(gadget-items(products-list)))
                   exit-dialog(dialog)
                 end;
               end method);
    let layout
      = make(<column-layout>,
             x-alignment: #"right",
             y-spacing: 10,
             children:
               vector(vertically (spacing: 10, max-width: $fill)
                        group-box-1;
                        group-box-2;
                      end,
                      make(<row-layout>,
                           x-alignment: #"right",
                           x-spacing: 4, equalize-widths?: #t,
                           children: vector(exit-button, cancel-button, apply-button))));
    
    let dialog
      = make(<dialog-frame>,
             title: title,
             layout: layout,
             exit-buttons?: #f,
             exit-button:   exit-button,
             cancel-button: cancel-button,
             mode: #"modal",
             owner: frame,
             width: 360,
             fixed-width?: #t,
             fixed-height?: #t);
    let success? = start-dialog(dialog);
    if (success?)
      #t
    end
  end;
end function do-register-developer-dialog;

/// All the extra arguments are needed because we've built the dialog by hand ...
define function do-register-product
    (frame :: <frame>, products-list :: <option-box>,
     serial-number :: <text-field>, license-number :: <text-field>)
 => (success? :: <boolean>)
  let product = gadget-value(products-list);
  let serial = gadget-value(serial-number);
  let key = gadget-value(license-number);
  if (size(serial) ~= size("FDeee-vvvv-nnnnnnnnnnnn"))
    environment-error-message("Malformed serial number; "
				"please correct your errors and try again.",
			      owner: frame);
    #f
  elseif (size(key) ~= size("aaaabbbbccccxxxx"))
    environment-error-message("Malformed license key; "
				"please correct your errors and try again.",
			      owner: frame);
    #f
  else
    let success?
      = block()
	  register-product(product, serial, key)
	exception (c :: <license-validation-failure>)
	  environment-error-message(format-to-string
				      ("%s\nPlease correct your errors and try again.",
				       c),
				    owner: frame);
	  #f
	end;
    if (success?)
      let old-products = gadget-items(products-list);
      let new-products = make(<vector>, size: size(old-products) - 1);
      unless (zero?(size(new-products)))
	let j :: <integer> = 0;
	for (i from 0 below size(old-products))
	  unless (product = second(old-products[i]))
	    new-products[j] := old-products[i];
	    j := j + 1
	  end;
	end;
	gadget-value(products-list) := second(new-products[0]);
      end;
      gadget-items(products-list) := new-products;
    end;
    success?
  end
end function do-register-product;


/// Help command table

define settings <documentation-settings> (<open-dylan-local-settings>)
  key-name "OnlineHelp";
  slot doctype :: <symbol> = #"None";
  slot docpath :: <string> = "";
end settings <documentation-settings>;

define constant $documentation-settings = make(<documentation-settings>);

define function online-doc-installed?
    () => (installed? :: <boolean>)
  $documentation-settings.doctype ==  #"HTMLHelp"
end function online-doc-installed?;

define function online-doc-location
    () => (location :: false-or(<file-locator>))
  let path = $documentation-settings.docpath;
  $documentation-settings.doctype ~= #"None"
    & path
    & ~empty?(path)
    & as(<file-locator>, path)
end function online-doc-location;

define sideways method frame-help-contents-and-index
    (frame :: <frame>) => ()
  do-frame-help(frame, <help-on-topics>)
end method frame-help-contents-and-index;

define sideways method frame-help-on-keyword
    (frame :: <frame>, keyword) => ()
  do-frame-help(frame, <help-on-keyword>, keyword: keyword)
end method frame-help-on-keyword;

//--- Hack because HTMLHelp doesn't seem to like multiple threads chattering away to it
define method do-frame-help
    (frame :: <frame>, class :: subclass(<command>), #rest initargs) => ()
  let location = online-doc-location();
  case
    online-doc-installed?() =>
      let primary = environment-primary-frame();
      // Make sure the primary frame is in front, to make sure the Help window
      // doesn't appear behind other windows because it is owned by the primary frame.
      call-in-frame(primary, deiconify-frame, primary);
      call-in-frame(primary, raise-frame, primary);
      let command = apply(make, class, server: primary, initargs);
      call-in-frame(primary, execute-command, command);
    location & file-exists?(location) =>
      frame-open-object(frame, location);
    otherwise =>
      if (environment-question
	    (format-to-string
	       ("%s online documentation is not installed.  "
		  "Download it from the web?",
		release-product-name()),
	     owner: frame,
	     exit-style: #"yes-no"))
	frame-open-dylan-web-page(frame, page: $download-doc-page)
      end;
  end
end method do-frame-help;

// "Service" function, mainly for use by environment-manager library.
define sideways method show-documentation
    (name :: <string>,
     project :: false-or(<project-object>),
     module :: false-or(<module-object>),
     object :: false-or(<definition-object>))
 => (success? :: <boolean>)
  let frame = environment-primary-frame();
  frame-help-on-keyword(frame, name)
end method show-documentation;


// define constant $tutorial-title = "Tutorial"; // ---*** not used yet

//---*** The release-info backend isn't available during initialization... :-(
//define constant $help-about-title = format-to-string("About %s", release-product-name());
define constant $help-about-title = "About Open Dylan";

//---*** Not yet implemented
/*
define command-table *environment-tutorial-command-table* (*global-command-table*)
  menu-item $tutorial-title = frame-start-tutorial,
    documentation: "Starts the Dylan tutorial.";
end command-table *environment-tutorial-command-table*;
*/

// Nope, not for the open-source version --tc
/*
define command-table *environment-web-links-command-table* (*global-command-table*)
end command-table *environment-web-links-command-table*;

add-command-table-menu-item
  (*environment-web-links-command-table*, "", <push-box>, #f,
   update-callback: method (menu-box :: <menu-box>)
		      let items
			= vector(format-to-string("Register %s",
						  release-product-name()));
		      gadget-items(menu-box) := items
		    end,
   label-key: identity,
   callback: method (menu-box :: <menu-box>)
	       let frame = sheet-frame(menu-box);
	       frame-register-developer(frame)
             end);
*/

define command-table *environment-help-about-command-table* (*global-command-table*)
  menu-item $help-about-title = <help-on-version>,
    documentation: "Displays program information, version, and copyright.";
end command-table *environment-help-about-command-table*;

define command-table *environment-specific-help-command-table* (*global-command-table*)
  //---*** Not yet implemented!
  // include *environment-tutorial-command-table*;
  // We don't have any Web links at the moment. --tc
  // include *environment-web-links-command-table*;
  include *environment-help-about-command-table*;
end command-table *environment-specific-help-command-table*;

define command-table *environment-help-command-table* (*global-command-table*)
  include *help-command-table*;
  include *environment-specific-help-command-table*;
end command-table *environment-help-command-table*;
