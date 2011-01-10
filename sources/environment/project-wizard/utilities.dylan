Module:    utilities
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ----------------------------------------------------------------------
/// HANDY UTILITIES

define function strings-size-info
    (drawable /* :: <drawable> */, strings :: <vector> /* of: <string> */)
 => (total-width :: <real>, min-width :: <real>, max-width :: <real>,
     total-height :: <real>, min-height :: <real>, max-height :: <real>)
  let (tw, w-, w+, th, h-, h+) = values(0, 0, 0, 0, 0, 0);
  for (str in strings)
    let (width, height) = text-size(drawable, str);
    tw := tw + width;
    th := th + height;
    when (width > w+)  w+ := width end;
    when (height > h+) h+ := height end;
    when (width < w-)  w- := width end;
    when (height < h-) h- := height end;
  end;
  values(tw, w-, w+, th, h-, h+)
end function;

// Given a pathname which may refer to a non-existent directory, try to
// find some ancestor directory (includind the directory itself) which does
// exist.
define function find-existing-ancestor
    (directory :: <directory-locator>)
 => (ancestor :: false-or(<directory-locator>))
  local method %find-existing-ancestor
      (directory :: <directory-locator>)
   => (ancestor :: false-or(<directory-locator>))
    if (file-exists?(directory))
      directory
    else
      let parent = directory.locator-directory;
      unless (parent = directory)
        %find-existing-ancestor(directory.locator-directory)
      end;
    end
  end method;
  %find-existing-ancestor(directory)
end function find-existing-ancestor;

define function maybe-ensure-project-directory
    (directory :: <directory-locator>, #key owner)
 => (dir-ok? :: <boolean>)
  if (file-exists?(directory))
    // If it's not empty, warn the user
    if (directory-empty?(directory))
      #t
    else
      let dir-string = as(<string>, directory);
      let message
	= concatenate
	    ("The directory '", dir-string, "' already exists"
	     " and is not empty.\nAre you sure you want to use it?\n"
	     "If not, you must enter a different location for the project.\n");
      notify-user
	(message, style: #"information", exit-style: #"yes-no",
	 owner: owner);
    end
  else
    ensure-directories-exist(directory) & #t
    //---*** What if it fails to create the directories?
  end;
end function maybe-ensure-project-directory;


/// ----------------------------------------------------------------------
/// CHOICE
/// Class encapsulating an object and a flag determining whether or not
/// it is in some sense "included" (or "chosen").  The <choice> can also
/// encapsulate "children" of the encapsulated object, which are regarded
/// as "included" iff they _and_ their "parent" are "included".

// --- Yes, I know "choice" is a stupid, over-general, uninformative name.

define sealed class <choice> (<object>)
  sealed constant slot choice-object,
    required-init-keyword: object:;
  sealed slot choice-children :: false-or(<vector>) = #f,
    init-keyword: children:;
  sealed slot choice-included? :: <boolean> = #f,
    init-keyword: included?:,
    setter: %included?-setter;
end class;

define sealed method choice-included?-setter
    (included? :: <boolean>, choice :: <choice>, #key recursive?)
 => (included? :: <boolean>)
  let children = choice.choice-children;
  when (recursive? & children)
    for (child in children)
      child.choice-included? := included?;
    end;
  end;
  choice.%included? := included?
end;

define function all-included-choices
    (choices :: <vector>) // of: <choice>
 => (included-choices :: <vector>) // of: <choice>
  if (empty?(choices))
    choices
  else
    let included-choices = make(<stretchy-vector>);
    reduce1(method (choice)
	      when (choice.choice-included?)
		add!(included-choices,
		     make(<choice>, object: choice.choice-object,
			  children:
			    all-included-choices(choice.choice-children),
			  included?: #t))
	      end;
	    end,
	    choices);
    included-choices
  end
end function;


// This is intended to be used in a call to 'union' two sequences of
// same-type <choices>.  If the two are the same according to 'test'
// (which you normally want to supply as '\='), it will mark both
// as 'included?' if either is.  If 'recursive?' is true, it will do
// the same 'union' for the two choice-children lists, and give both
// choices the same resulting children.
//
// The intended use of this is to merge two lists of library-choices
// so that all the required libraries and modules are included.

define function union-choice-inclusion!
    (choice1 :: <choice>, choice2 :: <choice>,
     #key test :: false-or(<function>) = \==,
          recursive? :: <boolean> = #f)
 => (same? :: <boolean>)
  when (test(choice1, choice2))
    let included? = choice1.choice-included? | choice2.choice-included?;
    choice1.choice-included? := included?;
    choice2.choice-included? := included?;

    when (recursive?)
      let children1 = choice1.choice-children;
      let children2 = choice2.choice-children;
      let new-children
        = if (children1 & children2)
	    union(children1, children2,
		  test: rcurry(union-choice-inclusion!,
			       test: test, recursive?: #t));
	  else
	    // Take whichever of the "sequences" is really a <sequence>
	    children1 | children2
	  end;
      new-children := new-children & as(<vector>, new-children);
      choice1.choice-children := new-children;
      choice2.choice-children := new-children;
    end;
    #t
  end;
end function union-choice-inclusion!;

define sealed method \< (choice1 :: <choice>, choice2 :: <choice>)
 => (less? :: <boolean>)
  choice1.choice-object < choice2.choice-object
end method;

define sealed method \= (choice1 :: <choice>, choice2 :: <choice>)
 => (equal? :: <boolean>)
  choice1.choice-object = choice2.choice-object
end method;



/// ----------------------------------------------------------------------
/// FILE-BROWSE PANE CLASS
/// A "gadget" incorporating a text field and a "Browse..." button.
/// Pressing the button pops up a file dialog and selecting a file
/// (or directory) in the dialog (and then "OK"ing the dialog) will
/// set the text field to that string.

define pane <file-browse-pane> ()
  // Arguments to pass to the choose-file dialog.
  constant slot %file-browse-function-initargs :: <sequence>,
    required-init-keyword: browse-function-initargs:;
  slot file-browse-function :: <function> = choose-file,
    init-keyword: browse-function:;
  pane file-browse-text-pane (pane)
    make(<text-field>);
  pane file-browse-button (pane)
    make(<push-button>, label: "Browse...",
	 activate-callback:
	   method (pb)
	     // Do choose-file and update text pane with results.
	     let location // ignore "filter"
	       = apply(pane.file-browse-function,
		       pane.%file-browse-function-initargs);
	     when (instance?(location, <string>))
	       gadget-value(pane.file-browse-text-pane, do-callback?: #t) 
		  := location;
	     end;
	   end);
  layout (pane)
    horizontally (x-spacing: 8, y-alignment: #"top", equalize-heights?: #t)
      pane.file-browse-text-pane;
      pane.file-browse-button;
    end;
end pane;

define method make
    (class == <file-browse-pane>, #rest initargs, #key, #all-keys)
 => (pane :: <file-browse-pane>)
  apply(next-method, class, browse-function-initargs: initargs, initargs)
end method;

define method initialize
    (pane :: <file-browse-pane>, #key value, #all-keys)
  next-method();
  gadget-value(pane.file-browse-text-pane) := value | "";
end method initialize;

define function file-browse-pane-enabled?-setter
    (enabled? :: <boolean>, pane :: <file-browse-pane>)
 => (enabled? :: <boolean>)
  gadget-enabled?(pane.file-browse-text-pane) := enabled?;
  gadget-enabled?(pane.file-browse-button) := enabled?;
  enabled?
end function;



/// ----------------------------------------------------------------------
/// TEXT-FIELD-OPTION PANE CLASS

define pane <text-field-option> ()
  sealed constant slot %label :: <string>,
    required-init-keyword: label:;
  sealed constant slot text-field-option-text-field :: <text-field>,
    required-init-keyword: text-field:;
  sealed slot text-field-option-text-field-value = "";
  pane text-field-option-check-button (pane)
    make(<check-button>, label: pane.%label, value: #t,
	 value-changed-callback:
	   method (cb)
	     text-field-option-enabled?(pane) := gadget-value(cb)
	   end);
  layout (pane)
    pane.text-field-option-check-button;
end pane;

define method initialize
    (pane :: <text-field-option>, #key enabled? = #t, value, #all-keys)
  next-method();
  if (~enabled?)
    text-field-option-enabled?(pane) := #f
  end;
  when (value)
    let text-field = pane.text-field-option-text-field;
    gadget-value(text-field) := value;
  end
end method initialize;

define method text-field-option-enabled?-setter
    (enabled? :: <boolean>, pane :: <text-field-option>) 
 => (enabled? :: <boolean>)
  let button = pane.text-field-option-check-button;
  gadget-value(button) := enabled?;
  gadget-enabled?(pane.text-field-option-text-field) := enabled?;
  // Whenever we change, swap the current value and our empty
  // string, so that the disabled text field is blank and has
  // an empty value when we retreive it to write the project.
  let temp = pane.text-field-option-text-field-value;
  pane.text-field-option-text-field-value
    := gadget-value(pane.text-field-option-text-field);
  gadget-value(pane.text-field-option-text-field) := temp;
  let cb = pane.text-field-option-text-field.gadget-value-changed-callback;
  cb & cb(pane.text-field-option-text-field);
  enabled?
end method text-field-option-enabled?-setter;

define function text-field-option-value
    (pane :: <text-field-option>)
 => (value :: false-or(<string>))
  gadget-value(pane.text-field-option-check-button)
    & gadget-value(pane.text-field-option-text-field)
end function;


/* #### MULTI-LINE TEXT #############################

/// ----------------------------------------------------------------------
/// MULTI-LINE TEXT PANE CLASS

define pane <multi-line-text-pane> ()
  sealed slot %labels :: <sequence>;
  layout (pane)
    make(<column-layout>, children: pane.%labels, y-spacing: 2);
  required keyword labels:, type: <sequence>;
end pane;

define method make
    (class == <multi-line-text-pane>, #key labels, #all-keys)
  let real-labels = map(method (l) make(<label>, label: l) end, labels);
  let space-req
    = make(<space-requirement>,
	   function:
	     method (mltp)
	       let (tw, w-, w+, th, h-, h+)
		 = strings-size-info(port(mltp), labels);
	       values(w+, w+, $fill, th, th, $fill)
	     end);
  apply(next-method, class, labels: real-labels,
	space-requirement: space-req, #())
end method initialize;



/// ----------------------------------------------------------------------
/// MULTI-PAGE PANE CLASS

// I'm only faking up the minimum I need here.

define pane <multi-page-pane> ()
  sealed slot pages :: <sequence>, // of: <pair>(label, page)
    required-init-keyword: pages:;
  sealed slot current-page,
    setter: %current-page-setter;
  layout (pane)
    /* ??? */;
  keyword current-page:;
end pane;

define method current-page-setter
    (page-label, pane :: <multi-page-pane>)
 => (page-label)
  let the-page = #f;
  for (page in pane.pages, until: the-page)
    when (head(page) == page-label)
      the-page := tail(page);
    end;
  end;
  when (the-page)
    sheet-mapped?(pane.current-page) := #f;
    pane.%current-page := the-page;
    sheet-mapped?(pane.current-page) := #t;
    relayout-parent(sheet-parent(the-page));
  end;
end method;

define method initialize
    (pane :: <multi-page-pane>, #key pages, current-page, #all-keys)
  debug-assert("Attempt to create <multi-page-pane> with no pages",
	       ~empty?(pages));
  next-method();
  pane.current-page := current-page | head(pages[0]);
end method initialize;
#### MULTI-LINE TEXT ############################# */
