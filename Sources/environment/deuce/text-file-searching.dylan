Module:    environment-deuce
Synopsis:  Searching Text Files
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project Sources search domain; searches sources of current project

// Search domain for searching through text files that may be open
// in the editor, or on disk
define abstract class <text-file-search-domain> (<search-domain>)
end class <text-file-search-domain>;

// Search domain for searching project source files
define abstract class <project-search-domain> (<text-file-search-domain>)
  constant slot search-domain-project :: <project-object>,
    required-init-keyword: project:;
end class <project-search-domain>;

// Search domain for direct project source files that are text files
define class <direct-project-sources-search-domain> (<project-search-domain>)
end class <direct-project-sources-search-domain>;

// Search domain for all project source files (including subproject sources)
define class <all-project-sources-search-domain> (<project-search-domain>)
end class <all-project-sources-search-domain>;

// Create search domains for a project when it is opened
define method search-domain-project-opened-receiver
    (message :: <project-opened-message>) => ()
  let project       = message.message-project;
  let direct-domain = make(<direct-project-sources-search-domain>, project: project);
  let all-domain    = make(<all-project-sources-search-domain>,    project: project);
  register-search-domain(direct-domain);
  register-search-domain(all-domain);
end method search-domain-project-opened-receiver;

// Remove domain when its project is closed
define method search-domain-project-closed-receiver
    (message :: <project-closed-message>) => ()
  let project = message.message-project;
  let obsolete-domains = make(<stretchy-vector>);
  do-search-domains
    (method (domain :: <search-domain>)
       when (instance?(domain, <project-search-domain>)
	     & domain.search-domain-project == project)
	 add!(obsolete-domains, domain)
       end
     end method);
  for (domain :: <search-domain> in obsolete-domains)
    unregister-search-domain(domain)
  end
end method search-domain-project-closed-receiver;

tune-in($project-channel,
	search-domain-project-opened-receiver,
	message-type: <project-opened-message>);

tune-in($project-channel,
	search-domain-project-closed-receiver,
	message-type: <project-closed-message>);

// Search domain UI labels
define constant $text-file-search-domain-target-kind-label  = "file";
define constant $direct-project-sources-search-domain-label = "Project '%s' Sources";
define constant $all-project-sources-search-domain-label    = "Project '%s' Sources"
                                                               " && Subproject Sources";

define method project-search-domain-base-label
    (domain :: <direct-project-sources-search-domain>) => (label :: <string>)
  $direct-project-sources-search-domain-label
end method project-search-domain-base-label;

define method project-search-domain-base-label
    (domain :: <all-project-sources-search-domain>) => (label :: <string>)
  $all-project-sources-search-domain-label
end method project-search-domain-base-label;

define method search-domain-label
    (domain :: <project-search-domain>) => (label :: <string>)
  let project = domain.search-domain-project;
  format-to-string(project-search-domain-base-label(domain),
		   environment-object-primitive-name(project, project))
end method search-domain-label;

define function direct-project-text-source-files
    (project :: <project-object>) => (locators :: <sequence>)
  let locators  = make(<stretchy-vector>);
  // Locators are relative to the project directory
  let directory = project-directory(project);
  // Get Dylan source records
  for (source :: <source-record> in project-sources(project))
    let locator = source.source-record-location;
    when (locator & object-has-source?(project, locator))
      let merged-locator = merge-locators(locator, directory);
      locators := add!(locators, merged-locator)
    end;
  end;
  // Get other source files
  for (locator :: <file-locator> in project-other-sources(project))
    when (object-has-source?(project, locator))
      let merged-locator = merge-locators(locator, directory);
      locators := add!(locators, merged-locator)
    end;
  end;
  locators
end function direct-project-text-source-files;

// Return direct project text source files
define method search-domain-targets
    (domain :: <direct-project-sources-search-domain>) => (locators :: <sequence>)
  let project = domain.search-domain-project;
  direct-project-text-source-files(project)
end method search-domain-targets;

// Return direct and subproject text source files
define method search-domain-targets
    (domain :: <all-project-sources-search-domain>) => (locators :: <sequence>)
  let project  = domain.search-domain-project;
  let locators = make(<stretchy-vector>);
  local method add-project-source-files (project :: <project-object>) => ()
	  for (locator :: <file-locator> in direct-project-text-source-files(project))
	    locators := add!(locators, locator);
	  end;
	end method;
  add-project-source-files(project);
  for (used-project in project-used-projects(project, indirect?: #t))
    add-project-source-files(used-project);
  end;
  locators
end method search-domain-targets;

define method search-domain-target-label
    (domain :: <text-file-search-domain>, locator :: <file-locator>)
 => (label :: <string>)
  locator-name(locator)
end method search-domain-target-label;

define method search-domain-target-kind-label
    (domain :: <text-file-search-domain>, locator :: <file-locator>)
 => (label :: <string>)
  $text-file-search-domain-target-kind-label
end method search-domain-target-kind-label;

//--- cpage: 1998.07.27 This is not yet called, since we have no UI that displays this.
/*
define method search-domain-target-icon
    (domain :: <text-file-search-domain>, locator :: <file-locator>)
 => (icon :: false-or(<image>))
  // TBD: Return the same icon displayed in the Sources page
  #f
end method search-domain-target-icon;
*/

define method search-domain-target-can-find?
    (domain :: <text-file-search-domain>, locator :: <file-locator>)
 => (can-find? :: <boolean>)
  #t
end method search-domain-target-can-find?;

define method search-domain-target-can-replace?
    (domain :: <text-file-search-domain>, locator :: <file-locator>)
 => (can-replace? :: <boolean>)
  // Always return #t. If a read-only file is encountered during a replace operation,
  // complain about it then.
  #t
end method search-domain-target-can-replace?;

define method search-domain-find
    (domain        :: <text-file-search-domain>,
     locator       :: <file-locator>,
     search-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  ignore(domain);
  // Find an existing buffer or create on in Fundamental mode
  //--- Not clear to me if the expense of creating a buffer is worth it...
  let (buffer, new-buffer?) = find-buffer-for-file(locator);
  // If searching from selection (and the buffer is not new, so there
  // is a selection), get a frame for the buffer, then defer
  // searching to find-in-frame; else search in the buffer and only get
  // a frame if there's a match. This way, we don't affect a frame's
  // current buffer setting if there's no match.
  if (from-selection? & ~new-buffer?)
    let frame  = find-frame-for-buffer(buffer);
    let window = frame-window(frame);
    // Make sure the buffer is the current frame buffer
    when (buffer ~= frame-buffer(frame))
      select-buffer(window, buffer);
      queue-redisplay(window, $display-all);
    end;
    let object = apply(find-in-frame, frame, search-string, keys);
    // Update the display
    call-in-frame(frame, redisplay-window, window);
    object & pair(locator, object)
  else
    let object = find-in-buffer(buffer,
				search-string,
				backwards?:  backwards?,
				wrap?:       wrap?,
				match-case?: match-case?,
				match-word?: match-word?);
    case
      object == #f & new-buffer? =>
	// If no match and the buffer is new, close it
	kill-buffer(buffer, frame: #f, editor: $environment-editor);
      new-buffer? =>
	// We got a match, put the new buffer into the right major mode
	// and sectionize it.  We delay sectionization until a successful
	// match just to make things a bit faster.
	let container  = buffer-source-container(buffer);
	let major-mode = find-mode-from-pathname(container-pathname(container));
	enter-mode(buffer, major-mode);
	sectionize-buffer(buffer);
    end;
    object & pair(locator, pair(buffer, object))
  end
end method search-domain-find;

//---*** cpage: 1997.07.27 This is not yet used. This will be "batch" searching.
/*
define method search-domain-find-all
    (domain          :: <text-file-search-domain>,
     locator         :: <file-locator>,
     register-object :: <function>,
     search-string   :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ()
  // TBD: I think this should be handled by a method on <search-domain>, though
  //      it may be faster to do it in a custom method.
  next-method();
end method search-domain-find-all;
*/

define method search-domain-replace-selection
    (domain         :: <text-file-search-domain>,
     locator        :: <file-locator>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  ignore(domain);
  let buffer = find-buffer-for-file(locator);
  let frame  = find-frame-for-buffer(buffer);
  let window = frame-window(frame);
  // Make sure the buffer is the current frame buffer
  when (buffer ~= frame-buffer(frame))
    select-buffer(window, buffer);
    queue-redisplay(window, $display-all);
  end;
  let object = apply(replace-in-frame, frame, search-string, replace-string, keys);
  // Update the display
  call-in-frame(frame, redisplay-window, window);
  object & pair(locator, pair(buffer, object))
end method search-domain-replace-selection;

define method search-domain-replace-all
    (domain         :: <text-file-search-domain>,
     locator        :: <file-locator>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (replace-count :: <integer>)
  // Don't do anything unless we're replacing from the selection
  // or there is at least one match
  if (from-selection?
	| apply(search-domain-find, domain, locator, search-string, keys))
    let buffer = find-buffer-for-file(locator);
    let frame  = find-frame-for-buffer(buffer);
    let window = frame-window(frame);
    // Make sure the buffer is the current frame buffer
    when (buffer ~= frame-buffer(frame))
      select-buffer(window, buffer);
      queue-redisplay(window, $display-all);
    end;
    let replace-count :: <integer>
      = apply(replace-all-in-frame, frame, search-string, replace-string, keys);
    // Update the display
    call-in-frame(frame, redisplay-window, window);
    replace-count
  else
    0
  end
end method search-domain-replace-all;

//---*** cpage: 1998.07.29 This is just about exactly what <frame-search-domain>
//              does. Perhaps we should forward some of this to $frame-search-domain.
define method search-domain-reveal-search-object
    (domain :: <text-file-search-domain>, object :: <object>) => (revealed? :: <boolean>)
  ignore(domain);
  let locator :: <file-locator> = object.head;
  let buffer = find-buffer-for-file(locator);
  let frame  = find-frame-for-buffer(buffer);
  when (frame & frame.frame-state ~= #"destroyed")
    call-in-frame(frame, method () => ()
			   deiconify-frame(frame);
			   raise-frame(frame);
			 end);
    frame-reveal-search-object(frame, object.tail)
  end
end method search-domain-reveal-search-object;

//---*** cpage: 1998.07.27 These two functions are not yet called, anyway, as
//              they are for displaying found items for "batch" searching.
/*
define method search-domain-search-object-label
    (domain :: <text-file-search-domain>, object :: <object>) => (label :: false-or(<string>))
  // TBD: Return the short file name, line number or range, and perhaps an excerpt
  //      of the text. Actually, we should probably break this into two functions,
  //      one for a "location" and the other for a "description".
  #f
end method search-domain-search-object-label;

define method search-domain-search-object-icon
    (domain :: <text-file-search-domain>, object :: <object>) => (icon :: false-or(<image>))
  // TBD: Return the editor title bar icon, probably.
  #f
end method search-domain-search-object-icon;
*/
