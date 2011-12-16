Module:    environment-profiler
Synopsis:  The profiling tool provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Profiler functions

define frame <profiler-functions> (<simple-frame>)
  pane functions-page (frame)
    make(<tab-control-page>,
	 label: "Functions",
	 id:    #"functions",
	 child: frame.functions-displayer);
  pane functions-displayer (frame)
    make(<table-control-displayer>,
	 element-label: "form",
	 information-available?-function: curry(profiler-has-results?, frame),
	 transaction-function: curry(perform-profiler-transaction, frame),
	 children-generator: curry(frame-profile-info, frame),
	 headings: #["Function", "Count", "Amount", "%"],
	 alignments: #[#"left", #"right", #"right", #"right"],
	 widths: #[200, 60, 60, 40],
	 generators: vector(wrapper-object,
			    wrapper-count,
			    wrapper-amount,
			    wrapper-percentage),
	 sort-orders: #[#"function", #"count", #"amount", #"percentage"],
	 sort-order: #"amount",
	 sort-function: curry(frame-sort-profile-info, frame),
	 label-key: curry(frame-default-object-name, frame),
	 items-changed-callback: note-profiler-functions-updated,
	 value-changed-callback: note-profiler-selected-functions-changed,
         activate-callback: curry(profiler-activate-callback, frame));
end frame <profiler-functions>;

define method refresh-profiler-page
    (frame :: <profiler-functions>, page == #"functions",
     #key clean? = #f, new-thread? = #t)
 => ()
  let project = frame.ensure-frame-project;
  let displayer = frame.functions-displayer;
  let thread = frame.frame-current-thread;
  debug-out(#"environment-profiler", "Refreshing functions page (clean?: %=)", clean?);
  unless (~thread & ~displayer.displayer-object)
    displayer-object(displayer, clean?: clean?, new-thread?: new-thread?)
      := thread
  end
end method refresh-profiler-page;

define method invalidate-profiler-page
    (frame :: <profiler-functions>, page == #"functions") => ()
  // frame.functions-displayer.displayer-valid? := #f
  invalidate-frame-property-page(frame, frame.functions-displayer, #f, page)
end method invalidate-profiler-page;

define class <profile-info-wrapper> (<object-wrapper>)
  constant slot wrapper-count :: <integer>,
    required-init-keyword: count:;
  constant slot wrapper-amount :: <integer>,
    required-init-keyword: amount:;
  slot wrapper-percentage :: <percentage>,
    init-keyword: percentage:;
end class <profile-info-wrapper>;

define method total-amount
    (wrappers :: <sequence>) => (total :: <integer>)
  let total :: <integer> = 0;
  for (wrapper :: <profile-info-wrapper> in wrappers)
    let amount = wrapper.wrapper-amount;
    total := total + wrapper.wrapper-amount
  end;
  total
end method total-amount;

define method frame-profile-info
    (frame :: <environment-frame>, thread :: <thread-object>)
 => (wrappers :: <sequence>)
  let project = frame.ensure-frame-project;
  let profile = frame.profiler-application-profile;
  let show-foreign-functions? = frame.profiler-show-foreign-functions?;
  let profile-info
    = process-profile-summary
        (project, profile,
	 show-foreign-functions?: show-foreign-functions?);
  let wrappers :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let amount-getter
    = if (profile.allocation-profiling?)
	info-allocation
      else
	info-cpu-time
      end;
  let total :: <integer> = 0;
  for (thread-info in profile-info.info-threads)
    if (thread-info.info-thread == thread)
      for (function-info :: <profile-function-info> in thread-info.info-objects)
	let amount = function-info.amount-getter;
	total := total + amount;
	add!(wrappers,
	     make(<profile-info-wrapper>,
		  object:     function-info.info-function,
		  count:      function-info.info-count,
		  amount:     amount))
      end
    end
  end;
  for (wrapper :: <profile-info-wrapper> in wrappers)
    let amount = wrapper.wrapper-amount;
    wrapper.wrapper-percentage := percentage(amount, total)
  end;
  debug-out(#"environment-profiler", "Found %d frames", wrappers.size);
  wrappers
end method frame-profile-info;

define method frame-sort-profile-info
    (frame :: <environment-frame>, wrappers :: <sequence>,
     order :: <symbol>)
 => (wrappers :: <sequence>)
  select (order)
    #"function" =>
      frame-sort-items(frame, wrappers, key: wrapper-object);
    otherwise =>
      let key
	= select (order)
	    #"count"      => wrapper-count;
	    #"amount"     => wrapper-amount;
	    #"percentage" => wrapper-percentage;
	  end;
      keyed-sort(wrappers, key: key, test: \>);
  end
end method frame-sort-profile-info;

define method note-profiler-functions-updated
    (displayer :: <table-control-displayer>)
 => ()
  update-profiler-functions-status(displayer)
end method note-profiler-functions-updated;

define method note-profiler-selected-functions-changed
    (displayer :: <table-control-displayer>, value :: <sequence>)
 => ()
  ignore(value);
  update-profiler-functions-status(displayer)
end method note-profiler-selected-functions-changed;

define method update-profiler-functions-status
    (displayer :: <table-control-displayer>) => ()
  let frame = sheet-frame(displayer);
  let profile = frame.profiler-application-profile;
  let amount-to-string
    = if (profile & profile.allocation-profiling?)
	allocation-to-string
      else
	time-to-string
      end;
  update-profiler-status
    ("function", displayer,
     available?:       frame.profiler-has-results?,
     amount-to-string: amount-to-string)
end method update-profiler-functions-status;

define method update-profiler-status
    (name :: <string>, displayer :: <table-control-displayer>,
     #key available? :: <boolean> = #t,
          amount-to-string :: <function>)
 => ()
  let frame = sheet-frame(displayer);
  let gadget = displayer.displayer-collection-gadget;
  let (message :: <string>, selection-message :: <string>)
    = if (available?)
	let items = gadget-items(gadget);
	let value = gadget-value(gadget);
	let total = total-amount(items);
	let selection? = ~empty?(value);
	let selected-total
	  = if (selection?) total-amount(value) else total end;
	values(if (selection?)
		 let count = value.size;
		 format-to-string("%d %s selected",
				  count,
				  string-pluralize(name, count: count))
	       else
		 let count = items.size;
		 format-to-string("%d %s",
				  count,
				  string-pluralize(name, count: count))
	       end,
	       format-to-string("%s (%s)",
				amount-to-string(selected-total),
				percentage-label
				  (percentage(selected-total, total),
				   decimal-points: 0)))
      else
	let project = frame.ensure-frame-project;
	values(project-not-built-message(project), "")
      end;
  frame-status-message(frame)    := message;
  frame-selection-message(frame) := selection-message
end method update-profiler-status;

define constant $second = 1000;
define constant $minute = $second * 60;
define constant $hour   = $minute * 60;

define method time-to-string
    (integer :: <integer>) => (string :: <string>)
  local method one-decimal-point-string
	    (type :: <string>, value :: <single-float>)
	 => (string :: <string>)
	  let (whole, part) = floor/(round(value * 10), 10);
	  format-to-string("%d.%d %s",
			   whole, part,
			   type)
	end method one-decimal-point-string;
  let value = as(<single-float>, integer);
  select (integer by \>)
    10 * $hour   => one-decimal-point-string("hours",   value / $hour);
    10 * $minute => one-decimal-point-string("minutes", value / $minute);
    10 * $second => one-decimal-point-string("seconds", value / $second);
    otherwise    => format-to-string("%d ms", integer);
  end
end method time-to-string;

/*---*** Add filtering next...
define method filter-profile-info
    (frame :: <environment-frame>, libraries :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (names :: <sequence>)
  let project = frame.ensure-frame-project;
  let no-filter? = empty?(substring-filter);
  let library = project-library(project);
  let used-libraries
    = if (library)
	source-form-used-definitions(project, library)
      else
	#[]
      end;
  local method object-matches-type-filter? 
	    (library :: <library-object>) => (matches? :: <boolean>)
	  select (type-filter)
	    #"libraries"      => #t;
	    #"used-libraries" => 
	      member?(library, used-libraries);
	  end
	end method object-matches-type-filter?;
  local method object-matches-substring-filter?
	    (library :: <library-object>) => (matches? :: <boolean>)
	  no-filter?
	    | begin
		let name = frame-default-object-name(frame, library);
		subsequence-position(name, substring-filter) ~= #f
	      end
	end method object-matches-substring-filter?;
  local method show-object?
	    (library :: <library-object>) => (show? :: <boolean>)
	  object-matches-type-filter?(library)
	    & object-matches-substring-filter?(library)
	end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in libraries)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-profile-info;
*/
