Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// XML syntax tables

define constant $xml-word-syntax :: <syntax-table> = make(<syntax-table>);
define constant $xml-atom-syntax :: <syntax-table> = make(<syntax-table>);
define constant $xml-list-syntax :: <syntax-table> = make(<syntax-table>);

define function initialize-xml-syntax-tables () => ()
  copy-syntax-table-into!($default-word-syntax, $xml-word-syntax);
  // In atom syntax table, '<' and '>' are separator characters
  copy-syntax-table-into!($default-atom-syntax, $xml-atom-syntax);
  let table = $xml-atom-syntax.%syntax-table;
  table[as(<integer>, '<')] := $atom-delimiter;
  table[as(<integer>, '>')] := $atom-delimiter;
  // In list syntax table, '<' and '>' are open and close characters
  copy-syntax-table-into!($default-list-syntax, $xml-list-syntax);
  let table = $xml-list-syntax.%syntax-table;
  table[as(<integer>, '<')] := $list-open;
  table[as(<integer>, '>')] := $list-close;
  #f
end function initialize-xml-syntax-tables;

initialize-xml-syntax-tables();


/// XML mode

define open class <xml-mode> (<text-mode>)
  keyword word-syntax: = $xml-word-syntax;
  keyword atom-syntax: = $xml-atom-syntax;
  keyword list-syntax: = $xml-list-syntax;
end class <xml-mode>;

define method initialize-major-mode
    (mode :: <xml-mode>, #key command-set = mode-command-set(mode)) => ()
  next-method();
  // Install some XML mode commands
  let nothing = 0;
  let shift   = $shift-key;
  let control = $control-key;
  let meta    = $meta-key;
  let control+meta  = logior($control-key, $meta-key);
  let control+shift = logior($control-key, $shift-key);
  let meta+shift    = logior($meta-key, $shift-key);
  let command-set   = copy-command-set(command-set);
  mode-command-set(mode) := command-set;
  //--- It would be nice to share the following with <dylan-mode>,
  //--- perhaps by putting these bindings into <language-mode>
  select (command-set-name(command-set))
    #"emacs" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
		    vector('f', control+meta, forward-expression),
		    vector('b', control+meta, backward-expression),
		    vector('u', control+meta, up-expression),
		    vector('d', control+meta, down-expression),
		    vector('a', control+meta, start-of-definition),
		    vector('e', control+meta, end-of-definition),
		    vector(#"right", meta,    forward-expression),
		    vector(#"left",  meta,    backward-expression),
		    vector(#"up",    meta,    up-expression),
		    vector(#"down",  meta,    down-expression), 
		    vector(#"prior", meta,    start-of-definition), 
		    vector(#"next",  meta,    end-of-definition),
		    vector(#"delete",    control+meta, delete-expression),
		    vector(#"backspace", control+meta, rubout-expression),
		    vector('k', control+meta, delete-expression),
		    vector('t', control+meta, transpose-expressions),
		    vector(';',    control,    insert-comment),
		    vector(#"tab", control,    insert-tab),
		    vector(#"tab", nothing,    indent-line),
		    vector('\\', control+meta, indent-region),
		    vector('q',  control+meta, indent-expression),
		    vector(')', meta,         xml-insert-close-element),
		    vector('i', control+meta, complete-name));
      let command-table = control-X-command-table(command-set);
      add-commands!(command-table,
		    vector(';', control,      comment-region));
      let command-table = escape-command-table(command-set);
      add-commands!(command-table,
		    vector('f', control,      forward-expression),
		    vector('b', control,      backward-expression),
		    vector('u', control,      up-expression),
		    vector('d', control,      down-expression),
		    vector('a', control,      start-of-definition),
		    vector('e', control,      end-of-definition),
		    vector(#"delete",    control, delete-expression),
		    vector(#"backspace", control, rubout-expression),
		    vector('k', control,      delete-expression),
		    vector('t', control,      transpose-expressions),
		    vector('\\', control,     indent-region),
		    vector('q',  control,     indent-expression),
		    vector(')', nothing,      xml-insert-close-element),
		    vector('i', control,      complete-name));
    #"windows" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
		    vector(#"right", meta,    forward-expression),
		    vector(#"left",  meta,    backward-expression),
		    vector(#"up",    meta,    up-expression),
		    vector(#"down",  meta,    down-expression), 
		    vector(#"prior", meta,    start-of-definition), 
		    vector(#"next",  meta,    end-of-definition),
		    vector(#"tab",   nothing, indent-region),
		    vector('i',      control, insert-tab));
    otherwise =>
      #[];
  end;
  command-set
end method initialize-major-mode;

begin
  gethash(*keyword->major-mode*,   #"xml") := <xml-mode>;
  gethash(*file-type->major-mode*, #"xml") := <xml-mode>;
  gethash(*file-type->major-mode*, #"xsl") := <xml-mode>;
end;

define method mode-name
    (mode :: <xml-mode>) => (name :: <byte-string>)
  "XML"
end method mode-name;

define method source-file-type
    (mode :: <xml-mode>) => (file-type)
  #"xml"
end method source-file-type;


/// XML sections

define sealed class <xml-section> (<basic-section>)
end class <xml-section>;

define sealed domain make (singleton(<xml-section>));
define sealed domain initialize (<xml-section>);


/// XML nodes

define sealed class <xml-node> (<section-node>)
end class <xml-node>;

define sealed domain make (singleton(<xml-node>));
define sealed domain initialize (<xml-node>);

define sealed class <xml-header-node> (<header-node>)
end class <xml-header-node>;

define sealed domain make (singleton(<xml-header-node>));
define sealed domain initialize (<xml-header-node>);


/// XML "expressions"

define method do-atom-under-bp
    (mode :: <xml-mode>, bp :: <basic-bp>)
 => (sbp :: <basic-bp>, ebp :: <basic-bp>)
  let node = bp-node(bp) | bp-buffer(bp);
  let sbp  = if (atom-syntax(bp-character-before(bp)) == $atom-delimiter)
	       forward-over(bp, #[' ', '\t', '\f'], interval: node)
	     else
	       move-over-atoms(bp, -1, interval: node)
	     end;
  let ebp  = move-over-atoms(sbp, 1, interval: node);
  // Some characters are magic at the beginning of an atom
  // Skip them, unless the user is right on top of one
  when (bp ~= sbp
	& member?(bp-character(sbp), #['+', '-', '~', '?', '/']))
    increment-bp!(sbp)
  end;
  values(sbp, ebp)
end method do-atom-under-bp;

define sealed method do-move-over-expressions
    (mode :: <xml-mode>, bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  move-over-xml-tags(bp, n, fixup?: fixup?, interval: interval)
end method do-move-over-expressions;

define sealed method do-move-up-or-down-expressions
    (mode :: <xml-mode>, bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  move-up-or-down-xml-tags(bp, n, fixup?: fixup?, interval: interval)
end method do-move-up-or-down-expressions;

define command xml-insert-close-element (frame)
    "Insert a close element to match the previous open element."
  //---*** Find the previous unclosed begin tag, then close it
end command xml-insert-close-element;


/// XML motion primitives

define sealed method move-over-xml-tags
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  block (return)
    let bp :: <basic-bp> = copy-bp(bp);
    unless (n = 0)
      let reverse? = (n < 0);
      let limit :: <basic-bp>
	= if (reverse?) interval-start-bp(interval) else interval-end-bp(interval) end;
      local method true (ch)
	      ignore(ch);
	      values(#t, #f)
	    end method,
	    method non-whitespace? (ch :: <byte-character>)
	      if (any-whitespace-char?(ch)) values(#f, #f)
	      else values(#t, #t) end
	    end method;
      for (i :: <integer> from 0 below abs(n))
	when (bp = limit)
          return(fixup? & limit)
        end;
	move-forward-or-backward!(bp, non-whitespace?, reverse?, interval: interval);
	unless (bp = limit)
	  let start-char
	    = if (reverse?) bp-character-before(bp) else bp-character(bp) end;
	  let syntax = list-syntax(start-char);
	  let nbp
	    = case
		syntax == $list-double-quote | syntax == $list-single-quote =>
		  move-over-matching-thing!(bp, start-char, reverse?, interval: interval);
		syntax == (if (reverse?) $list-close else $list-open end) =>
		  move-over-balanced-thing!(bp, start-char, reverse?, interval: interval);
		otherwise =>
		  move-over-atom!(bp, reverse?, interval: interval);
	      end;
	  nbp | return(fixup? & limit)
	end
      end
    end;
    bp      
  end
end method move-over-xml-tags;

define sealed method move-up-or-down-xml-tags
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-node(bp) | bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  //---*** Do this for real by moving in to or out of begin/end tags
  move-up-or-down-lists(bp, n, fixup?: #t, interval: interval)
end method move-up-or-down-xml-tags;

define sealed method xml-tag-name
    (bp :: <basic-bp>)
 => (tag-name :: false-or(<byte-string>),
     empty-element? :: <boolean>, end-tag? :: <boolean>)
  select (bp-character(bp))
    '<' =>
      let bp1 = increment-bp!(copy-bp(bp));
      let bp2 = move-over-atoms(bp1, 1);
      let empty? :: <boolean> = (bp-character(bp1) == '/');
      when (empty?)
        increment-bp!(bp1)
      end;
      when (bp-character(bp2) == '>')
        decrement-bp!(bp2)
      end;
      let end? :: <boolean> = (bp-character(bp2) == '/');
      when (end?)
        decrement-bp!(bp2)
      end;
      values(as(<byte-string>, make-interval(bp1, bp2, in-order?: #t)), empty?, end?);
    '>' =>
      let bp = search(bp, "<", reverse?: #t);
      bp & xml-tag-name(bp);
    otherwise =>
      values(#f, #f, #f);
  end
end method xml-tag-name;


/// XML comments

define method do-insert-comment
    (mode :: <xml-mode>, line :: <basic-line>, #key column)
 => (bp :: false-or(<basic-bp>))
  if (column)
    //--- This should attempt to insert the comment at the given column
    let bp1 = line-end(line);
    let bp2 = insert-moving!(bp1, "\t<!-- -->");
    move-over-characters(bp2, -3)
  else
    let bp1 = line-start(line);
    let bp2 = insert-moving!(bp1, "\t<!-- -->");
    move-over-characters(bp2, -3)
  end
end method do-insert-comment;

define method do-comment-region
    (mode :: <xml-mode>, region :: <basic-interval>, #key comment?) => ()
  let first-line = bp-line(interval-start-bp(region));
  let last-line  = bp-line(interval-end-bp(region));
  local method comment (line :: <basic-line>, si, ei, last?)
	  ignore(last?);
          when (line == first-line)
	    insert-into-line(line, si, "<!-- ")
	  end;
          when (line == last-line)
	    insert-into-line(line, ei, " -->")
	  end;
	end method,
	method uncomment (line :: <basic-line>, si, ei, last?)
	  ignore(last?);
	  //---*** How to do this?
	end method;
  do-lines(if (comment?) comment else uncomment end, region)
end method do-comment-region;


/// XML indentation

define method do-indent-line
    (mode :: <xml-mode>, line :: <basic-line>)
 => (bp :: false-or(<basic-bp>), dx :: <integer>, nchars :: <integer>)
  indent-xml-line(mode, line)
end method do-indent-line;

//--- Can we merge this with 'do-indent-line' and then move the method
//--- up into <language-mode>, sharing the new method with <dylan-mode>?
//--- If so, 'xxx-line-indentation' should be part of the protocol...
define sealed method indent-xml-line
    (mode :: <major-mode>, line :: <basic-line>, #key definition-type)
 => (bp :: false-or(<basic-bp>), dx :: <integer>, nchars :: <integer>)
  let bp1 = line-start(line);
  let bp2 = forward-over(bp1, #[' ', '\t']);
  local method insert-indentation!
	    (line :: <basic-line>, indentation :: <integer>)
	 => (bp :: <basic-bp>, nchars :: <integer>)
	  if (text-line?(line))
	    let interval = make-interval(bp1, bp2, in-order?: #t);
	    let old-n    = count-characters(interval);
	    let new-n    = max(indentation, 0);
	    delete!(interval);
	    let spaces = make(<byte-string>, size: new-n, fill: ' ');
	    values(insert-moving!(bp1, spaces), new-n - old-n)
	  else
	    values(bp1, 0)
	  end
	end method;
  let (old-indentation, new-indentation)
    = xml-line-indentation(mode, line, definition-type: definition-type);
  let dx = new-indentation - old-indentation;
  if (dx = 0)
    values(bp2, 0, 0)
  else
    let (bp, nchars) = insert-indentation!(line, new-indentation);
    values(bp, nchars, dx)
  end
end method indent-xml-line;

//--- Hey, why is this on <major-mode>?
define sealed method xml-line-indentation
    (mode :: <major-mode>, line :: <basic-line>, #key definition-type)
 => (old-indentation :: <integer>, new-indentation :: <integer>)
  let window :: <basic-window> = frame-window(*editor-frame*);
  let margin = line-margin(line, mode, window);
  let space-width = string-size(window, " ");
  local method line-indentation
	    (sbp :: <basic-bp>) => (indentation :: <integer>)
	  if (text-line?(bp-line(sbp)))
	    let indentation
	      = index->position(bp-line(sbp), mode, window, bp-index(sbp)) - margin;
	    floor/(indentation, space-width)
	  else
	    0
	  end
	end method;
  let section  = line-section(line);
  let this-bp  = line-start(line);
  let this-sbp = forward-over(this-bp, #[' ', '\t']);
  let this-indentation :: <integer> = line-indentation(this-sbp);
  let (this-name, this-empty?, this-end?) = xml-tag-name(this-sbp);
  let prev-line = line-previous(line);
  let prev-bp   = prev-line & line-start(prev-line);
  let prev-sbp  = prev-bp   & forward-over(prev-bp, #[' ', '\t']);
  let prev-indentation :: <integer> = if (prev-sbp) line-indentation(prev-sbp) else 0 end;
  let (prev-name, prev-empty?, prev-end?) = xml-tag-name(prev-sbp);
  //--- Crude, but not entirely ineffective
  block (return)
    if (this-name)
      if (this-end?)
        return(this-indentation, prev-indentation - 2)
      else
        if (prev-name & prev-empty?)
          return(this-indentation, prev-indentation)
        else
          return(this-indentation, prev-indentation + 2)
        end
      end
    else
      if (~prev-name | (prev-name & prev-empty?))
        return(this-indentation, prev-indentation)
      else
        return(this-indentation, prev-indentation + 2)
      end
    end
  end
end method xml-line-indentation;
