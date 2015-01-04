Module:       locators-internals
Synopsis:     Abstract modeling of locations
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constants

define constant $default-path-separator :: <character> = '/';


/// Basic parsing

define method prefix-equal?
    (string :: <string>, prefix :: <string>) => (equal? :: <boolean>)
  let prefix-size = prefix.size;
  if (string.size >= prefix-size)
    copy-sequence(string, end: prefix-size) = prefix
  end
end method prefix-equal?;


/// Delimiter handling

define inline function delimiter-to-string
    (character :: <character>) => (string :: <byte-string>)
  make(<byte-string>, size: 1, fill: character)
end function delimiter-to-string;

define method find-delimiter
    (string :: <string>, delimiter :: <character>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = string.size)
 => (position :: false-or(<integer>))
  block (return)
    for (index :: <integer> from start below stop)
      when (string[index] == delimiter)
        return(index)
      end
    end
  end
end method find-delimiter;

define method find-delimiters
    (string :: <string>, delimiters :: <sequence>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = string.size)
 => (position :: false-or(<integer>))
  block (return)
    for (index :: <integer> from start below stop)
      when (member?(string[index], delimiters))
        return(index)
      end
    end
  end
end method find-delimiters;

define method find-delimiter-from-end
    (string :: <string>, delimiter :: <character>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = string.size)
 => (position :: false-or(<integer>))
  block (return)
    for (index :: <integer> from stop - 1 to start by -1)
      when (string[index] == delimiter)
        return(index)
      end
    end
  end
end method find-delimiter-from-end;

define method find-delimiters-from-end
    (string :: <string>, delimiters :: <sequence>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = string.size)
 => (position :: false-or(<integer>))
  block (return)
    for (index :: <integer> from stop - 1 to start by -1)
      when (member?(string[index], delimiters))
        return(index)
      end
    end
  end
end method find-delimiters-from-end;


/// Path routines

define method canonicalize-path
    (path :: <sequence>)
 => (canonical-path :: <simple-object-vector>)
  let new-path :: <simple-object-vector>
    = make(<simple-object-vector>, size: path.size);
  for (item in path,
       index from 0)
    new-path[index]
      := select (item by \=)
           "."       => #"self";
           ".."      => #"parent";
           otherwise => item;
         end
  end;
  new-path
end method canonicalize-path;

define method parse-path
    (string :: <string>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = string.size,
          test :: <function> = curry(\==, $default-path-separator),
          separators :: <sequence> = #[])
 => (path :: <simple-object-vector>, relative? :: <boolean>)
  let path :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let old-position :: <integer> = start;
  let position :: <integer> = old-position;
  let relative? :: <boolean> = #t;
  while (position < stop)
    let character = string[position];
    if (test(character))
      if (position == start) relative? := #f end;
      if (old-position < position)
        add!(path, copy-sequence(string, start: old-position, end: position))
      end;
      old-position := position + 1;
    end;
    position := position + 1
  end;
  if (old-position < stop)
    add!(path, copy-sequence(string, start: old-position, end: stop))
  end;
  values(as(<simple-object-vector>, path), relative?)
end method parse-path;

//---*** It is a pity that we need this for efficiency...
define sealed copy-down-method parse-path
    (string :: <byte-string>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = string.size,
          test :: <function> = curry(\==, $default-path-separator),
          separators :: <sequence> = #[])
 => (path :: <simple-object-vector>, relative? :: <boolean>);

define method path-to-string
    (path :: <sequence>,
     #key relative? :: <boolean> = #f,
          separator :: <character> = $default-path-separator,
          class :: subclass(<string>) = <byte-string>)
 => (string :: <string>)
  local method item-name
            (item :: type-union(<string>, <symbol>)) => (name :: <string>)
          select (item)
            #"self"   => ".";
            #"parent" => "..";
            otherwise => item;
          end
        end method item-name;
  let string-size :: <integer> = size(path) + if (relative?) 0 else 1 end;
  for (item in path)
    string-size := string-size + item.item-name.size
  end;
  let string = make(class, size: string-size);
  let pos :: <integer> = 0;
  unless (relative?)
    string[pos] := separator;
    pos := pos + 1;
  end;
  for (item in path)
    for (character :: <character> in item.item-name)
      string[pos] := character;
      pos := pos + 1;
    end;
    string[pos] := separator;
    pos := pos + 1;
  end;
  string
end method path-to-string;

define method relative-path
    (path :: <simple-object-vector>, from-path :: <simple-object-vector>,
     #key test :: <function> = \=)
 => (relative-path :: <simple-object-vector>)
  let path-size :: <integer> = path.size;
  let from-path-size :: <integer> = from-path.size;
  iterate loop (i = 0)
    case
      i == path-size =>
        make(<simple-object-vector>,
             size: from-path-size - i,
             fill: #"parent");
      i == from-path-size =>
        copy-sequence(path, start: i);
      test(path[i], from-path[i]) =>
        loop(i + 1);
      otherwise =>
        concatenate(make(<simple-object-vector>,
                         size: from-path-size - i,
                         fill: #"parent"),
                    copy-sequence(path, start: i));
    end
  end
end method relative-path;

define method simplify-path
    (path :: <simple-object-vector>,
     #key resolve-parent? :: <boolean> = #t,
          relative? :: <boolean>)
 => (simplified-path :: <simple-object-vector>)
  let new-path :: <list> = #();
  for (item in path)
    select (item)
      #"self"   =>
        #f;
      #"parent" =>
        if (resolve-parent?
              & ~new-path.empty?
              & new-path.head ~== #"parent")
          new-path := new-path.tail
        else
          new-path := pair(item, new-path)
        end;
      otherwise =>
        new-path := pair(item, new-path);
    end
  end;
  if (empty?(new-path) & relative?)
    new-path := list(#"self")
  end;
  reverse!(as(<simple-object-vector>, new-path))
end method simplify-path;


/// Case insensitive comparisons
//---*** andrewa: needed for comparison of Microsoft locators.
//---*** This really should be defined somewhere.
//---*** Also, we should worry about internationalization issues.

define method case-insensitive=
    (object1 :: <object>, object2 :: <object>)
 => (equal? :: <boolean>)
  object1 = object2
end method case-insensitive=;

define method case-insensitive=
    (char1 :: <character>, char2 :: <character>)
 => (equal? :: <boolean>)
  as-lowercase(char1) == as-lowercase(char2)
end method case-insensitive=;

define method case-insensitive=
    (string1 :: <string>, string2 :: <string>)
 => (equal? :: <boolean>)
  if (string1.size == string2.size)
    block (return)
      for (char1 :: <character> in string1,
           char2 :: <character> in string2)
        unless (as-lowercase(char1) == as-lowercase(char2))
          return(#f)
        end
      end;
      #t
    end
  end
end method case-insensitive=;
