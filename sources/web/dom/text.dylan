Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Character data

//
// interface CharacterData : Node {
//            attribute  DOMString            data;
//                                  // raises(DOMException) on setting
//                                  // raises(DOMException) on retrieval
//   readonly attribute  unsigned long        length;
//   DOMString                 substringData(in unsigned long offset, 
//                                           in unsigned long count)
//                                           raises(DOMException);
//   void                      appendData(in DOMString arg)
//                                        raises(DOMException);
//   void                      insertData(in unsigned long offset, 
//                                        in DOMString arg)
//                                        raises(DOMException);
//   void                      deleteData(in unsigned long offset, 
//                                        in unsigned long count)
//                                        raises(DOMException);
//   void                      replaceData(in unsigned long offset, 
//                                         in unsigned long count, 
//                                         in DOMString arg)
//                                         raises(DOMException);
// };
//

//--- 'insert-data' and friends would benefit from a <stretchy-DOM-string>...
define open abstract class <character-data>
    (<leaf-node-mixin>, <node>)
end class <character-data>;

define sealed method do-clone-node
    (node :: <character-data>, new-node :: <character-data>) => ()
  // We need a fresh copy of the string data in the new node
  data(new-node) := copy-string(data(node))
end method do-clone-node;

define sealed inline method data
    (cdata :: <character-data>) => (data :: <DOM-string>)
  node-value(cdata)
end method data;

define sealed inline method data-setter
    (data :: <DOM-string>, cdata :: <character-data>) => (data :: <DOM-string>)
  node-value(cdata) := data
end method data-setter;

define sealed inline method length
    (cdata :: <character-data>) => (length :: <integer>)
  size(node-value(cdata))
end method length;

define sealed method insert-data
    (cdata :: <character-data>, offset :: <integer>, string :: <DOM-string>) => ()
  check-read-only(cdata);
  let data    :: <DOM-string> = data(cdata);
  let length  :: <integer>    = length(cdata);
  let slength :: <integer>    = size(string);
  when (offset < 0 | offset > length)
    error(make(<index-size-error>,
	       format-string: "Offset %d is wrong for character data node %=",
	       format-arguments: vector(offset, cdata)))
  end;
  let new-data :: <DOM-string> = make(<DOM-string>, size: length + slength);
  copy-string-into!(data, 0, offset, new-data, 0);
  copy-string-into!(string, 0, slength, new-data, offset);
  copy-string-into!(data, offset, length, new-data, offset + slength);
  data(cdata) := new-data
end method insert-data;

define sealed method append-data
    (cdata :: <character-data>, string :: <DOM-string>) => ()
  check-read-only(cdata);
  let data     :: <DOM-string> = data(cdata);
  let length   :: <integer>    = length(cdata);
  let slength  :: <integer>    = size(string);
  let new-data :: <DOM-string> = make(<DOM-string>, size: length + slength);
  copy-string-into!(data, 0, length, new-data, 0);
  copy-string-into!(string, 0, slength, new-data, length);
  data(cdata) := new-data
end method append-data;

define sealed method replace-data
    (cdata :: <character-data>, offset :: <integer>, count :: <integer>,
     string :: <DOM-string>) => ()
  check-read-only(cdata);
  check-read-only(cdata);
  let data    :: <DOM-string> = data(cdata);
  let length  :: <integer>    = length(cdata);
  let slength :: <integer>    = size(string);
  when (offset < 0 | offset > length | count < 0)
    error(make(<index-size-error>,
	       format-string: "Offset %d or count %d is wrong for character data node %=",
	       format-arguments: vector(offset, count, cdata)))
  end;
  let new-data :: <DOM-string> = make(<DOM-string>, size: length - count + slength);
  copy-string-into!(data, 0, offset, new-data, 0);
  copy-string-into!(string, 0, slength, new-data, offset);
  copy-string-into!(data, offset + count, length, new-data, offset + slength);
  data(cdata) := new-data
end method replace-data;

define sealed method delete-data
    (cdata :: <character-data>, offset :: <integer>, count :: <integer>) => ()
  check-read-only(cdata);
  let data   :: <DOM-string> = data(cdata);
  let length :: <integer>    = length(cdata);
  when (offset < 0 | offset > length | count < 0)
    error(make(<index-size-error>,
	       format-string: "Offset %d or count %d is wrong for character data node %=",
	       format-arguments: vector(offset, count, cdata)))
  end;
  let new-data :: <DOM-string> = make(<DOM-string>, size: length - count);
  copy-string-into!(data, 0, offset, new-data, 0);
  copy-string-into!(data, offset + count, length, new-data, offset);
  data(cdata) := new-data
end method delete-data;

define sealed method substring-data
    (cdata :: <character-data>, offset :: <integer>, count :: <integer>)
 => (string :: <DOM-string>)
  let data   :: <DOM-string> = data(cdata);
  let length :: <integer>    = length(cdata);
  when (offset < 0 | offset > length | count < 0)
    error(make(<index-size-error>,
	       format-string: "Offset %d or count %d is wrong for character data node %=",
	       format-arguments: vector(offset, count, cdata)))
  end;
  let _start :: <integer> = offset;
  let _end   :: <integer> = min(_start + count, length);
  if ((_end - _start) = 0)
    ""
  else
    let string :: <DOM-string> = make(<DOM-string>, size: _end - _start);
    copy-string-into!(data, _start, _end, string, 0);
    string
  end
end method substring-data;


define function copy-string
    (string :: <DOM-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (new-string :: <DOM-string>)
  if ((_end - _start) = 0)
    ""
  else
    let new-string :: <DOM-string> = make(<DOM-string>, size: _end - _start);
    copy-string-into!(string, _start, _end, new-string, 0);
    new-string
  end
end function copy-string;			

define sealed method copy-string-into!
    (from :: <DOM-string>, start1 :: <integer>, end1 :: <integer>,
     to   :: <DOM-string>, start2 :: <integer>) => ()
  without-bounds-checks
    for (i :: <integer> from start1 below end1,
	 j :: <integer> from start2)
      to[j] := from[i]
    end
  end
end method copy-string-into!;

define sealed method copy-string-into!
    (from :: <byte-string>, start1 :: <integer>, end1 :: <integer>,
     to   :: <byte-string>, start2 :: <integer>) => ()
  without-bounds-checks
    for (i :: <integer> from start1 below end1,
	 j :: <integer> from start2)
      to[j] := from[i]
    end
  end
end method copy-string-into!;


/// Text

//
// interface Text : CharacterData {
//   readonly attribute boolean          isWhitespaceInElementContent;
//   readonly attribute DOMString        wholeText;
//   Text                      splitText(in unsigned long offset)
//                                       raises(DOMException);
//   Text                      replaceWholeText(in DOMString content)
//                                              raises(DOMException);
// };
//

define sealed class <text> (<character-data>)
  keyword type: = $text-node;
end class <text>;

define sealed method split-text
    (text :: <text>, offset :: <integer>)
 => (new-text :: <text>)
  do-split-text(text, offset, create-text-node)
end method split-text;

define sealed method do-split-text
    (text :: <text>, offset :: <integer>, creator :: <function>)
 => (new-text :: <text>)
  check-read-only(text);
  let data     :: <DOM-string> = data(text);
  let length   :: <integer>    = length(text);
  when (offset < 0 | offset > length)
    error(make(<index-size-error>,
	       format-string: "Offset %d is wrong for text node %=",
	       format-arguments: vector(offset, text)))
  end;
  let string1 :: <DOM-string> = make(<DOM-string>, size: offset);
  let string2 :: <DOM-string> = make(<DOM-string>, size: length - offset);
  copy-string-into!(data, 0, offset, string1, 0);
  copy-string-into!(data, offset, length, string2, 0);
  data(text) := string1;
  let new-text :: <text> = creator(owner-document(text), string2);
  insert-before(node-parent(text), new-text, next-sibling(text));
  new-text
end method do-split-text;

define sealed method normalize-text
    (node :: <text>) => ()
  let text   :: <DOM-string> = node.%value;
  let length :: <integer> = size(text);
  let space? :: <boolean> = #f;		// previous character is whitespace
  let j      :: <integer> = 0;
  for (i :: <integer> from 0 below length)
    let char :: <byte-character> = text[i];
    if (xml-whitespace?(char))
      unless (space?)
	space?  := #t;
	text[j] := ' ';
	inc!(j)
      end
    else
      space?  := #f;
      text[j] := text[i];
      inc!(j)
    end
  end;
  let _start :: <integer> = if (j > 0 & text[0] = ' ') 1 else 0 end;
  let _end   :: <integer> = if (j > 0 & text[j - 1] = ' ') j - 1 else j end;
  if (_start < _end)
    node.%value := copy-string(text, start: _start, end: _end)
  else
    node.%value := ""
  end
end method normalize-text;


/// CDATA sections

//
// interface CDATASection : Text {
// };
//

define sealed class <CDATA-section> (<text>)
  keyword type: = $CDATA-section-node;
end class <CDATA-section>;

define sealed method split-text
    (text :: <CDATA-section>, offset :: <integer>)
 => (new-text :: <CDATA-section>)
  do-split-text(text, offset, create-CDATA-section)
end method split-text;


/// Comments

//
// interface Comment : CharacterData {
// };
//

define sealed class <comment> (<character-data>)
  keyword type: = $comment-node;
end class <comment>;
