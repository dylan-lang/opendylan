Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Add an elements page for collections, and make it the default

define sideways method frame-property-types
    (frame :: <environment-frame>, class :: subclass(<collection-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"elements"))
end method frame-property-types;

define sideways method frame-default-property-type
    (frame :: <environment-frame>, class :: subclass(<collection-object>))
 => (type :: false-or(<symbol>))
  #"elements"
end method frame-default-property-type;


/// Collection wrapper
//
// This object encapsulates a key and matching element from a collection

define constant <explicit-key-type>
  = type-union(<integer>, <string>, <environment-object>);

define abstract sealed class <collection-wrapper> (<object-wrapper>)
end class <collection-wrapper>;

define sealed domain make (singleton(<collection-wrapper>));
define sealed domain initialize (<collection-wrapper>);

define sealed class <sequence-collection-wrapper> (<collection-wrapper>)
  sealed constant slot wrapper-key :: <integer>,
    required-init-keyword: key:;
end class <sequence-collection-wrapper>;

define sealed class <range-wrapper> (<collection-wrapper>)
  sealed constant slot wrapper-key :: <string>,
    required-init-keyword: key:;
end class <range-wrapper>;

define sealed class <explicit-key-collection-wrapper> (<collection-wrapper>)
  sealed constant slot wrapper-key :: <explicit-key-type>,
    required-init-keyword: key:;
end class <explicit-key-collection-wrapper>;

define method frame-collection-contents
    (frame :: <environment-frame>,
     collection :: <collection-object>)
 => (contents :: <sequence>)
  let project = frame.ensure-frame-project;
  let size = frame-element-count(frame, collection);
  let element-range = range(from: 0, below: size);
  let keys = collection-keys(project, collection, range: element-range);
  let elements = collection-elements(project, collection, range: element-range);
  map(method
          (key :: <explicit-key-type>, element :: <environment-object>)
       => (wrapper :: <collection-wrapper>)
        let class
          = select (collection by instance?)
              <range-object> =>
                <range-wrapper>;
              <explicit-key-collection-object> =>
                <explicit-key-collection-wrapper>;
              <sequence-object> =>
                <sequence-collection-wrapper>;
              //---*** andrewa: shouldn't be necessary, but is...
              <collection-object> =>
                <sequence-collection-wrapper>;
            end;
        make(class, key: key, object: element)
      end,
      keys, elements)
end method frame-collection-contents;

//--- Ultimately this should probably be a registry setting
define constant $maximum-elements = 5000;

define method frame-element-count
    (frame :: <environment-frame>, object :: <collection-object>)
 => (new-size :: <integer>)
  let project = frame.ensure-frame-project;
  let size = collection-size(project, object);
  if (size > $maximum-elements
        & environment-question
            (format-to-string
               ("There are %d elements in '%s', show just the first %d?",
                size, frame-object-unique-name(frame, object),
                $maximum-elements),
             owner: frame))
    $maximum-elements
  else
    size
  end
end method frame-element-count;

define method frame-sort-collection-contents
    (frame :: <environment-frame>, contents :: <sequence>,
     order :: <symbol>)
 => (contents :: <sequence>)
  local method environment-object-key
            (object :: <environment-object>) => (label :: <string>)
          frame-print-collection-content(frame, object)
        end method environment-object-key;
  local method collection-label-key
            (wrapper :: <collection-wrapper>)
         => (label :: type-union(<string>, <integer>))
          let key = wrapper.wrapper-key;
          select (key by instance?)
            <environment-object> => environment-object-key(key);
            <integer>, <string>  => key;
          end
        end method collection-label-key;
  select (order)
    #"key" =>
      frame-sort-items(frame, contents,
                       label-key: collection-label-key);
    #"reverse-key" =>
      frame-sort-items(frame, contents,
                       label-key: collection-label-key,
                       test: \>);
    #"element" =>
      frame-sort-items
        (frame, contents, key: wrapper-object,
         label-key: curry(frame-print-collection-content, frame));
  end
end method frame-sort-collection-contents;


/// Elements property page

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>,
     class :: subclass(<collection-object>),
     type == #"elements")
 => (label :: <string>, displayer :: <table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<table-control-displayer>,
           element-label: "element",
           information-available?-function: curry(application-tethered?, project),
           transaction-function: curry(perform-application-transaction, project),
           children-generator: curry(frame-collection-contents, frame),
           headings: #["Key", "Element"],
           widths:   #[200, 1000],
           sort-orders: #[#[#"key", #"reverse-key"], #"element"],
           sort-order: #"key",
           sort-function: curry(frame-sort-collection-contents, frame),
           generators: vector(identity, wrapper-object),
           label-key: curry(frame-print-collection-content, frame));
  values("Elements", displayer)
end method make-frame-property-page-displayer;

define method frame-print-collection-content
    (frame :: <environment-frame>, object :: <environment-object>)
 => (string :: <string>)
  frame-print-environment-object(frame, object)
end method frame-print-collection-content;

define method frame-print-collection-content
    (frame :: <environment-frame>, wrapper :: <collection-wrapper>)
 => (string :: <string>)
  let project = frame.ensure-frame-project;
  let key = wrapper.wrapper-key;
  select (key by instance?)
    <environment-object> => frame-print-collection-content(frame, key);
    <string>             => key;
    otherwise            => format-to-string("%=", key);
  end
end method frame-print-collection-content;


/// Browsing of collection entries

define method frame-browse-collection-wrapper-key
    (frame :: <environment-frame>, target :: <command-target>)
 => ()
  let pane = target.target-pane;
  let wrapper = target.target-object;
  let key = wrapper.wrapper-key;
  frame-browse-target(frame, target: make-command-target(pane, key))
end method frame-browse-collection-wrapper-key;

define function frame-browse-target-key
    (frame :: <environment-frame>) => ()
  frame-browse-collection-wrapper-key(frame, frame.frame-command-target)
end function frame-browse-target-key;

define constant $browse-target-key-doc
  = "Opens a browser on the key for the selected element.";

define constant $browse-target-key-command
  = make-command-decorator("Browse Key", frame-browse-target-key,
                           documentation: $browse-target-key-doc);

define command-table *explicit-key-collection-browse-popup-menu-command-table*
    (*global-command-table*)
  command $describe-target-command;
  command $browse-target-command;
  command $browse-target-type-command;
  command $browse-target-key-command;
end command-table *explicit-key-collection-browse-popup-menu-command-table*;

define command-table *explicit-key-collection-popup-menu-command-table*
    (*global-command-table*)
  include *explicit-key-collection-browse-popup-menu-command-table*;
  include *popup-menu-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *explicit-key-collection-popup-menu-command-table*;

define method command-table-for-target
    (frame :: <environment-frame>,
     wrapper :: <explicit-key-collection-wrapper>)
 => (comtab :: <command-table>)
  if (instance?(wrapper.wrapper-key, <environment-object>))
    *explicit-key-collection-popup-menu-command-table*
  else
    next-method()
  end
end method command-table-for-target;
