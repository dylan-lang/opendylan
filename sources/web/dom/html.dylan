Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// HTML Elements

define constant $html-element-names :: <string-table> = make(<string-table>);

define macro html-element-definer
  { define html-element ?tag:expression
      ?attributes:*
    end }
    => { $html-element-names[?tag] := ?tag;
	 define html-attributes
	   ?attributes
	 end }
end macro html-element-definer;

define macro html-attributes-definer
  { define html-attributes end }
    => { }
  { define html-attributes
      readonly attribute ?tag:expression => ?attribute:name :: ?type:expression;
      ?attributes:*
    end }
    => { define html-attribute readonly ?tag => ?attribute :: ?type end;
	 define html-attributes ?attributes end }
  { define html-attributes
      attribute ?tag:expression => ?attribute:name :: ?type:expression;
      ?attributes:*
    end }
    => { define html-attribute ?tag => ?attribute :: ?type end;
	 define html-attributes ?attributes end }
end macro html-attributes-definer;

define macro html-attribute-definer
  { define html-attribute readonly ?tag:expression => ?attribute:name :: ?type:expression end }
    => { define sealed method ?attribute
	     (_elt :: <html-element>) => (_val :: ?type)
	   get-html-attribute(_elt, ?tag, ?type)
	 end method }
  { define html-attribute ?tag:expression => ?attribute:name :: ?type:expression end }
    => { define sealed method ?attribute
	     (_elt :: <html-element>) => (_val :: ?type)
	   get-html-attribute(_elt, ?tag, ?type)
	 end method;
	 define sealed method ?attribute ## "-setter"
	     (_val :: ?type, _elt :: <html-element>) => ()
	   set-html-attribute(_elt, ?tag, ?type, _val)
	 end method }
end macro html-attribute-definer;


define sealed method get-html-attribute
    (elt :: <html-element>, name :: <DOM-string>, type == <DOM-string>)
 => (_value :: <DOM-string>)
  get-attribute(elt, name)
end method get-html-attribute;

define sealed method set-html-attribute
    (elt :: <html-element>, name :: <DOM-string>, type == <DOM-string>, _value :: <DOM-string>) => ()
  set-attribute(elt, name, _value)
end method set-html-attribute;


define sealed method get-html-attribute
    (elt :: <html-element>, name :: <DOM-string>, type == <integer>)
 => (_value :: <integer>)
  string-to-integer(get-attribute(elt, name))
end method get-html-attribute;

define sealed method set-html-attribute
    (elt :: <html-element>, name :: <DOM-string>, type == <integer>, _value :: <integer>) => ()
  set-attribute(elt, name, integer-to-string(_value))
end method set-html-attribute;


define sealed method get-html-attribute
    (elt :: <html-element>, name :: <DOM-string>, type == <boolean>)
 => (_value :: <boolean>)
  select (get-attribute(elt, name) by string-equal?)
    "YES" => #t;
    "NO"  => #f
  end
end method get-html-attribute;

define sealed method set-html-attribute
    (elt :: <html-element>, name :: <DOM-string>, type == <boolean>, _value :: <boolean>) => ()
  let _value = if (_value) "YES" else "NO" end;
  set-attribute(elt, name, _value)
end method set-html-attribute;


/// Define the shared attribute methods

define html-attribute "ACCESSKEY"    => html/access-key    :: <DOM-string> end;
define html-attribute "ALIGN"        => html/align         :: <DOM-string> end;
define html-attribute "ALT"          => html/alt           :: <DOM-string> end;
define html-attribute "ARCHIVE"      => html/archive       :: <DOM-string> end;
define html-attribute "BGCOLOR"      => html/bg-color      :: <DOM-string> end;
define html-attribute "BORDER"       => html/border        :: <DOM-string> end;
define html-attribute "CH"           => html/ch            :: <DOM-string> end;
define html-attribute "CHOFF"        => html/ch-off        :: <DOM-string> end;
define html-attribute "CHARSET"      => html/charset       :: <DOM-string> end;
define html-attribute "CITE"         => html/cite          :: <DOM-string> end;
define html-attribute "CLASS"        => html/class         :: <DOM-string> end;
define html-attribute "CODE"         => html/code          :: <DOM-string> end;
define html-attribute "CODEBASE"     => html/code-base     :: <DOM-string> end;
define html-attribute "COLOR"        => html/color         :: <DOM-string> end;
define html-attribute "COMPACT"      => html/compact?      :: <boolean>    end;
define html-attribute "COORDS"       => html/coords        :: <DOM-string> end;
define html-attribute "DEFAULTVALUE" => html/default-value :: <DOM-string> end;
define html-attribute "DIR"          => html/dir           :: <DOM-string> end;
define html-attribute "DISABLED"     => html/disabled?     :: <boolean>    end;
define html-attribute "FACE"         => html/face          :: <DOM-string> end;
define html-attribute "FRAMEBORDER"  => html/frame-border  :: <DOM-string> end;
define html-attribute "HEIGHT"       => html/height        :: <DOM-string> end;
define html-attribute "HREF"         => html/href          :: <DOM-string> end;
define html-attribute "HREFLANG"     => html/href-lang     :: <DOM-string> end;
define html-attribute "HSPACE"       => html/hspace        :: <DOM-string> end;
define html-attribute "HTMLFOR"      => html/html-for      :: <DOM-string> end;
define html-attribute "LABEL"        => html/label         :: <DOM-string> end;
define html-attribute "LANG"         => html/lang          :: <DOM-string> end;
define html-attribute "LONGDESC"     => html/long-desc     :: <DOM-string> end;
define html-attribute "MARGINHEIGHT" => html/margin-height :: <DOM-string> end;
define html-attribute "MARGINWIDTH"  => html/margin-width  :: <DOM-string> end;
define html-attribute "MEDIA"        => html/media         :: <DOM-string> end;
define html-attribute "NAME"         => html/name          :: <DOM-string> end;
define html-attribute "READONLY"     => html/read-only?    :: <boolean>    end;
define html-attribute "REL"          => html/rel           :: <DOM-string> end;
define html-attribute "REV"          => html/rev           :: <DOM-string> end;
define html-attribute "SCROLLING"    => html/scrolling     :: <DOM-string> end;
define html-attribute "SHAPE"        => html/shape         :: <DOM-string> end;
define html-attribute "SIZE"         => html/size          :: <DOM-string> end;
define html-attribute "SRC"          => html/src           :: <DOM-string> end;
define html-attribute "TABINDEX"     => html/tab-index     :: <integer>    end;
define html-attribute "TARGET"       => html/target        :: <DOM-string> end;
define html-attribute "TEXT"         => html/text          :: <DOM-string> end;
define html-attribute "TITLE"        => html/title         :: <DOM-string> end;
define html-attribute "TYPE"         => html/type          :: <DOM-string> end;
define html-attribute "USEMAP"       => html/use-map       :: <DOM-string> end;
define html-attribute "VALIGN"       => html/valign        :: <DOM-string> end;
define html-attribute "VALUE"        => html/value         :: <DOM-string> end;
define html-attribute "VSPACE"       => html/vspace        :: <DOM-string> end;
define html-attribute "WIDTH"        => html/width         :: <DOM-string> end;

define sealed method html/form
    (elt :: <html-element>) => (form :: false-or(<html-element>))
  block (return)
    for (parent = node-parent(elt) then node-parent(parent),
	 until: ~parent)
      when (tag-name(parent) = "FORM")
	return(parent)
      end
    end;
    #f
  end
end method html/form;


//
// interface HTMLHtmlElement : HTMLElement {
//            attribute  DOMString            version;
// };
//

define html-element "HTML"
  attribute "VERSION" => html/version :: <DOM-string>;
end;


//
// interface HTMLHeadElement : HTMLElement {
//            attribute  DOMString            profile;
// };
//

define html-element "HEAD"
  attribute "PROFILE" => html/profile :: <DOM-string>;
end;

define html-element "H1" end;
define html-element "H2" end;
define html-element "H3" end;
define html-element "H4" end;
define html-element "H5" end;
define html-element "H6" end;


//
// interface HTMLLinkElement : HTMLElement {
//            attribute  boolean              disabled;
//            attribute  DOMString            charset;
//            attribute  DOMString            href;
//            attribute  DOMString            hreflang;
//            attribute  DOMString            media;
//            attribute  DOMString            rel;
//            attribute  DOMString            rev;
//            attribute  DOMString            target;
//            attribute  DOMString            type;
// };
//

define html-element "LINK"
  /* attribute "DISABLED" => html/disabled? :: <boolean>; */
  /* attribute "CHARSET"  => html/charset   :: <DOM-string>; */
  /* attribute "HREF"     => html/href      :: <DOM-string>; */
  /* attribute "HREFLANG" => html/href-lang :: <DOM-string>; */
  /* attribute "MEDIA"    => html/media     :: <DOM-string>; */
  /* attribute "REL"      => html/rel       :: <DOM-string>; */
  /* attribute "REV"      => html/rev       :: <DOM-string>; */
  /* attribute "TARGET"   => html/target    :: <DOM-string>; */
  /* attribute "TYPE"     => html/type      :: <DOM-string>; */
end;


//
// interface HTMLTitleElement : HTMLElement {
//            attribute  DOMString            text;
// };
//

define html-element "TITLE"
  /* attribute "TEXT" => html/text :: <DOM-string>; */		//---*** compute this
end;


//
// interface HTMLMetaElement : HTMLElement {
//            attribute  DOMString            content;
//            attribute  DOMString            httpEquiv;
//            attribute  DOMString            name;
//            attribute  DOMString            scheme;
// };
//

define html-element "META"
  attribute "CONTENT"   => html/content    :: <DOM-string>;
  attribute "HTTPEQUIV" => html/http-equiv :: <DOM-string>;
  /* attribute "NAME"   => html/name       :: <DOM-string>; */
  attribute "SCHEME"    => html/scheme     :: <DOM-string>;
end;


//
// interface HTMLAddressElement : HTMLElement {
// };
//

define html-element "ADDRESS"
end;


//
// interface HTMLBaseElement : HTMLElement {
//            attribute  DOMString            href;
//            attribute  DOMString            target;
// };
//

define html-element "BASE"
  /* attribute "HREF"   => html/href   :: <DOM-string>; */
  /* attribute "TARGET" => html/target :: <DOM-string>; */
end;


//
// interface HTMLIsIndexElement : HTMLElement {
//   readonly attribute  HTMLFormElement      form;
//            attribute  DOMString            prompt;
// };
//

define html-element "ISINDEX"
  /* readonly attribute "FORM" => html/form   :: <html-element>; */
  attribute "PROMPT"           => html/prompt :: <DOM-string>;
end;


//
// interface HTMLStyleElement : HTMLElement {
//            attribute  boolean              disabled;
//            attribute  DOMString            media;
//            attribute  DOMString            type;
// };
//

define html-element "STYLE"
  /* attribute "DISABLED" => html/disabled? :: <boolean>; */
  /* attribute "MEDIA"    => html/media     :: <DOM-string>; */
  /* attribute "TYPE"     => html/type      :: <DOM-string>; */
end;


//
// interface HTMLBodyElement : HTMLElement {
//            attribute  DOMString            aLink;
//            attribute  DOMString            background;
//            attribute  DOMString            bgColor;
//            attribute  DOMString            link;
//            attribute  DOMString            text;
//            attribute  DOMString            vLink;
// };
//

define html-element "BODY"
  attribute "ALINK"       => html/alink       :: <DOM-string>;
  attribute "BACKGROUND"  => html/background  :: <DOM-string>;
  /* attribute "BGCOLOR"  => html/bg-color    :: <DOM-string>; */
  attribute "LINK"        => html/link        :: <DOM-string>;
  /* attribute "TEXT"     => html/text        :: <DOM-string>; */	//---*** compute this
  attribute "VLINK"       => html/vlink       :: <DOM-string>;
end;


//
// interface HTMLFormElement : HTMLElement {
//   readonly attribute  HTMLCollection       elements;
//   readonly attribute  long                 length;
//            attribute  DOMString            name;
//            attribute  DOMString            acceptCharset;
//            attribute  DOMString            action;
//            attribute  DOMString            enctype;
//            attribute  DOMString            method;
//            attribute  DOMString            target;
//   void                      submit();
//   void                      reset();
// };
//

//---*** Do 'html/elements' and 'html/length'
define html-element "FORM"
  /* readonly attribute "ELEMENTS" => html/elements       :: <html-collection>; */
  /* readonly attribute "LENGTH"   => html/length         :: <integer>; */
  /* attribute "NAME"              => html/name           :: <DOM-string>; */
  attribute "ACCEPTCHARSET"        => html/accept-charset :: <DOM-string>;
  attribute "ACTION"               => html/action         :: <DOM-string>;
  attribute "ENCTYPE"              => html/enctype        :: <DOM-string>;
  attribute "METHOD"               => html/method         :: <DOM-string>;
  /* attribute "TARGET"            => html/target         :: <DOM-string>; */
end;

define sealed method html/submit
    (form :: <html-element>) => ()
  assert(string-equal?(tag-name(form), "FORM"),
	 "You can only call 'submit' on a <FORM>");
  //---*** Implement this
end method html/submit;

define sealed method html/reset
    (form :: <html-element>) => ()
  assert(string-equal?(tag-name(form), "FORM"),
	 "You can only call 'reset' on a <FORM>");
  //---*** Implement this
end method html/reset;


//
// interface HTMLSelectElement : HTMLElement {
//   readonly attribute  DOMString            type;
//            attribute  long                 selectedIndex;
//            attribute  DOMString            value;
//   readonly attribute  long                 length;
//   readonly attribute  HTMLFormElement      form;
//   readonly attribute  HTMLCollection       options;
//            attribute  boolean              disabled;
//            attribute  boolean              multiple;
//            attribute  DOMString            name;
//            attribute  long                 size;
//            attribute  long                 tabIndex;
//   void                      add(in HTMLElement element, 
//                                 in HTMLElement before);
//   void                      remove(in long index);
//   void                      blur();
//   void                      focus();
// };
//

//---*** Do 'html/options' and 'html/length'
//---*** Do 'add' and 'remove', but call them 'add-option' and 'remove-option'
define html-element "SELECT"
  /* readonly attribute "TYPE"    => html/type           :: <DOM-string>; */	//---*** compute this
  attribute "SELECTEDINDEX"       => html/selected-index :: <integer>;
  /* attribute "VALUE"            => html/value          :: <DOM-string>; */
  /* readonly attribute "LENGTH"  => html/length         :: <integer>; */
  /* readonly attribute "FORM"    => html/form           :: <html-element>; */
  /* readonly attribute "OPTIONS" => html/options        :: <html-collection>; */
  /* attribute "DISABLED"         => html/disabled?      :: <boolean>; */
  attribute "MULTIPLE"            => html/multiple?      :: <boolean>;
  /* attribute "NAME"             => html/name           :: <DOM-string>; */
  /* attribute "SIZE"             => html/size           :: <integer>; */
  /* attribute "TABINDEX"         => html/tab-index      :: <integer>; */
end;


//
// interface HTMLOptGroupElement : HTMLElement {
//            attribute  boolean              disabled;
//            attribute  DOMString            label;
// };
//

define html-element "OPTGROUP"
  /* attribute "DISABLED" => html/disabled? :: <boolean>; */
  /* attribute "LABEL"    => html/label     :: <DOM-string>; */
end;


//
// interface HTMLOptionElement : HTMLElement {
//   readonly attribute  HTMLFormElement      form;
//            attribute  boolean              defaultSelected;
//   readonly attribute  DOMString            text;
//            attribute  long                 index;
//            attribute  boolean              disabled;
//            attribute  DOMString            label;
//   readonly attribute  boolean              selected;
//            attribute  DOMString            value;
// };
//

define html-element "OPTION"
  /* readonly attribute "FORM"  => html/form               :: <html-element>; */
  attribute "DEFAULTSELECTED"   => html/default-selected?  :: <boolean>;
  /* readonly attribute "TEXT"  => html/text               :: <DOM-string>; */	//---*** compute this?
  attribute "INDEX"             => html/index              :: <integer>;
  /* attribute "DISABLED"       => html/disabled?          :: <boolean>; */
  /* attribute "LABEL"          => html/label              :: <DOM-string>; */
  readonly attribute "SELECTED" => html/selected?          :: <boolean>;
  /* attribute "VALUE"          => html/value              :: <DOM-string>; */
end;


//
// interface HTMLInputElement : HTMLElement {
//            attribute  DOMString            defaultValue;
//            attribute  boolean              defaultChecked;
//   readonly attribute  HTMLFormElement      form;
//            attribute  DOMString            accept;
//            attribute  DOMString            accessKey;
//            attribute  DOMString            align;
//            attribute  DOMString            alt;
//            attribute  boolean              checked;
//            attribute  boolean              disabled;
//            attribute  long                 maxLength;
//            attribute  DOMString            name;
//            attribute  boolean              readOnly;
//            attribute  DOMString            size;
//            attribute  DOMString            src;
//            attribute  long                 tabIndex;
//   readonly attribute  DOMString            type;
//            attribute  DOMString            useMap;
//            attribute  DOMString            value;
//   void                      blur();
//   void                      focus();
//   void                      select();
//   void                      click();
// };
//

define html-element "INPUT"
  /* attribute "DEFAULTVALUE"  => html/default-value    :: <DOM-string>; */
  attribute "DEFAULTCHECKED"   => html/default-checked? :: <boolean>;
  /* readonly attribute "FORM" => html/form             :: <html-element>; */
  attribute "ACCEPT"           => html/accept           :: <DOM-string>;
  /* attribute "ACCESSKEY"     => html/access-key       :: <DOM-string>; */
  /* attribute "ALIGN"         => html/align            :: <DOM-string>; */
  /* attribute "ALT"           => html/alt              :: <DOM-string>; */
  attribute "CHECKED"          => html/checked?         :: <boolean>;
  /* attribute "DISABLED"      => html/disabled?        :: <boolean>; */
  attribute "MAXLENGTH"        => html/max-length       :: <integer>;
  /* attribute "NAME"          => html/name             :: <DOM-string>; */
  /* attribute "READONLY"      => html/read-only?       :: <boolean>; */
  /* attribute "SIZE"          => html/size             :: <DOM-string>; */
  /* attribute "SRC"           => html/src              :: <DOM-string>; */
  /* attribute "TABINDEX"      => html/tab-index        :: <integer>; */
  /* readonly attribute "TYPE" => html/type             :: <DOM-string>; */	//---*** compute this
  /* attribute "USEMAP"        => html/use-map          :: <DOM-string>; */
  /* attribute "VALUE"         => html/value            :: <DOM-string>; */
end;

define sealed method html/blur
    (input :: <html-element>) => ()
  assert(string-equal?(tag-name(input), "SELECT")
	 | string-equal?(tag-name(input), "INPUT")
	 | string-equal?(tag-name(input), "TEXTAREA")
	 | string-equal?(tag-name(input), "A"),
	 "You can only call 'blur' on a <SELECT>, <INPUT>, <TEXTAREA>, or <A>");
  //---*** Implement this
end method html/blur;

define sealed method html/focus
    (input :: <html-element>) => ()
  assert(string-equal?(tag-name(input), "SELECT")
	 | string-equal?(tag-name(input), "INPUT")
	 | string-equal?(tag-name(input), "TEXTAREA")
	 | string-equal?(tag-name(input), "A"),
	 "You can only call 'focus' on a <SELECT>, <INPUT>, <TEXTAREA>, or <A>");
  //---*** Implement this
end method html/focus;

define sealed method html/select
    (input :: <html-element>) => ()
  assert(string-equal?(tag-name(input), "INPUT")
	 | string-equal?(tag-name(input), "TEXTAREA"),
	 "You can only call 'select' on a <SELECT> or <TEXTAREA>");
  //---*** Implement this
end method html/select;

define sealed method html/click
    (input :: <html-element>) => ()
  assert(string-equal?(tag-name(input), "INPUT"),
	 "You can only call 'click' on an <INPUT>");
  //---*** Implement this
end method html/click;


//
// interface HTMLTextAreaElement : HTMLElement {
//            attribute  DOMString            defaultValue;
//   readonly attribute  HTMLFormElement      form;
//            attribute  DOMString            accessKey;
//            attribute  long                 cols;
//            attribute  boolean              disabled;
//            attribute  DOMString            name;
//            attribute  boolean              readOnly;
//            attribute  long                 rows;
//            attribute  long                 tabIndex;
//   readonly attribute  DOMString            type;
//            attribute  DOMString            value;
//   void                      blur();
//   void                      focus();
//   void                      select();
// };
//

define html-element "TEXTAREA"
  /* attribute "DEFAULTVALUE"  => html/default-value :: <DOM-string>; */
  /* readonly attribute "FORM" => html/form          :: <html-element>; */
  /* attribute "ACCESSKEY"     => html/access-key    :: <DOM-string>; */
  attribute "NCOLS"            => html/ncols         :: <integer>;
  /* attribute "DISABLED"      => html/disabled?     :: <boolean>; */
  /* attribute "NAME"          => html/name          :: <DOM-string>; */
  /* attribute "READONLY"      => html/read-only?    :: <boolean>; */
  attribute "NROWS"            => html/nrows         :: <integer>;
  /* attribute "TABINDEX"      => html/tab-index     :: <integer>; */
  /* readonly attribute "TYPE" => html/type          :: <DOM-string>; */	//---*** compute this
  /* attribute "VALUE"         => html/value         :: <DOM-string>; */
end;


//
// interface HTMLButtonElement : HTMLElement {
//   readonly attribute  HTMLFormElement      form;
//            attribute  DOMString            accessKey;
//            attribute  boolean              disabled;
//            attribute  DOMString            name;
//            attribute  long                 tabIndex;
//   readonly attribute  DOMString            type;
//            attribute  DOMString            value;
// };
//

define html-element "BUTTON"
  /* readonly attribute "FORM" => html/form       :: <html-element>; */
  /* attribute "ACCESSKEY"     => html/access-key :: <DOM-string>; */
  /* attribute "DISABLED"      => html/disabled?  :: <boolean>; */
  /* attribute "NAME"          => html/name       :: <DOM-string>; */
  /* attribute "TABINDEX"      => html/tab-index  :: <integer>; */
  /* readonly attribute "TYPE" => html/type       :: <DOM-string>; */	//---*** compute this
  /* attribute "VALUE"         => html/value      :: <DOM-string>; */
end;


//
// interface HTMLLabelElement : HTMLElement {
//   readonly attribute  HTMLFormElement      form;
//            attribute  DOMString            accessKey;
//            attribute  DOMString            htmlFor;
// };
//

define html-element "LABEL"
  /* readonly attribute "FORM" => html/form       :: <html-element>; */
  /* attribute "ACCESSKEY"     => html/access-key :: <DOM-string>; */
  /* attribute "HTML-FOR"      => html/html-for   :: <DOM-string>; */
end;


//
// interface HTMLFieldSetElement : HTMLElement {
//   readonly attribute  HTMLFormElement      form;
// };
//

define html-element "FIELDSET"
  /* readonly attribute "FORM" => html/form :: <html-element>; */
end;


//
// interface HTMLLegendElement : HTMLElement {
//   readonly attribute  HTMLFormElement      form;
//            attribute  DOMString            accessKey;
//            attribute  DOMString            align;
// };
//

define html-element "LEGEND"
  /* readonly attribute "FORM" => html/form       :: <html-element>; */
  /* attribute "ACCESSKEY"     => html/access-key :: <DOM-string>; */
  /* attribute "ALIGN"         => html/align      :: <DOM-string>; */
end;


//
// interface HTMLUListElement : HTMLElement {
//            attribute  boolean              compact;
//            attribute  DOMString            type;
// };
//

define html-element "UL"
  /* attribute "COMPACT" => html/compact? :: <boolean>; */
  /* attribute "TYPE"    => html/type     :: <DOM-string>; */
end;


//
// interface HTMLOListElement : HTMLElement {
//            attribute  boolean              compact;
//            attribute  long                 start;
//            attribute  DOMString            type;
// };
//

define html-element "OL"
  /* attribute "COMPACT" => html/compact? :: <boolean>; */
  attribute "START"      => html/start    :: <integer>;
  /* attribute "TYPE"    => html/type     :: <DOM-string>; */
end;


//
// interface HTMLDListElement : HTMLElement {
//            attribute  boolean              compact;
// };
//

define html-element "DL"
  /* attribute "COMPACT" => html/compact? :: <boolean>; */
end;

define html-element "DT" end;
define html-element "DD" end;


//
// interface HTMLDirectoryElement : HTMLElement {
//            attribute  boolean              compact;
// };
//

define html-element "DIR"
  /* attribute "COMPACT" => html/compact? :: <boolean>; */
end;


//
// interface HTMLMenuElement : HTMLElement {
//            attribute  boolean              compact;
// };
//

define html-element "MENU"
  /* attribute "COMPACT" => html/compact? :: <boolean>; */
end;


//
// interface HTMLLIElement : HTMLElement {
//            attribute  DOMString            type;
//            attribute  long                 value;
// };
//

define html-element "LI"
  /* attribute "TYPE"  => html/type  :: <DOM-string>; */
  /* attribute "VALUE" => html/value :: <integer>; */
end;


//
// interface HTMLBlockquoteElement : HTMLElement {
//            attribute  DOMString            cite;
// };
//

define html-element "BLOCKQUOTE"
  /* attribute "CITE" => html/cite :: <DOM-string>; */
end;


//
// interface HTMLDivElement : HTMLElement {
//            attribute  DOMString            align;
// };
//

define html-element "DIV"
  /* attribute "ALIGN" => html/align :: <DOM-string>; */
end;


//
// interface HTMLCodeElement : HTMLElement {
//            attribute  DOMString            align;
// };
//

define html-element "CODE"
  /* attribute "ALIGN" => html/align :: <DOM-string>; */
end;


//
// interface HTMLParagraphElement : HTMLElement {
//            attribute  DOMString            align;
// };
//

define html-element "P"
  /* attribute "ALIGN" => html/align :: <DOM-string>; */
end;


//
// interface HTMLHeadingElement : HTMLElement {
//            attribute  DOMString            align;
// };
//

define html-element "HEADING"
  /* attribute "ALIGN" => html/align :: <DOM-string>; */
end;


//
// interface HTMLQuoteElement : HTMLElement {
//            attribute  DOMString            cite;
// };
//

define html-element "QUOTE"
  /* attribute "CITE" => html/cite :: <DOM-string>; */
end;


//
// interface HTMLPreElement : HTMLElement {
//            attribute  long                 width;
// };
//

define html-element "PRE"
  /* attribute "WIDTH" => html/width :: <integer>; */
end;


//
// interface HTMLBRElement : HTMLElement {
//            attribute  DOMString            clear;
// };
//

define html-element "BR"
  attribute "CLEAR" => html/clear :: <DOM-string>;
end;


//
// interface HTMLBaseFontElement : HTMLElement {
//            attribute  DOMString            color;
//            attribute  DOMString            face;
//            attribute  DOMString            size;
// };
//

define html-element "BASEFONT"
  /* attribute "COLOR" => html/color :: <DOM-string>; */
  /* attribute "FACE"  => html/face  :: <DOM-string>; */
  /* attribute "SIZE"  => html/size  :: <DOM-string>; */
end;


//
// interface HTMLFontElement : HTMLElement {
//            attribute  DOMString            color;
//            attribute  DOMString            face;
//            attribute  DOMString            size;
// };
//

define html-element "FONT"
  /* attribute "COLOR" => html/color :: <DOM-string>; */
  /* attribute "FACE"  => html/face  :: <DOM-string>; */
  /* attribute "SIZE"  => html/size  :: <DOM-string>; */
end;

define html-element "B" end;
define html-element "I" end;


//
// interface HTMLHRElement : HTMLElement {
//            attribute  DOMString            align;
//            attribute  boolean              noShade;
//            attribute  DOMString            size;
//            attribute  DOMString            width;
// };
//

define html-element "HR"
  /* attribute "ALIGN"   => html/align     :: <DOM-string>; */
  attribute "NO-SHADE"   => html/no-shade? :: <boolean>;
  /* attribute "SIZE"    => html/size      :: <DOM-string>; */
  /* attribute "WIDTH"   => html/width     :: <DOM-string>; */
end;


//
// interface HTMLModElement : HTMLElement {
//            attribute  DOMString            cite;
//            attribute  DOMString            dateTime;
// };
//

define html-element "MOD"
  /* attribute "CITE"  => html/cite      :: <DOM-string>; */
  attribute "DATETIME" => html/date-time :: <DOM-string>;
end;


//
// interface HTMLAnchorElement : HTMLElement {
//            attribute  DOMString            accessKey;
//            attribute  DOMString            charset;
//            attribute  DOMString            coords;
//            attribute  DOMString            href;
//            attribute  DOMString            hreflang;
//            attribute  DOMString            name;
//            attribute  DOMString            rel;
//            attribute  DOMString            rev;
//            attribute  DOMString            shape;
//            attribute  long                 tabIndex;
//            attribute  DOMString            target;
//            attribute  DOMString            type;
//   void                      blur();
//   void                      focus();
// };
//

define html-element "A"
  /* attribute "ACCESSKEY" => html/access-key :: <DOM-string>; */
  /* attribute "CHARSET"   => html/charset    :: <DOM-string>; */
  /* attribute "COORDS"    => html/coords     :: <DOM-string>; */
  /* attribute "HREF"      => html/href       :: <DOM-string>; */
  /* attribute "HREFLANG"  => html/href-lang  :: <DOM-string>; */
  /* attribute "NAME"      => html/name       :: <DOM-string>; */
  /* attribute "REL"       => html/rel        :: <DOM-string>; */
  /* attribute "REV"       => html/rev        :: <DOM-string>; */
  /* attribute "SHAPE"     => html/shape      :: <DOM-string>; */
  /* attribute "TABINDEX"  => html/tab-index  :: <integer>; */
  /* attribute "TARGET"    => html/target     :: <DOM-string>; */
  /* attribute "TYPE"      => html/type       :: <DOM-string>; */
end;


//
// interface HTMLImageElement : HTMLElement {
//            attribute  DOMString            lowSrc;
//            attribute  DOMString            name;
//            attribute  DOMString            align;
//            attribute  DOMString            alt;
//            attribute  DOMString            border;
//            attribute  DOMString            height;
//            attribute  DOMString            hspace;
//            attribute  boolean              isMap;
//            attribute  DOMString            longDesc;
//            attribute  DOMString            src;
//            attribute  DOMString            useMap;
//            attribute  DOMString            vspace;
//            attribute  DOMString            width;
// };
//

define html-element "IMG"
  attribute "LOWSRC"      => html/low-src   :: <DOM-string>;
  /* attribute "NAME"     => html/name      :: <DOM-string>; */
  /* attribute "ALIGN"    => html/align     :: <DOM-string>; */
  /* attribute "ALT"      => html/alt       :: <DOM-string>; */
  /* attribute "BORDER"   => html/border    :: <DOM-string>; */
  /* attribute "HEIGHT"   => html/height    :: <DOM-string>; */
  /* attribute "HSPACE"   => html/hspace    :: <DOM-string>; */
  attribute "ISMAP"       => html/is-map?   :: <boolean>;
  /* attribute "LONGDESC" => html/long-desc :: <DOM-string>; */
  /* attribute "SRC"      => html/src       :: <DOM-string>; */
  /* attribute "USEMAP"   => html/use-map   :: <DOM-string>; */
  /* attribute "VSPACE"   => html/vspace    :: <DOM-string>; */
  /* attribute "WIDTH"    => html/width     :: <DOM-string>; */
end;


//
// interface HTMLObjectElement : HTMLElement {
//   readonly attribute  HTMLFormElement      form;
//            attribute  DOMString            code;
//            attribute  DOMString            align;
//            attribute  DOMString            archive;
//            attribute  DOMString            border;
//            attribute  DOMString            codeBase;
//            attribute  DOMString            codeType;
//            attribute  DOMString            data;
//            attribute  boolean              declare;
//            attribute  DOMString            height;
//            attribute  DOMString            hspace;
//            attribute  DOMString            name;
//            attribute  DOMString            standby;
//            attribute  long                 tabIndex;
//            attribute  DOMString            type;
//            attribute  DOMString            useMap;
//            attribute  DOMString            vspace;
//            attribute  DOMString            width;
// };
//

define html-element "OBJECT"
  /* readonly attribute "FORM" => html/form      :: <html-element>; */
  /* attribute "CODE"          => html/code      :: <DOM-string>; */
  /* attribute "ALIGN"         => html/align     :: <DOM-string>; */
  /* attribute "ARCHIVE"       => html/archive   :: <DOM-string>; */
  /* attribute "BORDER"        => html/border    :: <DOM-string>; */
  /* attribute "CODEBASE"      => html/code-base :: <DOM-string>; */
  attribute "CODETYPE"         => html/code-type :: <DOM-string>;
  attribute "DATA"             => html/data      :: <DOM-string>;
  attribute "DECLARE"          => html/declare?  :: <boolean>;
  /* attribute "HEIGHT"        => html/height    :: <DOM-string>; */
  /* attribute "HSPACE"        => html/hspace    :: <DOM-string>; */
  /* attribute "NAME"          => html/name      :: <DOM-string>; */
  attribute "STANDBY"          => html/standby   :: <DOM-string>;
  /* attribute "TABINDEX"      => html/tab-index :: <integer>; */
  /* attribute "TYPE"          => html/type      :: <DOM-string>; */
  /* attribute "USEMAP"        => html/use-map   :: <DOM-string>; */
  /* attribute "VSPACE"        => html/vspace    :: <DOM-string>; */
  /* attribute "WIDTH"         => html/width     :: <DOM-string>; */
end;


//
// interface HTMLParamElement : HTMLElement {
//            attribute  DOMString            name;
//            attribute  DOMString            type;
//            attribute  DOMString            value;
//            attribute  DOMString            valueType;
// };
//

define html-element "PARAM"
  /* attribute "NAME"   => html/name       :: <DOM-string>; */
  /* attribute "TYPE"   => html/type       :: <DOM-string>; */
  /* attribute "VALUE"  => html/value      :: <DOM-string>; */
  attribute "VALUETYPE" => html/value-type :: <DOM-string>;
end;


//
// interface HTMLAppletElement : HTMLElement {
//            attribute  DOMString            align;
//            attribute  DOMString            alt;
//            attribute  DOMString            archive;
//            attribute  DOMString            code;
//            attribute  DOMString            codeBase;
//            attribute  DOMString            height;
//            attribute  DOMString            hspace;
//            attribute  DOMString            name;
//            attribute  DOMString            object;
//            attribute  DOMString            vspace;
//            attribute  DOMString            width;
// };
//

define html-element "APPLET"
  /* attribute "ALIGN"    => html/align     :: <DOM-string>; */
  /* attribute "ALT"      => html/alt       :: <DOM-string>; */
  /* attribute "ARCHIVE"  => html/archive   :: <DOM-string>; */
  /* attribute "CODE"     => html/code      :: <DOM-string>; */
  /* attribute "CODEBASE" => html/code-base :: <DOM-string>; */
  /* attribute "HEIGHT"   => html/height    :: <DOM-string>; */
  /* attribute "HSPACE"   => html/hspace    :: <DOM-string>; */
  /* attribute "NAME"     => html/name      :: <DOM-string>; */
  attribute "OBJECT"      => html/object    :: <DOM-string>;
  /* attribute "VSPACE"   => html/vspace    :: <DOM-string>; */
  /* attribute "WIDTH"    => html/width     :: <DOM-string>; */
end;


//
// interface HTMLMapElement : HTMLElement {
//   readonly attribute  HTMLCollection       areas;
//            attribute  DOMString            name;
// };
//

//---*** Do 'html/areas'
define html-element "MAP"
  /* readonly attribute "AREAS" => html/areas :: <html-collection>; */
  /* attribute "NAME"           => html/name  :: <DOM-string>; */
end;


//
// interface HTMLAreaElement : HTMLElement {
//            attribute  DOMString            accessKey;
//            attribute  DOMString            alt;
//            attribute  DOMString            coords;
//            attribute  DOMString            href;
//            attribute  boolean              noHref;
//            attribute  DOMString            shape;
//            attribute  long                 tabIndex;
//            attribute  DOMString            target;
// };
//

define html-element "AREA"
  /* attribute "ACCESSKEY" => html/access-key :: <DOM-string>; */
  /* attribute "ALT"       => html/alt        :: <DOM-string>; */
  /* attribute "COORDS"    => html/coords     :: <DOM-string>; */
  /* attribute "HREF"      => html/href       :: <DOM-string>; */
  attribute "NOHREF"       => html/no-href?   :: <boolean>;
  /* attribute "SHAPE"     => html/shape      :: <DOM-string>; */
  /* attribute "TABINDEX"  => html/tab-index  :: <integer>; */
  /* attribute "TARGET"    => html/target     :: <DOM-string>; */
end;


//
// interface HTMLScriptElement : HTMLElement {
//            attribute  DOMString            text;
//            attribute  DOMString            htmlFor;
//            attribute  DOMString            event;
//            attribute  DOMString            charset;
//            attribute  boolean              defer;
//            attribute  DOMString            src;
//            attribute  DOMString            type;
// };
//

define html-element "SCRIPTELEMENT"
  /* attribute "TEXT"    => html/text     :: <DOM-string>; */		//---*** compute this
  /* attribute "HTMLFOR" => html/html-for :: <DOM-string>; */
  attribute "EVENT"      => html/event       :: <DOM-string>;
  /* attribute "CHARSET" => html/charset  :: <DOM-string>; */
  attribute "DEFER"      => html/defer?      :: <boolean>;
  /* attribute "SRC"     => html/src      :: <DOM-string>; */
  /* attribute "TYPE"    => html/type     :: <DOM-string>; */
end;


//
// interface HTMLTableElement : HTMLElement {
//            attribute  HTMLTableCaptionElement caption;
//            attribute  HTMLTableSectionElement tHead;
//            attribute  HTMLTableSectionElement tFoot;
//   readonly attribute  HTMLCollection       rows;
//   readonly attribute  HTMLCollection       tBodies;
//            attribute  DOMString            align;
//            attribute  DOMString            bgColor;
//            attribute  DOMString            border;
//            attribute  DOMString            cellPadding;
//            attribute  DOMString            cellSpacing;
//            attribute  DOMString            frame;
//            attribute  DOMString            rules;
//            attribute  DOMString            summary;
//            attribute  DOMString            width;
//   HTMLElement               createTHead();
//   void                      deleteTHead();
//   HTMLElement               createTFoot();
//   void                      deleteTFoot();
//   HTMLElement               createCaption();
//   void                      deleteCaption();
//   HTMLElement               insertRow(in long index);
//   void                      deleteRow(in long index);
// };
//

//---*** Do 'html/caption', 'html/tHead', and 'html/tFoot'
//---*** Do 'html/rows' and 'html/tBodies'
define html-element "TABLE"
  /* attribute "CAPTION"          => html/caption  :: <html-element>; */	// <HTMLTableCaptionElement>
  /* attribute "THEAD"            => html/tHead    :: <html-element>; */	// <HTMLTableSectionElement>
  /* attribute "TFOOT"            => html/tFoot    :: <html-element>; */	// <HTMLTableSectionElement>
  /* readonly attribute "ROWS"    => html/rows     :: <html-collection>; */
  /* readonly attribute "TBODIES" => html/tBodies  :: <html-collection>; */
  /* attribute "ALIGN"            => html/align    :: <DOM-string>; */
  /* attribute "BGCOLOR"          => html/bg-color :: <DOM-string>; */
  /* attribute "BORDER"           => html/border   :: <DOM-string>; */
  attribute "CELLPADDING"         => html/cell-padding :: <DOM-string>;
  attribute "CELL-SPACING"        => html/cell-spacing :: <DOM-string>;
  attribute "FRAME"               => html/frame    :: <DOM-string>;
  attribute "RULES"               => html/rules    :: <DOM-string>;
  attribute "SUMMARY"             => html/summary  :: <DOM-string>;
  /* attribute "WIDTH"            => html/width    :: <DOM-string>; */
end;

define sealed method create-tHead
    (table :: <html-element>) => (tHead :: <html-element>)
  assert(string-equal?(tag-name(table), "TABLE"),
	 "You can only call 'create-tHead' on a <TABLE>");
  //---*** Implement this
  create-element(owner-document(table), "SECTION")
end method create-tHead;

define sealed method delete-tHead
    (table :: <html-element>) => ()
  assert(string-equal?(tag-name(table), "TABLE"),
	 "You can only call 'delete-tHead' on a <TABLE>");
  //---*** Implement this
end method delete-tHead;

define sealed method create-tFoot
    (table :: <html-element>) => (tFoot :: <html-element>)
  assert(string-equal?(tag-name(table), "TABLE"),
	 "You can only call 'create-tFoot' on a <TABLE>");
  //---*** Implement this
  create-element(owner-document(table), "SECTION")
end method create-tFoot;

define sealed method delete-tFoot
    (table :: <html-element>) => ()
  assert(string-equal?(tag-name(table), "TABLE"),
	 "You can only call 'delete-tFoot' on a <TABLE>");
  //---*** Implement this
end method delete-tFoot;

define sealed method create-caption
    (table :: <html-element>) => (caption :: <html-element>)
  assert(string-equal?(tag-name(table), "TABLE"),
	 "You can only call 'create-caption' on a <TABLE>");
  //---*** Implement this
  create-element(owner-document(table), "CAPTION")
end method create-caption;

define sealed method delete-caption
    (table :: <html-element>) => ()
  assert(string-equal?(tag-name(table), "TABLE"),
	 "You can only call 'delete-caption' on a <TABLE>");
  //---*** Implement this
end method delete-caption;

define sealed method insert-row
    (table :: <html-element>, index :: <integer>) => (caption :: <html-element>)
  assert(string-equal?(tag-name(table), "TABLE")
	 | string-equal?(tag-name(table), "SECTION"),
	 "You can only call 'insert-row' on a <TABLE> or <SECTION>");
  //---*** Implement this
  create-element(owner-document(table), "TR")
end method insert-row;

define sealed method delete-row
    (table :: <html-element>, index :: <integer>) => ()
  assert(string-equal?(tag-name(table), "TABLE")
	 | string-equal?(tag-name(table), "SECTION"),
	 "You can only call 'delete-row' on a <TABLE> or <SECTION>");
  //---*** Implement this
end method delete-row;


//
// interface HTMLTableCaptionElement : HTMLElement {
//            attribute  DOMString            align;
// };
//

define html-element "CAPTION"
  /* attribute "ALIGN" => html/align :: <DOM-string>; */
end;


//
// interface HTMLTableColElement : HTMLElement {
//            attribute  DOMString            align;
//            attribute  DOMString            ch;
//            attribute  DOMString            chOff;
//            attribute  long                 span;
//            attribute  DOMString            vAlign;
//            attribute  DOMString            width;
// };
//

define html-element "TC"
  /* attribute "ALIGN"  => html/align  :: <DOM-string>; */
  /* attribute "CH"     => html/ch     :: <DOM-string>; */
  /* attribute "CHOFF"  => html/ch-off :: <DOM-string>; */
  attribute "SPAN"      => html/span   :: <integer>;
  /* attribute "VALIGN" => html/valign :: <DOM-string>; */
  /* attribute "WIDTH"  => html/width  :: <DOM-string>; */
end;


//
// interface HTMLTableSectionElement : HTMLElement {
//            attribute  DOMString            align;
//            attribute  DOMString            ch;
//            attribute  DOMString            chOff;
//            attribute  DOMString            vAlign;
//   readonly attribute  HTMLCollection       rows;
//   HTMLElement               insertRow(in long index);
//   void                      deleteRow(in long index);
// };
//

//---*** Do 'html/rows'
define html-element "SECTION"
  /* attribute "ALIGN"         => html/align  :: <DOM-string>; */
  /* attribute "CH"            => html/ch     :: <DOM-string>; */
  /* attribute "CHOFF"         => html/ch-off :: <DOM-string>; */
  /* attribute "VALIGN"        => html/valign :: <DOM-string>; */
  /* readonly attribute "ROWS" => html/rows   :: <html-collection>; */
end;


//
// interface HTMLTableRowElement : HTMLElement {
//            attribute  long                 rowIndex;
//            attribute  long                 sectionRowIndex;
//            attribute  HTMLCollection       cells;
//            attribute  DOMString            align;
//            attribute  DOMString            bgColor;
//            attribute  DOMString            ch;
//            attribute  DOMString            chOff;
//            attribute  DOMString            vAlign;
//   HTMLElement               insertCell(in long index);
//   void                      deleteCell(in long index);
// };
//

//---*** Do 'html/cells'
define html-element "TR"
  attribute "ROWINDEX"          => html/row-index         :: <integer>;
  attribute "SECTIONROWINDEX"   => html/section-row-index :: <integer>;
  /* readonly attribute "CELLS" => html/cells             :: <html-collection>; */
  /* attribute "ALIGN"          => html/align             :: <DOM-string>; */
  /* attribute "BG-COLOR"       => html/bg-color          :: <DOM-string>; */
  /* attribute "CH"             => html/ch                :: <DOM-string>; */
  /* attribute "CHOFF"          => html/ch-off            :: <DOM-string>; */
  /* attribute "VALIGN"         => html/valign            :: <DOM-string>; */
end;

define sealed method insert-cell
    (tr :: <html-element>, index :: <integer>) => (caption :: <html-element>)
  assert(string-equal?(tag-name(tr), "TR"),
	 "You can only call 'insert-cell' on a <TR>");
  //---*** Implement this
  create-element(owner-document(tr), "TD")
end method insert-cell;

define sealed method delete-cell
    (tr :: <html-element>, index :: <integer>) => ()
  assert(string-equal?(tag-name(tr), "TR"),
	 "You can only call 'delete-cell' on a <TR>");
  //---*** Implement this
end method delete-cell;


//
// interface HTMLTableCellElement : HTMLElement {
//            attribute  long                 cellIndex;
//            attribute  DOMString            abbr;
//            attribute  DOMString            align;
//            attribute  DOMString            axis;
//            attribute  DOMString            bgColor;
//            attribute  DOMString            ch;
//            attribute  DOMString            chOff;
//            attribute  long                 colSpan;
//            attribute  DOMString            headers;
//            attribute  DOMString            height;
//            attribute  boolean              noWrap;
//            attribute  long                 rowSpan;
//            attribute  DOMString            scope;
//            attribute  DOMString            vAlign;
//            attribute  DOMString            width;
// };
//

define html-element "TD"
  attribute "CELLINDEX"  => html/cell-index :: <integer>;
  attribute "ABBR"       => html/abbr       :: <DOM-string>;
  /* attribute "ALIGN"   => html/align      :: <DOM-string>; */
  attribute "AXIS"       => html/axis       :: <DOM-string>;
  /* attribute "BGCOLOR" => html/bg-color   :: <DOM-string>; */
  /* attribute "CH"      => html/ch         :: <DOM-string>; */
  /* attribute "CHOFF"   => html/ch-off     :: <DOM-string>; */
  attribute "COLSPAN"    => html/col-span   :: <integer>;
  attribute "HEADERS"    => html/headers    :: <DOM-string>;
  /* attribute "HEIGHT"  => html/height     :: <DOM-string>; */
  attribute "NOWRAP"     => html/no-wrap?   :: <boolean>;
  attribute "ROWSPAN"    => html/row-span   :: <integer>;
  attribute "SCOPE"      => html/scope      :: <DOM-string>;
  /* attribute "VALIGN"  => html/valign     :: <DOM-string>; */
  /* attribute "WIDTH"   => html/width      :: <DOM-string>; */
end;


//
// interface HTMLFrameSetElement : HTMLElement {
//            attribute  DOMString            cols;
//            attribute  DOMString            rows;
// };
//

define html-element "FRAMESET"
  attribute "COLS" => html/cols :: <DOM-string>;
  attribute "ROWS" => html/rows :: <DOM-string>;
end;


//
// interface HTMLFrameElement : HTMLElement {
//            attribute  DOMString            frameBorder;
//            attribute  DOMString            longDesc;
//            attribute  DOMString            marginHeight;
//            attribute  DOMString            marginWidth;
//            attribute  DOMString            name;
//            attribute  boolean              noResize;
//            attribute  DOMString            scrolling;
//            attribute  DOMString            src;
// };
//

define html-element "FRAME"
  /* attribute "FRAMEBORDER"  => html/frame-border  :: <DOM-string>; */
  /* attribute "LONGDESC"     => html/long-desc     :: <DOM-string>; */
  /* attribute "MARGINHEIGHT" => html/margin-height :: <DOM-string>; */
  /* attribute "MARGINWIDTH"  => html/margin-width  :: <DOM-string>; */
  /* attribute "NAME"         => html/name          :: <DOM-string>; */
  attribute "NORESIZE"        => html/no-resize?    :: <boolean>;
  /* attribute "SCROLLING"    => html/scrolling     :: <DOM-string>; */
  /* attribute "SRC"          => html/src           :: <DOM-string>; */
end;


//
// interface HTMLIFrameElement : HTMLElement {
//            attribute  DOMString            align;
//            attribute  DOMString            frameBorder;
//            attribute  DOMString            height;
//            attribute  DOMString            longDesc;
//            attribute  DOMString            marginHeight;
//            attribute  DOMString            marginWidth;
//            attribute  DOMString            name;
//            attribute  DOMString            scrolling;
//            attribute  DOMString            src;
//            attribute  DOMString            width;
// };
//

define html-element "IFRAME"
  /* attribute "ALIGN"        => html/align         :: <DOM-string>; */
  /* attribute "FRAMEBORDER"  => html/frame-border  :: <DOM-string>; */
  /* attribute "HEIGHT"       => html/height        :: <DOM-string>; */
  /* attribute "LONGDESC"     => html/long-desc     :: <DOM-string>; */
  /* attribute "MARGINHEIGHT" => html/margin-height :: <DOM-string>; */
  /* attribute "MARGINWIDTH"  => html/margin-width  :: <DOM-string>; */
  /* attribute "NAME"         => html/name          :: <DOM-string>; */
  /* attribute "SCROLLING"    => html/scrolling     :: <DOM-string>; */
  /* attribute "SRC"          => html/src           :: <DOM-string>; */
  /* attribute "WIDTH"        => html/width         :: <DOM-string>; */
end;
