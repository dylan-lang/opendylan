Module: url-internals
Synopsis: Provide an interface to URLs
Synopsis: Loosely based up libwww-perl-5 implementation
Author: James Casey, January 1997
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// <url> is the superclass of all url scheme classes. Each scheme specific 
// class understands how to decode the scheme specific part of the URL
//  
define open  class <url> (<object>)
  // Every URL has a scheme
  slot url-scheme :: false-or(<byte-string>), init-value: #f;

  // The (optional) base for a relative URL
  slot url-base :: false-or(<byte-string>);
end class <url>;

// This creates a subclass of <url> thats knows how to parse the scheme 
//  specific part
define method make(this == <url>, #rest initargs,
		   #key url :: false-or(<byte-string>))
 =>(retval :: <url>)

  let this-url :: false-or(<byte-string>) = #f;
  let this-specific = "";
  let this-scheme = #f;
  let this-class :: <class> = <url>;
  
  if (url)
    // We should be leniant about whitespace
    this-url := url-trim-whitespace(url);
    
    let (scheme, specific) = extract-scheme!(this-url);
    this-scheme := scheme;
    this-specific := specific;

    if (this-scheme)
      this-class := url-scheme-as-class(this-scheme);
    else // No scheme
      this-class := <generic-url>; 
    end;
  end;

  let new-url :: false-or(<url>) = #f;
  if(this-class ~= <url>)
    new-url := apply(make, this-class, initargs);
  else
    signal(make(<malformed-url-error>,
		format-string:"Invalid scheme: %s",
		format-arguments: this-scheme));
  end;
  parse-url-scheme-specific(new-url, this-specific);
  new-url
end method make;

define generic url-default-port(this :: <url>)
 => (retval :: false-or(<integer>));

define method url-default-port(this :: <url>)
 => (retval :: false-or(<integer>))
  #f
end;

// Parse a scheme specific string into the given partially filled url
define generic parse-url-scheme-specific(this :: <url>, 
					 spec :: <byte-string>)
 =>();

define function url-trim-whitespace(this-url :: <byte-string>)
 =>(retval :: <byte-string>)

  let pos = 0;
  if(this-url.size > 0)
    while (pos < this-url.size & element(this-url, pos) = ' ')
      pos := pos + 1;
    end while;
    if (pos > 0)
      this-url := copy-sequence(this-url, start: pos);
    end;
  end;

  // skip any at the end

  if(this-url.size > 0)
    let end-pos = this-url.size - 1;
    pos := end-pos;
    while(element(this-url, pos) = ' ')
      pos := pos - 1;
    end while;
    if (pos < end-pos)
      this-url := copy-sequence(this-url, end: pos + 1)
    end if;
  end;
  this-url
end function url-trim-whitespace;

define function url-valid-scheme?(str :: <byte-string>, 
			       colon :: <integer>) 
 => (ret :: <boolean>)
  let p = 0;
  block (break)
    while ( p < colon)
      element(str, p) = '/' & break(#f);
      element(str,p) = '#' & break(#f);
      element(str,p) = '?' & break(#f);
      p := p + 1;
    end while;
    #t
  end block;
end function url-valid--scheme?;

define function extract-scheme!(url :: <byte-string>)
 =>( scheme :: false-or(<byte-string>), specific :: <byte-string>)
  
  let pos :: false-or(<integer>) = #f;
  let scheme :: false-or(<byte-string>) = #f;
  let specific :: <byte-string> = "";
  
  // Ignore IETF URL: prefix
  // XXX Can't do
  //  if((pos := subsequence-position(url,"url:")) = 0 |
  //     (pos := subsequence-position(url,"URL:")) = 0)
  if(url.size >= 4)
    let foo = copy-sequence(url,  end: 4);
    as-lowercase!(foo);
    if(foo = "url:")
      url := copy-sequence(url, start: 4);
    end;
  end;

  // Extract scheme, if it exists
  pos := subsequence-position(url,":");
  // XXX Can't use
  // if ((pos ~= #f) 
  //	 & proper-scheme?(url, pos))
  if(pos)
    if(url-valid-scheme?(url , pos))
      scheme := as-lowercase!(copy-sequence(url, end: pos ));
      specific := copy-sequence(url, start: pos + 1 );
    else
      specific := copy-sequence(url);
    end;
  else // Relative URL
    specific := copy-sequence(url);
  end if;

  values(scheme, specific)
end function extract-scheme!;

// These generics define a way to automatically map to a class which
// understands this scheme. We convert to a <symbol>, and dispatch on that
// 
// Subclasses need to provide a method for url-scheme-symbol-as-class
// Only url-scheme-as-class is externally exported
//
define generic url-scheme-as-class(scheme :: <byte-string>)
 =>(class :: <class>);

define generic url-scheme-symbol-as-class(scheme :: <symbol>)
 =>(class :: <class>);

define generic url-string(this :: <url>)
 => (retval :: <byte-string>);

// 
// Check if a url is relative. By default this is if there a scheme defined
// Can override to always return false for shceme which do not support 
// relative urls
//
define generic url-is-relative?(this :: <url>)
 => (retval :: <boolean>); 


// If no scheme is defined, return <url>
define method url-scheme-symbol-as-class(this :: <symbol>)
 =>(class :: <class>)
  <url>
end method url-scheme-symbol-as-class;

define method url-scheme-as-class(scheme :: <byte-string>)
 =>(class :: <class>)
  url-scheme-symbol-as-class(as(<symbol>,scheme))
end method url-scheme-as-class;

define method url-is-relative?(this :: <url>)
 =>(retval :: <boolean>)
  if(this.url-scheme) #t else #f end
end method url-is-relative?;

define class <malformed-url-error> (<error>)
end class <malformed-url-error>;
