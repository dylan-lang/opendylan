Module:       web-browser
Author:       Andy Armstrong
Synopsis:     DUIM web browser
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <web-page>

define class <web-page> (<object>)
  slot page-location :: <string>, required-init-keyword: location:;
  slot page-title :: <string> = "Untitled", init-keyword: title:;
end class;


/// stream reading protocol

define generic read-web-page (location) => (page :: <web-page>);


/// web-relative-location

define method web-page-directory (page :: <web-page>)
  let location = page-location(page);
  web-page-directory(location)
end method;

define method web-page-directory (location :: <string>)
  block (return)
    for (position from size(location) - 1 to 0 by -1)
      if (location[position] = '/')
        return(copy-sequence(location, end: position + 1))
      end
    end
  end
end method;

define method web-relative-location 
    (new-location :: <string>, page :: <web-page>)
  case
    http-address?(new-location) =>
      new-location;
    new-location[0] = '/' =>
      let web-site = web-site(page);
      concatenate(web-site | "", new-location);
    otherwise =>
      concatenate(web-page-directory(page), new-location);
  end
end method;

define method http-address? (location :: <string>)
  subsequence-position(location, "http:") = 0
end method;

define method web-site (page :: <web-page>)
  let location = page-location(page);
  web-site(location)
end method;

define method web-site (location :: <string>)
  if (http-address?(location))
    let start = if (location[5] = '/' & location[6] = '/') 7 else 5 end;
    let end-pos = for (i from start below size(location),
                       until: location[i] = '/')
                    finally i
                  end;
    copy-sequence(location, end: end-pos)
  end
end method;
