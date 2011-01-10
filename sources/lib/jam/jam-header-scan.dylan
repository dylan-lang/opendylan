Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method target-header-scan
    (jam :: <jam-state>, target :: <jam-target>)
 => ();
  let hdrscan
    = element(target.target-variables, "HDRSCAN", default: #f)
    | jam-variable(jam, "HDRSCAN");
  let hdrrule
    = element(target.target-variables, "HDRRULE", default: #f)
    | jam-variable(jam, "HDRRULE");
  
  if (~hdrscan.empty? & ~hdrrule.empty?)
    let regexp = parse-regular-expression(hdrscan[0]);

    with-open-file(stream = target.target-bound-locator, direction: #"input")
      iterate loop (buf :: false-or(<buffer>) = get-input-buffer(stream))
        if (buf)
          
          buf.buffer-next := buf.buffer-end;
          loop(next-input-buffer(stream));
        end if;
      end iterate;
      release-input-buffer(stream);
    end with-open-file;
  end if;
end method;
