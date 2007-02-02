module: unix-portability
author: Hannes Mehnert <hannes@mehnert.org>

define function unix-lseek (fd :: <integer>, position :: <integer>, mode :: <integer>)
 => (position :: <integer>)
  raw-as-integer
    (%call-c-function ("lseek")
       (fd :: <raw-c-unsigned-int>, position :: <raw-c-unsigned-long>, 
        mode :: <raw-c-unsigned-int>) 
       => (result :: <raw-c-signed-int>)
       (integer-as-raw(fd), integer-as-raw(position), integer-as-raw(mode))
     end)
end;



define function unix-errno () => (res :: <integer>)
 raw-as-integer(%call-c-function ("myerrno") () => (errnop :: <raw-c-signed-int>) () end)
end;

define constant $proc-path = "exe";
