module: dylan-user
author: Hannes Mehnert <hannes@mehnert.org>

define library unix-portability
  use dylan;

  export unix-portability;
end;

define module unix-portability
  use dylan;
  use dylan-direct-c-ffi;

  export unix-errno, $proc-path, unix-lseek;
end;