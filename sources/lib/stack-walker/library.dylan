module: dylan-user

define library stack-walker
  use common-dylan;
  use c-ffi;

  export stack-walker;
end;

define module stack-walker
  use common-dylan;
  use c-ffi;

  export walk-stack;
end;
