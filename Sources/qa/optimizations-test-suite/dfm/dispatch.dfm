METHOD dispatch-tests (#next next-method) => (#rest results)
  [METHOD-CALLi ^{<&method> static-function (<integer>)}(^1)]
  [METHOD-CALLi ^{<&method> static-function (<list>)}(^#())]
  [CALLo ^{<&generic> open-function}(^1)]
  *t2(0,#rest) := [CALLo ^{<&generic> open-function}(^#())] // tail call
  return *t2(0,#rest)
END

METHOD static-function (x :: <integer>, #next next-method) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

METHOD static-function (x :: <list>, #next next-method) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

METHOD open-function (x :: <integer>, #next next-method) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

METHOD open-function (x :: <list>, #next next-method) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

