METHOD optional-arguments-tests (#next next-method) => (#rest results)
  [CALLi ^{<&method> rest-function ()}(^#[])]
  [CALLi ^{<&method> rest-function ()}(^#[])]
  [CALLi ^{<&method> key-function ()}(^#[], ^10, ^11)]
  [CALLi ^{<&method> key-function ()}(^#[], ^1, ^11)]
  [CALLi ^{<&method> key-function ()}(^#[], ^10, ^2)]
  [CALLi ^{<&method> key-function ()}(^#[], ^1, ^2)]
  [CALLi ^{<&method> key-function ()}(^#[], ^1, ^2)]
  [CALLi ^{<&method> rest-key-function ()}(^#[], ^10, ^11)]
  [CALLi ^{<&method> rest-key-function ()}(^#[], ^1, ^11)]
  [CALLi ^{<&method> rest-key-function ()}(^#[], ^10, ^2)]
  [CALLi ^{<&method> rest-key-function ()}(^#[], ^1, ^2)]
  [CALLi ^{<&method> rest-key-function ()}(^#[], ^1, ^2)]
  [CALLi ^{<&method> required+rest-function (<object>, <object>)}(^"1", ^"2", ^#[])]
  [CALLi ^{<&method> required+rest-function (<object>, <object>)}(^"1", ^"2", ^#[])]
  [CALLi ^{<&method> required+key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^10, ^11)]
  [CALLi ^{<&method> required+key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^1, ^11)]
  [CALLi ^{<&method> required+key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^10, ^2)]
  [CALLi ^{<&method> required+key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^1, ^2)]
  [CALLi ^{<&method> required+key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^1, ^2)]
  [CALLi ^{<&method> required+rest-key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^10, ^11)]
  [CALLi ^{<&method> required+rest-key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^1, ^11)]
  [CALLi ^{<&method> required+rest-key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^10, ^2)]
  [CALLi ^{<&method> required+rest-key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^1, ^2)]
  *t2(0,#rest) := [CALLi ^{<&method> required+rest-key-function (<object>, <object>)}(^"1", ^"2", ^#[], ^1, ^2)] // tail call
  return *t2(0,#rest)
END

METHOD rest-function (#rest rest) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

METHOD key-function (#key ...) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

METHOD rest-key-function (#key ...) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

METHOD required+rest-function (x, y, #rest rest) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

METHOD required+key-function (x, y, #key ...) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

METHOD required+rest-key-function (x, y, #key ...) => (#rest results)
  *t2(1) := [VALUES ^#f]
  return *t2(1)
END

