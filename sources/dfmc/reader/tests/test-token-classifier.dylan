Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define sideways method classify-word-in (context, word)
  select (word)
    #"x" => $unreserved-name-token;
    #"begin" => $begin-word-only-token;
    #"constant" => $define-list-word-only-token;
    otherwise => #f;
  end select
end;

