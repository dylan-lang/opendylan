Module: dylan-test



define open class <simple-class-to-redefine> (<object>)
  slot sctr-slot-1 :: <integer> = 259, init-keyword: slot-1:;
  slot sctr-slot-2 :: <symbol> = #"foo", init-keyword: slot-2:;
end class;

define open generic sctr-slot-1 (x);
define open generic sctr-slot-1-setter (v, x);
define open generic sctr-slot-2 (x);
define open generic sctr-slot-2-setter (v, x);
define open generic sctr-slot-3 (x);
define open generic sctr-slot-3-setter (v, x);


  
define method tautology (k == #"simple-structural-redefinition")
  let <simple-class-to-redefine> = make(<class>,
					debug-name: "<simple-structural-redefinition-1>",
					superclasses: vector(<object>),
					slots: vector(vector(getter: sctr-slot-1, 
							     setter: sctr-slot-1-setter,
							     type: <integer>,
							     init-value: 259,
							     init-keyword: slot-1:),
						      vector(getter: sctr-slot-2, 
							     setter: sctr-slot-2-setter,
							     type: <symbol>,
							     init-value: #"foo",
							     init-keyword: slot-2:)));
  local method check-the-type-dynamically (i, cls, name)
	  if (~instance?(i, cls))
	    signal("%s - %= was not %=", name, i, cls)
	  end if
	end method;
  local method check-not-the-type-dynamically (i, cls, name)
	  if (instance?(i, cls))
	    signal("%s - %= was not supposed to be %=", name, i, cls)
	  end if
	end method;
  local method check-a-slot-dynamically (i, acc, expected, name)
	  let v = acc(i);
	  if (v ~== expected)
	    signal("%s - %= of %= was expected to be %= but was %=", name, acc, i, expected, v)
	  end if
	end method;
  let i1 = make(<simple-class-to-redefine>);
  let i2 = make(<simple-class-to-redefine>, slot-1: 50, slot-2: #"quux");
  check-the-type-dynamically(i1, <simple-class-to-redefine>, "initial i1 instancep");
  check-the-type-dynamically(i1, <simple-class-to-redefine>, "initial i2 instancep");
  check-a-slot-dynamically(i1, sctr-slot-1, 259, "initial i1 slot 1");
  check-a-slot-dynamically(i1, sctr-slot-2, #"foo", "initial i1 slot 2");
  check-a-slot-dynamically(i2, sctr-slot-1, 50, "initial i2 slot 1");
  check-a-slot-dynamically(i2, sctr-slot-2, #"quux", "initial i2 slot 2");
  
  %redefine-class(<simple-class-to-redefine>,
		  superclasses: vector(<object>),
		  slots: vector(vector(getter: sctr-slot-3, 
				       setter: sctr-slot-3-setter,
				       type: <class>,
				       init-value: <object>,
				       init-keyword: slot-3:)));

  let i3 = make(<simple-class-to-redefine>);
  let i4 = make(<simple-class-to-redefine>, slot-3: <integer>);
  
  check-the-type-dynamically(i3, <simple-class-to-redefine>, "redefined i3 instancep");
  check-the-type-dynamically(i4, <simple-class-to-redefine>, "redefined i4 instancep");
  check-a-slot-dynamically(i3, sctr-slot-3, <object>, "redefined i3 slot 3");
  check-a-slot-dynamically(i4, sctr-slot-3, <integer>, "redefined i4 slot 3");
  check-not-the-type-dynamically(i1, <simple-class-to-redefine>, "redefined i1 instancep");
  check-not-the-type-dynamically(i2, <simple-class-to-redefine>, "redefined i2 instancep");
end method;


//define method tautology (t == #"showoff")
//  show(has-instances?)
//end method;



tautologies := concatenate(tautologies, #(#"simple-structural-redefinition"
					    // , #"showoff"
					    ));
