module:   sealing-workbench
author:   Paul Haahr
synopsis: Sample or interesting uses of sealing-world.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
sealing-world
  library l0
    open-class <a> (&<object>);
  library l1
    sealed-class <b> (<a>),
    open-generic f(&<class>);
  dispatch
    f(<a>);
    f(<b>)
end sealing-world;

sealing-world
  library l1
    open-generic gf (&<number>, &<number>),
    add-method gf m1 (&<real>, &<real>),
    add-method gf m2 (&<integer>, &<integer>),
    seal-generic gf (&<rational>, &<rational>);
  dispatch
    gf(1, 2)
end sealing-world;

sealing-world
  library DIRM-page-57
    open-class <life-form> (&<object>),
    open-class <sentient> (<life-form>),
    open-class <bipedal> (<life-form>),
    open-class <intelligent> (<sentient>),
    open-class <humanoid> (<bipedal>),
    open-class <vulcan> (<intelligent>, <humanoid>),
    open-class <human> (<humanoid>, <intelligent>),
    sealed-generic superior-being (<life-form>, <life-form>),
    add-method superior-being most-intelligent
                              (<intelligent>, <intelligent>),
    add-method superior-being best-looking
                              (<humanoid>, <humanoid>),
    object spock :: <vulcan>,
    object mccoy :: <human>;
  dispatch
    superior-being(spock, mccoy)
end sealing-world;

sealing-world
  library l1
    open-class <a> (&<object>),
    open-class <b> (&<object>),
    sealed-generic f (&<object>),
    add-method f fa (<a>),
    add-method f fb (<b>),
    seal-generic f (<a>),
    object a :: <a>;
  dispatch
    f(a)
end sealing-world;

sealing-world
  library l1
    open-class <a> (&<object>),
    open-class <b> (&<object>),
    open-generic f (&<object>),
    add-method f fa (<a>) calls-next-method,
    add-method f fb (<b>),
    seal-generic f (<a>),
    object a :: <a>;
  library l2
    open-class <c> (<a>),
    // open-class <d> (<c>, <b>),  // sealing violation
    object c :: <c>;
  dispatch
    f(a);
    f(c)
end sealing-world;

sealing-world
  library Ducournau-Habib-Huchard-Mugnier-OOPSLA-94
    sealed-class <boat> (&<object>),
    sealed-class <day-boat> (<boat>),
    sealed-class <wheel-boat> (<boat>),
    sealed-class <engine-less> (<day-boat>),
    sealed-class <pedal-wheel-boat> (<engine-less>, <wheel-boat>),
    sealed-class <small-multihull> (<day-boat>),
    sealed-class <small-catamaran> (<small-multihull>),
    sealed-class <pedalo> (<pedal-wheel-boat>, <small-catamaran>),
    sealed-generic nav-zone (<boat>),
    add-method nav-zone m5 (<day-boat>),
    add-method nav-zone m100 (<wheel-boat>),
    object pedalo :: <pedalo>;
  dispatch
    nav-zone(pedalo)
end sealing-world;

sealing-world
  library L*Loops-vs-CLOS
    sealed-class <a> (&<object>),
    sealed-class <b> (&<object>),
    sealed-class <c> (<a>),
    sealed-class <d> (<b>),
    sealed-class <e> (<c>, <d>),
    sealed-class <f> (<b>, <a>),
    sealed-class <g> (<e>, <f>);
  dispatch
end sealing-world;

// screw case:  add-method must create disjointnesses on classes
// note that this is order dependent

sealing-world
  library a-lib
    open-class <a> (&<object>),
    open-generic f (&<object>),
    add-method f fa (<a>),
    seal-generic f (<a>);
  library b-lib
    open-class <b> (&<object>),
    add-method f fb (<b>);
  library c-lib
    open-class <c> (<a>, <b>);  // sealing violation
  library user
    object a :: <a>;
  dispatch
    f(a)
end sealing-world;

sealing-world
  library a-lib
    open-class <a> (&<object>),
    open-generic f (&<object>),
    add-method f fa (<a>),
    seal-generic f (<a>);
  library b-lib
    open-class <b> (&<object>);
  library c-lib
    open-class <c> (<a>, <b>);
  library fb-lib
    add-method f fb (<b>);  // sealing violation
  library user
    object a :: <a>;
  dispatch
    f(a)
end sealing-world;

sealing-world
  library l0
    open-class <a> (&<object>),
    open-class <b> (&<object>),
    open-generic f (<a>, <b>),
    add-method f f0 (<a>, <b>);
  library l1
    open-class <a1> (<a>),
    open-class <b1> (<b>),
    add-method f f1 (<a1>, <b1>),
    seal-generic f (<a1>, <b1>);
  library l2
    open-class <a2> (<a>),
    open-class <b2> (<b>),
    add-method f f2 (<a2>, <b2>);  // requires <a2> disjoint from <a1>
    				   // or <b2> disjoint from <b1>
  // library l3
  //  open-class <b3> (<b1>, <b2>);
  library user
    object a :: <a1>,
    object b :: <b1>;
  dispatch
    f(a, b)
end sealing-world;

sealing-world
  library DIRM-rule-3-bug
    open-generic G (&<object>),
    open-class <Ti> (&<object>),
    open-class <Si> (&<object>),
    open-class <TiSi> (<Ti>, <Si>),
    add-method G G_Si (<Si>),
    seal-generic G(<Ti>);
  library sealing-violation
    sealed-class <Dk> (<Ti>),
    sealed-class <Dj> (<Si>),
    sealed-class <C> (<Dj>, <Dk>),
    object c :: <C>;
  dispatch
    g(c)
end sealing-world;

*/
