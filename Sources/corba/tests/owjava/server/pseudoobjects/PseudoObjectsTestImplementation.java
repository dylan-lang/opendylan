package server.pseudoobjects;

import org.omg.CORBA.SystemException;

import shared._PseudoObjectsTestOperations;
import shared.PseudoObjectsTestPackage.*;
import shared.TestObjectA;
import shared.TestObjectB;
import shared.TestObjectC;
import shared.TestObjectD;
import shared.TestObjectX;
import shared._tie_TestObjectA;
import shared._tie_TestObjectB;
import shared._tie_TestObjectC;
import shared._tie_TestObjectD;
import shared._tie_TestObjectX;

class PseudoObjectsTestImplementation implements _PseudoObjectsTestOperations { 

    public PseudoObjectsTestImplementation () throws SystemException{
      super();
    };

    public TestObjectX TestObjectX_factory(int id) {
      TestObjectXImplementation obj = new TestObjectXImplementation ();
      _tie_TestObjectX ref = new _tie_TestObjectX(obj);
      return ref;
    };

    public TestObjectA TestObjectA_factory(int id) {
      TestObjectAImplementation obj = new TestObjectAImplementation (id);
      _tie_TestObjectA ref = new _tie_TestObjectA(obj);
      obj._obj = ref;
      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
      obj.ior(orb.object_to_string(ref));
      return ref;
    };

    public TestObjectB TestObjectB_factory(int id) {
      TestObjectBImplementation obj = new TestObjectBImplementation(id);
      _tie_TestObjectB ref = new _tie_TestObjectB(obj);
      obj._obj = ref;
      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
      obj.ior(orb.object_to_string(ref));
      return ref;
    };
 
    public TestObjectC TestObjectC_factory(int id) {
      TestObjectCImplementation obj = new TestObjectCImplementation(id);
      _tie_TestObjectC ref = new _tie_TestObjectC(obj);
      obj._obj = ref;
      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
      obj.ior(orb.object_to_string(ref));
      return ref;
    };
    public TestObjectD TestObjectD_factory(int id) {
      TestObjectDImplementation obj = new TestObjectDImplementation(id);
      _tie_TestObjectD ref = new _tie_TestObjectD(obj);
      obj._obj = ref;
      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
      obj.ior(orb.object_to_string(ref));
      return ref;
    };

    public TestObjectX TestObjectX_nil_factory() {
       return (TestObjectX) null;
    }; 

    public org.omg.CORBA.Object identity(org.omg.CORBA.Object x) {
      return x;
    };
     
    org.omg.CORBA.Object _object_attribute;

    public org.omg.CORBA.Object object_attribute() { 
      return _object_attribute;
    };

    public void object_attribute(org.omg.CORBA.Object value) {
      _object_attribute = value;
      return;
    };

    org.omg.CORBA.TypeCode _typecode_attribute;     
    public org.omg.CORBA.TypeCode typecode_attribute() {
       return _typecode_attribute;
    };

    public void typecode_attribute(org.omg.CORBA.TypeCode value) {
       _typecode_attribute = value;
       return;
    };

    public void check_object_attribute(String ior) throws failure {
       org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
       //if (!(orb.object_to_string(orb.string_to_object(ior))).equals(orb.object_to_string(object_attribute()))) {
       //    throw new failure (); } 

       if (!((orb.string_to_object(ior)))._is_equivalent(object_attribute())) {
             throw new failure (); } 
       return;
   };

    public void check_typecode_attribute() throws failure {
	 if (!(typecode_attribute().equal(failureHelper.type()))) {
   	     throw new failure ();} ;
    };

    public org.omg.CORBA.Object object_operation(org.omg.CORBA.Object one,org.omg.CORBA.ObjectHolder two,org.omg.CORBA.ObjectHolder three) {
          three.value = two.value;
          two.value = one;
          return one;
    };

    public org.omg.CORBA.TypeCode typecode_operation(org.omg.CORBA.TypeCode one,org.omg.CORBA.TypeCodeHolder two,org.omg.CORBA.TypeCodeHolder three) { 
          three.value = two.value;
          two.value = one;
          return one;
    };
};
