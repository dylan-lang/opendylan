package server.pseudoobjects;

import shared._TestObjectDOperations;

class  TestObjectDImplementation implements  _TestObjectDOperations {
    public TestObjectDImplementation (int id) {
       _id = id;
    };

    org.omg.CORBA.Object _obj = null;

    int _id = 0;
    public int id() {
      return _id;
    };
    public void id(int value){
       _id = value;
       return;
    };
    String _ior;
    public String ior() {
       return _ior;
    };
    public void ior(String value) {
      _ior = value;
      return;
    };
    public void destroy() {
      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
      orb.disconnect(_obj);
      return;
    };
};
