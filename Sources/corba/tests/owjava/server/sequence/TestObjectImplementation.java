package server.sequence;

import org.omg.CORBA.SystemException;

import shared._TestObjectOperations;

/**
 * The TestObject implementation class.
 */
class TestObjectImplementation implements _TestObjectOperations { 
    public int _id;
    public String _ior;
    public TestObjectImplementation (int id) throws SystemException{
      super();
      _id = id;
      _ior = "";
    };

    public int id() {
       return _id;
    };

    public void id(int value) {
       _id = value; 
       return;
    };

    public String ior() {
       return _ior;
    };

    public void ior(String value) {
       _ior = value;
       return;
    };
};
