package server.sequence;

import java.util.Vector;
import org.omg.CORBA.SystemException;

import shared.Structure;
import shared.TestObject;
import shared._tie_TestObject;
import shared.SequenceTestPackage.*;
import shared._SequenceTestOperations;

/**
 * The SequenceTest implementation class.
 */
class SequenceTestImplementation implements _SequenceTestOperations { 
    public Vector SequenceTest_implementation_objects = new Vector();
    public TestObject[] object_seq = {};
    public Structure[] struct_seq = {};
    public short[] short_seq = {};

    public SequenceTestImplementation () throws SystemException{
      super();
    };
    
    public short[] attribute_short_seq() {
           return short_seq;
    };
    

    public void attribute_short_seq(short[] value) {
           short_seq = value;
           return;
    };
    

    public void check_attribute_short_seq() throws failure {
       short[] seq = attribute_short_seq();
       for(int i = 0; i < seq.length; i++) {
           int expected = i * 11;
           if (seq[i] != expected) throw new failure((short) i);
       }; 
       return;
    };


    public void in_parameter_short_seq(short[] inseq) throws failure {
        short[] seq = attribute_short_seq();
        for(int i = 0; i < seq.length; i++) {
           if (seq[i] != inseq[i]) throw new failure((short) i);
        };
        return;
    };

    public void inout_parameter_short_seq(sequence_shortHolder inseq) throws failure {
        short[] seq = attribute_short_seq();
        for(int i = 0; i < seq.length; i++) {
           if (seq[i] != (inseq.value)[i]) throw new failure((short) i);
        };
        inseq.value = seq;
        return;
    };

    public void out_parameter_short_seq(sequence_shortHolder seq) {
       seq.value = attribute_short_seq();
       return;
    };


    public short[] result_short_seq() {
       return attribute_short_seq();
    };

    public Structure[] attribute_struct_seq() {
       return struct_seq;
    };

    public void attribute_struct_seq(Structure[] value) {
       struct_seq = value; 
       return; 
    };

    public boolean eq(Structure a,Structure b) {
       return (a.name.equals(b.name)) && (a.info == a.info);
    };
    String[] numbers = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"};

    public void check_attribute_struct_seq()  throws failure {
       Structure[] seq = attribute_struct_seq();
       for(int i = 0; i < seq.length; i++) {
           Structure expected = new Structure(numbers[i], (byte) i);
           if (!eq(seq[i],expected)) throw new failure((short) i);
       }; 
       return;
    };


    public void in_parameter_struct_seq(Structure[] inseq) throws failure {
        Structure[] seq = attribute_struct_seq();
        for(int i = 0; i < seq.length; i++) {
           if (!eq(seq[i],inseq[i])) throw new failure((short) i);
        };
        return;
    };


    public void inout_parameter_struct_seq(sequence_StructureHolder inseq) 
           throws failure {
        Structure[] seq = attribute_struct_seq();
        for(int i = 0; i < seq.length; i++) {
           if (!(eq(seq[i],(inseq.value)[i]))) throw new failure((short) i);
        };
        inseq.value = seq;
        return;
    };

    public void out_parameter_struct_seq(sequence_StructureHolder seq) {
       seq.value = attribute_struct_seq();
       return;
    };

    public Structure[] result_struct_seq(){
       return attribute_struct_seq();
    };

    public TestObject TestObject_factory(int id) {
       TestObjectImplementation  object = new TestObjectImplementation(id);
       TestObject reference = new _tie_TestObject(object);
       org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
       object._ior = orb.object_to_string(reference);
       SequenceTest_implementation_objects.addElement(object);
       return reference;
    };

    public TestObject[] attribute_object_seq() {
       return object_seq;
    };  

    public void attribute_object_seq(TestObject[] value)    {
       object_seq = value;
       return;
    };

    public String ior_string(TestObject reference){
       org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
       return orb.object_to_string(reference);
    };

    public String ior_string(TestObjectImplementation object) {
       return object._ior;
    };
    public boolean eq(TestObject reference, TestObjectImplementation object) {
       return (ior_string(reference)).equals(ior_string(object));
    };

    public boolean eq(TestObject reference, TestObject referenceA) {
       return (ior_string(reference)).equals(ior_string(referenceA));
    };

    public void check_attribute_object_seq() 
        throws failure {
       TestObject[] seq = attribute_object_seq();
       for(int i = 0; i < seq.length; i++) {
           TestObjectImplementation expected = (TestObjectImplementation) SequenceTest_implementation_objects.elementAt(i);
           if (!eq(seq[i],expected)) throw new failure((short) i);
       }; 
       return;
    };

    public void in_parameter_object_seq(TestObject[] inseq) 
        throws failure    {
        TestObject[] seq = attribute_object_seq();
        for(int i = 0; i < seq.length; i++) {
           if (!eq(seq[i],inseq[i])) throw new failure((short) i);
        };
        return;
    };

    public void inout_parameter_object_seq(sequence_TestObjectHolder inoutseq) 
        throws failure    {
        TestObject[] seq = attribute_object_seq();
        for(int i = 0; i < seq.length; i++) {
           if (!(eq(seq[i],(inoutseq.value)[i]))) throw new failure((short) i);
        };
        inoutseq.value = seq;
        return;
    };

    public void out_parameter_object_seq(sequence_TestObjectHolder seq) {
       seq.value = attribute_object_seq();
       return;
    };

    public TestObject[] result_object_seq()     {
       return attribute_object_seq();
    };

    public char[] short_name = {'B', 'a', 't', 'm', 'a', 'n', ' ', 'a', 'n', 'd', ' ', 'R', 'o', 'b', 'i', 'n'};
    public char[] long_name = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};

    public void set_short_name(char[] name) throws failure {
        for(int c = 0, i = 0; (c < name.length && i < short_name.length); c++,i++) {
             if (name[c] != short_name[i]) throw new failure((short) i);
        };
    };

    public void set_long_name(char[] name) {
        return;
    };
   
    public char[] get_short_name() {
        return short_name;
    };

    public char[] get_long_name() {
        return long_name;
    };

    public short get_name(BoundedStringHolder name) {
        name.value = short_name;
        return (short) short_name.length;
    };

    public void reverse_name(BoundedStringHolder name) {
        char[] temp = new char[name.value.length];
        for(int i = 0, j = name.value.length - 1; i < name.value.length; i++,j--){
            temp[j] = name.value[i];
        };
        name.value = temp;
        return;
    };

};
