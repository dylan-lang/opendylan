package server.array;

import org.omg.CORBA.SystemException;

import shared._ArrayTestOperations;
import shared.ArrayTestPackage.*;

/**
 * The ArrayTest implementation class.
 */
class ArrayTestImplementation implements _ArrayTestOperations { 
   
    public ArrayTestImplementation () throws SystemException{
      super();
    };

    String[] _string_array;

    public String[] string_array_attribute() {
      return _string_array;
    };

    public void string_array_attribute(String[] value) {
       _string_array = value;
       return;
    };

    short[][] _short_array;

    public short[][] short_array_attribute() {
       return _short_array;
    };

    public void short_array_attribute(short[][] value){
       _short_array = value;
       return;
    };

    float[][][] _float_array;

    public float[][][] float_array_attribute() {
       return _float_array;
    };

    public void float_array_attribute(float[][][] value){
       _float_array = value;
       return;
    };

    public static String[] messages = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};

    public void check_string_array_attribute() throws failure {       
       for(int i = 0; i < messages.length; i++) {
           if (!(_string_array[i].equals(messages[i]))) throw new failure();
       }; 
       return;
    };

   public void check_short_array_attribute() throws failure {
      int counter = 0;
      for(int i = 0; i < 4; i++) {
        for(int j = 0; j < 4; j++) {
           if (_short_array[i][j]!=counter) throw new failure();
           counter = counter+1;
	};
      }; 
      return;       
    };

  public void check_float_array_attribute() throws failure {
      float number = (float) 0.0;
      for(int i = 0; i < 3; i++) {
        for(int j = 0; j < 3; j++) {
	  for(int k = 0; k < 3; k++) {
           if (_float_array[i][j][k]!=number) throw new failure();
           number = number + (float) 0.7;
	  };
	};
      }; 
      return;       
   };


   public String[] string_array_operation(String[] one,shared.StringArrayHolder two,shared.StringArrayHolder three) {
      two.value = two.value;
      three.value = one;
      return one;
  };

   public short[][] short_array_operation(short[][] one,shared.ShortArrayHolder two,shared.ShortArrayHolder three) {
      two.value = two.value;
      three.value = one;
      return one;
   };

   public float[][][] float_array_operation(float[][][] one,shared.FloatArrayHolder two,shared.FloatArrayHolder three) {
      two.value = two.value;
      three.value = one;
      return one;
   };
};
