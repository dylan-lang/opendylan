package server.struct;

import org.omg.CORBA.SystemException;

import shared.StructureA;
import shared.StructureB;
import shared.StructureC;
import shared.StructureD;
import shared._StructTestOperations;

/**
 * The StructTest implementation class.
 */
class StructTestImplementation implements _StructTestOperations { 

    public StructTestImplementation () throws SystemException{
      super();
    };
    public StructureA cstruct_a = new StructureA((int) -40000, (int) 40000, (byte) 128);
    public StructureB cstruct_b = new StructureB(false);
    public StructureC cstruct_c = new StructureC((String) "Tinky Winky", cstruct_b, 'A');
    public StructureD cstruct_d = new StructureD((float) 0.0f, (double) 1.0d);

    public boolean eq(StructureA a,StructureA b){
       return  (a.a_long == b.a_long) &&
               (a.a_ulong == b.a_ulong) &&
               (a.a_octet == b.a_octet);
    };
    public boolean eq(StructureB a,StructureB b){
       return  (a.b_boolean == b.b_boolean);
    };

    public boolean eq(StructureC a,StructureC b){
       return  (a.c_string).equals(b.c_string) &&
               eq(a.c_struct, b.c_struct) &&
               (a.c_char == b.c_char);
    };

    public boolean eq(StructureD a,StructureD b){
       return  (a.d_float == b.d_float) &&
               (a.d_double == b.d_double);
    };
    
    public StructureA struct_a() {
      return cstruct_a;
    };

    public void struct_a(StructureA value) {
      cstruct_a = value;
    };

    public StructureB struct_b() {
      return cstruct_b;
    };
   
    public void struct_b(StructureB value) {
      cstruct_b = value;
    };

    public StructureC struct_c() {
      return cstruct_c;
    };

    public void struct_c(StructureC value) {
      cstruct_c = value;
    };

    public StructureD struct_d() {
      return cstruct_d;
    };
    public void struct_d(StructureD value) {
      cstruct_d = value;
    };

    public int get_a_long() {
      return cstruct_a.a_long;
    };

    public int get_a_ulong() {
      return cstruct_a.a_ulong;
    };

    public byte get_a_octet(){
      return cstruct_a.a_octet;
    };

    public boolean get_b_boolean() {
      return cstruct_b.b_boolean;
    };

    public String get_c_string() {
      return cstruct_c.c_string;
    };

    public StructureB get_c_struct() {
      return cstruct_c.c_struct;
    };

    public char get_c_char() {
      return cstruct_c.c_char;
    };

    public float get_d_float(){
      return cstruct_d.d_float;
    };

    public double get_d_double() {
      return cstruct_d.d_double;
    };

    public boolean in_parameter_a(StructureA structure) {
      return  eq(structure,cstruct_a);
    };

    public boolean in_parameter_b(StructureB structure) {
      return eq(structure,cstruct_b);
    };


    public boolean in_parameter_c(StructureC structure) {
      return eq(structure,cstruct_c);
    };

    public boolean in_parameter_d(StructureD structure) {
      return eq(structure,cstruct_d);
    };

    public StructureA result_a() {
      return cstruct_a;
    };
   

    public StructureB result_b() {
      return cstruct_b;
    };

    public StructureC result_c() {
      return cstruct_c;
    };

    public StructureD result_d() {
      return cstruct_d;
    };

};
