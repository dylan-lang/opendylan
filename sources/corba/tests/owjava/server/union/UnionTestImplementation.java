package server.union;

import org.omg.CORBA.SystemException;

import shared.RLE_entity_1;
import shared.RLE_entity_2;
import shared.RLE_entity_3;
import shared.RLE_entity_4;
import shared.RLE_entity_5;
import shared.RLE_entity_1Holder;
import shared.RLE_entity_2Holder;
import shared.RLE_entity_3Holder;
import shared.RLE_entity_4Holder;
import shared.RLE_entity_5Holder;
import shared.UnionTestPackage.*;
import shared._UnionTestOperations;

/**
 * The UnionTest implementation class.
 */
class UnionTestImplementation implements _UnionTestOperations { 

  public UnionTestImplementation () throws SystemException{
    super();
  };

  private RLE_entity_1 entity_1 = null;
  private RLE_entity_2 entity_2 = null;
  private RLE_entity_3 entity_3 = null;
  private RLE_entity_4 entity_4 = null;
  private RLE_entity_5 entity_5 = null;

  public RLE_entity_1 rle_entity_1_attribute () {
    return entity_1;
  };

  public void rle_entity_1_attribute (RLE_entity_1 value) {
    entity_1 = value;
  };

  public RLE_entity_2 rle_entity_2_attribute () {
    return entity_2;
  };

  public void rle_entity_2_attribute (RLE_entity_2 value) {
    entity_2 = value;
  };

  public RLE_entity_3 rle_entity_3_attribute () {
    return entity_3;
  };

  public void rle_entity_3_attribute (RLE_entity_3 value) {
    entity_3 = value;
  };

  public RLE_entity_4 rle_entity_4_attribute () {
    return entity_4;
  };

  public void rle_entity_4_attribute (RLE_entity_4 value) {
    entity_4 = value;
  };

  public RLE_entity_5 rle_entity_5_attribute () {
    return entity_5;
  };

  public void rle_entity_5_attribute (RLE_entity_5 value) {
    entity_5 = value;
  };


  public void check_rle_entity_1_attribute () throws failure {
    if (entity_1.length() != (short)10) throw new failure();
  };

  public void check_rle_entity_2_attribute () throws failure {
    if (entity_2.character() != 'h') throw new failure();
  };

  public void check_rle_entity_3_attribute () throws failure {
    if (entity_3.length() != (short)20) throw new failure();
  };

  public void check_rle_entity_4_attribute () throws failure {
    if (entity_4.character() != 'i') throw new failure();
  };

  public void check_rle_entity_5_attribute () throws failure {
    if (entity_5.length () != (short)30) throw new failure();
  };


  public RLE_entity_1 rle_entity_1_operation (RLE_entity_1 one, RLE_entity_1Holder two, RLE_entity_1Holder three) {
    two.value = two.value;
    three.value = one;
    return one;
  };

  public RLE_entity_2 rle_entity_2_operation (RLE_entity_2 one, RLE_entity_2Holder two, RLE_entity_2Holder three) {
    two.value = two.value;
    three.value = one;
    return one;
  };

  public RLE_entity_3 rle_entity_3_operation (RLE_entity_3 one, RLE_entity_3Holder two, RLE_entity_3Holder three) {
    two.value = two.value;
    three.value = one;
    return one;
  };

  public RLE_entity_4 rle_entity_4_operation (RLE_entity_4 one, RLE_entity_4Holder two, RLE_entity_4Holder three) {
    two.value = two.value;
    three.value = one;
    return one;
  };

  public RLE_entity_5 rle_entity_5_operation (RLE_entity_5 one, RLE_entity_5Holder two, RLE_entity_5Holder three) {
    two.value = two.value;
    three.value = one;
    return one;
  };
};
