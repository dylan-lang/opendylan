import lldb
from accessors import *

class SyntheticDylanValue(object):
  def __init__(self, value, internal_dict):
    tag = dylan_tag_bits(value)
    new_class = None
    if tag == OBJECT_TAG:
      class_name = dylan_object_class_name(value)
      new_class = SYNTHETIC_CLASS_TABLE.get(class_name, None)
      if new_class is not None:
        self.__class__ = new_class
    else:
      pass
    self.value = self.cast_value(value)
    self.update()

  def cast_value(self, value):
    return value

  def num_children(self):
    return 0

  def get_child_index(self, name):
    return -1

  def get_child_at_index(self, index):
    return None

  def has_children(self):
    return False

  def update(self):
    pass

class SyntheticSimpleObjectVector(object):
  def cast_value(self, value):
    target = lldb.debugger.GetSelectedTarget()
    vector_type = target.FindFirstType('dylan_simple_object_vector').GetPointerType()
    return value.Cast(vector_type)

  def num_children(self):
    return self.element_count

  def get_child_index(self, name):
    try:
      return int(name.lstrip('[').rstrip(']'))
    except:
      return -1

  def get_child_at_index(self, index):
    if index >= 0 and index < self.element_count:
      # +1 is to skip past the object header
      offset = (index + 1) * self.dylan_value_type.GetByteSize()
      return self.value.CreateChildAtOffset('[%d]' % index, offset, self.dylan_value_type)
    return None

  def has_children(self):
    return True

  def update(self):
    target = lldb.debugger.GetSelectedTarget()
    self.dylan_value_type = target.FindFirstType('dylan_value')
    self.element_count = dylan_integer_value(self.value.GetChildMemberWithName('size'))

SYNTHETIC_CLASS_TABLE = {
  '<simple-object-vector>': SyntheticSimpleObjectVector
}
