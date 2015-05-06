import lldb
from accessors import *

class SyntheticHideChildren(object):
  """A synthetic that shows no children."""
  def __init__(self, value, internal_dict):
    self.value = value
    self.update()

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

class SyntheticDylanValue(SyntheticHideChildren):
  """A shape-shifting synthetic that doesn't do anything by itself, but changes
     class to an appropriate synthetic when there is one.
  """
  def __init__(self, value, internal_dict):
    # In case we're looking at an object on the stack and not a
    # pointer to it.
    if not value.GetType().IsPointerType():
      value = value.address_of
    tag = dylan_tag_bits(value)
    new_class = None
    if tag == OBJECT_TAG:
      try:
        wrapper_symbol_name = dylan_object_wrapper_symbol_name(value)
        new_class = SYNTHETIC_CLASS_TABLE.get(wrapper_symbol_name, SyntheticObject)
        if new_class:
           self.__class__ = new_class
      except:
        # Something went wrong getting the wrapper's symbol name.
        self.__class__ = SyntheticHideChildren
    else:
      pass
    self.value = value
    self.update()

class SyntheticObject(object):
  """A synthetic that knows how to walk the slots of an arbitrary Dylan
     object."""
  # We lazily initialize this to avoid circular loops in creating synthetics
  # and getting slots, which creates more synthetics, etc.
  def num_children(self):
    self.initialize_if_needed()
    return len(self.slots)

  def get_child_index(self, name):
    return -1

  def get_child_at_index(self, index):
    self.initialize_if_needed()
    if index >= 0 and index < len(self.slots):
      slot_name = self.slots[index]
      return dylan_slot_element(self.value, index, '[' + slot_name + ']')
    return None

  def has_children(self):
    return True

  def update(self):
    self.initialized = False
    self.slots = []

  def initialize_if_needed(self):
    if not self.initialized:
      self.slots = dylan_object_class_slot_names(self.value)
      self.initialized = True

class SyntheticSimpleObjectVector(object):
  """A synthetic for representing a <simple-object-vector>."""
  def num_children(self):
    return self.element_count

  def get_child_index(self, name):
    try:
      return int(name.lstrip('[').rstrip(']'))
    except:
      return -1

  def get_child_at_index(self, index):
    if index >= 0 and index < self.element_count:
      # +2 is to skip past the object header and the size.
      offset = (index + 2) * self.dylan_value_type.GetByteSize()
      return self.value.CreateChildAtOffset('[%d]' % index, offset, self.dylan_value_type)
    return None

  def has_children(self):
    return True

  def update(self):
    target = lldb.debugger.GetSelectedTarget()
    self.dylan_value_type = target.FindFirstType('dylan_value')
    self.element_count = dylan_vector_size(self.value)

class SyntheticStretchyObjectVector(object):
  """A synthetic for representing a <stretchy-object-vector>."""
  def num_children(self):
    return self.element_count

  def get_child_index(self, name):
    try:
      return int(name.lstrip('[').rstrip(']'))
    except:
      return -1

  def get_child_at_index(self, index):
    if index >= 0 and index < self.element_count:
      representation = dylan_slot_element(self.value, 0)
      return dylan_slot_element(representation, 2 + index, '[%s]' % index)
    return None

  def has_children(self):
    return True

  def update(self):
    target = lldb.debugger.GetSelectedTarget()
    self.dylan_value_type = target.FindFirstType('dylan_value')
    self.element_count = dylan_stretchy_vector_size(self.value)

SYNTHETIC_CLASS_TABLE = {
  'KLbooleanGVKdW': SyntheticHideChildren,
  'KLbyte_stringGVKdW': SyntheticHideChildren,
  'KLdouble_floatGVKdW': SyntheticHideChildren,
  'KLempty_listGVKdW': SyntheticHideChildren,
  'KLsimple_object_vectorGVKdW': SyntheticSimpleObjectVector,
  'KLsingle_floatGVKdW': SyntheticHideChildren,
  'KLstretchy_object_vectorGVKeW': SyntheticStretchyObjectVector,
  'KLsymbolGVKdW': SyntheticHideChildren,
  'KLunboundGVKeW': SyntheticHideChildren
}
