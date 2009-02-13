import java.io.Serializable;
import java.util.Vector;

import javax.swing.AbstractListModel;
import javax.swing.MutableComboBoxModel;


public class SortedListComboBoxModel extends AbstractListModel implements
		MutableComboBoxModel, Serializable {

	private Vector elements;
	private Object selectedItem = null;
	
	public SortedListComboBoxModel () {
		elements = new Vector();
	}
	@Override
	public void addElement(Object arg0) {
		insertElementAt(arg0, 1);
	}

	@Override
	public void insertElementAt(Object element, int foo) {
		if (getSize() < 2)
			elements.insertElementAt(element, getSize());
		else
			for (int index = 1; index < getSize(); index++)
			{
				Comparable c = (Comparable)getElementAt( index );

				if (c.compareTo(element) > 0) {
					elements.insertElementAt(element, index);
					break;
				}
				if (index == getSize() - 1) {
					elements.add(element);
					break;
				}
			}
		fireIntervalAdded(this, 0, getSize());
	}

	@Override
	public void removeElement(Object arg0) {
		elements.remove(arg0);
	}

	@Override
	public void removeElementAt(int arg0) {
		elements.remove(arg0);
	}

	@Override
	public Object getSelectedItem() {
		return selectedItem;
	}

	@Override
	public void setSelectedItem(Object arg0) {
		selectedItem = arg0;
	    fireContentsChanged(this, -1, -1);
	}

	@Override
	public Object getElementAt(int arg0) {
		return elements.get(arg0);
	}

	@Override
	public int getSize() {
		return elements.size();
	}

}
