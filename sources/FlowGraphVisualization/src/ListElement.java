
public class ListElement implements Comparable {
	private String item;
	private int index;
	
	public ListElement (int index, String item) {
		this.item = item;
		this.index = index;
	}
	
	@Override
	public int compareTo(Object o) {
		if (o instanceof ListElement) {
			ListElement other = (ListElement)o;
			return this.item.compareTo(other.toString());
		}
		return 0;
	}
	
	public int myindex() {
		return index;
	}
	
	public String toString() {
		return item;
	}

}
