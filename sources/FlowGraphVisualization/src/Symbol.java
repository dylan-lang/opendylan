
public class Symbol {
	private String s;
	public Symbol (String t) { s = t; }
	
	public String toString () {
		return s;
	}
	
	public boolean isEqual (Symbol another) {
		return another.toString().equals(s);
	}
}
