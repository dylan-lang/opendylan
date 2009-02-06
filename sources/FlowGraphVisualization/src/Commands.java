import java.awt.Color;
import java.util.ArrayList;

import y.base.Edge;
import y.base.EdgeCursor;
import y.base.Node;
import y.view.NodeLabel;


public final class Commands {
	public static boolean processIfNoChange (IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		assert(answer.size() > 1);
		assert(answer.get(0) instanceof Symbol);
		Symbol key = (Symbol)answer.get(0);
		if (key.isEqual("highlight") || key.isEqual("highlight-queue")) {
			processCommand(ihl, answer, demo);
			return true;
		}
		return false;
	}
	
	public static boolean processCommand (IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		assert(answer.size() > 1);
		assert(answer.get(0) instanceof Symbol);
		Symbol key = (Symbol)answer.get(0);

		if (key.isEqual("beginning"))
			return beginning(ihl, answer, demo);
		if (! ihl.graphfinished) {
			if (! (key.isEqual("highlight") || key.isEqual("highlight-queue") || key.isEqual("relayouted")))
				ihl.numChanges++;
			if (! key.isEqual("relayouted"))
				ihl.changes.get(ihl.changes.size() - 1).add(answer);
		}
		if (key.isEqual("dfm-header"))
			return dfmheader(ihl, answer);
		if (key.isEqual("change-edge"))
			return changeedge(ihl, answer);
		if (key.isEqual("remove-edge"))
			return removeedge(ihl, answer);
		if (key.isEqual("insert-edge"))
			return insertedge(ihl, answer);
		if (key.isEqual("new-computation"))
			return newcomputation(ihl, answer);
		if (key.isEqual("remove-computation"))
			return removenode(ihl, answer);
		if (key.isEqual("add-temporary"))
			return addtemporary(ihl, answer);
		if (key.isEqual("add-temporary-user"))
			return addtemporaryuser(ihl, answer);
		if (key.isEqual("remove-temporary-user"))
			return removetemporaryuser(ihl, answer);
		if (key.isEqual("temporary-generator"))
			return temporarygenerator(ihl, answer);
		if (key.isEqual("remove-temporary"))
			return removenode(ihl, answer);
		if (key.isEqual("change-type"))
			return changetype(ihl, answer, demo);
		if (key.isEqual("relayouted"))
			return relayouted(ihl);
		if (key.isEqual("highlight"))
			return highlight(ihl, answer, demo);
		if (key.isEqual("highlight-queue"))
			return highlightqueue(ihl, answer, demo);
		System.out.println("shouldn't be here");
		return false;
	}

	private static Node getNode (IncrementalHierarchicLayout ihl, ArrayList answer, int index, boolean maybenull) {
		assert(answer.size() >= index);
		assert(answer.get(index) instanceof Integer);
		int nodeid = (Integer)answer.get(index);
		Node n = ihl.int_node_map.get(nodeid);
		if (! maybenull)
			assert(n != null);
		return n;
	}
	
	private static boolean beginning (IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		assert(answer.get(2) instanceof ArrayList);
		ArrayList mess = (ArrayList)answer.get(2);
		assert(mess.get(0) instanceof String);
		String ph = (String)mess.get(0);
		if (mess.size() == 2) {
			if (mess.get(1) instanceof Integer) {
				int id = (Integer)mess.get(1);
				ph = ph + " " + id; //or label text? but might be too long
			} else if (mess.get(1) instanceof Symbol) {
				Symbol tag = (Symbol)mess.get(1);
				if (tag.isEqual("global")) {
					//demo.phase.setText(ph);
					//demo.phase.validate();
					return false;
				}
			}
		}
		ihl.updatephase(ph);
		demo.calcLayout();
		return false;
	}
	
	private static boolean dfmheader (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 3);
		assert(answer.get(2) instanceof ArrayList);
		ArrayList cf = (ArrayList)answer.get(2);
		assert(cf.size() == 3);
		assert(cf.get(0) instanceof Symbol);
		assert(((Symbol)cf.get(0)).isEqual("method"));
		assert(cf.get(1) instanceof Symbol); //method name
		assert(cf.get(2) instanceof String); //arg, val
		ihl.addMethodNode(((Symbol)(cf.get(1))).toString(), (String)cf.get(2));
		return true;
	}
	
	private static boolean changeedge (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 6);
		assert(answer.get(5) instanceof Symbol);
		Node from = getNode(ihl, answer, 2, false);
		Node toold = getNode(ihl, answer, 3, false);
		Node tonew = getNode(ihl, answer, 4, false);
		Symbol label = (Symbol)answer.get(5);
		if (label.isEqual("no"))
			label = null;
		Edge change = null;
		for (EdgeCursor ec = from.outEdges(); ec.ok(); ec.next())
			if (ec.edge().target() == toold)
				if (label == null || label.isEqual(ihl.graph.getRealizer(ec.edge()).getLabelText())) {
					change = ec.edge();
					break;
				}
		if (change != null) {
			ihl.graph.changeEdge(change, from, tonew);
			return true;
		} else
			if (ihl.safeCreateEdge(from, tonew)) {
				System.out.println("only created edge");
				if (label != null)
					ihl.setEdgeLabel(label);
				return true;
			}
		return false;
	}
	
	private static boolean removeedge (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 5);
		assert(answer.get(4) instanceof Symbol);
		Node from = getNode(ihl, answer, 2, false);
		Node to = getNode(ihl, answer, 3, false);
		Symbol label = (Symbol)answer.get(4);
		if (label.isEqual("no"))
			label = null;
		for (EdgeCursor ec = from.outEdges(); ec.ok(); ec.next())
			if (ec.edge().target() == to)
				if (label == null || label.isEqual(ihl.graph.getRealizer(ec.edge()).getLabelText())) {
					ihl.graph.removeEdge(ec.edge());
					return true;
				}
		System.out.println("FAILED");
		return false;
	}
	
	private static boolean insertedge (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 5);
		Node from = getNode(ihl, answer, 2, false);
		Node to = getNode(ihl, answer, 3, false);
		assert(answer.get(4) instanceof Symbol);
		Symbol label = (Symbol)answer.get(4);
		if (label.isEqual("no"))
			label = null;
		if (ihl.safeCreateEdge((Integer)answer.get(2), (Integer)answer.get(3))) {
			if (label != null)
				ihl.setEdgeLabel(label);
			return true;
		}
		return false;
	} 
	
	private static boolean newcomputation (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 3);
		assert(answer.get(2) instanceof ArrayList);
		ArrayList text = (ArrayList)answer.get(2);
		ihl.createNewNode(text);
		return true;
	} 
	
	
	private static boolean removenode (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 3);
		Node del = getNode(ihl, answer, 2, false);
		ihl.graph.removeNode(del);
		return true;
	} 
	
	private static boolean addtemporary (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 4);
		assert(answer.get(2) instanceof Integer);
		assert(answer.get(3) instanceof Integer);
		int temp_id = (Integer)answer.get(2);
		int c_id = (Integer)answer.get(3);
		ihl.createTemporary(temp_id, c_id);
		return true;
	}
	
	private static boolean addtemporaryuser (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 4);
		Node temp = getNode(ihl, answer, 2, false);
		Node comp = getNode(ihl, answer, 3, false);
		if (ihl.safeCreateEdge(temp, comp)) {
			ihl.setEdgeColor(Color.pink);
			return true;
		}
		return false;
	} 
	
	private static boolean removetemporaryuser (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 4);
		Node temp = getNode(ihl, answer, 2, false);
		Node comp = getNode(ihl, answer, 3, false);
		for (EdgeCursor ec = temp.outEdges(); ec.ok(); ec.next())
			if (ec.edge().target() == comp) {
				ihl.graph.removeEdge(ec.edge());
				return true;
			}
		return false;
	} 
	
	private static boolean temporarygenerator (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 5);
		Node temp = getNode(ihl, answer, 2, false);
		Node newgenerator = getNode(ihl, answer, 3, false);
		Node oldgenerator = getNode(ihl, answer, 4, true);
		if (oldgenerator != null)
			for (EdgeCursor ec = temp.inEdges(); ec.ok(); ec.next())
				if (ec.edge().source() == oldgenerator) {
					ihl.graph.changeEdge(ec.edge(), temp, newgenerator);
					return true;
				}
		if (ihl.safeCreateEdge(temp, newgenerator)) {
			ihl.setEdgeColor(Color.pink);
			return true;
		}
		return false;
	} 
	
	private static boolean changetype (IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		assert(answer.size() == 4);
		Node n = getNode(ihl, answer, 2, false);
		assert(answer.get(3) instanceof String);
		NodeLabel nl = ihl.graph.getRealizer(n).getLabel();
		String old = nl.getText();
		//filter number out
		int start = old.indexOf(' ') + 1;
		nl.setText(old.substring(0, start) + (String)answer.get(3));
		ihl.graph.getRealizer(n).setWidth(nl.getWidth());
		demo.contentPane.repaint();
		return false;
	}
        
	private static boolean relayouted (IncrementalHierarchicLayout ihl) {
		ihl.activateLayouter();
		return false;
	} 
        
	private static boolean highlight (IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		Node highlightnew = getNode(ihl, answer, 2, false);
		if (demo.highlight != highlightnew) {
			if (demo.highlight != null)
				ihl.graph.getRealizer(demo.highlight).setFillColor(ihl.graph.getDefaultNodeRealizer().getFillColor());
			ihl.graph.getRealizer(highlightnew).setFillColor(Color.green);
			demo.highlight = highlightnew;
			demo.contentPane.repaint();
		}
		return false;
	} 
		
	private static boolean highlightqueue (IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		ArrayList queue = (ArrayList)answer.get(2);
		ArrayList<Integer> removed = new ArrayList<Integer>();
		ArrayList<Integer> added = new ArrayList<Integer>();
		for (Object ind : queue)
			if (! demo.opt_queue.contains((Integer)ind))
				added.add((Integer)ind);
		for (Integer old : demo.opt_queue)
			if (! queue.contains(old))
				removed.add(old);

		for (Integer rem : removed) {
			demo.opt_queue.remove(rem);
			Node unh = ihl.int_node_map.get(rem);
			if (unh != demo.highlight && unh != null)
				ihl.graph.getRealizer(unh).setFillColor(ihl.graph.getDefaultNodeRealizer().getFillColor());
		}
		for (Integer a : added) {
			demo.opt_queue.add(a);
			Node h = ihl.int_node_map.get(a);
			if (h != demo.highlight && h != null)
				ihl.graph.getRealizer(h).setFillColor(Color.orange);
		}
		demo.contentPane.repaint();
		return false;
	}

}
