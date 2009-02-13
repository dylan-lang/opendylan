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
			return removenode(ihl, answer, false);
		if (key.isEqual("add-temporary"))
			return addtemporary(ihl, answer);
		if (key.isEqual("add-temporary-user"))
			return addtemporaryuser(ihl, answer);
		if (key.isEqual("remove-temporary-user"))
			return removetemporaryuser(ihl, answer);
		if (key.isEqual("temporary-generator"))
			return temporarygenerator(ihl, answer);
		if (key.isEqual("remove-temporary"))
			return removenode(ihl, answer, true);
		if (key.isEqual("change-type"))
			return changetype(ihl, answer, demo);
		if (key.isEqual("change-entry-point"))
			return changeentrypoint(ihl, answer, demo);
		if (key.isEqual("set-loop-call-loop"))
			return setloopcallloop(ihl, answer);
		if (key.isEqual("relayouted"))
			return relayouted(ihl);
		if (key.isEqual("highlight"))
			return highlight(ihl, answer, demo);
		if (key.isEqual("highlight-queue"))
			return highlightqueue(ihl, answer, demo);
		System.out.println("shouldn't be here");
		return false;
	}

	private static boolean setloopcallloop(IncrementalHierarchicLayout ihl,
			ArrayList answer) {
		Node loopcall = getNode(ihl, answer, 2, false);
		Node loop = getNode(ihl, answer, 3, true);
		if (loop != null)
			ihl.graph.createEdge(loopcall, loop);
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
				Node n = getNode(ihl, mess, 1, false);
				String label = ihl.graph.getRealizer(n).getLabelText();
				ph = ph + " " + label; //or label text? but might be too long
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
		ArrayList cfs = (ArrayList)answer.get(2);
		String main = null;
		for (Object o : cfs) {
			assert(o instanceof ArrayList);
			ArrayList cf = (ArrayList)o;
			assert(cf.size() == 5);
			assert(cf.get(0) instanceof Symbol);
			assert(((Symbol)cf.get(0)).isEqual("method"));
			assert(cf.get(1) instanceof Symbol); //method name
			assert(cf.get(2) instanceof Integer); //bind
			Node bind = getNode(ihl, cf, 2, false);
			assert(cf.get(3) instanceof ArrayList); //args
			assert(cf.get(4) instanceof ArrayList); //arg names
			main = ((Symbol)(cf.get(1))).toString();
			ihl.addMethodNode(main, bind, (ArrayList)cf.get(3), (ArrayList)cf.get(4));
		}
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
	
	private static boolean removenode (IncrementalHierarchicLayout ihl, ArrayList answer, boolean mayfail) {
		assert(answer.size() == 3);
		Node del = getNode(ihl, answer, 2, mayfail);
		if (del != null) {
			if (ihl.graph.getRealizer(del).getLabelText().contains("bind") && del.inDegree() == 1)
				ihl.graph.removeNode(del.firstInEdge().source());
			//System.out.println("D:" + del.degree() + " " + ihl.graph.getRealizer(del).getLabelText());
			if (del.degree() > 0)
				for (EdgeCursor ec = del.edges(); ec.ok(); ec.next()) {
					//System.out.println("  was connected to " + ihl.graph.getRealizer(ec.edge().opposite(del)).getLabelText());
				}
			ihl.graph.removeNode(del);
			ihl.int_node_map.remove((Integer)answer.get(2));
			return true;
		}
		return false;
	} 
	
	private static boolean addtemporary (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 5);
		assert(answer.get(2) instanceof Integer);
		assert(answer.get(3) instanceof String);
		assert(answer.get(4) instanceof Integer);
		int temp_id = (Integer)answer.get(2);
		String text = (String)answer.get(3);
		int c_id = (Integer)answer.get(4);
		if (ihl.int_node_map.get(temp_id) == null) {
			ihl.createTemporary(temp_id, c_id, text + ":");
			return true;
		}
		//System.out.println("already added temporary " + temp_id + " " + text);
		return false;
	}
	
	private static boolean addtemporaryuser (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 4);
		Node temp = getNode(ihl, answer, 2, false);
		Node comp = getNode(ihl, answer, 3, false);
		ihl.graph.createEdge(temp, comp);
		ihl.setEdgeColor(Color.pink);
		return true;
	} 
	
	private static boolean removetemporaryuser (IncrementalHierarchicLayout ihl, ArrayList answer) {
		assert(answer.size() == 4);
		Node temp = getNode(ihl, answer, 2, true);
		if (temp == null) {
			System.out.println("temp not present " + (Integer)answer.get(2));
			return false; //happens with arguments of inlined functions
		}
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
		if (ihl.safeCreateEdge(newgenerator, temp)) {
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
		int start = old.indexOf(':', old.indexOf(':') + 1) + 1;
		nl.setText(old.substring(0, start) + (String)answer.get(3));
		ihl.graph.getRealizer(n).setWidth(nl.getWidth());
		System.out.println("change type " + old + " => " + (String)answer.get(3));
		demo.view.repaint();
		//ihl.isok = false;
		return true;
	}
        
	private static boolean changeentrypoint(IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		assert(answer.size() == 4);
		Node n = getNode(ihl, answer, 2, false);
		assert(answer.get(3) instanceof Symbol);
		NodeLabel nl = ihl.graph.getRealizer(n).getLabel();
		String old = nl.getText();
		//filter number out
		int start = old.indexOf(':') + 1;
		nl.setText(old.substring(0, start) + " " + ((Symbol)answer.get(3)).toString() + " " + old.substring(start));
		ihl.graph.getRealizer(n).setWidth(nl.getWidth());
		//System.out.println("change entry point " + old + " => " + ((Symbol)answer.get(3)).toString());
		demo.view.repaint();
		return true;		
	}
	
	private static boolean relayouted (IncrementalHierarchicLayout ihl) {
		ihl.activateLayouter();
		return false;
	} 
        
	private static boolean highlight (IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		if ((Integer)answer.get(2) == 0) {
			if (ihl.highlight != null) {
				ihl.graph.getRealizer(ihl.highlight).setFillColor(ihl.graph.getDefaultNodeRealizer().getFillColor());
				ihl.highlight = null;
				demo.view.repaint();
			}
			return false;
		}
		Node highlightnew = getNode(ihl, answer, 2, false);
		if (ihl.highlight != highlightnew) {
			if (ihl.highlight != null)
				ihl.graph.getRealizer(ihl.highlight).setFillColor(ihl.graph.getDefaultNodeRealizer().getFillColor());
			ihl.graph.getRealizer(highlightnew).setFillColor(Color.green);
			ihl.highlight = highlightnew;
			demo.view.repaint();
		}
		return false;
	} 
		
	private static boolean highlightqueue (IncrementalHierarchicLayout ihl, ArrayList answer, DemoBase demo) {
		ArrayList queue = (ArrayList)answer.get(2);
		ArrayList<Integer> removed = new ArrayList<Integer>();
		ArrayList<Integer> added = new ArrayList<Integer>();
		for (Object ind : queue)
			if (! ihl.opt_queue.contains((Integer)ind))
				added.add((Integer)ind);
		for (Integer old : ihl.opt_queue)
			if (! queue.contains(old))
				removed.add(old);

		for (Integer rem : removed) {
			ihl.opt_queue.remove(rem);
			Node unh = ihl.int_node_map.get(rem);
			if (unh != ihl.highlight && unh != null)
				ihl.graph.getRealizer(unh).setFillColor(ihl.graph.getDefaultNodeRealizer().getFillColor());
		}
		for (Integer a : added) {
			ihl.opt_queue.add(a);
			Node h = ihl.int_node_map.get(a);
			if (h != ihl.highlight && h != null)
				ihl.graph.getRealizer(h).setFillColor(Color.orange);
		}
		demo.view.repaint();
		return false;
	}

}
