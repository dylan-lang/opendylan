import java.awt.Color;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedList;

import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import y.base.DataMap;
import y.base.EdgeCursor;
import y.base.Node;
import y.base.NodeCursor;
import y.base.NodeMap;
import y.layout.hierarchic.ConstraintLayerer;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.TopologicalLayerer;
import y.layout.hierarchic.ConstraintLayerer.ConstraintFactory;
import y.layout.hierarchic.incremental.IncrementalHintsFactory;
import y.layout.hierarchic.incremental.OldLayererWrapper;
import y.layout.hierarchic.incremental.SwimLaneDescriptor;
import y.util.Maps;
import y.view.Arrow;
import y.view.BridgeCalculator;
import y.view.DefaultGraph2DRenderer;
import y.view.EdgeRealizer;
import y.view.GenericEdgeRealizer;
import y.view.GenericNodeRealizer;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.NodeLabel;
import y.view.NodeRealizer;

public class IncrementalHierarchicLayout
{
	protected IncrementalHierarchicLayouter hierarchicLayouter;
	protected HashMap<Integer, Node> int_node_map = new HashMap<Integer, Node>();
	
	protected Graph2D graph;
	private Graph2DView view;
	private DemoBase demobase;
	protected ConstraintFactory scf;
	
	public Node highlight = null;
	public ArrayList<Integer> opt_queue;


	public boolean changed = false;
	public int numChanges = 0;
	protected Hashtable<Integer, JLabel> sliderLabels = new Hashtable<Integer, JLabel>();
	protected int lastEntry = 0;
	private int lastChangeCount = 0;
	public ArrayList<ArrayList> changes = new ArrayList<ArrayList>();

	protected int lastslidervalue = 0;
	protected boolean graphfinished = false;
	
	protected final int graph_id;
	
	public Node selection;
	
	public boolean isok = true;

	private NodeMap swimLane;
	protected DataMap hintMap;
	protected IncrementalHintsFactory hintsFactory;
	private ArrayList<Node> topnodes;
	
	
	public IncrementalHierarchicLayout(DemoBase db, int id)
	{
		graph_id = id;
		graph = new Graph2D();
		view = db.view;
		demobase = db;
		
		// enable bridges for PolyLineEdgeRealizer
		BridgeCalculator bridgeCalculator = new BridgeCalculator();
		bridgeCalculator.setCrossingMode( BridgeCalculator.CROSSING_MODE_HORIZONTAL_CROSSES_VERTICAL );
		((DefaultGraph2DRenderer) view.getGraph2DRenderer()).setBridgeCalculator(bridgeCalculator );

		// create and configure the layout algorithm
		hierarchicLayouter = new IncrementalHierarchicLayouter();
		hierarchicLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_FROM_SCRATCH);
		
	    hierarchicLayouter.getEdgeLayoutDescriptor().setMinimumFirstSegmentLength(0);
	    hierarchicLayouter.getEdgeLayoutDescriptor().setMinimumLastSegmentLength(0);
	    //hierarchicLayouter.getEdgeLayoutDescriptor().setOrthogonallyRouted(true);
	    hierarchicLayouter.getEdgeLayoutDescriptor().setMinimumDistance(2.0d);

	    hierarchicLayouter.getNodeLayoutDescriptor().setLayerAlignment(0.0d);
	    hierarchicLayouter.setMinimumLayerDistance(3.0d);

		
		ConstraintLayerer cl = new ConstraintLayerer();
		TopologicalLayerer tl = new TopologicalLayerer();
		//tl.setRankingPolicy(TopologicalLayerer.DOWN_SHIFT);
		cl.setCoreLayerer(tl);
		//cl.setCoreLayerer(hierarchicLayouter.getLayerer());
		hierarchicLayouter.setFromScratchLayerer(new OldLayererWrapper(cl));

		sliderLabels.put(0, new JLabel("initial DFM models"));
		changes.add(new ArrayList());
		initGraph();
	}
	
	public void initGraph () {
		// make it look nice
		EdgeRealizer defaultER = graph.getDefaultEdgeRealizer();
		defaultER.setArrow(Arrow.STANDARD);
		if (scf != null)
			scf.dispose();
		scf = ConstraintLayerer.createConstraintFactory(graph);
		swimLane = graph.createNodeMap();
		graph.addDataProvider(IncrementalHierarchicLayouter.SWIMLANE_DESCRIPTOR_DPKEY, swimLane);
		hintMap = Maps.createHashedDataMap();
	    //graph.addDataProvider(IncrementalHierarchicLayouter.INCREMENTAL_HINTS_DPKEY, hintMap);
	    hintsFactory = hierarchicLayouter.createIncrementalHintsFactory();
		int_node_map = new HashMap<Integer, Node>();
		opt_queue = new ArrayList<Integer>();
		topnodes = new ArrayList<Node>();
		highlight = null;
		selection = null;
	}
	
	public void activateLayouter () {
		if (demobase.incrementallayouter != this) {
			changed = true;
			view.setGraph2D(graph);
			demobase.graphChanged(this);
		} else
			demobase.calcLayout();
	}

	protected void addMethodNode (String name, Node bind, ArrayList arguments, ArrayList argumentnames)
	{
		Node header = createNodeWithLabel(name, 0);
		graph.getRealizer(header).setFillColor(Color.LIGHT_GRAY);
		graph.createEdge(header, bind); //edge to bind!
		scf.addPlaceNodeAtTopConstraint(header);
		scf.addPlaceNodeBelowConstraint(header, bind);
		
		int i = 0;
		for (Object number : arguments) {
			assert(number instanceof Integer);
			Node n = int_node_map.get((Integer)number);
			if (n == null) {
				createTemporary((Integer)number, 0, (String)argumentnames.get(i) + ":");
				n = graph.lastNode();
			}
			EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
			myreal.setLineColor(Color.blue);
			graph.createEdge(header, n, myreal);
			scf.addPlaceNodeBelowConstraint(header, n);
			i++; //want loop over multiple variables!
		}
	}

	private void initGraphHelperHelper (Object o, int id) {
		if (o instanceof String)
			initGraphHelper((String)o, id);
		else if (o instanceof Symbol)
			initGraphHelper((Symbol)o, id);
		else if (o instanceof Integer)
			initGraphHelper((Integer)o, id);
		else if (o instanceof ArrayList)
			initGraphHelper((ArrayList)o, id);
	}

	private void initGraphHelper (ArrayList nodelist, int id) {
		assert (nodelist.size() > 1);

		assert(nodelist.get(0) instanceof Integer);
		assert(nodelist.get(1) instanceof Symbol);
		
		int comp_id = (Integer)nodelist.get(0);
		Symbol s = (Symbol)nodelist.get(1);

		if (s.isEqual("if")) {
			//test, exactly one element, a string
			assert(nodelist.size() == 5);
			assert(nodelist.get(2) instanceof ArrayList);
			ArrayList testnodes = (ArrayList)nodelist.get(2);
			assert(testnodes.size() == 1);
			assert(testnodes.get(0) instanceof String);
			Node test = createNodeWithLabel((String)testnodes.get(0), comp_id);
			changeLabel(test, "if ");
				
			//consequence
			assert(nodelist.get(3) instanceof ArrayList);
			ArrayList<Node> consequence = getNodes((ArrayList)nodelist.get(3));
				
			if (consequence.size() > 0) {
				graph.createEdge(test, consequence.get(0));
				setEdgeLabel("true");
			}
			
			
			
			for (Node c : consequence) {
				//scf.addPlaceNodeBelowConstraint(test, c);
				//groups.setInt(c, test.index());
			}
				
			//alternative
			assert(nodelist.get(4) instanceof ArrayList);
			ArrayList<Node> alternative = getNodes((ArrayList)nodelist.get(4));
				
			if (alternative.size() > 0) {
				graph.createEdge(test, alternative.get(0));
				setEdgeLabel("false");
			}
			for (Node a : alternative) {
				//scf.addPlaceNodeBelowConstraint(test, a);
				//groups.setInt(a, test.index() - 1);
			}

		} else if (s.isEqual("loop")) {
			Node loop = createNodeWithLabel("loop", comp_id);
			assert(nodelist.size() == 3);
			assert(nodelist.get(2) instanceof ArrayList);
				
			ArrayList<Node> body = getNodes((ArrayList)nodelist.get(2));

			//groupMap.set(n, loop-id);
			if (body.size() > 0)
				graph.createEdge(loop, body.get(0));
			
			for (Node b : body)
				if (graph.getRealizer(b).getLabelText().contains("CONTINUE"))
					safeCreateEdge(b, loop);

		} else if (s.isEqual("loop-call")) {
			assert(nodelist.size() == 3);
			assert(nodelist.get(2) instanceof Integer);
			Node loopc = createNodeWithLabel("CONTINUE", comp_id);
			if ((Integer) nodelist.get(2) != 0) {
				Node loop = int_node_map.get((Integer)nodelist.get(2));
				graph.createEdge(loopc, loop);
			}
		} else if (s.isEqual("bind-exit")) {
			assert(nodelist.size() == 3);
			//assert(nodelist.get(2) instanceof Integer); //entry-state
			assert(nodelist.get(2) instanceof ArrayList); //body
			Node bind_exit = createNodeWithLabel("bind-exit", comp_id);
			ArrayList<Node> body = getNodes((ArrayList)nodelist.get(2));
			if (body.size() > 0)
				graph.createEdge(bind_exit, body.get(0));
		} else if (s.isEqual("unwind-protect")) {
			assert(nodelist.size() == 4);
			assert(nodelist.get(2) instanceof ArrayList); //body
			assert(nodelist.get(3) instanceof ArrayList); //cleanups
			Node unwind_protect = createNodeWithLabel("unwind-protect", comp_id);
			ArrayList<Node> body = getNodes((ArrayList)nodelist.get(2));
			if (body.size() > 0)
				graph.createEdge(unwind_protect, body.get(0));
			ArrayList<Node> cleanups = getNodes((ArrayList)nodelist.get(3));
			if (body.size() > 0)
				graph.createEdge(unwind_protect, cleanups.get(0));
		}
	}
	
	private ArrayList<Node> getNodes(ArrayList arrayList) {
		ArrayList<Node> res = new ArrayList<Node>();
		for (Object o : arrayList) {
			assert(o instanceof Integer);
			Integer i = (Integer)o;
			assert(int_node_map.get(i) != null);
			res.add(int_node_map.get(i));
		}
		return res;
	}

	private Node initGraphHelper (String node, int id) {
		return createNodeWithLabel(node, id);
	}
	
	private Node initGraphHelper (Symbol node, int id) {
		return createNodeWithLabel(node.toString(), id);
	}
	
	private Node initGraphHelper (Integer node, int id) {
		return createNodeWithLabel(Integer.toString(node), id);
	}
	
	private Node createNodeWithLabel (String label, int id) {
		NodeRealizer n1 = new GenericNodeRealizer(graph.getDefaultNodeRealizer());
		NodeLabel nl1 = n1.createNodeLabel();
		nl1.setText(id + ": " + label);
		n1.setLabel(nl1);
		n1.setWidth(nl1.getWidth() + 10);
		Node n = graph.createNode(n1);
		if (id > 0) {
			assert(int_node_map.get(id) == null);
			int_node_map.put(id, n);
		} else
			topnodes.add(n);
		return n;
	}
	
	private void changeLabel (Node n, String app) {
		NodeLabel nl = graph.getRealizer(n).getLabel();
		String old = nl.getText();
		//filter number out
		int start = old.indexOf(' ') + 1;
		nl.setText(old.substring(0, start) + app + old.substring(start));
		graph.getRealizer(n).setWidth(nl.getWidth());
	}

	public void createTemporary(int temp_id, int c_id, String text) {
		Node t = createNodeWithLabel(text, temp_id);
		graph.getRealizer(t).setFillColor(Color.pink);
		if (c_id != 0) {
			Node gen = int_node_map.get(c_id);
			assert(gen != null);
			EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
			myreal.setLineColor(Color.pink);
			graph.createEdge(gen, t, myreal);
			scf.addPlaceNodeInSameLayerConstraint(t, gen);
		}
	}

	public boolean safeCreateEdge (Node source, Node target) {
		if (source == null || target == null) {
			System.out.println("FAIL from " + source + " target " + target + " (source or target null)");
			return false;
		}
		boolean connected = false;
		for (EdgeCursor ec = source.outEdges(); ec.ok(); ec.next())
			if (ec.edge().target() == target) {
				connected = true;
				break;
			}
		if (! connected) {
			graph.createEdge(source, target);
			//scf.addPlaceNodeBelowConstraint(source, target);
			return true;
		}
		System.out.println("FAIL: nodes " + source + " and " + target + " were already connected");
		return false;
	}

	public void createNewNode(ArrayList text) {
		assert(text.get(0) instanceof Integer);
		if (text.size() == 2)
			initGraphHelperHelper(text.get(1), (Integer)text.get(0));
		else
			initGraphHelperHelper(text, (Integer)text.get(0));
	}

	public void setEdgeLabel(String label) {
		EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
		myreal.setLabelText(label);
		if (label.equals("true"))
			myreal.setLineColor(Color.green);
		else if (label.equals("false"))
			myreal.setLineColor(Color.red);
		graph.setRealizer(graph.lastEdge(), myreal);
		
	}
	
	public void setEdgeLabel(Symbol label) {
		setEdgeLabel(label.toString());
	}
	
	public void setEdgeColor (Color color) {
		EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
		myreal.setLineColor(color);
		graph.setRealizer(graph.lastEdge(), myreal);
	}

	public void updatephase(String text) {
		boolean finished = false;
		if (text.equals("finished"))
			finished = true;
		final boolean rfinished = finished;
		if (lastChangeCount < numChanges) {
			lastEntry++;
			lastChangeCount = numChanges;
			changes.add(new ArrayList());
		}
		sliderLabels.put(lastEntry, new JLabel(text.substring(0, Math.min(40, text.length()))));
		Runnable updateMyUI = new Runnable() {
			public void run () {
				demobase.slider.setMaximum(lastEntry);
				demobase.slider.setValue(lastEntry);
				demobase.slider.updateUI();		
				if (rfinished)
					graphfinished = true;
			}
		};
		SwingUtilities.invokeLater(updateMyUI);
		lastslidervalue = lastEntry;
	}
	
	public boolean nextStep () {
		if (lastslidervalue <= lastEntry) {
			for (Object comm : changes.get(lastslidervalue)) {
				ArrayList com = (ArrayList)comm;
				Commands.processCommand(this, com, demobase);
			}

			lastslidervalue++;
			if (lastslidervalue <= lastEntry)
				for (Object o : changes.get(lastslidervalue)) {
					ArrayList comm = (ArrayList)o;
					if (! Commands.processIfNoChange(this, comm, demobase))
						break;
				}
			changed = true;
			demobase.updatingslider = true;
			demobase.slider.setValue(lastslidervalue);
			demobase.updatingslider = false;
			demobase.calcLayout();
			return true;
		}
		return false;
	}

	public void resetGraph(int step) {
		if (step >= lastslidervalue) {
			demobase.unselect();
			for (int i = lastslidervalue; i < step; i++)
				for (Object comm : changes.get(i)) {
					ArrayList com = (ArrayList)comm;
					Commands.processCommand(this, com, demobase);
				}
		} else { //I was too lazy to implement undo, so do the graph from scratch
			demobase.unselect();
			graph = new Graph2D();
			initGraph();
			view.setGraph2D(graph);
			for (int i = 0; i < step; i++)
				for (Object comm : changes.get(i)) {
					ArrayList com = (ArrayList)comm;
					Commands.processCommand(this, com, demobase);
				}
		}
		for (Object o : changes.get(step)) {
			ArrayList comm = (ArrayList)o;
			if (! Commands.processIfNoChange(this, comm, demobase))
				break;
		}
		lastslidervalue = step;
		changed = true;
		demobase.calcLayout();
	}

	public void calcSwimLanes() {
	    int i = 1;
		for (Node t : topnodes) {
			if (swimLane.get(t) == null) {
				SwimLaneDescriptor sld = new SwimLaneDescriptor(i * 16);
				swimLane.set(t, sld);
			}
			visited = graph.createNodeMap();
			currindex = i * 16;
			middle = i * 16;
			step = 8;
			for (EdgeCursor ec = t.outEdges(); ec.ok(); ec.next())
				if (graph.getRealizer(ec.edge()).getLineColor() == Color.black) 
					visitComputations(ec.edge().target(), null);
			int argc = i * 16 + 1;
			for (EdgeCursor ec = t.outEdges(); ec.ok(); ec.next()) {
				Color c = graph.getRealizer(ec.edge()).getLineColor(); 
				if (c == Color.blue) {
					int lane = 0;
					for (NodeCursor nc = ec.edge().target().successors(); nc.ok(); nc.next())
						lane += (Integer)((SwimLaneDescriptor)swimLane.get(nc.node())).getClientObject();
					if (swimLane.get(ec.edge().target()) == null) {
						int divisor = ec.edge().target().outDegree();
						int la = argc;
						if (divisor != 0)
							la = lane / divisor;
						argc++;
						SwimLaneDescriptor sld2 = new SwimLaneDescriptor(la);
						swimLane.set(ec.edge().target(), sld2);
					}
				}
			}
			i++;
		}
	}
	
	private NodeMap visited;
	private int middle = 16;
	private int currindex = 16;
	private int step = 8;

	private void update (int direction) {
		System.out.println("update called with " + direction + " (" + currindex + ", " + step + ")");
		if (direction == 1)
			currindex += step;
		else if (direction == -1)
			currindex -= step;
		else if (currindex < middle)
			currindex -= step;
		else
			currindex += step;
		step /= 2;
	}
	
	private void restore (int direction) {
		System.out.println("restore called (" + currindex + ", " + step + ")");
		step *= 2;
		if (direction == 1)
			currindex -= step;
		else if (direction == -1)
			currindex += step;
		else if (currindex < middle)
			currindex += step;
		else
			currindex -= step;
	}

	private Node visitComputations (Node t, String until) {
		final Deque<Node> workingset = new LinkedList<Node>();
		workingset.push(t);
		while (workingset.size() > 0) {
			Node work = workingset.pollFirst();
			if (until != null && graph.getLabelText(work).contains(until))
				return work;
			visited.set(work, true);
			Node next = visit(work);
			if (next != null) work = next;
			for (EdgeCursor ec = work.outEdges(); ec.ok(); ec.next())
				if (graph.getRealizer(ec.edge()).getLineColor() != Color.pink) {
					Node targ = ec.edge().target();
					if (visited.get(targ) == null)
						workingset.push(targ);
				}
		}
		return null;
	}
	
	private Node nextC (Node n) {
	    for (EdgeCursor ec = n.outEdges(); ec.ok(); ec.next())
		if ((graph.getRealizer(ec.edge()).getLineColor() != Color.pink) || (graph.getRealizer(n).getFillColor() == Color.pink))
				return ec.edge().target();
		return null;
	}
	
	private Node visit (Node n) {
		String text = graph.getLabelText(n);
		setL(currindex, n);
		for (EdgeCursor ec = n.edges(); ec.ok(); ec.next())
			if (graph.getRealizer(ec.edge()).getLineColor() == Color.pink) {
				int weight = -1 * step;
				//if (nextC(n) == nextC(ec.edge().opposite(n)))
				//	weight = step;
				if (ec.edge().opposite(n).degree() == 1) {
					setLf(currindex, ec.edge().opposite(n));
				} else if (currindex < middle)
					setL(currindex - weight, ec.edge().opposite(n));
				else
					setL(currindex + weight, ec.edge().opposite(n));
			}
		if (text.contains(" if ")) {
			Node merge = null;
			int direction = 1;
			for (EdgeCursor ec = n.outEdges(); ec.ok(); ec.next())
				if (graph.getRealizer(ec.edge()).getLineColor() == Color.green) {
					if (! graph.getLabelText(ec.edge().target()).contains("IF-MERGE"))
						if (swimLane.get(ec.edge().target()) != null)
							if ((Integer)((SwimLaneDescriptor)swimLane.get(ec.edge().target())).getClientObject() < currindex)
								direction = -1;
					update(direction);
					merge = visitComputations(ec.edge().target(), "IF-MERGE");
					restore(direction);
				} else { //Color == red!
					if (! graph.getLabelText(ec.edge().target()).contains("IF-MERGE"))
						if (swimLane.get(ec.edge().target()) != null)
							if ((Integer)((SwimLaneDescriptor)swimLane.get(ec.edge().target())).getClientObject() > currindex)
								direction = -1;
					update(-1 * direction);
					Node merge2 = visitComputations(ec.edge().target(), "IF-MERGE");
					if (merge == null)
						merge = merge2;
					restore(-1 * direction);
				}
			if (merge != null) {
				setL(currindex, merge);
				visited.set(merge, true);
			}
			return merge;
		}
		if (text.endsWith(" loop")) {
			update(0);
			Node mbreak = visitComputations(n.firstOutEdge().target(), "BREAK");
			restore(0);
			if (mbreak != null) {
				setL(currindex, mbreak);
				visited.set(mbreak, true);
			}
			return mbreak;
		}
		return n;
	}
	
	private void setL (int num, Node n) {
		if (swimLane.get(n) == null) {
			System.out.println("setting swimlane to " + num + " of " + graph.getLabelText(n));
			SwimLaneDescriptor sld = new SwimLaneDescriptor(num);
			swimLane.set(n, sld);
		} else {
		    SwimLaneDescriptor sld = (SwimLaneDescriptor)swimLane.get(n);
		    int oldnum = (Integer)sld.getClientObject();
		    if ((oldnum > (middle + 16)) || (oldnum < (middle - 16))) {
			        System.out.println("resetting swimlane to " + num + " of " + graph.getLabelText(n));
				SwimLaneDescriptor sld2 = new SwimLaneDescriptor(num);
				swimLane.set(n, sld);
		    }
		}
	}

	private void setLf (int num, Node n) {
		if (swimLane.get(n) == null) {
			System.out.println("setting swimlane to " + num + " of " + graph.getLabelText(n));
			SwimLaneDescriptor sld = new SwimLaneDescriptor(num);
			swimLane.set(n, sld);
		} else {
		    System.out.println("resetting swimlane to " + num + " of " + graph.getLabelText(n));
		    SwimLaneDescriptor sld = new SwimLaneDescriptor(num);
		    swimLane.set(n, sld);
		}
		    
	}

}

