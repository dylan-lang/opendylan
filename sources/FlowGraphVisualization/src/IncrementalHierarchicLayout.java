import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;

import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import y.base.EdgeCursor;
import y.base.Node;
import y.base.NodeMap;
import y.layout.hierarchic.ClassicLayerSequencer;
import y.layout.hierarchic.ConstraintLayerer;
import y.layout.hierarchic.HierarchicLayouter;
import y.layout.hierarchic.ConstraintLayerer.ConstraintFactory;
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
	protected HierarchicLayouter hierarchicLayouter;
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
	
	private NodeMap groups;
	private NodeMap groumasters;
	
	public Node selection;
	
	public boolean isok = true;
	
	
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
		hierarchicLayouter = new HierarchicLayouter();
		ConstraintLayerer cl = new ConstraintLayerer();
		cl.setCoreLayerer(hierarchicLayouter.getLayerer());
		hierarchicLayouter.setLayerer(cl);

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
		groups = graph.createNodeMap();
		graph.addDataProvider(ClassicLayerSequencer.GROUP_KEY, groups);
		int_node_map = new HashMap<Integer, Node>();
		opt_queue = new ArrayList<Integer>();
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
		}
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

	public boolean safeCreateEdge (int source, int target) {
		if (int_node_map.get(source) != null)
			return safeCreateEdge(int_node_map.get(source), target);
		System.out.println("FAIL from " + source + " target " + target + " (source not available)");
		return false;
	}
	public boolean safeCreateEdge (int source, Node target) {
		if (int_node_map.get(source) != null)
			return safeCreateEdge(int_node_map.get(source), target);
		System.out.println("FAIL from " + source + " target " + target + " (source not available)");
		return false;
	}
	public boolean safeCreateEdge (Node source, int target) {
		if (int_node_map.get(target) != null)
			return safeCreateEdge(source, int_node_map.get(target));
		System.out.println("FAIL from " + source + " target " + target + " (target not available)");
		return false;
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

	public synchronized void updatephase(String text) {
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
}

