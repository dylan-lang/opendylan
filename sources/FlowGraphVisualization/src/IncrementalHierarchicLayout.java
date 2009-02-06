/****************************************************************************
 **
 ** This file is part of yFiles-2.6.0.1. 
 ** 
 ** yWorks proprietary/confidential. Use is subject to license terms.
 **
 ** Redistribution of this file or of an unauthorized byte-code version
 ** of this file is strictly forbidden.
 **
 ** Copyright (c) 2000-2009 by yWorks GmbH, Vor dem Kreuzberg 28, 
 ** 72070 Tuebingen, Germany. All rights reserved.
 **
 ***************************************************************************/

import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;

import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import y.base.EdgeCursor;
import y.base.Node;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.incremental.SequenceConstraintFactory;
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
	protected SequenceConstraintFactory scf;

	public boolean changed = false;
	public int numChanges = 0;
	protected Hashtable<Integer, JLabel> sliderLabels = new Hashtable<Integer, JLabel>();
	protected int lastEntry = 0;
	private int lastChangeCount = 0;
	public ArrayList<ArrayList> changes = new ArrayList<ArrayList>();

	private int lastslidervalue = 0;
	protected boolean graphfinished = false;
	
	protected final int graph_id;
	private ArrayList<Node> arguments = new ArrayList<Node>();
	private Node bind = null;

	
	public IncrementalHierarchicLayout(DemoBase db, int id)
	{
		graph_id = id;
		graph = new Graph2D();
		view = db.view;
		demobase = db;
		
		// make it look nice
		EdgeRealizer defaultER = graph.getDefaultEdgeRealizer();
		defaultER.setArrow(Arrow.STANDARD);

		// enable bridges for PolyLineEdgeRealizer
		BridgeCalculator bridgeCalculator = new BridgeCalculator();
		bridgeCalculator.setCrossingMode( BridgeCalculator.CROSSING_MODE_HORIZONTAL_CROSSES_VERTICAL );
		((DefaultGraph2DRenderer) view.getGraph2DRenderer()).setBridgeCalculator(bridgeCalculator );

		// create and configure the layout algorithm
		hierarchicLayouter = new IncrementalHierarchicLayouter();
		
		scf = hierarchicLayouter.createSequenceConstraintFactory(graph);
		
		sliderLabels.put(0, new JLabel("initial DFM models"));
		changes.add(new ArrayList());
	}
	
	public void activateLayouter () {
		if (demobase.incrementallayouter != this) {
			changed = true;
			view.setGraph2D(graph);
			demobase.graphChanged(this);
		} else
			demobase.calcLayout();
	}

	protected void initGraph (String name, String args)
	{
		Node header = createNodeWithLabel(name + " " + args, -1);
		assert(bind != null);
		graph.createEdge(header, bind); //edge to bind!
		scf.addPlaceNodeAtHeadConstraint(header);
		
		for (Node n : arguments) {
			EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
			myreal.setLineColor(Color.blue);
			graph.createEdge(header, n, myreal);
			
		}
		
		changed = true;
		if (! graphfinished)
		    demobase.addGraph(graph_id, name);
		activateLayouter();
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
				
			//alternative
			assert(nodelist.get(4) instanceof ArrayList);
			ArrayList<Node> alternative = getNodes((ArrayList)nodelist.get(4));
				
			if (alternative.size() > 0) {
				graph.createEdge(test, alternative.get(0));
				setEdgeLabel("false");
			}

		} else if (s.isEqual("loop")) {
			Node loop = createNodeWithLabel("loop", comp_id);
			assert(nodelist.size() == 3);
			assert(nodelist.get(2) instanceof ArrayList);
				
			ArrayList<Node> body = getNodes((ArrayList)nodelist.get(2));

			//groupMap.set(n, loop-id);
			if (body.size() > 0)
				graph.createEdge(loop, body.get(0));
		} else if (s.isEqual("loop-call")) {
			assert(nodelist.size() == 3);
			assert(nodelist.get(2) instanceof Integer);
			Node loop = int_node_map.get((Integer)nodelist.get(2));
			Node loopc = createNodeWithLabel("CONTINUE", comp_id);
			graph.createEdge(loopc, loop);
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
		if (label.equalsIgnoreCase("[BIND]") && bind == null)
			bind = n;
		assert(int_node_map.get(id) == null);
		int_node_map.put(id, n);
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

	public void createTemporary(int temp_id, int c_id) {
		Node t = createNodeWithLabel("temporary", temp_id);
		if (bind == null)
			arguments.add(t);
		graph.getRealizer(t).setFillColor(Color.magenta);
		if (safeCreateEdge(c_id, t))
			setEdgeColor(Color.pink);
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

	public synchronized void updatephase(String text, boolean minor) {
		demobase.phase.setText(text);
		demobase.phase.validate();
		final String txt = text;
		boolean incremented = false;
		if (lastChangeCount < numChanges) {
			incremented = true;
			lastEntry++;
			lastChangeCount = numChanges;
		}
		String slidertext = text;
		if (minor)
			slidertext = "   " + slidertext;
		JLabel foo = new JLabel(slidertext);
		sliderLabels.put(lastEntry, foo);
		demobase.slider.setMaximum(lastEntry);
		Runnable updateMyUI = new Runnable() {
			public void run () {
				demobase.slider.updateUI();		
				demobase.slider.setValue(lastEntry);
				if (txt.equals("finished")) {
					graphfinished = true;
					changed = true;
				}
			}
		};
		if (incremented || ! minor)
			changes.add(new ArrayList());
		if (! minor && ! incremented) {
			lastEntry++;
			lastChangeCount = numChanges;
		}
		SwingUtilities.invokeLater(updateMyUI);
		lastslidervalue = lastEntry;
	}

	public void resetGraph(int step) {
		if (step >= lastslidervalue)
			for (int i = lastslidervalue; i < step; i++)
				for (Object comm : changes.get(i)) {
					ArrayList com = (ArrayList)comm;
					Commands.processCommand(this, com, demobase);
				}
		else { //I was too lazy to implement undo, so do the graph from scratch
			System.out.println("scratch");
			graph = new Graph2D();
			int_node_map = new HashMap<Integer, Node>();
			bind = null;
			for (int i = 0; i < step; i++)
				for (Object comm : changes.get(i)) {
					ArrayList com = (ArrayList)comm;
					//System.out.println("doing " + com);
					Commands.processCommand(this, com, demobase);
				}
		}
		lastslidervalue = step;
		arguments = new ArrayList<Node>();
		changed = true;
		view.setGraph2D(graph);
		demobase.calcLayout();
	}
}

