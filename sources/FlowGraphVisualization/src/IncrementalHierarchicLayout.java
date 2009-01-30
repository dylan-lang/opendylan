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
import java.util.ArrayList;
import java.util.HashMap;

import y.base.DataMap;
import y.base.EdgeMap;
import y.base.Node;
import y.base.NodeCursor;
import y.base.NodeMap;
import y.layout.PortConstraintKeys;
import y.layout.hierarchic.ClassicLayerSequencer;
import y.layout.hierarchic.GivenLayersLayerer;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.incremental.IncrementalHintsFactory;
import y.layout.hierarchic.incremental.IntValueHolderAdapter;
import y.layout.hierarchic.incremental.OldLayererWrapper;
import y.layout.hierarchic.incremental.SequenceConstraintFactory;
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

/**
 * This demo shows how to use the {@link y.layout.hierarchic.IncrementalHierarchicLayouter} together
 * with sophisticated customized {@link y.view.ViewMode}s.
 * The application will automatically perform a new layout whenever the user
 * makes changes to the graph. <br/>
 * It demonstrates how to use a predetermined layering and how the application
 * can retrieve the layering computed during the layout. The layering information
 * is visualized using a {@link y.view.Drawable} in the canvas. <br/>
 * For a simpler demo that depicts the basics of {@link y.layout.hierarchic.IncrementalHierarchicLayouter}
 * see {@link SimpleIncrementalHierarchicLayouterDemo}.
 * @see y.layout.hierarchic.IncrementalHierarchicLayouter
 */
public class IncrementalHierarchicLayout
{
	protected IncrementalHierarchicLayouter hierarchicLayouter;
	protected HashMap<Integer, Node> int_node_map = new HashMap<Integer, Node>();
	
	protected Graph2D graph;
	private Graph2DView view;
	private DemoBase demobase;
	protected SequenceConstraintFactory scf;

	
	public IncrementalHierarchicLayout(DemoBase db)
	{
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

		
		//hierarchicLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_INCREMENTAL);

		//hierarchicLayouter.getEdgeLayoutDescriptor().setSourcePortOptimizationEnabled(true);
		//hierarchicLayouter.getEdgeLayoutDescriptor().setTargetPortOptimizationEnabled(true);
		//hierarchicLayouter.getEdgeLayoutDescriptor().setOrthogonallyRouted(true);

		// deferred since the mode is created in the super class's constructor
		//paMode.setSpc(sourcePortMap);
		//paMode.setTpc(targetPortMap);
	}
	
	public void activateLayouter () {
		view.setGraph2D(graph);
		demobase.graphChanged(this, graph);
	}

	protected void initGraph (ArrayList controlflow)
	{
		byte oldMode = hierarchicLayouter.getLayoutMode();
		hierarchicLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_FROM_SCRATCH);
		//top level entry, sexp is (?prefix #"METHOD" methodname args->values instr*)
		Node prev = null;

		Object first = controlflow.get(0);
		assert (first instanceof Symbol);
		assert(((Symbol)first).isEqual("method"));
		
		Object namei = controlflow.get(1);
		assert(namei instanceof Symbol);
		Symbol name = (Symbol)namei;
		
		Object args_vals = controlflow.get(2);
		assert(args_vals instanceof String);
		
		prev = createNodeWithLabel(name.toString() + " " + args_vals, -1);

		for (int i = 3; i < controlflow.size(); i++) {
			Object o = controlflow.get(i);
			prev = initGraphHelperHelper(o, prev, -1);
		}
		
		for (Node n : todelete) {
			//assert(n.outDegree() == 1); //well, unwind-protect adds an edge, which is safe to remove
			assert(n.inDegree() == 1);
			Node previous = n.firstInEdge().source();
			Node next = n.firstOutEdge().target();
			String label = graph.getLabelText(n.firstInEdge());
			graph.removeNode(n);
			EdgeRealizer realf = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
			realf.setLabelText(label);
			graph.createEdge(previous, next, realf);
		}
		todelete.clear();
		
		try {
			demobase.calcLayout();
		} finally {
			hierarchicLayouter.setLayoutMode(oldMode);
		}
	}

	
	private Node othernode = null;
	private ArrayList<Node> todelete = new ArrayList<Node>();
	public Node oldhighlight;
	
	protected Node initGraphHelperHelper (Object o, Node prev, int id) {
		Node curr = null;
		if (o instanceof String)
			curr = initGraphHelper((String)o, prev, id);
		else if (o instanceof Symbol)
			curr = initGraphHelper((Symbol)o, prev, id);
		else if (o instanceof Integer)
			curr = initGraphHelper((Integer)o, prev, id);
		else if (o instanceof ArrayList)
			curr = initGraphHelper((ArrayList)o, prev, id);
		return curr;
	}

	private Node initGraphHelper (ArrayList nodelist, Node prev, int id) {
		Node last = null;
		if (nodelist.size() == 0)
			return prev;
		//temporary hack
		String t = null;
		int index = 0;
		if (nodelist.get(0) instanceof Symbol) {
			Symbol s = (Symbol)nodelist.get(0);
			if (s.isEqual("temporary")) {
				assert(nodelist.get(1) instanceof String);
				t = (String)nodelist.get(1);
				index = 2;
			} else if (s.isEqual("local")) {
				//local method: local, method, name, args, body
				assert(nodelist.size() > 4);
				assert(nodelist.get(1) instanceof Symbol);
				assert(((Symbol)nodelist.get(1)).isEqual("method"));
				assert(nodelist.get(2) instanceof Symbol);
				assert(nodelist.get(3) instanceof String);
				Node header = createNodeWithLabel("local method " + ((Symbol)nodelist.get(2)).toString() + " " + (String)nodelist.get(3), -1);
				Node p = header;
				for (int i = 4; i < nodelist.size(); i++)
					p = initGraphHelperHelper(nodelist.get(i), p, -1);
				return prev;
			}
		} 

		int comp_id = -1;
		if (nodelist.get(index) instanceof Integer) {
			comp_id = (Integer)nodelist.get(index);
			index++;
		}
		if (nodelist.get(index) instanceof Symbol) {
			Symbol s = (Symbol)nodelist.get(index);
			index++;
			if (s.isEqual("if")) {
				//test, exactly one element, a string
				assert(nodelist.get(index) instanceof ArrayList);
				Object n = (ArrayList)nodelist.get(index);
				index++;
				ArrayList testnodes = (ArrayList) n;
				assert(testnodes.size() == 1);
				assert(testnodes.get(0) instanceof String);
				Node test = createNodeWithLabel((String)testnodes.get(0), comp_id);
				changeLabel(test, "if ", true);
				graph.createEdge(prev, test);
				
				//consequence
				assert(nodelist.get(index) instanceof ArrayList);
				Node consequence = initGraphHelperHelper((ArrayList)nodelist.get(index), test, -1);
				index++;
				
				EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
				myreal.setLabelText("true");
				Node firstc = null;
				if (consequence == test) {
					consequence = createNodeWithLabel("", -1);
					firstc = consequence;
					todelete.add(consequence);
				} else {
					assert(test.outDegree() == 1);
					firstc = test.firstOutEdge().target();
					graph.removeEdge(test.firstOutEdge());
				}
				
				//alternative
				assert(nodelist.get(index) instanceof ArrayList);
				Node alternative = initGraphHelperHelper((ArrayList)nodelist.get(index), test, -1);
				index++;
				
				EdgeRealizer realf = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
				realf.setLabelText("false");
				Node firsta = null;
				if (alternative == test) {
					alternative = createNodeWithLabel("", -1);
					firsta = alternative;
					todelete.add(alternative);
				} else {
					firsta = test.lastOutEdge().target();
					graph.removeEdge(test.firstOutEdge());
				}
				graph.createEdge(test, firstc, myreal);
				graph.createEdge(test, firsta, realf);

				othernode = alternative;
				return consequence;
			} else if (s.isEqual("loop")) {
				Node loop = createNodeWithLabel("loop", comp_id);
				if (comp_id > 0)
					int_node_map.put(comp_id, loop);
				graph.createEdge(prev, loop);
				assert(nodelist.size() == 3);
				assert(nodelist.get(index) instanceof ArrayList);
				return loopHelper((ArrayList)nodelist.get(index), loop);
			} else if (s.isEqual("unwind-protect")) {
				assert(nodelist.size() == 5);
				//entry-state
				assert(nodelist.get(index) instanceof ArrayList);
				ArrayList entrystate = (ArrayList)nodelist.get(index);
				index++;
				
				assert(entrystate.size() == 1);
				assert(entrystate.get(0) instanceof String);
				Node up = createNodeWithLabel("unwind-protect [entry-state: " + (String)entrystate.get(0) + "]", comp_id);
				if (comp_id > 0)
					int_node_map.put(comp_id, up);
				graph.createEdge(prev, up);
				//body
				assert(nodelist.get(index) instanceof ArrayList);
				Node lastbody = initGraphHelper((ArrayList)nodelist.get(index), up, -1);
				index++;
				
				//cleanup ("end-protected-block entry-state: " entry-state)
				assert(nodelist.get(index) instanceof ArrayList);
				Node lastcleanup = initGraphHelper((ArrayList)nodelist.get(index), lastbody, -1);
				index++;
				
				assert(lastbody.outDegree() == 1);
				Node firstcleanup = lastbody.firstOutEdge().target();
				boolean active = false;
				for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()) {
					EdgeRealizer real = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
					real.setLineColor(Color.red);
					Node n = nc.node();
					if (n == up)
						active = true;
					if (n == lastbody)
						active = false;
					if (active)
						graph.createEdge(n, firstcleanup, real);
				}
				return lastcleanup;
			} else if (s.isEqual("bind-exit")) {
				assert(nodelist.size() == 4);
				assert(nodelist.get(index) instanceof ArrayList);
				ArrayList entrystate = (ArrayList)nodelist.get(index);
				index++;
				
				assert(entrystate.size() == 1);
				assert(entrystate.get(0) instanceof String);
				Node start = createNodeWithLabel("bind-exit [entry-state: " + (String)entrystate.get(0) + "]", comp_id);
				if (comp_id > 0)
					int_node_map.put(comp_id, start);
				graph.createEdge(prev, start);
				
				assert(nodelist.get(index) instanceof ArrayList);
				Node lastbody = initGraphHelper((ArrayList)nodelist.get(index), start, -1);
				index++;
				//transform calls to entry-state to reflect this in CF!
				return lastbody;
			}
		}

		for (int i = index; i < nodelist.size(); i++) {
			Object o = nodelist.get(i);
			if (last == null) last = prev;
			last = initGraphHelperHelper(o, last, comp_id);
			if (t != null) {
				changeLabel(last, t + " = ", true);
				t = null;
			}
		}
		return last;
	}
	
	private Node initGraphHelper (String node, Node prev, int id) {
		Node t = createNodeWithLabel(node, id);
		connect(prev, t);
		return t;
	}
	
	private Node initGraphHelper (Symbol node, Node prev, int id) {
		Node t = createNodeWithLabel(node.toString(), id);
		connect(prev, t);
		return t;
	}
	
	private Node initGraphHelper (Integer node, Node prev, int id) {
		Node t = createNodeWithLabel(Integer.toString(node), id);
		connect(prev, t);
		return t;
	}
	
	private void connect (Node prev, Node t) {
		if (prev != null) {
			graph.createEdge(prev, t);
			if (othernode != null) {
				graph.createEdge(othernode, t);
				othernode = null;
			}
		}
	}

	private Node loopHelper(ArrayList nodelist, Node loop) {
		Node breaks = null;
		//first, an if (with empty else)
		//the interesting part is CONTINUE (args) <- loop-call
		//and BREAK <- end-loop
		Node last = initGraphHelper(nodelist, loop, -1);
		boolean active = false;
		for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()) {
			Node n = nc.node();
			//groupMap.set(n, loop-id);
			if (n == loop)
				active = true;
			if (active)
				if (graph.getRealizer(n).getLabel().getText().contains("[CONTINUE")) {
					assert(n.outDegree() == 1);
					Node target = n.firstOutEdge().target();
					if (! graph.getRealizer(target).getLabel().getText().equals("loop")) {
						graph.changeEdge(n.firstOutEdge(), n, loop);
					}
				}
				else if (graph.getRealizer(n).getLabel().getText().equals("BREAK")) {
					if (n.outDegree() == 0)
						breaks = n;
				}
			if (n == last)
				active = false;
		}
		assert(breaks == last);
		return breaks;
	}
	
	private Node createNodeWithLabel (String label, int id) {
		NodeRealizer n1 = new GenericNodeRealizer(graph.getDefaultNodeRealizer());
		NodeLabel nl1 = n1.createNodeLabel();
		nl1.setText(label);
		n1.setLabel(nl1);
		n1.setWidth(nl1.getWidth() + 10);
		Node n = graph.createNode(n1);
		System.out.println("added node " + id + " : " + label);
		int_node_map.put(id, n);
		return n;
	}
	
	private void changeLabel (Node n, String app, boolean append) {
		NodeLabel nl = graph.getRealizer(n).getLabel();
		if (append)
			nl.setText(app + nl.getText());
		else
			nl.setText(app);
		graph.getRealizer(n).setWidth(nl.getWidth());
	}
	
}

