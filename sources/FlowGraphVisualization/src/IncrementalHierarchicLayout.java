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

import y.anim.AnimationFactory;
import y.anim.AnimationPlayer;
import y.base.DataMap;
import y.base.Edge;
import y.base.EdgeCursor;
import y.base.EdgeList;
import y.base.EdgeMap;
import y.base.Node;
import y.base.NodeCursor;
import y.base.NodeList;
import y.base.NodeMap;
import y.geom.YPoint;
import y.layout.BufferedLayouter;
import y.layout.GraphLayout;
import y.layout.NodeLayout;
import y.layout.PortConstraint;
import y.layout.PortConstraintKeys;
import y.layout.hierarchic.GivenLayersLayerer;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.incremental.IncrementalHintsFactory;
import y.layout.hierarchic.incremental.IntValueHolderAdapter;
import y.layout.hierarchic.incremental.OldLayererWrapper;
import y.util.Maps;
import y.view.Arrow;
import y.view.Bend;
import y.view.BendCursor;
import y.view.BendList;
import y.view.BridgeCalculator;
import y.view.CreateEdgeMode;
import y.view.DefaultGraph2DRenderer;
import y.view.Drawable;
import y.view.EdgeRealizer;
import y.view.EditMode;
import y.view.GenericEdgeRealizer;
import y.view.GenericNodeRealizer;
import y.view.Graph2D;
import y.view.HitInfo;
import y.view.HotSpotMode;
import y.view.LayoutMorpher;
import y.view.LineType;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.PopupMode;
import y.view.PortAssignmentMoveSelectionMode;
import y.view.YLabel;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.event.ActionEvent;
import java.awt.geom.Rectangle2D;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
public class IncrementalHierarchicLayout extends DemoBase
{
	private EdgeMap sourcePortMap;
	private EdgeMap targetPortMap;
	private LayerDrawable layerDrawable;
	private NodeMap layerIdMap;
	private DataMap hintMap;

	private PortAssignmentMoveSelectionMode paMode;

	private IncrementalHierarchicLayouter hierarchicLayouter;
	private IncrementalHintsFactory hintsFactory;
	private GivenLayersLayerer gll;
	private Graph2D graph;
	
	public IncrementalHierarchicLayout()
	{
		graph = view.getGraph2D();

		// make it look nice
		EdgeRealizer defaultER = graph.getDefaultEdgeRealizer();
		defaultER.setArrow(Arrow.STANDARD);

		// enable bridges for PolyLineEdgeRealizer
		BridgeCalculator bridgeCalculator = new BridgeCalculator();
		bridgeCalculator.setCrossingMode( BridgeCalculator.CROSSING_MODE_HORIZONTAL_CROSSES_VERTICAL );
		((DefaultGraph2DRenderer) view.getGraph2DRenderer()).setBridgeCalculator(bridgeCalculator );

		// allocate a couple of maps
		layerIdMap = graph.createNodeMap();
		sourcePortMap = graph.createEdgeMap();
		targetPortMap = graph.createEdgeMap();
		hintMap = Maps.createHashedDataMap();

		// register them with the graph
		graph.addDataProvider(PortConstraintKeys.SOURCE_PORT_CONSTRAINT_KEY,sourcePortMap);
		graph.addDataProvider(PortConstraintKeys.TARGET_PORT_CONSTRAINT_KEY,targetPortMap);
		graph.addDataProvider(GivenLayersLayerer.LAYER_ID_KEY, layerIdMap);
		graph.addDataProvider(IncrementalHierarchicLayouter.INCREMENTAL_HINTS_DPKEY, hintMap);
	    graph.addDataProvider(IncrementalHierarchicLayouter.LAYER_VALUE_HOLDER_DPKEY, new IntValueHolderAdapter(layerIdMap));

		// create a drawable that displays layers
		this.layerDrawable = new LayerDrawable(graph, layerIdMap);
		view.addBackgroundDrawable(layerDrawable);

		// create and configure the layout algorithm
		hierarchicLayouter = new IncrementalHierarchicLayouter();
		hierarchicLayouter.setFixedElementsLayerer(new OldLayererWrapper(gll = new GivenLayersLayerer()));
		hintsFactory = hierarchicLayouter.createIncrementalHintsFactory();
		hierarchicLayouter.setComponentLayouterEnabled(false);
		hierarchicLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_INCREMENTAL);

		hierarchicLayouter.getEdgeLayoutDescriptor().setSourcePortOptimizationEnabled(true);
		hierarchicLayouter.getEdgeLayoutDescriptor().setTargetPortOptimizationEnabled(true);
		hierarchicLayouter.getEdgeLayoutDescriptor().setOrthogonallyRouted(true);

		// deferred since the mode is created in the super class's constructor
		paMode.setSpc(sourcePortMap);
		paMode.setTpc(targetPortMap);
	}

	protected void registerViewModes() {
		EditMode editMode = new IncrementalEditMode();
		editMode.setMoveSelectionMode(paMode = new IncrementalMoveSelectionMode());
		editMode.setPopupMode(new IncrementalPopupMode());
		editMode.setCreateEdgeMode(new IncrementalEdgeCreateMode());
		editMode.setHotSpotMode(new IncrementalHotSpotMode());
		view.addViewMode( editMode );
	}

	protected JToolBar createToolBar()
	{
		JToolBar bar = super.createToolBar();
		bar.add(new LayoutAction());
		return bar;
	}
	
	protected void initGraph (ArrayList controlflow)
	{
		byte oldMode = hierarchicLayouter.getLayoutMode();
		hierarchicLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_FROM_SCRATCH);
		//top level entry, sexp is (?prefix #"METHOD" methodname args->values instr*)
		Node prev = null;
		int index = 0;
		Object first = controlflow.get(index);
		if (first instanceof Symbol) {
			assert(((Symbol)first).isEqual(new Symbol("METHOD")));
		} else {
			assert(first instanceof String);
			index++;
		}
		index++;
		
		Object namei = controlflow.get(index);
		assert(namei instanceof Symbol);
		Symbol name = (Symbol)namei;
		index++;
		
		Object args_vals = controlflow.get(index);
		assert(args_vals instanceof String);
		index++;
		
		prev = createNodeWithLabel(name.toString() + " " + args_vals);

		for (int i = index; i < controlflow.size(); i++) {
			Object o = controlflow.get(i);
			prev = initGraphHelperHelper(o, prev);
		}

		try {
			calcLayout();
		} finally {
			hierarchicLayouter.setLayoutMode(oldMode);
		}
	}

	
	private Node othernode = null;
	private EdgeRealizer edgerealizer = null;
	
	private Node initGraphHelperHelper (Object o, Node prev) {
		Node curr = null;
		if (o instanceof String)
			curr = initGraphHelper((String)o, prev);
		else if (o instanceof Symbol)
			curr = initGraphHelper((Symbol)o, prev);
		else if (o instanceof Integer)
			curr = initGraphHelper((Integer)o, prev);
		else if (o instanceof ArrayList)
			curr = initGraphHelper((ArrayList)o, prev);
		return curr;
	}

	private Node initGraphHelper (ArrayList nodelist, Node prev) {
		Node last = null;
		if (nodelist.size() == 0)
			return prev;
		//temporary hack
		String t = null;
		int index = 0;
		if (nodelist.get(0) instanceof Symbol) {
			Symbol s = (Symbol)nodelist.get(0);
			if (s.isEqual(new Symbol("temporary"))) {
				assert(nodelist.get(1) instanceof String);
				t = (String)nodelist.get(1);
				index = 2;
			} else if (s.isEqual(new Symbol("if"))) {
				//test, exactly one element, a string
				Object n = nodelist.get(1);
				assert(n instanceof ArrayList);
				ArrayList testnodes = (ArrayList) n;
				assert(testnodes.size() == 1);
				assert(testnodes.get(0) instanceof String);
				Node test = createNodeWithLabel((String)testnodes.get(0));
				changeLabel(test, "if ", true);
				graph.createEdge(prev, test);
				
				//consequence
				assert(nodelist.get(2) instanceof ArrayList);
				Node consequence = initGraphHelperHelper((ArrayList)nodelist.get(2), test);
				assert(test.outDegree() == 1);
				Node firstc = test.firstOutEdge().target();
				graph.removeEdge(test.firstOutEdge());
				EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
				myreal.setLabelText("true");
				
				//alternative
				assert(nodelist.get(3) instanceof ArrayList);
				Node alternative = initGraphHelperHelper((ArrayList)nodelist.get(3), test);
				EdgeRealizer realf = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
				realf.setLabelText("false");
				if (alternative == test) {
					edgerealizer = realf;
				} else {
					assert(test.outDegree() == 1);
					Node firsta = test.firstOutEdge().target();
					graph.removeEdge(test.firstOutEdge());
					graph.createEdge(test, firsta, realf);
				}
				graph.createEdge(test, firstc, myreal);

				
				othernode = alternative;
				return consequence;
			} else if (s.isEqual(new Symbol("loop"))) {
				Node loop = createNodeWithLabel("loop");
				graph.createEdge(prev, loop);
				assert(nodelist.size() == 2);
				assert(nodelist.get(1) instanceof ArrayList);
				return loopHelper((ArrayList)nodelist.get(1), loop);
			} else if (s.isEqual(new Symbol("unwind-protect"))) {
				assert(nodelist.size() == 4);
				//entry-state
				assert(nodelist.get(1) instanceof ArrayList);
				ArrayList entrystate = (ArrayList)nodelist.get(1);
				assert(entrystate.size() == 1);
				assert(entrystate.get(0) instanceof String);
				Node up = createNodeWithLabel("unwind-protect [entry-state: " + (String)entrystate.get(0) + "]");
				graph.createEdge(prev, up);
				//body
				assert(nodelist.get(2) instanceof ArrayList);
				Node lastbody = initGraphHelper((ArrayList)nodelist.get(2), up);
				
				//cleanup ("end-protected-block entry-state: " entry-state)
				assert(nodelist.get(3) instanceof ArrayList);
				Node lastcleanup = initGraphHelper((ArrayList)nodelist.get(3), lastbody);
				
				
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
			} else if (s.isEqual(new Symbol("bind-exit"))) {
				assert(nodelist.size() == 3);
				assert(nodelist.get(1) instanceof ArrayList);
				ArrayList entrystate = (ArrayList)nodelist.get(1);
				assert(entrystate.size() == 1);
				assert(entrystate.get(0) instanceof String);
				Node start = createNodeWithLabel("bind-exit [entry-state: " + (String)entrystate.get(0) + "]");
				graph.createEdge(prev, start);
				
				assert(nodelist.get(2) instanceof ArrayList);
				Node lastbody = initGraphHelper((ArrayList)nodelist.get(2), start);
				//transform calls to entry-state to reflect this in CF!
				return lastbody;
			} else if (s.isEqual(new Symbol("local"))) {
				//local method: local, method, name, args, body
				assert(nodelist.size() == 5);
				assert(nodelist.get(1) instanceof Symbol);
				assert(((Symbol)nodelist.get(1)).isEqual(new Symbol("method")));
				assert(nodelist.get(2) instanceof Symbol);
				assert(nodelist.get(3) instanceof String);
				Node header = createNodeWithLabel("local method " + ((Symbol)nodelist.get(2)).toString() + " " + (String)nodelist.get(3));
				assert(nodelist.get(4) instanceof ArrayList);
				initGraphHelper((ArrayList)nodelist.get(4), header);
				return prev;
			}
		} 
		for (int i = index; i < nodelist.size(); i++) {
			Object o = nodelist.get(i);
			if (last == null) last = prev;
			last = initGraphHelperHelper(o, last);
			if (t != null) {
				changeLabel(last, t + " = ", true);
				t = null;
			}
		}
		return last;
	}
	
	private Node initGraphHelper (String node, Node prev) {
		Node t = createNodeWithLabel(node);
		graph.createEdge(prev, t);
		if (othernode != null) {
			if (edgerealizer != null)
				graph.createEdge(othernode, t, edgerealizer);
			else
				graph.createEdge(othernode, t);
			edgerealizer = null;
			othernode = null;
		}
		return t;
	}
	
	private Node initGraphHelper (Symbol node, Node prev) {
		Node t = createNodeWithLabel(node.toString());
		graph.createEdge(prev, t);
		if (othernode != null) {
			if (edgerealizer != null)
				graph.createEdge(othernode, t, edgerealizer);
			else
				graph.createEdge(othernode, t);
			edgerealizer = null;
			othernode = null;
		}
		return t;
	}
	
	private Node initGraphHelper (Integer node, Node prev) {
		Node t = createNodeWithLabel(Integer.toString(node));
		graph.createEdge(prev, t);
		if (othernode != null) {
			if (edgerealizer != null)
				graph.createEdge(othernode, t, edgerealizer);
			else
				graph.createEdge(othernode, t);
			edgerealizer = null;
			othernode = null;
		}
		return t;
	}
	
	private Node loopHelper(ArrayList nodelist, Node loop) {
		Node breaks = null;
		//first, an if (with empty else)
		//the interesting part is CONTINUE (args) <- loop-call
		//and BREAK <- end-loop
		Node last = initGraphHelper(nodelist, loop);
		boolean active = false;
		for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()) {
			Node n = nc.node();
			if (n == loop)
				active = true;
			if (active)
				if (graph.getRealizer(n).getLabel().getText().startsWith("[CONTINUE")) {
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
	
	private Node createNodeWithLabel (String label) {
		NodeRealizer n1 = new GenericNodeRealizer(graph.getDefaultNodeRealizer());
		NodeLabel nl1 = n1.createNodeLabel();
		nl1.setText(label);
		n1.setLabel(nl1);
		n1.setWidth(nl1.getWidth() + 10);
		return graph.createNode(n1);
	}
	
	private void changeLabel (Node n, String app, boolean append) {
		NodeLabel nl = graph.getRealizer(n).getLabel();
		if (append)
			nl.setText(app + nl.getText());
		else
			nl.setText(app);
		graph.getRealizer(n).setWidth(nl.getWidth());
	}
	
	/**
	 * Simple Layout action (incremental)
	 */
	final class LayoutAction extends AbstractAction
	{
		LayoutAction()
		{
			super("Layout");
			URL imageURL = ClassLoader.getSystemResource("demo/view/resource/Layout16.gif");
			if (imageURL != null){
				this.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
			}
			this.putValue( Action.SHORT_DESCRIPTION, "Layout");
		}
		public void actionPerformed(ActionEvent ev)
		{
			calcLayout();
		}
	}

	/**
	 * Simple Layout action (from scratch)
	 */
	final class FreshLayoutAction extends AbstractAction
	{
		boolean resetPCs;
		FreshLayoutAction(String name, boolean resetPCs)
		{
			super(name);
			this.resetPCs = resetPCs;
		}

		public void actionPerformed(ActionEvent ev)
		{
			if (resetPCs){
				for (EdgeCursor ec = view.getGraph2D().edges(); ec.ok(); ec.next()){
					sourcePortMap.set(ec.edge(), null);
					targetPortMap.set(ec.edge(), null);
				}
			}
			byte oldMode = hierarchicLayouter.getLayoutMode();
			hierarchicLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_FROM_SCRATCH);
			try {
				calcLayout();
			} finally {
				hierarchicLayouter.setLayoutMode(oldMode);
			}
		}
	}
	/**
	 * Optimizes nodes (inserts or recalculates layouts incrementally)
	 */
	final class OptimizeNodesAction extends AbstractAction {
		private NodeCursor nc;
		private boolean resetPCs;
		public OptimizeNodesAction(String name, NodeCursor nodes, boolean resetPCs){
			super(name);
			this.nc = nodes;
			this.resetPCs = resetPCs;
		}

		public void actionPerformed(ActionEvent ae){
			this.nc.toFirst();
			for (NodeCursor nc = this.nc; nc.ok(); nc.next()){
				Node v = nc.node();
				hintMap.set(v, hintsFactory.createLayerIncrementallyHint(v));
				if (resetPCs) {
					for (EdgeCursor ec = v.edges(); ec.ok(); ec.next()){
						if (ec.edge().source() == v){
							sourcePortMap.set(ec.edge(), null);
						} else {
							targetPortMap.set(ec.edge(), null);
						}
					}
				}
			}
			calcLayout();
			this.nc.toFirst();
			for (NodeCursor nc = this.nc; nc.ok(); nc.next()){
				Node v = nc.node();
				hintMap.set(v, null);
			}
		}
	}

	/**
	 * Fixes nodes (inserts or recalculates layouts incrementally)
	 */
	final class FixNodesAction extends AbstractAction {
		private final NodeCursor nc;
		private final boolean layer;
		private final boolean sequence;

		public FixNodesAction(String name, NodeCursor nodes, boolean layer, boolean sequence){
			super(name);
			this.nc = nodes;
			this.layer = layer;
			this.sequence = sequence;
		}

		public void actionPerformed(ActionEvent ae){
			this.nc.toFirst();
			for (NodeCursor nc = this.nc; nc.ok(); nc.next()){
				Node v = nc.node();
				if (layer && sequence) {
					hintMap.set(v, hintsFactory.createUseExactCoordinatesHint(v));
					NodeRealizer realizer = view.getGraph2D().getRealizer(v);
					realizer.setFillColor(Color.red);
					realizer.repaint();
				} else if (layer) {
					hintMap.set(v, hintsFactory.createUseExactLayerCoordinatesHint(v));
					NodeRealizer realizer = view.getGraph2D().getRealizer(v);
					realizer.setFillColor(Color.red.darker());
					realizer.repaint();
				} else if (sequence) {
					hintMap.set(v, hintsFactory.createUseExactSequenceCoordinatesHint(v));
					NodeRealizer realizer = view.getGraph2D().getRealizer(v);
					realizer.setFillColor(Color.red.darker().darker());
					realizer.repaint();
				} else {
					hintMap.set(v, null);
					NodeRealizer realizer = view.getGraph2D().getRealizer(v);
					realizer.setFillColor(view.getGraph2D().getDefaultNodeRealizer().getFillColor());
					realizer.repaint();
				}
			}
		}
	}

	/**
	 * Optimizes edges (inserts or recalculates layouts incrementally)
	 */
	final class OptimizeEdgesAction extends AbstractAction {
		private EdgeCursor ec;
		private boolean resetPCs;
		public OptimizeEdgesAction(String name, EdgeCursor edges, boolean resetPCs){
			super(name);
			this.ec = edges;
			this.resetPCs = resetPCs;
		}

		public void actionPerformed(ActionEvent ae){
			this.ec.toFirst();
			for (EdgeCursor ec = this.ec; ec.ok(); ec.next()){
				final Edge edge = ec.edge();
				hintMap.set(edge, hintsFactory.createSequenceIncrementallyHint(edge));
				if (resetPCs) {
					sourcePortMap.set(edge, null);
					targetPortMap.set(edge, null);
				}
			}
			calcLayout();
			this.ec.toFirst();
			for (EdgeCursor ec = this.ec; ec.ok(); ec.next()){
				Edge e = ec.edge();
				hintMap.set(e, null);
			}
		}
	}

	/**
	 * Drawable implementation and utility functions
	 */
	static final class LayerDrawable implements Drawable {


		private List layers = new ArrayList(20);
		private static final Color[] colors = new Color[] {new Color(255, 255, 150), new Color(255, 255, 100)};

		private Rectangle bounds = new Rectangle(20,20,200,200);

		private Graph2D graph;
		private NodeMap layerIdMap;

		LayerDrawable(Graph2D graph, NodeMap layerIdMap){
			this.graph = graph;
			this.layerIdMap = layerIdMap;
		}

		public Rectangle getBounds()
		{
			return bounds;
		}

		public void updateLayers(){
			final double spacing = 20.0d;
			layers.clear();
			if (graph.N() < 1) return;
			double minX = Double.MAX_VALUE, maxX = -Double.MAX_VALUE;
			for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()){
				final Node node = nc.node();
				final int layer = layerIdMap.getInt(node);
				if (layer < 0) continue;
				while (layers.size() - 1 < layer){
					layers.add(new Rectangle2D.Double(0,0,-1,-1));
				}
				Rectangle2D.Double layerRect = (Rectangle2D.Double) layers.get(layer);
				final NodeLayout nl = graph.getNodeLayout(node);
				if (layerRect.width < 0){
					layerRect.setFrame(nl.getX(), nl.getY(), nl.getWidth(), nl.getHeight());
				} else {
					layerRect.add(nl.getX(), nl.getY());
					layerRect.add(nl.getX() + nl.getWidth(), nl.getY() + nl.getHeight());
				}
				minX = Math.min(nl.getX(), minX);
				maxX = Math.max(nl.getX() + nl.getWidth(), maxX);
			}

			double minY = Double.MAX_VALUE;
			double maxY = -Double.MAX_VALUE;
			for (int i = 0; i < layers.size(); i++){
				Rectangle2D.Double rect = (Rectangle2D.Double) layers.get(i);
				rect.x = minX - spacing;
				rect.width = maxX - minX + spacing * 2;
				if (i == 0){
					rect.y -= spacing;
					rect.height += spacing;
					minY = rect.y;
				}
				if (i == layers.size() - 1){
					rect.height += spacing;
					maxY = rect.height + rect.y;
				} else if (i < layers.size() - 1){
					Rectangle2D.Double nextRect = (Rectangle2D.Double) layers.get(i + 1);
					final double mid = (rect.getY() + rect.getHeight() + nextRect.getY()) * 0.5d;
					rect.height += mid - (rect.y + rect.height);
					final double nextDelta = mid - nextRect.y;
					nextRect.y += nextDelta;
					nextRect.height -= nextDelta;
				}
			}
			bounds.setFrame(minX - spacing, minY, maxX - minX + 2 * spacing, maxY - minY);
			graph.updateViews();
		}

		public final int inset = 8;

		public int getLayerId(double x, double y){
			if (x < bounds.x - outerInsets || x > bounds.x + bounds.width + outerInsets){
				return Integer.MAX_VALUE;
			}
			if (y < bounds.y + inset){
				return -1;
			}
			if (y > bounds.y + bounds.height - inset){
				return layers.size();
			}
			for (int i = 0; i < layers.size(); i++){
				final Rectangle2D.Double rect= (Rectangle2D.Double) layers.get(i);
				if (y >= rect.y + inset && y <= rect.y + rect.height - inset){
					return i;
				} else if (y < rect.y + inset){
					return -(i+1);
				}
			}
			return Integer.MAX_VALUE;
		}

		public static final double outerInsets = 40;

		public Rectangle2D getLayerBounds(int layer){
			if (layer >= 0 && layer < layers.size()){
				Rectangle2D.Double rect = (Rectangle2D.Double) layers.get(layer);
				rect = new Rectangle2D.Double(rect.x, rect.y + inset, rect.width, rect.height - 2 * inset);
				return rect;
			}
			if (layer == -1){
				return new Rectangle2D.Double(bounds.x, bounds.y - outerInsets, bounds.width, outerInsets + inset);
			}
			if (layer >= layers.size() && (layer != Integer.MAX_VALUE)){
				return new Rectangle2D.Double(bounds.x, bounds.y + bounds.height - inset, bounds.width, outerInsets);
			}
			if (layer < 0){
				int beforeLayer = -(layer + 1);
				if (beforeLayer < layers.size()){
					Rectangle2D.Double rect = (Rectangle2D.Double) layers.get(beforeLayer);
					rect = new Rectangle2D.Double(rect.x, rect.y - inset, rect.width, 2 * inset);
					return rect;
				}
			}
			return new Rectangle2D.Double(bounds.x - 2 * outerInsets, bounds.y -  2 * outerInsets, bounds.width + 4.0d * outerInsets, bounds.height + outerInsets * 4.0d);
		}

		public void paint(Graphics2D g)
		{
			for (int i = 0; i < layers.size(); i++){
				Color color = colors[i % colors.length];
				g.setColor(color);
				g.fill((Shape) layers.get(i));
			}
		}
	}

	/**
	 * Animated layout assignment
	 */
	public void calcLayout(){
		if (!view.getGraph2D().isEmpty()){
			gll.normalize(view.getGraph2D(), layerIdMap, layerIdMap);
			Cursor oldCursor = view.getCanvasComponent().getCursor();
			try {
				view.getCanvasComponent().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				GraphLayout result = new BufferedLayouter(hierarchicLayouter).calcLayout(view.getGraph2D());
				LayoutMorpher morpher = new LayoutMorpher(view, result);
				morpher.setSmoothViewTransform(true);
				morpher.setPreferredDuration(300);
				final AnimationPlayer player = new AnimationPlayer();
				player.addAnimationListener(view);
				player.setFps(120);
				player.animate(AnimationFactory.createEasedAnimation(morpher));
			} finally {
				view.getCanvasComponent().setCursor(oldCursor);
			}
		}
		layerDrawable.updateLayers();
		view.updateView();
	}

	/**
	 * Recalculate layout after resizings
	 */
	final class IncrementalHotSpotMode extends HotSpotMode {
		public void mouseReleasedLeft(double x, double y)
		{
			super.mouseReleasedLeft(x, y);
			calcLayout();
		}
	}

	/**
	 * Recalculate layout after edge creation
	 */
	final class IncrementalEdgeCreateMode extends CreateEdgeMode {
		protected void edgeCreated(Edge edge)
		{
			super.edgeCreated(edge);
			EdgeRealizer er = view.getGraph2D().getRealizer(edge);
			if (er.bendCount()>0){
				parseBend(er.getBend(0));
			}
			if (er.bendCount()>1){
				parseBend(er.getBend(er.bendCount()-1));
			}
			if (er.bendCount() == 0){
				hintMap.set(edge, hintsFactory.createSequenceIncrementallyHint(edge));
			}
			calcLayout();
			hintMap.set(edge, null);
		}
	}

	/**
	 * Recalculate layout after node creation
	 */
	final class IncrementalEditMode extends EditMode {
		protected void nodeCreated(Node v)
		{
			super.nodeCreated(v);
			final YPoint center = view.getGraph2D().getCenter(v);
			int layerId = layerDrawable.getLayerId(center.x, center.y);
			setLayers(new NodeList(v).nodes(), layerId, Integer.MAX_VALUE);
			if (lastReleaseEvent.isControlDown()) { // fix the nodes position
				hintMap.set(v, hintsFactory.createUseExactCoordinatesHint(v));
				view.getGraph2D().getRealizer(v).setFillColor(Color.red);
			}
			calcLayout();
		}
	}

	/**
	 * Utility method to assign nodes to a new layer.
	 */
	protected void setLayers(NodeCursor nodes, int newLayer, int previousLayer){
		if (!nodes.ok()) return;
		//calculate number of layers to insert
		int lesserLayers = 0;
		int greaterLayers = 0;
		final Set nodeSet = new HashSet();
		for (nodes.toFirst(); nodes.ok(); nodes.next()){
			nodeSet.add(nodes.node());
		}
		if (previousLayer != Integer.MAX_VALUE){
			for (nodes.toFirst(); nodes.ok(); nodes.next()){
				int pLayer = layerIdMap.getInt(nodes.node());
				if (pLayer < previousLayer){
					lesserLayers = Math.max(lesserLayers, previousLayer - pLayer);
				}
				if (pLayer > previousLayer){
					greaterLayers = Math.max(greaterLayers, pLayer - previousLayer);
				}
			}
		} else {
			previousLayer = 0;
		}
		final int newLayerCount = lesserLayers + greaterLayers + 1;
		if (newLayer < 0){
			int beforeLayer = -(newLayer + 1);
			for (NodeCursor nc = view.getGraph2D().nodes(); nc.ok(); nc.next()){
				if (!nodeSet.contains(nc.node())){
					int oldLayer = layerIdMap.getInt(nc.node());
					if (oldLayer >= beforeLayer){
						layerIdMap.setInt(nc.node(), oldLayer + newLayerCount);
					}
				}
			}
			for (nodes.toFirst(); nodes.ok(); nodes.next()){
				int oldLayer = layerIdMap.getInt(nodes.node());
				layerIdMap.setInt(nodes.node(), beforeLayer + lesserLayers + oldLayer - previousLayer);
			}
		} else {
			if (newLayer == Integer.MAX_VALUE){
				int maxLayer = -1;
				for (NodeCursor nc = view.getGraph2D().nodes(); nc.ok(); nc.next()){
					if (!nodeSet.contains(nc.node())){
						int layer = layerIdMap.getInt(nc.node());
						maxLayer = Math.max(layer, maxLayer);
					}
				}
				newLayer = maxLayer + 1;
			}
			if (lesserLayers > 0 || greaterLayers > 0){
				for (NodeCursor nc = view.getGraph2D().nodes(); nc.ok(); nc.next()){
					if (!nodeSet.contains(nc.node())){
						int layer = layerIdMap.getInt(nc.node());
						if (layer == newLayer) {
							layerIdMap.setInt(nc.node(), layer + lesserLayers);
						} else if (layer > newLayer){
							layerIdMap.setInt(nc.node(), layer + newLayerCount);
						}
					}
				}
			}
			for (nodes.toFirst(); nodes.ok(); nodes.next()){
				int oldLayer = layerIdMap.getInt(nodes.node());
				layerIdMap.setInt(nodes.node(), newLayer + lesserLayers + oldLayer - previousLayer);
			}
		}
	}

	/**
	 * Utility method to assign PCs from the sketch
	 */
	public void parseBend(Bend b){
		Edge e = b.getEdge();
		EdgeRealizer er = view.getGraph2D().getRealizer(e);
		if (b == er.getBend(0)){
			YPoint center = view.getGraph2D().getCenter(e.source());
			sourcePortMap.set(e, getPortConstraint(b.getX() - center.x, b.getY() - center.y));
		}
		if (b == er.getBend(er.bendCount()-1)){
			YPoint center = view.getGraph2D().getCenter(e.target());
			targetPortMap.set(e, getPortConstraint(b.getX() - center.x, b.getY() - center.y));
		}
	}

	/**
	 * Helper method to assign PCs from the sketch
	 */
	private static final PortConstraint getPortConstraint(final double bdx, final double bdy){
		if (Math.abs(bdx) > Math.abs(bdy)){
			return PortConstraint.create(bdx > 0 ? PortConstraint.EAST : PortConstraint.WEST);
		} else {
			return PortConstraint.create(bdy > 0 ? PortConstraint.SOUTH : PortConstraint.NORTH);
		}
	}

	/**
	 * Provides popups for all kinds of actions
	 */
	final class IncrementalPopupMode extends PopupMode {

		public JPopupMenu getNodePopup(final Node v)
		{
			JPopupMenu pm = new JPopupMenu();
			NodeCursor node = new NodeList(v).nodes();
			addNodeActions(pm, node);
			return pm;
		}

		private void addNodeActions(JPopupMenu pm, NodeCursor node) {
			pm.add(new OptimizeNodesAction("Optimize Node", node, false));
			pm.add(new OptimizeNodesAction("Optimize Node and Reset PCs", node, true));
			JMenu fixNodesMenu = new JMenu("Fix nodes");
			pm.add(fixNodesMenu);
			fixNodesMenu.add(new FixNodesAction("Fix Coordinates", node, true, true));
			fixNodesMenu.add(new FixNodesAction("Fix Layer Coordinates", node, true, false));
			fixNodesMenu.add(new FixNodesAction("Fix Sequence Coordinates", node, false, true));
			fixNodesMenu.add(new FixNodesAction("Unfix Coordinates", node, false, false));
		}

		public JPopupMenu getEdgePopup(final Edge e)
		{
			JPopupMenu pm = new JPopupMenu();
			addEdgeActions(pm, new EdgeList(e).edges());
			return pm;
		}

		public JPopupMenu getSelectionPopup(double x, double y)
		{
			JPopupMenu pm = new JPopupMenu();
			final NodeCursor snc = getGraph2D().selectedNodes();
			if (snc.ok()){
				addNodeActions(pm, snc);
			} else {
				final EdgeCursor sec = getGraph2D().selectedEdges();
				if (sec.ok()){
					addEdgeActions(pm, sec);
				} else {
					return null;
				}
			}
			return pm;
		}

		private void addEdgeActions(JPopupMenu pm, EdgeCursor sec) {
			pm.add(new OptimizeEdgesAction("Optimize Edges", sec, false));
			pm.add(new OptimizeEdgesAction("Optimize Edges and Reset PCs", sec, true));
		}

		public JPopupMenu getPaperPopup(double x, double y)
		{
			if (getGraph2D().isEmpty()) return null;
			JPopupMenu pm = new JPopupMenu();
			pm.add(new FreshLayoutAction("Fresh Layout", false));
			pm.add(new FreshLayoutAction("Fresh Layout and Reset PCs", true));
			if (getGraph2D().E() > 0){
				addEdgeActions(pm, getGraph2D().edges());
			}
			return pm;
		}
	}

	/**
	 * Recalculate layout after selection move
	 */
	final class IncrementalMoveSelectionMode extends PortAssignmentMoveSelectionMode {
		private boolean firstTime = true;
		private MoveSelectionDrawable drawable;
		private NodeList selectedNodes;
		private BendList selectedBends;

		public IncrementalMoveSelectionMode(){
			super(null, null);
		}

		protected void selectionMovedAction(double dx, double dy, double x, double y)
		{
			super.selectionMovedAction(dx, dy, x, y);
			if (selectedNodes != null){
				view.removeBackgroundDrawable(drawable);
				drawable = null;
				int newLayer = layerDrawable.getLayerId(x, y);
				HitInfo hi = this.getLastHitInfo();
				Node movedNode = hi.getHitNode();
				int originalLayer = movedNode != null ? layerIdMap.getInt(movedNode) : Integer.MAX_VALUE;
				if (newLayer != originalLayer){
					setLayers(selectedNodes.nodes(), newLayer, originalLayer);
				}
				if (newLayer != Integer.MAX_VALUE){
					List hints = new ArrayList(128);
					for (NodeCursor nc = selectedNodes.nodes(); nc.ok(); nc.next()){
						for (EdgeCursor edges = nc.node().edges(); edges.ok(); edges.next()){
							hints.add(edges.edge());
							hintMap.set(edges.edge(), hintsFactory.createSequenceIncrementallyHint(edges.edge()));
						}
					}
					calcLayout();
					for (int i = 0; i < hints.size(); i++){
						hintMap.set(hints.get(i), null);
					}
				} else {
					List hints = new ArrayList(128);
					for (NodeCursor nc = selectedNodes.nodes(); nc.ok(); nc.next()){
						hints.add(nc.node());
						hintMap.set(nc.node(), hintsFactory.createLayerIncrementallyHint(nc.node()));
					}
					calcLayout();
					for (int i = 0; i < hints.size(); i++){
						Node node = (Node) hints.get(i);
						hintMap.set(node, null);
					}
					layerDrawable.updateLayers();
				}
				selectedNodes = null;
			} else if (selectedBends != null){
				calcLayout();
			}
			selectedBends = null;
			selectedNodes = null;
			firstTime = true;
		}

		protected void selectionOnMove(double dx, double dy, double x, double y)
		{
			if (firstTime){
				firstTime = false;
				Graph2D g = getGraph2D();
				NodeCursor nc = g.selectedNodes();
				selectedBends = null;
				selectedNodes = null;
				if (nc.ok()){
					selectedNodes = new NodeList(nc);
					drawable = new MoveSelectionDrawable();
					view.addBackgroundDrawable(drawable);
				}
				BendCursor bc = g.selectedBends();
				if (selectedNodes == null && bc.ok()){
					selectedBends = new BendList(bc);
				}
			}
			super.selectionOnMove(dx, dy, x, y);
			if (selectedNodes != null) {
				int layer = layerDrawable.getLayerId(x, y);
				drawable.layer = layer;
				drawable.layerCount = layerDrawable.layers.size();
				drawable.drawable = layerDrawable.getLayerBounds(layer);
			}
		}

		final class MoveSelectionDrawable implements Drawable {
			Shape drawable;
			int layer;
			int layerCount;
			Color color = Color.red;
			Color color2 = Color.orange;
			Color color3 = Color.red.darker();

			public Rectangle getBounds()
			{
				return drawable.getBounds();
			}

			public void paint(Graphics2D g)
			{
				Stroke s = g.getStroke();
				if (layer == Integer.MAX_VALUE){
					g.setColor(color3);
					g.setStroke(LineType.DOTTED_3);
					g.draw(drawable);
				} else {
					if (layer >= 0 && layer < layerCount){
						g.setColor(color);
						g.setStroke(LineType.LINE_3);
						g.draw(drawable);
					} else {
						g.setColor(color2);
						g.fill(drawable);
					}
				}
				g.setStroke(s);
			}
		}
	}

	/**
	 * Launches this demo.
	 */

}

