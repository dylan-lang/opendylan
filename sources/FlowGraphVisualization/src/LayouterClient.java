import java.awt.Color;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;

import y.base.Edge;
import y.base.EdgeCursor;
import y.base.Node;
import y.base.NodeCursor;
import y.view.EdgeRealizer;


public class LayouterClient extends Thread {
	private Socket socket;
	private BufferedReader reader;
	private PrintWriter writer;
	private ArrayList<IncrementalHierarchicLayout> graphs = new ArrayList<IncrementalHierarchicLayout>();
	
	public LayouterClient (Socket s) {
		try {
			socket = s;
			reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
			writer = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public IncrementalHierarchicLayout getGraph (int index) {
		return graphs.get(index);
	}
	
	private ArrayList readMessage () throws NumberFormatException, IOException {
		char[] buffer = new char[6];
		if (reader.read(buffer, 0, 6) == 6) {
			String size = "";
			for (int i = 0; i < 6; i++)
				size += Character.toString(buffer[i]);
			int message_length = Integer.parseInt(size, 16);
			return read_s_expression(message_length);
		}
		return null;
	}
	
	public void run() {
		try {
			ArrayList answer = readMessage();
			assert(answer.size() == 2);
			assert(answer.get(0) instanceof Symbol);
			assert(answer.get(1) instanceof Symbol);
			assert(((Symbol)answer.get(0)).isEqual("connection-identifier"));
			Symbol identifier = (Symbol)answer.get(1);
			
			DemoBase demo = new DemoBase(identifier.toString(), this);
			demo.start();
			
			while (true) {
				answer = readMessage();
				assert(answer.size() > 1);
				assert(answer.get(0) instanceof Symbol);
				Symbol key = (Symbol)answer.get(0);
				if (key.isEqual("beginning")) {
					assert(answer.get(1) instanceof ArrayList);
					assert(((ArrayList)answer.get(1)).size() == 1);
					assert(((ArrayList)answer.get(1)).get(0) instanceof String);
					String ph = (String)((ArrayList)answer.get(1)).get(0);
					demo.phase.setText(ph);
					demo.phase.validate();
					continue;
				}
				assert(answer.get(1) instanceof Integer);
				int dfm_id = (Integer)answer.get(1);
				IncrementalHierarchicLayout gr = null;
				if (! (key.isEqual("highlight") || key.isEqual("highlight-queue") || key.isEqual("relayouted")))
					System.out.println(key.toString() + " for " + dfm_id + " : " + answer.subList(2, answer.size()));
				if (graphs.size() <= dfm_id) {
					gr = new IncrementalHierarchicLayout(demo, dfm_id);
					graphs.add(gr);
				}
				gr = graphs.get(dfm_id);
				if (key.isEqual("dfm-header")) {
					assert(answer.size() == 3);
					assert(answer.get(2) instanceof ArrayList);
					ArrayList cf = (ArrayList)answer.get(2);
					assert(cf.size() == 3);
					assert(cf.get(0) instanceof Symbol);
					assert(((Symbol)cf.get(0)).isEqual("method"));
					assert(cf.get(1) instanceof Symbol); //method name
					assert(cf.get(2) instanceof String); //arg, val
					gr.initGraph(((Symbol)(cf.get(1))).toString(), (String)cf.get(2));
				} else if (key.isEqual("change-edge")) {
					assert(answer.size() == 6);
					assert(answer.get(2) instanceof Integer);
					assert(answer.get(3) instanceof Integer);
					assert(answer.get(4) instanceof Integer);
					assert(answer.get(5) instanceof Symbol);
					Node from = gr.int_node_map.get((Integer)answer.get(2));
					Node toold = gr.int_node_map.get((Integer)answer.get(3));
					Node tonew = gr.int_node_map.get((Integer)answer.get(4));
					assert(from != null);
					assert(tonew != null);
					if (toold == tonew) {
						System.out.println("got edge with equal toold and tonew");
						continue;
					}
					Symbol label = (Symbol)answer.get(5);
					if (label.isEqual("no"))
						label = null;
					Edge change = null;
					if (toold != null) {
						for (EdgeCursor ec = from.outEdges(); ec.ok(); ec.next())
							if (ec.edge().target() == toold)
								if (label == null || label.isEqual(gr.graph.getRealizer(ec.edge()).getLabelText())) {
									change = ec.edge();
									break;
								}
						if (change != null) {
							gr.graph.changeEdge(change, from, tonew);
							gr.changed = true;
						}
					}
					if (change == null)
						if (gr.safeCreateEdge(from, tonew)) {
							System.out.println("only created edge");
							if (label != null)
								gr.setEdgeLabel(label);
							gr.changed = true;
						}
				} else if (key.isEqual("remove-edge")) {
					assert(answer.size() == 5);
					assert(answer.get(2) instanceof Integer);
					assert(answer.get(3) instanceof Integer);
					assert(answer.get(4) instanceof Symbol);
					Node from = gr.int_node_map.get((Integer)answer.get(2));
					Node to = gr.int_node_map.get((Integer)answer.get(3));
					assert(from != null);
					assert(to != null);
					Symbol label = (Symbol)answer.get(4);
					if (label.isEqual("no"))
						label = null;
					boolean removed = false;
					for (EdgeCursor ec = from.outEdges(); ec.ok(); ec.next())
						if (ec.edge().target() == to)
							if (label == null || label.isEqual(gr.graph.getRealizer(ec.edge()).getLabelText())) {
								gr.graph.removeEdge(ec.edge());
								gr.changed = true;
								removed = true;
								break;
							}
					if (! removed)
						System.out.println("FAILED");
				} else if (key.isEqual("insert-edge")) {
					assert(answer.size() == 5);
					assert(answer.get(2) instanceof Integer);
					assert(answer.get(3) instanceof Integer);
					assert(answer.get(4) instanceof Symbol);
					Symbol label = (Symbol)answer.get(4);
					if (label.isEqual("no"))
						label = null;
					if (gr.safeCreateEdge((Integer)answer.get(2), (Integer)answer.get(3))) {
						if (label != null)
							gr.setEdgeLabel(label);
						gr.changed = true;
					}
				} else if (key.isEqual("new-computation")) {
					assert(answer.size() == 3);
					assert(answer.get(2) instanceof ArrayList);
					ArrayList text = (ArrayList)answer.get(2);
					gr.createNewNode(text);
					gr.changed = true;
				} else if (key.isEqual("remove-computation")) {
					assert(answer.size() == 3);
					assert(answer.get(2) instanceof Integer);
					gr.graph.removeNode(gr.int_node_map.get((Integer)answer.get(2)));
					gr.changed = true;
				} else if (key.isEqual("add-temporary")) {
					assert(answer.size() == 4);
					assert(answer.get(2) instanceof Integer);
					assert(answer.get(3) instanceof Integer);
					int temp_id = (Integer)answer.get(2);
					int c_id = (Integer)answer.get(3);
					gr.createTemporary(temp_id, c_id);
					gr.changed = true;
				} else if (key.isEqual("add-temporary-user")) {
					assert(answer.size() == 4);
					assert(answer.get(2) instanceof Integer);
					assert(answer.get(3) instanceof Integer);
					int temp_id = (Integer)answer.get(2);
					int c_id = (Integer)answer.get(3);
					System.out.println("inserting temporary edge in graph " + dfm_id + " from " + temp_id + " to " + c_id);
					if (gr.safeCreateEdge(temp_id, c_id)) {
						gr.setEdgeColor(Color.pink);
						gr.changed = true;
					}
				} else if (key.isEqual("remove-temporary-user")) {
					assert(answer.size() == 4);
					assert(answer.get(2) instanceof Integer);
					assert(answer.get(3) instanceof Integer);
					int temp_id = (Integer)answer.get(2);
					int c_id = (Integer)answer.get(3);
					Node c = gr.int_node_map.get(c_id);
					for (EdgeCursor ec = gr.int_node_map.get(temp_id).outEdges(); ec.ok(); ec.next())
							if (ec.edge().target() == c) {
								gr.graph.removeEdge(ec.edge());
								gr.changed = true;
								break;
							}
					gr.changed = true;
				} else if (key.isEqual("temporary-generator")) {
					assert(answer.size() == 5);
					assert(answer.get(2) instanceof Integer);
					assert(answer.get(3) instanceof Integer);
					assert(answer.get(4) instanceof Integer);
					Node temp = gr.int_node_map.get((Integer)answer.get(2));
					Node oldgen = gr.int_node_map.get((Integer)answer.get(4));
					Node newgen = gr.int_node_map.get((Integer)answer.get(3));
					assert(temp != null);
					assert(newgen != null);
					boolean found = false;
					if (oldgen != null)
						for (EdgeCursor ec = temp.inEdges(); ec.ok(); ec.next())
							if (ec.edge().source() == oldgen) {
								gr.graph.changeEdge(ec.edge(), temp, newgen);
								gr.changed = true;
								found = true;
								break;
							}
					if (! found)
						if (gr.safeCreateEdge(temp, newgen)) {
							gr.setEdgeColor(Color.pink);
							gr.changed = true;
						}
				} else if (key.isEqual("relayouted")) {
					for (NodeCursor nc = gr.graph.nodes(); nc.ok(); nc.next())
						//if (nc.node().degree() == 0) {
						//	gr.graph.removeNode(nc.node());
						//	gr.changed = true;
						//}
					gr.activateLayouter();
				} else if (key.isEqual("highlight")) {
					gr.activateLayouter();
					int from = (Integer)answer.get(2);
					Node highlightnew = gr.int_node_map.get(from);
					if (highlightnew != null && demo.highlight != highlightnew) {
						if (demo.highlight != null)
							gr.graph.getRealizer(demo.highlight).setFillColor(gr.graph.getDefaultNodeRealizer().getFillColor());
						gr.graph.getRealizer(highlightnew).setFillColor(Color.green);
						demo.highlight = highlightnew;
						demo.contentPane.repaint();
					}
				} else if (key.isEqual("highlight-queue")) {
					gr.activateLayouter();
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
						Node unh = gr.int_node_map.get(rem);
						if (unh != demo.highlight && unh != null)
							gr.graph.getRealizer(unh).setFillColor(gr.graph.getDefaultNodeRealizer().getFillColor());
					}
					for (Integer a : added) {
						demo.opt_queue.add(a);
						Node h = gr.int_node_map.get(a);
						if (h != demo.highlight && h != null)
							gr.graph.getRealizer(h).setFillColor(Color.orange);
					}
					demo.contentPane.repaint();
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private int compute_s_expression_size (Symbol message) {
		return message.toString().length();
	}
	private void write_s_expression (Symbol message) {
		writer.write(message.toString());
	}
	
	private int compute_s_expression_size (String message) {
		return 2 + message.length();
	}
	private void write_s_expression (String message) {
		writer.write((int)'"');
		writer.write(message);
		writer.write((int)'"');
	}
	
	private int compute_s_expression_size (Integer message) {
		return (Integer.toString(message)).length();
	}
	private void write_s_expression (Integer message) {
		writer.write(Integer.toString(message));
	}
	
	private int compute_s_expression_size (ArrayList message) {
		int size = 1; //'(' + ')'
		for (Object s : message) {
			if (s instanceof String)
				size += compute_s_expression_size((String)s);
			else if (s instanceof Symbol)
				size += compute_s_expression_size((Symbol)s);
			else if (s instanceof Integer)
				size += compute_s_expression_size((Integer)s);
			else if (s instanceof ArrayList)
				size += compute_s_expression_size((ArrayList)s);
			size++; //' '
		}
		return size;
	}
	private void write_s_expression (ArrayList message) {
		int i = message.size();
		writer.write((int)'(');
		for (Object s : message) {
			i--;
			if (s instanceof String)
				write_s_expression((String)s);
			else if (s instanceof Symbol)
				write_s_expression((Symbol)s);
			else if (s instanceof Integer)
				write_s_expression((Integer)s);
			else if (s instanceof ArrayList)
				write_s_expression((ArrayList)s);
			if (i > 0)
				writer.write((int)' ');
		}
		writer.write((int)')');
	}
	
	public void printMessage (ArrayList message) {
		int size = compute_s_expression_size(message);
		String sizeb = Integer.toHexString(size);
		for (int i = sizeb.length(); i < 6; i++)
			writer.write((int)'0');
		writer.write(sizeb);
		write_s_expression(message);
		writer.flush();
	}
	
	private enum ParseState { Number, Symbol, String, Nested };
	
	private boolean isWhitespace (char next) {
		if (next == ' ' | next == '\t' | next == '\n' | next == '\r')
			return true;
		return false;
	}
	
	private boolean isNumeric (char n) {
		if (n == '0' | n == '1' | n == '2' | n == '3' | n == '4' |
				n == '5' | n == '6' | n == '7' | n == '8' | n == '9')
			return true;
		return false;
	}

	private int usedTokens = 0;
	private int level = 0;
	private ArrayList read_s_expression (int message_length) throws IOException {
		level++;
		//System.out.println("called with level " + level + " message length " + message_length);
		ArrayList res = new ArrayList();
		ParseState state = ParseState.Nested;
		String result = "";
		boolean first = true;
		for (int i = 0; i < message_length; i++) {
			char next = (char)reader.read();
			//System.out.println("result:" + result + " i:" + i + " state:" + state + " next:" + next);
			switch (state) {
			case Number:
				if (isWhitespace(next)) {
					res.add(Integer.parseInt(result));
					result = "";
					state = ParseState.Nested;
				} else if (isNumeric(next)) {
					result += Character.toString(next);
				} else if (next == ')') {
					res.add(Integer.parseInt(result));
					usedTokens = i + 1;
					level--;
					return res;
				}
				break;
			case Symbol:
				if (isWhitespace(next)) {
					res.add(new Symbol(result));
					result = "";
					state = ParseState.Nested;
				} else if (next == ')') {
					res.add(new Symbol(result));
					usedTokens = i + 1;
					level--;
					return res;
				} else
					result += Character.toString(next);
				break;
			case String:
				if (next == '"') {
					res.add(result);
					result = "";
					state = ParseState.Nested;
				} else {
					if (next == '\\') {
						char nextt = (char)reader.read();
						i++;
						if (nextt == 't') result += "\t";
						else if (nextt == 'r') result += "\r";
						else if (nextt == 'n') result += "\n";
						else result += Character.toString(nextt);
					} else
						result += Character.toString(next);
				}
				break;
			case Nested:
				if (isWhitespace(next)) {
					//do nothing
					break;
				} else if (next == '(') {
					if (first && level == 1) break;
					res.add(read_s_expression(message_length - i - 1));
					i += usedTokens;
				} else if (next == '"') {
					state = ParseState.String;
				} else if (isNumeric(next)) {
					state = ParseState.Number;
					result += Character.toString(next);
				} else if (next == ')') {
					usedTokens = i + 1;
					level--;
					return res;
				} else {
					state = ParseState.Symbol;
					result += Character.toString(next);
				}
				break;
			}
			first = false;
		}
		level--;
		//System.out.println("leaving outer level");
		return res;
	}

}
