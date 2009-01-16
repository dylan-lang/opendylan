import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;


public class LayouterClient extends Thread {
	private Socket socket;
	private BufferedReader reader;
	private PrintWriter writer;
	
	public LayouterClient (Socket s) {
		try {
			socket = s;
			reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
			writer = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void run() {
		char[] buffer = new char[6];
		try {
			while (true) {
				if (reader.read(buffer, 0, 6) == 6) {
					String size = "";
					for (int i = 0; i < 6; i++)
						size += Character.toString(buffer[i]);
					int message_length = Integer.parseInt(size, 16);
					ArrayList result = read_s_expression(message_length);
					System.out.println("read [" + message_length + "]: " + result);
				} else break;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
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
	
	private ArrayList read_s_expression (int message_length) throws IOException {
		ArrayList res = new ArrayList();
		ParseState state = ParseState.Nested;
		String result = "";
		boolean first = true;
		for (int i = 0; i < message_length; i++) {
			char next = (char)reader.read();
			//System.out.println("next " + next + " state " + state + " res " + result + " i is " + i + " lenght " + message_length);
			switch (state) {
			case Number:
				if (isWhitespace(next) | next == ')') {
					res.add(Integer.parseInt(result));
					result = "";
					state = ParseState.Nested;
				} else if (isNumeric(next))
					result += Character.toString(next);
				else
					System.out.println("unknown numeric token: " + next + " in " + result);
				break;
			case Symbol:
				if (isWhitespace(next) | next == ')') {
					res.add(result);
					result = "";
					state = ParseState.Nested;
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
					if (first) break;
					res.add(read_s_expression(message_length - i - 1));
					i += usedTokens;
				} else if (next == '"') {
					state = ParseState.String;
				} else if (isNumeric(next)) {
					state = ParseState.Number;
					result += Character.toString(next);
				} else if (next == ')') {
					usedTokens = i + 1;
					return res;
				} else {
					state = ParseState.Symbol;
					result += Character.toString(next);
				}
				break;
			}
			first = false;
		}
		System.out.println("should not happen");
		return res;
	}

}
