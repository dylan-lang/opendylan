import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;


public class FlowGraphVisualizer {
	private static final int port = 1234;
	
	public static void main(String[] args) {
		DemoBase.initLnF();
		FlowGraphVisualizer.listen();
	}

	static void listen () {
		try {
			ServerSocket sock = new ServerSocket(port);
			while (true) {
				Socket cli = sock.accept();
				new LayouterClient(cli).start();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
