import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;


public class TCPListener {
	private final int port = 1234;
	
	void listen () {
		try {
			ServerSocket sock = new ServerSocket(1234);
			while (true) {
				Socket cli = sock.accept();
				new LayouterClient(cli).start();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
