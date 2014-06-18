import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

/**
 *
 */
public class Client {
	/* === Fields === */
	/* === Constructors === */
	
	/** Standard contructor */
	//public Client() {}
	
	/* === Accessors === */
	/* === Mutators === */
	/* === Misc. Functions === */

	/**
	 * Connects client to ipAddress:port
	 * @param ipAddress IP Address to connect to.
	 * @param port port number to connect to.
	 */
	public void connect(String ipAddress, int port) {
		try {
			Socket clientSocket = new Socket(ipAddress, port);
			
			BufferedReader read = 
					new BufferedReader(new InputStreamReader(System.in));
			DataOutputStream send = 
					new DataOutputStream(clientSocket.getOutputStream());
			BufferedReader receive =
					new BufferedReader(new InputStreamReader(
							clientSocket.getInputStream()));
			
			String toServ, fromServ;
			
			toServ = read.readLine() + '\n';
			send.writeBytes(toServ);
			fromServ = receive.readLine();
			
			System.out.println("Server says: " + fromServ);
			
			clientSocket.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/* === Main === */
	
	public static void main(String[] args) {
		Client client = new Client();		
		client.connect("127.0.0.1", 5555);
	}
}
