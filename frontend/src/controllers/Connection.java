package controllers;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

/**
 * Connection class communicates with a game server.
 */
public class Connection {
	
	/* === Fields === */
	
	/** */
	private BufferedReader receive;
	/** */
	private DataOutputStream send;
	/** */
	private Socket clientSocket;
	/** IP address of game server */
	private String ipAddress;
	/** Port number of game server */
	private int port;
	
	/* === Constructors === */
	
	/**
	 * Standard constructor.
	 * @param ipAddress IP Address to connect to.
	 * @param port port number to connect to.
	 */
	public Connection(String ipAddress, int port) {
		this.ipAddress = ipAddress;
		this.port = port;
		connect();
	}
	
	/**
	 * Alternate constructor, port number set to 7777.
	 * @param ipAddress IP Address to connect to.
	 */
	public Connection(String ipAddress) {
		this(ipAddress, 7777);
	}
	
	/* === Accessors === */
	
	/** @return IP address of game server */
	public String getIpAddress() {return ipAddress;}
	/** @return Port number of game server */
	public int getPort() {return port;}
	
	/* === Mutators === */
	/* === Misc. Functions === */

	/**
	 * Connects client to ipAddress:port
	 */
	private void connect() {
		try {
			Socket clientSocket = new Socket(ipAddress, port);
			send = new DataOutputStream(clientSocket.getOutputStream());
			receive = new BufferedReader(new InputStreamReader(
					clientSocket.getInputStream()));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 *  Disconnects client from server.
	 */
	public void disconnect() {
		try {
			clientSocket.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Send data to server
	 * @param data Data to send to server
	 */
	public void send(String data) {
		// TODO.
		try {
			send.writeBytes(data + '\n');
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Reveive data from server
	 * @return Data received from server
	 */
	public String receive() {
		// TODO.
		String str = "";
		try {
			str = receive.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		} return str;
	}
}
