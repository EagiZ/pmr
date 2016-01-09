package controllers;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

import models.Message;

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
	}
	
	/**
	 * Alternative constructor, port number set to 7777.
	 * @param ipAddress IP Address to connect to.
	 */
	public Connection(String ipAddress) {
		this(ipAddress, 7777);
	}
	
	/* === Accessors === */
	
	public String getIpAddress() {return ipAddress;}
	public int getPort() {return port;}
	
	/* === Mutators === */
	/* === Misc. methods === */

	/**
	 * Connects client to ipAddress:port
	 * @param player Player to connect to server
	 * @return Answer from server
	 */
	public String connect(String player) {
		String answer = "";
		try {
			clientSocket = new Socket(ipAddress, port);
			clientSocket.setReceiveBufferSize(5); // Need more research into this
			send = new DataOutputStream(clientSocket.getOutputStream());
			receive = new BufferedReader(new InputStreamReader(
					clientSocket.getInputStream()));
			answer = handshake(player);
		} catch (IOException e) {
			e.printStackTrace();
		} return answer;
	}

	/**
	 * Disconnects client from server.
	 */
	public void disconnect(String player) {
		try {
			send(new Message("disconnect", player));
			clientSocket.close();
			send.close();
			receive.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Send data to server
	 * @param data Data to send to server
	 */
	public void send(Message message) {
		// TODO.
		try {
			send.writeBytes(message.getMessage());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Receive data from server
	 * @return Data received from server
	 */
	public synchronized String receive() {
		// TODO.
		String str = "";
		try {
			//System.out.println("Trying to recieve");
			
			str = receive.readLine();
			//System.out.println("Recieved message: " + str);
		} catch (IOException e) {
			e.printStackTrace();
		} return str;
	}
	
	public synchronized String receiveNotBlocking() {
		String str = null;
		try {
			while(receive.ready()) {
				str = receive();
			}
		} catch (IOException e) {
			e.printStackTrace();
		} return str;
	}
	
	/**
	 * @return All players in game
	 */
	public String refresh() {
		send(new Message("refresh"));
		return receive();
	}
	
	/**
	 * Handshakes with server.
	 * @param  player Player to connect to server
	 * @return Handshake answer from server.
	 */
	private String handshake(String player) {
		send(new Message("connect", player)); // Version information et.c. should be sent here.
		return receive();
	}
}
