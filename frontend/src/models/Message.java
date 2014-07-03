package models;

public class Message {
	
	/* === Fields === */
	
	private String type;
	private String data;
	
	/* === Constructors === */
	
	/**
	 * Standard constructor
	 * @param type Type of the message
	 * @param message Message
	 */
	public Message(String type, String data) {
		this.type = type;
		this.data = data;
	}
	
	/**
	 * Alternative constructor, data field is set to empty string.
	 * @param type Type of the message
	 */
	public Message(String type) {
		this(type, "");
	}
	
	/* === Accessors === */
	
	public String getType() {return type;}
	public String getData() {return data;}
	/** @return type and data concatenated, use this when sending data.*/
	public String getMessage() {
		String message = type + ("" == data ? "" : " " + data) + '\n';
		return message;
	}
	
	/* === Mutators === */
	
	public void setType(String type) {this.type = type;}
	public void setData(String data) {this.data = data;}
	
	/* === Misc. methods === */
	
}
