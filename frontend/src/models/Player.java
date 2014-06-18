package models;

/**
 * The player class. Represents a player and contains information
 * about its position.
 */
public class Player {
	private float x;
	private float y;
	private float xAccel;
	private float yAccel;
	
	private String name;
	
	/**
	 * @param x		The player's position on the x-axis
	 * @param y		The player's position on the y-axis
	 * @param playerName	The player name
	 */
	public Player(int x, int y, String playerName) {
		this.x = x;
		this.y = y;
		this.name = playerName;
	}
	
	public void setX(int x) {
		this.x = x;
	}
	
	public void setY(int y) {
		this.y = y;
	}
	
	
}
