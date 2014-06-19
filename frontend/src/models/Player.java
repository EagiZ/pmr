package models;

import org.newdawn.slick.geom.Circle;
import org.newdawn.slick.geom.Vector2f;

/**
 * The player class. Represents a player and contains information
 * about its position, hitbox, velocity et.c.
 */
public class Player {
	
	/* === Fields === */

	private Circle hitbox;
	private Vector2f velocity;
	private String playerName;
	
	/* === Constructors === */
	
	/**
	 * @param x		The player's position on the x-axis
	 * @param y		The player's position on the y-axis
	 * @param playerName	The player name
	 */
	public Player(int x, int y, int radius, String playerName) {
		hitbox = new Circle(x, y, radius);
		velocity = new Vector2f(0, 0);
		this.setPlayerName(playerName);
	}
	
	/* === Accessors === */
	
	/** @return the hitbox cirlce of the player. */
	public Circle getHitbox() {return hitbox;}
	/** @return the position vector of the player. */
	public Vector2f getPosition() {return hitbox.getLocation();}
	/** @return the velocity vector of the player. */
	public Vector2f getVelocity() {return velocity;}
	/** @return the player name of the player. */
	public String getPlayerName() {return playerName;}
	
	/* === Mutators === */
	
	/** @param velocity new velocity vector to set */
	public void setVelocity(Vector2f velocity) {this.velocity = velocity;}
	/** @param playerName new name of the player to set */
	public void setPlayerName(String playerName) {this.playerName = playerName;}
	
	/* === Misc. Functions === */
	
	/**
	 * Updates position of player with the current velocity data.
	 */
	public void update() {
		hitbox.setLocation(hitbox.getLocation().add(velocity));
	}
}
