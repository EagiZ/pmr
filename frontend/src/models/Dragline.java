package models;

import org.newdawn.slick.Color;
import org.newdawn.slick.geom.Line;
import org.newdawn.slick.geom.Vector2f;

public class Dragline {
	
	/* === Fields === */
	
	private Line line;
	private Color color;
	private boolean isAlive;
	
	/* === Constructors === */
	
	/**
	 * Null constructor
	 */
	public Dragline() {
		Vector2f zero = new Vector2f(0, 0);
		line = new Line(zero, zero);
		color = new Color(0, 0, 0);
		isAlive = false;
	}
	
	/**
	 * @param start start
	 * @param end end
	 */
	public Dragline(Vector2f start, Vector2f end) {
		this();
		line = new Line(start, end);
		color = new Color(0xff, 0, 0);
		isAlive = false;
	}
	
	/* === Accessors === */
	
	public Vector2f getStart() {return line.getStart().copy();}
	public Vector2f getEnd() {return line.getEnd().copy();}
	public Line getLine() {
		return new Line(line.getStart(), line.getEnd());}
	public Color getColor() {return new Color(color);}
	public boolean isAlive() {return isAlive;}
	
	/* === Mutators === */
	
	public void setStart(Vector2f start) {
		line.set(start, line.getEnd());}
	public void setEnd(Vector2f end) {
		line.set(line.getStart(), end);}
	public void setLine(Line line) {this.line = line;}
	public void setLine(Vector2f start, Vector2f end) {
		this.line = new Line(start, end);}
	public void setColor(Color color) {this.color = color;}
	public void setAlive(boolean isAlive) {this.isAlive = isAlive;}
	
	/* === Misc. methods === */
	
	public void update() {
		if(isAlive) {
			// TODO: Fix this, color is not changing correctly.
			float lengthColor = 255 - line.length();
			lengthColor = (lengthColor < 0 ? 0 : lengthColor);
			System.out.println(line.length() + " length -/- lc " + lengthColor);
			setColor(new Color(0xff, (int) lengthColor, 0x0));
			
			System.out.println(getColor().toString());
		}
	}
}
