package views;

import controllers.*;
import models.*;

import org.newdawn.slick.*;
import org.newdawn.slick.geom.Circle;
import org.newdawn.slick.geom.Line;
import org.newdawn.slick.geom.Vector2f;

import com.eclipsesource.json.JsonObject;

public class Game extends BasicGame {
	
	Input input;
	Player player;
	Dragline dragline;
	Boolean playerPressed = false;
	Connection connection;
	
	public Game() {
        super("Putt My Redneck");
    }	

	@Override
	public void init(GameContainer container) throws SlickException {
		//player = new Player("Kalle Kleauparret");
		player = new Player("Kalle Kleauparret");
		dragline = new Dragline();
		connection = new Connection("127.0.0.1");
		connection.connect(Player.toJSON(player).toString());
	}

	@Override
	public void update(GameContainer container, int delta) throws SlickException {
		dragline.setStart(player.getPosition());
		dragline.update();
		player.update();
		//testCon.receive(); //TODO, do something with the result
	}
	
	@Override
	public void mouseDragged(int oldx, int oldy, int newx, int newy) {
		// TODO: render some kind of animation to show the force of the shot.
		if(playerPressed) {
			dragline.setEnd(new Vector2f(newx, newy));
		}
	}
	
	@Override
	public void mousePressed(int button, int x, int y) {
		Circle playerHitBox = player.getHitbox();
		
		if (playerHitBox.contains(x, y)) {
			playerPressed = true;
			
			Vector2f playerPos = player.getPosition();
			dragline.setAlive(true);
			dragline.setLine(new Line(playerPos, playerPos));
		} else {
			playerPressed = false;
		}
	}
	
	@Override
	public void mouseReleased(int button, int x, int y) {
		if (playerPressed) {
			Vector2f playerPos = player.getPosition();
			Vector2f dragVec = new Vector2f(playerPos.x - x, playerPos.y - y);
			
			player.setVelocity(dragVec.scale(0.01f));
			
			// TODO: here's where we should send data to the server.
			
			String sampleJSON = Player.toJSON(player).toString();
			
			connection.send(new Message("", sampleJSON));
			
			String testStr = connection.receive();
			player = Player.fromJSON(JsonObject.readFrom(testStr));
			
			dragline.setAlive(false);
		} 
	}


	@Override
	public void render(GameContainer container, Graphics g) throws SlickException {
		if(dragline.isAlive()) { 
			g.setColor(dragline.getColor());
			g.draw(dragline.getLine());
			System.out.println("\t FDFD: " + dragline.getColor().toString());
		}
		g.setColor(new Color(0xff, 0, 0x80));
		g.fill(player.getHitbox());
	}
	
	public static void main(String[] arguments) {
        try {
        	AppGameContainer app = new AppGameContainer(new Game());
            app.setDisplayMode(1024, 768, false);
            app.setVSync(true);
            app.start();
        } catch (SlickException e) {
            e.printStackTrace();
        }
    }
}
