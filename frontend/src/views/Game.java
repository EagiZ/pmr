package views;

import controllers.*;
import models.*;

import org.newdawn.slick.*;
import org.newdawn.slick.geom.Circle;
import org.newdawn.slick.geom.Vector2f;

public class Game extends BasicGame {
	Input input;
	Player player;
	
	//Connection testCon = new Connection("79.102.55.164");
	Connection testCon = new Connection("127.0.0.1");
	public Game() {
        super("Putt my redneck");
    }	

	@Override
	public void init(GameContainer container) throws SlickException {
		player = new Player(400, 400, 10, "Kalle Klåpare");
		
		
		
	}

	@Override
	public void update(GameContainer container, int delta) throws SlickException {
		player.update();
		
		//testCon.send("move " + "newX: " + newX + " newY: " + newY + "\n");
		//testCon.receive(); //TODO, do something with the result
	}
	
	@Override
	public void mouseDragged(int oldx, int oldy, int newx, int newy) {
		// TODO: render some kind of animation to show the force of the shot.
	}
	
	@Override
	public void mouseReleased(int button, int x, int y) {
		System.out.println("new posx: " + x + " new posy: " + y);
		Vector2f playerPos = player.getPosition();
		Vector2f dragVec = new Vector2f(playerPos.x - x, playerPos.y - y);
		
		player.setVelocity(dragVec.scale(0.01f));
		
		// TODO: here's where we should send data to the server.
	}


	@Override
	public void render(GameContainer container, Graphics g) throws SlickException {
		g.draw(player.getHitbox());
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
