package views;

import controllers.*;

import org.newdawn.slick.*;
import org.newdawn.slick.geom.Circle;

public class Game extends BasicGame {
	Input input;
	Circle circle;
	Connection testCon = new Connection("127.0.0.1");
	
	public Game() {
        super("Putt my redneck");
    }	

	@Override
	public void init(GameContainer container) throws SlickException {
		circle = new Circle(400.0f, 400.0f, 20.0f);
	}

	@Override
	public void update(GameContainer container, int delta) throws SlickException {
		input = container.getInput();
		
		float newX = input.getMouseX();
		float newY = input.getMouseY();
				
		circle.setCenterX(newX);
		circle.setCenterY(newY);
		
		testCon.send("move " + "newX: " + newX + " newY: " + newY + "\n");
		System.out.println(testCon.receive());
	}


	@Override
	public void render(GameContainer container, Graphics g) throws SlickException {
		g.draw(circle);
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
