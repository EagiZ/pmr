package views;

import org.newdawn.slick.*;

public class Game extends BasicGame {
	
	Input input;
	
	public Game()
    {
        super("Putt my redneck");
    }	

	@Override
	public void init(GameContainer container) throws SlickException {
		
	}

	@Override
	public void update(GameContainer container, int delta) throws SlickException {

	}


	@Override
	public void render(GameContainer container, Graphics g) throws SlickException {;

	}
	
	public static void main(String[] arguments)
    {
        try
        {
            AppGameContainer app = new AppGameContainer(new Game());
            app.setDisplayMode(1024, 768, false);
            app.setVSync(true);
            app.start();
        }
        catch (SlickException e)
        {
            e.printStackTrace();
        }
    }

}
