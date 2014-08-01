package views;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import controllers.*;
import models.*;

import org.newdawn.slick.*;
import org.newdawn.slick.geom.Circle;
import org.newdawn.slick.geom.Line;
import org.newdawn.slick.geom.Vector2f;

import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonValue;



public class Game extends BasicGame {	
	Input input;
	Player player;
	Dragline dragline;
	Boolean playerPressed = false;
	Connection connection;
	private Set<Player> players = Collections.synchronizedSet(new HashSet<Player>());
	
	public Game() {
        super("Putt My Redneck");
    }	

	@Override
	public void init(GameContainer container) throws SlickException {
		//player = new Player("Kalle Kleauparret");
		player = new Player("Kalle Kleauparret");
		dragline = new Dragline();
		connection = new Connection("127.0.0.1");
		player = Player.fromJSON(JsonObject.readFrom(
				connection.connect(Player.toJSON(player).toString())));
		players = playersToSet(JsonArray.readFrom(connection.refresh()));

		Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
		    public void run() {
		    	connection.disconnect();
		    }
		}));
	}

	@Override
	public void update(GameContainer container, int delta) throws SlickException {
		dragline.setStart(player.getPosition());
		dragline.update();
		
		for(Player p : players) {
			if(p.getUserID() == player.getUserID()) {
				player = p;
			} else {
				p.update();
			}
		} player.update();
		
		players = playersToSet(JsonArray.readFrom(connection.receive())); //TODO, do something with the result
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
		//players = playersToSet(JsonArray.readFrom(connection.refresh()));
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
			
			connection.send(new Message("move", sampleJSON));
			
			String testStr = connection.receive();
			players = playersToSet(JsonArray.readFrom(testStr));
			
			dragline.setAlive(false);
		} 
	}


	@Override
	public void render(GameContainer container, Graphics g) throws SlickException {
		g.setColor(new Color(0xff, 0x80, 0));
		for(Player p : players) {
			if(p.getUserID() != player.getUserID()) g.fill(p.getHitbox());
		}
		
		if(dragline.isAlive()) { 
			g.setColor(dragline.getColor());
			g.draw(dragline.getLine());
			System.out.println("\t FDFD: " + dragline.getColor().toString());
		}
		g.setColor(new Color(0xff, 0, 0x80));
		g.fill(player.getHitbox());
		
		
	}
	
	/**
	 * Serializes players to JsonArray.
	 * @return JsonArray containing all Players in Set players as JsonValues.
	 */
	public JsonArray playersAsJson() {
		JsonArray playersAsJson = new JsonArray();
		
		for (Player player : players) {
			playersAsJson.add(Player.toJSON(player));
		}
		
		return playersAsJson;
	}
	
	/**
	 * @param playersAsJson JsonArray containing Players in Json-format.
	 * @return all parsed Players in playersAsJson as a Set.
	 */
	public Set<Player> playersToSet(JsonArray playersAsJson) {
		Set<Player> parsedPlayers = 
				Collections.synchronizedSet(new HashSet<Player>());
		
		for (JsonValue val : playersAsJson) {
			parsedPlayers.add(Player.fromJSON(val.asObject()));
		}
		
		return parsedPlayers;
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
