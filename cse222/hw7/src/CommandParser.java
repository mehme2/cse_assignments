import java.util.LinkedList;
import java.util.List;
import java.util.Iterator;
import java.io.BufferedReader;
import java.io.FileReader;

/**
 * Parses commands from a file and executes them on a StockDataManager.
 */
public class CommandParser {
    private List<String> lines = new LinkedList<String>();
    private Iterator<String> iterator;

    /**
     * Initalizes CommandParser from given file path.
     */
    public CommandParser(String path) {
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
            br.close();
        }
        catch (Exception e) {
        }
        restart();
    }

    /**
     * Checks if there are more commands to parse.
     * @return True if there are more commands, false otherwise.
     */
    public boolean hasNext() {
        return iterator.hasNext();
    }

    /**
     * Restarts the command parsing process from line zero.
     */
    public void restart() {
        iterator = lines.iterator();
    }

    /**
     * Parses the next command and executes it on the provided StockDataManager.
     * @param manager The StockDataManager instance to execute the command on.
     * @return The index of the command executed (0 for ADD, 1 for REMOVE, 2 for SEARCH, 3 for UPDATE),
     *         or -1 if no command was executed.
     */
    public int parseNext(StockDataManager manager) {
        if(!iterator.hasNext() || manager == null) return -1;
        String[] tokens = iterator.next().split(" ");
        String command = tokens[0];

        int commandNo = -1;

        switch (command) {
            case "ADD":
                manager.addOrUpdateStock(tokens[1], Double.parseDouble(tokens[2]), Long.parseLong(tokens[3]), Long.parseLong(tokens[4]));
                commandNo = 0;
                break;
            case "REMOVE":
                manager.removeStock(tokens[1]);
                commandNo = 1;
                break;
            case "SEARCH":
                Stock stock = manager.searchStock(tokens[1]);
                commandNo = 2;
                break;
            case "UPDATE":
                manager.updateStock(tokens[1], tokens[2], Double.parseDouble(tokens[3]), Long.parseLong(tokens[4]), Long.parseLong(tokens[5]));
                commandNo = 3;
                break;
        }
        
        return commandNo;
    }
}
