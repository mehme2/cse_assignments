import java.io.FileWriter;

/**
 * Generates random input command files for testing the stock data management system.
 */
public class RandomInputGenerator {
    /**
     * Main method to generate random input commands and write them to a file.
     * @param args The command-line arguments: [output file] [add size] [remove size] [search size] [update size].
     */
    public static void main(String[] args) {
        int nAdd;
        int nRemove;
        int nSearch;
        int nUpdate;
        FileWriter fw;
        // try to read given arguments
        try {
            if(args.length != 5) {
                throw new Exception();
            }
            fw = new FileWriter(args[0]);
            nAdd = Integer.parseInt(args[1]);
            nRemove = Integer.parseInt(args[2]);
            nSearch = Integer.parseInt(args[3]);
            nUpdate = Integer.parseInt(args[4]);
        }
        catch(Exception e) {
            System.out.println("Arguments should be: [output file] [add size] [remove size] [search size] [update size]");
            return;
        }

        try {
            while(nAdd > 0 || nRemove > 0 || nSearch > 0 || nUpdate > 0) {
                switch((int) (Math.random() * 4)) {
                    case 0:
                        if(nAdd > 0) {
                            fw.write("ADD "
                                    + "SYM" + (int) (Math.random() * 100) + " "
                                    + (Math.random() * 100) + " "
                                    + (long) (Math.random() * 1000000) + " "
                                    + (long) (Math.random() * 1000000000)
                                    + "\n");
                            nAdd--;
                        }
                        break;
                    case 1:
                        if(nRemove > 0) {
                            fw.write("REMOVE "
                                    + "SYM" + (int) (Math.random() * 100)
                                    + "\n");
                            nRemove--;
                        }
                        break;
                    case 2:
                        if(nSearch > 0) {
                            fw.write("SEARCH "
                                    + "SYM" + (int) (Math.random() * 100)
                                    + "\n");
                            nSearch--;
                        }
                        break;
                    case 3:
                        if(nUpdate > 0) {
                            fw.write("UPDATE "
                                    + "SYM" + (int) (Math.random() * 100) + " "
                                    + "SYM" + (int) (Math.random() * 100) + " "
                                    + (Math.random() * 100) + " "
                                    + (long) (Math.random() * 1000000) + " "
                                    + (long) (Math.random() * 1000000000)
                                    + "\n");
                            nUpdate--;
                        }
                        break;
                }
            }
            fw.close();
        }
        catch(Exception e) {
            System.out.println("An error occurred.");
        }
    }
}
