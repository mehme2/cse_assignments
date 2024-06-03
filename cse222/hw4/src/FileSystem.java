import java.util.Scanner;
import java.time.format.DateTimeFormatter;

/**
 * Basic file system class for user interface.
 */
public class FileSystem {
    private Directory root;
    private Directory current;

    /**
     * Creates a file system object with
     * root directory name "root" and
     * current directory pointing to root.
     */
    public FileSystem() {
        root = new Directory("root");
        current = root;
    }

    /**
     * Returns current directory.
     */
    public Directory currentDirectory() {
        return current;
    }

    /**
     * Returns root directory.
     */
    public Directory rootDirectory() {
        return root;
    }

    /**
     * Changes the current directory to the directory
     * with the given path. The path is relative to the
     * root. Returns true if successful. Returns false
     * if the path does not point to a valid directory.
     */
    public boolean changeDirectory(String path) {
        FileSystemElement d = root.fromPath(path);
        if(d instanceof Directory) {
            current = (Directory)d;
            return true;
        }
        return false;
    }

    /**
     * Prints the ancestors of the given file to
     * standard output in a tree form recursively.
     * Given file should be a directory.
     * Returns the depth of the given file relative to
     * the root. The depth of the root is 0.
     */
    public static int printAncestors(FileSystemElement d) {
        if(!(d instanceof Directory)) return 0;
        int depth = printAncestors(d.getParent());
        System.out.println("");
        for(int i = 0; i < depth; i++) {
            System.out.print("  ");
        }
        System.out.print("* ");
        System.out.print(d.getListName());
        return depth + 1;
    }

    /**
     * Prints the children of the given directory with given settings to the standard output.
     * @param indent how much spaces should be left before each line
     * @param date prints the file creation date if true
     */
    public static void printDirectoryContents(Directory d, int indent, boolean date) {
        for(FileSystemElement c : d) {
            for(int i = 0; i < indent; i++) {
                System.out.print(" ");
            }
            if(c instanceof Directory) {
                System.out.print("* ");
            }
            System.out.print(c.getListName());
            if(date) {
                System.out.print(c.getDate().toLocalDateTime().format(DateTimeFormatter.ofPattern(" (yyyy-MM-dd kk:mm:ss)")));
            }
            System.out.println("");
        }
    }

    /**
     * The main function for the file system CLI.
     */
    public static void main(String[] args) {
        FileSystem fs = new FileSystem();
        System.out.println("===== File System Management Menu =====");
        System.out.println("1. Change directory");
        System.out.println("2. List directory contents");
        System.out.println("3. Create file/directory");
        System.out.println("4. Delete file/directory");
        System.out.println("5. Move file/directory");
        System.out.println("6. Search file/directory");
        System.out.println("7. Print directory tree");
        System.out.println("8. Sort contents by date created");
        System.out.println("9. Exit");
        Scanner s = new Scanner(System.in);
        int opt = -1;
        while(opt != 9) {
            System.out.print("\nPlease select an option: ");
            try {
                opt = s.nextInt();
            }
            catch(Exception e) {
                System.out.println("Invalid input.");
                opt = -1;
                try {
                    s.nextLine();
                }
                catch(Exception e2) {
                }
            }
            switch(opt) {
                case 1:
                    System.out.print("Current directory: ");
                    System.out.println(fs.currentDirectory().getPath());
                    System.out.print("Enter new directory path: ");
                    try {
                        if(fs.changeDirectory(s.next())) {
                            System.out.println("Directory changed to: " + fs.currentDirectory().getPath());
                        }
                        else {
                            System.out.println("Couldn't change directory.");
                        }
                    }
                    catch(Exception e) {
                        System.out.println("Invalid input.");
                    }
                    break;
                case 2:
                    System.out.print("Listing contents of ");
                    System.out.println(fs.currentDirectory().getPath() + ":");
                    printDirectoryContents(fs.currentDirectory(), 0, false);
                    break;
                case 3:
                    System.out.print("Current directory: ");
                    System.out.println(fs.currentDirectory().getPath());
                    System.out.print("Create file or directory (f/d): ");
                    try {
                        String ts = s.next();
                        // the new file
                        FileSystemElement nf = null;
                        if(ts.equals("d")) {
                            System.out.print("Enter name for new directory: ");
                            nf = new Directory(s.next());
                        }
                        else if(ts.equals("f")) {
                            System.out.print("Enter name for new file: ");
                            nf = new File(s.next());
                        }
                        else {
                            System.out.println("Invalid input.");
                        }
                        if(nf.getName().indexOf('/') != -1) {
                            System.out.println("File name cannot include \'/\'.");
                            break;
                        }
                        if(fs.currentDirectory().add(nf)) {
                            System.out.println(nf.getClass().getName() + " created: " + nf.getListName());
                        }
                        else {
                            System.out.println("Couldn't create new file.");
                        }
                    }
                    catch(Exception e) {
                        System.out.println("Invalid input.");
                    }
                    break;
                case 4:
                    System.out.print("Current directory: ");
                    System.out.println(fs.currentDirectory().getPath());
                    System.out.print("Enter name of file/directory to delete: ");
                    try {
                        // file to be removed
                        // java garbage collector removes directory children recursively
                        FileSystemElement f = fs.currentDirectory().pop(s.next());
                        if(f != null) {
                            System.out.println(f.getClass().getName() + " deleted: " + f.getListName());
                        }
                        else {
                            System.out.println("File not found.");
                        }
                    }
                    catch(Exception e) {
                        System.out.println("Invalid input.");
                    }
                    break;
                case 5:
                    System.out.print("Current directory: ");
                    System.out.println(fs.currentDirectory().getPath());
                    try {
                        System.out.print("Enter name of file/directory to move: ");
                        String n = s.next();
                        System.out.print("Enter new directory path: ");
                        String p = s.next();
                        // if user tries to move a directory inside itself
                        // checks it by checking if given path starts with
                        // path of the file to be moved
                        if(p.indexOf(fs.currentDirectory().getPath() + "/" + n) == 0) {
                            System.out.println("Please do not try to move a directory inside itself.");
                            break;
                        }
                        FileSystemElement d = fs.rootDirectory().fromPath(p);
                        if(d instanceof Directory) {
                            FileSystemElement f = fs.currentDirectory().pop(n);
                            if(f != null) {
                                ((Directory)d).add(f);
                                System.out.println("File moved: " + f.getListName() + " to " + p);
                            }
                            else {
                                System.out.println("File not found.");
                            }
                        }
                    }
                    catch(Exception e) {
                        System.out.println("Invalid input.");
                    }
                    break;
                case 6:
                    System.out.print("Search query: ");
                    try {
                        String name = s.next();
                        System.out.println("Searching from " + fs.rootDirectory().getName() + "...");
                        FileSystemElement f = fs.rootDirectory().search(name);
                        if(f != null) {
                            System.out.println("Found: " + f.getPath());
                        }
                        else {
                            System.out.println("File with the name \'" + name + "\' not found.");
                        }
                    }
                    catch(Exception e) {
                        System.out.println("Invalid input.");
                    }
                    break;
                case 7: {
                        System.out.print("Path to current directory from root:");
                        int depth = printAncestors(fs.currentDirectory());
                        System.out.println(" (Current Directory)");
                        printDirectoryContents(fs.currentDirectory(), depth * 2, false);
                    }
                    break;
                case 8:
                    fs.currentDirectory().sortByDate();
                    System.out.println("Sorted contents of " + fs.currentDirectory().getPath() + " by date created:");
                    printDirectoryContents(fs.currentDirectory(), 0, true);
                    break;
                case 9:
                    break;
            }
        }
    }
}
