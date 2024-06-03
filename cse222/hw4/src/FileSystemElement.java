import java.sql.Timestamp;
import java.time.Instant;

/**
 * Abstract class that can be extended for different file types.
 */
public abstract class FileSystemElement {
    private String name;
    private Timestamp dateCreated;
    private FileSystemElement parent = null;

    /**
     * Creates a file with given name string
     * and takes the current date for creation date
     * and parent is initially null.
     */
    public FileSystemElement(String n) {
        name = n;
        dateCreated = Timestamp.from(Instant.now());
    }

    /**
     * Returns the file name.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns creation date.
     */
    public Timestamp getDate() {
        return dateCreated;
    }

    /**
     * Returns the parent.
     */
    public FileSystemElement getParent() {
        return parent;
    }

    /**
     * Changes the parent to given object.
     */
    public void setParent(FileSystemElement p) {
        parent = p;
    }

    /**
     * Recursively calculates and returns the path string relative to the root.
     */
    public String getPath() {
        String path = "/" + name;
        if(parent != null) {
            path = parent.getPath() + path;
        }
        return path;
    }

    /**
     * Returns a more complete file name.
     */
    public abstract String getListName();

    /**
     * Tries to find a file from given path string.
     * Returns null if file is not found.
     */
    public abstract FileSystemElement fromPath(String path);

    /**
     * Searches a file with given name.
     * Returns null if file with name is not found.
     */
    public abstract FileSystemElement search(String n);
}
