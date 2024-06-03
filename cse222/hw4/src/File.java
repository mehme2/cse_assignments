/**
 * Class for generic files.
 */
public class File extends FileSystemElement {
    /**
     * Creates a file with given name string
     * and takes the current date for creation date
     * and parent is initially null.
     */
    public File(String n) {
        super(n);
    }

    /**
     * Returns a more complete file name.
     * Returned string is same as {@link getName()}.
     */
    public String getListName() {
        return getName();
    }

    /**
     * Tries to find a file from given path string.
     * Returns null if file is not found.
     * Only works for paths with one depth.
     */
    public FileSystemElement fromPath(String path) {
        if(path.equals("/" + getName())) {
            return this;
        }
        return null;
    }

    /**
     * Searches a file with given name.
     * Returns null if file with name is not found.
     * Only checks itself.
     */
    public FileSystemElement search(String n) {
        if(n.equals(getName())) {
            return this;
        }
        return null;
    }
}
