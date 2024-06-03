import java.util.LinkedList;
import java.util.Iterator;

/**
 * Class for directories with child files.
 * <p>
 * Children can be iterated over.
 */
public class Directory extends FileSystemElement implements Iterable<FileSystemElement> {
    private LinkedList<FileSystemElement> children = new LinkedList<FileSystemElement>();

    /**
     * Creates a directory with given name string
     * and takes the current date for creation date
     * and parent is initially null
     * and children list is initially empty.
     */
    public Directory(String n) {
        super(n);
    }

    /**
     * Adds the given file object to children.
     * Returns true if successful.
     * Returns false if a file with the same name exists or
     * given object is null.
     */
    public boolean add(FileSystemElement e) {
        if(e == null) {
            return false;
        }
        if(fromPath("/" + getListName() + e.getName()) != null) {
            return false;
        }
        e.setParent(this);
        children.add(e);
        return true;
    }

    /**
     * Searches a direct child with given name.
     * If the child with the name is found removes it from
     * the list and returns it.
     * Returns null if the child with the name is not found.
     */
    public FileSystemElement pop(String n) {
        for(Iterator<FileSystemElement> i = iterator(); i.hasNext();) {
            FileSystemElement c = i.next();
            if(c.getName().equals(n)) {
                i.remove();
                c.setParent(null);
                return c;
            }
        }
        return null;
    }

    /**
     * Returns a file iterator that iterates over every
     * single direct child.
     */
    public Iterator<FileSystemElement> iterator() {
        return children.iterator();
    }

    /**
     * Returns a more complete file name.
     * Returned string is the file name
     * string with '/' appended to the end.
     */
    public String getListName() {
        return getName() + "/";
    }

    /**
     * Tries to find a file from given path string.
     * Returns null if file is not found.
     * Searches the children recursively.
     */
    public FileSystemElement fromPath(String path) {
        String prefix = "/" + getName();
        if(!path.startsWith(prefix)) {
            return null;
        }
        String trimmed = path.substring(prefix.length());
        if(trimmed.isEmpty()) {
            return this;
        }
        FileSystemElement ret = null;
        for(FileSystemElement c : children) {
            ret = c.fromPath(trimmed);
            if(ret != null) {
                break;
            }
        }
        return ret;
    }

    /**
     * Searches a file with given name.
     * Returns null if file with name is not found.
     * Checks itself and its children recursively.
     */
    public FileSystemElement search(String n) {
        if(n.equals(getName())) {
            return this;
        }
        FileSystemElement ret = null;
        for(Iterator<FileSystemElement> i = iterator(); i.hasNext() && ret == null; ) {
            ret = i.next().search(n);
        }
        return ret;
    }

    /**
     * Sorts the children list by date.
     * Earlier date comes first.
     */
    public void sortByDate() {
        children.sort((a, b) -> a.getDate().compareTo(b.getDate()));
    }
}
