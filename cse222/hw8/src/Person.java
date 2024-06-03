import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;

public final class Person {
    private String name;
    private int age;
    private List<String> hobbies;
    private Timestamp joinDate;

    /**
     * @throws Exception if name is empty or age is negative
     */
    public Person(String n, int a, List<String> h) throws Exception {
        setName(n);
        setAge(a);
        setHobbies(h);
        joinDate = Timestamp.from(Instant.now());
        // set nanoseconds to zero to make comparing easier
        joinDate.setNanos(0);
    }

    public String getName() { return name; }
    public int getAge() { return age; }
    public List<String> getHobbies() { return hobbies; }
    public Timestamp getJoinDate() { return joinDate; }

    /**
     * @throws Exception if name is empty
     */
    public void setName(String n) throws Exception {
        if(n.isEmpty())
            throw new Exception("Name cannot be empty.");
        name = n;
    }

    /**
     * @throws Exception if age is negative
     */
    public void setAge(int a) throws Exception {
        if(a < 0)
            throw new Exception("Age cannot be negative.");
        age = a;
    }
    public void setHobbies(List<String> h) { hobbies = h; }
}
