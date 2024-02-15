import com.gtu.*;

public class Main {
    public static void main(String[] args) {
        JavaSet<Integer> s1 = new JavaSet<Integer>();
        JavaSet<Integer> s2 = new JavaSet<Integer>();
        s1.add(1);
        System.out.format("s1.add(1): %s\n", s1);
        s1.add(1);
        System.out.format("s1.add(1): %s\n", s1);
        s1.add(2);
        System.out.format("s1.add(2): %s\n", s1);
        s1.add(3);
        System.out.format("s1.add(3): %s\n", s1);
        s1.remove(1);
        System.out.format("s1.remove(1): %s\n", s1);
        s2.add(3);
        System.out.format("s2.add(3): %s\n", s2);
        s2.add(2);
        System.out.format("s2.add(2): %s\n", s2);
        System.out.format("s1.equals(s2): %b\n", s1.equals(s2));
    }
}
