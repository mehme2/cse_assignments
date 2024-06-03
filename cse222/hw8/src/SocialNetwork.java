import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.util.ArrayList;
import java.sql.Timestamp;
import java.util.Queue;
import java.util.LinkedList;
import java.util.TreeSet;
import java.util.Set;
import java.util.HashSet;

/**
 * class to manage a social network graph
 */
public class SocialNetwork {
    private Map<String, Person> people;
    private Map<Person, List<Person>> graph;

    /**
     * initalizes friendship graph
     */
    public SocialNetwork() {
        people = new HashMap<String, Person>();
        graph = new HashMap<Person, List<Person>>();
    }

    /**
     * adds person to graph
     * @throws Exception if person with the name already exists
     */
    public void addPerson(String name, int age, List<String> hobbies) throws Exception {
        if(people.get(name) != null)
            throw new Exception("Person named " + name + " already exists.");
        Person p = new Person(name, age, hobbies);
        people.put(p.getName(), p);
        graph.put(p, new ArrayList<Person>());
        System.out.println("Person added: " + p.getName() + " (Timestamp: " + p.getJoinDate() + ")");
    }

    /**
     * removes person from graph
     * @throws Exception if cant find a person with matching parameters
     */
    public void removePerson(String name, Timestamp date) throws Exception {
        getPerson(name, date);
        Person p = people.remove(name);
        if(p == null)
            throw new Exception(String.format("No person found with the name \"%s\".", name));
        List<Person> fs = graph.remove(p);
        if(fs == null)
            throw new Exception(String.format("%s is not properly added to the network.", p.getName()));
        for(Person f : fs) {
            try {
                removeFriendshipOneWay(f, p);
            }
            catch(Exception e) {}
        }
        System.out.println("Person removed: " + p.getName());
    }

    /**
     * links two given people as friends
     * @throws Exception if cant find people with matching parameters or if they are already friends
     */
    public void addFriendship(String name1, Timestamp date1, String name2, Timestamp date2) throws Exception {
        Person p1 = getPerson(name1, date1);
        Person p2 = getPerson(name2, date2);
        List<Person> fs1, fs2;
        fs1 = graph.get(p1);
        fs2 = graph.get(p2);
        if(fs1 == fs2)
            throw new Exception(String.format("%s and %s are the same person.", p1.getName(), p2.getName()));
        if(fs1.indexOf(p2) != -1)
            throw new Exception(String.format("%s and %s are already friends.", p1.getName(), p2.getName()));
        fs1.add(p2);
        fs2.add(p1);
        System.out.println("Friendship added between " + p1.getName() + " and " + p2.getName() + ".");
    }

    private void removeFriendshipOneWay(Person p1, Person p2) throws Exception {
        if(!graph.get(p1).remove(p2))
            throw new Exception(String.format("%s and %s are not friends.", p1.getName(), p2.getName()));
    }

    /**
     * removes friendship link of two people
     * @throws Exception if cant find people with matching parameters or if they are already not friends
     */
    public void removeFriendship(String name1, Timestamp date1, String name2, Timestamp date2) throws Exception {
        Person p1 = getPerson(name1, date1);
        Person p2 = getPerson(name2, date2);
        removeFriendshipOneWay(p1, p2);
        removeFriendshipOneWay(p2, p1);
        System.out.println("Friendship removed between " + p1.getName() + " and " + p2.getName() + ".");
    }

    /**
     * finds and prints shortest friend path between two people
     * @throws Exception if cant find people with matching parameters or there is no path between them
     */
    public void findShortestPath(String name1, Timestamp date1, String name2, Timestamp date2) throws Exception {
        Person start = getPerson(name1, date1);
        Person end = getPerson(name2, date2);
        if(start == end) {
            System.out.println("Shortest path: " + start.getName());
            return;
        }
        // to hold previous names as string in path
        Map<Person, String> visited = new HashMap<Person, String>();
        Queue<Person> queue = new LinkedList<Person>();
        queue.offer(start);
        visited.put(start, start.getName());
        while(queue.peek() != null) {
            Person p = queue.poll();
            for(Person f : graph.get(p)) {
                // if already processed
                if(visited.get(f) != null) continue;
                // concatenate path with own name
                String path = visited.get(p) + " -> " + f.getName();
                if(f == end) {
                    System.out.println("Shortest path: " + path);
                    return;
                }
                visited.put(f, path);
                queue.offer(f);
            }
        }
        throw new Exception(String.format("No path found between %s and %s.", start.getName(), end.getName()));
    }

    /**
     * suggests n friends based on friendship score
     * @throws Exception if cant find a person with matching parameters
     */
    public void suggestFriends(String name, Timestamp date, int n) throws Exception {
        Person p = getPerson(name, date);
        class FriendshipScore implements Comparable<FriendshipScore> {
            Person other;
            double score;
            int commonHobbies;
            int mutualFriends;

            public FriendshipScore(Person o) {
                other = o;
                // find number of mutual friends
                List<Person> fs1, fs2;
                fs1 = graph.get(o);
                fs2 = graph.get(p);
                mutualFriends = 0;
                for(Person f : fs1)
                    if(fs2.indexOf(f) != -1)
                        mutualFriends++;
                // find number of common hobbies
                List<String> hl1, hl2;
                hl1 = o.getHobbies();
                hl2 = p.getHobbies();
                commonHobbies = 0;
                for(String h : hl1)
                    if(hl2.indexOf(h) != -1)
                        commonHobbies++;
                score = 1.0 * mutualFriends + 0.5 * commonHobbies;
            }

            public String toString() {
                return other.getName() + " (Score: " + score + ", " + mutualFriends + " mutual friends, " + commonHobbies + " common hobbies)";
            }

            public int compareTo(FriendshipScore o) {
                return Double.compare(o.score, score);
            }
        }

        // tree set to sort scores
        TreeSet<FriendshipScore> ts = new TreeSet<FriendshipScore>();

        for(Person o : graph.keySet())
            if(p != o)
                ts.add(new FriendshipScore(o));

        // print first n scores
        int i = 0;
        for(FriendshipScore fs : ts) {
            System.out.println(fs);
            if(++i >= n) return;
        }
    }

    /**
     * detects and prints clusters
     */
    public void countClusters() {
        List<List<Person>> clusters = new ArrayList<List<Person>>();
        Set<Person> visited = new HashSet<Person>();
        Queue<Person> queue = new LinkedList<Person>();
        for(Person p : graph.keySet()) {
            // if not visited before
            if(visited.add(p)) {
                // create new cluster
                List<Person> cluster = new ArrayList<Person>();
                clusters.add(cluster);
                cluster.add(p);
                // bsf traversal
                queue.offer(p);
                while(queue.peek() != null) {
                    Person n = queue.poll();
                    for(Person f : graph.get(n)) {
                        // if already visited
                        if(visited.contains(f)) continue;
                        visited.add(f);
                        queue.offer(f);
                        cluster.add(f);
                    }
                }
            }
        }
        // print clusters
        System.out.println("Number of clusters found: " + clusters.size());
        int i = 0;
        for(List<Person> c : clusters) {
            System.out.println("Cluster " + ++i + ":");
            for(Person p : c)
                System.out.println(p.getName());
        }
    }

    private Person getPerson(String name, Timestamp date) throws Exception {
        Person p = people.get(name);
        if(p == null)
            throw new Exception(String.format("No person found with the name \"%s\".", name));
        if(p.getJoinDate().compareTo(date) != 0)
            throw new Exception("Given date does not match.");
        return p;
    }
}
