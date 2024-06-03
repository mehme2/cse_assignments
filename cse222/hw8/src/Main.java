import java.util.Scanner;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.sql.Timestamp;

public class Main {
    public static void main(String[] args) {
        try {
            Scanner s = new Scanner(System.in);
            SocialNetwork sn = new SocialNetwork();
            System.out.println("Welcome to the Social Network Management System.");
            System.out.println("Options:");
            System.out.println("1 - Add Person");
            System.out.println("2 - Remove Person");
            System.out.println("3 - Add Friendship");
            System.out.println("4 - Remove Friendship");
            System.out.println("5 - Find Shortest Path");
            System.out.println("6 - Suggest Friends");
            System.out.println("7 - Counting Clusters");
            System.out.println("Other - Exit");
            boolean run = true;
            while(run) {
                try {
                    System.out.print("\n> ");
                    int selection = s.nextInt();
                    String name;
                    Timestamp date;
                    String name1;
                    Timestamp date1;
                    String name2;
                    Timestamp date2;
                    int age;
                    String hobbies;
                    int n;
                    switch(selection) {
                        case 1:
                            s.nextLine();
                            System.out.print("Enter name: ");
                            name = s.nextLine();
                            System.out.print("Enter age: ");
                            age = s.nextInt();
                            System.out.print("Enter hobbies (hobby1, hobby2,..., hobbyn): ");
                            s.nextLine();
                            hobbies = s.nextLine();
                            sn.addPerson(name, age, new ArrayList<String>(Arrays.asList(hobbies.replaceAll(", ", ",").split(","))));
                            break;
                        case 2:
                            s.nextLine();
                            System.out.print("Enter name: ");
                            name = s.nextLine();
                            System.out.print("Enter date: ");
                            date = Timestamp.valueOf(s.nextLine());
                            sn.removePerson(name, date);
                            break;
                        case 3:
                            s.nextLine();
                            System.out.print("Enter name of first person: ");
                            name1 = s.nextLine();
                            System.out.print("Enter date of first person: ");
                            date1 = Timestamp.valueOf(s.nextLine());
                            System.out.print("Enter name of second person: ");
                            name2 = s.nextLine();
                            System.out.print("Enter date of second person: ");
                            date2 = Timestamp.valueOf(s.nextLine());
                            sn.addFriendship(name1, date1, name2, date2);
                            break;
                        case 4:
                            s.nextLine();
                            System.out.print("Enter name of first person: ");
                            name1 = s.nextLine();
                            System.out.print("Enter date of first person: ");
                            date1 = Timestamp.valueOf(s.nextLine());
                            System.out.print("Enter name of second person: ");
                            name2 = s.nextLine();
                            System.out.print("Enter date of second person: ");
                            date2 = Timestamp.valueOf(s.nextLine());
                            sn.removeFriendship(name1, date1, name2, date2);
                            break;
                        case 5:
                            s.nextLine();
                            System.out.print("Enter name of first person: ");
                            name1 = s.nextLine();
                            System.out.print("Enter date of first person: ");
                            date1 = Timestamp.valueOf(s.nextLine());
                            System.out.print("Enter name of second person: ");
                            name2 = s.nextLine();
                            System.out.print("Enter date of second person: ");
                            date2 = Timestamp.valueOf(s.nextLine());
                            sn.findShortestPath(name1, date1, name2, date2);
                            break;
                        case 6:
                            s.nextLine();
                            System.out.print("Enter name: ");
                            name = s.nextLine();
                            System.out.print("Enter date: ");
                            date = Timestamp.valueOf(s.nextLine());
                            System.out.print("Enter maximum number of friends to suggest: ");
                            n = s.nextInt();
                            sn.suggestFriends(name, date, n);
                            break;
                        case 7:
                            sn.countClusters();
                            break;
                        default:
                            run = false;
                            break;
                    }
                }
                catch(InputMismatchException e) {
                    System.out.println("Invalid input.");
                }
                catch(Exception e) {
                    System.out.println(e.getMessage());
                }
            }
        }
        catch(Exception e) {
            System.out.println("Could not initalize input.");
        }
        System.out.println("Exiting...");
    }
}
