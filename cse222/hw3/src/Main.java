import java.util.Scanner;

import com.shop.Inventory;
import com.shop.device.*;

public class Main {
    public static void main(String[] args) {
        Inventory inv = new Inventory();
        System.out.println("Welcome to the Electronics Inventory Management System!");
        Scanner s = new Scanner(System.in);
        int option;
        try {
            inv.addDevice("Smart Phone", "Samsung S21", 500.0f, 20);
            inv.addDevice("Smart Phone", "IPhone 15", 1000.0f, 50);
            inv.addDevice("Laptop", "Apple Macbook Air", 1300.0f, 40);
            inv.addDevice("Laptop", "Dell XPS 13", 1200.0f, 20);
            inv.addDevice("Game Console", "Nintendo Switch", 300.0f, 17);
            inv.addDevice("Game Console", "Sony PlayStation 5", 500.0f, 2);
            inv.addDevice("Keyboard", "Logitech", 50.0f, 200);
            inv.addDevice("TV", "LG", 800.0f, 20);
        }
        catch (Exception e) {
        }
        do {
            System.out.println("Please select an option:");
            System.out.println("1. Add a new device");
            System.out.println("2. Remove a device");
            System.out.println("3. Update device details");
            System.out.println("4. List all devices");
            System.out.println("5. Find the cheapest device");
            System.out.println("6. Sort devices by price");
            System.out.println("7. Calculate total inventory value");
            System.out.println("8. Restock a device");
            System.out.println("9. Export inventory report");
            System.out.println("0. Exit");
            System.out.print("> ");
            try {
                option = s.nextInt();
            }
            catch (Exception e) {
                option = -1;
            }
            try {
                switch(option) {
                    case 0:
                        break;
                    case 1: {
                            s.nextLine();
                            System.out.print("Enter category name: ");
                            String c = s.nextLine();
                            System.out.print("Enter device name: ");
                            String n = s.nextLine();
                            try {
                                inv.getDevice(n);
                                System.out.println("Device with name already exists.");
                            }
                            catch (Exception e) {
                                System.out.print("Enter price: $");
                                float p = s.nextFloat();
                                System.out.print("Enter quantity: ");
                                int q = s.nextInt();
                                inv.addDevice(c, n, p, q);
                            }
                        }
                        break;
                    case 2: {
                            s.nextLine();
                            System.out.print("Enter device name: ");
                            String n = s.nextLine();
                            inv.removeDevice(n);
                        }
                        break;
                    case 3: {
                                Device d = null;
                                s.nextLine();
                                System.out.print("Enter device name: ");
                                String n = s.nextLine();
                                d = inv.getDevice(n);
                                System.out.print("Enter price: $");
                                String p = s.nextLine();
                                if(!p.isEmpty()) {
                                    d.setPrice(Float.parseFloat(p));
                                }
                                System.out.print("Enter quantity: ");
                                String q = s.nextLine();
                                if(!q.isEmpty()) {
                                    d.setQuantity(Integer.parseInt(q));
                                }
                    }
                        break;
                    case 4:
                        inv.listDevices();
                        break;
                    case 5:
                        inv.getCheapestDevice().print();
                        break;
                    case 6:
                        inv.sortDevices();
                        break;
                    case 7:
                        System.out.print("Total value: ");
                        System.out.println(inv.totalPrice());
                        break;
                    case 8: {
                                s.nextLine();
                                System.out.print("Enter device name: ");
                                String n = s.nextLine();
                                Device d = inv.getDevice(n);
                                int old = d.getQuantity();
                                System.out.print("Add or Remove?: ");
                                String addRemove = s.nextLine();
                                if(addRemove.equals("Add")) {
                                    System.out.print("Quantity to add: ");
                                    d.setQuantity(old + s.nextInt());
                                }
                                else if(addRemove.equals("Remove")) {
                                    System.out.print("Quantity to remove: ");
                                    d.setQuantity(old - s.nextInt());
                                }
                                else {
                                    System.out.println("Invalid input.");
                                }
                                if(d != null) {
                                    System.out.print("New quantity: ");
                                    System.out.println(d.getQuantity());
                                }
                        }
                        break;
                    case 9:
                        inv.exportReport();
                        break;
                    default:
                        System.out.println("Invalid input.");
                        break;
                }
            }
            catch (ClassNotFoundException e) {
                System.out.println("Invalid category.");
            }
            catch (NumberFormatException e) {
                System.out.println("Invalid input.");
            }
            catch (Exception e) {
                System.out.println(e.getMessage());
            }
        }
        while(option != 0);
    }
}
