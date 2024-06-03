package com.shop;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Iterator;
import java.io.FileWriter;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import com.shop.device.*;

public class Inventory {
    private LinkedList<ArrayList<Device>> list;

    /**
     * Constructs an empty device list.
     * <p>
     * Has O(1) complexity.
     */
    public Inventory() {
        list = new LinkedList<ArrayList<Device>>();
    }

    /**
     * Adds the given device to the list.
     * Creates a new {@link java.util.ArrayList} for the category if not created yet.
     * <p>
     * Searching for the category ArrayList is O(n) complexity
     * (n being the number of categories), adding the device to
     * ArrayList is O(1) complexity and creating and adding a new
     * category ArrayList is O(1) complexity. The overall complexity
     * is O(n).
     */
    public void addDevice(Device d) {
        Iterator<ArrayList<Device>> i = list.iterator();
        ArrayList<Device> cl = null;
        String category = d.getCategory();
        while(i.hasNext()) {
            ArrayList<Device> clt = i.next();
            if(clt.get(0).getCategory().equals(category)) {
                cl = clt;
            }
        }
        if(cl == null) {
            cl = new ArrayList<Device>();
            list.add(cl);
        }
        cl.add(d);
    }

    /**
     * Constructs a new device object from given parameters.
     * <p>
     * Device constructions complexity is O(1), and used method
     * {@link #addDevice(Device)}'s complexity is O(n) (n = number of categories), so the overall complexity is
     * O(n).
     * @param category The derived device class is determined from this parameter.
     * {@link java.lang.Class#forName(String)} is used.
     * @throws Exception if name, price, or quantity are invalid
     * @throws ClassNotFoundException if the class com.shop.device.category
     * does not exist
     */
    public void addDevice(String category, String name, float price, int quantity) throws Exception, ClassNotFoundException {
        Object ins = Class.forName("com.shop.device." + category.replace(' ', '_')).getConstructor().newInstance();
        if(ins instanceof DeviceAbs) {
            Device d = (Device)ins;
            d.setName(name);
            d.setPrice(price);
            d.setQuantity(quantity);
            addDevice(d);
        }
    }

    /**
     * Removes the device with the given name.
     * Removes the categories {@link java.util.ArrayList} if it is empty
     * after removal.
     * <p>
     * The device lookup and removal is O(n) complexity (n = total number of devices),
     * the category list removal is O(1). Overall complexity is O(n).
     * @throws Exception if device with the given name is not found
     */
    public void removeDevice(String name) throws Exception {
        Iterator<ArrayList<Device>> i = list.iterator();
        while(i.hasNext()) {
            ArrayList<Device> cl = i.next();
            Iterator<Device> cli = cl.iterator();
            while(cli.hasNext()) {
                Device d = cli.next();
                if(d.getName().equals(name)) {
                    cli.remove();
                    if(cl.isEmpty()) {
                        i.remove();
                    }
                    return;
                }
            }
        }
        throw new Exception("Device not found.");
    }

    /**
     * Searches and returns the device with the given name.
     * <p>
     * The device lookup is O(n) complexity (n = total number of devices).
     * @throws Exception if no device with the given name is found
     */
    public Device getDevice(String name) throws Exception {
        Iterator<ArrayList<Device>> i = list.iterator();
        while(i.hasNext()) {
            ArrayList<Device> cl = i.next();
            Iterator<Device> cli = cl.iterator();
            while(cli.hasNext()) {
                Device d = cli.next();
                if(d.getName().equals(name)) {
                    return d;
                }
            }
        }
        throw new Exception("Device not found.");
    }

    /**
     * Prints every device in the list to stdout.
     * <p>
     * Since the method iterates over and prints every device
     * the complexity is O(n) (n = total number of devices).
     */
    public void listDevices() {
        System.out.println("Device List:");
        int index = 1;
        Iterator<ArrayList<Device>> i = list.iterator();
        while(i.hasNext()) {
            ArrayList<Device> cl = i.next();
            Iterator<Device> cli = cl.iterator();
            while(cli.hasNext()) {
                System.out.print(String.format("%d. ", index));
                cli.next().print();
                ++index;
            }
        }
    }

    /**
     * Returns the device with the lowest price.
     * <p>
     * Since the method iterates over every device to find the cheapest
     * price the complexity is O(n) (n = total number of devices).
     *
     * @throws Exception if there are no devices
     */
    public Device getCheapestDevice() throws Exception {
        if(list.isEmpty()) {
            throw new Exception("There are no devices in inventory.");
        }
        Device cheapest = list.get(0).get(0);
        Iterator<ArrayList<Device>> i = list.iterator();
        while(i.hasNext()) {
            ArrayList<Device> cl = i.next();
            Iterator<Device> cli = cl.iterator();
            while(cli.hasNext()) {
                Device d = cli.next();
                if(d.getPrice() < cheapest.getPrice()) {
                    cheapest = d;
                }
            }
        }
        return cheapest;
    }

    /**
     * Sorts every device by price in ascending order and prints the list.
     * <p>
     * The complexity of used method {@link java.util.List#sort(Java.util.Comparator)}
     * is O(nlogn) (n = total number of devices) and the complexity of printing
     * every device is O(n), so the overall complexity is O(nlogn).
     */
    public void sortDevices() {
        ArrayList<Device> all = new ArrayList<Device>();
        Iterator<ArrayList<Device>> i = list.iterator();
        while(i.hasNext()) {
            ArrayList<Device> cl = i.next();
            all.addAll(cl);
        }
        all.sort(null);
        int index = 1;
        Iterator<Device> si = all.iterator();
        while(si.hasNext()) {
            System.out.print(String.format("%d. ", index));
            si.next().print();
            ++index;
        }
    }

    /**
     * Returns the total of quantity * price of every device.
     * <p>
     * Iterates over every device to calculate the value so the
     * complexity is O(n) (n = total number of devices).
     */
    public float totalPrice() {
        float total = 0.0f;
        Iterator<ArrayList<Device>> i = list.iterator();
        while(i.hasNext()) {
            ArrayList<Device> cl = i.next();
            Iterator<Device> cli = cl.iterator();
            while(cli.hasNext()) {
                Device d = cli.next();
                total += d.getPrice() * d.getQuantity();
            }
        }
        return total;
    }

    /**
     * Exports the date, information about every device, total number of devices,
     * and output of {@link #totalPrice()} to "report.txt".
     * <p>
     * The complexity of writing the date is O(1), information of every device
     * and total device number and total price are all O(n) complexity
     * (n = total number of devices). The overall complexity is O(n).
     */
    public void exportReport() {
        try {
            FileWriter fw = new FileWriter("report.txt");
            fw.write("Electronics Shop Inventory Report\n");
            fw.write("Generated on: ");
            fw.write(LocalDate.now().format(DateTimeFormatter.ofPattern("dd MMMM yyyy")));
            fw.write("\n\n");
            fw.write("-----------------------------------------------------------\n");
            fw.write("| No. | Category    | Name            | Price  | Quantity |\n");
            fw.write("-----------------------------------------------------------\n");
            int index = 1;
            Iterator<ArrayList<Device>> i = list.iterator();
            while(i.hasNext()) {
                ArrayList<Device> cl = i.next();
                Iterator<Device> cli = cl.iterator();
                while(cli.hasNext()) {
                    Device d = cli.next();
                    fw.write(String.format("| %3d | %12s | %15s | $%4.2f | %5d |\n",
                            index, d.getCategory(), d.getName(), d.getPrice(), d.getQuantity()));
                    ++index;
                }
            }
            fw.write("-----------------------------------------------------------\n");
            fw.write("\nSummary:\n");
            fw.write(String.format("- Total Number of Devices: %d\n", index - 1));
            fw.write(String.format("- Total Inventory Value: $%.2f\n", totalPrice()));
            fw.write("\nEnd od Report\n");
            fw.close();
        }
        catch (Exception e) {
            System.out.println(e.getClass());
            System.out.println(e.getMessage());
        }
    }
}
