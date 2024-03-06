import java.io.File;
import java.util.Scanner;

public class cts {
    private customer[] customers;
    private int nCustomers;
    private operator[] operators;
    private int nOperators;
    private order[] orders;
    private int nOrders;

    static final int ARRAY_SIZE = 100;

    public cts(String filename) {
        Scanner s;
        try {
            s = new Scanner(new File(filename));
        }
        catch(Exception e) {
            s = new Scanner("");
        }
        customers = new customer[ARRAY_SIZE];
        operators = new operator[ARRAY_SIZE];
        orders = new order[ARRAY_SIZE];
        nCustomers = 0;
        nOperators = 0;
        nOrders = 0;
        while(s.hasNextLine()) {
            try {
                String line = s.nextLine();
                String[] params = line.split(";", -1);
                if(params[0].equals("order") && params.length == 6) {
                    orders[nOrders] = new order(
                            params[1],
                            Integer.parseInt(params[2]),
                            Integer.parseInt(params[3]),
                            Integer.parseInt(params[4]),
                            Integer.parseInt(params[5])
                            );
                    ++nOrders;
                }
                else if((params[0].equals("retail_customer") && params.length == 7) || 
                        (params[0].equals("corporate_customer") && params.length == 8)) {
                    if(params.length == 7) {
                        customers[nCustomers] = new retail_customer(
                                params[1],
                                params[2],
                                params[3],
                                params[4],
                                Integer.parseInt(params[5]),
                                Integer.parseInt(params[6])
                                );
                    }
                    else {
                        customers[nCustomers] = new corporate_customer(
                                params[1],
                                params[2],
                                params[3],
                                params[4],
                                Integer.parseInt(params[5]),
                                Integer.parseInt(params[6]),
                                params[7]
                                );
                    }
                    int id = customers[nCustomers].get_ID();
                    // search for duplicate id
                    for(int i = 0; i < nCustomers; ++i) {
                        if(customers[i].get_ID() == id) {
                            customers[nCustomers] = null;
                            throw new Exception();
                        }
                    }
                    for(int i = 0; i < nOperators; ++i) {
                        if(operators[i].get_ID() == id) {
                            customers[nCustomers] = null;
                            throw new Exception();
                        }
                    }
                    ++nCustomers;
                }
                else if(params[0].equals("operator") && params.length == 7) {
                    operators[nOperators] = new operator(
                            params[1],
                            params[2],
                            params[3],
                            params[4],
                            Integer.parseInt(params[5]),
                            Integer.parseInt(params[6])
                            );
                    int id = operators[nOperators].get_ID();
                    // search for duplicate id
                    for(int i = 0; i < nCustomers; ++i) {
                        if(customers[i].get_ID() == id) {
                            operators[nOperators] = null;
                            throw new Exception();
                        }
                    }
                    for(int i = 0; i < nOperators; ++i) {
                        if(operators[i].get_ID() == id) {
                            operators[nOperators] = null;
                            throw new Exception();
                        }
                    }
                    ++nOperators;
                }
            }
            catch(Exception e) {
            }
        }
        customer[][] ocTable = new customer[nOperators][nCustomers]; // operator-customer table
        int[] cpo = new int[nOperators]; // customers per operator
        for(int i = 0; i < nCustomers; ++i) {
            int id = customers[i].get_operator_ID();
            int j = 0;
            for(; j < nOperators && operators[j].get_ID() != id; ++j); //search operator by id
            if(j < nOperators) { // operator found
                ocTable[j][cpo[j]] = customers[i];
                ++cpo[j];
            }
            else { // operator not found (delete customer)
                --nCustomers;
                for(int k = i; k < nCustomers; ++k) {
                    customers[k] = customers[k + 1];
                }
                customers[nCustomers] = null;
            }
        }
        for(int i = 0; i < nOperators; ++i) {
            customer[] trimmed = new customer[cpo[i]];
            System.arraycopy(ocTable[i], 0, trimmed, 0, cpo[i]);
            operators[i].define_customers(trimmed);
        }
        order[][] coTable = new order[nCustomers][nOrders]; // customer-order table
        int[] opc = new int[nCustomers]; // orders per customer
        for(int i = 0; i < nOrders; ++i) {
            int id = orders[i].get_customer_ID();
            int j = 0;
            for(; j < nCustomers && customers[j].get_ID() != id; ++j); //search customer by id
            if(j < nCustomers) { // customer found
                coTable[j][opc[j]] = orders[i];
                ++opc[j];
            }
            else { // customer not found (delete order)
                --nOrders;
                for(int k = i; k < nOrders; ++k) {
                    orders[k] = orders[k + 1];
                }
                orders[nOrders] = null;
            }
        }
        for(int i = 0; i < nCustomers; ++i) {
            order[] trimmed = new order[opc[i]];
            System.arraycopy(coTable[i], 0, trimmed, 0, opc[i]);
            customers[i].define_orders(trimmed);
        }
    }

    public void print_by_ID(int id) throws Exception {
        int found = 0;
        for(int i = 0; i < nCustomers; ++i) {
            if(customers[i].get_ID() == id) {
                System.out.println("*** Customer Screen ***");
                customers[i].print_customer();
                customers[i].print_orders();
                found = 1;
            }
        }
        for(int i = 0; i < nOperators; ++i) {
            if(operators[i].get_ID() == id) {
                System.out.println("*** Operator Screen ***");
                System.out.println("------------------------------");
                operators[i].print_operator();
                operators[i].print_customers();
                found = 1;
            }
        }
        if(found == 0) {
            throw new Exception();
        }
    }

    public static void main(String[] args) {
        cts c = new cts("content.txt");
        Scanner s = new Scanner(System.in);
        int id;
        while(true) {
            System.out.println("Please enter your ID...");
            try {
                id = s.nextInt();
            }
            catch(Exception e) {
                System.out.println("Invalid input. Terminating...");
                return;
            }
            try {
                c.print_by_ID(id);
            }
            catch(Exception e) {
                System.out.println(String.format("No operator/customer was found with ID %d. Please try again.", id));
            }
        }
    }
}
