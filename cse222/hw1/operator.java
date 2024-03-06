public class operator extends person {
    private int wage;
    private customer[] customers;

    public operator(String n, String sn, String a, String p, int i, int w) throws Exception {
        super(n, sn, a, p, i);
        if(w <= 0) {
            throw new Exception();
        }
        wage = w;
    }
    
    public void print_operator() {
        print_person();
        System.out.println(String.format("Wage: %d", wage));
    }

    public void print_customers() {
        System.out.println("------------------------------");
        if(customers == null) {
            System.out.println("This operators customers are not defined.");
            System.out.println("------------------------------");
            return;
        }
        if(customers.length == 0) {
            System.out.println("This operator doesn't have any customers.");
            System.out.println("------------------------------");
            return;
        }
        for(int i = 0; i < customers.length; ++i) {
            System.out.print(String.format("Customer #%d", i + 1));
            if(customers[i] != null) {
                if(customers[i] instanceof corporate_customer) {
                    System.out.print(" (a coprotate customer)");
                }
                else if(customers[i] instanceof retail_customer) {
                    System.out.print(" (a retail customer)");
                }
                System.out.println(":");
                customers[i].print_customer();
                customers[i].print_orders();
            }
            else {
                System.out.println(":\n null");
            }
            System.out.println("------------------------------");
        }
    }

    public void define_customers(customer[] c) {
        customers = c;
    }
}
