public class order {
    private String product_name;
    private int count;
    private int total_price;
    private int status;
    private int customer_ID;

    public order(String pn, int c, int tp, int s, int cid) throws Exception {
        if(pn.isEmpty() || c <= 0 || tp <= 0 || s < 0 || s > 3 || cid <= 0) {
            throw new Exception();
        }
        product_name = pn;
        count = c;
        total_price = tp;
        status = s;
        customer_ID = cid;
    }

    public void print_order() {
        System.out.print(String.format(
                    "Product name: %s - Count: %d - Total price: %d - Status: ",
                    product_name,
                    count,
                    total_price
                    ));
        switch(status) {
            case 0:
                System.out.println("Initalized.");
                break;
            case 1:
                System.out.println("Processing.");
                break;
            case 2:
                System.out.println("Completed.");
                break;
            case 3:
                System.out.println("Cancelled.");
                break;
        }
    }

    public int get_customer_ID() {
        return customer_ID;
    }
}
