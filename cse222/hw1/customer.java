public class customer extends person {
    private order[] orders;
    private int operator_ID;

    public customer(String n, String sn, String a, String p, int i, int oi) throws Exception {
        super(n, sn, a, p, i);
        operator_ID = oi;
    }

    public void print_customer() {
        print_person();
        System.out.println(String.format("Operator ID: %d", operator_ID));
    }

    public void print_orders() {
        if(orders == null) {
            System.out.println("This customers orders are not defined.");
            return;
        }
        if(orders.length == 0) {
            System.out.println("This customer doesn't have any orders.");
            return;
        }
        for(int i = 0; orders != null && i < orders.length; ++i) {
            System.out.print(String.format("Order #%d => ", i + 1));
            if(orders[i] != null) {
                orders[i].print_order();
            }
            else {
                System.out.println("null");
            }
        }
    }

    public void define_orders(order[] o) {
        orders = o;
    }

    public int get_operator_ID() {
        return operator_ID;
    }
}
