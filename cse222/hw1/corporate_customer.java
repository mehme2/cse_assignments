public class corporate_customer extends customer {
    private String company_name;

    public corporate_customer(String n, String sn, String a, String p, int i, int oi, String cn) throws Exception {
        super(n, sn, a, p, i, oi);
        if(cn.isEmpty()) {
            throw new Exception();
        }
        company_name = cn;
    }

    public void print_customer() {
        super.print_customer();
        System.out.println(String.format("Company name: %s", company_name));
    }
}
