public class person {
    private String name;
    private String surname;
    private String address;
    private String phone;
    private int ID;

    public person(String n, String sn, String a, String p, int i) throws Exception {
        if(n.isEmpty() || sn.isEmpty() || a.isEmpty() || p.isEmpty() || i <= 0) {
            throw new Exception();
        }
        name = n;
        surname = sn;
        address = a;
        phone = p;
        ID = i;
    }

    public int get_ID() {
        return ID;
    }

    public void print_person() {
        System.out.print(String.format(
                    "Name & Surname: %s %s\nAddress: %s\nPhone: %s\nID: %d\n",
                    name, surname, address, phone, ID
                    ));
    }
}
