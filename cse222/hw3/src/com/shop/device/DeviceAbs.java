package com.shop.device;

public abstract class DeviceAbs implements Device, Comparable<DeviceAbs> {
    private String name;
    private float price;
    private int quantity;

    /**
     * Constructs a device as following: <br>
     * name: "No Name" <br>
     * price: 0.01f <br>
     * quantity: 0
     * <p>
     * Complexity is O(1).
     */
    public DeviceAbs() {
        name = "No Name";
        price = 0.01f;
        quantity = 0;
    }

    /**
     * Constructs a device with given parameters.
     * <p>
     * Complexity is O(1).
     * @throws Exception see: {@link #setName(String)}, {@link #setPrice(float)}, {@link #setQuantity}
     */
    public DeviceAbs(String _name, float _price, int _quantity) throws Exception {
        setName(_name);
        setPrice(_price);
        setQuantity(_quantity);
    }

    /**
     * {@inheritDoc}
     * Returned string is the final part of the full class name.
     * <p>
     * Complexity is O(1).
     */
    public String getCategory() {
        String cn = getClass().getName().replace('_', ' ');
        return cn.substring(cn.lastIndexOf('.') + 1);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Complexity is O(1).
     */
    public String getName() {
        return name;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Complexity is O(1).
     */
    public float getPrice() {
        return price;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Complexity is O(1).
     */
    public int getQuantity() {
        return quantity;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Complexity is O(1).
     */
    public void setName(String _name) throws Exception {
        if(_name.isEmpty()) {
            throw new Exception("Device name cannot be empty.");
        }
        name = _name;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Complexity is O(1).
     */
    public void setPrice(float _price) throws Exception {
        if(_price <= 0.0f) {
            throw new Exception("Price should be higher than $0.00.");
        }
        price = _price;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Complexity is O(1).
     */
    public void setQuantity(int _quantity) throws Exception {
        if(_quantity < 0) {
            throw new Exception("Quantity cannot be negative.");
        }
        quantity = _quantity;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Complexity is O(1).
     */
    public void print() {
        System.out.println(String.format(
                "Category: %s, Name: %s, Price: $%.2f, Quantity: %d",
                getCategory(), name, price, quantity
                ));
    }

    /**
     * Compares the prices of this and o.
     * <p>
     * Complexity is O(1).
     */
    public int compareTo(DeviceAbs o) {
        return Double.compare(getPrice(), o.getPrice());
    }
}
