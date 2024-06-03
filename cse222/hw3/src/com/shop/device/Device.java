package com.shop.device;

public interface Device {
    /**
     * Returns the category as a string.
     */
    String getCategory();

    /**
     * Returns the name as a string.
     */
    String getName();

    /**
     * Returns the price as a float.
     */
    float getPrice();

    /**
     * Returns the quantity as an int.
     */
    int getQuantity();

    /**
     * Changes the current device name.
     * @throws Exception if _name is empty
     */
    void setName(String _name) throws Exception;

    /**
     * Changes the current device price.
     * @throws Exception if _price is not positive
     */
    void setPrice(float _price) throws Exception;

    /**
     * Changes the current device quantity.
     * @throws Exception if _quantity is negative
     */
    void setQuantity(int _quantity) throws Exception;

    /**
     * Prints the information about this device to stdout.
     */
    void print();
}
