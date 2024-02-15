package com.gtu;

public interface JavaContainer<T> {
    /**Adds an element to the container.*/
    void add(T e);
    /**Removes a given element.*/
    void remove(T e);
    /**Returns the number of elements.*/
    int size();
    /**Returns an Iterator object.*/
    Iterator<T> getIterator();
}
