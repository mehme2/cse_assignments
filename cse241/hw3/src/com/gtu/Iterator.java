package com.gtu;

public interface Iterator<T> {
    /**Returns true if there is a next element.*/
    boolean hasNext();
    /**Returns the next element and advances the iterator*/
    T next();
}
