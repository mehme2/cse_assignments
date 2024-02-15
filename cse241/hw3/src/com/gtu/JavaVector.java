package com.gtu;

public class JavaVector<T> implements JavaContainer<T> {
    private Object[] elements;
    private int used;

    /**Same as JavaVector(8).*/
    public JavaVector() {
        this(8);
    }

    /**Instantiates the vector array with the given capacity.*/
    public JavaVector(int cap) {
        elements = new Object[cap];
        used = 0;
    }

    /**Adds the element e and doubles the array capacity if needed.*/
    public void add(T e) {
        if(used >= elements.length) {
            Object[] n = new Object[elements.length * 2];
            for(int i = 0; i < elements.length; ++i) {
                n[i] = elements[i];
            }
            elements = n;
        }
        elements[used++] = e;
    }

    /**Removes the first instance of e.*/
    public void remove(T e) {
        int idx = find(e);
        if(idx != -1) {
            used--;
            for(int i = idx; i < used; ++i) {
                elements[i] = elements[i + 1];
            }
            elements[used] = null;
        }
    }

    public int size() {
        return used;
    }

    public Iterator<T> getIterator() {
        return new VectorIterator();
    }

    /**Returns the index of first instance of e found.
     * Returns -1 if e is not found.
     * Comparison is made with the equals(Object) method.*/
    public int find(T e) {
        for(int i = 0; i < used; ++i) {
            if(e.equals(elements[i])) {
                return i;
            }
        }
        return -1;
    }

    /**Concanates every elements string returned by toString().*/
    public String toString() {
        String s = new String();
        for(int i = 0; i < used; ++i) {
            s = s.concat(String.format("%s ", elements[i].toString()));
        }
        return s;
    }

    public boolean equals(Object o) {
        if(getClass() == o.getClass()) {
            var v = (JavaVector<T>) o;
            boolean eq = used == v.used;
            for(int i = 0; eq && i < used; ++i) {
                eq = elements[i].equals(v.elements[i]);
            }
            return eq;
        }
        return false;
    }

    public class VectorIterator implements Iterator<T> {
        private int idx;

        private VectorIterator() {
            idx = 0;
        }

        public boolean hasNext() {
            return idx < used - 1;
        }

        public T next() {
            if(hasNext()) {
                return (T) elements[idx++];
            }
            return null;
        }
    }
}
