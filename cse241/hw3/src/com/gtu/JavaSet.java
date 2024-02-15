package com.gtu;

public class JavaSet<T> extends JavaVector<T> {
    /** Adds element e if it is not found,
     * does nothing otherwise.
     */
    public void add(T e) {
        if(find(e) == -1) {
            super.add(e);
        }
    }

    public boolean equals(Object o) {
        if(getClass() == o.getClass()) {
            var s = (JavaSet<T>) o;
            boolean eq = size() == s.size();
            var i = getIterator();
            while(eq && i.hasNext()) {
                eq = (s.find(i.next()) != -1);
            }
            return eq;
        }
        return false;
    }
}
