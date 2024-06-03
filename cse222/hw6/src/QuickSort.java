public class QuickSort extends SortAlgorithm {

	public QuickSort(int input_array[]) {
		super(input_array);
	}
	
    private int partition(int start, int end) {
        // fill this method
        // index of the next swap
        int i = start;
        for(int j = start; j < end; ++j) {
            ++comparison_counter;
            // make a swap if the number is lower than pivot (end of array)
            if(arr[j] < arr[end]) swap(i++, j);
        }
        // finally put the pivot to the next index
        swap(i, end);
        // return the place of the pivot
        return i;
    }

    private void sort(int start, int end){
        // fill this method
        if(start < end) {
            // partition returns the index of pivot
            int i = partition(start, end);
            // sort the left of pivot
            sort(start, i - 1);
            // sort the right of pivot
            sort(i + 1, end);
        }
    }

    @Override
    public void sort() {
    	sort(0, arr.length - 1);
    }

    @Override
    public void print() {
    	System.out.print("Quick Sort\t=>\t");
    	super.print();
    }
}
