public class SelectionSort extends SortAlgorithm {

	public SelectionSort(int input_array[]) {
		super(input_array);
	}

    @Override
    public void sort() {
        // fill this method
        for(int i = 0; i < arr.length - 1; ++i) {
            // index of the lowest number
            int minIdx = i;
            for(int j = i + 1; j < arr.length; ++j) {
                ++comparison_counter;
                // check if the number is smaller
                if(arr[minIdx] > arr[j]) minIdx = j;
            }
            // put the smaller number to the next place
            swap(minIdx, i);
        }
    }

    @Override
    public void print() {
    	System.out.print("Selection Sort\t=>\t");
    	super.print();
    }
}
