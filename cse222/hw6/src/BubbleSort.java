public class BubbleSort extends SortAlgorithm {

	public BubbleSort(int input_array[]) {
		super(input_array);
	}
	
    @Override
    public void sort() {
    	// fill this method
        for(int i = 0; i < arr.length - 1; ++i) {
            boolean swapped = false;
            for(int j = 0; j < arr.length - i - 1; ++j) {
                ++comparison_counter;
                // compare the element with the next one and swap if needed
                if(swapped = arr[j] > arr[j + 1]) swap(j, j + 1);
            }
            // if no swap happened then the array is already sorted
            if(!swapped) break;
        }
    }
    
    @Override
    public void print() {
    	System.out.print("Bubble Sort\t=>\t");
    	super.print();
    }
}
