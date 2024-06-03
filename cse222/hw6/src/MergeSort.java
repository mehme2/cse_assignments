public class MergeSort extends SortAlgorithm {
	
	public MergeSort(int input_array[]) {
		super(input_array);
	}
	
	private void merge(int start, int mid, int end){
        // fill this method
        // lengths of left and right array
        int ll = mid - start + 1;
        int rl = end - mid;
        // create new arrays to seperate them
        int[] left = new int[ll];
        int[] right = new int[rl];
        // copy from the original array
        for(int i = 0; i < ll; ++i) left[i] = arr[i + start];
        for(int i = 0; i < rl; ++i) right[i] = arr[i + mid + 1];
        // indices of left, right and the original array
        int li = 0, ri = 0, ai = start;
        while(li < ll && ri < rl) {
            ++comparison_counter;
            // put the smaller of the two arrays to the next index
            arr[ai++] = left[li] <= right[ri] ? left[li++] : right[ri++];
        }
        // insert the remaining numbers
        while(li < ll) arr[ai++] = left[li++];
        while(ri < rl) arr[ai++] = right[ri++];
    }

    private void sort(int start, int end){
        // fill this method
        if(start < end) {
            // find the middle index to split the array
            int mid = start + (end - start) / 2;
            // sort the both halves seperately
            sort(start, mid);
            sort(mid + 1, end);
            // merge the sorted arrays
            merge(start, mid, end);
        }
    }
    
    @Override
    public void sort() {
    	sort(0, arr.length - 1);
    }
    
    @Override
    public void print() {
    	System.out.print("Merge Sort\t=>\t");
    	super.print();
    }
}
