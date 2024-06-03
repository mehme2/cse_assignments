public class preprocessor {
	private String initial_string;
	private String preprocessed_string;
		
	public preprocessor(String str) {
        initial_string = str;
	}

	public void preprocess() {
		// do not edit this method
		capitalize();
		clean();
	}
	
	private void capitalize() {
        preprocessed_string = initial_string.toUpperCase();
	}

	private void clean() {
        //capitalized string from the previous stage
        String unclean = preprocessed_string;
        preprocessed_string = "";
        for(char c : unclean.toCharArray()) {
            //if the character is an upper case letter add it to the final string
            if(Character.isUpperCase(c)) {
                preprocessed_string += c;
            }
        }
	}
	
	public String get_preprocessed_string() {
        return preprocessed_string;
	}
}
