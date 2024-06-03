import java.util.Map;
import java.util.Iterator;

public class decryptor {
	private Map<Character, Map<Character, Character>> map;
	private String key;
	private String keystream = "";
	private String plain_text = "";
	private String cipher_text;
	
	public decryptor(Map<Character, Map<Character, Character>> _map, String _key, String text) {
        map = _map;
        key = _key;
        cipher_text = text;
	}

	public void decrypt() {
		// do not edit this method
		generate_keystream();
		generate_plain_text();
	}
	
	private void generate_keystream() {
        for(int i = 0; i < cipher_text.length(); ++i) {
            //modulo operator is to avoid overflow and loop to beginning if text is longer than the key
            keystream += key.charAt(i % key.length());
        }
	}
	
	private void generate_plain_text() {
		// You must use map.get(x).keySet() with an iterator in this method
        for(int i = 0; i < cipher_text.length(); ++i) {
            //iterate over every row of the key characters column
            for(char c : map.get(keystream.charAt(i)).keySet()) {
                //value of the row mathces the cipher character
                if(cipher_text.charAt(i) == map.get(keystream.charAt(i)).get(c)) {
                    plain_text += c;
                    break;
                }
            }
        }
	}

	public String get_keystream() {
        return keystream;
	}
	
	public String get_plain_text() {
        return plain_text;
	}
}
