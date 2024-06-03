import java.util.Map;

public class encryptor {
	private Map<Character, Map<Character, Character>> map;
	private String key;
	private String keystream = "";
	private String plain_text;
	private String cipher_text = "";
	
	public encryptor(Map<Character, Map<Character, Character>> _map, String _key, String text) {
        map = _map;
        key = _key;
        plain_text = text;
	}
	
	public void encrypt() {
		// do not edit this method
		generate_keystream();
		generate_cipher_text();
	}
	
	private void generate_keystream() {
        for(int i = 0; i < plain_text.length(); ++i) {
            //modulo operator is to avoid overflow and loop to beginning if text is longer than the key
            keystream += key.charAt(i % key.length());
        }
	}
	
	private void generate_cipher_text() {
        for(int i = 0; i < plain_text.length(); ++i) {
            //add the character that corresponds to key and text characters as column and row
            cipher_text += map.get(keystream.charAt(i)).get(plain_text.charAt(i));
        }
	}

	public String get_keystream() {
        return keystream;
	}
	
	public String get_cipher_text() {
        return cipher_text;
	}
}
