package util;

public class StringUtil {
	public static String getPrettyName (String name) {
    	int i=0;
    	for (; i<name.length(); i++) {
    		if (name.charAt(i)>='0' && name.charAt(i)<='9')
    			break;
    	}
    	if (i!=0)
    		return name.substring(0, i);
    	else
    		return name;
	}
}
