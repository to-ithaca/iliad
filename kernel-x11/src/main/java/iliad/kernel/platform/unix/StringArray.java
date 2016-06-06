package iliad.kernel.platform.unix;

import com.sun.jna.Memory;
import com.sun.jna.Pointer;
import com.sun.jna.PointerType;

public class StringArray extends PointerType {

    public StringArray() {
        super();
    }

    public StringArray(String string) {
        super();
        Pointer pointer = new Memory(Pointer.SIZE);
        //jna adds an extra padding byte, so we need to add 1 to the length
        Pointer valuePointer = new Memory(string.getBytes().length + 1);
        valuePointer.setString(0, string);
        pointer.setPointer(0, valuePointer);
        setPointer(pointer);
    }
}
