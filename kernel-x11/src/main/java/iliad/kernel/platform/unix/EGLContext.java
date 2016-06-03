package iliad.kernel.platform.unix;

import com.sun.jna.Pointer;
import com.sun.jna.PointerType;

public final class EGLContext extends PointerType {
    public EGLContext() {
        setPointer(Pointer.createConstant(0));
    }
}
