package iliad.kernel.platform.unix;

import com.sun.jna.Native;

public interface X11 extends com.sun.jna.platform.unix.X11 {
    
    X11 INSTANCE = (X11) Native.loadLibrary("X11", X11.class);
    int XSetWMProtocols(com.sun.jna.platform.unix.X11.Display display, 
                        com.sun.jna.platform.unix.X11.Window window, 
                        com.sun.jna.platform.unix.X11.Atom[] atom, 
                        int count);
}


