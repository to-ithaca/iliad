#+x11
package iliad.platform.unix;

import com.sun.jna.Native;
import com.sun.jna.platform.unix.X11.Display;
import com.sun.jna.platform.unix.X11.Window;
import com.sun.jna.platform.unix.X11.Atom;

public interface X11 extends com.sun.jna.platform.unix.X11 {
    
    X11 INSTANCE = (X11) Native.loadLibrary("X11", X11.class);
    int XSetWMProtocols(Display display, 
                        Window window, 
                        Atom[] atom, 
                        int count);

    int XInitThreads();

    void XLockDisplay(Display display);
    void XUnlockDisplay(Display display);
}
#-x11
