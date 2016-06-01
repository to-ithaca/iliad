package iliad.kernel.platform.win32;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Native;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.WinDef.HDC;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.HCURSOR;
import com.sun.jna.platform.win32.WinDef.HINSTANCE;
import com.sun.jna.win32.W32APIOptions;

public interface User32 extends com.sun.jna.platform.win32.User32 {
    /** The instance. */
    User32 INSTANCE = (User32) Native.loadLibrary("user32", User32.class, W32APIOptions.DEFAULT_OPTIONS); //Native.loadLibrary("user32", User32.class, W32APIOptions.DEFAULT_OPTIONS);

   class PAINTSTRUCT extends Structure {
       public HDC hdc;
       public boolean fErase;
       public RECT rcPaint;
       public boolean fRestore;
       public boolean fIncUpdate;
       public byte rgbReserved[] = new byte[32];
       
       @Override
       protected List<?> getFieldOrder() {
           return Arrays.asList("hdc", "fErase", "rcPaint", "fRestore", "fIncUpdate", "rgbReserved");
       }
    }

    HDC BeginPaint(HWND hWnd, PAINTSTRUCT lpPaint);
    boolean EndPaint(HWND hWnd, PAINTSTRUCT lpPaint);
    HCURSOR LoadCursor(HINSTANCE hInstance, int lpCursorName);
}
