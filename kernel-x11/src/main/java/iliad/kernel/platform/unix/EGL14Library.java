package iliad.kernel.platform.unix;

import com.sun.jna.*;
import com.sun.jna.platform.unix.X11;

import java.nio.IntBuffer;

public interface EGL14Library extends Library {

    EGL14Library INSTANCE = (EGL14Library) Native.loadLibrary("EGL", EGL14Library.class);

    //1_0
    boolean eglChooseConfig(EGLDisplay dpy, int[] attrib_list, EGLConfig[] configs, int config_size, IntBuffer num_config);
    boolean eglCopyBuffers(EGLDisplay dpy, EGLSurface surface, com.sun.jna.platform.unix.X11.Pixmap pixmap);
    EGLContext eglCreateContext(EGLDisplay dpy, EGLConfig config, EGLContext share_context, int[] attrib_list);
    EGLSurface eglCreatePbufferSurface(EGLDisplay dpy, EGLConfig config, int[] attrib_list);
    EGLSurface eglCreatePixmapSurface(EGLDisplay dpy, EGLConfig config, com.sun.jna.platform.unix.X11.Pixmap pixmap, int[] attrib_list);
    EGLSurface eglCreateWindowSurface(EGLDisplay dpy, EGLConfig config, com.sun.jna.platform.unix.X11.Window win, int[] attrib_list);
    boolean eglDestroyContext(EGLDisplay dpy, EGLContext ctx);
    boolean eglDestroySurface(EGLDisplay dpy, EGLSurface surface);
    boolean eglGetConfigAttrib(EGLDisplay dpy, EGLConfig config, int attribute, IntBuffer value);
    boolean eglGetConfigs(EGLDisplay dpy, EGLConfig[] configs, int config_size, IntBuffer num_config);
    EGLDisplay eglGetCurrentDisplay();
    EGLSurface eglGetCurrentSurface(int readdraw);
    EGLDisplay eglGetDisplay(com.sun.jna.platform.unix.X11.Display display_id);
    int eglGetError();
    void eglGetProcAddress(String procname);
    boolean eglInitialize(EGLDisplay dpy, IntBuffer major, IntBuffer minor);
    boolean eglMakeCurrent(EGLDisplay dpy, EGLSurface draw, EGLSurface read, EGLContext ctx);
    boolean eglQueryContext(EGLDisplay dpy, EGLContext ctx, int attribute, IntBuffer value);
    String eglQueryString(EGLDisplay dpy, int name);
    boolean eglQuerySurface(EGLDisplay dpy, EGLSurface surface, int attribute, IntBuffer value);
    boolean eglSwapBuffers(EGLDisplay dpy, EGLSurface surface);
    boolean eglTerminate(EGLDisplay dpy);
    boolean eglWaitGL();
    boolean eglWaitNative(int engine);

    //1_1
    boolean eglBindTexImage(EGLDisplay dpy, EGLSurface surface, int buffer);
    boolean eglReleaseTexImage(EGLDisplay dpy, EGLSurface surface, int buffer);
    boolean eglSurfaceAttrib(EGLDisplay dpy, EGLSurface surface, int attribute, int value);
    boolean eglSwapInterval(EGLDisplay dpy, int interval);

    //1_2
    boolean eglBindAPI(int api);
    EGLSurface eglCreatePbufferFromClientBuffer(EGLDisplay dpy, int buftype, EGLClientBuffer buffer, EGLConfig config, int[] attrib_list);
    int eglQueryAPI();
    boolean eglReleaseThread();
    boolean eglWaitClient();

    EGLContext eglGetCurrentContext();
}
