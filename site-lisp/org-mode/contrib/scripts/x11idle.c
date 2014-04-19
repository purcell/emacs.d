#include <X11/extensions/scrnsaver.h>
#include <stdio.h>

/* Based on code from
 * http://coderrr.wordpress.com/2008/04/20/getting-idle-time-in-unix/
 *
 * compile with 'gcc -l Xss x11idle.c -o x11idle' and copy x11idle into your
 * path
 */
main() {
    XScreenSaverInfo *info = XScreenSaverAllocInfo();
    //open the display specified by the DISPLAY environment variable
    Display *display = XOpenDisplay(0);

    //display could be null if there is no X server running
    if (info == NULL || display == NULL) {
    return -1;
    }

    //X11 is running, try to retrieve info
    if (XScreenSaverQueryInfo(display, DefaultRootWindow(display), info) == 0) {
	return -1;
    }

    //info was retrieved successfully, print idle time
    printf("%lu\n", info->idle);
    return 0;
}
