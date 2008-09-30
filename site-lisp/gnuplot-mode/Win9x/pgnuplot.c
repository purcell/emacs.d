#include <windows.h>

/* You MUST change the value of FULLPATH to the actual location of the
 * gnuplot executable on your computer.  */
#define FULLPATH    "e:/prg/gp36/gp37hbb/wgnuplot.exe"
#define WINDOWNAME  "gnuplot"
#define PARENTCLASS "wgnuplot_parent"
#define TEXTCLASS   "wgnuplot_text"
#define GRAPHWINDOW "gnuplot graph"
#define GRAPHCLASS  "wgnuplot_graph"

int main (int argc, char *argv[])
{
  /* Customize this path if needed */
  char *d, buf[80];
  HWND hwnd_parent;
  HWND hwnd_text;
  BOOL startedWgnuplotMyself = FALSE;

  /* First, try to find if there is an instance of gnuplot
   * running, already. If so, use that. */
  hwnd_parent = FindWindow(PARENTCLASS, WINDOWNAME);

  if ( ! hwnd_parent) {
    /* None there, so start one: load gnuplot (minimized in order to
     * show only the graphic window). Pass all command line arguments
     * on to wgnuplot, by concatting the wgnuplot full path name and
     * the given arguments, building up a new, usable command line:
     */
    char *cmdline = strdup (FULLPATH);

    while (*(++argv)) {
      /* Puzzle together a working from the given arguments. To account
       * for possible spaces in arguments, we'll have to put double quotes
       * around each of them:
       */
      /* FIXME: doesn't check for out of memory */
      cmdline = realloc(cmdline, strlen(cmdline)+3+strlen(argv[0]));
      strcat(cmdline, " \"");
      strcat(cmdline, *argv);
      strcat(cmdline, "\"");
    }

    if (WinExec(cmdline, SW_SHOWMINNOACTIVE) < 32) {
      printf("Can't load gnuplot\n");
      exit(EXIT_FAILURE);
    }

    startedWgnuplotMyself = TRUE;

    /* wait for the gnuplot window */
    /* FIXME: is this necessary? As documented, WinExec shouldn't return
     * until wgnuplot first calls GetMessage(). By then, the window should
     * be there, shouldn't it?
     */
    Sleep(1000);
    hwnd_parent = FindWindow(PARENTCLASS, WINDOWNAME);
  }

  if ( ! hwnd_parent) {
    /* Still no gnuplot window? Problem! */
    printf("Can't find the gnuplot window");
    exit(EXIT_FAILURE);
  }

  /* find the child text window */
  hwnd_text = FindWindowEx(hwnd_parent, NULL, "wgnuplot_text", NULL );

  if (isatty(fileno(stdin))) {
    /* Do not try to read from stdin if it hasn't been redirected
     * (i.e., it should read from a pipe or a file) */
    exit(EXIT_SUCCESS);
  }

  /* wait for commands on stdin, and pass them on to the wgnuplot text
   * window */
  do {
    d = fgets(buf, sizeof(buf), stdin);

    if (NULL == d) {
      if (startedWgnuplotMyself) {
        /* close gnuplot cleanly: */
        strcpy (buf, "exit");
        d = buf;
        startedWgnuplotMyself = FALSE;
      } else {
        break;
      }
    }

    while(*d) {
      PostMessage(hwnd_text, WM_CHAR, *d, 1L);
      d++;
    }
  } while (NULL != d);

  /* Just in case stdin didn't have a terminating newline, add one: */
  PostMessage(hwnd_text, WM_CHAR, '\n', 1L);

  return EXIT_SUCCESS;
}
