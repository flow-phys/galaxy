/**
 * @file   simple-plot.c
 * @author Emmet Caulfield
 * @brief  Provides a simple Xlib graphing server
 *
 * Ideally, I don't want the client code to have to link with Xlib or
 * have to grapple with multi-threading in any way, since this could
 * make debugging the client a real pain.
 *
 * My solution is to launch a graphing server as a seperate process,
 * and have the plot() routine send the data to be plotted via a
 * pipe. The program still has to link with Xlib, but other than that,
 * all the X functionality is totally isolated in the child, so
 * (assuming it's reasonably robust) it shouldn't cause any confusion
 * when debugging the parent. We could avoid this by making the server
 * completely seperate and exec()-ing it, after the fork(), but
 * linking with Xlib isn't that big a deal.
 *
 * I used a pipe, rather than some other nice POSIX IPC, because I
 * don't want any IPC stuff to persist after a crash, nor have to
 * explain 'ipcrm', which I'd have to do if I used shared memory or
 * message queues :o)
 *
 * To make this work, we have to drive the main loop off select(), and
 * watch both the pipe from the client and the XServer for events,
 * then depending on which FD (data pipe or X) caused select to
 * return, Do The Right Thing (TM) --- either eat all the pending X
 * events, or read the new data from the pipe and plot it.
 *
 * The pipe reads and writes are wrapped in loops because there exists
 * a slim chance of partial reads. This is a standard technique for
 * sockets and pipes. POSIX does require that pipe writes are atomic
 * provided that they're less than PIPE_BUF bytes, which is only
 * guaranteed to be 512, though typically 4096 (e.g. Linux and
 * Cygwin). Even 512B is probably enough, but it's as well to be
 * defensive and do it the right way.
 *
 * Most of the code in here is internal to the server/child and this
 * file, so nothing other than the 3 functions in the public interface
 * need external linkage.
 *
 * We should really install a SIG_PIPE handler, and be more rigorous
 * about checking return values.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <unistd.h>
#include <signal.h>
#include <sys/select.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>

#include <simple-plot.h>

#ifndef PACKAGE_NAME
#   define PACKAGE_NAME "simple-plot"
#endif
//#define PONG 1
#undef PONG
#include <error-macros.h>


/*==========================================\
|  PRIVATE INTERNAL --- SERVER/CHILD CODE   |
\==========================================*/

#define DEF_BORDER_WIDTH 0	/* X border -- not really needed    */
#define GRAT_BOX_WIDTH  1	/* Graticule out box line-width     */
#define GRAT_LINE_WIDTH 1	/* Width of graticule inner lines   */
#define PLOT_LINE_WIDTH 1	/* Plot line width		    */
#define PADDING 10		/* Window edge to graticule padding */
#define N_DIVS 10		/* Number of x/y grid lines         */

#define NC_BUFSZ 20		/* Buffer size for number of calls  */


/**
 * Color codes (indexes into _colors)
 */
enum _colorcode {
    BLACK,
    WHITE,
    DKGRAY,
    GRAY,
    LTGRAY,
    RED,
    GREEN,
    BLUE,
    N_COLORS
};


/**
 * Hex color triples in #RRGGBB format
 */
static const char *_colors[] = {
    "#000000",
    "#FFFFFF",
    "#303030",
    "#909090",
    "#B0B0B0",
    "#FF0000",
    "#00FF00",
    "#0000FF"
};


/**
 * Client/server shutdown code
 */
static const unsigned short _shutdown=0xffff;


/**
 * Data that we always need to transmit to the server:
 */
typedef struct {
    unsigned short nels;
    short _pad;
} sp_data_s;

/**
 * Default initializer
 */
#define SP_DATA_S_INIT {0, 0}


/**
 * Server configuration struct
 */
typedef struct {
    Display   *display;
    Window     window;
    Colormap   colormap;
    int        screen;
    GC         gc;
    XColor     color[N_COLORS];

    int        width;
    int        height;
    int        top;
    int        left;

    int        pipe;
    sp_data_s  meta;
    float      *x;
    float      *y;

    XPoint    *points;
    int        npoints;
} sp_config_s;


/*
 * Can never remember which is which in pipe()
 */
#define READ_END  0
#define WRITE_END 1


/**
 * @brief Creates and maps the plotting window and populates the
 * config struct accordingly.
 *
 * This runs in the server/child and is not exposed to the
 * parent/client code.
 */
static Window _new_window(sp_config_s *sp)
{
    int i;
    unsigned long vmask=0L;
    XGCValues v;
    Atom a;

    PING();

    assert(sp!=NULL);

    sp->display=XOpenDisplay( getenv("DISPLAY") );
    RCHECK(sp->display, !=NULL, XOpenDisplay);

    sp->screen = DefaultScreen(sp->display);
    sp->width  = 3*DisplayWidth(sp->display,sp->screen)/4;
    sp->height = 3*DisplayHeight(sp->display,sp->screen)/4;
    sp->left   = sp->width/10;
    sp->top    = sp->height/10;

    sp->window = XCreateSimpleWindow(sp->display, 
				     RootWindow(sp->display, sp->screen),
				     sp->left, sp->top,
				     sp->width, sp->height, 
				     DEF_BORDER_WIDTH,
				     WhitePixel(sp->display, sp->screen),
				     BlackPixel(sp->display, sp->screen)
	);
    /*
     * If we don't add WM_DELETE_WINDOW to WM_PROTOCOLS (and test for
     * it in the X event loop) we can, depending on the WM, lose the
     * opportunity to handle window close events. When the user clicks
     * the close button, the WM just closes the display without us
     * getting a chance to shutdown cleanly, and the server/child
     * exits with an X error.
     */
    a=XInternAtom(sp->display, "WM_DELETE_WINDOW", False), 
    XChangeProperty(sp->display, sp->window, 
		    XInternAtom(sp->display, "WM_PROTOCOLS", False),
		    XA_ATOM,
		    32, 
		    PropModeAppend,
		    (unsigned char *)&a,
		    1);

    XSelectInput(sp->display, sp->window, 
		 ExposureMask | KeyPressMask | StructureNotifyMask );
    XStoreName(sp->display, sp->window, PACKAGE_NAME);

    XMapWindow(sp->display, sp->window);
    sp->colormap=DefaultColormap(sp->display, sp->screen);
    for(i=0; i<N_COLORS; i++) {
	XParseColor(sp->display, sp->colormap, _colors[i], &sp->color[i]);
	XAllocColor(sp->display, sp->colormap, &sp->color[i]);
    }

    sp->gc=XCreateGC(sp->display, sp->window, vmask, &v);
    RCHECK(sp->gc, !=NULL, XCreateGC);
    XSetBackground(sp->display, sp->gc, sp->color[BLACK].pixel);

    XFlush(sp->display);

    return sp->window;
}


/**
 * Frees server/child resources and exit()s. 
 *
 * This runs in the server/child and is not exposed to the
 * parent/client code.
 */
static void _close(sp_config_s *sp)
{
    assert(sp!=NULL);
    close(sp->pipe);
    XFreeGC(sp->display, sp->gc);
    XCloseDisplay(sp->display);
    if( sp->points!=NULL )
	free(sp->points);
    if( sp->x!=NULL )
	free(sp->x);
    if( sp->y!=NULL )
	free(sp->y);
    free(sp);
    exit(EXIT_SUCCESS);
}


/**
 * Converts plot data to window coordinates according to given
 * min/max y values and current window dimensions and plots it.
 *
 * This runs in the server/child and is not exposed to the
 * parent/client code.
 */
static void _plot(sp_config_s *sp)
{
    int i;
    int t, l, w, h; /* top, left, width, height */
    float vscale;
    float hscale;
    XPoint *p;
    static int call = 0;
    int  clen;
    char buf[NC_BUFSZ];   /* Buffer for call as string */

    PING();

    assert(sp!=NULL);

    if(sp->meta.nels==0) {
	/* Nothing to plot */
	return;
    }

    if(sp->meta.nels > sp->npoints && sp->points!=NULL) {
	/* Not enough room for new points */
	free(sp->points);
	sp->points=NULL;
    }
    if( sp->points == NULL ) {
	/* Allocate memory for points */
	sp->points=malloc(sp->meta.nels*sizeof(XPoint));
	sp->npoints=sp->meta.nels;
	RCHECK(sp->points, !=NULL, malloc);
    }
    p=sp->points; // Pure laziness

    t=PADDING;
    l=PADDING;
    w=sp->width-2*PADDING;
    h=sp->height-2*PADDING;

    vscale=(float)h;
    hscale=(float)w;

    // FIXME: Need to work this shit out
    for(i=0; i<sp->meta.nels; i++) {
	p[i].x=l+  (int)roundf(hscale*sp->x[i]);
	p[i].y=t+h-(int)roundf(vscale*sp->y[i]);
    }

    XSetForeground(sp->display, sp->gc, sp->color[WHITE].pixel);
//    XSetLineAttributes(sp->display,sp->gc, PLOT_LINE_WIDTH,
//		       LineSolid, CapRound, JoinRound);
    XDrawPoints(sp->display, sp->window, sp->gc, p, sp->meta.nels, 
	       CoordModeOrigin);

    call++;
    snprintf(buf, NC_BUFSZ, "%d", call-4);
    clen=strlen(buf);
    XDrawString(sp->display, sp->window, sp->gc, 
		l+10, t+20, buf, clen);
    XFlush(sp->display);
}


/**
 * Draws the graticule and, if there's data yet, calls _plot() to plot
 * the data.
 *
 * This runs in the server/child and is not exposed to the
 * parent/client code.
 */
static void _draw(sp_config_s *sp)
{
    int t, l, w, h; /* top, left, width, height */
    int i, p, q;
    float xdiv, ydiv;

    PING();

    assert(sp!=NULL);

    t=PADDING;
    l=PADDING;
    w=sp->width-2*PADDING;
    h=sp->height-2*PADDING;

    xdiv=(float)w/(float)N_DIVS;
    ydiv=(float)h/(float)N_DIVS;
    XSetForeground(sp->display, sp->gc, sp->color[DKGRAY].pixel);
    XSetLineAttributes(sp->display,sp->gc, GRAT_LINE_WIDTH,
		       LineSolid, CapRound, JoinRound);

    for(i=1; i<N_DIVS; i++) {
	p=t+(int)roundf(ydiv*(float)i);
	q=l+(int)roundf(xdiv*(float)i);
	XDrawLine(sp->display, sp->window, sp->gc,
		  l, p, l+w, p);
	XDrawLine(sp->display, sp->window, sp->gc,
		  q, t, q, t+h);
    }

    XSetForeground(sp->display, sp->gc, sp->color[LTGRAY].pixel);
    XSetLineAttributes(sp->display,sp->gc, GRAT_BOX_WIDTH,
		       LineSolid, CapRound, JoinRound);
    XDrawRectangle(sp->display, sp->window, sp->gc, 
		   t,l, w,h);

    if( sp->meta.nels != 0 ) {
	_plot(sp);
    }

    XFlush(sp->display);
}


/**
 * Processes the X event queue.
 *
 * @return True if the close button has been clicked, or the 'q' or
 * 'ESC' keys has been hit, false otherwise.
 *
 * This runs in the server/child and is not exposed to the
 * parent/client code.
 */
static bool _empty_x_event_q(sp_config_s *sp)
{
    bool   done = false;
    XEvent ev;
    KeySym k;
    Atom a;

    PING();

    assert(sp!=NULL);

    while ( XPending(sp->display) ) {
	XNextEvent(sp->display, &ev);

	switch (ev.type) {
        case ConfigureNotify:
	    sp->width  = ev.xconfigure.width;
	    sp->height = ev.xconfigure.height;
	case Expose:
	case MapNotify:
	    _draw(sp);
	    break;
	    
	case ClientMessage:
	    a=XInternAtom(sp->display, "WM_DELETE_WINDOW", False);
	    if( a == (Atom)ev.xclient.data.l[0] ) {
		done=true;
	    }
	    break;
	case DestroyNotify:
	    done = true;
	    break;
	    
        case KeyPress:
	    /* exit */
	    k=XKeycodeToKeysym(sp->display, ev.xkey.keycode, 0);
	    if( k==XK_q || k==XK_Q || k==XK_Escape )
		done = true;
	    break;
	    
        default:
//	    CLOG("Unhandled event: %d\n",ev.type);
	    break;
	} /* switch */
    } /* while */

    return done;
}


/**
 * Main event-loop: see top-of-file comments for details.
 *
 * This runs in the server/child and is not exposed to the
 * parent/client code.
 */
static void _event_loop(sp_config_s *sp)
{
    int dpy_fd;		/* Display FD			   */
    fd_set rfs;		/* Read FD set			   */
    bool done=false;	/* Are we done?			   */
    int max_fd;		/* Max value of any FD in rfs      */
    ssize_t sofar;	/* Bytes received by read() so far */
    ssize_t rc;		/* Code returned by read()	   */
    sp_data_s meta;	/* Metadata for plot		   */
    ssize_t nbytes	/* Number of data bytes to receive */

    PING();

    assert(sp!=NULL);

    dpy_fd = ConnectionNumber(sp->display);
    max_fd = sp->pipe>dpy_fd ? sp->pipe : dpy_fd;
    max_fd++;

    while (!done) {
	PING();

	FD_ZERO( &rfs );
	FD_SET ( sp->pipe, &rfs );
	FD_SET ( dpy_fd, &rfs );

	select(max_fd, &rfs, NULL, NULL, NULL);

	if( FD_ISSET(sp->pipe,&rfs) ) {
	    /*
	     * sizeof(sp_data_s) is *well* below PIPE_BUF, so we can
	     * be sure we get it in one read.
	     */
	    rc=read(sp->pipe, (void *)&meta, sizeof(sp_data_s));
	    DIE_IF(rc!=sizeof(sp_data_s), "Bad read on pipe");
	    if( meta.nels == _shutdown ) {
		done=true;
	    } else {
		/*
		 * Make sure there's enough room for the data
		 * to be received.
		 */
		if( sp->x != NULL && sp->y != NULL 
		    && sp->meta.nels < meta.nels ) {
		    free(sp->x); sp->x = NULL;
		    free(sp->y); sp->y = NULL;
		}
		sp->meta = meta;
		nbytes=meta.nels*sizeof(float);
		if( sp->x == NULL ) {
		    sp->x=malloc(nbytes);
		    RCHECK(sp->x, !=NULL, malloc);
		}
		if( sp->y == NULL ) {
		    sp->y=malloc(nbytes);
		    RCHECK(sp->y, !=NULL, malloc);
		}

		sofar=0;
		do {
		    errno=0;
		    rc=read(sp->pipe, (void *)sp->x, nbytes-sofar);
		    if( rc==-1 && errno!=EINTR ) {
			DIE("Bad read on pipe");
		    } else {
			sofar+=rc;
		    }
		} while(sofar<nbytes);
		sofar=0;

		do {
		    errno=0;
		    rc=read(sp->pipe, (void *)sp->y, nbytes-sofar);
		    if( rc==-1 && errno!=EINTR ) {
			DIE("Bad read on pipe");
		    } else {
			sofar+=rc;
		    }
		} while(sofar<nbytes);
	    }

	    if( !done ) {
		/*
		 * We protectively process the X event queue here
		 * because otherwise, if the pipe is saturated by the
		 * client, X resize events can be lost, or deferred so
		 * long they might as well be lost, while we're busy
		 * servicing the pipe and plotting --- we never get a
		 * chance to update the window size to the new
		 * geometry. The worst that can happen is an extra
		 * call to _empty_x_event_q() that does nothing.
		 */
		done=_empty_x_event_q(sp);
		if( !done ) {
		    XClearWindow(sp->display, sp->window);
		    _draw(sp);
		}
	    }
	}

	if( FD_ISSET(dpy_fd,&rfs) ) {
	    done=_empty_x_event_q(sp);
	}
    }
}



/*==========================================\
|  PUBLIC INTERFACE --- CLIENT/PARENT CODE  |
\==========================================*/

/**
 * Plots the given data
 *
 * @param  sph    Handle returned by sp_init()
 * @param  n      Number of x-y pairs
 * @param  x      x data
 * @param  y_max  y data
 * @return        0 on success, -1 on error.
 */
int sp_plot(const sp_handle_s *const sph, 
	    unsigned n,
	    const float *const x,
	    const float *const y)
{
    pid_t pid;
    int status;
    ssize_t sofar=0;
    ssize_t nbytes=0;
    ssize_t rc;
    sp_data_s meta=SP_DATA_S_INIT;


    assert(sph!=NULL);
    assert(x!=NULL);
    assert(y!=NULL);

    meta.nels = n;
    nbytes = meta.nels*sizeof(float);

    pid=waitpid(sph->cpid, &status, WNOHANG);
    if( pid==0 ) {
	rc=write( sph->pipe, &meta, sizeof(meta) );
	DIE_IF(rc==-1, "Error writing to pipe to plot server");
	sofar=0;
	while( sofar < nbytes ) {
	    errno=0;
	    rc=write(sph->pipe, (void *)x, nbytes-sofar);
	    if( rc==-1 && errno!=EINTR ) {
		WARN("Error writing to pipe to graphing server");
		return -1;
	    } else {
		sofar+=rc;
	    }
	}
	sofar=0;
	while( sofar < nbytes ) {
	    errno=0;
	    rc=write(sph->pipe, (void *)y, nbytes-sofar);
	    if( rc==-1 && errno!=EINTR ) {
		WARN("Error writing to pipe to graphing server");
		return -1;
	    } else {
		sofar+=rc;
	    }
	}
    } else {
	WARN("Graphing server exited with status %x", status);
	return -1;
    }

    return 0;
}


/**
 * Initializes the simple-plot subsystem.
 *
 * @return  Handle to identify simple-plot connection.
 */
sp_handle_s *sp_init(void)
{
    int rc;
    sp_config_s *spc;
    sp_handle_s *sph;
    pid_t pid;
    int p[2];

    rc=pipe(p);
    RCHECK(rc, ==0, pipe);

    pid=fork();
    RCHECK(pid, >=0, fork);

    if( pid==0 ) {
	/* We're the child */
#ifdef GPROF
	errno=0;
	rc=mkdir("gc", 0777);
	if( rc==0 || (rc==-1 && errno==EEXIST) )
	    chdir("gc");
	else
	    WARN("Unable to 'cd gc' in child/server");
#endif
	close(p[WRITE_END]);
	spc=calloc(1,sizeof(sp_config_s));
	RCHECK(spc, !=NULL, calloc);
	spc->pipe=p[READ_END];
	_new_window(spc);
	_draw(spc);
	_event_loop(spc);
	/* We shouldn't really get here */
	_close(spc);
	return NULL;
    } else {
	/* We're the parent */
	close(p[READ_END]);
	sph=calloc(1,sizeof(sp_handle_s));
	RCHECK(sph, !=NULL, calloc);
	sph->cpid=pid;
	sph->pipe=p[WRITE_END];
	return sph;
    }

    return NULL;
}


/**
 * Closes down the simple-plot subsystem frees its resources.
 *
 * @param  sph  Handle returned by sp_init()
 *
 * The argument (sph) is freed. Attempting to free() or dereference it
 * after a call to sp_close() will cause a segfault.
 */
void sp_close(sp_handle_s *sph) {
    int status;
    ssize_t nbytes;
    sp_data_s meta=SP_DATA_S_INIT;

    assert(sph!=NULL);

    meta.nels=_shutdown;
    nbytes=write(sph->pipe, (void *)&meta, sizeof(sp_data_s));
    if( nbytes!=sizeof(sp_data_s) ) {
	kill(sph->cpid, SIGTERM);
    }
    close(sph->pipe);
    waitpid(sph->cpid, &status, 0);
    free(sph);
}

/*
 * A quick hack to make this work with Fortran, evilly depends on a
 * long and a pointer being the same size
 */
long sp_init_(void)
{
    return (long)sp_init();
}

int sp_plot_(long *sph, int *n, float *x, float *y)
{
    return sp_plot(
	(const sp_handle_s *const)*sph,
	(unsigned)*n,
	(const float *const)x,
	(const float *const)y
	);
}

void sp_close_(long *sph)
{
    sp_close( (sp_handle_s *)*sph );
}

