#ifndef SIMPLE_PLOT_H
#define SIMPLE_PLOT_H

#include <unistd.h>	/* pid_t    */
#include <limits.h>	/* PIPE_BUF */

typedef struct {
    pid_t cpid;
    int   pipe;
} sp_handle_s;


sp_handle_s *sp_init(void);

int sp_plot(const sp_handle_s *const sph, 
	    unsigned n,
	    const float *const x,
	    const float *const y);

void sp_close(sp_handle_s *sph);

#endif /* SIMPLE_PLOT_H */
