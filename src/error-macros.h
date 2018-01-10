#ifndef ERROR_MACROS_H
#define ERROR_MACROS_H
/**
 * @file    error-macros.h
 * @brief   Generally useful error-check macros.
 * @author  Emmet Caulfield
 * @version 0.1
 *
 * Contains fairly crude error-checking macros to enable bailing with
 * a reasonably consistent-looking and meaningful error message under
 * a variety of circumstances.
 *
 * $Id: error-macros.h 275 2010-05-28 18:58:16Z emmet $
 */

#include <stdlib.h>
#include <stdio.h>
#include <libgen.h>
#include <errno.h>
#include <string.h>

#ifdef HAVE_CONFIG_H
#   include <config.h>
#endif

/** @def RCHECK(C,F)
 *
 * A crude macro intended to be called directly after a library/system
 * call with condition, C, and the name of the function called, F. If
 * C fails, a pretty message is produced and the program aborts. We
 * assume that the function-call is on the previous line.
 */
#define RCHECK(VAR,TST,FN) if(!(VAR TST)) {				\
        int errsv=errno;						\
	(void)fprintf(stderr, "ERROR: Test " #VAR #TST " failed ("	\
		#VAR " was %ld) at %s:%d:%s() in call to " #FN "(): ",	\
		(long)VAR, __FILE__, __LINE__-1, __func__);		\
        if( errsv > 0 )							\
  	    (void)fputs(strerror(errsv), stderr);			\
	else if ( (long)VAR > 0 )					\
  	    (void)fputs(strerror((long)VAR), stderr);			\
        else								\
            (void)fputs("[API-specific error]", stderr);		\
        (void)fputc('\n', stderr);					\
	exit(EXIT_FAILURE);							\
    }

/** @def DIE_IF(C,M)
 *
 * A crude macro which fails with message M if condition C is true.
 */
#define DIE(...) {						\
	(void)fprintf(stderr, "FATAL: %s:%d:%s(): ",		\
		basename(__FILE__), __LINE__, __func__);	\
	(void)fprintf(stderr, __VA_ARGS__);			\
	(void)fputs(".\n", stderr);				\
	exit(EXIT_FAILURE);					\
    }
#define DIE_IF(C,...) if(C) DIE(__VA_ARGS__)


/** @def WARN_IF(C,M)
 *
 * A crude macro which warns and continues with message M if condition
 * C is true.
 */
#define WARN(...) {						\
	(void)fprintf(stderr, "WARNING: %s:%d:%s(): ",		\
		basename(__FILE__), __LINE__, __func__);	\
	(void)fprintf(stderr, __VA_ARGS__);			\
	(void)fputs(".\n", stderr);				\
    }
#define WARN_IF(C,...) if(C) WARN(__VA_ARGS__)


/** @def CLOG(M)
 *
 * Console logs: logs a message to stdout with the name of this
 * executable prepended.
 */
#if defined(PACKAGE_NAME)
#   define CLOG(...) (void)printf(PACKAGE_NAME ": " __VA_ARGS__)
#elif defined(EXE)
#   define CLOG(...) (void)printf(EXE ": " __VA_ARGS__)
#else
#   error "No name defined in CLOG()"
#endif


/** @def PING()
 *
 * Logs filename, function, and line number to the console
 */
#if defined(PONG)
#   define PING() (void)fprintf(stderr, "PING: %s:%d:%s()\n", \
				basename(__FILE__), __LINE__, __func__)
#else
#   define PING()
#endif


/** @def DPRINTF()
 *
 * Logs a message to stderr with the package name prepended
 */
#if defined(DEBUG)
#   define DPRINTF(...) (void)fprintf(stderr, PACKAGE_NAME "> " __VA_ARGS__)
#else
#   define DPRINTF(...)
#endif

#endif /* ERROR_MACROS_H */
