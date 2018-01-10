# Makefile-  General Makefile for Final, CME212
# Will build the executables from source and clean object 
# files when prompted
##  $Rev: 51 $
##  $Id: Makefile 51 2010-06-04 06:58:44Z bolson@stanford.edu $

BINS = galaxy bhgalaxy
CC = gcc
LD = gcc
FC = gfortran
XFLAGS   = -Wall -Wextra -g -O3 -funroll-loops -lblas ###-pg
CPPFLAGS = -I.
CFLAGS  = $(XFLAGS) -DEXE=demo
FFLAGS  = $(XFLAGS) -std=f95 -fall-intrinsics
LDFLAGS = $(XFLAGS) -L/usr/X11/lib
LDLIBS  = -lm -lX11

all:	$(BINS)

%.o:	%.c %.h 
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

galaxy: galaxy.f90 simple-plot.o
	$(FC) $(FFLAGS) $< -o $@ simple-plot.o $(LDFLAGS) $(LDLIBS)

bhgalaxy: bhgalaxy.f90 simple-plot.o
	$(FC) $(FFLAGS) $< -o $@ simple-plot.o $(LDFLAGS) $(LDLIBS)

clean:
	rm -f $(BINS) *.o *~ core *.mod

.PHONY: clean all
