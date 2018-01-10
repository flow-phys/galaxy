!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  File:        galaxy.f90
!!
!!  Description: Performs N-body simulation of N stars
!!         of unit mass under gravitational forces.  This
!!         version is simply the brute force method.
!!
!!  Author:      Britton J. Olson
!!
!!  Date:        June-1-2010
!!
!!  $Rev: 45 $
!!  $Id: galaxy.f90 45 2010-06-03 00:08:09Z bolson@stanford.edu $   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE estrella
  REAL, DIMENSION(:,:), ALLOCATABLE :: star 
  INTEGER, PARAMETER :: vars = 4         !  Number of variables per star
  INTEGER, PARAMETER :: pX = 1           !  The X-position index
  INTEGER, PARAMETER :: pY = 2           !  The Y-position index
  INTEGER, PARAMETER :: vX = 3           !  The X-velocity index
  INTEGER, PARAMETER :: vY = 4           !  The Y-velocity index
  REAL, PARAMETER :: alpha = 0.5         !  Essentricity of initial field
  REAL, PARAMETER :: V0 = 50.0           !  Initial velocity of particles
  REAL, PARAMETER :: dt = .0001          !  Time step size
  REAL, PARAMETER :: G = .05             !  Gravitational constant
  REAL, PARAMETER :: huge = 1.0e12       !  Large number
END MODULE estrella

PROGRAM galaxy
  USE estrella
  INTEGER(8), EXTERNAL :: sp_init        !  Vis plot init function
  EXTERNAL :: sp_plot, sp_close          !  Vis plot plot and close function
  INTEGER(8) :: sph                      !  Vis handle
  INTEGER :: rc                          !  Vis handle
  LOGICAL :: vis = .FALSE.               !  Vis flag 
  INTEGER :: tmax                        !  Max sim time steps
  INTEGER :: n                           !  N-bodies of unit mass
  INTEGER :: i                           !  Working index for time loop
  CHARACTER(LEN=20) :: buffer            !  Input buffer
  INTEGER :: Narg                        !  Input argument counter
  
  !! Check to make sure an input file was given
  IF (iargc() < 2 .OR. iargc() > 3) STOP 'Usage: ./galaxy [-v optional vis] [Time Steps] [Stars]' 
  Narg = 1  

  !! Get the vis-flag option if it has been specified
  IF (iargc() == 3) THEN
     CALL GETARG(Narg,buffer)
     Narg = Narg + 1
     IF ( trim(buffer) .NE. '-v') THEN
        WRITE(*,*) 'Unknown option:',trim(buffer)
        STOP 
     ELSE
        vis = .TRUE.
     END IF
  END IF

  !! Check to see that buffer is a positive INTEGER
  CALL GETARG(Narg,buffer)
  Narg = Narg + 1
  IF (VERIFY(TRIM(buffer),'0123456789') .ne. 0) THEN
     WRITE (*,*) 'Error: Please enter an integer > 0 for [Timesteps]'
     STOP
  END IF
  READ (buffer,'(I10)') tmax
  IF (tmax < 1) THEN
     WRITE (*,*) 'Error: Please enter an integer > 0 for [Timesteps]'
     STOP
  END IF
  
  !! Check to see that buffer is a positive INTEGER > 1
  CALL GETARG(Narg,buffer)
  Narg = Narg + 1
  IF (VERIFY(TRIM(buffer),'0123456789') .ne. 0) THEN
     WRITE (*,*) 'Error: Please enter an integer > 1 for [Stars]'
     STOP
  END IF
  READ (buffer,'(I10)') n
  IF (n < 2) THEN
     WRITE (*,*) 'Error: Please enter an integer > 1 for [Stars]'
     STOP
  END IF
  
  !! Initialize the viz-stuff for plotting
  IF( vis ) THEN
     sph = sp_init()
  END IF

  !! Allocate the stars and initialize the positions and velocities
  ALLOCATE( star(n,vars) )
  CALL init()

  !! TIME ADVANCEMNET LOOP HERE
  !! Main Loop over time steps
  DO i=1,tmax
     CALL step()
     IF( vis ) THEN
        rc = sp_plot( sph, n, star(:,pX), star(:,pY) )
     END IF
  END DO

  !! Deallocate memory and close the vis window
  DEALLOCATE(star)   
  IF( vis ) THEN
     CALL sp_close( sph )
  END IF

END PROGRAM galaxy


!! Initialization routine for n-stars
SUBROUTINE init()
  USE estrella
  IMPLICIT NONE
  INTEGER :: n                               !  Number of stars
  REAL, DIMENSION(:), ALLOCATABLE :: theta   !  Random angle for each star   
  REAL, DIMENSION(:), ALLOCATABLE :: R       !  Random radius for each star
  
  !! Get star size and allocate the arrays
  n = size(star,1)
  ALLOCATE(theta(n))
  ALLOCATE(R(n))

  !! Populate with the PRN and scale for each variable  
  CALL random_number(theta)
  CALL random_number(R)
  theta = theta * 2.0e0 * 3.14159265
  R = .25 * R

  !! Set the positions for each star
  star(:,pX) = .5 + R * cos( theta )
  star(:,pY) = .5 + R * sin( theta ) * alpha

  !! Set the velocities for each star
  star(:,vX) = -V0 * R * sin( theta )
  star(:,vY) =  V0 * R * cos( theta )

  !! Free up the used memory
  DEALLOCATE(theta,R)

END SUBROUTINE init

!! Main routine that will update the velocities and positions of each mass
!! for one time step
SUBROUTINE step()
  USE estrella
  IMPLICIT NONE
  INTEGER :: n                               !  Number of stars
  INTEGER :: i,j                             !  Working indices for loops
  REAL, DIMENSION(:), ALLOCATABLE :: r1,r2   !  Vector of unit-vectors pointing to stars
  REAL, DIMENSION(:), ALLOCATABLE :: fx,fy   !  Vector of force vectors for each star
  REAL, DIMENSION(:), ALLOCATABLE :: x,y     !  Tmp position vectors for speed
  REAL, DIMENSION(:), ALLOCATABLE :: ones    !  Vector of 1's used in BLAS
  REAL :: rad                                !  Radius to each star
  REAL :: sdot                               !  Dot product BLAS routine

  !! Get number of stars and allocate the working arrays
  n = size(star,1)
  ALLOCATE(r1(n),r2(n))
  ALLOCATE(fx(n),fy(n))
  ALLOCATE(ones(n))
  ALLOCATE(x(n),y(n))
 
  !! Initialize and set values
  ones = 1.0
  x = star(:,pX)
  y = star(:,pY)
 
  !! Main loop over the N-stars
  DO i=1,n
     r1 = x(i) - x
     r2 = y(i) - y
     r1(i) = 1.0        ! Treat singularity by assigning dummy number
     r2(i) = 1.0        ! Treat singularity by assigning dummy number
     DO j=1,n
        rad = ( r1(j)**2 + r2(j)**2 )
        rad = 1.0/rad
        r1(j) = r1(j)*rad
        r2(j) = r2(j)*rad
     END DO
     r1(i) = 0.0        ! Treat singularity removing force contribution
     r2(i) = 0.0        ! Treat singularity removing force contribution
     r1 = -G*r1
     r2 = -G*r2

     !! BLAS dot product with vector of ones gives a small speed-up in summation
     fx(i) = SDOT(n,r1,1,ones,1)
     fy(i) = SDOT(n,r2,1,ones,1)
     
  END DO

  !! SEMI-IMPLICIT EULER TEMPORAL INTEGRATION- use BLAS for small speed-up
  !! Integrate the Velocity in time: V^{n+1} = V^{n} + dt*F^{n}
  CALL SAXPY(n,dt,fx,1,star(:,vX),1)
  CALL SAXPY(n,dt,fy,1,star(:,vY),1)

  !! Integrate the Position in time: P^{n+1} = P^{n} + dt*P^{n+1}
  CALL SAXPY(n,dt,star(:,vX),1,star(:,pX),1)
  CALL SAXPY(n,dt,star(:,vY),1,star(:,pY),1)

  !! Free memory
  DEALLOCATE(r1,r2,fx,fy,ones)

END SUBROUTINE step
