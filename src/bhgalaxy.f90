!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  File:        bhgalaxy.f90
!!
!!  Description: Performs N-body simulation of N stars
!!         of unit mass under gravitational forces.  This
!!         version is the Barnes-Hut method.
!!
!!  Author:      Britton J. Olson
!!
!!  Date:        June-1-2010
!!
!!  $Rev: 54 $
!!  $Id: bhgalaxy.f90 54 2010-06-04 18:49:18Z bolson@stanford.edu $   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!! ---------- MODULES ------------ !!!
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
  REAL, PARAMETER :: beta = 0.85
END MODULE estrella

MODULE quad_tree
  TYPE :: tree_branch                          !  Derived data-type for branches in quad_tree
     INTEGER :: id                             !  The unique ID number for each branch
     INTEGER :: level                          !  The level or depth of branch (1-top....n-bottom)
     INTEGER :: xloc                           !  The x-index of the grid partition
     INTEGER :: yloc                           !  The y-index of the grid partition
     INTEGER, DIMENSION(4) :: children         !  The 4 ID's of your children
     INTEGER :: parent                         !  The ID of your parent
     INTEGER, DIMENSION(:), POINTER :: stars   !  Array of star ID's which in branch
     INTEGER :: total_stars                    !  Total number of stars, or size(stars)
     REAL, DIMENSION(2) :: x                   !  Physical x-range of this branch domain
     REAL, DIMENSION(2) :: y                   !  Physical y-range of this branch domain
     REAL :: xc                                !  Physical x coordinate of center of mass
     REAL :: yc                                !  Physical y coordinate of center of mass
     REAL :: mc                                !  Total mass in this branch
  END TYPE tree_branch
  
  !! Global variables for quad_tree
  TYPE(tree_branch), DIMENSION(:), ALLOCATABLE :: tree   !  The actual tree for the BH algorithm
  INTEGER :: branch_counter                              !  Counter for assigning ID's
  INTEGER, PARAMETER :: root = 1                         !  Index of the starting branch
END MODULE quad_tree


MODULE interfaces
  !! Simple interface for the build_tree function
  INTERFACE build_tree
     SUBROUTINE build_tree(star)
       REAL, DIMENSION(:,:), INTENT(IN) :: star 
     END SUBROUTINE build_tree
  END INTERFACE
END MODULE interfaces

!!! ---------- END MODULES ------------ !!!


!!! ---------- PROGRAM ------------ !!!
PROGRAM bhgalaxy
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

END PROGRAM bhgalaxy

!!! ---------- END PROGRAM ------------ !!!


!!! ---------- SUBROUTINES/FUNCTIONS ------------ !!!

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
  USE interfaces
  USE quad_tree, ONLY: tree,branch_counter,root
  IMPLICIT NONE
  INTEGER :: n                               !  Number of stars
  INTEGER :: i                               !  Working indices for loops
  REAL, DIMENSION(:), ALLOCATABLE :: r1,r2   !  Vector of unit-vectors pointing to stars
  REAL, DIMENSION(:), ALLOCATABLE :: fx,fy   !  Vector of force vectors for each star
  REAL, DIMENSION(2) :: f                    !  Temp force vector for tree_force subroutine
  REAL :: radE,x0,y0
  INTEGER :: nrad = 0
  
  !! Get number of stars and allocate the working arrays
  n = size(star,1)
  ALLOCATE(r1(n),r2(n))
  ALLOCATE(fx(n),fy(n))
 
  !! Initialize and set values
  CALL build_tree(star)
  CALL compute_mass(root)
  
  !! Main loop over the N-stars
  x0 = 0.0
  y0 = 0.0
  DO i=1,n
     x0 = x0 + star(i,pX)
     y0 = y0 + star(i,pY)
  END DO
  x0 = x0/n
  y0 = y0/n

  nrad = 0
  DO i=1,n
     CALL tree_force(f,i,root)   ! Traverse the branches for each mass
     fx(i) = f(1) !- star(i,vX)*ABS(star(i,vX))*1.0D1 ! Add viscosity/drag    
     fy(i) = f(2) !- star(i,vY)*ABS(star(i,vX))*1.0D1 ! Add viscosity/drag
     !radE = sqrt( (star(i,pX)-x0)**2 + (star(i,pY)-y0)**2 )
     !if (radE <= .1) then
     !   nrad = nrad + 1
     !end if
  END DO

  ! Explosion
  !if (nrad >= int(.95*float(n) ) ) then
  !   fx = -fx*10.0
  !   fy = -fy*10.0
  !end if
  
  !! SEMI-IMPLICIT EULER TEMPORAL INTEGRATION- use BLAS for small speed-up
  !! Integrate the Velocity in time: V^{n+1} = V^{n} + dt*F^{n}
  CALL SAXPY(n,dt,fx,1,star(:,vX),1)
  CALL SAXPY(n,dt,fy,1,star(:,vY),1)

  !! Integrate the Position in time: P^{n+1} = P^{n} + dt*P^{n+1}
  CALL SAXPY(n,dt,star(:,vX),1,star(:,pX),1)
  CALL SAXPY(n,dt,star(:,vY),1,star(:,pY),1)

  !! Free memory
  CALL kill_tree
  DEALLOCATE(r1,r2,fx,fy)

END SUBROUTINE step


!! Routine to build the quad_tree structure for every time step
!! by initializing the root and then branching recursively
SUBROUTINE build_tree(star)
  USE quad_tree
  USE estrella, ONLY: pX,pY
  IMPLICIT NONE
  REAL, DIMENSION(:,:), INTENT(IN) :: star 
  INTEGER :: n

  n = size(star,1)
  
  !! Allocate and build the root node of the quad_tree
  ALLOCATE(tree(20*n))
  tree(root)%total_stars = n
  CALL init_tree(tree(root))

  !! Build the rest of the quad_tree by recursively branching
  CALL branch(root)
  
END SUBROUTINE build_tree


!! Branch routine that calls itself recursivly until
!! there is only 1 star in that branch. Make children on 
!! the fly and book-keeps the quad-tree
RECURSIVE SUBROUTINE branch(p)
  USE quad_tree, ONLY: tree,branch_counter
  IMPLICIT NONE
  INTEGER :: p,i
  INTEGER, DIMENSION(4) :: c
  LOGICAL :: has_stars

  !! Set ID's (book-keeping) to new children.  Get ID from global branch_counter
  !! and then set the inherited attributes for each child and branch them. Branching
  !! will stop once that node has no stars
  IF (has_stars(tree(p))) THEN
     c(1) = branch_counter + 1
     c(2) = c(1) + 1
     c(3) = c(2) + 1
     c(4) = c(3) + 1
     branch_counter = c(4)
     tree(c)%id = c
     CALL make_children( tree(p),tree(c(1)),tree(c(2)),tree(c(3)),tree(c(4)) )
     DO i=1,4
        CALL branch(c(i))
     END DO
  END IF
  
END SUBROUTINE branch


!! Make 4 children of the parent argument for the quad-tree.  Most attributes
!! are deterministic based on branch depth (level). All attributes set here
!! including list of stars.  Keep list saves search time for children.
SUBROUTINE make_children(parent,child1,child2,child3,child4)
  USE quad_tree, ONLY: tree_branch
  IMPLICIT NONE
  TYPE(tree_branch), INTENT(INOUT) :: parent
  TYPE(tree_branch), INTENT(INOUT) :: child1,child2,child3,child4
  TYPE(tree_branch), DIMENSION(4)  :: children
  REAL :: dx
  INTEGER :: i
  
  !! Some tmp array for easy of use
  children(1) = child1
  children(2) = child2
  children(3) = child3
  children(4) = child4

  !! Assign pedagree
  parent%children = children%id
  children%parent = parent%id
  
  !!  Make children 1 level higher then parents
  children%level = parent%level + 1

  !!  Get the new xloc/yloc based on level and parents xloc/yloc
  children(1)%xloc = parent%xloc * 2 - 1
  children(2)%xloc = parent%xloc * 2 
  children(3)%xloc = parent%xloc * 2 - 1
  children(4)%xloc = parent%xloc * 2 

  children(1)%yloc = parent%yloc * 2 - 1
  children(3)%yloc = parent%yloc * 2 
  children(2)%yloc = parent%yloc * 2 - 1
  children(4)%yloc = parent%yloc * 2
  
  !! Get the x and y bounds on the unit square
  dx = 1.0 / 2.0**( parent%level )
    
  !! Convert index to physical domain range
  children%x(1) = REAL(children%xloc-1)*dx
  children%x(2) = children%x(1) + dx
  children%y(1) = REAL(children%yloc-1)*dx
  children%y(2) = children%y(1) + dx
  
  !! Loop over children and count and keep track of their stars
  DO i=1,4
     CALL count_stars( children(i), parent)
  END DO
  
  !! Re-assign back to subroutine argument variables
  child1 = children(1)
  child2 = children(2)
  child3 = children(3)
  child4 = children(4)

END SUBROUTINE make_children
   

!! Simple logical check to see if a particular branch has any stars in it
LOGICAL FUNCTION has_stars(branch)
  USE quad_tree, ONLY: tree_branch
  USE estrella, ONLY: star
  IMPLICIT NONE
  TYPE(tree_branch), INTENT(IN) :: branch
  
  IF (branch%total_stars > 1) THEN
     has_stars = .TRUE.
  ELSE
     has_stars = .FALSE.
  END IF

END FUNCTION has_stars

!! Will count the stars contained inside a particular branch and catalog
!! the id's of the star so its children have a smaller search space
SUBROUTINE count_stars(branch,parent)
  USE quad_tree, ONLY: tree_branch !,tree
  USE estrella, ONLY: star,pX,pY
  IMPLICIT NONE
  TYPE(tree_branch), INTENT(IN) :: parent
  TYPE(tree_branch), INTENT(INOUT) :: branch
  INTEGER, DIMENSION(:), ALLOCATABLE :: p_star_list,tmp
  INTEGER :: i,p_id,p_stars,s_id,count
    
  !! Find the stars residing in branch's parent that belong to you
  p_id = parent%id
  p_stars = parent%total_stars
  ALLOCATE( p_star_list(p_stars))
  ALLOCATE( tmp(p_stars))
  p_star_list = parent%stars

  !! Loop through the parents stars and find that ones that are in this child's range
  count = 1
  DO i = 1,p_stars
     s_id = p_star_list(i)
     ! Check if star(s_id) is in limits of this branch
     IF( star(s_id,pX) > branch%x(1) .AND. star(s_id,pX) < branch%x(2)) THEN
        IF( star(s_id,pY) > branch%y(1) .AND. star(s_id,pY) < branch%y(2)) THEN
           tmp(count) = s_id       ! Add to the list
           count = count + 1       ! Increment the counter
        END IF
     END IF
  END DO
  
  !! Set the total stars found in child and make the star array which 
  !! keeps track of the stars in that branch
  branch%total_stars = count - 1
  IF (branch%total_stars > 0) THEN
     ALLOCATE( branch%stars(count-1))
     branch%stars = tmp(1:count-1)
  END IF

END SUBROUTINE count_stars


!! Initialize the root branch to contain all the stars and have the default
!! attributes for the largest, "lowest" level node (contains all others)
SUBROUTINE init_tree(branch)
  USE quad_tree, ONLY: tree_branch,branch_counter
  USE estrella, ONLY: star,pX,pY
  IMPLICIT NONE
  TYPE(tree_branch), INTENT(INOUT) :: branch
  INTEGER :: n,i

  n = size(star,1)
  
  !! Set the root attributes manually 
  branch_counter = 1                  ! Restart counter
  branch%id = branch_counter          ! Give starting ID
  branch%level = 1                    ! Start at level 1
  branch%xloc = 1                     ! Index in x
  branch%yloc = 1                     ! Index in y
  ALLOCATE( branch%stars(n))          ! Allocate ALL stars in branch
  branch%stars = (/(REAL(i),i=1,n)/)  ! Set stars
  branch%total_stars = n              ! Contains all stars
  
END SUBROUTINE init_tree


!! Will recursively compute the center of mass for each branch based on
!! the center of mass of the branches children
RECURSIVE SUBROUTINE compute_mass( id )
  USE quad_tree, ONLY: tree
  USE estrella, ONLY: star,pX,pY
  IMPLICIT NONE
  REAL, DIMENSION(4) :: mass
  REAL, DIMENSION(4) :: Xcm,Ycm
  REAL :: mT,xT,yT
  INTEGER :: id,i
  INTEGER :: c_id
  
  !! Determine if branch has stars/children and proceed accordingly
  IF ( tree(id)%total_stars == 0 ) THEN         ! No stars in this branch... return mass = 0
     tree(id)%mc = 0.0
     tree(id)%xc = 0.0
     tree(id)%yc = 0.0
  ELSE IF ( tree(id)%total_stars == 1) THEN     ! Only one star so the attributes are identical to star's
     tree(id)%mc = 1.0
     tree(id)%xc = star( tree(id)%stars(1), pX )
     tree(id)%yc = star( tree(id)%stars(1), pY )
  ELSE                                          ! Has multiple stars and therefore, children
                                                ! compute the centroid based on children
     !! Loop over children add call compute_mass recursively
     DO i=1,4                                
        c_id = tree(id)%children(i) 
        CALL compute_mass( c_id )
        mass(i) = tree(c_id)%mc
        Xcm(i) = tree(c_id)%xc
        Ycm(i) = tree(c_id)%yc
     END DO
     
     !! Average and assign the COM to each branch
     mT = sum(mass)
     xT = sum( mass*Xcm )/mT
     yT = sum( mass*Ycm )/mT
     tree(id)%mc = mT
     tree(id)%xc = xT
     tree(id)%yc = yT

  END IF

END SUBROUTINE compute_mass


!! Will compute the total force felt by star s_id by recursively summing the forces
RECURSIVE SUBROUTINE tree_force( f, s_id, t_id)
  USE estrella, ONLY: star,pX,pY,G,beta
  USE quad_tree, ONLY: tree
  IMPLICIT NONE 
  INTEGER :: s_id
  INTEGER :: t_id
  REAL, DIMENSION(2) :: f,ftmp
  REAL :: r1,r2,rad,D,M
  INTEGER :: i

  !! Set to zero force's
  f = 0.0
  ftmp = 0.0

  !! Main conditional statement to determine how to compute the force
  IF ( tree(t_id)%total_stars == 1 ) THEN
     
     !! Only 1 star so compute force in the standard fashion
     IF (s_id .NE. tree(t_id)%stars(1) ) THEN
        r1 = star(s_id,pX) - tree(t_id)%xc
        r2 = star(s_id,pY) - tree(t_id)%yc
        rad = sqrt( r1**2 + r2**2 )
        M = tree(t_id)%mc
        rad = rad**2
        IF (rad .NE. 0) THEN
           f(1) = -G * r1 / rad * M
           f(2) = -G * r2 / rad * M
        END IF
     END IF

  ELSEIF ( tree(t_id)%total_stars == 0 ) THEN
     
     !! No stars in the branch so NO force contribution
     f = 0.0
  ELSE
  
     !! Multiple stars, thus children so determine whether we need the children's
     !! COM or if we can use this branche's.  Based on D/rad parameter (beta in this code)
     r1 = star(s_id,pX) - tree(t_id)%xc
     r2 = star(s_id,pY) - tree(t_id)%yc
     rad = sqrt( r1**2 + r2**2 )
     D = 1.0 / 2.0**( tree(t_id)%level - 1)
     
     IF ( D/rad < beta ) THEN
        !! Use this branch for force calculation
        M = tree(t_id)%mc
        rad = rad**2
        IF (rad .NE. 0.0) THEN
           f(1) = -G * r1 / rad * M
           f(2) = -G * r2 / rad * M
        END IF
     ELSE
        !! Use children for force calculation...call tree_force recursively
        DO i=1,4
           CALL tree_force( f, s_id, tree(t_id)%children(i))
           ftmp = ftmp + f
        END DO
        f = ftmp
     END IF
  END IF

END SUBROUTINE tree_force


!! Deallocate the tree to clear out contents and reset for next tree_build
SUBROUTINE kill_tree
  USE quad_tree, ONLY: tree
  IMPLICIT NONE
  
  !! Free memory
  DEALLOCATE(tree)

END SUBROUTINE kill_tree

!!! ---------- END SUBROUTINES/FUNCTIONS ------------ !!!
