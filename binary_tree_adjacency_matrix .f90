! Thr purpose of this program is to generate the adjacency matrix for a requalr tree 
! the matrix will be all 0 and 1's
! This is dveloped my Ali Khaledi Nasab(ali.khaledi1989@gmail.com), Deaprtment of Physics, Ohio univeristy, Athens, OH 45701
! This  code is free for all non-comercial uses. 
! 

    PROGRAM NEURON_Tree
    IMPLICIT NONE 
    INTEGER :: m,n, i, j, f, nl, nnl, nd, g
    INTEGER, allocatable, dimension(:,:) :: A
!*******************************************************************************
         n=2    ! number of generations
         m=3   ! number of duoughters in each generation 
        nl= m**n! number of leaves at in thhe last branches 
!*******************************************************************************

     CALL NN(n,m,nd)    ! nd is the dimesnion of the matrix 
     ALLOCATE(A(nd,nd)) 
     WRITE(*,*) 'number of leaves is:', nl,'number of nodes is:',nd
!*******************************************************************************
      A=0 ! The whole matrix is zero unless those we find as non-zero
      f=0 ! this is just a parameter to find the position of 1's in the matrix
     DO i=1,nd
       Do j=2+f,1+f+m
     if (f .GT. nd)go to 33
     A(i,j)=1
     END DO
     f=f+m
       END DO

33     DO i=1,nd
        Do j=i+1,nd
           A(j,i) = A(i,j)
        ENDDO
     ENDDO
                
 DO i=1, nd
    WRITE(*,'(40i2)') A(i,1:nd)
  END DO
!*************************************************************
    END PROGRAM NEURON_Tree

!**************************************************************
! This subroutine geenrate the dimension of the matrix given the values of n and m
!   
        SUBROUTINE NN(n,m,f)
   	INTEGER, INTENT(IN):: n,m
   	INTEGER, INTENT(OUT)::f
        f=0
        Do i=0, n
           f= f+ m**i
        END DO
 	 
 	RETURN
 	END
