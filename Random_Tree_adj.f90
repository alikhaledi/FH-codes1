! Thr purpose of this program is to generate teh caley tree of neuros as a matrix 
! the matrix will be all 0 and 1's
! I came up with the algorithm. For the verification I am going to look at some of the matrix we can calcualte by hand


    PROGRAM NEURON_Tree
    IMPLICIT NONE 
    INTEGER :: i, j, f,k, nl, nnl, nd, g, ng, nn,max_ng,n_count,np_count
    INTEGER ::node, nh
    INTEGER, allocatable, dimension(:,:) :: A, adj, link
    INTEGER, allocatable, dimension(:):: B, S, n1, hemind,nlink
    REAL*8 , ALLOCATABLE, DIMENSION(:):: ran
    REAL*8:: m,n, m_nd
!*******************************************************************************
         max_ng=1
         ng=5     ! Number of generations
         nd=3     ! Max number of duoughters in each generation 
         nl= nd**ng ! Max number of leaves at in thhe last branches 
         nn=(nd**(ng+1)-1)/(nd-1) ! Max numebr of all nodes 
!*******************************************************************************
     ALLOCATE(B(nn),ran(nn),S(nn), n1(nn))    ! B= rnadom dauthers rang:1-nd, S=summations of the nodes in each generation
                                              !, n1= the whole population
     S=0.d0
     n1=0.d0
     call random_seed
     call random_number(ran)
     B=int(ran*nd)+1 ! B is the random array of # of daughters,
                     ! we use it to determine the dimention of the adjencay matrix 
     
     S(1)=B(1)+1   !nods in ng=1, +1 is for the cental node
     n1(1)=sum(S)  ! dimension of adj matrix for ng=1
     S(2)=sum(B(2:n1(1))) ! nodes in ng=2
     n1(2)=sum(S)         ! dimension fo adj for ng=2
     
     do i=1,max_ng
     S(i+2)=sum( B((n1(i)+i): (n1(i+1)+i) )  )   
     n1(i+2)=sum(S)
     enddo
      print*,'population of nodes',n1(1:max_ng+2) 
      node=maxval(n1)
      print*,'dimenion of adj matrix',node 
      nh=S(max_ng+2)
      print*,'number of peripegrals', nh
      ALLOCATE(adj(node,node),link(node,node),nlink(node), hemind(nh)) ! the adj matrix has the dimeniosn of the total number of the nodes  
      adj=0 

!*******************************************************************************
      f=0
      do i=1,node
       if (1+f+B(i) .GT. node) goto 33
         do j=2+f,1+f+B(i)
           adj(i,j)=1
         enddo
        f=f+B(i)       
       enddo
       33 continue
        do i=1,node
         do j=i+1,node
           adj(j,i)=adj(i,j)
         enddo
       enddo
       
     do i=1,nh
         hemind(i)=node-i+1
       enddo
       link=0
       do i=1,node
          k=0
          do j=1,node
             if(adj(i,j).eq.1) then
                k=k+1
                link(i,k)=j
             endif
          enddo
          nlink(i)=k
       enddo
       
 DO i=1, node
    WRITE(*,'(40i2)') adj(i,1:node)
  END DO
    
!*************************************************************
    END PROGRAM NEURON_Tree


