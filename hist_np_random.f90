    PROGRAM NEURON_Tree
    IMPLICIT NONE 
    INTEGER :: i,nl,nd,ng,nn,kk
    INTEGER ::node,nh,np_hist
    INTEGER, allocatable, dimension(:,:) :: adj, link
    INTEGER, allocatable, dimension(:):: B,S,n1,hemind,nlink
    REAL*8:: m,n, m_nd, R
!*******************************************************************************
    np_hist=1000000
    open (15,file='np_nn1.dat', action='write')  
    open (16,file='npn1.dat', action='write')   
    do kk=1,np_hist
       ng=5    
       nd=4    
       nl= nd**ng 
       nn=(nd**(ng+1)-1)/(nd-1)
       ALLOCATE(B(nn),S(nn),n1(nn))   
       CALL size_rand_adj(ng,nn,nd,B,S,n1,node,nh) 
       ALLOCATE(adj(node,node),link(node,node),nlink(node),hemind(nh)) 
       CALL random_adj(nh,nn,node,B,link,nlink,hemind,adj)  
    R=dble(nh)/dble(node)
    write(15,*) R, R/dble(node)
    write(16,*) nh,node
    deallocate(B,S,n1,adj,hemind,nlink,link)  
  enddo  

!*************************************************************
    END PROGRAM NEURON_Tree
    
    
    subroutine size_rand_adj(ng,nn,nd,B,S,n1,node,nh)
    integer,intent(in) ::ng,nn,nd
    integer            :: min_nd
    integer,intent(out)::node,nh
    integer,intent(out):: B(nn),S(nn),n1(nn)
    real*8:: ran(nn)
    
     S=0.d0
     n1=0.d0
     B=0
     ran=0.d0
     min_nd=1 ! lower level of random numbers, min of dauthers 
     77 continue 
     call random_seed()
     call random_number(ran)
     B=min_nd + FLOOR((nd+1-min_nd)*ran) ! B is the random array of # of daughters,
                                             ! we use it to determine the dimention of the adjencay matrix 
     if (B(1)==1) then 
         goto 77
      endif   

     S(1)=B(1)+1   !nods in ng=1, +1 is for the cental node
     n1(1)=sum(S)  ! dimension of adj matrix for ng=1
     S(2)=sum(B(2:n1(1))) ! nodes in ng=2
     n1(2)=sum(S)         ! dimension fo adj for ng=2
     
     do i=1,ng-2
     S(i+2)=sum( B((n1(i)+i): (n1(i+1)+i) )  )   
     n1(i+2)=sum(S)
     enddo 
      node=maxval(n1)
      nh=S(ng)
    return 
    end 
 
    subroutine random_adj(nh,nn,node,B,link,nlink,hemind,adj)
    implicit none 
    integer:: nh,nn,node,f,i,j,k
    integer:: B(nn),nlink(node),link(node,node),hemind(nh),adj(node,node)
    adj=0
    nlink=0
    link=0
    hemind=0
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
    return 
    end 


