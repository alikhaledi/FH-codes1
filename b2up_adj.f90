program adj_b2up
implicit none 
integer:: nd, i,j,k,F,nh,nn, a, c, d
integer, allocatable, dimension(:) :: hemind, nlink,B
integer, allocatable, dimension (:,:) :: adj,link

nd=3 
nh=F(nd)
nn=nh+1
do j=1,nd-1
nn=F(nd)/(F(j))+nn
end do 
print*,nn
 allocate (hemind(nh),nlink(nn),link(nn,nn),adj(nn,nn),B(nn))
 call branch_order(nd,nn,B)
 call b2up_adj(nh,nn,B,link,nlink,hemind,adj)

 do i=1,nn
 print*,adj(i,1:nn)
 end do
end 



INTEGER FUNCTION F(n)
 IMPLICIT NONE 
 INTEGER, INTENT(IN) :: n
 INTEGER :: i, Ans 
  Ans = 1
  DO i = 1, n
  Ans = Ans * i
  END DO
  F = Ans 
END FUNCTION F


subroutine branch_order(nd,nn,B)
  implicit none 
  integer:: nd, nn,a,c,F,i
  integer:: B(nn)
   B=0
   B(1)=nd
   a=2
   c=F(nd)/F(nd-1)+1
   B(a:c)=nd-1
   do i=2,nd-1
      a =c+1
      c=c+F(nd)/F(nd-i)
      B(a:c)=nd-i
   end do
end 

 
  subroutine b2up_adj(nh,nn,B,link,nlink,hemind,adj)
    implicit none 
    integer:: nh,nn,f,i,j,k
    integer:: B(nn),nlink(nn),link(nn,nn),hemind(nh),adj(nn,nn)
    adj=0
    nlink=0
    link=0
    hemind=0
      f=0
      do i=1,nn
       if (1+f+B(i) .GT. nn) goto 33
         do j=2+f,1+f+B(i)
           adj(i,j)=1
         enddo
        f=f+B(i)       
       enddo
       33 continue
        do i=1,nn
         do j=i+1,nn
           adj(j,i)=adj(i,j)
         enddo
       enddo
       
     do i=1,nh
         hemind(i)=nn-i+1
       enddo
       link=0
       do i=1,nn
          k=0
          do j=1,nn
             if(adj(i,j).eq.1) then
                k=k+1
                link(i,k)=j
             endif
          enddo
          nlink(i)=k
       enddo
    return 
    end 


