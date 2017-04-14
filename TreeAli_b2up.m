clc
clear all
nd=2;     %number of daughers in the first generation 

nh=factorial(nd);   % numebr of peripheral nodes 
nn=nh+1;             % number of the total nodes 

for j=1:nd-1
    nn=nn+ (factorial(nd)/factorial(j));
end 
nh
nn
nh/nn
B=0; 
B(1)=nd; 
a=2; 
c=(factorial(nd)/(factorial(nd-1)))+1; 
B(a:c)=nd-1;


f=0;
for i=2: nd-1;
   a=c+1;
    c=c+(factorial(nd)/(factorial(nd-i))); 
    B(a:c)=nd-i;
end 
B(c+1:nn)=0;
adj=zeros(nn,nn);
for i=1:nn
     if (1+B(i)+f) > nn
       break
    end 
       for j=2+f:1+f+B(i)
           adj(i,j)=1;
       end  
      f=f+B(i);
end 

 for i=1:nn    
     for j=1:nn
         adj(j,i)=adj(i,j);
     end 
 end
 
 %radial_plot(adj,1)
