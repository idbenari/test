# QSD Calc
library(rgl)
N = 400
probs=matrix(0,nrow=N+1,ncol=N+1,byrow=TRUE)
probs[N,1]=1 # at N+1,0
for (x in (N-2):0)
for (c in 0:(N-x-1))
  { 
    if (x+c<(N-1)) probs[x+1,c+1]=probs[x+1,c+1] + probs[x+2,c+1]*(x+1)/N*(N-(x+1)-c)/(N-x-c-1)
    if (c>0) probs[x+1,c+1]=probs[x+1,c+1]+ probs[x+1,c]*(N-x)/N*(N-x-c)/(N-x-c)
  }
    
gd=which(probs>0,arr.ind = TRUE)
xx=gd[,1]
cc=gd[,2]
zz=probs[gd]
plot3d(xx,cc,zz,pch='.')
rglwidget()