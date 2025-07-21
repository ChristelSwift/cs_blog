library(mnormt)

k=3
r=.5
G=function(x) qnorm(pt(x,df=k))
dg=function(x) dt(x,df=k)/dnorm(qnorm(pt(x,df=k)))
Ginv=function(x) qt(pnorm(x),df=k)
S=matrix(c(1,r,r,1),2,2)
f=function(x,y) dmt(cbind(Ginv(x),Ginv(y)),S=S,df=k)/(dg(x)*dg(y))
vx=seq(-3,3,length=201)
vy=seq(-3,3,length=201)
z=outer(vx,vy,f)
set.seed(1)
Z=rmt(1500,S=S,df=k)
X=G(Z)
xhist <- hist(X[,1], plot=FALSE)
yhist <- hist(X[,2], plot=FALSE)
top <- max(c(xhist$density, yhist$density,dnorm(0)))
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(3,3,1,1))
image(vx,vy,z,col=rev(heat.colors(101)))
contour(vx,vy,z,col="blue",add=TRUE)
points(X,cex=.2)
par(mar=c(0,3,1,1))
barplot(xhist$density, axes=FALSE, ylim=c(0, top), space=0, col="light blue")
lines((density(X[,1])$x-xhist$breaks[1])/diff(xhist$breaks)[1],
      dnorm(density(X[,1])$x),col="red")
par(mar=c(3,0,1,1))
barplot(yhist$density, axes=FALSE, xlim=c(0, top), space=0, 
        horiz=TRUE,col="light blue")
lines(dnorm(density(X[,2])$x),(density(X[,2])$x-yhist$breaks[1])/
        diff(yhist$breaks)[1],col="red")