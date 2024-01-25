ssecontour = function(x, y, f, arange, brange, na=100, nb=100, levels=0, aname='a', bname='b') {

	amin = arange[1] 
	amax = arange[2] 
	bmin = arange[1] 
	bmax = arange[2] 
	
  agrid = seq(amin,amax,length=na)
  bgrid = seq(bmin,bmax,length=nb)
  sseij = matrix(nrow=na,ncol=nb)

  for (i in 1:na) {
  	for (j in 1:nb) {
  		yij = f(x,agrid[i],bgrid[j])
  		sseij[i,j] = sum((yij-y)^2)
  	}
  }

  if (length(levels) <= 1) {levels = c(2,5,10,20,30,40,50,75,100,150,250,1000,10000,1e6)}

  contour(agrid,bgrid,sseij,levels=levels,xlab=aname,ylab=bname,labcex=1) # ,cex.lab=1.5,cex.axis=1.5

}
