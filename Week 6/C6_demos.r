# Correlation coefficient

x = seq(0,10,0.5)
y = x
plot(x,y)
cov(x,y) # covariance
var(x)   # variance
var(y)
cov(x,y)/(sd(x)*sd(y)) # definition of Pearson correlation coefficient
cor(x,y)               # Pearson correlation coefficient function

y = x*2
plot(x,y)
cov(x,y)
cor(x,y)

y = 5 + 3*x
plot(x,y) 
cor(x,y)

y = 6 - 3*x
plot(x,y)
cor(x,y)

y = x*0
plot(x,y)
cor(x,y) # returns NA and a warning

y = x+rnorm(21)
plot(x,y)
cor(x,y)

y = x+rnorm(21,sd=3)
plot(x,y)
cor(x,y)

y = x+rnorm(21,sd=6)
plot(x,y)
cor(x,y)

y = x+rnorm(21,sd=10)
plot(x,y)
cor(x,y)
cor(x,y, method='spearman')
cor(x,y, method='kendall')

cor.test(x,y)
cor.test(x,y, method='spearman')
cor.test(x,y, method='kendall')

y = tan((x-5)/11*pi)
plot(x,y)
cor(x,y)
cor(x,y, method='spearman') # better for non-linear relations
cor(x,y, method='kendall')
cor.test(x,y)$p.value
cor.test(x,y, method='spearman')$p.value

x[5] = 30  # create an outlier
plot(x,y)
cor.test(x,y)$p.value
cor.test(x,y, method='spearman')$p.value   # much better if outliers present


##########

# Simple linear regression

x = seq(0,10,0.5)
y = x+rnorm(21,sd=1)
plot(x,y)
m = lm(y~x)
m # compare to input coefficients (a=0, b=1)
str(m)
m$coefficients
summary(m)
lines(x,m$fitted.values,col='red')
lines(x,predict(m),col='red',lwd=3)
plot(m) # check plot; have to page through 

# Linear regression with nonlinear functions of x

 # Polynomial linear regression
x = seq(0,10,0.2)
y = 5 - x + x^2 + 2*rnorm(51)
plot(x,y)

m = lm(y~x) # fit a line (not a good fit!)
lines(x,predict(m),col='red',lwd=3)
plot(m) # does four plots individually; press return to page through
par(mfrow=c(2,2))
plot(m) # shows all the plots together
par(mfrow=c(1,1))

m = lm(y~x+I(x^2)) # fit a quadratic
plot(x,y)
lines(x,predict(m),col='red',lwd=3)
plot(m)
summary(m)

 # Sine-wave linear regression
y = 2 + 2*sin(x) + rnorm(51)
plot(x,y)
m = lm(y~I(sin(x)))
lines(x,predict(m),col='red',lwd=3)
summary(m)
 # Plot a nicer curve and extrapolate
xplot = seq(-2,12,0.01)
lines(xplot,predict(m,list(x=xplot)),col='red',lwd=3)


###########


# Linear regression with transformed non-linear data

y = 2.2*exp(-0.27*x)+0.05*rnorm(51)
plot(x,y)
logy = log(y)   # transform the data
plot(x,logy)
tm = lm(logy~x) # fit the transformed model
lines(x,predict(tm),col='red',lwd=3) # plot in the transformed system
plot(x,y)  
ymodel = exp(predict(tm)) # back-transform the model values to original system
lines(x,ymodel,col='red',lwd=3) # plot in our original x vs. y system
par(mfrow=c(2,2))
plot(tm)
par(mfrow=c(1,1))

 # Weighted linear regression as a way of dealing with induced heteroscadicity

plot(x,y)
lines(x,ymodel,col='red',lwd=3)

sd(y-ymodel) # get a rough estimate of the uncertainty
yunc=rep(0.05,51) # create a vector of uncertainties
ylogunc = log(y+yunc) - log(y) # transform the uncertainties
wm = lm(logy~x, weights=1/ylogunc^2)
ywmodel = exp(predict(wm)) # y values for the weighted model
lines(x,ywmodel,col='blue',lwd=3)
plot(wm)


 # Nonlinear regresssion

#y = 2.2*exp(-0.27*x)+0.05*rnorm(51) # same as before
plot(x,y)
nlm = nls(y~c*exp(-d*x)) # fails; no starting guess
nlm = nls(y~c*exp(-d*x),start=list(c=0,d=0)) # fails; bad starting guess
nlm = nls(y~c*exp(-d*x),start=list(c=0,d=1)) # fails; bad starting guess
nlm = nls(y~c*exp(-d*x),start=list(c=1,d=1)) # finally succeeds
summary(nlm)
lines(x, predict(nlm),col='red',lwd=3)

source('ssecontour.r') # download from Canvas
f = function(x,c,d) {c*exp(-d*x)}
ssecontour(x,y,f,arange=c(-1,3),brange=c(-1,3),aname='c',bname='d')


##########


# Nonparametric Regression

x = seq(0,10,0.05)
y = cos(0.1*x^2+30/(x^2+4)^0.5)-0.01*x^2 + 0.2*rnorm(length(x)) + 2
plot(x,y) # a really complex curve...

  # LOESS (local regression)
mloess = loess(y~x)
lines(x,predict(mloess), col='green') # overly smoothed
mloess = loess(y~x,span=0.1)
lines(x,predict(mloess), col='orange') # not smoothed enough
mloess = loess(y~x,span=0.2)
lines(x,predict(mloess), col='red', lwd=3)    # about right 

  # Smoothing splines
ss = smooth.spline(x,y)
#lines(x, predict(ss), col='blue') doesn't work with smooth.spline
lines(x, predict(ss)$y, col='blue', lwd=3) # have to subscript predict in this case
lines(ss, col='blue', lwd=3)  # also works

  # Generalized additive model
library(mgcv)
gm = gam(y~s(x))
lines(x,predict(gm),col='pink',lwd=3)
summary(gm)


# note - recurrent warnings associated with graphics commands can be removed with e.g. suppressWarnings(lines)
