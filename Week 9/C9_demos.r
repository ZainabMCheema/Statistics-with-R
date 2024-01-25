# --- Simulating binomial data (p=1/6, N=3) ---

# This simulates the situation discussed in lecture where we roll
# three dice and calculate the probabilities of rolling zero sixes,
# one six, two sixes, or three sixes.
# This corresponds to a binomial simulation with p=1/6 and N=3.

threedicerolls = function() {
   # simulates three dice rolls.  Counts how many sixes come up.
   rolls = sample(1:6, 3, replace=TRUE) # use prob=... to specify individual probabilities
   number.of.sixes = sum(rolls==6)
   return(number.of.sixes)
}

many.threedicerolls = function(nsimulations) {
   # simulates many repeated three-dice rolls.
   # the number of sixes that come up each time is recorded and stored in a vector.
   number.of.sixes = numeric(nsimulations)  # create empty vector
   for (i in 1:nsimulations) number.of.sixes[i] = threedicerolls()
   return(number.of.sixes)
}

# Test the simulator

nsixes = many.threedicerolls(100000) # equivalent to rbinom(100000,3,1/6)
hist(nsixes, col='beige', breaks=seq(-0.5,3.5,1), freq=FALSE, xlab='Number of sixes')
xp = c(0,1,2,3)
lines(xp, dbinom(xp,size=3,p=1/6), typ='b', lwd=3, cex=1.5)
table(nsixes)/sum(table(nsixes))  # simulation-measured probabilities
dbinom(xp,size=3,p=1/6)           # theoretical probabilities


########


# --- Poisson data ---

c = rpois(1000, lambda=1)
#hist(c)  # bad break positions
hist(c,breaks=seq(-0.5,6.5),col='beige',freq=FALSE)
table(c)
mean(c)
sd(c)
lines(0:10, dpois(0:10, 1), type='b', lwd=2)

c = rpois(1000, 10)
hist(c,breaks=seq(-0.5,max(c)+0.5,1),col='beige',freq=FALSE)
mean(c)
sd(c)
sqrt(10)
lines(0:20, dpois(0:20, 10), type='b')
lines(0:20, dnorm(0:20, mean=10, sd=sqrt(10)), col='red') # normal approx.

# --- Poisson confidence intervals ---

counts = 5 # observed during a single, one-second interval

# Q: what possible lambdas (true long term average) are consistent with observing counts = 5?
# A:
qchisq(0.025, 2*5)/2      # lower interval boundary
qchisq(0.975, 2*(5+1))/2  # upper interval boundary

# or even easier, use the convenience function:
poisson.test(5) # p value not really meaningful: mainly for confidence interval
poisson.test(5)$conf.int[1:2]

5 + 2*sqrt(5)*c(-1,1) # very different from large-N approximation!!!

poisson.test(100)$conf.int[1:2] # n = 100 counts
100 + 2*sqrt(100)*c(-1,1) # here the large-N approximation is OK.

# --- Binomial tests and confidence intervals ---

binom.test(5, 50) # 5 successes out of 50 tries
binom.test(5, 50)$conf.int[1:2]
binom.test(0, 5)$conf.int[1:2] # confidence interval on heads fraction for 5/5 tails
binom.test(0, 5, p=0.4605) # no wins in five rounds of two-card high-card game
#binom.test(0, 5, p=0.4605, alternative="less") 


########

# --- Poisson regression (via transform) ---

# Create some data
x = seq(2,60,1)
lambda = x*1.2
y = rpois(length(x), lambda) # now lambda varies for each simulated measurement
plot(x,y)

# Method 1: naive linear model fit
m1 = lm(y ~ x)
lines(x,predict(m1),col='blue', lwd=3)
plot(m1)

# Method 2: variance-stabilizing transform
yt = sqrt(y) # transform y
xt = sqrt(x) # also transform x (can always do this) to keep the model "similar" to before
plot(xt,yt)
m2 = lm(yt ~ xt) # this is a slightly different model: yt = a + b*xt, or y = (a + b*sqrt(x))^2, or y = a^2 + b^2 * x + a*b*sqrt(x)
plot(x,y)
lines(x,predict(m2)^2,col='red', lwd=3)    # don't forget to back-transform!
plot(m2)

# Method 3: variance-stabilizing transform with nonlinear model
m3 = nls(yt ~ sqrt(a+b*x), start=list(a=0,b=0)) # fit the true, transformed linear model
summary(m3)
plot(x,y)
lines(x,predict(m3)^2,col='green', lwd=3)  # don't forget to back-transform!
# check plots unfortunately not available for nlm

# Method 4: error weights
w = 1/y
w[y==0] = 1
m4 = lm(y ~ x, weight=w)  # use weighted regression
lines(x, predict(m4), col='violet', lwd=3)
plot(m4) # Scale-Location plot is a lot nicer than with m1, but has some high-leverage points

plot(x,y)
lines(x, predict(m1), col='red', lwd=3)
lines(x, predict(m2)^2, col='green', lwd=3)
lines(x, predict(m3)^2, col='blue', lwd=3)
lines(x, predict(m4), col='violet', lwd=3)


# --- Poisson regression (via generalized linear model) ---

plot(x, y)

 # Method 5: Poisson regression with canonical link
gm1x = glm(y~x, family=poisson) # use canonical link function (log for Poisson)
lines(x, predict(gm1x,type='response'), col='orange', lwd=3) # must specify type='response'
  # not a good fit, because the model it's actually fitting is log(y) = a + bx
logx = log(x)
gm1 = glm(y~logx, family=poisson)
lines(x, predict(gm1,type='response'), col='brown', lwd=3)

etahat = predict(gm1)
etahatSE = predict(gm1,se.fit=TRUE)$se.fit  # get SE on the linear predictor 
lines(x, exp(etahat+2*etahatSE), col='brown', lwd=2, lty=2) # inverse-link-transform the linear predictor
lines(x, exp(etahat-2*etahatSE), col='brown', lwd=2, lty=2)
plot(gm1) # check plots work for GLM

# Method 6: Poisson regression with identity link
gm2 = glm(y~x, family=poisson(link=identity)) # use identity link function to mimic a LM
plot(x, y)
lines(x, predict(gm2,type='response') , col='cyan', lwd=3) 
 # note - identity link can produce impossible predictions, so this sometimes crashes!


# --- Logistic regression ---

# Create some fake opinion poll data
source('pollsim.r')
head(poll)
y = ans=='yes' # turn categorical to logical/binary
plot(poll$age, y, pch='.') # not very informative

# show the data more informatively by binning
agegroup = as.factor(10*round(poll$age/10))
t = table(poll$ans, agegroup, poll$gender)
t
n_f_yes = t['yes',,'F'] # number of F saying yes by age group
n_m_yes = t['yes',,'M']   
n_f = n_f_yes+t['no',,'F']
n_m = n_m_yes+t['no',,'M']   
prop_f_yes = n_f_yes/n_f # proportion of F saying yes by age group
prop_m_yes = n_m_yes/n_m
ag = as.numeric(levels(agegroup))
points(ag, prop_f_yes, col='red')
points(ag, prop_m_yes, col='blue')


gpoll1 = glm(y ~ gender, data=poll, family=binomial)
summary(gpoll1)
gpoll2 = glm(y ~ gender+age, data=poll, family=binomial)
summary(gpoll2)
anova(gpoll1, gpoll2, test='Chisq')

aplot = 18:80
predm = predict(gpoll1, list(age=aplot,gender=rep('M',63)), type='response')
predf = predict(gpoll1, list(age=aplot,gender=rep('F',63)), type='response')
lines(aplot, predm, col='blue')
lines(aplot, predf, col='red')


#errorbars = function(x, ymin, ymax, col=0, length=0.25) {
#   arrows(x,ymin,x,ymax,code=3,angle=90,col=col, length=length)
#}

# could add crude error bars this way
#p=prop_f_yes ; q = 1-p ; N = apply(t[,,'F'],2,sum)
#err_f = sqrt(p*q/N)  # Lecture 9, slide 83 - crude, better to use a conf. int.
#p=prop_m_yes ; q = 1-p ; N = apply(t[,,'M'],2,sum)
#err_m = sqrt(p*q/N)
#errorbars(ag,prop_f_yes-err_f,prop_f_yes+err_f, col='red', length=0.15)
#errorbars(ag,prop_m_yes-err_m,prop_m_yes+err_m, col='blue', length=0.15)

# could add proper error bars this way
#for (i in 1:7) {
#  ci_m = binom.test(n_m_yes[i], n_m[i])$conf.int[1:2]
#  errorbars(ag[i],ci_m[1],ci_m[2], col='blue', length=0.15)
#  ci_f = binom.test(n_f_yes[i], n_f[i])$conf.int[1:2]
#  errorbars(ag[i],ci_f[1],ci_f[2], col='red', length=0.15)
#}


# --- Binomial regression ---

# Create some random binomial data
x = seq(0,10,0.1)
p = 0.1+0.05*x
tries = rep(8,length(x))
successes = rbinom(length(x),size=tries,prob=p)
plot(x, successes)
plot(x, successes/tries)

failures = tries-successes
y = cbind(successes, failures) # you can skip this only if tries=1 (logistic data)

gmb = glm(y~x, family=binomial)
summary(gmb)
lines(x, predict(gmb, type='response'), col='red',lwd=3) # plots p (probability)

gmb2 = glm(y~x+I(x^2), family=binomial) # allow for some additional freedom in the model
lines(x, predict(gmb2, type='response'), col='green',lwd=3)

anova(gmb, gmb2, test='Chisq')

