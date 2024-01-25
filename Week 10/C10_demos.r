
# ---- Survival analysis ---

library(survival)
?lung
head(lung)
summary(lung)
# Looks like "status" is a censorship variable.  1=censored, 2=dead
# Sex is stored as a numeric variable (1=M, 2=F), but should be categorical
# There are some NA's but not too many

lung2 = na.omit(lung) # remove the NA-affected rows.

lung2$sex = as.factor(lung2$sex) # Convert numeric sex code to a factor
dead = (lung2$status == 2) # Normal way to indicate censorship in R (1=uncensored, 0=uncensored),
                           # although it understands the 1/2 convention here as well.

lungsurv = Surv(lung2$time, dead)
lungsurv[1:100] # the "+" indicates censored: we have only a limiting value

lungkm = survfit(lungsurv~1) # Kaplan-Meier curve; interpret
plot(lungkm)
plot(lungkm, xlab='Time', ylab='K-M Survivorship')
lungkm # a few handy values (note: summary() is not very useful here)
str(lungkm)
lungkm$time[100]
lungkm$surv[100] # survivorship is 41.5% at 363 days (about 1 year)
approx(lungkm$time,lungkm$surv,xout=365)$y # even better: interpolate to 1 year "exactly"

lungkm.sex = survfit(lungsurv~sex, data=lung2)
plot(lungkm.sex, xlab='Time', ylab='K-M Survivorship', col=c(4,2))

# Survival regression (parametric survival analysis)

lungmodel.sex = survreg(lungsurv~sex, data=lung2) # default model is Weibull
summary(lungmodel.sex)
lungmodel.sex$coefficients
scale.M = exp(lungmodel.sex$coefficients[1])
scale.F = exp(lungmodel.sex$coefficients[1]+lungmodel.sex$coefficients[2])
shape = 1/lungmodel.sex$scale # yes, this is weird
shape
t = 1:1000
lines(t,1-pweibull(t,shape,scale.M)) # Best-fit theoretical survivorship curve
lines(t,1-pweibull(t,shape,scale.F))
  # important reminder: this is not a regression model in 'time'.  Regression variable is 'sex' (1/2)

lungmodel.ecog = survreg(lungsurv~ph.ecog, data=lung2)
summary(lungmodel.ecog)

lungmodel.all = survreg(lungsurv~sex+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss, data=lung2)
summary(lungmodel.all)
lungmodel.simp = step(lungmodel.all)
summary(lungmodel.simp)
predict(lungmodel.simp, list(sex='1',ph.ecog=1,ph.karno=60,pat.karno=80,wt.loss=10))

  # non-Weibull models

lungmodel.sex = survreg(lungsurv~sex, data=lung2)
lungmodel.sex.exp = survreg(lungsurv~sex, data=lung2, dist='exponential')
anova(lungmodel.sex, lungmodel.sex.exp, test='Chi') # exp is definitely worse, stick to Weibull
AIC(lungmodel.sex, lungmodel.sex.exp)

  # see C10_demos_coxph.r for some examples using a Cox proportional-hazards model

######


# --- Bayesian likelihood & posteriors ---

# 1 parameter, 1 measurement

x = 10.  # from exponential distribution of unknown lambda

#H1: lambda=0.1
dexp(x,rate=0.1)

#H2: lambda=0.2
dexp(x,rate=0.2)

#H3: lambda=0.4
dexp(x,rate=0.4)

rate = seq(0.001,1,0.001)
Lexp = dexp(x,rate)
plot(rate, Lexp, typ='l',ylab='Likelihood given x=10')
abline(v=1/x, col='lightblue', lty=2) # max likelihood at 1/x, as expected

posterior = Lexp / sum(Lexp*0.001)   # 0.001 is the grid spacing / bin size
plot(rate, posterior, typ='l')

# numerically integrate posterior to measure probability over a finite interval
sum(posterior[rate < 0.1]*0.001) # 26% chance that rate is <0.1 under our prior
sum(posterior[rate > 0.6]*0.001) # 1.7% chance that rate is <0.1 under our prior

# for a credible interval, find a region over which posterior integrates to 0.95
sum(posterior[rate > 0.05 & rate < 0.5]*0.001)  # an 86% credible interval
sum(posterior[rate > 0.03 & rate < 0.6]*0.001)  # a 94% credible interval
sum(posterior[rate > 0.028 & rate < 0.6]*0.001)  # a 95% credible interval

sum(posterior[posterior > 0.5]*0.001)  # a better 94% credible interval
sum(posterior[posterior > 0.4]*0.001)  # ... 95.1% credible interval
sum(posterior[posterior > 0.41]*0.001)  # ... 95% credible interval
abline(h=0.41, col='blue') # "water level method"
range(rate[posterior > 0.41])
sum(posterior[rate > 0.005 & rate < 0.475]*0.001)  # confirm
abline(v=0.005, col='blue')
abline(v=0.475, col='blue')

# 1 parameter, 3 measurements

x = c(10, 6, 11)
Lexp = dexp(x[1],rate)*dexp(x[2],rate)*dexp(x[3],rate)
plot(rate, Lexp, typ='l',ylab='Likelihood given x=c(10,6,11)')
abline(v=1/mean(x), col='lightblue', lty=2)
posterior3 = Lexp / sum(Lexp*0.001)   # 0.001 is the grid spacing
plot(rate, posterior3, typ='l')
lines(rate, posterior, col='lightgray') # compare to posterior for x=10 alone

sum(posterior3[posterior3 > 0.5]*0.001)  # a 97% credible interval
sum(posterior3[posterior3 > 0.8]*0.001)  # a 95% credible interval
range(rate[posterior3 > 0.8])
abline(v=0.027, col='blue')
abline(v=0.294, col='blue')
 # remember, this is still an exponential...! the posterior is a function of the parameters.
hist(x, breaks=seq(-0.5,25.5), freq=FALSE) # here's the data vs. ...
lines(0:25, dexp(0:25,rate=1/mean(x)))     # ... max-L PDF for comparison

 # note: the choice of prior can make a big difference with small numbers of
 # measurements, as in this case.  Most statisticians do *not* advise a 
 # flat prior for distributions like this.


