# More on simulations

tsimulation = function(N, ntrials, mu=0, sigma=1) {
	t.trials = numeric(ntrials)
	for (i in 1:ntrials) {
		x = rnorm(N, mean=mu, sd=sigma)
		diff = (mean(x)-mu)
		se = sd(x)/sqrt(N)
		t = diff / se
		t.trials[i] = t
	}
	return(t.trials)
}

simt = tsimulation(5,10000,0,1)
hist(simt)
hist(simt,breaks=seq(floor(min(simt)),ceiling(max(simt)),0.1),xlim=c(-5,5),freq=FALSE)
tplot = seq(-10,10,0.1)
lines(tplot, dt(tplot,4), col='orange', lwd=3)


###########

# Box plots and bar plots with errors

survey = read.csv('survey.csv',as.is=FALSE)
summary(survey)
plot(survey$bev, survey$height) # you can abbreviate column names in dataframes
survey = survey[survey$bev != 'none' & !is.na(survey$height) & !is.na(survey$bev),]
plot(survey$bev, survey$height)
survey$beverage = droplevels(survey$beverage) # remove the 'none' level
plot(survey$bev, survey$height)
boxplot(survey$height~survey$beverage) 
boxplot(survey$height~survey$beverage, notch=TRUE) # very ugly for small samples!!
groupheights = tapply(survey$height,survey$bev,mean)
barplot(groupheights)
barplot(groupheights,col=c('brown','red','green','beige'))

sds = tapply(survey$height,survey$bev,sd)
sds
sizes = tapply(survey$height,survey$bev,length)
sizes
ses = sds/sqrt(sizes)
ses                  # one NA because 1 data point is not enough for a SD
# let's assume that energy drink(ers) have the same SD as coffee drinkers
ses['energy drink'] = sds['coffee']/sqrt(sizes['energy drink'])
ses

errorbars = function(x, ymin, ymax) {
  arrows(x,ymin,x,ymax,code=3,angle=90)
}

bx = barplot(groupheights,col=c('brown','red','green','beige'), ylim=c(0,75))
errorbars(bx,groupheights-ses, groupheights+ses)



# Two-sample t-test

teadrinkers = survey[survey$beverage=='tea',]
coffeedrinkers = survey[survey$beverage=='coffee',]

height.t = teadrinkers$height
height.c = coffeedrinkers$height
mean.t = mean(height.t)
mean.c = mean(height.c)
sd.t = sd(height.t)
sd.c = sd(height.c)
n.t = nrow(teadrinkers)
n.c = nrow(coffeedrinkers)
sp = sqrt( ((n.t-1)*sd.t^2 + (n.c-1)*sd.c^2 ) / (n.t+n.c-2) )
t = (mean.t-mean.c) / (sp*sqrt(1/n.t+1/n.c))
2*(1-pt(abs(t),n.t+n.c-2))
t.test(teadrinkers$height, coffeedrinkers$height, var.equal=TRUE) 
t.test(teadrinkers$height, coffeedrinkers$height, var.equal=FALSE)

men    = survey[(survey$gender=='male'),]
women  = survey[(survey$gender=='female'),]
mean(men$height)
mean(women$height)
nm = nrow(men)
nw = nrow(women)

t.test(men$height, women$height, var.equal=TRUE)


# Paired t-tests

# Get the wakeup times as simple numeric hours past midnight
wake_wkd = survey$wake_time_wkday
wake_hr_wkd = as.numeric(substr(wake_wkd,0,2))+as.numeric(substr(wake_wkd,4,5))/60
wake_wke = survey$wake_time_wkend
wake_hr_wke = as.numeric(substr(wake_wke,0,2))+as.numeric(substr(wake_wke,4,5))/60
t.test(wake_hr_wkd, wake_hr_wke) # unpaired comparison
t.test(wake_hr_wkd, wake_hr_wke, paired=TRUE) # paired comparison - better CI
t.test(wake_hr_wkd-wake_hr_wke)  # same thing as above


# F-test

f = var(teadrinkers$height)/var(coffeedrinkers$height)
f
n.t = length(teadrinkers$height)
n.c = length(coffeedrinkers$height)
2*(pf(f,n.t-1,n.c-1)) # we are on the lower tail and want a 2-tailed test
var.test(teadrinkers$height, coffeedrinkers$height)


##########


# Reminder about ECDF plots
em = ecdf(men$height)
ew = ecdf(women$height)
x = c(men$height, women$height) # combined data vector
x = sort(x)
plot(x, em(x), typ='s', col='blue', lwd=3, ylab='ECDF', xlim=c(58,75))
lines(x, ew(x), typ='s', col='red', lwd=3)
xp = c(57,x,76) # plotting variables that includes all the points plus plot limits
plot(xp, em(xp), typ='s', col='blue', lwd=3, ylab='ECDF', xlim=c(58,75))
lines(xp, ew(xp), typ='s', col='red', lwd=3)

# Nonparametric two-sample tests

# Wilcoxon rank sum
wilcox.test(men$height, women$height)

# Kolmogorov-Smirnov
K = max(abs(em(x)-ew(x)))
K
ks.test(men$height, women$height)

# Anderson-Darling
points(men$height, em(men$height))
points(women$height, ew(women$height))
ea = ecdf(c(men$height,women$height))
xs = x[1:length(x)-1]
AD = sum( ((em(xs)-ew(xs))^2) / (ea(xs)*(1-ea(xs))))
#install.packages('twosamples')
library(twosamples)
ad_test(men$height, women$height)
  # package value of test statistic will differ due to varying 
  # conventions (divide by N, etc.) and special handling of ties


# Chi-squared contingency test

summary(survey)

lfcfan = survey$football_club == 'Liverpool'
bev = survey$bev
obs = table(bev,lfcfan) # safely ignores NA values
obs

rowsum = apply(obs,1,sum)
colsum = apply(obs,2,sum)
n = sum(obs)
rowp   = rowsum/n
colp   = colsum/n

exp.p  = outer(rowp,colp) # outer product of vectors forms a matrix
exp.p
exp    = exp.p * n 
exp

chi2 = sum((obs-exp)^2/exp)
chi2
dof = (4-1)*(2-1) # 4 columns and 2 rows

1-pchisq(chi2, dof)

chisq.test(obs)  # Counts are too low here to rely on chi^2 test

fisher.test(obs)
