
# --- If/then/else conditionals ---

data = rnorm(20) # random normal dist
t = mean(data)/(sd(data)/sqrt(20))
p = 2*(1-pt(abs(t),df=19))

if (p < 0.05) print('False positive!')
if (p >= 0.05) print('True negative.')

if (p < 0.05) print ('False positive!') else print('True negative.')

data = rnorm(20)
t[2] = mean(data)/(sd(data)/sqrt(20))
t
p = 2*(1-pt(abs(t),df=19))
p
if (p < 0.05) print ('False positive!') else print('True negative.')
  # Produces warning message: "if" does not generalize to vectors

#Note: "if" is used sparingly in R; we prefer to use vector/logical operations


# ----- Functions ------

# short functions (one-line)
squared = function(x) { x^2 }  # x is ANY variable
squared(5)
v = c(1,2,3)
squared(v)

times = function(x,y) { x*y }
times(7,9)

stderr.mean = function(v) { sd(v)/sqrt(length(v)) } # SE on the mean
stderr.mean(rnorm(100))

# write a function to find the mode of a vector of discrete data (doesn't deal with ties)
datamode = function(v) { names(sort(table(v),decreasing=TRUE)[1]) }
datamode(c(1,2,3,3,4))

# long functions (many lines)
# begin datamode.r
datamode = function(v) {  # function with multiple lines
	t = table(v)
	w = which(t == max(t))
	maxv = names(t[w])
	if (is.numeric(v)) {
		return(as.numeric(maxv))  # need an explicit return command
	}
	else {
		return(maxv)
	}
}
# end datamode.r
# source('datamode.r')

v = c(1,2,3,3,3,4)
datamode(v)
s = c('a','b','c','c','d')
datamode(s)

# functions don't have to return anything: this adds error bars to a plot
errorbars = function(x, ymin, ymax) {
  arrows(x,ymin,x,ymax,code=3,angle=90)
}
# test the errorbars function
x = 1:5
y = 3:7
plot(x,y,xlim=c(0,6),ylim=c(0,10))
errorbars(x,y-1,y+1)


# functions for simulations
diceroll = function(ndice) { sum(sample(1:6,ndice,replace=TRUE)) }
diceroll(2)
diceroll(2)
diceroll(2)

# specify a default argument (6 sides)
diceroll = function(ndice, nsides=6) { sum(sample(1:nsides,ndice,replace=TRUE))  } #default argument
diceroll(2)
diceroll(2, nsides=20)
diceroll(2, 20)

skewness = function(x) { sum((x - mean(x))^3)/(length(x)*sd(x)^3) } # brackets really important here!
normdata = rnorm(100000)
hist(normdata,col='yellow')
skewness(normdata)

skewdata = abs(rnorm(100000))
hist(skewdata,col='red')
skewness(skewdata)


##########



# Read in some data for the next few examples and store it

survey = read.csv('survey.csv')
height = survey$height   
height
height = height[!is.na(survey$height)]
height = sort(height)
height
plot(height)
n = length(height)

# --- Empirical CDF ---

plot(height,seq(1,n))  # Plot them on the x-axis
plot(height,seq(1,n),ylab='Number same or shorter')
plot(height,seq(1,n)/n,ylab='Proportion same or shorter')
lines(height,seq(1,n)/n,typ='s',col='red')
e = ecdf(height) # defines a new function, e
e(69)
e(64)
e(50)
e(100)
h = seq(55,75,0.1) # x-axis plotting variable
lines(h,e(h),col='blue',typ='s')
h = c(0,height,1000)# best to use data values (plus a little extra)
lines(h,e(h),col='green',typ='s') 

plot(height,seq(1,n)/n,ylab='ECDF',ylim=c(0,1))
lines(h,e(h),col='blue',typ='s')

plot(e)             # easier but less flexible
plot(ecdf(height))  # (same thing)

# --- Quantiles ---

plot(height)
plot(seq(1,n), height)
plot(seq(1,n), height, xlab='Rank')
plot(seq(1,n)/n, height, xlab='Fractional rank')
plot(seq(0,n-1)/(n-1), height, xlab='Fraction of sample',xlim=c(0,1), ylab='Height quantile')
#lines(seq(0,n-1)/(n-1), height, typ='S',col='red')
lines((seq(0,n-1))/(n-1), height,col='blue') # empirical QF

p = seq(0,1,0.001)
lines(p, quantile(height,p),col='green') #empirical QF

quantile(height,0.25) # 1st quartile (25th percentile)
abline(v=0.25, lty=2)
quantile(height,0.75) # 3rd quartile (75th percentile)
abline(v=0.75, lty=2)
quantile(height,0.5)  # median (50th percentile)
summary(height)


# There are other data quantile conventions - this one is used for Q-Q plots:
plot((seq(1,n)-0.5)/n, height,xlim=c(0,1))
lines((seq(1,n)-0.5)/n, height,col='blue')
lines(p, quantile(height,p,type=5),col='red')


# --- QQ plot ---

f = (seq(1,n)-0.5)/n  # The fraction of the data, see above
plot(f,height,xlab='Fraction', ylab='Height')
plot(f,qnorm(f),xlab='Fraction',ylab='Expected z-score') 
plot(qnorm(f),height,xlab='Expected z-score')
qqnorm(height)

# the line in a qq plot is approximately y = sigma*x + mean
pq = seq(-2,2)
lines(pq,sd(height)*pq+mean(height))
qqline(height)

# (the actual qqline line goes through these two points):
lines( qnorm(c(0.25,0.75)), quantile(height,c(0.25,0.75)), col='red', lwd=4)

# (details don't really matter, point is that major deviations from the line -> non-normal)


# --- Some normality tests ---

shapiro.test(height)

skewness(height)
sqrt(6/n) # SE on skewness (from textbook)

ekurtosis = function(x) { sum((x - mean(x))^4)/length(x)/(sd(x)^4) - 3}
ekurtosis(height)
sqrt(24/n) # SE on kurtosis (from textbook)

#install.packages("moments")
library(moments) # note that this overwrites your own skewness(/kurtosis) function
jarque.test(height)


########


# --- Dropping a vector element ---

v = c('a','b','c','d','e')
v[1]
v[-1]
v[-3]
v[c(-1,-3)]
v
w = v[-3]
w



# --- Transforms ---

pop = survey$hometown_pop
pop
pop = pop[!is.na(pop)]
pop = pop[pop < 37e6] # someone claimed their city populaion was 500 million...
pop = sort(pop)
pop
hist(pop)             # very obviously not normal
hist(pop,breaks=seq(0,2e7,5e4))
qqnorm(pop)
qqline(pop)
shapiro.test(pop)

logpop = log10(pop) # apply a logarithmic transform
hist(logpop)
qqnorm(logpop)
qqline(logpop)
shapiro.test(logpop)


# --- Random sampling ---

normdata = rnorm(10000)
hist(normdata)
alsonormdata = qnorm(runif(10000)) # uniformly distributed -> normally distributed
hist(alsonormdata)
qqplot(normdata, alsonormdata) # QQ plot to compare two data sets
qqnorm(normdata)               # QQ plot to compare data to normal distribution
qqnorm(alsonormdata)
# (can use the appropriate quantile function to sample *any* arbitrary
#  distribution by feeding uniform random data to it)
