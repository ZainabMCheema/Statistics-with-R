# reminder to install necessary packages for lab exercises:
#  install.packages("rmarkdown")
#  tinytex::install_tinytex()

# --- Multiple statements on the same line ---

i = 2; i = i + 1; i  

# --- Single statement spanning multiple lines ---

i - 
	1

pnorm(0.3
  )        


# --- A few more quick notes on strings ---

nchar('here is a string of 33 characters')  # outputs 33
length('here is a string of 33 characters') # outputs 1
length(c('here','is','a','vector','of','many','strings')) # outputs 7
nchar(c('here','is','a','vector','of','many','strings'))

# --- Combinatorics ---

factorial(5)
choose(5,3)

# --- Sorting a dataframe ---

df = data.frame(a=1:10, b=seq(16,-11,-3)^2)
df
df = df[order(df$b),]
df
df = df[order(df$a),]
df


# --- Normal functions ---

x = seq(-5,5,0.1)            
plot(x,dnorm(x))              # standard normal PDF
plot(x,dnorm(x),type='l')    
lines(x,dnorm(x,mean=1))      # lines overplots a curve
lines(x,dnorm(x,mean=1),col='red')
lines(x,dnorm(x,mean=1,sd=2),col='blue')

plot(x,pnorm(x),type='l')     # standard normal CDF
p = seq(0,1,0.01)
plot(p,qnorm(p),type='l')     # standard normal quantile function
lines(pnorm(x), x, col='red') # this also plots the quantile function

par(mfrow=c(2,2), mar=c(2,2,1,1)) # multiple plot on a page
plot(x, dnorm(x), type='l', main='PDF')
plot(x, pnorm(x), type='l', main='CDF')
plot(p, qnorm(p), type='l', main='QF')
par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1)) # restore default


# --- Random numbers ---

# Normal distribution
rnorm(1)
rnorm(2)
rnorm(2)
set.seed(999)
rnorm(2)
rnorm(2)
set.seed(999)
rnorm(2)

v = rnorm(100)
v
mean(v)
sd(v)
v = rnorm(100,mean=5,sd=3)
mean(v)
sd(v)
hist(v)
v = rnorm(1e6,mean=5,sd=3)
mean(v)
sd(v)
hist(v)  # default is too coarse...
#hist(v,breaks=-5:15) # excludes some values - R refuses to make the histogram
min(v); max(v)
hist(v,breaks=-15:25,xlim=c(-5,15))
hist(v,breaks=-15:25,xlim=c(-5,15),freq=FALSE,col='beige')
x = seq(-15,25,0.1)
lines(x, dnorm(x,mean=5,sd=3), col='red', lwd=2)


# Uniform distribution

runif(10)
runif(10,min=-1,max=1)
v = runif(10000,min=-1,max=1)
hist(v,col='lightgreen')



# Draw from a list of values

col = c('red','blue','green')
sample(col,1)
sample(col,2)  # draws without replacement
#sample(col,4)  # doesn't work
sample(col,4,replace=TRUE)  # OK
sample(c('H','T'),10,replace=TRUE)
sample(1:6,10,replace=TRUE)


##########


# ---- Loops ----

# for loop : count along a sequence

for (i in 1:3) print('hi')  # you have to explicitly "print" in a loop
for (i in 1:3) print(i)
for (i in 1:3) print(paste(i,'squared is',i^2))
for (i in seq(1,3,0.5)) print(paste(i,'squared is',i^2))
for (i in seq(3,1,-0.5)) print(paste(i,'squared is',i^2))
v = c(4,7,7,1)
for (vi in v) print(vi)
for (i in 1:length(v)) print(paste('The ',i,'th element of v is', v[i]))
for (i in 1:length(v)){w = v[i]; print(w); }

# the following block is saved in demoloop.R
for (i in 1:length(v)) {
  w = v[i]
  print(w)
}
# end demoloop.R
source('demoloop.R',echo=TRUE)

for (i in 1:3) for (j in 1:3) {print(paste(i,j))} # nested loop


# while loop : continue until a condition is no longer satisfied
i = 0
while (i < 10) {i = i + 1}
i
i = 0
while (i < 10) {print(i); i = i + 1;}
i

##########

# Standard error simulation

data = rnorm(25)
sd(data)
hist(data)
mean(data)       # not exactly zero (statistical fluctuations)
data = rnorm(25) # try a new sample
mean(data)

means = numeric(1000)
for (i in 1:1000) means[i] = mean(rnorm(25)) # 25 data points per trial, 1000 trials
hist(means, freq=FALSE)
sd(means)   # about 0.2, as expected: SE = 1/sqrt(25)
1/sqrt(25)

xp = seq(-3,3,0.01)
lines(xp, dnorm(xp, sd=0.20))

  
# t-tests 

dat = rnorm(10,mean=5,sd=1)
m = mean(dat)
n = length(dat)
s = sd(dat)
se = s/sqrt(n)
t = m/se       # for hypothesized mu=0
2*(1-pt(abs(t), df=n-1)) # two-tailed test
t = (m-3)/se   # for hypothesized mu=3
2*(1-pt(abs(t), df=n-1))

t.test(dat)    # convenience function
t.test(dat, mu=3)

m + c(-1,1)*qt(1-0.05/2, df=n-1)*se

