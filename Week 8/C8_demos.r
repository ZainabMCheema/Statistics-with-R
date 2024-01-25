# --- Column dropping and dataframe merging/splitting ---

# Dropping
dat1 = data.frame(x=1:10, y=3:12, z=6:15, u=11:20, v=LETTERS[rep(1:2,each=5)], w=21:30)
dat1
within(dat1, rm(u)) # drop one column
subset(dat1, select = -c(u, w)) # drop multiple columns at once
dat1[c('x','y')]  # choose columns to retain using named subscript (others are dropped)
# data.frame(x=dat1$x, y=dat1$y, z=dat1$z, v=dat1$v) # could also do it this way

dat1 = subset(dat1, select = -c(u, w)) # remember, have to use assignment to actually change the variable


# Merging
dat2 = data.frame(x=1:10, q=21:30)
merge(dat1, dat2) # automatically picks up that x is the common variable
merge(dat1, dat2, by="x") # safer
mergedat = merge(dat1, dat2, by="x")

dat3 = data.frame(x=1:10, z=16:25)
merge(mergedat, dat3, by="x")  # both data frames have a z column, so gets renamed
merge(mergedat, dat3, by="x", suffixes=c('1','3'))
mergedat = merge(mergedat, dat3, by="x", suffixes=c('1','3'))

# Splitting
splitdat = split(mergedat, mergedat$v)
str(splitdat)
splitdat$A
splitdat$B


# --- Model choice example (ANOVA/F-test) ---

# create fake data
x = seq(0,2,0.1)
y = 1+x+x^2 + 0.2*rnorm(length(x))

plot(x,y)
m = lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5))
summary(m) 
lines(x,predict(m),col='red')

plot(x,y,xlim=c(-2,4),ylim=c(-2,10))
xplot = seq(-1,3,0.1)
lines(xplot,predict(m,list(x=xplot)),col='red')
summary(m)      # note that few or no terms are significant, but this is deceptive
summary.aov(m)  # clearly shows what terms are significant
m2 = lm(y~x+I(x^2))
summary(m2)     # adjusted R-squared is about the same
summary.aov(m2) # confirm no simplification needed
lines(xplot,predict(m2,list(x=xplot)),col='blue')

anova(m2,m) # if we want compare the 2nd to 5th order models directly



# -- AIC and non-nested model choice --

# create fake data
x = seq(0.1,5,0.1)
y = 3/(2+x)^2 + 0.05*rnorm(length(x))

plot(x,y)
m1 = nls(y~a/(b+x)^c,start=list(a=3,b=2,c=2))
lines(x,predict(m1,x),col='blue')
m2 = nls(y~f+exp(-k*x),start=list(f=1,k=1))
lines(x,predict(m2,x),col='red')
summary(m1)
summary(m2)
AIC(m1)
AIC(m2)
AIC(m1,m2)
BIC(m1,m2)

###############


# --- ANCOVA ---

# create fake data (more involved this time)
set.seed(42)
xx = seq(0,10,0.1)
ya = xx+rnorm(101)
yb = xx+2+rnorm(101)
yc = xx+3+rnorm(101)
yd = xx+6+rnorm(101)
x = c(xx,xx,xx,xx)
y = c(ya,yb,yc,yd)
cat = rep(letters[1:4],each=101)
cat = as.factor(cat)

plot(x, y)
cols = c('red','green','blue','purple')
plot(x, y, pch=21, bg=cols[cat])

minteract = lm(y~x*cat) # interacting model: different slopes and intercepts
summary(minteract)
summary.aov(minteract) # interactions are not significant

madditive = lm(y~x+cat) # non-interacting model: same slopes, different intercepts (best model)
summary(madditive)
summary.aov(madditive)
plot(x, y, pch=21, bg=cols[cat])
xplot = c(0,10)
for (i in 1:4) lines(xplot, predict(madditive,list(x=xplot,cat=levels(cat)[c(i,i)])),col=cols[i],lwd=3)
#par(mfrow=c(2,2))
plot(madditive)
#par(mfrow=c(1,1))

msimple = lm(y~x)  # x-only model: same slope and intercepts (not a good fit)
summary(msimple)
plot(x, y, pch=21, bg=cols[cat])
lines(xplot,predict(msimple,list(x=xplot)),lwd=3)

msameintercept = lm(y~x:cat) # same intercept, different slopes (not a good fit)
summary(msameintercept)
plot(x, y, pch=21, bg=cols[cat])
for (i in 1:4) lines(xplot, predict(msameintercept,list(x=xplot,cat=levels(cat)[c(i,i)])),col=cols[i],lwd=3)



# --- Multiple Regression ---

df = read.csv('multireg1.csv',row.names=1)

pairs(df)
pairs(df,pch='.')
pairs(df,pch='.',panel=panel.smooth)

# or, plot to png to view in higher resolution outside R
png('pairs.png', width=1500, height=1500)
pairs(df)
dev.off()

cor(df) # correlation matrix
signif(cor(df),2)

m = lm(y~.*.,data=df)
summary(m)
m2 = step(m)
m2 = step(m, trace=0)
summary(m2)

# remove some additional marginal/insignificant terms
m3 = m2
m3 = update(m3,~.-c:g)
m3 = update(m3,~.-f:g) # etc.
summary(m3)
#par(mfrow=c(2,2))
plot(m3)
#par(mfrow=c(1,1))


AIC(m3)
AIC(m2)

mlin = lm(y~.,data=df) # how much worse does a noninteracting model do?
summary(mlin)
AIC(mlin)


# PRESS (cross-validation based goodness of fit)

estar3 = resid(m3)/(1-lm.influence(m3)$hat)
PRESS3 = sum(estar3^2)
PRESS3
sqrt(PRESS3/length(estar3)) # true predictive accuracy

estar1 = resid(m)/(1-lm.influence(m)$hat)
PRESS1 = sum(estar1^2)
PRESS1                    # PRESS is less than m3, even though SSE is lower
