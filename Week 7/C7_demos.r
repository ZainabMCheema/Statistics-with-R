# --- Single-factor ANOVA ---

survey = read.csv('survey.csv', as.is=FALSE)
survey = survey[!is.na(survey$height),]
attach(survey)

par(mar=c(4,4,1,1))
# search for a relationship between favourite colour and height
  # visual inspection: box plot
plot(colour1, height)
plot(height ~ colour1)
plot(height ~ colour1, col=c('blue','green','red'))
  # visual inspection: colour-coded scatter plot (no x-axis)
plot(height, pch=21, bg=c('blue','green','red')[colour1], cex=2)
o = order(colour1)
plot(height[o], pch=21, bg=c('blue','green','red')[colour1[o]], cex=2)
  # visual inspection: barplot
means = tapply(height, colour1, mean)
pl = barplot(means, col=c('blue','green','red'), ylim=c(0,80))
SE = tapply(height, colour1, sd)/sqrt(tapply(height, colour1, length))
errorbars = function(x, ymin, ymax) arrows(x,ymin,x,ymax,code=3,angle=90) 
errorbars(pl, means-SE, means+SE)
  # visual inspection: plot with error bars
plot(means, ylim=c(60,75), xlim=c(0.5,3.5), pch=21, bg=c('blue','green','red'), cex=2, 
		 xaxt='n', xlab='', ylab='mean height')
errorbars(1:3, means-SE, means+SE)
axis(1, at=1:3, labels=names(means), las=3)

  # t-tests (not the best for multiple comparisons)
height.blue = height[colour1=='Blue']
height.green = height[colour1=='Green']
height.red = height[colour1=='Red']
t.test(height.blue, height.red, var.equal=TRUE)
t.test(height.blue, height.green, var.equal=TRUE)
t.test(height.green, height.red, var.equal=TRUE)

  # analysis of variance
m = aov(height~colour1)
summary(m)
summary.lm(m)
  # Tukey's HSD (a post-hoc test)
TukeyHSD(m)

# search for a relationship between favourite beverage and height
beverage[is.na(beverage)] = 'none'
plot(height ~ beverage, col=c('darkgray','red','green','white','brown'))
m = aov(height~beverage)
summary(m)
summary.lm(m)
TukeyHSD(m)

#search for a relationship between football club and height
m = aov(height~football_club)
summary(m)

#search for a relationship between gender and height
m = aov(height~gender)
summary(m)
summary.lm(m)

#detach(survey)

##########


# --- Two-factor ANOVA ---

#survey = read.csv('survey.csv')
#survey = survey[!is.na(survey$height),]
#attach(survey)
colour = factor(colour1, levels=c('Blue','Red','Green')) # reorder factor levels
means = tapply(height, list(colour,beverage), mean)
means
barplot(means, beside=TRUE,col=c('blue','red','green'))

m2 = aov(height~colour*beverage)
summary(m2)
summary.lm(m2)

m2s = aov(height~colour+beverage)
summary(m2s)
summary.lm(m2s)


# --- Multi-factor ANOVA ---

m3 = aov(height~gender*beverage*colour)
summary(m3)
detach(survey)

ic = read.csv('icecream.csv',row.names=1,as.is=FALSE)
head(ic)
summary(ic)
7*2*2*3*2  # number of parameters if all interactions considered
nrow(ic) # not enough data to consider them all

par(mfrow=c(2,3))
par(mar=c(2,2,2,0.5))
plot(ic$profits)
plot(ic$day,ic$profits,main='day')
plot(ic$sky,ic$profits,main='sky')
plot(ic$wind,ic$profits,main='wind')
plot(ic$rain,ic$profits,main='rain')
plot(ic$temp,ic$profits,main='temp')
par(mfrow=c(1,1))
par(mar=c(4,4,1,1))

tapply(ic$profits, list(ic$day,ic$temp), mean)
tapply(ic$profits, list(ic$day,ic$rain), mean)

icm1 = aov(profits~.*.,data=ic)  # Multi-variable ANOVA with two-way interactions
summary(icm1)

icm2 = aov(profits~day+wind+rain+temp, data=ic) # sky and interactions weren't significant: drop
summary(icm2)
summary.lm(icm2)

# Reduce days of week by merging Mo+Tu+We+Th into weekday
icw = ic
levels(icw$day)
levels(icw$day) = c('Fr','weekday','Sa','Su','weekday','weekday','weekday') 
icm3 = aov(profits~day+wind+rain+temp,data=icw)
anova(icm3, icm2)    # simpler model not significantly different, so adopt it


# Predict whether to open or close on various days based on weather forecast
predict(icm3,list(day='Sa',     wind='calm',rain='rainy',temp='cool'))
predict(icm3,list(day='weekday',wind='calm',rain='rainy',temp='cool'))
predict(icm3,list(day='weekday',wind='calm',rain='rainy',temp='cool'),level=0.95,interval="prediction")

# Check plots
plot(icm3)

# Reordering factors
levels(icw$day)
icw$day = factor(icw$day,levels(icw$day)[c(2,1,3,4)]) # what was 2nd now is in the first position...
levels(icw$day)



##########

# --- Multi-level ANOVA ---

re.full = read.csv('reactiontimes.csv',as.is=FALSE)
summary(re.full)
a.wrong = aov(reaction~gender*treatment,data=re.full) # wrong analysis
summary(a.wrong)
head(re.full)

re = aggregate(reaction~gender*names*treatment,data=re.full,FUN=mean) # average over pseudoreplicates
head(re)

# Ask three questions:
#  Q1 - Does gender affect reaction time?
#  Q2 - Does caffeine improve reaction time?
#  Q3 - Does gender affect how much caffeine improves reaction times?

# Method A: Do the error grouping ourselves with aggregate()

# Q1 is a between-subjects test: gender is a fixed property of the subjects
re2 = aggregate(reaction~gender*names,data=re.full,FUN=mean) # average over caffeine (assumes balanced design)
re2
tapply(re2$reaction, re2$gender, mean)
mean(re2$reaction[re2$gender=='M'])
mean(re2$reaction[re2$gender=='F'])
mean(re2$reaction[re2$gender=='M'])-mean(re2$reaction[re2$gender=='F'])
t.test(re2$reaction[re2$gender=='M'],re2$reaction[re2$gender=='F'],var.equal=TRUE) # p = 0.305

# Q2 is a within-subjects test: caffeine was varied within each subject
re
t.test(re$reaction[re$treatment=='control'],re$reaction[re$treatment=='caffeine'],paired=TRUE) # p = 0.013
 # alternative approach:
diff = re$reaction[re$treatment=='control'] - re$reaction[re$treatment=='caffeine']
diff # this is the improvement in reaction time for all 10 individuals
mean(diff) # 0.05 seconds faster reaction
t.test(diff, mean=0) # one-sample t-test; confirm what we found already from the paired test


# Q3 is a mixed-effects model: both within and between subjects variables are involved
re
re.diff = data.frame(difference=diff, gender=re$gender[1:10], names=re$names[1:10])
re.diff
t.test(re.diff$difference[re.diff$gender=='F'],re.diff$difference[re.diff$gender=='M'],var.equal=TRUE) # p = 0.918


# Method B: Using Error() in R

# Q1/Q2
a12 = aov(reaction~gender+treatment+Error(names/treatment),data=re.full)
summary(a12)

# Q3
a3 = aov(reaction~gender*treatment+Error(names/treatment),data=re.full)
summary(a3)

interaction.plot(re.full$gender,re.full$treatment,re.full$reaction)
