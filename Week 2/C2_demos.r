# Reminder: to install Rmarkdown, type: install.packages("rmarkdown")

# --- Variable types and casting/coercion ----

a = 4
a = 44.4
b = 'text'
b = '4'
c = TRUE
a
b
c
mode(a)
mode(b)
mode(c)
typeof(a)
typeof(b)
typeof(c)
str(a)           # str = structure (nothing to do with strings)
str(b)
str(c)

#a+b             # doesn't work
as.numeric(b)
a+as.numeric(b)
as.character(a)
paste(as.character(a),b) # connect two character strings together
paste(a,b)               # numeric is automatically coerced to character in paste
paste('my strings:',a,'and',b)
paste(a,b,sep='_') # connect two character strings together
paste0(a,b)

a+c  # logical is automatically coerced to numeric (no need to convert): FALSE=0, TRUE=1


# --- String manipulation ---

s = "Some Text"  # either single or double quotes is OK
mode(s)

substr(s,2,3)
toupper(s)
tolower(s)

gsub(' ','.',s)

strsplit(s, ' ')
strsplit(s, 'e')

strsplit(s,' ')[2]  # trying to get the second word, but this doesn't work
u = strsplit(s,' ')
str(u)              # u is a list with one element, not a vector (see below)
u[[1]]              # this will return the vector of words
u[[1]][2]           # this will return the second word
v = strsplit(s,' ')[[1]]  # Get the vector directly
v[2]

####################

# --- Matrices ----

m = matrix(0,3,3)
m
m = matrix(c(1,2,3),3,3)
m
m = t(m)  # transpose
m

m[1,1] # element of a matrix
m[1,2] 
m[2,1]
m[1:2,1] # multiple elements of a matrix
m[1,1:2]
m[1:2,1:2]
m[1,c(1,3)]

m[1,] # first row
m[,1] # first column

m[1,1] = 0   # or use <- instead of =

m = rbind(m,c(9,9,9)) # add a row
m

m = cbind(m,c(5,5,5,5)) # add a column
m

sum(m)           # over the whole matrix
median(m)

apply(m,1,sum)  # for each row, sum over all columns
apply(m,2,sum)  # for each column, sum over all rows
apply(m,2,median) # for each column, median over all rows



# --- Named vectors ---

v = c(1,3,4,5)
v
names(v) = c('bob','bill','jim','joe')
v
v[1]
v['bob']
str(v)

# Also works on matrices
rownames(m) = c('A','B','C','D')
colnames(m) = c('Q','R','S','T')
m
m['B','T']
m[c('A','B'),c('S','T')]

# --- Lists ----

l = list('a', 1, TRUE, v)
l[1]
l[4]

l = list(series='Star Wars', genre='Sci-Fi', sales=34819048, titles=c('A New Hope','The Empire Strikes Back','Return of the Jedi'))
l[1]
l[4]
l['series']
l['titles']
l['titles'][2]   # doesn't work - actually single brackets creates a NEW list containing just $titles and nothing else...
str(l['titles'])
l[['titles']][2] # this does work (but looks ugly)
l$titles[2]      # this is the best way to do it
str(l$titles)


####################


# --- Dataframe ---

teams = c('A', 'B', 'C', 'D')
wins = c(1,3,1,4)
losses = c(4,1,3,1)

df = data.frame(wins=wins,losses=losses, row.names=teams)
df
df['A','wins']
df[1,'wins']
df['A',]
df[,'wins']
df$wins

apply(df,1,sum) # sum over dimension 1 (x) to produce number of games for each team
apply(df,2,sum) # sum over dimension 2 (y) to produce total wins, and total losses, for all teams
winfrac = df[,'wins']/apply(df,1,sum)
newdf = cbind(df, winfrac)
newdf
newdf$winfrac
newdf$lossfrac = 1-winfrac # also works, even if column doesn't exist yet

# --- Loading tables from disk ---

getwd() # check current working directory
#setwd('somedirectory') # change the working directory, if necessary

games = read.table('winloss.txt',header=TRUE,row.names=1)
games

survey = read.csv('survey.csv')             # older versions of R
survey = read.csv('survey.csv',as.is=FALSE) # newer versions of R
survey = read.csv('survey.csv',stringsAsFactors=TRUE) # largely equivalent
 # Could also convert columns to factors if needed using as.factor()

survey[1,]
survey[3,]
survey[,2]
survey$height 
survey$gender  # factor level vector
str(survey)    # structure of a variable

head(survey)
summary(survey)

# --- More on factor levels ---

f = survey$football_club
f
str(f)
typeof(f)
levels(f)
f[1]
f[1] = 'Liverpool'
f[1]
# f[1] = 'Tottenham'  # doesn't work
levels(f)
levels(f) = c(levels(f),'Tottenham') # create a new factor level
f[1] = 'Tottenham'    # now it works
f[1]
survey$football_club[1] # we only changed a copy

mean(survey$distance)
mean(survey$berlin_dist_est)
survey$berlin_dist_est
mean(survey$berlin_dist_est,na.rm=TRUE) # NA is not the same as NaN
0/0  #NaN


# --- Logical subscripting with data frames ---

survey$distance <= 5               # for each student, are they within 5 miles of Liverpool centre?
which(survey$distance > 5)         # which students (sequence numbers) are beyond 5 of Liverpool?
survey$beverage[survey$distance > 5]  # what are the beverage preferences of students with longer commutes?

# --- Logical subscripting and missing data ---

survey$beverage == 'coffee'
survey$beverage                         # Logical operations on NA's return NA again!
survey$gender[survey$beverage == 'coffee'] # Propagates forward even though no missing age values...

is.na(survey$beverage)
survey$gender[survey$beverage == 'coffee' & !is.na(survey$beverage)]
survey$gender[survey$beverage == 'coffee' & 
             !is.na(survey$beverage)]  # split a command across two lines

  # or, for a more long-lasting solution
survey2 = survey[!is.na(survey$beverage),] # Remove rows with missing beverage data
survey2$age[survey2$beverage == 'coffee']
survey3 = na.omit(survey)   # remove ALL rows with ANY missing data - might be overkill!!
nrow(survey) 
nrow(survey3)  # lost almost half our responses

# --- Operating by categorical factor ---

survey$height[survey$gender=='male']
median(survey$height)
tapply(survey$height,survey$gender,median) # blocks by categorical factor
tapply(survey$height,survey$gender,min)
tapply(survey$height,survey$gender,max)


# --- Plotting with data frames  ---

plot(survey$age, survey$height)
# getting bored of entering in "survey"?
plot(height ~ age, data=survey)

attach(survey)  # copy all internal vectors to the top level for easy access (use with caution)
plot(age,height)


# --- Boxplots and factors as independent variables ---

plot(gender,height)   

height[gender=='male']
sort(height[gender=='male'])

boxplot(height~gender)  # boxplot syntax
boxplot(height~siblings)    # works on non-categorical data if similar values
siblfactor = as.factor(siblings)
plot(siblfactor,height)

plot(distance,height)
plot(distance,height,log='x') # log(0) = -infty, so there is a warning message
plot(distance,height,log='x',pch=21,cex=1.5)
plot(distance,height,log='x',pch=21,cex=1.5,bg=c('red','blue')[gender]) # Colour coding
plot(distance,height,log='x',pch=c(21,22)[gender],cex=1.5,bg=c('red','blue')[gender]) # Colour+symbol coding
as.numeric(gender)  # factor levels are stored as (and coerce to) simple integers

legend("bottom",levels(gender),bg='white',pt.bg=c('red','blue'),cex=1.5,pch=c(21,22))
# legend(locator(1),levels(gender),pt.bg=c('red','blue'),cex=1.5,pch=21) # point and click



# --- Histograms and frequency plots ---

hist(height)
hist(siblings) # Odd result - hist is not really designed for discrete integers.
hist(siblings,breaks=seq(-0.5,5.5))
hist(siblings,breaks=seq(-0.5,5.5),col='red')
hist(siblings,breaks=seq(-0.5,5.5),col='red',freq=FALSE)

table(siblings) # frequency table
table(football_club)
barplot(table(football_club))
barplot(table(football_club), las=2)
par(mar=c(8,2,1,1))
barplot(table(football_club), las=2, cex.names=0.9)

detach(survey)  # remember to clean up!!

