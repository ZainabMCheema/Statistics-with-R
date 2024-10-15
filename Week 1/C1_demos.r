# Demo content from Week 1 (first lab)
# Run all this at once with the command: source('C1_demos.r',echo=TRUE)


# --- Install RMarkdown (needed for tutorials and coursework) ---

# install.packages("rmarkdown")
# tinytex::install_tinytex()

# --- Basic calculator operations ---

1+1
3*7  
40/4 # this is a comment, everything after the hash is ignored by R
2^10
2^100
1/3      # Note that there is no need to worry about integer maths
1+2+4
1+2*3
(1+2)*3  # Specify order of operations with (round brackets)
1^2*3
1^(2*3)

# --- Mathematics -----

pi
cos(pi)
exp(3)
log(3)
log10(10)
round(pi)
round(pi,2)

# --- Variables and assignment ----

x <- 2
x = 2
x       # prints it out
# X     # variables are case sensitive
jimmy = 6
var.name = 1  # allowed
var_name = 1  # allowed
s = "some letters"   # a string

x+1   # operations on variables
x^2   #  (note: none of these change the value of x)
1/x
x+x^2

y = x  # assign variables to others
x = 3
x = x - 1
x
# x -= 1    unfortunately, no increment operator in R


# --- Logical operations ---

y = 3   # set y equal to 3
y == 3  # check if y is equal to 3
y == 4
y != 4
!(y==4)
x > y
x < y
x <= y
x == y
x != y
(x == 2) & (y == 3)
(x == 2) | (y == 3)
(x == 2) && (y == 3) # does the same thing for scalars
(x == 2) || (y == 3)

##########

# --- Vectors ---

v = c(5,6,7,8,9)
v
v = 5:9        # does the same thing
v = seq(5,9)   # does the same thing
seq(10,20,2)
seq(20,10,-2)

# --- Vector indexing ---

v
v[1] # the first element
v[0] # NOT the first element
v[3]
v[1:2] # subset vectors with vectors
v[c(1,3)]

# --- Vector arithmetic ---

v + 1
v^2
1/v
sin(v)
v == 6  # Logical operation
v > 6
(v > 6) & (v < 8)   # compares all elements
(v > 6) && (v < 8)  # checks only first element of each (rarely wanted)

# --- Vector operations ---

sum(v)
length(v)
sort(v)
min(v)
max(v)
mean(v)
median(v)
sd(v)

?sort  # help function
??sort # search for a word in the help database

# --- Special vector creation ---

rep(2,22)    # repeat the number 2, 22 times
numeric(20)  # twenty zeroes

# --- Vector subset reassignment ---

v = 5:9 
v
w = v^2
w
w[1:2]
w[1:2] = -1
w[1:2]
w

v > 7         # Checking which values meet a condition
which(v > 7)
v[4:5]
v[which(v > 7)]
v[v > 7]    # conditional subscripting

w[v > 7]    # conditional subscripting for paired vectors 
w[v > 7] = 0
w
w[v > 7] = v # doesn't work: trying to put five elements of v into two of w
w[v > 7] = v[v > 7]
w

##########

# --- Plotting ----

plot(v)
plot(v,w)           # scatter plot
?plot               # help command
plot(v,w,type='l')  # line plot
plot(v,w,type='l',main='v versus w')

plot(v,w)
plot(v,w,ylim=c(0,100))
plot(v,w,ylab='the y axis')
plot(v,w,pch=21,bg='red') # filled circles
lines(v,w)                # add lines to a plot
#plot(v,w,typ='l',add=TRUE) # this used to also add lines, but no longer works it seems
lines(v,w+5,lty=2)        # dashed lines
lines(v,w+10,lty=3,lwd=3,col='red')  # dotted, thick, red
points(v,v)               # add points to a plot
points(v,v,cex=2)         # point size

abline(h=20)         # annotate a horizontal line
abline(v=5.5)        # vertical line
arrows(7,7, 7.5,15)  # arrow
# can save with dropdown or at command line (within R console)

# --- Multi-panel plots ---

par(mfrow=c(1,2))   # Two plots on screen
plot(v,v)
plot(v,v^2)

par(mfrow=c(2,2))   # Four plots on screen (2x2)
plot(v,v)
plot(v,w)
plot(w,v)
plot(w,w)

par(mar=c(4,4,1,2))  # reduce unused whitespace
plot(v,v)
plot(v,w)
plot(w,v)
plot(w,w)
par(mfrow=c(1,1))    # Back to single panel plot 


# --- Bar plots ---

c = c('A','B','C')  # character vector
d = c(1,2,4)
barplot(d,names.arg=c)
barplot(d,names.arg=c,col=c('red','green','blue'))

# --- Pie charts ---

pie(d, labels=c)

