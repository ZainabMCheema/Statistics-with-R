
counts = 5  # this is our measurement from a counting (Poisson) process -
            # perhaps the number of times something that happened in a one-second interval.
            # What could lambda (the true, long-term average rate per second) be?

# could the true, long-term average be...
# ... 10 per second?  (our current hypothesis)
plot(0:30, dpois(0:30, lambda=10.0), pch=0, xlab='n', ylab='p(n) if lambda=10')
points(0:5, dpois(0:5, lambda=10.0), pch=0, col='red', lwd=3) # "as or more extreme" than 5
sum(dpois(0:5, lambda=10.0))   # yes, our result is reasonably likely if so
ppois(5, lambda=10)            # another way to do the above
# ... 12 per second?
sum(dpois(0:5, lambda=12.0))   # no, our result is rather unlikely if so
# ... 11.66 per second?
sum(dpois(0:5, lambda=11.66))  # 11.66 per second?  marginal: this is our conf. interval bound
# note - we are actually looking for 0.025 because high-n would also have been "extreme"
# and the direct dpois/ppois value only accounts for n <= the observed k.

# ... 2 per second?   (our current hypothesis)
plot(0:30, dpois(0:30, lambda=2.0), pch=0, xlab='n', ylab='p(n) if lambda=10')
points(5:30, dpois(5:30, lambda=2.0), pch=0, col='red', lwd=3) # "as or more extreme" than 5
sum(dpois(5:100, lambda=2.0))  # ... yes, likely
1-ppois(5-1, lambda=2) # another way to do the above (note: need for -1 when using ppois here)
sum(dpois(5:100, lambda=1.0))  # 1.0 per second?  no, unlikely
sum(dpois(5:100, lambda=1.62)) # 1.62 per second?  marginal: this is our conf. interval bound

# We have to solve for the value where the sum is 0.025 (two-tailed case).
# This could be done iteratively, or use the lecture equation:

qchisq(0.025, 2*5)/2      # lower interval boundary ~=  1.62
qchisq(0.975, 2*(5+1))/2  # upper interval boundary ~= 11.66


# visually recap
plot(0:30, dpois(0:30, lambda=11.66), pch=0, xlab='n', ylab='p(n) if lambda=11.66')
points(0:5, dpois(0:5, lambda=11.66), pch=0, col='red', lwd=3) # sum of these is 0.05/2
plot(0:10, dpois(0:10, lambda=1.62), pch=0, xlab='n', ylab='p(n) if lambda=1.62')
points(5:10, dpois(5:10, lambda=1.62), pch=0, col='red', lwd=3) # sum of these is 0.05/2


