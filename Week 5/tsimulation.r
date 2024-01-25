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

# test with the code below
N = 10
simt = tsimulation(N,1000)
hist(simt,breaks=seq(floor(min(simt)),ceiling(max(simt)),0.1),xlim=c(-3,3),freq=FALSE,col='yellow')
tplot = seq(-10,10,0.1)
lines(tplot, dt(tplot,N-1))
