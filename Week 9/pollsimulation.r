# simulate a fake opinion poll
ansm = sample(c('no','yes'),1000,prob=c(0.45,0.55),replace=TRUE)
ansf = sample(c('no','yes'),1000,prob=c(0.35,0.65),replace=TRUE)
ans = c(ansm, ansf)
gender = as.factor(rep(c('M','F'),each=1000))
age = round(runif(2000,18,80))
poll = data.frame(ans, gender, age)
