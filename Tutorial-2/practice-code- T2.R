data_1=rnorm(10, 0, 1)
q=quantile(data, c(0.25, 0.75))

iqr=q[2]-q[1]
se_median1= iqr/(1.349* sqrt(length(data)))
se_median1


data_2=rnorm(50, 0, 1)
q=quantile(data_2, c(0.25, 0.75))

iqr=q[2]-q[1]
se_median2= iqr/(1.349* sqrt(length(data)))
se_median2

data_3=rnorm(250, 0, 1)
q=quantile(data_3, c(0.25, 0.75))

iqr=q[2]-q[1]
se_median3= iqr/(1.349* sqrt(length(data)))
se_median3

data_4=rnorm(1250, 0, 1)
q=quantile(data_4, c(0.25, 0.75))

iqr=q[2]-q[1]
se_median4= iqr/(1.349* sqrt(length(data)))
se_median4
