#Monte Carlo Approximation Standard Error
set.seed(32)

m = 10000
a = 2
b = 1/3

theta = rgamma(m,a,b)
theta
# Show only 6 first values
head(theta)
# Show only 6 last values
tail(theta)

hist(theta, freq=FALSE)
#draw a curve on the plot
curve(dgamma(x,a,b), col="blue", add=TRUE)

#find a Monte Carlo approximation to the expected value of theta
# 1a.take an average of our simulated values
sum(theta)/m
# 1b. Faster Method
mean(theta)

#expected value of theta
a/b

#variance of theta
var(theta)
#Theoretical Variance
a/b^2

#approximate the probability that theta < 5
ind= theta < 5
head(ind)

mean(ind)

#Compare to the true probability
pgamma(q=5,a,b)

#Simulated 0.9 quantile MC 
quantile(theta, probs=0.9)
#Actual 0.9 quantile
qgamma(p=0.9,a,b)
