##############################################################
#PART 1
lg = function(mu, n, ybar) {
  mu2 = mu^2
  n * (ybar * mu - mu2 / 2.0) - log(1 + mu2)
}
## Random-Walk Metropolis-Hastings algorithm

#create the function for Metr.Hast
mh = function(n, ybar, n_iter, mu_init, cand_sd) { 

  ## step 1, initialize
  mu_out = numeric(n_iter)
  accpt = 0
  mu_now = mu_init
  lg_now = lg(mu=mu_now, n=n, ybar=ybar)
  
  ## step 2, iterate iterations=n_iter
  for (i in 1:n_iter) {
    ## step 2a
    mu_cand = rnorm(n=1, mean=mu_now, sd=cand_sd) # draw a candidate
    
    ## step 2b
    lg_cand = lg(mu=mu_cand, n=n, ybar=ybar) # evaluate log of g with the candidate
    lalpha = lg_cand - lg_now # log of acceptance ratio
    alpha = exp(lalpha)
    
    ## step 2c
    u = runif(1) # draw a uniform variable which will be less than alpha with probability min(1, alpha)
    if (u < alpha) { # then accept the candidate
      mu_now = mu_cand
      accpt = accpt + 1 # to keep track of acceptance
      lg_now = lg_cand
    }
    
    ## collect results
    mu_out[i] = mu_now # save this iteration's value of mu
  }
  
  ## return a list of output
  list(mu=mu_out, accpt=accpt/n_iter)
}
#################################################################
#PART 2
#setup
y = c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
ybar = mean(y)
n = length(y)
hist(y, freq=FALSE, xlim=c(-1.0, 3.0)) # histogram of the data
curve(dt(x=x, df=1), lty=2, add=TRUE) # prior for mu
points(y, rep(0,n), pch=1) # individual data points to see where they are in the distribution
points(ybar, 0, pch=19) # sample mean, pch creates a field circle and adds the sample mean on the graph

#Finally, we're ready to run the sampler! Let's use m=1000 iterations 
#and proposal standard deviation (which controls the proposal step size) 3, 
#and initial value at the prior median 0.

set.seed(43) # set the random seed for reproducibility
post = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=3.0)
str(post) #to find out what's in the object

library("coda") #for exploring the posterior distribution
traceplot(as.mcmc(post$mu))
#This last plot is called a trace plot. It shows the history of the chain and provides 
#basic feedback about whether the chain has reached its stationary distribution.
#
#It appears our proposal step size was too large (acceptance rate below 23%). 
#Let's try another.
post = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=0.05)
post$accpt

traceplot(as.mcmc(post$mu))
#the acceptance rate is too high (above 50%). Let's try something in between.
post = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=0.9)
post$accpt

traceplot(as.mcmc(post$mu))
#hat looks pretty good. Just for fun, 
#let's see what happens if we initialize the chain at some far-off value.
post = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=30.0, cand_sd=0.9)
post$accpt

traceplot(as.mcmc(post$mu))
#It took awhile to find the stationary distribution, but it looks like we succeeded! 
#If we discard the first 100 or so values, it appears like the rest of the samples 
#come from the stationary distribution, our posterior distribution! 

#Let's plot the posterior density against the prior to see 
#how the data updated our belief about ??.

#post analysis
post$mu_keep = post$mu[-c(1:100)] # discard the first 200 samples
plot(density(post$mu_keep, adjust=2.0), main="", xlim=c(-1.0, 3.0), xlab=expression(mu)) # plot density estimate of the posterior
curve(dt(x=x, df=1), lty=2, add=TRUE) # prior for mu
points(ybar, 0, pch=19) # sample mean

curve(0.017*exp(lg(mu=x, n=n, ybar=ybar)), from=-1.0, to=3.0, add=TRUE, col="blue") # approximation to the true posterior in blue
#These results are encouraging, but they are preliminary. 
#We still need to investigate more formally whether our Markov chain has converged 
#to the stationary distribution. We will explore this in a future lesson.

#Obtaining posterior samples using the Metropolis-Hastings algorithm 
#can be time-consuming and require some fine-tuning, as we've just seen. 
#The good news is that we can rely on software to do most of the work for us. 
