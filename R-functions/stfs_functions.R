#By M.D. Edge, 9/19/2018.
#This script defines functions used in the main text or exercises of
#Statistical Thinking from Scratch.

#To run this script, loading all packages and defining all functions used in the book, 
#install and load the devtools package and use the source_url() function, like so:
#if(!("devtools" %in% installed.packages())){install.packages("devtools")}
#library(devtools)
#source_url("https://raw.githubusercontent.com/mdedge/stfs/master/R-functions/stfs_functions.R")

#All of the functions contained here are also included in the book's companion 
#R package, stfspack. To install and load stfspack, use
#if(!("devtools" %in% installed.packages())){install.packages("devtools")}
#library(devtools) 
#Next, install and load the package.
#if(!("stfspack" %in% installed.packages())){install_github("mdedge/stfspack")}
#library(stfspack)

#Install (if necessary) and load all packages used in the book's code.
if(!("animation" %in% installed.packages())){install.packages("animation")}
if(!("car" %in% installed.packages())){install.packages("car")}
if(!("coda" %in% installed.packages())){install.packages("coda")}
if(!("devtools" %in% installed.packages())){install.packages("devtools")}
if(!("gpairs" %in% installed.packages())){install.packages("gpairs")}
if(!("gvlma" %in% installed.packages())){install.packages("gvlma")}
if(!("MASS" %in% installed.packages())){install.packages("MASS")}
if(!("MCMCpack" %in% installed.packages())){install.packages("MCMCpack")}
if(!("quantreg" %in% installed.packages())){install.packages("quantreg")}
library(animation)
library(car)
library(coda)
library(devtools)
library(gpairs)
library(gvlma)
library(MASS)
library(MCMCpack)
library(quantreg)

#Function that simulates a specified number of samples of a specified
#size from the beta distribution and plots the distribution of 
#sample means.
#shape1 and shape2 are parameters of the beta distribution.
dosm.beta.hist <- function(samp.size, n.samps, shape1 = 1, shape2 = 1, ...){
  samps <- rbeta(samp.size*n.samps, shape1, shape2)
  sim.mat <- matrix(samps, nrow = n.samps)
  dosm <- rowMeans(sim.mat)
  hist(dosm, freq=FALSE, ...)#freq=FALSE re-scales the height
  x <- seq(0,1,length.out = 1000)
  lines(x, dnorm(x, mean = mean(dosm), sd = sd(dosm)))
}

#Functions to simulate data from Pareto distribution.
#You can also get these from the rmutil package.
rpareto <- function(n, a=0.5, b=1){qpareto(runif(n),a,b)}
qpareto <- function(u, a=0.5, b=1){b/(1-u)^(1/a)}

#Compares proportion of observed sample means beyond 
#k deviations(numerator) with probability of being beyond k
#standard deviations away from expectation if the distribution
#is normal (as a quotient). Numbers close to 1 indicate 
#that the proportion of means within k standard deviations 
#is close to normal expectation. (You will see in 
#ch. 7 that we would usually call the standard deviation 
#of a mean a "standard error.") Numbers greater than 
#1 indicate that there are more sample means beyond 
#k standard deviations away from the expectation than expected.
#x is a vector of means, k is a number of standard deviations,
#expec is the expected value of the sample means, sd.mean is the
#standard deviation of the sample means.
compare.tail.to.normal <- function(x, k, expec, sd.mean){
  mean(x < (expec - k* sd.mean) | x > (expec + k* sd.mean))/(1 - (pnorm(k) - pnorm(-k)))
}

#Generates one set of n paired observations using a simple
#linear model with intercept a, slope b, and (by default) normally
#distributed disturbances.
sim.lm <- function(a, b, var.eps = 1, n = 50, mu.x = 8, var.x = 4, rx = rnorm, rdist = rnorm){
  x <- sort(rx(n, mu.x, sqrt(var.x)))
  disturbs <- rdist(n, 0, sqrt(var.eps))
  y <- a + b*x + disturbs
  cbind(x,y)
}

#Draw nsamps samples of size n from a normal distribution with
#expectation mu and standard deviation sigma.
norm.samps <- function(mu = 0, sigma = 1, n = 25, nsamps = 10000){
  samps <- rnorm(n*nsamps, mu, sigma)
  matrix(samps, nrow = nsamps, ncol = n)
}

#function to draw a sample of size n from a Laplace distribution
#with expectation equal to the parameter mean and standard deviation equal
#to sd.
rlaplace <- function(n, mean = 0, sd = 1){
  exp1 <- rexp(2*n, 1)
  x <- exp1[1:n] - exp1[(n+1):(2*n)]
  x * sd/sqrt(2) + mean
}

#An analogue of norm.samps() that produces a matrix where
#each row is a sample size size n from a laplace distribution
#with expectation mu and standard deviation sigma.
laplace.samps <- function(mu = 0, sigma = 1, n = 25, nsamps = 10000){
  samps <- rlaplace(n*nsamps, mu, sigma)
  matrix(samps, nrow = nsamps, ncol = n)
}

#A function to draw normal samples that are "contaminated"
#from outliers from a gamma distribution.
#nsamps is the number of samples to draw, 
#n is the number of observations in each sample, 
#gamma is the probability that each observation is 
#from the non-target distribution, 
#theta is the #parameter to be estimated, and 
#lambda is the expectation of the #non-target distribution. 
#Each column in the output matrix is a sample of size n.
rnorm.out <- function(nsamps, n, gamma, theta = 0, lambda = 10){
  n.target <- rbinom(1, n*nsamps, 1-gamma)
  n.nontarget <- n*nsamps - n.target
  samp.target <- rnorm(n.target, theta, 1)
  samp.nontarget <- rnorm(n.nontarget, lambda, 1)
  allsamps <- sample(c(samp.target, samp.nontarget))
  matrix(allsamps, nrow = n, ncol = nsamps)
}

#Draw nsims samples of two variables under a linear model
#using sim.lm(), and save estimated intercept and slope
#parameters from each. By default, lm() is used to return
#least-squares estimates, but other functions can be specified
#using the estfun argument. (For example, if the quantreg
#package is loaded, then setting estfun = rq produces least-
#absolute-error estimates)
sim.lm.ests <- function(nsims, n = 50, a = 0, b = 0, var.eps = 1, mu.x = 0, var.x = 1, rx = rnorm, rdist = rnorm, estfun = lm){
  ests <- matrix(nrow = nsims, ncol = 2)
  for(i in 1:nsims){
    dat <- sim.lm(n = n, a = a, b = b, var.eps = var.eps, mu.x = 		mu.x, var.x = var.x, rx = rx, rdist = rdist)
    ests[i,] <- estfun(dat[,2] ~ dat[,1])$coef
  }
  ests
}

#Draw a sample from a mixture of two normals. The "contaminating" distribution
#is represented with a probability contam.p and has a mean and sd contam.mean
#and contam.sd. If contam.p = 0, then this acts just like rnorm() 
#(but is a little slower).
rnorm.mix <- function(n, mean = 0, sd = 1, contam.p = 0.01, contam.mean = -5, contam.sd = 0){
  ncontam <- rbinom(1, n, contam.p)
  c(rnorm(n - ncontam, mean, sd), rnorm(ncontam, contam.mean, contam.sd))
}

#Given a computed mean of a sample (x.bar) that is assumed
#to follow a normal distirbution, a hypothesized expectation (mu),
#and a standard error (stand.err), computes a two-tailed p value.
#of the null hypothesis that the expectation is mu.
#twotailed.p.normal <- function(x.bar, mu, stand.err){
#  abs.diff <- abs(x.bar - mu)
#  2 * pnorm(mu - abs.diff, mean = mu, sd = stand.err)
#}


#Function for simulating multiple testing on n.meas measurements,
#for a test comparing two group means.
#n is the number of participants in each group, level is the
#significance level of the test, n.meas is the number
#of measurements being tested, correl is the correlation
#between distinct measurements, and n.sims is the
#number of simulations to run.
many.outcome.sim <- function(n, level, n.meas, correl, n.sims){
  #A function for computing a p-value comparing the means
  #of two groups whose scores are entered in a vector.
  #Assumes an input vector with an even number of entries,
  #the first half of which are in one group, with the second
  #half in a different group..
  t.p <- function(x){
    n.tot <- length(x)
    n.1 <- n.tot/2
    x1 <- x[1:n.1]
    x2 <- x[(n.1+1):n.tot]
    t.st <- (mean(x1) - mean(x2))/sqrt((sd(x1)^2 + sd(x2)^2)/n.1)
    2*pt(-abs(t.st), n.tot - 2)
  }
  #Specify a covariance matrix for mvrnorm().
  sigma <- matrix(correl, nrow = n.meas, ncol = n.meas)
  diag(sigma) <- 1
  #Initialize a matrix for holding p values.
  sim.ps <- matrix(-9, nrow = n.sims, ncol = n.meas)
  #simulate all the necessary observations.
  sim.dat <- mvrnorm(n.sims*n*2, mu = rep(0, n.meas), Sigma = sigma)
  ps.mat <- matrix(NA, nrow = n.sims, ncol = n.meas)
  #For each measurement, conduct hypothesis tests for each sample.
  for(i in 1:n.meas){
    test.dat <- matrix(sim.dat[,i], nrow = n.sims)
    ps.mat[,i] <- apply(test.dat, 1, t.p)
  }
  ps.mat
}

#Function for simulating serial testing
#ns is a vector giving the number of particiants in each
#group at each test, level is the significance level of 
#the test and n.sims is the number of simulations to run.
serial.testing.sim <- function(ns = c(20, 30, 40, 50), n.sims = 10000){
  #A function for computing a p-value comparing the means
  #of two groups whose scores are entered in a vector.
  #Assumes an input vector with an even number of entries,
  #the first half of which are in one group, with the second
  #half in a different group..
  t.p <- function(x){
    n.tot <- length(x)
    n.1 <- n.tot/2
    x1 <- x[1:n.1]
    x2 <- x[(n.1+1):n.tot]
    t.st <- (mean(x1) - mean(x2))/sqrt((sd(x1)^2 + sd(x2)^2)/n.1)
    2*pt(-abs(t.st), n.tot - 2)
  }
  #Initialize a matrix for holding p values.
  sim.ps <- matrix(-9, nrow = n.sims, ncol = length(ns))
  #simulate all the necessary observations.
  sim.dat <- matrix(rnorm(n.sims*max(ns)*2, 0, 1), nrow = n.sims)
  ps.mat <- matrix(NA, nrow = n.sims, ncol = length(ns))
  max.n <- max(ns)
  #For each measurement, conduct hypothesis tests for each sample.
  for(i in 1:length(ns)){
    test.dat <- sim.dat[,c(1:ns[i], (max.n + 1):(max.n + ns[i]))]
    ps.mat[,i] <- apply(test.dat, 1, t.p)
  }
  ps.mat
}

#Function giving a power estimate by simulating nsims random 
#samples of n observations each, drawn from a normal distribution 
#with an expectation that is d standard deviations away from the 
#expectation under the null hypothesis. Each sample is tested 
#against the null hypothesis, and the power is estimated by 
#computing the proportion of the samples that generate p values 
#less than the level. 
ps.1sz <- function(d, n, level = 0.05, nsim = 10000){
  simmat <- matrix(rnorm(n*nsim, d, 1), nrow = nsim)
  samp.means <- rowMeans(simmat)
  neg.devs <- -abs(samp.means)
  ps <- 2*pnorm(neg.devs, 0, 1/sqrt(n))
  mean(ps < level)
}

#Function to simulate the "winner's curse"---the effect that especially in low-power
#situations, estimates that result in signficant tests for the null hypothesis of
#theta = 0 tend also to be overestimates.
#In each simulation, a single normal sample is drawn and the hypothesis that the true effect size is zero
#is tested(by one-sample t-test).
#The output is named vector with a a true effect size (measured in number of standard deviations
#from the value under the null hypothesis), the "estimated" effect size, which
#is the mean effect size from simulations that produced significant results,
#and the power, which is the proportion of simulations that achieved significance.
#A histogram is also produced, with all the estimated effect sizes shown, and the
#estimates associated with significant tests colored grey.
#d is the true effect size in the simulations.
#n is the size of each simulated sample
#lev is the significance level---effect size estimates are averaged in the "estimated d" part of the output only if the one-sample z test produces a p value less than the level.
#nsim is the number of simuations to run.
#abs.vals controls whether we pay attention to the sign of the estimate (if FALSE, the default, we do).
#br is the breaks parameter for the histogram.
wc.1sz <- function(d, n, lev = 0.05, nsim = 10000, abs.vals = FALSE, br = 50){
  samps <- rnorm(n*nsim, d, 1)
  simmat <- matrix(samps, nrow = nsim)
  samp.means <- rowMeans(simmat)
  neg.devs <- -abs(samp.means)
  ps <- 2*pnorm(neg.devs, 0, 1/sqrt(n))
  power <- mean(ps < lev)
  if(abs.vals == TRUE){
    ests.out <- abs(samp.means)
  }
  if(abs.vals == FALSE){
    ests.out <- samp.means
  }
  est.d <- mean(ests.out[ps < lev])
  cut.xbar.n <- qnorm(lev/2, 0, 1/sqrt(n))
  h <- hist(ests.out, breaks = br)
  cuts <- cut(h$breaks, c(-Inf, cut.xbar.n, -cut.xbar.n, Inf))
  plot(h, col = c("grey", "white", "grey")[cuts], xlab = 	"Estimated effect sizes", main = "")
  return(c("true d" = d, "estimated d" = est.d, "power" = 	power))
}


#Draw a bootstrap sample of entries from a vector or rows 
#from a matrix.
boot.samp <- function(x){
  #If x is a vector, convert it to a matrix with one column.
  if(is.null(dim(x))){
    x <- matrix(x, ncol = 1)
  }
  n <- nrow(x)
  boot.inds <- sample(1:n, replace = TRUE)
  x[boot.inds,]
}

#Compute the method of moments estimator of the slope 
#for simple linear regression. (that is, the least-squares
#slope. You can also get this with 
#lm(y ~ x)$coefficients[2], but beta.mm() is a little faster.)
#(equation 8.3).
beta.mm <- function(x, y){
  n <- length(x)
  (sum(x*y) - (1/n)*sum(x)*sum(y)) / 
    (sum(x^2) - (1/n)*sum(x)^2)
}

#Permute the columns of a matrix x independently.
perm.samp <- function(x){
  apply(x, 2, sample)
}

#function for simulating data under a linear model where y is a 
#linear function of x and (unobserved) z. n is the number of
#pairs of observations in each sample; nsims is the number
#of samples. alpha is an intercept, beta the coefficient of
#x, and gamma the coefficient of z. d.sd is the standard 
#deviation of the disturbances, and rho is the correlation
#of x and z (which are jointly normally distributed).
sim.2var <- function(n, nsims, alpha, beta, gamma, d.sd, rho){
  sig <- matrix(c(1, rho, rho, 1), nrow = 2)
  ivs <- mvrnorm(n*nsims, mu = c(0,0), sig)
  x <- ivs[,1]
  z <- ivs[,2]
  disturb <- rnorm(n*nsims, 0, d.sd)
  y <- alpha + beta * x + gamma * z + disturb
  xmat <- matrix(x, nrow = nsims)
  ymat <- matrix(y, nrow = nsims)
  cbind(xmat, ymat)
}


#Return the value of a statistic (by default, the mean),
#computed from B bootstrap samples from a vector x. 
boot.dist.1d <- function(x, B, FUN = mean, ...){
  boot.samps <- sample(x, length(x)*B, replace = TRUE)
  boot.mat <- matrix(boot.samps, ncol = B)
  apply(boot.mat, 2, FUN, ...)
}

#Simulate a sample from a normal distribution, take B bootstrap 
#samples. Plot a histogram and return the mean and standard 
#deviation of the bootstrap distribution.
wrap.bm <- function(n, B, mu = 0, sigma = 1, FUN = mean, ...){
  sim <- rnorm(n, mu, sigma)
  boots <- boot.dist.1d(sim, B, FUN = FUN, ...)
  hist(boots, main = "", xlab = "Bootstrap Means")
  list("boot m"= mean(boots), "boot se"= sd(boots))
}

#A function that calls sim.lm to simulate datasets, conducting a permutation test
#of the hypothesis of zero slope for each one and returning the p values.
#a is the true intercept, b the slope, n.perm the number of permtuations
#per simulation, n.sim the number of simulations, var.eps the variance of
#the distubances, n the sample size, mu.x and var.x the expectation and variance
#of the x variable, rdist a function for sampling the disturbances, and
#rx a function for sampling the x.
sim.perm.B <- function(a, b, n.perm = 500, n.sim = 500, var.eps = 1, 
                       n = 50, mu.x = 8, var.x = 4, rdist = rnorm, rx = rnorm){
  #Initialize variables.
  ps <- numeric(n.sim)
  perm.dist <- numeric(n.perm)
  for(i in 1:n.sim){
    #Simulate data and estimate beta.
    dat <- sim.lm(a, b, var.eps, n, mu.x, var.x, rdist = rdist, rx = rx)
    beta.orig <- beta.mm(dat[,1] , dat[,2])
    #Compute a permutation distribution.
    for(j in 1:n.perm){
      p.dat <- perm.samp(dat)
      perm.dist[j] <- beta.mm(p.dat[,1] , p.dat[,2])
    }
    #Permutation p value
    ps[i] <- mean(abs(perm.dist) > abs(beta.orig))
  }
  #Return the p-values
  ps
}


#Function to compute Wald statistic for slope in simple
#linear regression.
#wald.stat.slr <- function(x, y, B0 = 0){
#  n <- length(x)
#  #compute MLEs of beta and alpha
#  B.hat <- (sum(x*y)-sum(x)*sum(y)/n)/( sum(x^2) - sum(x)^2/n)
#  A.hat <- (sum(y) - B.hat*sum(x))/n
#  #Compute estimated variance of MLE of beta
#  vhat.dists <- sum((y - A.hat - B.hat*x)^2)/(n-2)
#  vhat.Bhat <- vhat.dists/sum((x - mean(x))^2)
#  #Wald statistic
#  (B.hat - B0)/sqrt(vhat.Bhat)
#}

#A function that calls sim.lm to simulate datasets, conducting a Wald test
#of the hypothesis of zero slope for each one and returning the p values.
#a is the true intercept, b the slope, n.sim the number of simulations, 
#var.eps the variance of the distubances, n the sample size, 
#mu.x and var.x the expectation and variance
#of the x variable, rdist a function for sampling the disturbances,
#rx a function for sampling the x, and pfun a cdf function for computing
#the p value.
sim.Wald.B <- function(a, b, B0 = 0, n.sim = 1000, var.eps = 1, n = 50, 
                       mu.x = 8, var.x = 4, rdist = rnorm, rx = rnorm, pfun = pnorm, ...){
  #Initialize variables.
  ps <- numeric(n.sim)
  for(i in 1:n.sim){
    #Simulate data and compute p value.
    dat <- sim.lm(a, b, var.eps, n, mu.x, var.x, rdist = rdist, rx = rx)    
    x <- dat[,1]
    y <- dat[,2]
    #compute MLEs of beta and alpha
    B.hat <- (sum(x*y)-sum(x)*sum(y)/n)/( sum(x^2) - sum(x)^2/n)
    A.hat <- (sum(y) - B.hat*sum(x))/n
    #Compute estimated variance of MLE of beta
    vhat.dists <- sum((y - A.hat - B.hat*x)^2)/(n-2)
    vhat.Bhat <- vhat.dists/sum((x - mean(x))^2)
    #Wald statistic
    wald <- (B.hat - B0)/sqrt(vhat.Bhat)
    ps[i] <- 2*pfun(-abs(wald), ...)
  }
  #Return the p values
  return(ps)
}


#Compute the likelihood-ratio statistic comparing a simple linear regression
#with and without a slope. x is the independent variable, y the
#dependent variable.
#lr.stat.slr <- function(x, y){
#  n <- length(x)
#  #compute MLEs of beta and alpha
#  B.hat <- (sum(x*y)-sum(x)*sum(y)/n)/( sum(x^2) - sum(x)^2/n)
#  A.hat <- (sum(y) - B.hat*sum(x))/n
#  #Compute estimated variance of MLE of beta
#  vhat <- sum((y - A.hat - B.hat*x)^2)/(n-2)
#  #likelihood-ratio statistic
#  (sum((y - mean(y))^2) - sum((y - A.hat - B.hat*x)^2)) /vhat
#}


#Compute conjugate posterior expectation and variance for the first parameter 
#of a normal distribution with known standard deviation
#using a normal prior. z is a vector of data. (equation 10.4-10.5)
post.conj.norm.norm <- function(z, known.sd, prior.mean, prior.sd){
  xbar <- mean(z)
  post.expec <- (prior.mean / prior.sd^2 + xbar*length(z) / 
    known.sd^2)/(1 /   prior.sd^2 + length(z) / known.sd^2)
  post.var <- 1 / (1 /   prior.sd^2 + length(z) / known.sd^2)
  list("posterior.expectation" = post.expec, "posterior.variance" = post.var)
}


#Wrapper for get.1.samp.norm() that gets rejection sample of desired size from posterior.
reject.samp.norm <- function(z, known.sd = 1, prior.mn = 0, prior.sd = 1, nsamps = 10000){
  #Get 1 sample under rejection sampling from normal with known sd.
  #Prior is a normal.
  #z is a vector of data.
  get.1.samp.norm <- function(z, known.sd = 1, prior.mn = 0, prior.sd = 1){
    accepted <- FALSE
    max.like <- exp(sum(log(dnorm(z, mean = mean(z), sd = known.sd))))
    while(accepted == FALSE){
      cand <- rnorm(1, prior.mn, prior.sd)
      like <- exp(sum(log(dnorm(z, mean = cand, sd = known.sd))))
      crit <- like / max.like
      xunif <- runif(1,0,1)
      if(xunif <= crit){accepted <- TRUE}
    }
    cand
  }
  samps <- numeric(nsamps)
  for(i in seq_along(samps)){
    samps[i] <- get.1.samp.norm(z, known.sd, prior.mn, prior.sd)
  }
  samps
}

