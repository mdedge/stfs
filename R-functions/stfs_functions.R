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



#By M.D. Edge, 9/19/2018.
#Functions used in the main text or exercises of
#Statistical Thinking from Scratch.


#' Simulate and plot the distribution of the sample mean
#' 
#' Simulates a specified number of samples of a specified size from the beta distribution and plots the distribution of sample means with the density of an approximating normal distribution overlaid.
#' @param n The size of each simulated sample.
#' @param nsim The number of samples to simulate.
#' @param shape1 The first parameter of the beta distribution from which the samples will be drawn.
#' @param shape2 The second parameter of the beta distribution.
#' @return A named vector with the mean, standard deviation, and variance of the distribution of sample means. Also plots a histogram of the distribution of sample means with an approximating normal distribution overlaid.
#' @keywords sample mean, law of large numbers, central limit theorem, distribution of the sample mean
#' @export
#' @examples 
#' dosm.beta.hist(25, 1000, 2, 2) 
dosm.beta.hist <- function(n, nsim, shape1 = 1, shape2 = 1, ...){
  samps <- rbeta(n*nsim, shape1, shape2)
  sim.mat <- matrix(samps, nrow = nsim)
  dosm <- rowMeans(sim.mat)
  hist(dosm, freq=FALSE, ...)#freq=FALSE re-scales the height
  x <- seq(0,1,length.out = 1000)
  lines(x, dnorm(x, mean = mean(dosm), sd = sd(dosm)))
  c("mean of DOSM" = mean(dosm), "SD of DOSM" = sd(dosm), "var of DOSM" = var(dosm))
}


#' Sample from a Pareto distribution
#' 
#' Simulates a sample of Pareto-distributed data.
#' @param n The number of independent observations to draw (i.e. sample size).
#' @param a The shape parameter of the Pareto distribution.
#' @param b The scale parameter of the Pareto distribution.
#' @return A vector of Pareto-distributed random numbers of length n.
#' @keywords simulated data, Pareto
#' @export
#' @examples 
#' rpareto(50, 0.5, 1)
rpareto <- function(n, a=0.5, b=1){
  qpareto <- function(u, a=0.5, b=1){b/(1-u)^(1/a)}
  qpareto(runif(n),a,b)
}



#' Compare tails of a sample to normal expectation.
#' 
#' Compares the proportion of observations in a vector beyond k standard deviations to the expectation under a normal distribution. The output is a ratio of the proportion of data beyond k standard deviations from the expectation divided by the Normal-theory probability of observations beyond k standard deviations from the expectation.
#' @param x A numeric vector
#' @param k The number of standard deviations beyond which is considered the "tail" (for the purpose of the computation)
#' @param mu The expectation of a normal distribution to which x will be compared
#' @param sigma The standard deviation of a normal distribution to which x will be compared
#' @return A ratio of the proportion of data beyond k standard deviations from the expectation divided by the Normal-theory probability of observations beyond k standard deviations from the expectation.
#' @keywords extreme observations, central limit theorem
#' @export
#' @examples 
#' compare.tail.to.normal(rnorm(10000, 0, 1), 3, 0, 1)
#' compare.tail.to.normal(rt(10000, 2), 3, 0, 1)
compare.tail.to.normal <- function(x, k, mu, sigma){
  mean(x < (mu - k* sigma) | x > (mu + k* sigma))/(1 - (pnorm(k) - pnorm(-k)))
}

#' Simulate data from a simple linear model.
#' 
#' Generates one set of n independent pairs of observations using a simple linear model (y = a + b*x + disturbance) with intercept a, slope b, and (by default) normally distributed disturbances. The output is a matrix with x in the first column and y in the second column. The output matrix is sorted by the x values, with the lowest x value in the first row.
#' @param n The number of pairs of observations to simulate.
#' @param a The intercept parameter of the linear regression model to be simulated.
#' @param b The slope parameter of the linear regression model to be simulated.
#' @param sigma.disturb The variance of the disturbances in the model y = a + b*x + disturbance.
#' @param mu.x The expectation of the x values.
#' @param sigma.x The variance of the x values.
#' @param rdisturb A function for drawing the random disturbances. rnorm() is the default, which makes the disturbances normally distributed, but you can use any function for random number generation with first argument the sample size, second argument the expectation, and third argument the standard deviation.
#' @param rx A function for drawing the random x values. rnorm() is the default, which makes x normally distributed, but you can use any function for random number generation with first argument the sample size, second argument the expectation, and third argument the standard deviation.
#' @param het.coef A number introducing some heteroscedasticity (i.e. non-constant variance) to the disturbances. If het.coef = 0 (the default), then the disturbances have constant variance. If it is positive, then the standard deviation of the disturbances increases with x; if negative, then the standard deviation of the disturbances decreases with x.
#' @return A matrix with n rows, x in the first column and y in the second column. The output matrix is sorted by the x values, with the lowest x value in the first row.
#' @keywords Simulation, simple linear regression
#' @export
#' @examples 
#' sim.lm(n = 11, a = 3, b = 1/2)
#' plot(sim.lm(1000, 3, 1/2))
#' plot(sim.lm(1000, 3, 1/2, rdisturb = rlaplace))
#' plot(sim.lm(1000, 3, 1/2, het.coef = .2))
sim.lm <- function(n, a, b, sigma.disturb = 1, mu.x = 8, sigma.x = 2, rdisturb = rnorm, rx = rnorm, het.coef = 0){
  x <- sort(rx(n, mu.x, sigma.x))
  disturbs <- rdisturb(n, 0, sapply(sigma.disturb + scale(x)*het.coef, max, 0))
  y <- a + b*x + disturbs
  cbind(x,y)
}


#' Generate a matrix of samples from a chosen distribution.
#' 
#' Draws random data and formats them into a matrix, where each row contains a sample. By default, the random data are drawn form a normal distribution, but the user can supply an alternative.
#' @param n The number of independent observations to include in each sample (i.e. each row of the output matrix).
#' @param nsim The number of samples to generate (i.e. the number of rows in the output matrix).
#' @param rx A function generating random data (rnorm by default).
#' @param ... Additional arguments to rx.
#' @return A matrix of independent random numbers with nsim rows and n columns.
#' @keywords simulation, samples, matrix
#' @export
#' @examples 
#' mat.samps(3, 5) #data from standard normal
#' mat.samps(3, 5, mean = 10, sd = 2) #normal with expectation 10 and sd 2.
#' mat.samps(3, 5, rx = rexp, .1) #exponential with rate .1
#' mat.samps(3, 5, rx = rnorm.contam, contam.p = .2) #standard normal contaminated with 20% outliers
mat.samps <- function(n, nsim = 10000, rx = rnorm, ...){
  samps <- rx(n*nsim, ...)
  matrix(sample(samps), nrow = nsim, ncol = n) #sample() is there because rnorm.out produces
}


#' Sample from a Laplace (also called double-exponential) distribution
#' 
#' Simulates a sample of Laplace-distributed data. Note that there are other rlaplace() functions (such as in the rmutil package), but their parameters may differ from this one, which uses the expectation and standard deviation (analogous to rnorm()).
#' @param n The number of independent observations to draw (i.e. sample size).
#' @param mu The expectation of the Laplace distribution.
#' @param sigma The standard deviation of the Laplace distribution.
#' @return A vector of Laplace-distributed random numbers of length n.
#' @keywords simulated data, Laplace, double-exponential
#' @export
#' @examples 
#' rlaplace(10, 0, 1)
rlaplace <- function(n, mu = 0, sigma = 1){
  exp1 <- rexp(2*n, 1)
  x <- exp1[1:n] - exp1[(n+1):(2*n)]
  x * sigma/sqrt(2) + mu
}


#' Sample from a Normal distribution with outliers
#' 
#' Simulates a samples of normally distributed data, each "contaminated" with outliers from a different normal distribution according to a user-specified probability. Similar to rnorm.out, except that the order of arguments is different and the output is formatted as a vector.
#' @param n The number of independent observations per sample
#' @param mu The expectation of the target distribution.
#' @param sigma The standard deviation of the target distribution.
#' @param contam.p The probability that each observation is from the non-target distribution.
#' @param contam.mu The expectation of the non-target / "contamination" / outlier distribution.
#' @param contam.sigma The standard deviation of the outlier distribution.
#' @return A vector of independent random numbers n entries.
#' @keywords simulated data, normal distribution, outliers
#' @export
#' @examples 
#' rnorm.mix(20, 0, 1, .2, 10, 1)
rnorm.contam <- function(n, mu = 0, sigma = 1, contam.p = 0.01, contam.mu = -5, contam.sigma = 0){
  ncontam <- rbinom(1, n, contam.p)
  c(rnorm(n - ncontam, mu, sigma), rnorm(ncontam, contam.mu, contam.sigma))
}



#' Simulate data from a linear model and estimate the parameters.
#' 
#' Simulates data from a linear model using the sim.lm() function, each time estimating the parameters using a user-supplied function.
#' @inheritParams sim.lm 
#' @param nsim The number of samples to simulate.
#' @param estfun A function for estimating the parameters. lm() is the default, which gives least-squares estimates. Other functions can be supplied so long as they accept the same formula-style input in the first argument and return a list that has the coefficients stored in a vector of length 2 accessible by $coef. rq() in the quantreg package is another function that works.
#' @return A matrix with 2 columns and nsamps rows. Each row contains estimation results from a different sample, with the intercept estimate in the first column and the slope estimate in the second column.
#' @keywords simulated data, simple linear regression
#' @export
#' @examples 
#' sim.lm.ests(n = 50, nsim = 10, a = 3, b = 1/2)
#' sim.lm.ests(n = 50, nsim = 10, a = 3, b = 1/2, estfun = rq) #if package quantreg is loaded.
sim.lm.ests <- function(n, nsim, a, b, sigma.disturb = 1, mu.x = 8, sigma.x = 2, rdisturb = rnorm, rx = rnorm, het.coef = 0, estfun = lm){
  ests <- matrix(nrow = nsim, ncol = 2)
  for(i in 1:nsim){
    dat <- sim.lm(n, a, b, sigma.disturb, mu.x, sigma.x, rdisturb, rx, het.coef)
    ests[i,] <- estfun(dat[,2] ~ dat[,1])$coef
  }
  ests
}



#' Simulate a multiple-testing scenario
#' 
#' Simulates type I error inflation from multiple testing for a test comparing two group means. The groups are compared on n.dv outcome measurements, each mutually correlated at a user-specified level. A matrix of p values is returned with n.dv columns and nsim rows.
#' @param n The number of people measured in each group per simulation.
#' @param nsim The number of simulated samples to run.
#' @param n.dv The number of outcome measurements (i.e. dependent variables or DVs) taken per person.
#' @param correl The correlation of the (normally distributed) outcome measurements---all pairs of measurements have the same correlation.
#' @return A matrix of p values with n.dv columns and nsim rows. Each row corresponds to a different sample, and the p values from the respective outcome measures are in distinct columns.
#' @keywords simulated data, multiple testing, p hacking
#' @export
#' @examples 
#' sims <- many.outcome.sim(100, nsim = 1000, n.dv = 5, correl = .4)
#' mean(apply(sims, 1, min) < .05) #familywise error rate with no correction
#' mean(apply(sims, 1, min) < .05/5) #familywise error rate with Bonferonni correction for 5 comparisons
many.outcome.sim <- function(n, nsim, n.dv = 3, correl = .5){
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
  sigma <- matrix(correl, nrow = n.dv, ncol = n.dv)
  diag(sigma) <- 1
  #Initialize a matrix for holding p values.
  sim.ps <- matrix(-9, nrow = nsim, ncol = n.dv)
  #simulate all the necessary observations.
  sim.dat <- MASS::mvrnorm(nsim*n*2, mu = rep(0, n.dv), Sigma = sigma)
  ps.mat <- matrix(NA, nrow = nsim, ncol = n.dv)
  #For each measurement, conduct hypothesis tests for each sample.
  for(i in 1:n.dv){
    test.dat <- matrix(sim.dat[,i], nrow = nsim)
    ps.mat[,i] <- apply(test.dat, 1, t.p)
  }
  ps.mat
}


#' Simulate an optional stopping p-value scenario
#' 
#' Simulates type I error inflation from testing a difference in group means many times as the trial is ongoing, stopping if the test is significant. A matrix of p values is returned with length(ns) columns and nsim rows.
#' @param ns A vector of numbers, each of which is a number of people per group at which the group difference should be tested.
#' @param nsim The number of simulated samples to run.
#' @return A matrix of p values with length(ns) columns and nsim rows. Each row corresponds to a different sample, and the p values resulting from tests of the nested subsamples of size specified by ns are in different columns.
#' @keywords simulated data, p hacking
#' @export
#' @examples 
#' sims <- serial.testing.sim(nsim = 1000)
#' mean(apply(sims, 1, min) < .05) #How often would optional stopping lead to a rejection of the null at the .05 level?
serial.testing.sim <- function(ns = c(20, 30, 40, 50), nsim = 10000){
  t.p <- function(x){
    n.tot <- length(x)
    n.1 <- n.tot/2
    x1 <- x[1:n.1]
    x2 <- x[(n.1+1):n.tot]
    t.st <- (mean(x1) - mean(x2))/sqrt((sd(x1)^2 + sd(x2)^2)/n.1)
    2*pt(-abs(t.st), n.tot - 2)
  }
  #Initialize a matrix for holding p values.
  sim.ps <- matrix(-9, nrow = nsim, ncol = length(ns))
  #simulate all the necessary observations.
  sim.dat <- matrix(rnorm(nsim*max(ns)*2, 0, 1), nrow = nsim)
  ps.mat <- matrix(NA, nrow = nsim, ncol = length(ns))
  max.n <- max(ns)
  #For each measurement, conduct hypothesis tests for each sample.
  for(i in 1:length(ns)){
    test.dat <- sim.dat[,c(1:ns[i], (max.n + 1):(max.n + ns[i]))]
    ps.mat[,i] <- apply(test.dat, 1, t.p)
  }
  ps.mat
}


#' Estimate the power of a one-sample z test by simulation
#' 
#' Simulates nsims normal samples with expectations differing from a value specified by the null hypothesis by d (known) standard deviations. For each sample, the null hypothesis is tested by a one-sample z test, and the p value is compared with the specified significance level.
#' @param n The size of each simulated sample.
#' @param nsim The number of simulated samples to run.
#' @param d An effect size: the number of (known) standard deviations by which the expectation of the sampled data differs from a hypothesized expectation.
#' @param lev The significance level of the one-sample z-test.
#' @return The proportion of p values less than the specified level (i.e. the estimated power).
#' @keywords simulation, power, z test
#' @export
#' @examples 
#' powersim.1sz(n = 20, nsim = 10000, d = .5)
power.sim.1sz <- function(n, nsim, d, lev = 0.05){
  simmat <- matrix(rnorm(n*nsim, d, 1), nrow = nsim)
  samp.means <- rowMeans(simmat)
  neg.devs <- -abs(samp.means)
  ps <- 2*pnorm(neg.devs, 0, 1/sqrt(n))
  mean(ps < lev)
}


#' Simulate the winner's curse with a one-sample z-test
#' 
#' Simulates the "winner's curse"---the effect that especially in low-power situations, estimates that result in signficant tests for the null hypothesis of theta = 0 tend also to be overestimates. In each simulation, a single normal sample is drawn and the hypothesis that the true effect size is zero is tested(by one-sample t-test). The output is a named vector with a a true effect size (measured in number of standard deviations from the value under the null hypothesis), the "estimated" effect size, which is the mean effect size from simulations that produced significant results, and the power, which is the proportion of simulations that achieved significance. A histogram is also produced, with all the estimated effect sizes shown, and the estimates associated with significant tests colored grey.
#' @param n The size of each simulated sample.
#' @param nsim The number of simulated samples to run.
#' @param d The true effect size in the simulations.
#' @param lev The significance level of the one-sample z-test. Effect size estimates are averaged in the "estimated d" part of the output only if the one-sample z test produces a p value less than the level.
#' @param abs.vals controls whether we pay attention to the sign of the estimate (if FALSE, the default, we do).
#' @param br the breaks parameter for the histogram.
#' @return a named vector with a a true effect size (measured in number of standard deviations from the value under the null hypothesis), the "estimated" effect size, which is the mean effect size from simulations that produced significant results, and the power, which is the proportion of simulations that achieved significance. A histogram is also produced, with all the estimated effect sizes shown, and the estimates associated with significant tests colored grey.
#' @keywords simulation, power, z test, winner's curse
#' @export
#' @examples 
#' wincurse.sim.1sz(d = 0.1, n = 100, nsim = 10000)
#' wincurse.sim.1sz(d = 0.1, n = 100, nsim = 10000, abs.vals = TRUE)
wincurse.sim.1sz <- function(n, nsim, d, lev = 0.05, abs.vals = FALSE, br = 50){
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
  plot(h, col = c("grey", "white", "grey")[cuts], xlab = "Estimated effect sizes", main = "")
  return(c("true d" = d, "estimated d" = est.d, "power" = power))
}

#' Draw a bootstrap sample from a matrix or vector
#' 
#' Given a matrix with n rows (or a vector of length n), samples n rows with replacement from the original matrix.
#' @param x A matrix or vector.
#' @return A matrix containing a bootstrap sample of the original matrix or vector.
#' @keywords Bootstrapping
#' @export
#' @examples 
#' boot.samp(rnorm(10))
boot.samp <- function(x){
  #If x is a vector, convert it to a matrix with one column.
  if(is.null(dim(x))){
    x <- matrix(x, ncol = 1)
  }
  n <- nrow(x)
  boot.inds <- sample(1:n, replace = TRUE)
  x[boot.inds,]
}

#' Compute the least-squares slope estimate for simple linear regression.
#' 
#' Given two numeric vectors, computes a least-squares slope estimate for the model y = a + b*x + disturbance. lm() also returns this number, but beta.mm() is faster, which is advantageous for simulating bootstrapping or permutation testing.
#' @param x A numeric vector containing entries for the independent variable.
#' @param y A numeric vector containing entries for the dependent variable, must be the same length as x.
#' @return The least-squares slope estimate (i.e. the estimate of b) for the model y = a + b*x + disturbance.
#' @keywords least-squares, simple linear regression, method of moments
#' @export
#' @examples 
#' beta.mm(rnorm(10), rnorm(10))
beta.mm <- function(x, y){
  n <- length(x)
  (sum(x*y) - (1/n)*sum(x)*sum(y)) / 
    (sum(x^2) - (1/n)*sum(x)^2)
}


#' Randomly permute the columns of a matrix independently
#' 
#' Given a matrix, return a version with its columns randomly and independently permuted.
#' @param x A matrix.
#' @return A matrix resulting from randomly permuting the columns of x independently.
#' @keywords permutation
#' @export
#' @examples 
#' perm.samp(matrix(1:25, nrow = 5))
perm.samp <- function(x){
  apply(x, 2, sample)
}


#' Simulate data from a a linear model with an unobserved confounder.
#' 
#' Generates nsim samples of n independent pairs of observations using a linear model y = a + b1*x + b2*z + disturbance where z is unobserved. x and z are jointly normally distributed, and the disturbances are also normal.
#' @param n The number of independent pairs of observations to generate per simulation.
#' @param nsim The number of simulations to run.
#' @param a The intercept.
#' @param b1 The slope for the observed independent variable.
#' @param b2 The slope for the unobserved confounder.
#' @param sigma.disturb The standard deviation of the disturbances.
#' @param correl The correlation of (observed) x and (unobserved) z.
#' @return A list of two matrices. The first matrix contains the x values from each simulation, with one simuluation in each row. The second matrix contains the y values in the same configuration.
#' @keywords Simulation, simple linear regression, confounding, omitted variable bias.
#' @export
#' @examples
#' sim.2var(10, 5, a = 3, b1 = 1/2, b2 = 1/5, sigma.disturb = 1, correl = .5)
sim.2var <- function(n, nsim, a, b1, b2 = 0, sigma.disturb = 1, correl = 0){
  sig <- matrix(c(1, correl, correl, 1), nrow = 2)
  ivs <- MASS::mvrnorm(n*nsim, mu = c(0,0), sig)
  x <- ivs[,1]
  z <- ivs[,2]
  disturb <- rnorm(n*nsim, 0, sigma.disturb)
  y <- a + b1 * x + b2 * z + disturb
  xmat <- matrix(x, nrow = nsim)
  ymat <- matrix(y, nrow = nsim)
  list(xmat, ymat)
}


#' Generate a bootstrap distribution for a statistic computed on a vector.
#' 
#' Given data x, generates a bootstrap distribution with B bootstrap samples, computing a statistic speficied by FUN each time.
#' @param x A numeric vector of data.
#' @param B The number of bootstrap samples to draw.
#' @param FUN A function that computes the statistic of interest. FUN must have a vector of data as its first argument and must return a numeric vector of length 1 (i.e. a scalar).
#' @param ... Additional arguments passed to FUN.
#' @return A numeric vector containing the bootstrap distribution of the statistic specified by FUN.
#' @keywords Bootstrapping, Bootstrap distribution
#' @export
#' @examples 
#' boot.dist.1d(rnorm(50, 0, 1), 1000, median)
boot.dist.1d <- function(x, B, FUN = mean, ...){
  boot.samps <- sample(x, length(x)*B, replace = TRUE)
  boot.mat <- matrix(boot.samps, ncol = B)
  apply(boot.mat, 2, FUN, ...)
}

#' Simulate a normal sample and compute a bootstrap distribution
#' 
#' Wraps boot.dist.1d, generating a normal sample and computing a bootstrap distribution for a statistic of interest. Plots a histogram of the bootstrap distribution and returns a list containing the mean and standard deviation of the bootstrap distribution.
#' @param n The size of the normal sample to draw.
#' @param B The number of bootstrap samples to draw from the normal sample.
#' @param mu The expectation of the normal distribution from which the data are drawn.
#' @param sigma The standard deviation of the normal distribution from which the data are drawn.
#' @param FUN A function that computes the statistic of interest. FUN must have a vector of data as its first argument and must return a numeric vector of length 1 (i.e. a scalar).
#' @param ... Additional arguments passed to FUN.
#' @return A list containing the mean and standard deviation of the bootstrap distribution.
#' @keywords Bootstrapping, Bootstrap distribution
#' @export
#' @examples 
#' wrap.bm(50, 1000, FUN = median)
wrap.bm <- function(n, B, mu = 0, sigma = 1, FUN = mean, ...){
  sim <- rnorm(n, mu, sigma)
  boots <- boot.dist.1d(sim, B, FUN = FUN, ...)
  hist(boots, main = "", xlab = "Bootstrap distribution")
  list("boot m"= mean(boots), "boot se"= sd(boots))
}





#' @inheritParams sim.lm
#' @param estfun A function for estimating the parameters. lm() is the default, which gives least-squares estimates. Other functions can be supplied so long as they accept the same formula-style input in the first argument and return a list that has the coefficients stored in a vector of length 2 accessible by $coef. rq() in the quantreg package is another function that works.
#' @return A matrix with 2 columns and nsamps rows. Each row contains estimation results from a different sample, with the intercept estimate in the first column and the slope estimate in the second column.
#' @keywords simulated data, simple linear regression
#' @export
#' @examples 
#' sim.lm.ests(n = 50, nsim = 10, a = 3, b = 1/2)
#' sim.lm.ests(n = 50, nsim = 10, a = 3, b = 1/2, estfun = rq) #if package quantreg is loaded.
sim.lm.ests <- function(n, nsim, a, b, sigma.disturb = 1, mu.x = 8, sigma.x = 2, rdisturb = rnorm, rx = rnorm, het.coef = 0, estfun = lm){
  ests <- matrix(nrow = nsim, ncol = 2)
  for(i in 1:nsim){
    dat <- sim.lm(n, a, b, sigma.disturb, mu.x, sigma.x, rdisturb, rx, het.coef)
    ests[i,] <- estfun(dat[,2] ~ dat[,1])$coef
  }
  ests
}


#' Simulate simple linear regression data and apply a permutation test
#' 
#' Simulates data from a linear model using the sim.lm() function, each time applying a permutation test to test the null hypothesis that the slope is zero.
#' @inheritParams sim.lm 
#' @param nsim The number of simulations to conduct.
#' @param nperm The number of permutations to use per simulation to conduct the permutation test.
#' @return A vector of p values of length nsims, one from each permutation test of the hypothesis that the slope is zero.
#' @keywords simulated data, simple linear regression, permutation test
#' @export
#' @examples 
#' ps <- sim.perm.B(10, 200, a = 3, b = .1, nperm = 200)
#' mean(ps < .05) #power at .05 significance level
sim.perm.B <- function(n, nsim, a, b, nperm = 500, sigma.disturb = 1, 
                           mu.x = 8, sigma.x = 2, rdisturb = rnorm, rx = rnorm, het.coef = 0){
  #This version differs from the one in the text and is about twice as fast.
  #simulate all data
  dat <- sim.lm(n*nsim, a, b, sigma.disturb, mu.x, sigma.x, rdisturb, rx, het.coef)[sample(1:(n*nsim)),]
  #reorganize for beta.mm.vec
  xs <- matrix(dat[,1], nrow = n)
  ys <- matrix(dat[,2], nrow = n)
  dat.all <- rbind(xs, ys)
  #compute all the beta estimates
  beta.mm.vec <- function(z){
    n <- length(z)/2
    x <- z[1:n]
    y <- z[(n+1):(2*n)]
    (sum(x*y) - (1/n)*sum(x)*sum(y)) / 
      (sum(x^2) - (1/n)*sum(x)^2)
  }
  betas <- apply(dat.all, 2, beta.mm.vec)
  #for each permutation, permute the y values and recompute
  #beta estimates
  perm.second.half <- function(x){
    n <- length(x)/2
    c(x[1:n], sample(x[(n+1):(2*n)]))
  }
  perm.dists <- matrix(nrow = nperm, ncol = nsim)
  for(i in 1:nperm){
    perm.all <- apply(dat.all, 2, perm.second.half)
    perm.dists[i,] <- apply(perm.all, 2, beta.mm.vec)
  }
  #compare the betas to their permutation distributions to
  #arrive at p values.
  getp <- function(x){
    mean(abs(x[2:length(x)]) > abs(x[1]))
  }
  apply(rbind(matrix(betas, nrow = 1), perm.dists), 2, getp)
}


#' Simulate simple linear regression data and apply a Wald test
#' 
#' Simulates data from a linear model using the sim.lm() function, each time applying a Wald test test the null hypothesis that the slope = B0.
#' @inheritParams sim.perm.B
#' @param B0 The value of the slope under the null hypothesis to be tested.
#' @param pfun A cumulative distribution function to which to compare the Wald statistic.
#' @param ... Additional arguments to pfun
#' @return A vector of p values of length nsims, one from each Wald test.
#' @keywords simulated data, simple linear regression, Wald
#' @export
#' @examples 
#' sim.Wald.B(10, 100, 3, .1)
sim.Wald.B <- function(n, nsim, a, b, B0 = 0, sigma.disturb = 1, 
                       mu.x = 8, sigma.x = 2, rdisturb = rnorm, rx = rnorm, het.coef = 0, pfun = pnorm, ...){
  #Initialize variables.
  ps <- numeric(nsim)
  for(i in 1:nsim){
    #Simulate data and compute p value.
    dat <- sim.lm(n, a, b, sigma.disturb, mu.x, sigma.x, rdisturb, rx, het.coef)    
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
  ps
}


#' Compute normal conjugate posterior
#' 
#' Given a sample of data from a normal distribution with known variance / standard deviation, and a normal prior for the expectation of the data, compute the parameters of the posterior for the expectation.
#' @param z A numeric vector (assumed to be drawn from a normal distribution with known variance / standard deviation)
#' @param known.sd The known standard deviation of the data distribution.
#' @param prior.mean The expectation of the normal prior.
#' @param prior.sd The standard devation of the normal prior.
#' @return A list containing the mean and standard deviation of the conjugate posterior distribution.
#' @keywords Conjugate prior, Normal, posterior.
#' @export
#' @examples 
#' post.conj.norm.norm(rnorm(100), 1, 2, 4)
post.conj.norm.norm <- function(z, known.sd, prior.mean, prior.sd){
  xbar <- mean(z)
  post.expec <- (prior.mean / prior.sd^2 + xbar*length(z) / 
                   known.sd^2)/(1 /   prior.sd^2 + length(z) / known.sd^2)
  post.var <- 1 / (1 /   prior.sd^2 + length(z) / known.sd^2)
  list("posterior.expectation" = post.expec, "posterior.variance" = post.var)
}


#' Perform rejection sampling for normally distributed data with a normal prior on the expectation.
#' 
#' Given a sample of data from a normal distribution with known variance / standard deviation, and a normal prior for the expectation of the data, draw samples from the posterior by rejection sampling.
#' @param z A numeric vector (assumed to be drawn from a normal distribution with known variance / standard deviation)
#' @param known.sd The known standard deviation of the data distribution.
#' @param prior.mean The expectation of the normal prior.
#' @param prior.sd The standard devation of the normal prior.
#' @param nsamps The number of observations to draw from the posterior by rejection sampling.
#' @return A numeric vector of length nsamps containing simulated data from the posterior distribution.
#' @keywords Rejection sampling, normal, posterior.
#' @export
#' @examples 
#' reject.samp.norm(rnorm(100), 1, 2, 4, nsamps = 50)
reject.samp.norm <- function(z, known.sd = 1, prior.mean = 0, prior.sd = 1, nsamps = 10000){
  #Get 1 sample under rejection sampling from normal with known sd.
  #Prior is a normal.
  #z is a vector of data.
  get.1.samp.norm <- function(z, known.sd = 1, prior.mean = 0, prior.sd = 1){
    accepted <- FALSE
    max.like <- exp(sum(log(dnorm(z, mean = mean(z), sd = known.sd))))
    while(accepted == FALSE){
      cand <- rnorm(1, prior.mean, prior.sd)
      like <- exp(sum(log(dnorm(z, mean = cand, sd = known.sd))))
      crit <- like / max.like
      xunif <- runif(1,0,1)
      if(xunif <= crit){accepted <- TRUE}
    }
    cand
  }
  samps <- numeric(nsamps)
  for(i in seq_along(samps)){
    samps[i] <- get.1.samp.norm(z, known.sd, prior.mean, prior.sd)
  }
  samps
}


