#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 5.

#Some of the solutions below require functions that are contained
#in the book's package. Code for the functions is at
#github.com/mdedge/stfs
#To install and load the package stfspack, run the following:

#First, get devtools, which allows installation from github
if(!("devtools" %in% installed.packages())){install.packages("devtools")}
library(devtools) 
#Next, install and load the package.
if(!("stfspack" %in% installed.packages())){install_github("mdedge/stfspack")}
library(stfspack)

#############################################
#Exercise Set 5-1
#Problem 2

#a)
#Simulate n.samps samples of size samp.size from a normal distribution
#and make a histogram of their means
samp.size <- 20 #sample size
n.samps <- 1000 #number of samples to simulate
samps <- rnorm(samp.size*n.samps, mean = 0, sd = 1) #draw samples
samp.mat <- matrix(samps, ncol = n.samps) #organize samples into a matrix
samp.means <- colMeans(samp.mat) #take the mean of each column
hist(samp.means)

#The mat.samps() function in stfspack also does the step of reformatting
#as a matrix.

#If you increase the samp.size variable from 20 to larger numbers,
#you will see that the means become more tightly clustered
#around the expectation, which is zero (pay attention to the x-axis
#range).

#You could do this same thing with a for() loop like so
samp.size <- 20 #sample size
n.samps <- 1000 #number of samples to simulate
samp.means <- numeric(n.samps)
for(i in 1:n.samps){
  samp.means[i] <- mean(rnorm(samp.size, 0, 1))
}
hist(samp.means)

#You could also make a basic function to save code on the whole set of 
#requested parameters:

lln.hist <- function(samp.size, n.samps){
  samps <- rnorm(samp.size*n.samps, mean = 0, sd = 1) #draw samples
  samp.mat <- matrix(samps, ncol = n.samps) #organize samples into a matrix
  samp.means <- colMeans(samp.mat) #take the mean of each column
  hist(samp.means)
}

lln.hist(1, 1000)
lln.hist(5, 1000)
lln.hist(20, 1000)
lln.hist(100, 1000)
lln.hist(1000, 1000)

#b) Here is a modified version for the exponential distribution

lln.hist.exp <- function(samp.size, n.samps){
  samps <- rexp(samp.size*n.samps, 1) #draw samples
  samp.mat <- matrix(samps, ncol = n.samps) #organize samples into a matrix
  samp.means <- colMeans(samp.mat) #take the mean of each column
  hist(samp.means)
}

lln.hist.exp(1, 1000)
lln.hist.exp(5, 1000)
lln.hist.exp(20, 1000)
lln.hist.exp(100, 1000)
lln.hist.exp(1000, 1000)

#The shape of the exponential density is markedly different from the shape of 
#the normal distribution. No observations smaller than 0 are allowed. When 
#the expected value is set to 1, most of the observations are near 0, and 
#the observations trail off far to the right. We say the distribution is 
#skewed right. Again, when we take means of samples, we find that the 
#sample means get closer to the expectation as the sample size increases. 
#You ought to notice something odd here, though. The sample means cluster 
#more tightly around the expectation as the sample size grows, but the 
#shape of the distribution also changes. Namely, it starts to look more 
#symmetric and bell-like, more normal. This is a preview of the central 
#limit theorem, which will appear soon.


##########################
#Exercise Set 5-4

#1)
#Run the Galton Board / quincunx animation

if(!("animation" %in% installed.packages())){
  install.packages("animation")
}
library(animation)

nball <- 100 #change the number of balls
nlayer <- 25 #change the number of rows of pegs on the board
rate <- 10   #change the speed at which the balls fall
ani.options(nmax = nball + nlayer - 2, interval = 1/rate)
#Uncomment the next line to run the animation
#quincunx(balls = nball, layers = nlayer)

# Problem 2) 

#Here's a set of plots for the parameter set (1,1)
dosm.beta.hist(1, 10000, shape1 = 1, shape2 =  1)
dosm.beta.hist(2, 10000, shape1 = 1, shape2 =  1)
dosm.beta.hist(3, 10000, shape1 = 1, shape2 =  1)
dosm.beta.hist(4, 10000, shape1 = 1, shape2 =  1)
dosm.beta.hist(5, 10000, shape1 = 1, shape2 =  1)
dosm.beta.hist(10, 10000, shape1 = 1, shape2 =  1)
dosm.beta.hist(50, 10000, shape1 = 1, shape2 =  1)

#Here's a set of plots for the parameter set (.2,.2). Modify
#by changing the values of s.pars and rerunning the plots.
s.pars <- c(.2,.2)
dosm.beta.hist(1, 10000, shape1 = s.pars[1], shape2 =  s.pars[2])
dosm.beta.hist(2, 10000, shape1 = s.pars[1], shape2 =  s.pars[2])
dosm.beta.hist(3, 10000, shape1 = s.pars[1], shape2 =  s.pars[2])
dosm.beta.hist(4, 10000, shape1 = s.pars[1], shape2 =  s.pars[2])
dosm.beta.hist(5, 10000, shape1 = s.pars[1], shape2 =  s.pars[2])
dosm.beta.hist(10, 10000, shape1 = s.pars[1], shape2 =  s.pars[2])
dosm.beta.hist(50, 10000, shape1 = s.pars[1], shape2 =  s.pars[2])

#If the distribution starts out close to normal, it takes less time to converge to
#approximate normality.

#Problem 3)

#Below is commented code that performs the simulations and makes the comparisons. 
#With the parameters and sample size requested, the distribution of sample means 
#is a good fit to the normal only within about 2 standard deviations of the 
#expectation. Beyond that, the Pareto sample mean distribution has much heavier 
#tails than the normal—extreme observations are much more likely than normal 
#theory predicts. For example, there are about 100 times as many observations 
#beyond 5 standard deviations from the expectation as would be predicted by the 
#normal distribution, and there are thousands of times as many observations 
#beyond 6 standard deviations as the normal distribution predicts. Thus, 
#with this distribution and n=1,000, convergence in the center of the distribution 
#is good, but convergence in the tails is poor. If the probability of an extreme 
#event (such as, say, an earthquake of Richter magnitude >8) is important to know, 
#then the central limit theorem can lead to spectacularly poor predictions. 
#Convergence is worse with smaller shape parameters and smaller sample size.

#Sample size per simulation (n) and number of simulations.
n <- 1000
n.sim <- 100000

#Pareto parameters. Variance is finite, and so
#CLT applies, if a > 2. For large a, convergence to 
#normal is better. With small a, convergence is slow,
#especially in the tails.
a <- 4
b <- 1

#Compute the expectation and variance of the distribution
#of the sample mean. a must be above 2 for these expressions
#to hold.
expec.par <- a*b/(a-1)
var.par <- a*b^2 / ((a-1)^2 * (a-2))
sd.mean <- sqrt(var.par / n)

#Simulate data, compute sample means.
sim <- matrix(rpareto(n*n.sim, a, b), nrow = n.sim)
means.sim <- rowMeans(sim)

#Draw a histogram of the sample means along with the approximate
#normal pdf that follows from the CLT.
hist(means.sim, prob = TRUE)
curve(dnorm(x, expec.par, sd.mean), add = TRUE)

compare.tail.to.normal(means.sim, 1/2, expec.par, sd.mean)
compare.tail.to.normal(means.sim, 1, expec.par, sd.mean)
compare.tail.to.normal(means.sim, 2, expec.par, sd.mean)
compare.tail.to.normal(means.sim, 3, expec.par, sd.mean)
compare.tail.to.normal(means.sim, 4, expec.par, sd.mean)
compare.tail.to.normal(means.sim, 5, expec.par, sd.mean)
compare.tail.to.normal(means.sim, 6, expec.par, sd.mean)


########################################
#Exercise Set 5-5

#Problem 2)

#Simulates a version of the linear model from the end of chapter 5.
#x is drawn from a normal distribution with mean mu.x and variance var.x.
#a is the intercept.
#b is the slope.
#var.eps is the variance of the disturbances, which are drawn from a
#normal distribution with expectation 0.
sim <- sim.lm(n = 100, a = 3, b = .5)
plot(sim)
abline(3, 1/2) #draw the E(Y|x) line

#Draw disturbances from a Laplace distribution (if stfspack is loaded)
sim <- sim.lm(n = 100, a = 3, b = .5, rdist = rlaplace)
plot(sim)
abline(3, 1/2) #draw the E(Y|x) line

#Make the standard deviation of the disturbances increase linearly with x
sim <- sim.lm(n = 100, a = 3, b = .5, het.coef = .2)
plot(sim)
abline(3, 1/2) #draw the E(Y|x) line

