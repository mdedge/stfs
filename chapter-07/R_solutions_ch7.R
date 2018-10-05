#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 7.

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

##############################
##############################
##############################
#Exercise Set 7-1
#Problem 1)
#d) 
s.mat <- mat.samps(n = 25, nsim =10000)
ests.median <- apply(s.mat, 1, median)

#Heres the new part:
sd(ests.median)
#You will get an answer of approximately 0.25, which is 
#larger than the standard error of the sample mean.

#2) a) 
pnorm(1)-pnorm(-1)
#b)
pnorm(2)-pnorm(-2)

##############################
##############################
##############################
#Exercise Set 7-2

#Problem 1)
#a)
qnorm(.75)
#gives the answer.
#A picture of the problem:
a <- 0.5 #parameter: 1 minus the confidence level.
#plot a standard normal distribution
x <- seq(-3.5, 3.5, length.out = 10000)
z.a2 <- qnorm(1 - a/2)
fx <- dnorm(x)
plot(x, fx, type = "l")

#Shade in the appropriate area
x.zs <- seq(-z.a2, z.a2, length.out = 10000)
fx.zs <- dnorm(x.zs)
polygon(c(-z.a2, x.zs, z.a2), c(0, fx.zs, 0), col = "grey", border = FALSE)

#f)
1 - pnorm(2.02)

##############################
##############################
##############################
#Exercise Set 7-3

#Problem 2)

#2d)

#To simulate the means of 10,000 samples of size four, we have two options. 
#We can either simulate the samples (here, using the mat.samps function from stfspack) 
#and take their means:

sim.mat <- mat.samps(n = 4, nsim = 10000, rx = rnorm, 100, 2)
sim.means <- rowMeans(sim.mat)

#Or we can simulate the means directly, remembering that the means 
#are normally distributed with expectation 100 and variance 1:

sim.means <- rnorm(10000, mean = 100, sd = 1)

#To get the distribution of ps, we could use a for() loop:

twotailed.p.normal <- function(x.bar, mu, stand.err){
  abs.diff <- abs(x.bar - mu)
  2 * pnorm(mu - abs.diff, mean = mu, sd = stand.err)
}

ps <- numeric(10000)
for(i in 1:10000){
  ps[i] <- twotailed.p.normal(sim.means[i], 100, 1)
}

#Or, even better, we could use sapply(), a version of apply() 
#that takes vectors as input:

ps <- sapply(sim.means, FUN = twotailed.p.normal, mu = 100, 	stand.err = 1)

#We can plot the distribution of the p values with 
hist(ps) 
#It is approximately uniform. To find the proportion of p values less than 0.05, use 
mean(ps < 0.05) 
#It should be approximately 0.05. Similarly, the proportion of p values 
#less than 0.10 should be about 0.10. 
mean(ps < 0.1) 
#This is a good result; it means 
#that when the null hypothesis is true, the test works approximately as advertised.

#2e) To simulate normal samples of size 4 from a NOrmal(101,4) distribution 
#and test the null hypothesis that mu=100, use the following:

sim.mat <- mat.samps(n = 4, nsim = 10000, rx = rnorm, 101, 2)
sim.means <- rowMeans(sim.mat)
ps <- sapply(sim.means, FUN = twotailed.p.normal, mu = 100, 	stand.err = 1)

hist(ps) 
mean(ps < 0.05) 
mean(ps < 0.1) 

#The distribution of p values is no longer uniform---it has a concentration of 
#low p values, representing samples that would be unlikely to be drawn 
#if µ were in fact 100. I find that about 17% of the p values are less than 
#0.05 and that about 26% are less than 0.10.

#2f) To simulate normal samples of size 4 from a NOrmal(102,4) distribution and 
#test the null hypothesis that mu=100, use the following:

sim.mat <- mat.samps(n = 4, nsim = 10000, rx = rnorm, 102, 2)
sim.means <- rowMeans(sim.mat)
ps <- sapply(sim.means, FUN = twotailed.p.normal, mu = 100, 	stand.err = 1)

hist(ps) 
mean(ps < 0.05) 
mean(ps < 0.1) 

#Again, the distribution of p values shows a concentration of low values, 
#even more pronounced than in part (e). 

#2g) To simulate normal samples of size 16 from a Normal(101,4) distribution and 
#test the null hypothesis that mu=100, use the following:

sim.mat <- mat.samps(n = 16, nsim = 10000, rx = rnorm, 101, 2)
sim.means <- rowMeans(sim.mat)
ps <- sapply(sim.means, FUN = twotailed.p.normal, mu = 100, 	stand.err = 1/2)

hist(ps) 
mean(ps < 0.05) 
mean(ps < 0.1) 

#Again, the distribution of p values shows a concentration of low values, 
#even more pronounced than in part (e). 
#Notice that doubling the difference between the true mean and the mean 
#under the null hypothesis had the same effect on the distribution of 
#p values as quadrupling the sample size. There is a good reason for 
#this---both changes have the effect of doubling the number of standard errors 
#separating the true parameter from the value postulated by the null hypothesis.

##############################
##############################
##############################
#Exercise Set 7-5

#Problem 1)
#a)

library(MASS)

ps <- many.outcome.sim(n = 20, nsim = 10000, n.dv = 7, correl = .7)
sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant for each measurement.
mean(rowMeans(sigs) > 0) #significant result for at least one of the measurements.

#b) Here are a couple of other possibilities:

ps <- many.outcome.sim(n = 20, nsim = 10000, n.dv = 20, correl = .7)
sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant for each measurement.
mean(rowMeans(sigs) > 0) #significant result for at least one of the measurements.

ps <- many.outcome.sim(n = 20, nsim = 10000, n.dv = 7, correl = .1)
sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant for each measurement.
mean(rowMeans(sigs) > 0) #significant result for at least one of the measurements.

#Increasing the number of tests or lowering the correlation between them increases
#the probability that at least one of the tests will produce a type I error,
#also called the familywise error rate.

#Problem 2)

ps <- serial.testing.sim(ns = c(10, 20, 30, 40, 50))

sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant at each stopping point.
mean(rowMeans(sigs) > 0) #significant result for at least one stopping point.

ps <- serial.testing.sim(ns = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant at each stopping point.
mean(rowMeans(sigs) > 0) #significant result for at least one stopping point.


#The proposed procedure leads to an incorrect rejection of the null 
#hypothesis about 11-12% of the time, which grows worse with more repeated testing. 


##############################
##############################
##############################
#Exercise set 7-6

#1)

#The following block of code provides one way to produce the necessary plot, 
#assuming that the power.sim.1sz() function is defined:

n <- 25
d <- seq(-2, 2, length.out = 101)
pow <- numeric(length(d))
for(i in 1:length(d)){
  pow[i] <- power.sim.1sz(n = n, nsim = 1000, d[i])
}
plot(d, pow, ylim = c(0,1), type = "l", ylab = "Power")

#2)
# few possible parameter choices.
wincurse.sim.1sz(n = 50, nsim = 10000, d = .3)
wincurse.sim.1sz(n = 100, nsim = 10000, d = .3)
wincurse.sim.1sz(n = 25, nsim = 10000, d = .3)
wincurse.sim.1sz(n = 50, nsim = 10000, d = .1)
wincurse.sim.1sz(n = 50, nsim = 10000, d = .3, lev = .01)

#b)

true.d <- 0.3
ns <- seq(5, 200, by = 5)
pows <- numeric(length(ns))
est.ds <- numeric(length(ns))

#Save power and estimated effect sizes.
for(i in 1:length(ns)){
  wc <- wincurse.sim.1sz(n = ns[i], nsim = 10000, d = true.d)
  est.ds[i] <- wc[2]
  pows[i] <- wc[3]
}

#First Plot: Cursed effect size estimate as a function of sample #size.
plot(ns, est.ds, type = "l", lty = 2, lwd = 2, ylim = c(0, max(est.ds)), ylab = "d", xlab = "n")
lines(ns, rep(true.d, length(ns)), lwd = 2)
legend("topright", lwd = c(2,2), lty = c(2,1), legend = c("Cursed", "True"))

#Second Plot: Size of the winner's curse effect as a function of 
#power.
curse.size <- est.ds - true.d
plot(pows, curse.size, type = "l", lwd = 2, xlab = "Power", ylab = "Size of Winner's Curse Effect")
