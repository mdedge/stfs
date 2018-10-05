#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 9.

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
#Exercise set 9-2

#Problem 1)

#c)

#Draw a sample
n <- 10
p <- 0.6
x <- rbinom(n,1,p)

#Given a vector of values for p and a vector of Bernoulli trials 
#x, this function computes the
#likelihood for each value of p.
Ln.Bern <- function(p, x){
  k <- sum(x)
  n <- length(x)
  Ln <- numeric(length(p))
  for(i in 1:length(p)){
    Ln[i] <- prod(p[i]^k * (1-p[i])^(n-k))
  }
  return(Ln)
}

#a set of values of p to plot
p <- seq(0.001, 0.999, length.out = 999)

#The likelihood.
Ln <- Ln.Bern(p, x)
#The log-likelihood
ln <- log(Ln)
plot(p, Ln, type = "l")
plot(p, ln, type = "l")

#d)
summary(x)


##############################
##############################
##############################
#Exercise set 9-5

#Problem 1)

#Function to compute Wald statistic for slope in simple
#linear regression.
wald.stat.slr <- function(x, y, B0 = 0){
  n <- length(x)
  #compute MLEs of beta and alpha
  B.hat <- (sum(x*y)-sum(x)*sum(y)/n)/( sum(x^2) - sum(x)^2/n)
  A.hat <- (sum(y) - B.hat*sum(x))/n
  #Compute estimated variance of MLE of beta
  vhat.dists <- sum((y - A.hat - B.hat*x)^2)/(n-2)
  vhat.Bhat <- vhat.dists/sum((x - mean(x))^2)
  #Wald statistic
  (B.hat - B0)/sqrt(vhat.Bhat)
}

wald.stat.slr(anscombe$x1, anscombe$y1, 0)

#lm() also computes the statistic, labeling it "t value"
summary(lm(y1 ~ x1, data = anscombe))

#Problem 3)
mean(sim.Wald.B(n = 10, nsim = 500, a = 0, b = 0) < .05)
mean(sim.Wald.B(n = 10, nsim = 500, a = 0, b = 0.1) < .05)
mean(sim.Wald.B(n = 10, nsim = 500, a = 0, b = 0.2) < .05)
mean(sim.Wald.B(n = 50, nsim = 500, a = 0, b = 0) < .05)
mean(sim.Wald.B(n = 50, nsim = 500, a = 0, b = 0.1) < .05)
mean(sim.Wald.B(n = 50, nsim = 500, a = 0, b = 0.2) < .05)
mean(sim.Wald.B(n = 100, nsim = 500, a = 0, b = 0) < .05)
mean(sim.Wald.B(n = 100, nsim = 500, a = 0, b = 0.1) < .05)
mean(sim.Wald.B(n = 100, nsim = 500, a = 0, b = 0.2) < .05)


#For fun, here's what happens when you compare the Wald statistic to
#the appropriate t distribution instead of a standard normal
mean(sim.Wald.B(n = 10, nsim = 500, a = 0, b = 0, pfun = pt, df = 8) < .05)
mean(sim.Wald.B(n = 10, nsim = 500, a = 0, b = 0.1, pfun = pt, df = 8) < .05)
mean(sim.Wald.B(n = 10, nsim = 500, a = 0, b = 0.2, pfun = pt, df = 8) < .05)
mean(sim.Wald.B(n = 50, nsim = 500, a = 0, b = 0, pfun = pt, df = 48) < .05)
mean(sim.Wald.B(n = 50, nsim = 500, a = 0, b = 0.1, pfun = pt, df = 48) < .05)
mean(sim.Wald.B(n = 50, nsim = 500, a = 0, b = 0.2, pfun = pt, df = 48) < .05)
mean(sim.Wald.B(n = 100, nsim = 500, a = 0, b = 0, pfun = pt, df = 98) < .05)
mean(sim.Wald.B(n = 100, nsim = 500, a = 0, b = 0.1, pfun = pt, df = 98) < .05)
mean(sim.Wald.B(n = 100, nsim = 500, a = 0, b = 0.2, pfun = pt, df = 98) < .05)


##############################
##############################
##############################
#Exercise set 9-6

#Problem 1)

#Compute the likelihood-ratio statistic comparing a simple linear regression
#with and without a slope. x is the independent variable, y the
#dependent variable.
lr.stat.slr <- function(x, y){
  n <- length(x)
  #compute MLEs of beta and alpha
  B.hat <- (sum(x*y)-sum(x)*sum(y)/n)/( sum(x^2) - sum(x)^2/n)
  A.hat <- (sum(y) - B.hat*sum(x))/n
  #Compute estimated variance of MLE of beta
  vhat <- sum((y - A.hat - B.hat*x)^2)/(n-2)
  #likelihood-ratio statistic
  (sum((y - mean(y))^2) - sum((y - A.hat - B.hat*x)^2)) /vhat
}

lr.stat.slr(anscombe$x1, anscombe$y1)

1 - pchisq(lr.stat.slr(anscombe$x1, anscombe$y1), 1) #p value from chi square distribution
1 - pf(lr.stat.slr(anscombe$x1, anscombe$y1), 1, 9) # p value from F distribution (as in lm())
