#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 10.

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
#Exercise set 10-1

#Problem 1)

#To begin, let's set some parameters and simulate data we'll use in parts (a-c).

n <- 20
true.mean <- 2
known.sd <- 1
prior.mean <- 0
prior.sd <- 1

set.seed(8675309)
z <- rnorm(n,true.mean,known.sd)

#a) The mean of my sample is 2.15, fairly close to the true expectation of 2. 
#Here is a function to compute the posterior mean and variance using 
#equations 10.4 and 10.5:

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

post.conj.norm.norm(z, known.sd, prior.mean, prior.sd)
sqrt(post.conj.norm.norm(z, known.sd, prior.mean, prior.sd)[[2]]) #to compare with MCMCpack

#b) 
#install and load MCMCpack if necessary
if(!("MCMCpack" %in% installed.packages())){install.packages("MCMCpack")}
library(MCMCpack)

mn.mod <- MCnormalnormal(z, sigma2 = 1, mu0 = prior.mean, tau20 = prior.sd^2, mc = 10000)
summary(mn.mod)

#MCMCpack results are similar to conjugate prior.

#c) 
rsamps <- reject.samp.norm(z, known.sd, prior.mean, prior.sd)
mean(rsamps)
sd(rsamps)

#Rejection sampling results agree with the other methods.


#2) Here are the functions (commented out), which are already defined as part of the stfspack package:

#Get 1 sample under rejection sampling from normal with known sd.
#z is a vector of data.
#get.1.samp.norm <- function(z, known.sd = 1, prior.mean = 0, prior.sd = 1){
#  accepted <- FALSE
#  max.like <- exp(sum(log(dnorm(z, mean = mean(z), sd = known.sd))))
#  while(accepted == FALSE){
#    cand <- rnorm(1, prior.mean, prior.sd)
#    like <- exp(sum(log(dnorm(z, mean = cand, sd = known.sd))))
#    crit <- like / max.like
#    xunif <- runif(1,0,1)
#    if(xunif <= crit){accepted <- TRUE}
#  }
#  cand
#}

#Wrapper for get.1.samp.norm() that gets rejection sample from posterior of desired size.
#reject.samp.norm <- function(z, known.sd = 1, prior.mean = 0, prior.sd = 1, nsamps = 10000){
#  samps <- numeric(nsamps)
#  for(i in seq_along(samps)){
#    samps[i] <- get.1.samp.norm(z, known.sd, prior.mean, prior.sd)
#  }
#  samps
#}


##############################
##############################
##############################
#Exercise set 10-2.

#install and load MCMCpack if necessary
if(!("MCMCpack" %in% installed.packages())){install.packages("MCMCpack")}
library(MCMCpack)

#Problem 1)

#a) least-squares estimates come from lm()

y <- anscombe$y1
x <- anscombe$x1
reg.ml <- lm(y~x)
summary(reg.ml)

#b) Code to fit and summarize model with all 9 possible priors is:

reg11 <- MCMCregress(y ~ x, b0 = c(0,0), B0 = 0.0001)
reg12 <- MCMCregress(y ~ x, b0 = c(0,0), B0 = 1)
reg13 <- MCMCregress(y ~ x, b0 = c(0,0), B0 = 100)
reg21 <- MCMCregress(y ~ x, b0 = c(3,0), B0 = 0.0001)
reg22 <- MCMCregress(y ~ x, b0 = c(3,0), B0 = 1)
reg23 <- MCMCregress(y ~ x, b0 = c(3,0), B0 = 100)
reg31 <- MCMCregress(y ~ x, b0 = c(10,-5), B0 = 0.0001)
reg32 <- MCMCregress(y ~ x, b0 = c(10,-5), B0 = 1)
reg33 <- MCMCregress(y ~ x, b0 = c(10,-5), B0 = 100)

summary(reg11)
plot(reg11)
summary(reg12)
plot(reg12)
summary(reg13)
plot(reg13)
summary(reg21)
plot(reg21)
summary(reg22)
plot(reg22)
summary(reg23)
plot(reg23)
summary(reg31)
plot(reg31)
summary(reg32)
plot(reg32)
summary(reg33)
plot(reg33)

#When prior precision is low—meaning that prior variance is high—then 
#the prior means are not especially important; these three choices 
#lead to similar conclusions. If the prior precision is higher, 
#then the prior means matter much more, and estimates are generally 
#close to the prior means. 

##############################
##############################
##############################
#Exercise set 10-3.

#1)
#install and load MCMCpack if necessary
if(!("MCMCpack" %in% installed.packages())){install.packages("MCMCpack")}
library(MCMCpack)
#install and load coda if necessary
if(!("coda" %in% installed.packages())){install.packages("coda")}
library(coda)

#First, run the regressions from the previous exercise set 
#(Exercise Set 10-2, Problem 1). Once the regressions are run,

#The quantile credible intervals are the ranges from the .025 to 
#.0975 quantiles of the posterior:
summary(reg11)$quantiles[,c(1,5)] 
summary(reg12)$quantiles[,c(1,5)] 
summary(reg13)$quantiles[,c(1,5)] 
summary(reg21)$quantiles[,c(1,5)] 
summary(reg22)$quantiles[,c(1,5)] 
summary(reg23)$quantiles[,c(1,5)] 
summary(reg31)$quantiles[,c(1,5)] 
summary(reg32)$quantiles[,c(1,5)] 
summary(reg33)$quantiles[,c(1,5)] 

#And the highest-posterior-density credible intervals come from
#HPDinterval
HPDinterval(reg11, .95)
HPDinterval(reg12, .95)
HPDinterval(reg13, .95)
HPDinterval(reg21, .95)
HPDinterval(reg22, .95)
HPDinterval(reg23, .95)
HPDinterval(reg31, .95)
HPDinterval(reg32, .95)
HPDinterval(reg33, .95)

##############################
##############################
##############################
#Exercise set 10-4.

#Problem 1) 

#First, define x and y for brevity below
y <- anscombe$y1
x <- anscombe$x1

#a) Here is the code:
reg0 <- MCMCregress(y~1, b0 = 0, B0 = 1/100, c0 = 0.001, d0 = 0.001, marginal.likelihood = "Laplace")
reg1 <- MCMCregress(y~x, b0 = 0, B0 = 1/100, c0 = 0.001, d0 = 0.001, marginal.likelihood = "Laplace")
summary(reg0)
summary(reg1)
summary(BayesFactor(reg1, reg0))

#Some relevant outputs are: 
#  Intercept under H_1: posterior mean of 2.97, 95% credible interval [0.48, 5.52]
#Slope under H_1: posterior mean of 0.50, 95% credible interval [0.23, 0.77]
#Bayes factor B_10: 3.26. By Kass & Raftery’s scale, this is positive evidence for H_1 over H_0.  

#b) Here is the code:

reg01 <- MCMCregress(y~1, b0 = 0, B0 = 1/16, c0 = 0.001, d0 = 0.001, marginal.likelihood = "Laplace")
reg11 <- MCMCregress(y~x, b0 = 0, B0 = 1/16, c0 = 0.001, d0 = 0.001, marginal.likelihood = "Laplace")
summary(reg01)
summary(reg11)
summary(BayesFactor(reg11, reg01))

#Some relevant outputs are: 
#  Intercept under H_1: posterior mean of 2.76, 95% credible interval [0.33, 5.13]
#Slope under H_1: posterior mean of 0.52, 95% credible interval [0.27, 0.78]
#Bayes factor B_10: 26.9. By Kass & Raftery’s scale, this is strong evidence for H_1 over H_0.  

#c) Here is the code:

reg02 <- MCMCregress(y~1, b0 = 0, B0 = 1/10000, c0 = 0.001, d0 = 0.001, marginal.likelihood = "Laplace")
reg12 <- MCMCregress(y~x, b0 = 0, B0 = 1/10000, c0 = 0.001, d0 = 0.001, marginal.likelihood = "Laplace")
summary(reg02)
summary(reg12)
summary(BayesFactor(reg12, reg02))

#Some relevant outputs are: 
#  Intercept under H_1: posterior mean of 3.01, 95% credible interval [0.51, 5.61]
#Slope under H_1: posterior mean of 0.50, 95% credible interval [0.22, 0.76]
#Bayes factor B_10: 0.259. By Kass & Raftery's scale, this is positive evidence 
#for H_0 over H_1---notice that we have switched from having the data support H_1 to having them support H_0. 
