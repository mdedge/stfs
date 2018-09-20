#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 8.

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
#Exercise set 8-1

#1)

n <- 20 #size of sample for ecdf.
x.vals <- seq(-3, 3, length.out = 10000)
Fx <- pnorm(x.vals, 0, 1)
plot(x.vals, Fx, xlab = "z", ylab = "F(z)", type = "l")
x <- rnorm(n, 0, 1)
lines(ecdf(x), verticals = TRUE, do.points = FALSE, lty = 2)

#As the sample size n increases, the empirical distribution 
#function matches the true cumulative distribution function 
#increasingly closely. For large sample sizes (say, n = 10^5), 
#the empirical distribution function and true cumulative 
#distribution function are not visibly distinguishable in the figure.

#2) Here's a sample script to make a similar plot with the exponential distribution:

min.x <- 0 # min value to plot
rate.x <- 1 #exponential parameter
max.x <- 5/rate.x #max value to plot
n <- 20 #size of sample for ecdf.

x.vals <- seq(min.x, max.x, length.out = 10000)
Fx <- pexp(x.vals, rate.x)
plot(x.vals, Fx, xlab = "z", ylab = "F(z)", type = "l")

x <- rexp(n, rate.x)
lines(ecdf(x), verticals = TRUE, do.points = FALSE, lty = 2)

#And here's the Poisson distribution:

min.x <- 0 # min value to plot
rate.x <- 5 #Poisson parameter
max.x <- 2.2*rate.x #max value to plot
n <- 20 #size of sample for ecdf.

x.vals <- seq(min.x, max.x, length.out = 10000)
Fx <- ppois(x.vals, rate.x)
plot(x.vals, Fx, xlab = "z", ylab = "F(z)", type = "l")

x <- rpois(n, rate.x)
lines(ecdf(x), verticals = TRUE, do.points = FALSE, lty = 2)

#The Poisson distribution looks different because it is discrete, 
#so its true cumulative distribution function looks like a step function.

##############################
##############################
##############################
#Exercise set 8-2

#Problem 2)
#a) Here is one way to do it using a for() statement:

n <- 5
nsamps <- 100000
vars.pi <- numeric(nsamps)
for(i in 1:nsamps){
  samp <- rnorm(n, 0, 1)
  var.pi <- sum((samp-mean(samp))^2)/length(samp)
  vars.pi[i] <- var.pi
}
mean(vars.pi)

#Here is another approach that uses apply() instead of 
#for(). 
n <- 5
nsamps <- 100000
x <- rnorm(nsamps*n,0,1)
samps <- matrix(x, nrow = nsamps, ncol = n)
var.pi <- function(vec){
  return(sum((vec-mean(vec))^2)/length(vec))
}
vars.pi <- apply(samps, 1, var.pi)
mean(vars.pi)

#Executing either block of code gives answers very close to 0.8, not 1.

#b) #Here is one way to do it that wraps the solution from part (a)
#into a function
#Function that returns the mean plug-in variance estimate from nsamps
#normal(0,1) samples, each of size n.
normsamp.meanpluginvar.sims <- function(n, nsamps = 10000){
  x <- rnorm(nsamps*n,0,1)
  samps <- matrix(x, nrow = nsamps, ncol = n)
  var.pi <- function(vec){
    sum((vec-mean(vec))^2)/length(vec)
  }
  mean(apply(samps, 1, var.pi))
}

#Use sapply() to apply the function just defined to values of n in the 
#vector 2:10.
sapply(2:10, FUN = normsamp.meanpluginvar.sims, nsamps = 10000)

#Problem 3) 

#Here is some R code that estimates the requested moments using 
#the sample moments. I used a sample of ten million observations, 
#but you could increase the precision further by taking a larger 
#sample or by aggregating the results from several samples 
#with a larger total size:

x <- rnorm(10^7, 0, 1)
mean(x^4)
mean(x^5)
mean(x^6)
mean(x^7)
mean(x^8)

#The estimates I obtained were close to the true values, which 
#are "E(X^4)=3, E(X^5 )=0, "E(X^6 )=15, E(X^7 )=0, and E(X^8 )=105. 

##############################
##############################
##############################
#Exercise set 8-3

#Problem 1)
#The following R code simulates a sample of m independent Binomial(n,p) 
#observations and estimates n and p shown in the main text. 
#Increasing the value of m produces estimates closer to the true values. 
#When m=10^7, for example, the estimates are very close to the true values.

m <- 10000
n <- 50
p <- 0.3
x <- rbinom(m, n, p)
n.est <- ((sum(x)/m)^2)/((sum(x)/m)^2 + sum(x)/m - sum(x^2)/m)
p.est <- ((sum(x)/m)^2 + sum(x)/m - sum(x^2)/m)/(sum(x)/m)
n.est
p.est

##############################
##############################
##############################
#Exercise set 8-4

#Problem 1)

#true se = 1/sqrt(5) = .44
wrap.bm(5, 10)
wrap.bm(5, 100)
wrap.bm(5, 1000)
wrap.bm(5, 5000)
#true se = 1/sqrt(20) = .22
wrap.bm(20, 10)
wrap.bm(20, 100)
wrap.bm(20, 1000)
wrap.bm(20, 5000)
#true se = 1/sqrt(50) = .14
wrap.bm(50, 10)
wrap.bm(50, 100)
wrap.bm(50, 1000)
wrap.bm(50, 5000)
#true se = 1/sqrt(100) = .1
wrap.bm(100, 10)
wrap.bm(100, 100)
wrap.bm(100, 1000)
wrap.bm(100, 5000)


#Problem 2)

#compute the midrange for a numeric vector x
midrange <- function(x){
  (min(x) + max(x)) / 2
}

#true se = sqrt(pi^2/(24*log(5))) = .505
wrap.bm(5, 10, FUN = midrange)
wrap.bm(5, 100, FUN = midrange)
wrap.bm(5, 1000, FUN = midrange)
wrap.bm(5, 5000, FUN = midrange)
#true se = sqrt(pi^2/(24*log(20))) = .371
wrap.bm(20, 10, FUN = midrange)
wrap.bm(20, 100, FUN = midrange)
wrap.bm(20, 1000, FUN = midrange)
wrap.bm(20, 5000, FUN = midrange)
#true se = sqrt(pi^2/(24*log(50))) = .324
wrap.bm(50, 10, FUN = midrange)
wrap.bm(50, 100, FUN = midrange)
wrap.bm(50, 1000, FUN = midrange)
wrap.bm(50, 5000, FUN = midrange)
#true se = sqrt(pi^2/(24*log(100))) = .299
wrap.bm(100, 10, FUN = midrange)
wrap.bm(100, 100, FUN = midrange)
wrap.bm(100, 1000, FUN = midrange)
wrap.bm(100, 5000, FUN = midrange)
#true se = sqrt(pi^2/(24*log(1000))) = .244
wrap.bm(1000, 10, FUN = midrange)
wrap.bm(1000, 100, FUN = midrange)
wrap.bm(1000, 1000, FUN = midrange)
wrap.bm(1000, 5000, FUN = midrange)


##############################
##############################
##############################
#Exercise set 8-5

#Problem 2)

mean(sim.perm.B(0, 0, n = 10) < .05)
mean(sim.perm.B(0, 0.1, n = 10) < .05)
mean(sim.perm.B(0, 0.2, n = 10) < .05)
mean(sim.perm.B(0, 0, n = 50) < .05)
mean(sim.perm.B(0, 0.1, n = 50) < .05)
mean(sim.perm.B(0, 0.2, n = 50) < .05)
mean(sim.perm.B(0, 0, n = 100) < .05)
mean(sim.perm.B(0, 0.1, n = 100) < .05)
mean(sim.perm.B(0, 0.2, n = 100) < .05)