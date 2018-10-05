#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 6.

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


########################################
#Exercise Set 6-1

#Problem 2)

#Use the mat.samps() function to draw a set of samples from the standard normal:
s.mat <- mat.samps(n = 25, nsim = 10000)

#The 25 in the function call specifies the size of each sample; 
#the 10000 specifies how many samples we draw.

#We can calculate the mean and median of each sample using 
#apply() :
ests.mean <- apply(s.mat, 1, mean)
ests.median <- apply(s.mat, 1, median)

#Or using a for() loop:
ests.mean <- numeric(10000)
ests.median <- numeric(10000)
for(i in 1:10000){
  ests.mean[i] <- mean(s.mat[i,])
  ests.median[i] <- median(s.mat[i,])
}

#You can use the hist() function to plot the means and medians of 
#each sample, and you can also take their mean. The mean of the 
#sample means will approach the expectation of the sample mean 
#by the law of large numbers. The median does too, but the 
#law of large numbers doesn’t tell us that.

hist(ests.mean)
hist(ests.median)
mean(ests.mean)
mean(ests.median)

#You should see that the histogram of the sample median is 
#centered around theta---in this case, set to 0 by the norm.samps() 
#call---and that the mean of the sample medians is very close to 
#zero. Repeating the procedure gives similar results. The 
#results correctly suggest that the sample median is an 
#unbiased estimate of theta when the data are independent samples 
#from a Normal(theta,1) distribution. These results do not 
#constitute proof, but they do suggest what turns out to 
#be the right answer in this case.

#############################
#Exercise Set 6-2

#Problem 2)

#This problem also requires a sample from mat.samps,
#then you need compute the means and medians.
s.mat <- mat.samps(n = 25, nsim = 10000)
ests.mean <- apply(s.mat, 1, mean)
ests.median <- apply(s.mat, 1, median)

#Now here's the new part:
var(ests.mean)
var(ests.median)
#You will see that the variance of the median is larger
#in this case. You can also see the difference in spread
#by making a simple boxplot (sample medians are on the right):
boxplot(ests.mean, ests.median)
#You can try this with different sample sizes by replacing the 25
#in the norm.sampse command with a different number.

##############################
#Exercise set 6-4

#Problem 2)
#The sample median is unbiased, so by equation 6.7, to convince 
#ourselves that it is consistent,we only need to 
#convince ourselves that the variance of the sample median decreases 
#to zero as the sample size increases.

s.mat <- mat.samps(n = 25, nsim = 10000)
ests.median <- apply(s.mat, 1, median)

#Use var() to estimate the variance of the sample median:
var(ests.median)

#Now try increasing the sample size, set to 25 in the above call. 
#As you increase the size of the sample, you should see that the 
#variance of the sample median gets smaller. 
#Here is a set of commands that will do the trick:
#Generate 5 sets of normal samples with 10,000 samples of each 
#of these sizes: 25, 50, 100, 500, 1000.
s.mat.25 <- mat.samps(n = 25, nsim = 10000)
s.mat.50 <- mat.samps(n = 50, nsim = 10000)
s.mat.100 <- mat.samps(n = 100, nsim = 10000)
s.mat.500 <- mat.samps(n = 500, nsim = 10000)
s.mat.1000 <- mat.samps(n = 1000, nsim = 10000)
#Calculate the median of each sample generated above.
ests.median.25 <- apply(s.mat.25, 1, median)
ests.median.50 <- apply(s.mat.50, 1, median)
ests.median.100 <- apply(s.mat.100, 1, median)
ests.median.500 <- apply(s.mat.500, 1, median)
ests.median.1000 <- apply(s.mat.1000, 1, median)
#Estimate the variance of the sample median at each of the 
#specified sample sizes.
var(ests.median.25)
var(ests.median.50)
var(ests.median.100)
var(ests.median.500)
var(ests.median.1000)
#Look at the variability of the sample median for each sample #size.
#Each box represents a different sample size.
boxplot(ests.median.25, ests.median.50, ests.median.100, ests.median.500, ests.median.1000)
#You should see that the variance of the sample median decreases 
#as the size of the sample increases. Simulation is not rigorous proof,
#but it is in fact true that the variance of the sample median continues 
#to approach a limit of zero as the sample size increases.


#############################
#Exercise Set 6-5.
#Problem 1)

mu <- 0
s.mat <- mat.samps(n = 25, nsim = 10000)
ests.mean <- apply(s.mat, 1, mean)
ests.median <- apply(s.mat, 1, median)

#Here's the new part:
#The relative efficiency is estimated as the quotient of the
#MSEs. The relative efficiency of the sample mean vs. the 
#sample median has the MSE of the sample mean in the
#denominator.
re <- mean((ests.median - mu)^2)/mean((ests.mean - mu)^2)
re

#When I ran this code, I obtained a relative efficiency of 1.4. 
#For samples of size five from a normal distribution, the 
#sample mean is a more efficient estimator of the first parameter
#of a normal distribution than the sample median is. 
#That is, the sample mean's mean squared error is lower.

#b) Here is some R code that computes the requested estimates 
#of relative efficiency and makes a basic plot of them:

n <- c(2,5,10,20,50,100,200,500)
nsims <- 2000
mu <- 0
sigma <- 1
re <- numeric(length(n))
for(i in 1:length(n)){
  x <- mat.samps(n = n[i], nsim = nsims, mean = mu, sd = sigma)
  ests.median <- apply(x, 1, median)
  ests.mean <- apply(x, 1, mean)
  re[i] <- mean((ests.median - mu)^2)/mean((ests.mean - mu)^2)
}
plot(n, re, xlab = "sample size", ylab = "RE of sample mean vs. median for normal data")

#When I run this code, the relative efficiency appears to level off 
#between 1.5 and 1.6. This agrees with theoretical results—a little math 
#(beyond our scope) shows that the true asymptotic relative efficiency 
#is pi/2 ~= 1.57.

#Problem 2

#Now repeat the solution to problem 1, replacing
#the rx argument of mat.samps with rlaplace
#a)

mu <- 0
s.mat <- mat.samps(25, 10000, rx = rlaplace)
ests.mean <- apply(s.mat, 1, mean)
ests.median <- apply(s.mat, 1, median)
re <- mean((ests.median - mu)^2)/mean((ests.mean - mu)^2)
re

#b)
n <- c(2,5,10,20,50,100,200,500)
nsims <- 2000
mu <- 0
sigma <- 1
re <- numeric(length(n))
for(i in 1:length(n)){
  x <- mat.samps(n = n[i], nsim = nsims, rx = rlaplace, mu, sigma)
  ests.median <- apply(x, 1, median)
  ests.mean <- apply(x, 1, mean)
  re[i] <- mean((ests.median - mu)^2)/mean((ests.mean - mu)^2)
}
plot(n, re, xlab = "sample size", ylab = "RE of sample mean vs. median for laplace data")

#if the data are Laplace distributed, then the median is actually a more 
#efficient /lower variance estimator than the mean is, particularly for 
#large samples. The point is that efficiency is not a property of a 
#statistic, it is a property of an estimator under a model. If the model 
#changes, then the relative efficiency of estimators may also change.

#Exercise set 6-6

#Problem 1
#a) Code to draw plot for absolute-error (or "L1") loss:
x <- c(0, 1000, 2000)
y <- c(1000, 0 , 1000) 
plot(x, y, pch = "", xlab = "Estimate", ylab = "Loss")
lines(x,y)
#b) Code to draw plot for linear loss specified in the problem:
x <- c(0, 1000, 2000)
y <- c(1000, 0, 2000) 
plot(x, y, pch = "", xlab = "Estimate", ylab = "Loss")
lines(x,y)
#c) A 0-1 Loss function.
x <- 1:6
y <- c(1,1,0,1,1,1)
plot(x, y, pch = 19, xlab = "Estimate", ylab = "Loss")

#Exercise set 6-7

#Problem 1) 
#a) You already have simulations to approximate the risk of the median of 
#100 independent normal samples under squared error loss. Because the 
#sample median is unbiased, the risk under squared error loss is equal 
#to the variance (exercise set 6-3, problem 2). We can use the mat.samps() 
#function from exercise set 6-1, problem 2 to draw 100,000 samples of size 100, 
#then use code from exercise set 6-4, problem 2 to compute the sample medians and check their variance:

s.mat <- mat.samps(100, 10000)
ests.median <- apply(s.mat, 1, median)
var(ests.median)

#When I run this code, I get an estimate of about 0.0154, 
#which is larger than the risk of the sample mean.

#b) After running the code in part (a) to simulate independent 
#samples from a normal distribution, calculate the mean of each sample:

ests.mean <- apply(s.mat, 1, mean)

#Then, if you simulated with mu=0, you can calculate the 
#approximate risk under absolute error loss with
mean(abs(ests.mean))
mean(abs(ests.median))

#If you set mu to be a value other than 0, you need to subtract it from 
#every entry in your vector of means before taking the absolute value. 
#For example, if you set mu=5, then you would approximate the 
#absolute error risk with
#mean(abs(ests.mean - 5))

#When I run this code, I find that the risk for the sample mean is 
#about 0.08 and that the risk for the sample median is about 0.10, 
#still larger than the risk for the sample mean. Note that though 
#the risk of the mean and median are larger and more similar with 
#absolute error loss than with squared-error loss, absolute error 
#loss and squared error loss are in different units—original units 
#vs. squared units. Thus, the fact that the risks are larger and 
#more similar isn’t necessarily meaningful.

#c) After running the code in parts (a) and (b), assuming that you 
#set mu=0, the approximate risk is given by

mean(abs(ests.mean^3))
mean(abs(ests.median^3))

#I get an approximate risk of 0.0016 for the sample mean 
#and 0.0031 for the sample median.

#Problem 2) Here is some R code that draws all four risk functions on the same plot. 
#You can use it to check your answers for parts (a-d). 
#Justification for each risk function is given below:

n <- 3
theta <- seq(4,8,length.out = 1000)
r.sm <- rep(1/n, length(theta))
r.fo <- rep(1, length(theta))
r.6 <- (theta - 6)^2
r.td <- (3 - theta/2)^2 + 1/(4*n)

plot(theta, r.6, pch = "", xlab = "theta", ylab = "risk")
lines(theta, r.sm)
lines(theta, r.fo, lty = 2)
lines(theta, r.6, lty = 3)
lines(theta, r.td, lty = 4)

##############################
##############################
##############################
#Exercise set 6-8
#Problem 1)

#1) 
#Here is code to examine the first set of specified parameters:

dat <- mat.samps(n = 100, nsim = 1000, rx = rnorm.contam, contam.p = 0.001, contam.mu = 3, contam.sigma = 1)
means <- apply(dat,2,mean)
medians <- apply(dat,2,median)
mean(means)
var(means)
mean(medians)
var(medians)
hist(means)
hist(medians)

#You can examine the other parameter sets by replacing the 0.001 
#in the above function call with the desired contamination probability and replacing 
#the 3 with the desired lambda. You will notice that when gamma
#and lambda are large, both the median and the mean are biased 
#upward, and they both increase in variance. The median, however, 
#is much less affected by the aberrant observations than the mean is. 
#When gamma is small, the median is almost unaffected. 

#This exercise demonstrates the median's robustness against 
#outliers. 


##############################
##############################
##############################
#Exercise Set 6-9.

#Problem 1) a)
ests <- sim.lm.ests(n = 10, nsim = 1000, a = 3, b = 1/2)
hist(ests[,2])
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 50, nsim = 1000, a = 3, b = 1/2)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 100, nsim = 1000, a = 3, b = 1/2)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 1000, nsim = 1000, a = 3, b = 1/2)
colMeans(ests)
apply(ests, 2, var)

#In this scenario, the least-squares estimators are unbiased and 
#consistent. (You will have a chance to prove this in some optional 
#exercises in chapter 9.) At each sample size, the means of the 
#estimates are close to the true values, and the variances of the 
#estimates decrease as the sample size increases.

#b) 

if(!("quantreg" %in% installed.packages())){install.packages("quantreg")}
library(quantreg)
ests <- sim.lm.ests(n = 10, nsim = 1000, a = 3, b = 1/2, estfun = rq)
hist(ests[,2])
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 50, nsim = 1000, a = 3, b = 1/2, estfun = rq)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 100, nsim = 1000, a = 3, b = 1/2, estfun = rq)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 1000, nsim = 1000, a = 3, b = 1/2, estfun = rq)
colMeans(ests)
apply(ests, 2, var)

#The results for the least-absolute-errors estimators are similar 
#to those in part (a), though the variances are somewhat larger. 

#c) In this scenario (normally distributed disturbances of constant 
#variance), the least-squares estimators are more efficient than the 
#least-absolute-errors estimators—both sets of estimators appear 
#close to unbiased in the simulations (and they are in fact unbiased), 
#and the variances of the least-squares estimators are smaller at 
#each sample size.

#Problem 2)

#a)
#for example, call this many times:
plot(sim.lm(n = 50, a = 3, b = 0.5, rdisturb = rlaplace))
#The cloud of observations is more vertically dispersed when the 
#disturbances are Laplace distributed, but the effect is too 
#subtle to detect reliably just by looking. (Statistical tests like 
#those in the gvlma package [see the postlude chapter] are more 
#sensitive than our eyes.)  

#b) Repeating some of the code for problem 1, but with 
#Laplace disturbances:

#Least-squares
ests <- sim.lm.ests(n = 50, nsim = 1000, a = 3, b = 1/2, rdisturb = rlaplace)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 100, nsim = 1000, a = 3, b = 1/2, rdisturb = rlaplace)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 1000, nsim = 1000, a = 3, b = 1/2, rdisturb = rlaplace)
colMeans(ests)
apply(ests, 2, var)

#Least-absolute errors
ests <- sim.lm.ests(n = 50, nsim = 1000, a = 3, b = 1/2, estfun = rq, rdisturb = rlaplace)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 100, nsim = 1000, a = 3, b = 1/2, estfun = rq, rdisturb = rlaplace)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 1000, nsim = 1000, a = 3, b = 1/2, estfun = rq, rdisturb = rlaplace)
colMeans(ests)
apply(ests, 2, var)

#Both sets of estimators appear to be approximately unbiased, and the simulations 
#suggest that they may be consistent. However, the relative efficiency is 
#reversed---now the least-squares estimators are less efficient than the 
#least-absolute-errors estimators. Once again, efficiency is a property of an 
#estimator under a specific model, not of the statistic itself. 

#Problem 3)

#a) for example, call this many times:
plot(sim.lm(n = 500, a = 3, b = 0.5, rdist = rnorm.contam))
#The command shown shows a cloud of points centered around a line (not drawn) 
#with intercept 3 and slope 1/2. In some trials, there are some points in the 
#lower-right corner that are far removed from the rest of the data. These 
#are outliers both in the sense of being removed from the rest of the data 
#and from being actually created by a different process---their disturbances 
#are from the contaminating distribution.

#b) Repeating the problem 1 code with rdist = rnorm.mix:

#Least-squares
ests <- sim.lm.ests(n = 50, nsim = 1000, a = 3, b = 1/2, rdisturb = rnorm.contam)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 100, nsim = 1000, a = 3, b = 1/2, rdisturb = rnorm.contam)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 1000, nsim = 1000, a = 3, b = 1/2, rdisturb = rnorm.contam)
colMeans(ests)
apply(ests, 2, var)

#Least-absolute errors
ests <- sim.lm.ests(n = 50, nsim = 1000, a = 3, b = 1/2, estfun = rq, rdisturb = rnorm.contam)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 100, nsim = 1000, a = 3, b = 1/2, estfun = rq, rdisturb = rnorm.contam)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(n = 1000, nsim = 1000, a = 3, b = 1/2, estfun = rq, rdisturb = rnorm.contam)
colMeans(ests)
apply(ests, 2, var)

#With this form of data contamination / outliers, neither set of estimators is unbiased 
#or consistent. Both tend to produce slope estimates that are too low; the 
#line is being "pulled down" by the outlying points in the lower right. 
#However, the least-absolute-errors estimators are much more robust than 
#the least-squares estimators. They are closer to the true values on 
#average and have lower variance.

#Problem 4)

#b)
library(MASS)

n <- 50
nsims = 1000
beta <- .3
gamma <- .4
rho <- .5
dat <- sim.2var(n, nsims, 0, beta, gamma, 1, rho)
ests <- numeric(nsims)
for(i in 1:nsims){
  ests[i] <- lm(dat[[2]][i,] ~ dat[[1]][i,])$coef[2]
}
hist(ests)
summary(ests)
