#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch.

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

###########################
#Exercise set 2-2

#Problem 1

#Compute the sample mean and median
mean(iris$Petal.Length)
median(iris$Petal.Length)

#Histograms provide a visual summary of the distribution
hist(iris$Petal.Length, xlab = "Petal Length")
mean(iris$Petal.Length[iris$Species=="setosa"]) 

#We'll use the conditional.mean function 
conditional.mean <- function(x, y){
  for(i in unique(y)){
    print(mean(x[y == i]))
  }
}

conditional.mean(x = iris$Petal.Length, y = iris$Species)
conditional.mean(x = iris$Petal.Width, y = iris$Species)

#(Here are example of how you'd do it with tapply() or aggregate())
tapply(iris$Petal.Length, iris$Species, mean)
tapply(iris$Petal.Width, iris$Species, mean)

aggregate(iris$Petal.Length, list(iris$Species), mean)
aggregate(iris$Petal.Width, list(iris$Species), mean)

#A basic boxplot
boxplot(iris$Petal.Length ~ iris$Species)
title(xlab = "Species", ylab = "Petal Length") #Add axis labels

#A scatterplot
plot(iris$Petal.Length, iris$Petal.Width, pch = as.numeric(iris$Species), 
     xlab = "Petal Length", ylab = "Petal Width")
#And add a legend
legend("topleft", pch = c(1,2,3), legend = c("setosa", "versicolor", "virginica"))


#Problem 2

#This version installs the package only if it is not already installed
if(!("gpairs" %in% installed.packages())){
  install.packages("gpairs")
}
library(gpairs) #load the package
gpairs(iris, scatter.pars = list(col = as.numeric(iris$Species)))

########################
########################
########################
#Exercise set 3-1

#Problem 1
# a) Here's one way you could use equations 3.6 and 3.7 in R.

x <- anscombe$x1
y <- anscombe$y1
n <- length(x) #number of pairs of observations
#calculate the slope and intercept.
b <- (sum(x*y) - (1/n) * sum(x) * sum(y))/(sum(x^2) - (1/n) * sum(x)^2)
a <- (sum(y) - b*sum(x))/n
a	
b

#I assigned values values to x, y, and n to make the code a little 
#easier to read, but you could calculate a and b without doing that, 
#just replacing x with anscombe$x1, replacing y with anscombe$y1, 
#and replacing n with length(anscombe$x1). 

#To use equations 3.8 and 3.9, you could write

x <- anscombe$x1
y <- anscombe$y1
b <- sum((x - mean(x))*(y - mean(y))) / sum((x - mean(x))^2)
a <- mean(y) - b*mean(x)
a
b

#Both of these show that a ~= 3 and b ~= .5.

#b) The result of a is the same as for lm():

mod.fit <- lm(y1 ~ x1, data = anscombe)
mod.fit
summary(mod.fit)

########################
#Exercise set 3-2

#Problem 4
#a)
if(!("quantreg" %in% installed.packages())){
  install.packages("quantreg")
}
library(quantreg)

#b)
mod.fit.L1  <- rq(anscombe$y1 ~ anscombe$x1)
summary(mod.fit.L1)

#c)
#plot the L1 and L2 lines on the same plot:
plot(anscombe$x1, anscombe$y1)
mod.fit.L2  <- lm(anscombe$y1 ~ anscombe$x1)
mod.fit.L1  <- rq(anscombe$y1 ~ anscombe$x1)
abline(mod.fit.L2, lty = 1)
abline(mod.fit.L1, lty = 2)
legend("topleft", legend = c("L2", "L1"), lty = c(1,2))

#d) 
plot(anscombe$x3, anscombe$y3)

#e)
plot(anscombe$x3, anscombe$y3)
mod.fit.L2  <- lm(anscombe$y3 ~ anscombe$x3)
mod.fit.L1  <- rq(anscombe$y3 ~ anscombe$x3)
abline(mod.fit.L2, lty = 1)
abline(mod.fit.L1, lty = 2)
legend("topleft", legend = c("L2", "L1"), lty = c(1,2))
#The outlying point influences the L2 line a lot, but not the L1
#line. Because the L2 line is minimizing squared errors, large
#line errors are more important than smaller ones.

#############################################
#Exercise Set 4-4

#Problem 1
#Code for drawing the requested cumulative distribution function
x <- c(0,1)
Fx <- x 
plot(x, Fx, type = "l", xlim = c(-1,2))
lines(c(-1, 0), c(0, 0))
lines(c(1,2), c(1, 1))

#Problem 2
#Code for drawing a cdf meeting the problem description
x1 <- c(0,0.4)
x2 <- c(0.4, 0.6)
x3 <- c(0.6, 1)
Fx1 <- c(0,0.3)
Fx2 <- c(0.3, 0.7)
Fx3 <- c(0.7, 1)
plot(x1, Fx1, type = "l", xlim = c(-1,2), ylim = c(0,1), xlab = "x", ylab = "Fx")
lines(x2, Fx2)
lines(x3, Fx3)
lines(c(-1, 0), c(0, 0))
lines(c(1,2), c(1, 1))


#############################################
#Exercise Set 4-6

#Problem 3) a) 
x <- seq(-3, 3, length.out = 1000)
plot(x, dnorm(x, mean = 0, sd = 1), type = "l")
  
 # b)
x <- seq(-3, 3, length.out = 1000)
plot(x, pnorm(x, mean = 0, sd = 1), type = "l")
  
  #c) What value of x is at the 97.5th percentile of the standard normal distribution?
qnorm(0.975, mean = 0, sd = 1)
#This value, 1.96, is a useful number to remember, for reasons that will be discussed in chapter 7.

#Problem 4) a) 
normsims <- rnorm(1000, mean = 0, sd = 1)
hist(normsims)
  
  #b) 
unifsims <- runif(1000, 0 , 1)
hist(qnorm(unifsims, mean = 0, sd = 1))
  
  #We simulated uniform random draws, but we were able to transform them to draws from 
#a normal distribution by feeding them through the normal quantile function, 
#or the inverse of the normal distribution function. 
  
#Use this code to see a picture that shows how this works:
    
r <- seq(-3, 3, length.out = 1000)
cdf <- pnorm(r)
#Draw the normal cumulative distribution function.
plot(r, cdf, type = "l", xaxs = "i", yaxs = "i", xlim = c(-3, 3), xlab = expression(italic(x)), ylab = expression(paste(italic(F[X]), "(", italic(x), ")", sep = "")), lwd = 2)
  
#Draw light grey lines representing random samples from the 
#standard normal distribution.
x <- rnorm(500)
for(i in x){
  lines(c(i,i), c(min(x), pnorm(i)), col = rgb(190, 190, 190, 	alpha = 60, max = 255))
  lines(c(min(x)-1,i), c(pnorm(i), pnorm(i)), col = rgb(190, 	190, 190, alpha = 60, max = 255))
}
  
#The cumulative distribution function of X is drawn in solid black. The light grey 
#lines represent 500 random draws from the distribution of X. Start on the 
#horizontal axis. Each light grey line traces from the value of one of the 
#random samples of X on the x-axis up to the cumulative distribution function. 
#Once it hits the cumulative distribution function, it turns left until it hits 
#the vertical axis. Notice that the positions where the lines hit the x-axis are 
#centered on zero, symmetric, and concentrated near the middle—they look like a 
#normal distribution. Entering hist(x) will confirm the suspicion. In contrast, 
#the lines hit the y-axis with roughly uniform density from zero to one. 
hist(pnorm(x)) 
#confirms uniformity. 
  
#To make the plot, we simulated x-values from the normal distribution and fed 
#them into pnorm() to get uniformly distributed data. Effectively, we traced 
#grey lines from the horizontal axis up to the cumulative distribution function 
#and then to the left, ending with a uniform distribution along the vertical 
#axis. But we can also go backwards, starting with uniformly distributed data 
#on the vertical axis, tracing lines to the right until we get to the cumulative 
#distribution function, and then drawing lines straight down to get normally 
#distributed data. This is what happens when we apply qnorm() to uniformly 
#distributed data.  
  
#This is a powerful idea, not just a curiosity. This approach lets us draw 
#pseudorandom samples from any distribution with a known cumulative distribution 
#function as long as it is possible to generate pseudorandom samples from a 
#continuous uniform distribution.
  

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
a <- 3
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
sim_0_1 <- sim.lm(a = 0, b = 1)
plot(sim_0_1[,1], sim_0_1[,2])
abline(0,1) #draw the E(Y|x) line



########################################
#Exercise Set 6-1

#Problem 2)

#Use the norm.samps() function to draw a set of samples:
s.mat <- norm.samps(0, 1, 25, 10000)

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
#centered around theta—in this case, set to 0 by the norm.samps() 
#call—and that the mean of the sample medians is very close to 
#zero. Repeating the procedure gives similar results. The 
#results correctly suggest that the sample median is an 
#unbiased estimate of theta when the data are independent samples 
#from a Normal(theta,1) distribution. These results do not 
#constitute proof, but they do suggest what turns out to 
#be the right answer in this case.

#############################
#Exercise Set 6-2

#Problem 2)

#This problem also requires a sample from norm.samps,
#then you need compute the means and medians.
s.mat <- norm.samps(0, 1, 25, 10000)
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

s.mat <- norm.samps(0, 1, 25, 10000)
ests.median <- apply(s.mat, 1, median)

#Use var() to estimate the variance of the sample median:
var(ests.median)

#Now try increasing the sample size, set to 25 in the above call. 
#As you increase the size of the sample, you should see that the 
#variance of the sample median gets smaller. 
#Here is a set of commands that will do the trick:
#Generate 5 sets of normal samples with 10,000 samples of each 
#of these sizes: 25, 50, 100, 500, 1000.
s.mat.25 <- norm.samps(0, 1, 25, 10000)
s.mat.50 <- norm.samps(0, 1, 50, 10000)
s.mat.100 <- norm.samps(0, 1, 100, 10000)
s.mat.500 <- norm.samps(0, 1, 500, 10000)
s.mat.1000 <- norm.samps(0, 1, 1000, 10000)
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
s.mat <- norm.samps(mu, 1, 25, 10000)
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
  x <- norm.samps(mu, sigma, n[i], nsims)
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
#norm.samps() with laplace.samps():
#a)

mu <- 0
s.mat <- laplace.samps(mu, 1, 25, 10000)
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
  x <- laplace.samps(mu, sigma, n[i], nsims)
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
#to the variance (exercise set 6-3, problem 2). We can use the norm.samps() 
#function from exercise set 6-1, problem 2 to draw 100,000 samples of size 100, 
#then use code from exercise set 6-4, problem 2 to compute the sample medians and check their variance:

s.mat <- norm.samps(0, 1, 100, 10000)
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

#1) Here is the rnorm.out() function

#Here is code to examine the first set of specified parameters:

dat <- rnorm.out(1000, 100, 0.001, lambda = 3)
means <- apply(dat,2,mean)
medians <- apply(dat,2,median)
mean(means)
var(means)
mean(medians)
var(medians)
hist(means)
hist(medians)

#You can examine the other parameter sets by replacing the 0.001 
#in the above function call with the desired gamma and replacing 
#the 3 with the desired lambda. You will notice that when gamma
#and lambda are large, both the median and the mean are biased 
#upward, and they both increase in variance. The median, however, 
#is much less affected by the aberrant observations than the mean is. 
#When gamma is small, the median is almost unaffected. 

#This exercise demonstrates the median’s robustness against 
#outliers. 


##############################
##############################
##############################
#Exercise Set 6-9.

#Problem 1) a)
ests <- sim.lm.ests(1000, n = 10, a = 3, b = 1/2)
hist(ests[,2])
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 50, a = 3, b = 1/2)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 100, a = 3, b = 1/2)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 1000, a = 3, b = 1/2)
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
ests <- sim.lm.ests(1000, n = 10, a = 3, b = 1/2, estfun = rq)
hist(ests[,2])
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 50, a = 3, b = 1/2, estfun = rq)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 100, a = 3, b = 1/2, estfun = rq)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 1000, a = 3, b = 1/2, estfun = rq)
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
plot(sim.lm(n = 50, a = 3, b = 0.5, rdist = rlaplace))
#The cloud of observations is more vertically dispersed when the 
#disturbances are Laplace distributed, but the effect is too 
#subtle to detect reliably just by looking. (Statistical tests like 
#those in the gvlma package [see the postlude chapter] are more 
#sensitive than our eyes.)  

#b) Repeating some of the code for problem 1, but with 
#Laplace disturbances:

#Least-squares
ests <- sim.lm.ests(1000, n = 50, a = 3, b = 1/2, rdist = rlaplace)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 100, a = 3, b = 1/2, rdist = rlaplace)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 1000, a = 3, b = 1/2, rdist = rlaplace)
colMeans(ests)
apply(ests, 2, var)

#Least-absolute errors
ests <- sim.lm.ests(1000, n = 50, a = 3, b = 1/2, estfun = rq, rdist = rlaplace)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 100, a = 3, b = 1/2, estfun = rq, rdist = rlaplace)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 1000, a = 3, b = 1/2, estfun = rq, rdist = rlaplace)
colMeans(ests)
apply(ests, 2, var)

#Both sets of estimators appear to be approximately unbiased, and the simulations 
#suggest that they may be consistent. However, the relative efficiency is 
#reversed—now the least-squares estimators are less efficient than the 
#least-absolute-errors estimators. Once again, efficiency is a property of an 
#estimator under a specific model, not of the statistic itself. 

#Problem 3)

#a) for example, call this many times:
plot(sim.lm(n = 50, a = 3, b = 0.5, rdist = rnorm.mix))
#The command shown shows a cloud of points centered around a line (not drawn) 
#with intercept 3 and slope 1/2. In some trials, there are some points in the 
#lower-right corner that are far removed from the rest of the data. These 
#are outliers both in the sense of being removed from the rest of the data 
#and from being actually created by a different process—their disturbances 
#are from the contaminating distribution.

#b) Repeating the problem 1 code with rdist = rnorm.mix:

#Least-squares
ests <- sim.lm.ests(1000, n = 50, a = 3, b = 1/2, rdist = rnorm.mix)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 100, a = 3, b = 1/2, rdist = rnorm.mix)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 1000, a = 3, b = 1/2, rdist = rnorm.mix)
colMeans(ests)
apply(ests, 2, var)

#Least-absolute errors
ests <- sim.lm.ests(1000, n = 50, a = 3, b = 1/2, estfun = rq, rdist = rnorm.mix)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 100, a = 3, b = 1/2, estfun = rq, rdist = rnorm.mix)
colMeans(ests)
apply(ests, 2, var)
ests <- sim.lm.ests(1000, n = 1000, a = 3, b = 1/2, estfun = rq, rdist = rnorm.mix)
colMeans(ests)
apply(ests, 2, var)

#With this form of data contamination / outliers, neither set of estimators is unbiased 
#or consistent. Both tend to produce slope estimates that are too low—the 
#line is being "pulled down" by the outlying points in the lower right. 
#However, the least-absolute-errors estimators are much more robust than 
#the least-squares estimators—they are closer to the true values on 
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


##############################
##############################
##############################
#Exercise Set 7-1
#Problem 1)
#d) 
s.mat <- norm.samps(0, 1, 25, 10000)
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
#We can either simulate the samples (here, storing them in a matrix) 
#and take their means:
  
sim.mat <- matrix(rnorm(40000, mean = 100, sd = 2), ncol = 4, 	nrow = 10000)
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

sim.mat <- matrix(rnorm(40000, mean = 101, sd = 2), ncol = 4, 	nrow = 10000)
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

sim.mat <- matrix(rnorm(40000, mean = 102, sd = 2), ncol = 4, 	nrow = 10000)
sim.means <- rowMeans(sim.mat)
ps <- sapply(sim.means, FUN = twotailed.p.normal, mu = 100, 	stand.err = 1)

hist(ps) 
mean(ps < 0.05) 
mean(ps < 0.1) 

#Again, the distribution of p values shows a concentration of low values, 
#even more pronounced than in part (e). 

#2g) To simulate normal samples of size 16 from a Normal(101,4) distribution and 
#test the null hypothesis that mu=100, use the following:

sim.mat <- matrix(rnorm(160000, mean = 101, sd = 2), ncol = 16, 	nrow = 10000)
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
#Exercise Set 7-4

#Problem 2)
#a)

library(MASS)

ps <- many.outcome.sim(20, 3, .7, 10000)
sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant for each measurement.
mean(rowMeans(sigs) > 0) #significant result for at least one of the measurements.

#b) Here are a couple of other possibilities:

ps <- many.outcome.sim(20, 10, .7, 10000)
sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant for each measurement.
mean(rowMeans(sigs) > 0) #significant result for at least one of the measurements.

ps <- many.outcome.sim(20, 3, .1, 10000)
sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant for each measurement.
mean(rowMeans(sigs) > 0) #significant result for at least one of the measurements.

#Increasing the number of tests or lowering the correlation between them increases
#the probability that at least one of the tests will produce a type I error,
#also called the familywise error rate.

#Problem 3)

ps <- serial.testing.sim()

sigs <- ps < .05 #Which entries are significant?
colMeans(sigs) # proportion of tests that were significant for each measurement.
mean(rowMeans(sigs) > 0) #significant result for at least one of the measurements.

#The proposed procedure leads to an incorrect rejection of the null 
#hypothesis about 11-12% of the time, which grows worse with more repeated testing. 


##############################
##############################
##############################
#Exercise set 7-5

#1)

#The following block of code provides one way to produce the necessary plot, 
#assuming that the ps.1sz() function is defined:

n <- 25
d <- seq(-2, 2, length.out = 101)
pow <- numeric(length(d))
for(i in 1:length(d)){
  pow[i] <- ps.1sz(d[i], n)
}
plot(d, pow, ylim = c(0,1), type = "l", ylab = "Power")

#2)
# few possible parameter choices.
wc.1sz( .3, 50, .05) 
wc.1sz( .5, 50, .05) 
wc.1sz( .1, 50, .05) 
wc.1sz( .3, 25, .05) 
wc.1sz( .3, 50, .01)

#b)

true.d <- 0.3
ns <- seq(5, 200, by = 5)
pows <- numeric(length(ns))
est.ds <- numeric(length(ns))

#Save power and estimated effect sizes.
for(i in 1:length(ns)){
  wc <- wc.1sz(true.d, ns[i])
  est.ds[i] <- wc[2]
  pows[i] <- wc[3]
}

#First Plot: Cursed effect size estimate as a function of sample #size.
plot(ns, est.ds, type = "l", lty = 2, lwd = 2, ylim = c(0, max(est.ds)), ylab = "d", xlab = "n")
lines(ns, rep(true.d, length(ns)), lwd = 2)
legend("topright", lwd = c(2,2), lty = c(2,1), legend = c("Cursed", "True"))

#Second Plot: Size of the winner's curse effect as a function of #power.
curse.size <- est.ds - true.d
plot(pows, curse.size, type = "l", lwd = 2, xlab = "Power", ylab = "Size of Winner's Curse Effect")


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

mean(sim.Wald.B(0, 0, n = 10) < .05)
mean(sim.Wald.B(0, 0.1, n = 10) < .05)
mean(sim.Wald.B(0, 0.2, n = 10) < .05)
mean(sim.Wald.B(0, 0, n = 50) < .05)
mean(sim.Wald.B(0, 0.1, n = 50) < .05)
mean(sim.Wald.B(0, 0.2, n = 50) < .05)
mean(sim.Wald.B(0, 0, n = 100) < .05)
mean(sim.Wald.B(0, 0.1, n = 100) < .05)
mean(sim.Wald.B(0, 0.2, n = 100) < .05)

#For fun, here's what happens when you compare the Wald statistic to
#the appropriate t distribution instead of a standard normal
mean(sim.Wald.B(0, 0, n = 10, pfun = pt, df = 8) < .05)
mean(sim.Wald.B(0, 0.1, n = 10, pfun = pt, df = 8) < .05)
mean(sim.Wald.B(0, 0.2, n = 10, pfun = pt, df = 8) < .05)
mean(sim.Wald.B(0, 0, n = 50, pfun = pt, df = 48) < .05)
mean(sim.Wald.B(0, 0.1, n = 50, pfun = pt, df = 48) < .05)
mean(sim.Wald.B(0, 0.2, n = 50, pfun = pt, df = 48) < .05)
mean(sim.Wald.B(0, 0, n = 100, pfun = pt, df = 98) < .05)
mean(sim.Wald.B(0, 0.1, n = 100, pfun = pt, df = 98) < .05)
mean(sim.Wald.B(0, 0.2, n = 100, pfun = pt, df = 98) < .05)


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


##############################
##############################
##############################
#Exercise set 10-1

#Problem 1)

#To begin, let's set some parameters and simulate data we’ll use in parts (a-c).

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
#get.1.samp.norm <- function(z, known.sd = 1, prior.mn = 0, prior.sd = 1){
#  accepted <- FALSE
#  max.like <- exp(sum(log(dnorm(z, mean = mean(z), sd = known.sd))))
#  while(accepted == FALSE){
#    cand <- rnorm(1, prior.mn, prior.sd)
#    like <- exp(sum(log(dnorm(z, mean = cand, sd = known.sd))))
#    crit <- like / max.like
#    xunif <- runif(1,0,1)
#    if(xunif <= crit){accepted <- TRUE}
#  }
#  cand
#}

#Wrapper for get.1.samp.norm() that gets rejection sample from posterior of desired size.
#reject.samp.norm <- function(z, known.sd = 1, prior.mn = 0, prior.sd = 1, nsamps = 10000){
#  samps <- numeric(nsamps)
#  for(i in seq_along(samps)){
#    samps[i] <- get.1.samp.norm(z, known.sd, prior.mn, prior.sd)
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

##############################
##############################
##############################
#Exercise set Post-1.

#Problem 1)
par(mfrow = c(2,2))
lm.fit1 <- lm(y1 ~ x1, data = anscombe)
plot(lm.fit1)
lm.fit2 <- lm(y2 ~ x2, data = anscombe)
plot(lm.fit2)
lm.fit3 <- lm(y3 ~ x3, data = anscombe)
plot(lm.fit3)
lm.fit4 <- lm(y4 ~ x4, data = anscombe)
plot(lm.fit4)
par(mfrow = c(1,1))

##############################
##############################
##############################
#Exercise set Post-2.

#Problem 1)

#If necessary, install and load gvlma
if(!("gvlma" %in% installed.packages())){install.packages("gvlma")}
library(gvlma)

#Run the tests
lm.fit1 <- lm(y1 ~ x1, data = anscombe)
gvlma(lm.fit1)
lm.fit2 <- lm(y2 ~ x2, data = anscombe)
gvlma(lm.fit2)
lm.fit3 <- lm(y3 ~ x3, data = anscombe)
gvlma(lm.fit3)
lm.fit4 <- lm(y4 ~ x4, data = anscombe)
gvlma(lm.fit4)

#As expected from the plot, tests using the first dataset reveal no 
#clear reasons for concern. But remember that the samples size in this 
#example is very small, which means that power to detect deviations is low. 

#The second model shows that the link function test, which is intended to 
#detect departures from linearity, returns a low p value. 
#This makes sense: the data clearly fit a curve and not a line. 

#The third dataset reveals trouble with the normality assumption. 
#This isn't as informative as looking at the plot, which would likely 
#lead us to investigate whether the single point falling off the line is 
#some sort of error. But at least the test has returned an alarm when it should. 

#The fourth result is more disquieting---the tests detect no problems 
#with the assumptions, even though the plot suggests something is wrong. 

#In short, the tests are useful in conjunction with the plots, but they do not replace them.

##############################
##############################
##############################
#Exercise set Post-3.

#Problem 1)

#In this problem, we relate simple and multiple linear regression by
#showing how multiple regression estimates can be obtained from a 
#series of simple linear regressions.

#a) 
mod.fit <- lm(mpg ~ am + hp, data = mtcars)
summary(mod.fit)

#b) 
resid.am.hp <- lm(am ~ hp, data = mtcars)$residuals #1
resid.mpg.hp <- lm(mpg ~ hp, data = mtcars)$residuals #2
resid.mod.hp <- lm(resid.mpg.hp ~ resid.am.hp) #3
summary(resid.mod.hp)
#the slope estimate for the residuals of am on hp is the same as the
#slope estimate for am in part (a)

#c)
resid.hp.am <- lm(hp ~ am, data = mtcars)$residuals #1
resid.mpg.am <- lm(mpg ~ am, data = mtcars)$residuals #2
resid.mod.am <- lm(resid.mpg.am ~ resid.hp.am) #3
summary(resid.mod.am)
#the slope estimate for the residuals of hp on am is the same as the
#slope estimate for hp in part (a)

#d)
mean(mtcars$mpg) - mean(mtcars$am)*5.277 - mean(mtcars$hp)*(-0.05888)
#This gives the intercept from (a)

#Problem 2)
#a)
t.test(mtcars$mpg ~ mtcars$am, var.equal = TRUE)
mod <- lm(mtcars$mpg ~ mtcars$am)
summary(mod)
#The t value (up to the sign) and p value are the same. When
#the Wald statistic (labeled by lm() as the t value) 
#is compared with a t distribution (as it is
#by lm()), the classic (i.e. equal variances) t-test is a special
#case of simple linear regression.

#b) 
cor.test(anscombe$x1, anscombe$y1)
summary(lm(y1 ~ x1, data = anscombe))
#The t statistics and p values are the same for simple linear regression
#and the correlation test. Moreover, the correlation estimate is equal to
#the slope estimate multiplied by sd(x) / sd(y) (as suggested in ch. 5).
lm(y1 ~ x1, data = anscombe)$coef[2] * sd(anscombe$x1) / sd(anscombe$y1)
cor.test(anscombe$x1, anscombe$y1)$estimate

#c)
anova.fit <- aov(weight ~ group, data = PlantGrowth)
summary(anova.fit)
lr.fit <- lm(weight ~ group, data = PlantGrowth)
summary(lr.fit)
#The F value and p value for the overall fit (see the last line
#of the lm() summary table) are the same for both models.


##############################
##############################
##############################
#Exercise set Post-4.

#install and load the car package if necessary
if(!("car" %in% installed.packages())){install.packages("car")}
library(car)

#Problem 1)
probit.fit <- glm(volunteer ~ extraversion + neuroticism + sex, data = Cowles, family = binomial("probit"))
summary(probit.fit)

logit.fit <- glm(volunteer ~ extraversion + neuroticism + sex, data = Cowles, family = binomial("logit"))
summary(logit.fit)

#The results are substantively rather similar. (The estimates change a little, but the coefficients
#also have different interpretations in the two models.)

#Problem 2)
#a) 

#Simulate data
n <- 100
a <- -1
b <- 0.2
x <- rnorm(n, 0, 2)
eps <- rnorm(n, 0, 1)
z <- a + b*x + eps
y <- as.numeric(z > 0)

#Fit the model
prob.fit <- glm(y ~ x, family = binomial("probit"))
summary(prob.fit)

#If you increase n to be very large (100000 or more), then the estimates
#will be quite close to the specified parameter values. This is suggestive
#of consistency. We know that they are asymptotically consistent because
#they are maximum-likelihood estimates (and the weak conditions required
#for asymptotic consistency of maximum-likelihood estimators are met).

#b) Now replace the line from part (a) that generates the disturbances
#to introduce heteroskedasticity

#Simulate data
n <- 100000
a <- -1
b <- 0.2
x <- rnorm(n, 0, 2)
eps <- rnorm(n, 0, 1 + max(-1, 0.1*x))
z <- a + b*x + eps
y <- as.numeric(z > 0)

#Fit the model
prob.fit <- glm(y ~ x, family = binomial("probit"))
summary(prob.fit)

#Changing the 0.1 in
#eps <- rnorm(n, 0, 1 + max(-1, 0.1*x))
#changes the severity of the heteroskedasticity--larger values
#mean more heteroskedasticity.
#The estimators are not consistent and can be badly biased in the
#presence of heteroskedasticity for the disturbances of the latent variable.
#This is in contrast to the linear regression case, where heteroskedasticity
#can cause inefficiency but the estimators remain consistent.

##############################
##############################
##############################
#Exercise set Post-5.

#Problem 2)

#Here is code to carry out the simulations.

#Set parameters
alpha <- 3
beta <- 1/2
eps.sd <- sqrt(1/2)
re.sd <- 1
yrs <- 10
n.sims <- 10000

#Initialize variables
ints <- numeric(n.sims)
slopes <- numeric(n.sims)

#Simulate datasets and save least-squares estimates
for(i in 1:n.sims){
  x <- rep(anscombe$x1, yrs)
  rand.ints <- rnorm(length(anscombe$x1), 0, re.sd)
  y <- alpha + beta*x + rep(rand.ints, yrs) + rnorm(length(x), 0, eps.sd)
  mod.fit <- lm(y ~ x)
  ints[i] <- mod.fit$coefficients[1]
  slopes[i] <- mod.fit$coefficients[2]
}

#Plot and summarize estimates
hist(ints)
summary(ints)
sd(ints)

hist(slopes)
summary(slopes)
sd(slopes)

#The least-squares estimates are unbiased. With these parameters, the 
#standard deviation of the intercept estimates (which estimates the 
#standard error of the estimator) is about 0.93, and the standard 
#deviation of the slope estimates is about 0.10. These standard 
#deviations are roughly in agreement with the mixed-model standard 
#error estimates reported in the main text. They are much larger 
#than the standard error estimates from simple linear regression. 
#Ignoring dependence among the observations causes us to overestimate 
#the amount of information we have, leading to standard error 
#estimates that are too small.

