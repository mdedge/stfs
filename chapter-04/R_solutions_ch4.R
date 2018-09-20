#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 4.

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
