#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 3.

########################
########################
########################
#Exercise set 3-1

#Problem 1
# a) Here is one way you could use equations 3.6 and 3.7 in R.

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
