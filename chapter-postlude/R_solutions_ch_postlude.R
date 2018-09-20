#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, postlude chapter.

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

