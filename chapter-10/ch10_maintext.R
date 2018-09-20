#############################
#Chapter 10, main text code
#Statistical Thinking from Scratch

if(!("MCMCpack" %in% installed.packages())){
  install.packages("MCMCpack")
}
library(MCMCpack)

#Fit Bayesian linear regression models with and without slopes.
y <- anscombe$y1
x <- anscombe$x1
reg0 <- MCMCregress(y~1, b0 = 0, B0 = 1/100, marginal.likelihood = "Laplace")
reg1 <- MCMCregress(y~x, b0 = 0, B0 = 1/100, marginal.likelihood = "Laplace")

#Compute a Bayes factor.
summary(BayesFactor(reg1, reg0))

