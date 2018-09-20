#############################
#Postlude, main text code
#Statistical Thinking from Scratch

#Essentially identical regression fits for all four
#anscombe datasets.
mod1 <- lm(anscombe$y1 ~ anscombe$x1)
summary(mod1)
mod2 <- lm(anscombe$y2 ~ anscombe$x2)
summary(mod2)
mod3 <- lm(anscombe$y3 ~ anscombe$x3)
summary(mod3)
mod4 <- lm(anscombe$y4 ~ anscombe$x4)
summary(mod4)

#But look at the plots
par(mfrow = c(2,2))
plot(anscombe$x1, anscombe$y1)
abline(mod1)
plot(anscombe$x2, anscombe$y2)
abline(mod2)
plot(anscombe$x3, anscombe$y3)
abline(mod3)
plot(anscombe$x4, anscombe$y4)
abline(mod4)

par(mfrow = c(1,1))

#Test for assumptions
if(!("gvlma" %in% installed.packages())){
  install.packages("gvlma")
}
library(gvlma)

gvlma(mod1) 


