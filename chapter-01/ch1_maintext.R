#####
#Chapter 1, main text code.
#Statistical Thinking from Scratch

#Fit a simple linear regression model.
mod.fit <- lm(y1 ~ x1, data = anscombe)

#Plot the data and draw a line through it.
plot(anscombe$x1, anscombe$y1)
abline(mod.fit)

#Look at the model output.
summary(mod.fit)

#Now try a second model
mod.fit2 <- lm(y3 ~ x3, data = anscombe)
summary(mod.fit2) #The output is nearly identical

#The data look very different, and strange.
plot(anscombe$x3, anscombe$y3)
abline(mod.fit2)


