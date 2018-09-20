#9/20/18 by M.D. Edge, medge3@gmail.com.
#Code to complete R exercises in Statistical Thinking from 
#Scratch, chapter 2.

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


#Problem 3

#To install and load stfspack, use
install.packages("devtools")
library(devtools) 
#Next, install and load the package.
install_github("mdedge/stfspack")
library(stfspack)

