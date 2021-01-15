#9/7/16, by Doc Edge
#Appendix B code from How Statistics Works

#Covers variables, data structures (vectors, matrices, data frames,
# lists) data creation, logic, subsetting, 


#Variables
p <- pnorm(-2)
p

p < 0.05
sig <- (p < 0.05)
sig

#Other logical operators:
# < <= > >= == != ! %in% | &

this.is.an.object.name <- "This, in contrast, is a datum."
this.is.an.object.name

#Creating vectors
x <- c(1,2,4,8)
y <- c("So What", "Freddie Freeloader", "Blue in Green")
z <- c(TRUE, TRUE, FALSE, FALSE)


#Other useful commands for creating vectors
#include seq() and the colon operator :, as in seq(10,13,1); 10:13

#Subsetting vectors
x[3] #use index
x[-4] #use minus to exclude index (or vector of indices)
x[c(1,3)] #use vector of indices

#Subsetting with logical vectors.
x
x[x > 3]
x > 3

x <- c(1,2,4,8)
w <- c(1,3,9,27)

x[x > 3 & w > 20]

x > 3
w > 20
x > 3 & w > 20

x[x > 3 | w > 20]

#Examples of logical operations
x <- c(FALSE, FALSE, TRUE, TRUE)
y <- c(FALSE, TRUE, FALSE, TRUE)

x & y #"and"
x | y #"or"
x == y#"equal"
x != y #"not equal"
!(x & y) #"not"



#Matrices

vec <- 1:12
vec
mat <- matrix(vec, nrow = 3, ncol = 4)
mat

mat[2,3]
mat[1,]
mat[,4]
mat[3, mat[3,] > 7]
mat[1, mat[3,] == 6]

mat



####Data frames
head(iris)
iris$Sepal.Length[1:6]
iris$Species[1:6]

#Coercion
iris.char <- iris
iris.char$Species <- as.character(iris.char$Species)
head(iris.char)
iris.char$Species[1:6]



#######Lists
my.list <- list(c("a","b","c"), matrix(1:12, nrow = 3), iris)
my.list[[2]]
my.list[[2]][3,1]
my.list <- list(char.vec = c("a","b","c"), num.mat = matrix(1:12, nrow = 3), iris.dat = iris)
my.list$num.mat


###############
#Useful functions
#Getting help
help(mean)
?(mean)
help.search("mean")
str(iris)
ls()

#Data creation.
#c(), 
x <- c(1,2)
y <- c(3,4)
z <- c(x,y)
z

#from:to, 
x <- 1:10
x

#matrix()

x <- 1:12
matrix(x, nrow = 3, ncol = 4)
matrix(x, nrow = 3, ncol = 4, byrow = TRUE)
matrix(nrow = 3, ncol = 4)

#cbind(), rbind()

x <- c(1,2,3)
y <- c(4,5,6)
z <- c(7,8,9)
rbind(x,y,z)
cbind(x,y,z)


#Size of objects
x <- c(1,2,3,4,5)
length(x)

dim(iris)

#three ways to get number of rows
dim(iris)[1]
nrow(iris)
length(iris[,1])



#Math
x <- iris$Sepal.Length
sum(x)
mean(x)
var(x)
sd(x)
median(x)

#plotting
plot(anscombe$x1, anscombe$y1)
#a few improvements
plot(anscombe$x1, anscombe$y1, ylab = "y", xlab = "x", pch = 20, bty = "n")

hist(iris$Sepal.Width)
#a few improvements
hist(iris$Sepal.Width, xlab = "Sepal Width", main = "", breaks = 20, col = "grey")

#demo("graphics")

#also boxplot(), barplot(), and many others.
#set graphics parameters with par() to change margins
#text size, layout, and dozens of other features.

#Linear models
my.lm <- lm(anscombe$y1 ~ anscombe$x1)
summary(my.lm)

plot(anscombe$x1, anscombe$y1, ylab = "y", xlab = "x", pch = 19, bty = "n")
abline(my.lm)

#Also see glm() for generalized linear models,
#the lme4 and nlme packages for mixed models,
#and gee and geepack for GEE models.

#Random numbers and probability distributions
#rnorm(), pnorm(), dnorm(), qnorm().
#similar syntax for many distributions.

z <- rnorm(1000, mean = 100, sd = 10)
hist(z)

###################################
#Programming

#for() iterating through vectors, lists, or matrices
#if() #executing a task conditionally
#function() #defining a sequence of commands that applies to a variety of input arguments
#return()



#Function that takes two numbers x and y as input
#and returns their sum.
mysum <- function(x, y){
  if(!is.numeric(x) | !is.numeric(y)){
    stop("These are not numbers. Supply numbers please.")
  }
  z <- x + y
  return(z)
}


#x and y are arguments. They could be set to have defaults.
#return() is not strictly necessary, could just write z.

mysum(3,4)

#Here are three functions that do the same thing.
#return() is not strictly necessary but is sometimes
#helpful, especially when functions are exited early.
mysum <- function(x, y){
  z <- x + y
  z
}

mysum <- function(x, y){
  return(x + y)
}

mysum <- function(x, y){
  x + y
}


##########
#If statements.

mysum <- function(x, y, doubleit = FALSE){
  z <- x + y
  if(doubleit == TRUE){
    z <- z*2
  }
  return(z)
}

mysum(2,2)
mysum(2, 2, doubleit = TRUE)



#for() loops

#A function to sum the entries in a vector x.
slowsum <- function(x){
  s <- 0
  for(i in x){
    s <- s + i
  }
  return(s)
}

sum(1:100)
slowsum(1:100)

#for() loops can be slower than vectorized operations,
#particularly when the vectorized operation is a heavily optimized
#built-in function.
vec <- rnorm(10000000, mean = 0, sd = 1)
system.time(sum(vec))
system.time(slowsum(vec))




#apply()
mat <- matrix(1:100, nrow = 10, ncol = 10)
mat

mymeans <- numeric(10)
for(i in 1:10){
  mymeans[i] <- mean(mat[,i])
}

mymeans

apply(mat, 2, mean)



#Here's a speed test where for() is faster than apply().
mat <- matrix(rnorm(10^6), nrow = 2*10^5)

#Function that uses a for() loop to compute row means.
for.rowmeans<- function(x){
  rms <- numeric(nrow(x))
  for(i in 1:nrow(x)){
    rms[i] <- mean(x[i,])
  }
  rms
}

system.time(for.rowmeans(mat))
system.time(apply(mat, 1, mean))


########################################
#File input/output
write.table(iris, "iris.txt")

write.table(iris, "iris.csv", quote = FALSE, sep = ",", row.names = FALSE)

iris.dat <- read.table("iris.txt")

iris.dat <- read.table("iris.csv")
head(iris.dat)

iris.dat <- read.table("iris.csv", sep = ",", header = TRUE)
head(iris.dat)



############################
#Saving graphics
tiff("fig1_1.tif", width = 6, height = 4.5, units = "in", res = 600, compression = "lzw")
plot(anscombe$x1, anscombe$y1, xlab = "Fertilizer Consumption (kg/hectare)", ylab = "Cereal Yield (100 kg/hectare)", pch = 19)
dev.off()


