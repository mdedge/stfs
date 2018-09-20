#########
#Statistical Thinking from Scratch
#Chapter 2, main text code

#R can do arithmetic computations.
3+4
(9*8*7*sqrt(6))/3

#You can assign variables particular values, for
#example here, assigning the variable x the value 7.
#Assignment works with either <- or =
x <- 7
x = 7

#To see the value assigned to x, type
x

#You can now use x in computations, for example
x*7

#R is case sensitive, and using an uppercase X will give you an error
X*7

#Commands can be spread over multiple lines:
(1+3+5)/(2+4+6
)

#Tutorial: The iris data

#########################
#Basic functions for exploring data

#Use head() to see the first few rows of a dataframe or matrix.
#Here we'll use the built-in iris dataframe
head(iris)

#head() has a second argument, n, that tells R how many rows to include:
head(iris, n = 10)

#When functions have multiple arguments, you don't have to name
#them explicitly if they come in the order specified on the 
#help page for the function (but you can still name them
#if you want). If you switch the order, then the names
#are necessary. For example, all of these produce the
#same output:
head(iris, 10)
head(iris, n = 10)
head(x = iris, 10)
head(x = iris, n = 10)
head(n = 10, x = iris)
#But calling
head(10, iris)
#gives an error

#summary() is a great function for exploring datasets
#as well as the objects produced by statistical analyses.
#Here it is on the iris data:
summary(iris)

#Use the $ operator to pull specific objects from dataframe or list,
#as in
iris$Sepal.Length 
#which returns just the sepal length variable.
summary(iris$Sepal.Length)

#Compute the sample mean and median
mean(iris$Sepal.Length)
median(iris$Sepal.Length)

#Histograms provide a visual summary of the distribution
#of a sample of data
hist(iris$Sepal.Length)

#Default R plots are often not particularly nice, but a
#lot can be done to improve them. For example, 
hist(iris$Sepal.Length, xlab = "Sepal Length")
#Adds a label to the x axis that looks nicer. There are
#many other optional arguments to hist (and other plotting
#functions), as well as graphical parameters that
#can be set with par().


#Extracting data

#Extracting data--This computes the mean of the
#sepal length variable only for rows where the
#species name is "setosa"
mean(iris$Sepal.Length[iris$Species=="setosa"]) 
#Note that the double-equal sign == checks to see
#whether two objects are equal, whereas a single equals
#sign assigns the variable on the left to have
#the value on the right.

#There are other ways of extracting data; see
#appendix B for a more complete account.


########################
#for() loops

#for() loop example:
for(i in 1:3){
  print(i)
}

#This is exactly the same as
i <- 1
print(i)
i <- 2 
print(i)
i <- 3 
print(i)

#The for() loop tells R to reassign i to equal each entry
#of the vector after "in" serially, and then to execute
#the commands in brackets. Here is another example
#that prints the mean sepal length of each species:
for(i in unique(iris$Species)){
  print(mean(iris$Sepal.Length[iris$Species == i]))
}

#Here, the unique() command makes a vector containing
#each species name in the iris$Species vector exactly once
unique(iris$Species)

#The for() loop works by looping over each entry. It's the
#same as
i <- unique(iris$Species)[1]
mean(iris$Sepal.Length[iris$Species == i])
i <- unique(iris$Species)[2]
mean(iris$Sepal.Length[iris$Species == i])
i <- unique(iris$Species)[3]
mean(iris$Sepal.Length[iris$Species == i])

#############
#An example function

#Here is a function that does what the for() loop did,
#letting us substitute any variable we want for iris$Sepal.Length
#(which is in the role of x) and iris$Species (which is abstracted
#as y):
conditional.mean <- function(x, y){
  for(i in unique(y)){
    print(mean(x[y == i]))
  }
}

#Once the function is defined, we can replicate what we got before
#with
conditional.mean(x = iris$Sepal.Length, y = iris$Species)

#And we can also substitute other vectors in easily, without
#repeating all the code in the function definition
conditional.mean(x = iris$Sepal.Width, y = iris$Species)

#This function is an example to show how you can define your
#own functions, but you wouldn't want to use this one in
#practice. Two better functions that would do this particular job
#are tapply() and aggregate().

tapply(iris$Sepal.Length, iris$Species, mean)
aggregate(iris$Sepal.Length, list(iris$Species), mean)

#These functions are more flexible, faster with large datasets, and
#give convenient labels. Particularly, when you are starting in R, 
#it's good to search for functions that already exist for the 
#tasks you need to do before defining your own.


##################
#More exploratory data analysis
#A basic boxplot
boxplot(iris$Sepal.Length ~ iris$Species)
title(xlab = "Species", ylab = "Sepal Length") #Add axis labels

#A scatterplot
plot(iris$Sepal.Length, iris$Sepal.Width)

#But this doesn't show us which species is which.
#To show the species and add better labels, use
plot(iris$Sepal.Length, iris$Sepal.Width, pch = as.numeric(iris$Species), 
     xlab = "Sepal Length", ylab = "Sepal Width")
#And add a legend
legend("topright", pch = c(1,2,3), legend = c("setosa", "versicolor", "virginica"))

#Not shown in the book, you can make it easier to read by adding colors
pal <- c("Dark Green", "Orange", "Purple")
plot(iris$Sepal.Length, iris$Sepal.Width, pch = as.numeric(iris$Species), col = pal[as.numeric(iris$Species)],
     xlab = "Sepal Length", ylab = "Sepal Width")
#And add a legend
legend("topright", pch = c(1,2,3), col = pal, legend = c("setosa", "versicolor", "virginica"))

#c() concatenates its arguments into a vector:
c(1,2,3)

