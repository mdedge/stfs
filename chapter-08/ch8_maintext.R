#############################
#Chapter 8, main text code
#Statistical Thinking from Scratch

#Drawing one bootstrap sample
cbind(anscombe$x1, anscombe$y1)[order(anscombe$x1),]
set.seed(8675309)
cbind(anscombe$x1, anscombe$y1)[sample(1:11, replace = TRUE),]

#Draw a bootstrap sample of entries from a vector or rows #from a matrix.
boot.samp <- function(x){
  #If x is a vector, convert it to a matrix with one column.
  if(is.null(dim(x))){
    x <- matrix(x, ncol = 1)
  }
  n <- nrow(x)
  boot.inds <- sample(1:n, replace = TRUE)
  x[boot.inds,]
}

#Compute the method of moments estimator of the slope 
#for simple linear regression. (that is, the least-squares
#slope. You can also get this with 
#lm(y ~ x)$coefficients[2].)
#(equation 8.3).
beta.mm <- function(x, y){
  n <- length(x)
  beta.est <- (sum(x*y) - (1/n)*sum(x)*sum(y)) / 
    (sum(x^2) - (1/n)*sum(x)^2)
  return(beta.est)
}

set.seed(8675309) #Optional. 
B <- 10000
boot.dist <- numeric(B)
dat <- cbind(anscombe$x1, anscombe$y1)
for(i in 1:B){
  samp <- boot.samp(dat)
  boot.dist[i] <- beta.mm(samp[,1], samp[,2])
}

hist(boot.dist) #display the bootstrap distribution.
sd(boot.dist) #bootstrap standard error
quantile(boot.dist, c(0.025, 0.975)) #percentile interval
b.est <- beta.mm(anscombe$x1, anscombe$y1) 
2*b.est - quantile(boot.dist, c(0.975, 0.025)) #pivotal interval

#Permutation testing
#One permutation sample
set.seed(8675309)
cbind(anscombe$x1, anscombe$y1[sample(1:11, replace = FALSE)])[ order(anscombe$x1),]

#Permute the columns of a matrix independently.
perm.samp <- function(x){
  samp <- apply(x, 2, sample)
  return(samp)
}

#Compute a permutation distribution for the slope estimate
set.seed(8675309) #Optional. 
nperms <- 10000
perm.dist <- numeric(nperms)
dat <- cbind(anscombe$x1, anscombe$y1)
for(i in 1:nperms){
  samp <- perm.samp(dat)
  perm.dist[i] <- beta.mm(samp[,1], samp[,2])
}

hist(perm.dist) #display the permutation distribution.

b.orig <- beta.mm(anscombe$x1, anscombe$y1)
b.orig
mean(abs(perm.dist) >= b.orig) #p value

