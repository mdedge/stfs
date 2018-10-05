#By M.D. Edge, 9/19/2018.
#This script makes the R figures (i.e. most of the figures) from
#Statistical Thinking from Scratch. Image files will be saved
#in the working directory. To make pdfs, set the pdf_ind variable
#below to 1; for tiffs, set it to 0.


if(!("animation" %in% installed.packages())){install.packages("animation")}
if(!("grDevices" %in% installed.packages())){install.packages("grDevices")}
library(animation)
library(grDevices)
plotlabx <- "Fertilizer Consumption (kg/hectare)"
plotlaby <- "Cereal Yield (100 kg/hectare)"
figwidth <- 6 #6
figheight <- 4.5 #4.5
un <- "in"
reso <- 600 #pixels/in
cex.pts <- 0.6
cex.labs <- 1.2
cex.axs <- 1.1
mgp.set <- c(2.3, .8, 0)
mar.df <- c(3.5, 3.5, 1, 1)
cex.labs.fx <- 1.4
mar.fx <- c(5,5,2,1)
fontfam <- "sans"
type.plot <- "cairo"

pdf_ind <- 0 #0 to make tiffs, 1 to make pdfs

#Function to write to either pdf or tiff depending on an inpute 
#variable pdf_ind.
pdf_or_tiff <- function(pdf_ind, name, ...){
  if(pdf_ind){pdf(paste(name, ".pdf", sep = ""), ...)}
  if(!pdf_ind){tiff(paste(name, ".tif", sep = ""), ..., res = reso, units = un, compression = "lzw", type = type.plot)}
}

########################################
########################################
########################################
#Chapter 1

#Figure 1-1
pdf_or_tiff(pdf_ind, "fig1_1", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
plot(anscombe$x1, anscombe$y1, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs)
dev.off()

#Figure 1-2
pdf_or_tiff(pdf_ind, "fig1_2", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
ls.fit <- lm(y1 ~ x1, data = anscombe)
plot(anscombe$x1, anscombe$y1, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs)
abline(ls.fit)
dev.off()

#Figure 1-3
pdf_or_tiff(pdf_ind, "fig1_3", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
ls.fit <- lm(y3 ~ x3, data = anscombe)
plot(anscombe$x3, anscombe$y3, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs)
abline(ls.fit)
dev.off()


########################################
########################################
########################################
#Chapter 2

#Figure 2-1 code shown in main text; this is slightly modified.

pdf_or_tiff(pdf_ind, "fig2_1", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, mgp = mgp.set)
hist(iris$Sepal.Length, xlab = "Sepal Length", main = "",
     cex.lab = cex.labs, cex.axis = cex.axs, col = "grey55", border = "white")
dev.off()

#Figure 2-2 code shown in main text.

pdf_or_tiff(pdf_ind, "fig2_2", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, mgp = mgp.set, bty = "n")
boxplot(iris$Sepal.Length ~ iris$Species,  cex.axis = cex.axs, pars = list(boxwex = .5, staplewex = 0.5, outwex = 0.5))
title(xlab = "Species", ylab = "Sepal Length", cex.lab = cex.labs)
dev.off()

#Figure 2-3 code shown in main text.

pdf_or_tiff(pdf_ind, "fig2_3", width = figwidth, height = figwidth*0.85)
par(mar = mar.df, las = 1, family = fontfam,  mgp = mgp.set)
plot(iris$Sepal.Length, iris$Sepal.Width, pch = as.numeric(iris$Species), xlab = "Sepal Length", 
     ylab = "Sepal Width", bty = "n", cex.lab = cex.labs, cex.axis = cex.axs)
legend("topright", pch = c(1,2,3), legend = c("setosa", "versicolor", "virginica"), bty = "n", cex = cex.labs)
dev.off()



########################################
########################################
########################################
#Chapter 3

#Figure 3-1
pdf_or_tiff(pdf_ind, "fig3_1", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
plot(anscombe$x1, anscombe$y1, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs)
dev.off()

#Figure 3-2
pdf_or_tiff(pdf_ind, "fig3_2", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
plot(anscombe$x1, anscombe$y1, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs)
x <- seq(min(anscombe$x1)-5, max(anscombe$x1)+5, length.out=10)
y <- 3.5+0.4*x
lines(x,y)
points(anscombe$x1, 3.5 + 0.4*anscombe$x1, pch = 4)
for(i in 1:length(anscombe$x1)){
lines(c(anscombe$x1[i],anscombe$x1[i]),c(3.5 + 0.4*anscombe$x1[i], anscombe$y1[i]), lty = 2)
}
dev.off()

#Figure 3-3
pdf_or_tiff(pdf_ind, "fig3_3", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
ls.fit <- lm(y1 ~ x1, data = anscombe)
plot(anscombe$x1, anscombe$y1, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs)
abline(ls.fit)
dev.off()



########################################
########################################
########################################
#Chapter 4

#Figure 4-3
#pdf_or_tiff(pdf_ind, "fig4_3", width = figwidth, height = (4/3)*figwidth )
#par(mar = mar.fx, las = 1, family = fontfam, bty = "n")
#par(mfrow=c(2,1))
#x <- 1:6
#fx <- rep(1/6,6)
#Fx <- cumsum(fx)

#plot(x, fx, type = "h", xlab = expression(italic(x)), 
#     ylab = expression(paste(italic(f[X]), "(", italic(x), ")", sep = "")), 
#     xlim = c(0,7), ylim = c(0,1), cex.axis = cex.labs.fx, cex.lab = cex.labs.fx)
#mtext("Probability mass function", adj = 0, cex = cex.labs.fx)

#cdf.plot <- stepfun(x, c(0,Fx), f=0)
#plot.stepfun(cdf.plot, xlab = expression(italic(x)), 
#             ylab = expression(paste(italic(F[X]), "(", italic(x), ")", sep = "")), 
#             xlim = c(0,7), ylim = c(0,1),pch= 16, main = "", verticals = FALSE,
#             cex.axis = cex.labs.fx, cex.lab = cex.labs.fx)
#mtext("Cumulative distribution function", adj = 0, cex = cex.labs.fx)
#dev.off()

#Figure 4-3
pdf_or_tiff(pdf_ind, "fig4_3", width = figwidth, height = (4/3)*figwidth )
par(mar = mar.fx, las = 1, family = fontfam, bty = "n")
par(mfrow=c(2,1))
x <- 1:6
fx <- rep(1/6,6)
Fx <- cumsum(fx)

plot(x, fx, pch = 19, xlab = expression(italic(x)), 
     ylab = expression(paste(italic(f[X]), "(", italic(x), ")", sep = "")), 
     xlim = c(0,7), ylim = c(0,1), cex.axis = cex.labs.fx, cex.lab = cex.labs.fx)
lines(c(-1,8),c(0,0))
points(1:6,rep(0,6))
mtext("Probability mass function", adj = 0, cex = cex.labs.fx)
#text("{", x= .85, y = 1/12, cex = 2.4)
lines(c(1,1), c(0, 1/6), lty = 2, col = "grey55")
text(expression(italic(f[X](1))), x = .65, y = 1/12, cex = 1, col = "grey55")
lines(c(2,2), c(0, 1/6), lty = 2, col = "grey55")
text(expression(italic(f[X](2))), x = 1.65, y = 1/12, cex = 1, col = "grey55")
lines(c(3,3), c(0, 1/6), lty = 2, col = "grey55")
text(expression(italic(f[X](3))), x = 2.65, y = 1/12, cex = 1, col = "grey55")
lines(c(4,4), c(0, 1/6), lty = 2, col = "grey55")
text(expression(italic(f[X](4))), x = 3.65, y = 1/12, cex = 1, col = "grey55")
lines(c(5,5), c(0, 1/6), lty = 2, col = "grey55")
text(expression(italic(f[X](5))), x = 4.65, y = 1/12, cex = 1, col = "grey55")
lines(c(6,6), c(0, 1/6), lty = 2, col = "grey55")
text(expression(italic(f[X](6))), x = 5.65, y = 1/12, cex = 1, col = "grey55")


cdf.plot <- stepfun(x, c(0,Fx), f=0)
plot.stepfun(cdf.plot, xlab = expression(italic(x)), 
             ylab = expression(paste(italic(F[X]), "(", italic(x), ")", sep = "")), 
             xlim = c(0,7), ylim = c(0,1),pch= 16, main = "", verticals = FALSE,
             cex.axis = cex.labs.fx, cex.lab = cex.labs.fx)
mtext("Cumulative distribution function", adj = 0, cex = cex.labs.fx)
lines(c(1,1), c(0, 1/6), lty = 2, col = "grey55")
text(expression(italic(f[X](1))), x = .65, y = 1/12, cex = 1, col = "grey55")
lines(c(2,2), c(1/6, 2/6), lty = 2, col = "grey55")
text(expression(italic(f[X](2))), x = 1.65, y = 3/12, cex = 1, col = "grey55")
lines(c(3,3), c(2/6, 3/6), lty = 2, col = "grey55")
text(expression(italic(f[X](3))), x = 2.65, y = 5/12, cex = 1, col = "grey55")
lines(c(4,4), c(3/6, 4/6), lty = 2, col = "grey55")
text(expression(italic(f[X](4))), x = 3.65, y = 7/12, cex = 1, col = "grey55")
lines(c(5,5), c(4/6, 5/6), lty = 2, col = "grey55")
text(expression(italic(f[X](5))), x = 4.65, y = 9/12, cex = 1, col = "grey55")
lines(c(6,6), c(5/6, 6/6), lty = 2, col = "grey55")
text(expression(italic(f[X](6))), x = 5.65, y = 11/12, cex = 1, col = "grey55")

dev.off()


#Figure 4-4
pdf_or_tiff(pdf_ind, "fig4_4", width = figwidth, height = (4/3)*figwidth )
par(mar = mar.fx, las = 1, family = fontfam, bty = "n")
par(mfrow=c(2,1))
x <- seq(0, 5, length.out = 1000)
fx <- dexp(x)
Fx <- pexp(x)

plot(x, fx, type = "l", xlab = expression(italic(x)), 
     ylab = expression(paste(italic(f[X]), "(", italic(x), ")", sep = "")), lwd = 2,
     cex.axis = cex.labs.fx, cex.lab = cex.labs.fx)
polygon(c(0, seq(0, 1, length.out = 1000), 1), c(0, dexp(seq(0, 1, length.out = 1000)), 0),  col = "grey", border = NA)
text(expression(italic(F[X](1))), x = 1/2, y = 1/4)
mtext("Probability density function", adj = 0, cex = cex.labs.fx)

plot(x, Fx, type = "l", xlab = expression(italic(x)), 
     ylab = expression(paste(italic(F[X]), "(", italic(x), ")", sep = "")), lwd = 2,
     cex.axis = cex.labs.fx, cex.lab = cex.labs.fx)
text(expression(italic(F[X](1))), x = 1.2, y = 1/4, srt = 90, col = "grey55")
mtext("Cumulative distribution function", adj = 0, cex = cex.labs.fx)
lines(c(1,1), c(0, pexp(1)), lty = 2, col = "grey55")
dev.off()



########################################
########################################
########################################
#Chapter 5

#Figure 5-1
pdf_or_tiff(pdf_ind, "fig5_1", width = figwidth, height = figheight*.8)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
x <- seq(-10, 10, length.out = 1000)
fx1 <- dnorm(x)
fx2 <- dnorm(x, mean = 2)
plot(x, fx1, type = "l", xlab = expression(italic(x)), 
     ylab = expression(paste(italic(f[X]), "(", italic(x), ")", sep = "")), 
     lwd = 2, xlim = c(-3, 5), cex.lab = cex.labs, cex.axis = cex.axs)
lines(x, fx2, lwd = 2, lty = 2)
legend("topright", lty = c(1,2), legend = c("E(X) = 0", "E(X) = 2"), cex = cex.labs, bty = "n")
dev.off()

#Figure 5-2
pdf_or_tiff(pdf_ind, "fig5_2", width = figwidth, height = figheight*.8)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
x <- seq(-10, 10, length.out = 1000)
fx1 <- dnorm(x)
fx2 <- dnorm(x, sd = sqrt(2))
fx3 <- dnorm(x, sd = 2)
plot(x, fx1, type = "l", xlab = expression(italic(x)), 
     ylab = expression(paste(italic(f[X]), "(", italic(x), ")", sep = "")), 
     lwd = 2, xlim = c(-6, 6), cex.lab = cex.labs, cex.axis = cex.axs)
lines(x, fx2, lwd = 2, lty = 2)
lines(x, fx3, lwd = 2, lty = 3)
legend("topright", lty = c(1,2,3), legend = c("Var(X) = 1", "Var(X) = 2", "Var(X) = 4"), 
       cex = cex.labs, bty = "n")
dev.off()


# Figure 5-3

x <- seq(-4,4, length.out = 200)
y <- seq(-4,4, length.out = 200)

dbvnorm <- function(x, y, meanx=0, meany=0, sdx = 1, sdy = 1, cor = 0){
fxy <- (1/(2*pi*sdx*sdy*sqrt(1-cor^2)))*exp(-(1/(2*(1-cor^2)))*(((x-meanx)^2)/(sdx^2) + ((y-meany)^2)/(sdy^2) - 2*cor*(x-meanx)*(y-meany)/(sdx*sdy)))
return(fxy)
}

densmat <- outer(x, y, dbvnorm, meanx = 0, meany = 0, sdx = 1, sdy = 1, cor = 0)

pdf_or_tiff(pdf_ind, "fig5_3", width = figwidth, height = figwidth)
par(cex.lab = cex.labs.fx, mar = c(1,1,1,1), oma = c(1,1,1,1), las = 1, family = fontfam)
#persp(x,y,z = densmat, phi = 30, theta = 0, shade = .25, expand = 0.6, xlim = c(-4,4), border = NA, ylab = "y\n", zlab = "f(x,y)")

res = persp(x,y,z = densmat, phi = 35, theta = 0, shade = .25, d = 1, expand = 0.6, xlim = c(-4,4), border = NA, box = FALSE, zlim = c(0, max(densmat)))

xlab.pos <- trans3d(0,-4-0.2, 0, res)
text(xlab.pos$x, xlab.pos$y, expression(italic(x)), cex = 1.5 )
ylab.pos <- trans3d(-4-.3, 0, 0, res)
text(ylab.pos$x, ylab.pos$y, expression(italic(y)), cex = 1.5)
zlab.pos <- trans3d(-4 - 0.3, -4, mean(c(min(densmat), max(densmat))), res)
text(zlab.pos$x, zlab.pos$y, expression(italic(f["X,Y"](x,y))), cex = 1.5, srt = -75)
lines(trans3d(c(-4,4), -4, 0, res) , col="black")
lines(trans3d(-4, c(-4,4), 0, res) , col="black")
lines(trans3d(-4, -4, c(0, max(densmat)), res) , col="black")

dev.off()



#Figure 5-4
x <- seq(-4,4, length.out = 200)
y <- seq(-4,4, length.out = 200)

dbvnorm <- function(x, y, meanx=0, meany=0, sdx = 1, sdy = 1, cor = 0){
  fxy <- (1/(2*pi*sdx*sdy*sqrt(1-cor^2)))*exp(-(1/(2*(1-cor^2)))*(((x-meanx)^2)/(sdx^2) + ((y-meany)^2)/(sdy^2) - 2*cor*(x-meanx)*(y-meany)/(sdx*sdy)))
  return(fxy)
}

pdf_or_tiff(pdf_ind, "fig5_4", width = figwidth*2, height = figwidth)
par(cex = cex.labs.fx, mar = mar.fx, las = 1, family = fontfam, bty = "n")
par(mfrow = c(1,2))
densmat <- outer(x, y, dbvnorm, meanx = 0, meany = 0, sdx = 1, sdy = 1, cor = 0)
contour(x,y,z = densmat, xlab = expression(italic(x)), ylab = expression(italic(y)), 
        drawlabels = FALSE, cex.axis = cex.labs.fx, cex.lab = cex.labs.fx*1.2)
lines(x=c(0,0),y=c(-1000, 1000))
lines(x=c(-1000,1000),y=c(0, 0))
tcex <- 1.6
text(x=2.5,y=2.5,labels="xy is positive", cex = tcex)
text(x=-2.5,y=-2.5,labels="xy is positive", cex = tcex)
text(x=-2.5,y=2.5,labels="xy is negative", cex = tcex)
text(x=2.5,y=-2.5,labels="xy is negative", cex = tcex)
mtext("Uncorrelated variables", adj = 0, cex = cex.labs.fx*1.4)


densmat <- outer(x, y, dbvnorm, meanx = 0, meany = 0, sdx = 1, sdy = 1, cor = 0.5)

contour(x,y,z = densmat, xlab = expression(italic(x)), ylab = expression(italic(y)), 
        drawlabels = FALSE, cex.axis = cex.labs.fx, cex.lab = cex.labs.fx*1.2)
lines(x=c(0,0),y=c(-1000, 1000))
lines(x=c(-1000,1000),y=c(0, 0))
text(x=2.5,y=2.5,labels="xy is positive", cex = tcex)
text(x=-2.5,y=-2.5,labels="xy is positive", cex = tcex)
text(x=-2.5,y=2.5,labels="xy is negative", cex = tcex)
text(x=2.5,y=-2.5,labels="xy is negative", cex = tcex)
mtext("Correlated variables", adj = 0, cex = cex.labs.fx*1.4)
dev.off()




x <- seq(-3,3, length.out = 200)
y <- seq(-3,3, length.out = 200)

dbvnorm <- function(x, y, meanx=0, meany=0, sdx = 1, sdy = 1, cor = 0){
  fxy <- (1/(2*pi*sdx*sdy*sqrt(1-cor^2)))*exp(-(1/(2*(1-cor^2)))*(((x-meanx)^2)/(sdx^2) + ((y-meany)^2)/(sdy^2) - 2*cor*(x-meanx)*(y-meany)/(sdx*sdy)))
  return(fxy)
}

pdf_or_tiff(pdf_ind, "fig5_corr", width = figwidth*5, height = figwidth)
par(cex = cex.labs.fx, mar = mar.fx, las = 1, family = fontfam, bty = "n", xpd = TRUE)
par(mfrow = c(1,5))

densmat <- outer(x, y, dbvnorm, meanx = 0, meany = 0, sdx = 1, sdy = 1, cor = -.8)
contour(x,y,z = densmat, 
        drawlabels = FALSE, cex.axis = cex.labs.fx, cex.lab = cex.labs.fx*1.2, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 1, labels = FALSE)
axis(side = 2, labels = FALSE)
text("Cor = -.8", x = -1.5, y = 3.2, cex = cex.labs.fx*3.5)


densmat <- outer(x, y, dbvnorm, meanx = 0, meany = 0, sdx = 1, sdy = 1, cor = -.4)
contour(x,y,z = densmat, xlab = expression(italic(x)), ylab = expression(italic(y)), 
        drawlabels = FALSE, cex.axis = cex.labs.fx, cex.lab = cex.labs.fx*1.2)
text("Cor = -.4", x = -1.5, y = 3.2, cex = cex.labs.fx*3.5)


densmat <- outer(x, y, dbvnorm, meanx = 0, meany = 0, sdx = 1, sdy = 1, cor = 0)
contour(x,y,z = densmat, xlab = expression(italic(x)), ylab = expression(italic(y)), 
        drawlabels = FALSE, cex.axis = cex.labs.fx, cex.lab = cex.labs.fx*1.2)

text("Cor = 0", x = -1.5, y = 3.2, cex = cex.labs.fx*3.5)


densmat <- outer(x, y, dbvnorm, meanx = 0, meany = 0, sdx = 1, sdy = 1, cor = 0.4)

contour(x,y,z = densmat, xlab = expression(italic(x)), ylab = expression(italic(y)), 
        drawlabels = FALSE, cex.axis = cex.labs.fx, cex.lab = cex.labs.fx*1.2)
text("Cor = .4", x = -1.5, y = 3.2, cex = cex.labs.fx*3.5)

densmat <- outer(x, y, dbvnorm, meanx = 0, meany = 0, sdx = 1, sdy = 1, cor = 0.8)

contour(x,y,z = densmat, xlab = expression(italic(x)), ylab = expression(italic(y)), 
        drawlabels = FALSE, cex.axis = cex.labs.fx, cex.lab = cex.labs.fx*1.2)
text("Cor = .8", x = -1.5, y = 3.2, cex = cex.labs.fx*3.5)

dev.off()



#Figure 5-5
y.axis <- c(0,3,5,12)
x.axis <- c(0,12)
min.x <- 0
max.x <- 12
min.y <- 0
max.y <- 12
rho = 0.6

x <- seq(min.x, max.x, length.out = (max.x-min.x)*25+1)
y <- seq(min.y, max.y, length.out = (max.y-min.y)*25+1)

densmat <- outer(x, y, dbvnorm, meanx=mean(c(min.x, max.x)), meany=mean(c(min.x, max.x)), sdx = (max.x - min.x)/6, sdy = (max.y - min.y)/6, cor = rho)

min.z <- 0
max.z <- max(densmat)
z.axis <- c(min.z, max.z)

pdf_or_tiff(pdf_ind, "fig5_5", width = figwidth*2, height = figwidth)

par(cex.lab = cex.labs.fx, las = 1, family = fontfam, mar = mar.df*c(1,1,1.4,1))
par(mfrow = c(1,2))
res <- persp(x,y,z = densmat, phi = 30, theta = 0, shade = .25, expand = 1,  border = NA, box = FALSE, axes = FALSE)
lines(trans3d(x = x, y = rep(y[76], length(x)), z = densmat[76,], res), lwd = 2, lty = 3)
lines(trans3d(x = x, y = rep(y[126], length(x)), z = densmat[126,], res), lwd = 2, lty = 2)

#scheme for drawing axes and ticks modified from here: 
#http://entrenchant.blogspot.com/2014/03/custom-tick-labels-in-r-perspective.html
lines(trans3d(x.axis, min.x, min.z, res) , col="black")
lines(trans3d(min.x, y.axis, min.z, res) , col="black")
lines(trans3d(min.x, min.y, z.axis, res) , col="black")

#y-axis ticks
tick.start <- trans3d(max.x, y.axis[-c(1,4)], min.z, res)
tick.end <- trans3d(max.x + 0.20, y.axis[-c(1,4)], min.z, res)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)


labels <- c(expression(italic("y=c"[1])), expression(italic("y=c"[2])))
label.pos <- trans3d((max.x + 0.25), y.axis[-c(1,4)], min.z, res)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex = 1.5)

xlab.pos <- trans3d(mean(c(min.x, max.x)),min.y-0.3, min.z, res)
text(xlab.pos$x, xlab.pos$y, expression(italic(x)), cex = 1.5 )
ylab.pos <- trans3d(min.x-0.4, mean(c(min.y, max.y)), min.z, res)
text(ylab.pos$x, ylab.pos$y, expression(italic(y)), cex = 1.5)
zlab.pos <- trans3d(min.x - 0.4, min.y, mean(c(min.z, max.z)), res)
text(zlab.pos$x, zlab.pos$y, expression(italic(f["X,Y"](x,y))), cex = 1.5, srt = -75)

mtext("Joint density", adj = 0, cex = cex.labs.fx*1.4)

#Panel B

mu.x <- mean(c(min.x, max.x))
sd.x <- (max.x - min.x)/6
mu.y <- mean(c(min.y, max.y))
sd.y <- (max.y - min.y)/6
c1 <- y[76]
c2 <- y[126]

b <- rho*(sd.x/sd.y)
a <- mu.x - b*mu.y

ex.c1 <- a + b * c1
ex.c2 <- a + b * c2
sd.cond <- sqrt(sd.x^2*(1-rho^2))

fx <- dnorm(x, mu.x, sd.x)
fx.c1 <- dnorm(x, ex.c1, sd.cond)
fx.c2 <- dnorm(x, ex.c2, sd.cond)


par(cex.lab = cex.labs.fx, family = fontfam)
plot(x,fx.c1, type = "l", xlim = c(min.x,max.x), yaxt = "n", xaxt = "n", lwd = 2, 
     lty = 3,  xlab = "", ylab = "", bty = "n")
mtext("(Conditional) Density", 2, cex = cex.labs.fx*1.2, line = 1, las = 0) 
mtext(expression(italic(x)), 1, cex = cex.labs.fx*1.2, line = 1)
axis(1, at=c(min(x), max(x)), labels = c("",""))
axis(2, at=c(min(fx.c1), max(fx.c1)), labels = c("",""))
lines(x, fx, lwd = 2, lty = 1)
lines(x, fx.c2, lwd = 2, lty = 2)
legend("topright", lwd = 2, lty = c(1,3,2), cex = cex.labs.fx, 
       legend = c(expression(italic(f[X](x))), expression(italic( paste(f['X|Y'], "(x|Y=", c[1], ")", sep = ""))),expression(italic( paste(f['X|Y'], "(x|Y=", c[2], ")", sep = ""))))
       , bty = "n")
mtext("Conditional densities", adj = 0, cex = cex.labs.fx*1.4)
dev.off()



#Figure 5-6
pdf_or_tiff(pdf_ind, "fig5_6", width = figwidth, height = figheight*.8)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
x <- seq(-10, 10, length.out = 1000)
fx1 <- dnorm(x)
plot(x, fx1, type = "l", xlab = expression(italic(x)), 
     ylab = expression(paste(italic(f[X]), "(", italic(x), ")", sep = "")), 
     lwd = 2, xlim = c(-3, 3), cex.lab = cex.labs, cex.axis = cex.axs)
dev.off()


#Figure 5-7, the quincunx
#This is slow, so I've commented it out.
#library(animation)
#tiff("fig5_10.tif", width = figwidth, height = figwidth*1.6, units = un, res = reso, compression = "lzw")
#nball <- 500 #change the number of balls
#nlayer <- 25 #change the number of rows of pegs on the board
#rate <- 20   #change the speed at which the balls fall
#ani.options(nmax = nball + nlayer - 20, interval = 1/rate)
#quincunx(balls = nball, layers = nlayer, cex.balls = 1, col.balls = "black")
#dev.off()


pdf_or_tiff(pdf_ind, "fig5_8", width = figwidth, height = figheight)
par(mfrow = c(2,2))
x <- c(5, 7,9,11,13)

par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")
plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-2,2,length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dens.eps, 5.5 + eps)
lines(c(5, 5 - dnorm(0,0,2/3)), c(5.5,5.5), lty = 3)
lines(7 - dens.eps, 6.5 + eps)
lines(c(7, 7 - dnorm(0,0,2/3)), c(6.5,6.5), lty = 3)
lines(9 - dens.eps, 7.5 + eps)
lines(c(9, 9 - dnorm(0,0,2/3)), c(7.5,7.5), lty = 3)
lines(11 - dens.eps, 8.5 + eps)
lines(c(11, 11 - dnorm(0,0,2/3)), c(8.5,8.5), lty = 3)
lines(13 - dens.eps, 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)

mtext("All assumptions met", adj = 0, cex = cex.axs*.9)

x <- c(5, 7,9,11,13)

plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-2,2,length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dens.eps, 7.5 + eps)
lines(c(5, 5 - dnorm(0,0,2/3)), c(7.5,7.5), lty = 3)
lines(7 - dens.eps, 6.5 + eps)
lines(c(7, 7 - dnorm(0,0,2/3)), c(6.5,6.5), lty = 3)
lines(9 - dens.eps, 6 + eps)
lines(c(9, 9 - dnorm(0,0,2/3)), c(6,6), lty = 3)
lines(11 - dens.eps, 7.5 + eps)
lines(c(11, 11 - dnorm(0,0,2/3)), c(7.5,7.5), lty = 3)
lines(13 - dens.eps, 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Linearity violated", adj = 0, cex = cex.axs*.9)


x <- c(5, 7,9,11,13)

plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
#points(7, 6.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-2,2,length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dnorm(seq(-.75,.75, length.out = 10000), mean = 0, sd = 1/4), 5.5 + seq(-0.75,0.75, length.out = 10000))
lines(c(5, 5 - dnorm(0,0,1/4)), c(5.5,5.5), lty = 3)
lines(7 - dnorm(seq(-1,1, length.out = 10000), mean = 0, sd = 1/3), 6.5 + seq(-1,1, length.out = 10000))
lines(c(7, 7 - dnorm(0,0,1/3)), c(6.5,6.5), lty = 3)
lines(9 - dnorm(seq(-6/5,6/5, length.out = 10000), mean = 0, sd = 2/5), 7.5 + seq(-6/5,6/5, length.out = 10000))
lines(c(9, 9 - dnorm(0,0,2/5)), c(7.5,7.5), lty = 3)
lines(11 - dnorm(seq(-3/2,3/2, length.out = 10000), mean = 0, sd = 1/2), 8.5 + seq(-3/2,3/2, length.out = 10000))
lines(c(11, 11 - dnorm(0,0,1/2)), c(8.5,8.5), lty = 3)
lines(13 - dnorm(eps, mean = 0, sd = 2/3), 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Homoscedasticity violated", adj = 0, cex = cex.axs*.9)

x <- c(5, 7,9,11,13)

plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-.99,3,length.out = 10000)
dens.eps <- dgamma(eps + 1, 2, 2)

lines(5 - dens.eps, 5.5 + eps)
lines(c(5, 5 - dnorm(0,0,2/3)), c(5.5,5.5), lty = 3)
lines(7 - dens.eps, 6.5 + eps)
lines(c(7, 7 - dnorm(0,0,2/3)), c(6.5,6.5), lty = 3)
lines(9 - dens.eps, 7.5 + eps)
lines(c(9, 9 - dnorm(0,0,2/3)), c(7.5,7.5), lty = 3)
lines(11 - dens.eps, 8.5 + eps)
lines(c(11, 11 - dnorm(0,0,2/3)), c(8.5,8.5), lty = 3)
lines(13 - dens.eps, 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Non-normal (fine until ch. 9)", adj = 0, cex = cex.axs*.9)

dev.off()



####################################################
####################################################
####################################################
#Chapter Interlude

#Figure Int-1 is a reproduction of Figure 3-3:
pdf_or_tiff(pdf_ind, "figInt_1", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
ls.fit <- lm(y1 ~ x1, data = anscombe)
plot(anscombe$x1, anscombe$y1, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs)
abline(ls.fit)
dev.off()

########################################
########################################
########################################
#Chapter 6

draw.circ <- function(radius = 1){
  x <- seq(-radius, radius, length.out = 1000)
  y.up <- sqrt(radius^2 - x^2)
  y.lo <- -y.up
  lines(x, y.up)
  lines(x, y.lo)
}

target_plot <- function(n, xm = 0, xs = 1, ym = 0, ys = 1, ...){
  x <- rnorm(10)
  plot(x, xlim = c(-4.5,4.5), ylim=c(-4.5,4.5), pch = "",xaxt = "n", yaxt = "n", ylab = "", xlab = "", asp = 1, ...)
  sapply(1:4, draw.circ)
  x.cds <- rnorm(n, xm, xs)
  y.cds <- rnorm(n, ym, ys)
  points(x.cds, y.cds, pch = 4)
}

pdf_or_tiff(pdf_ind, "fig6_1", width = figwidth*.85, height = figheight)
set.seed(8675309)
par(mar = c(1,1,1.1,1), las = 1, family = fontfam)
par(mfrow = c(2,2))
target_plot(15, 0, 1/2, 0, 1/2, bty = "n")
mtext("Accurate and precise", adj = 0, cex = cex.labs)
target_plot(15, 0, 3/2, 0, 3/2, bty = "n")
mtext("Accurate but imprecise", adj = 0, cex = cex.labs)
target_plot(15, 2, 1/2, 2, 1/2, bty = "n")
mtext("Inaccurate but precise", adj = 0, cex = cex.labs)
target_plot(15, 2, 3/2, 2, 3/2, bty = "n")
mtext("Inaccurate and imprecise", adj = 0, cex = cex.labs)
dev.off()


target_plot_sm <- function(n, xm = 0, xs = 1, ym = 0, ys = 1, ...){
  x <- rnorm(10)
  plot(x, xlim = c(-4.08,4.08), ylim=c(-4.08,4.08), pch = "",xaxt = "n", yaxt = "n", ylab = "", xlab = "", asp = 1, ...)
  sapply(1:4, draw.circ)
  x.cds <- rnorm(n, xm, xs)
  y.cds <- rnorm(n, ym, ys)
  points(x.cds, y.cds, pch = 4)
}


pdf_or_tiff(pdf_ind, "fig6_3", width = figwidth*1.7, height = figheight)
set.seed(8675309)
par(mar = c(0,2,2,0), las = 1, family = fontfam, xaxs="i", yaxs = "i")
par(mfrow = c(2,4))

target_plot_sm(15, 0, 3/2, 0, 3/2, bty = "n")
mtext(expression("Small samples"), cex = cex.labs*1.25)
mtext("Estimator 1", side = 2, las = 0, cex = cex.labs*1.25)
target_plot_sm(15, 0, 2/3, 0, 2/3, bty = "n")
mtext(expression("Moderate samples"), cex = cex.labs*1.25)
target_plot_sm(15, 0, 1/4, 0, 1/4, bty = "n")
mtext(expression("Large samples"), cex = cex.labs*1.25)
target_plot_sm(15, 0, 1e-6, 0, 1e-6, bty = "n")
mtext(expression(textstyle("Sample size" %->% ~ infinity)), cex = cex.labs*1.25, line = 0)
target_plot_sm(15, 2, 3/2, 2, 3/2, bty = "n")
mtext("Estimator 2", side = 2, las = 0, cex = cex.labs*1.25)
target_plot_sm(15, 1, 2/3, 1, 2/3, bty = "n")
target_plot_sm(15, 1/3, 1/3, 1/3, 1/3, bty = "n")
target_plot_sm(15, 0, 1e-6, 0, 1e-6, bty = "n")
dev.off()



pdf_or_tiff(pdf_ind, "fig6_4", width = figwidth*1.7, height = figheight)
set.seed(8675309)
par(mar = c(0,2,2,0), las = 1, family = fontfam, xaxs="i", yaxs = "i")
par(mfrow = c(2,4))

target_plot_sm(15, 0, 3/2, 0, 3/2, bty = "n")
mtext(expression("Small samples"), cex = cex.labs*1.25)
mtext("Estimator 1", side = 2, las = 0, cex = cex.labs*1.25)
target_plot_sm(15, 0, 2/3, 0, 2/3, bty = "n")
mtext(expression("Moderate samples"), cex = cex.labs*1.25)
target_plot_sm(15, 0, 1/4, 0, 1/4, bty = "n")
mtext(expression("Large samples"), cex = cex.labs*1.25)
target_plot_sm(15, 0, 1e-6, 0, 1e-6, bty = "n")
mtext(expression(textstyle("Sample size" %->% ~ infinity)), cex = cex.labs*1.25, line = 0)
target_plot_sm(15, 0, 2/3, 0, 2/3, bty = "n")
mtext("Estimator 2", side = 2, las = 0, cex = cex.labs*1.25)
target_plot_sm(15, 0, 1/4, 0, 1/4, bty = "n")
target_plot_sm(15, 0, 1e-1, 0, 1e-1, bty = "n")
target_plot_sm(15, 0, 1e-6, 0, 1e-6, bty = "n")
dev.off()




pdf_or_tiff(pdf_ind, "fig6_6", width = figwidth*.85, height = figheight)
set.seed(123456)
par(mar = c(1,1.1,1.1,1), las = 1, family = fontfam)
par(mfrow = c(2,2))
target_plot(15, 0, 1/2, 0, 1/2, bty = "n")
mtext("Assumptions met", adj = 0, cex = cex.labs)
mtext("Not Robust", side = 2, las = 0, cex = cex.labs)
target_plot(15, 2.5, 1, 2.5, 1, bty = "n")
mtext("Assumptions violated", adj = 0, cex = cex.labs)
target_plot(15, 0, 3/4, 0, 3/4, bty = "n")
mtext("Robust", side = 2, las = 0, cex = cex.labs)
target_plot(15, 1/4, 3/4, 1/4, 3/4, bty = "n")
dev.off()

####################################################
####################################################
####################################################
#Chapter 7

#Figure 7-1
pdf_or_tiff(pdf_ind, "fig7_1", width = figwidth*1.2, height = figheight)
set.seed(867234567)
n.ints <- 50
conf <- 0.90
se.z <- 1
mean.z <- 0

par(mar = mar.fx, las = 1, family = fontfam, mgp = mgp.set, bty = "n")

cutoff.ses <- qnorm(conf + (1-conf)/2)

x <- rnorm(n.ints, mean.z, se.z)
lbs <- x - se.z*cutoff.ses
ubs <- x + se.z*cutoff.ses

mean(ubs > mean.z & lbs < mean.z)

plot(1:n.ints, x, pch = "", ylim = c(-4.3,4.3), xlab = "Sample number", 
     ylab = expression(paste("Confidence Interval for  ", theta, " (true ", theta, " = 0)")), 
     cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs, las = 1, xaxt = "n")
axis(1,  at = c(1,10,20,30,40,50), cex.lab = cex.labs, cex.axis = cex.axs)
lines(c(-1000, 1000), c(mean.z, mean.z), lty = 2)
for(i in 1:n.ints){
  lines(c(i,i), c(lbs[i], ubs[i]))
}

dev.off()




#Figure 7-2
pdf_or_tiff(pdf_ind, "fig7_2", width = figwidth, height = figheight)

par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")
mu = 64
sigma = 0.8
mu_minus_xbar <- 1

xbar <- mu - mu_minus_xbar
xbar_twotail <- mu + mu_minus_xbar

x <- seq(mu - 3.5*sigma, mu + 3.5*sigma, length.out = 10000)
fx <- dnorm(x, mu, sigma)

plot(x, fx, type = "l", xlab = expression(italic(bar(x))), 
     ylab = expression(paste(italic(f[bar(X)]), "(", italic(bar(x)), "|", italic(H[0]), ")" )), 
     las = 1, cex.axis = cex.axs, cex.lab = cex.labs)

x1 <- min(xbar, xbar_twotail)
x2 <- max(xbar, xbar_twotail)

polygon(c(min(x),x[x<=x1],x1),c(0,fx[x<=x1],0), col = "black", border = NA)
polygon(c(x2,x[x>=x2],max(x)),c(0,fx[x>=x2],0), col = "grey", border = NA)

xlinelab <- expression(paste("Observed ", italic(bar(x)), sep = ""))

lines(c(xbar, xbar), c(-100, 100), lty = 2)
text(x = xbar - sigma/5, y = max(fx[x <= xbar] + 0.1), labels = xlinelab, las = 2, srt = 90)

dev.off()


#Figure 7-4
pdf_or_tiff(pdf_ind, "fig7_4", width = figwidth, height = figheight)

par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")

t1 <- 0.05
na <- 5
nb <- 25
nc <- 100
line.w <- 1.5

d <- seq(-2,2, length.out = 10000)
#this function assumes d as in eq. 6.4
pow.1samp.z.2side <- function(t1, n, d){
  crit.z <- abs(qnorm(t1/2))
  crit.val <- crit.z / sqrt(n)
  power <- 1 - pnorm(crit.val, mean = abs(d), sd = 1/sqrt(n))
  return(power)
}

pow.a <- pow.1samp.z.2side(t1, na, d)
pow.b <- pow.1samp.z.2side(t1, nb, d)
pow.c <- pow.1samp.z.2side(t1, nc, d)

plot(d, pow.a, type = "l", xlab = expression(italic(d)), ylab = "Power", lwd = line.w, 
     cex = cex.pts, cex.axis = cex.axs, cex.lab = cex.labs, las = 1)
lines(d, pow.b, lty = 2, lwd = line.w)
lines(d, pow.c, lty = 3, lwd = line.w)

legend("bottomright", legend = c(paste("n =", as.character(na)), 
                                 paste("n =", as.character(nb)), 
                                 paste("n =", as.character(nc))), 
       lty = c(1,2,3), lwd = line.w, cex = cex.labs, bty = "n")

dev.off()



#Figure Power and CI 7-5

cisimplot <- function(mean.z, se.z, n.ints = 50, conf = .9, null.mean = NULL, print.pow = FALSE, pts = TRUE, ... ){
  cutoff.ses <- qnorm(conf + (1-conf)/2)
  x <- rnorm(n.ints, mean.z, se.z)
  lbs <- x - se.z*cutoff.ses
  ubs <- x + se.z*cutoff.ses
  plot(1:n.ints, x, pch = "", las = 1, xaxt = "n", yaxt = "n", ...)
  axis(2, at = c(-3, mean.z,3), label = c("",expression(theta),""))
  if(!is.null(null.mean)){
    axis(4, at = null.mean, label = expression(theta[0]), mgp = c(0,0,0), tick = FALSE)
  }
  #axis(1, at = c(1,n.ints),label = c("",""),cex.lab = cex.labs, cex.axis = cex.axs)
  lines(c(0, n.ints*1.02), c(mean.z, mean.z), lty = 2)
  if(!is.null(null.mean)){
    lines(c(0, n.ints*1.02), c(null.mean, null.mean), lty = 1)
  }
  for(i in 1:n.ints){
    lines(c(i,i), c(lbs[i], ubs[i]))
  }
  if(pts){points(1:n.ints, x, pch = 20, cex = .5)}
  if(pts & !is.null(null.mean)){points((1:n.ints)[lbs < null.mean & ubs > null.mean], x[lbs < null.mean & ubs > null.mean], col = "grey", pch = 20, cex = .5)}
  if(!(is.null(null.mean))){
    for(i in 1:n.ints){
      if(lbs[i] < null.mean & ubs[i] > null.mean){
        lines(c(i,i), c(lbs[i], ubs[i]), col = "grey")
      }
    }
  }
  if(!is.null(null.mean)){print(1 - mean(ubs > null.mean & lbs < null.mean))}
  print(mean(ubs > mean.z & lbs < mean.z))
}

pdf_or_tiff(pdf_ind, "fig7_5", width = figwidth*1, height = figheight)
set.seed(867234567)
par(mfrow = c(3,3))
par(mar = c(0,3,3,2), las = 1, family = fontfam, mgp = mgp.set, bty = "n", xpd = NA)

cisimplot(0, .8, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")
#mtext("Null is true", cex = cex.labs*.7, line = 1)
mtext(expression(paste("Effect ",theta - theta[0] == 0)), cex = cex.labs*.7, line = 1)
mtext(expression(paste("Small ", italic(n))), side = 2, las = 0, cex = cex.labs*.7, line = 1.8)
cisimplot(.5, .8, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")
#mtext("Moderate true effect", cex = cex.labs*.7, line = 1)
mtext(expression(paste("Effect ",theta - theta[0] , "= small")), cex = cex.labs*.7, line = 1)
cisimplot(1, .8, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")
#mtext("Large true effect", cex = cex.labs*.7, line = 1)
mtext(expression(paste("Effect ",theta - theta[0] , "= large")), cex = cex.labs*.7, line = 1)


cisimplot(0, .4, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")
mtext(expression(paste("Moderate ", italic(n))), side = 2, las = 0, cex = cex.labs*.7, line = 1.8)
cisimplot(.5, .4, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")
cisimplot(1, .4, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")

cisimplot(0, .2, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")
mtext(expression(paste("Large ", italic(n))), side = 2, las = 0, cex = cex.labs*.7, line = 1.8)
cisimplot(.5, .2, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")
cisimplot(1, .2, ylim = c(-3.2,3.2), null.mean = 0, ylab = "", xlab = "")

dev.off()



####################################################
####################################################
####################################################
#Chapter 8


#Figure 8-1, a nonparametric-looking (but not actually nonparametric)
#distribution, based on iris Petal Width data.

pdf_or_tiff(pdf_ind, "fig8_1", width = figwidth, height = figheight)
dens <- density(iris$Petal.Width) #
y.mod <- dens$y
y.mod[51:150] <- y.mod[51:150] - c(1:50,50:1)/200
y.mod[401:500] <- y.mod[401:500] + c(1:50,50:1)/200
y.mod[201:210] <- y.mod[201:210] - c(1:5,5:1)/150
y.mod[241:250] <- y.mod[241:250] + c(1:5,5:1)/150

par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")
plot(dens$x, y.mod, type = "l", main = "",  ylab = expression(paste(italic(f), "(", italic(x), ")", sep = "")), 
     xlab = expression(italic(x)), cex.axis = cex.axs, cex.lab = cex.labs) # 
dev.off()


#Figure 8-2

pdf_or_tiff(pdf_ind, "fig8_2", width = figwidth, height = figheight)
par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")
set.seed(54321)
x.vals <- seq(-3,3, length.out = 10000)
Fx <- pnorm(x.vals)
plot(x.vals, Fx, xlab = expression(italic(z)), ylab = expression(italic("F(z)")), 
     type = "l", cex.axis = cex.axs, cex.lab = cex.labs, las = 1)

x <- rnorm(10)
lines(ecdf(x), verticals = TRUE, do.points = FALSE, main = "", lty = 2)
x <- rnorm(50)
lines(ecdf(x), verticals = TRUE, do.points = FALSE, main = "", lty = 3)


legend(x = -3, y = .95, legend = c("True cdf", "n = 10", "n = 50"), lty = 1:3, cex = cex.labs, bty = "n")
dev.off()



pdf_or_tiff(pdf_ind, "fig8_4", width = figwidth, height = figheight)
par(mfrow = c(2,2))
x <- c(5, 7,9,11,13)

par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")
plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-2,2,length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dens.eps, 5.5 + eps)
lines(c(5, 5 - dnorm(0,0,2/3)), c(5.5,5.5), lty = 3)
lines(7 - dens.eps, 6.5 + eps)
lines(c(7, 7 - dnorm(0,0,2/3)), c(6.5,6.5), lty = 3)
lines(9 - dens.eps, 7.5 + eps)
lines(c(9, 9 - dnorm(0,0,2/3)), c(7.5,7.5), lty = 3)
lines(11 - dens.eps, 8.5 + eps)
lines(c(11, 11 - dnorm(0,0,2/3)), c(8.5,8.5), lty = 3)
lines(13 - dens.eps, 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)

mtext("Normal, homoscedastic", adj = 0, cex = cex.axs*.9)


plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
#points(7, 6.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-2,2,length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dnorm(seq(-.75,.75, length.out = 10000), mean = 0, sd = 1/4), 5.5 + seq(-0.75,0.75, length.out = 10000))
lines(c(5, 5 - dnorm(0,0,1/4)), c(5.5,5.5), lty = 3)
lines(7 - dnorm(seq(-1,1, length.out = 10000), mean = 0, sd = 1/3), 6.5 + seq(-1,1, length.out = 10000))
lines(c(7, 7 - dnorm(0,0,1/3)), c(6.5,6.5), lty = 3)
lines(9 - dnorm(seq(-6/5,6/5, length.out = 10000), mean = 0, sd = 2/5), 7.5 + seq(-6/5,6/5, length.out = 10000))
lines(c(9, 9 - dnorm(0,0,2/5)), c(7.5,7.5), lty = 3)
lines(11 - dnorm(seq(-3/2,3/2, length.out = 10000), mean = 0, sd = 1/2), 8.5 + seq(-3/2,3/2, length.out = 10000))
lines(c(11, 11 - dnorm(0,0,1/2)), c(8.5,8.5), lty = 3)
lines(13 - dnorm(eps, mean = 0, sd = 2/3), 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Normal, heteroscedastic", adj = 0, cex = cex.axs*.9)

x <- c(5, 7,9,11,13)

plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-.99,3,length.out = 10000)
dens.eps <- dgamma(eps + 1, 2, 2)

lines(5 - dens.eps, 5.5 + eps)
lines(c(5, 5 - dnorm(0,0,2/3)), c(5.5,5.5), lty = 3)
lines(7 - dens.eps, 6.5 + eps)
lines(c(7, 7 - dnorm(0,0,2/3)), c(6.5,6.5), lty = 3)
lines(9 - dens.eps, 7.5 + eps)
lines(c(9, 9 - dnorm(0,0,2/3)), c(7.5,7.5), lty = 3)
lines(11 - dens.eps, 8.5 + eps)
lines(c(11, 11 - dnorm(0,0,2/3)), c(8.5,8.5), lty = 3)
lines(13 - dens.eps, 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Non-normal, homoscedastic", adj = 0, cex = cex.axs*.9)


plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

#density function for laplace RV
dlaplace <- function (x, mean = 0, sd = 1) {
  exp(-abs(x - mean) * sqrt(2)/sd)/(sd * sqrt(2))
}

eps <- seq(-2,2, length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dlaplace(seq(-.75,.75, length.out = 10000), mean = 0, sd = 1/4), 5.5 + seq(-0.75,0.75, length.out = 10000))
lines(c(5, 5 - dlaplace(0,0,1/4)), c(5.5,5.5), lty = 3)
lines(7 - dlaplace(seq(-1,1, length.out = 10000), mean = 0, sd = 1/3), 6.5 + seq(-1,1, length.out = 10000))
lines(c(7, 7 - dlaplace(0,0,1/3)), c(6.5,6.5), lty = 3)
lines(9 - dlaplace(seq(-6/5,6/5, length.out = 10000), mean = 0, sd = 2/5), 7.5 + seq(-6/5,6/5, length.out = 10000))
lines(c(9, 9 - dlaplace(0,0,2/5)), c(7.5,7.5), lty = 3)
lines(11 - dlaplace(seq(-3/2,3/2, length.out = 10000), mean = 0, sd = 1/2), 8.5 + seq(-3/2,3/2, length.out = 10000))
lines(c(11, 11 - dlaplace(0,0,1/2)), c(8.5,8.5), lty = 3)
lines(13 - dlaplace(eps, mean = 0, sd = 2/3), 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Non-normal, heteroscedastic", adj = 0, cex = cex.axs*.9)


dev.off()





#Figure 8-7.
#Draw a bootstrap sample of entries from a vector or columns #from a matrix.
boot.samp <- function(x){
  #If x is a vector, convert it to a matrix.
  if(is.null(dim(x))){
    x <- matrix(x, ncol = 1)
  }
  n <- nrow(x)
  boot.inds <- sample(1:n, replace = TRUE)
  samp <- x[boot.inds,]
  return(samp)
}

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

pdf_or_tiff(pdf_ind, "fig8_7", width = figwidth, height = figheight)
par(mar = c(3.5,4,1,1), mgp = c(2.3,.8,0), las = 1, family = fontfam, bty = "n")

hist(boot.dist, main = "", ylim = c(0, 2000), xlab = expression(paste("Bootstrapped ", tilde(beta)^"*", sep = "")), 
     breaks = seq(-.2, 1.2, by = 0.05), cex.axis = cex.axs, cex.lab = cex.labs, ylab = "", 
     col = "grey55", border = "white")
title(ylab = "Frequency", cex.lab = cex.labs, mgp = c(3,1,0))

dev.off()


#Figure 8-9.
perm.samp <- function(x){
  samp <- apply(x, 2, sample)
  return(samp)
}

beta.mm <- function(x, y){
  n <- length(x)
  beta.est <- (sum(x*y) - (1/n)*sum(x)*sum(y)) / 
    (sum(x^2) - (1/n)*sum(x)^2)
  return(beta.est)
}

set.seed(8675309) #Optional. 
nperms <- 10000
perm.dist <- numeric(nperms)
dat <- cbind(anscombe$x1, anscombe$y1)
for(i in 1:nperms){
  samp <- perm.samp(dat)
  perm.dist[i] <- beta.mm(samp[,1], samp[,2])
}

pdf_or_tiff(pdf_ind, "fig8_9", width = figwidth, height = figheight)
par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")

hist(perm.dist, main = "", ylim = c(0, 1000), 
     xlab = expression(paste( tilde(beta)["p"], " from Permuted Samples", sep = "")), 
     breaks = seq(-0.6, 0.6, by = 0.05), cex.axis = cex.axs, cex.lab = cex.labs, ylab = "", 
     col = "grey55", border = "white")
b.orig <- beta.mm(anscombe$x1, anscombe$y1)
lines(c(b.orig, b.orig), c(0, 500), lty = 2)
text(b.orig, 550, labels = expression(paste("Original ", tilde(beta))), srt = 0, cex = cex.labs)
title(ylab = "Frequency", cex.lab = cex.labs, mgp = c(2.6,1,0))

dev.off()

####################################################
####################################################
####################################################
#Chapter 9

#Figure 9-1
#Plot a likelihood and a log-likelihood
pdf_or_tiff(pdf_ind, "fig9_1", width = figwidth, height = (4/3)*figwidth)
par(mar = mar.fx, las = 1, family = fontfam, bty = "n")
par(mfrow=c(2,1))

step = 0.0001
n <- 5
k <- 1
ll_ymin = -6
theta <- seq(step, 1, by = step)
likelihood <- choose(n,k)*theta^k*(1-theta)^(n-k)

plot(theta, likelihood, type = "l", xlab = expression(italic(theta)), ylab = "", bty = "n",
     cex.axis = cex.labs.fx, cex.lab = cex.labs.fx, lwd = 2)
#title(ylab = expression(paste("\u2112", "(", theta, ")", sep = "")))
title(ylab = expression(italic(L(theta))), cex.lab = cex.labs.fx)
mtext("Likelihood", cex = cex.labs.fx, adj = 0)
plot(c(0,theta), c(ll_ymin*10, log(likelihood)), type = "l", xlab = expression(italic(theta)),
     ylab = "", ylim = c(ll_ymin, max(log(likelihood))), bty = "n",
     cex.axis = cex.labs.fx, cex.lab = cex.labs.fx, lwd = 2)
title(ylab = expression(italic(l(theta))), cex.lab = cex.labs.fx)
mtext("Log-likelihood", cex = cex.labs.fx, adj = 0)
dev.off()

#Figure 9-2
pdf_or_tiff(pdf_ind, "fig9_2", width = figwidth, height = figheight)
par(mfrow = c(2,2))
x <- c(5, 7,9,11,13)

par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")
plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-2,2,length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dens.eps, 5.5 + eps)
lines(c(5, 5 - dnorm(0,0,2/3)), c(5.5,5.5), lty = 3)
lines(7 - dens.eps, 6.5 + eps)
lines(c(7, 7 - dnorm(0,0,2/3)), c(6.5,6.5), lty = 3)
lines(9 - dens.eps, 7.5 + eps)
lines(c(9, 9 - dnorm(0,0,2/3)), c(7.5,7.5), lty = 3)
lines(11 - dens.eps, 8.5 + eps)
lines(c(11, 11 - dnorm(0,0,2/3)), c(8.5,8.5), lty = 3)
lines(13 - dens.eps, 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)

mtext("Normal, homoscedastic", adj = 0, cex = cex.axs*.9)


plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
#points(7, 6.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-2,2,length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dnorm(seq(-.75,.75, length.out = 10000), mean = 0, sd = 1/4), 5.5 + seq(-0.75,0.75, length.out = 10000))
lines(c(5, 5 - dnorm(0,0,1/4)), c(5.5,5.5), lty = 3)
lines(7 - dnorm(seq(-1,1, length.out = 10000), mean = 0, sd = 1/3), 6.5 + seq(-1,1, length.out = 10000))
lines(c(7, 7 - dnorm(0,0,1/3)), c(6.5,6.5), lty = 3)
lines(9 - dnorm(seq(-6/5,6/5, length.out = 10000), mean = 0, sd = 2/5), 7.5 + seq(-6/5,6/5, length.out = 10000))
lines(c(9, 9 - dnorm(0,0,2/5)), c(7.5,7.5), lty = 3)
lines(11 - dnorm(seq(-3/2,3/2, length.out = 10000), mean = 0, sd = 1/2), 8.5 + seq(-3/2,3/2, length.out = 10000))
lines(c(11, 11 - dnorm(0,0,1/2)), c(8.5,8.5), lty = 3)
lines(13 - dnorm(eps, mean = 0, sd = 2/3), 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Normal, heteroscedastic", adj = 0, cex = cex.axs*.9)

x <- c(5, 7,9,11,13)

plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

eps <- seq(-.99,3,length.out = 10000)
dens.eps <- dgamma(eps + 1, 2, 2)

lines(5 - dens.eps, 5.5 + eps)
lines(c(5, 5 - dnorm(0,0,2/3)), c(5.5,5.5), lty = 3)
lines(7 - dens.eps, 6.5 + eps)
lines(c(7, 7 - dnorm(0,0,2/3)), c(6.5,6.5), lty = 3)
lines(9 - dens.eps, 7.5 + eps)
lines(c(9, 9 - dnorm(0,0,2/3)), c(7.5,7.5), lty = 3)
lines(11 - dens.eps, 8.5 + eps)
lines(c(11, 11 - dnorm(0,0,2/3)), c(8.5,8.5), lty = 3)
lines(13 - dens.eps, 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Non-normal, homoscedastic", adj = 0, cex = cex.axs*.9)


plot(anscombe$x1, anscombe$y1, xlab = "x", ylab = "y", pch = 19,  cex = cex.pts, cex.lab = 1.5, type = "n",
     xlim = c(3, 15), ylim = c(3,12))
abline(3, 0.5)
rug(x, col = "grey", ticksize = 0.05, lwd = 1)

#density function for laplace RV
dlaplace <- function (x, mean = 0, sd = 1) {
  exp(-abs(x - mean) * sqrt(2)/sd)/(sd * sqrt(2))
}

eps <- seq(-2,2, length.out = 10000)
dens.eps <- dnorm(eps, mean = 0, sd = 2/3)

lines(5 - dlaplace(seq(-.75,.75, length.out = 10000), mean = 0, sd = 1/4), 5.5 + seq(-0.75,0.75, length.out = 10000))
lines(c(5, 5 - dlaplace(0,0,1/4)), c(5.5,5.5), lty = 3)
lines(7 - dlaplace(seq(-1,1, length.out = 10000), mean = 0, sd = 1/3), 6.5 + seq(-1,1, length.out = 10000))
lines(c(7, 7 - dlaplace(0,0,1/3)), c(6.5,6.5), lty = 3)
lines(9 - dlaplace(seq(-6/5,6/5, length.out = 10000), mean = 0, sd = 2/5), 7.5 + seq(-6/5,6/5, length.out = 10000))
lines(c(9, 9 - dlaplace(0,0,2/5)), c(7.5,7.5), lty = 3)
lines(11 - dlaplace(seq(-3/2,3/2, length.out = 10000), mean = 0, sd = 1/2), 8.5 + seq(-3/2,3/2, length.out = 10000))
lines(c(11, 11 - dlaplace(0,0,1/2)), c(8.5,8.5), lty = 3)
lines(13 - dlaplace(eps, mean = 0, sd = 2/3), 9.5 + eps)
lines(c(13, 13 - dnorm(0,0,2/3)), c(9.5,9.5), lty = 3)
mtext("Non-normal, heteroscedastic", adj = 0, cex = cex.axs*.9)


dev.off()


#Figure 9-3
#Plot a log-likelihood twice, once with more data.
pdf_or_tiff(pdf_ind, "fig9_3", width = figwidth, height = figwidth )
par(mar = mar.fx, cex.lab = cex.labs.fx, las = 1, family = fontfam, bty = "n", cex.axis = cex.labs.fx*.8)
par(mfrow=c(2,2))

step = 0.0001
n1 <- 5
k1 <- 1
n2 <- n1*5
k2 <- k1*5
ll_ymin = -6
theta <- seq(step, 1, by = step)
likelihood1 <- choose(n1, k1)*theta^k1*(1-theta)^(n1-k1)
likelihood2 <- choose(n2, k2)*theta^k2*(1-theta)^(n2-k2)

plot(theta, likelihood1, type = "l", xlab = expression(italic(theta)),
     ylab = "", ylim = c(0, max(likelihood1)))
title(ylab = expression(italic(L[5](theta))))
mtext("Less data", adj = 0, cex = cex.labs.fx*.8)

plot(theta, likelihood2, type = "l", xlab = expression(italic(theta)),
     ylab = "", ylim = c(0, max(likelihood2)), yaxt = "n")
axis(2, at = c(0, 0.1, 0.2))
title(ylab = expression(italic(L[25](theta))))
mtext("More data", adj = 0, cex = cex.labs.fx*.8)


plot(c(0,theta), c(ll_ymin*10, log(likelihood1)), type = "l", xlab = expression(italic(theta)),
     ylab = "", ylim = c(ll_ymin, max(log(likelihood1))))
title(ylab = expression(italic(l[5](theta))))
mtext("Less data", adj = 0, cex = cex.labs.fx*.8)

plot(c(0,theta), c(ll_ymin*10, log(likelihood2)), type = "l", xlab = expression(italic(theta)),
     ylab = "", ylim = c(ll_ymin, max(log(likelihood2))))
title(ylab = expression(italic(l[25](theta))))
mtext("More data", adj = 0, cex = cex.labs.fx*.8)

dev.off()



pdf_or_tiff(pdf_ind, "fig9_4", width = figwidth, height = figheight )
par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")
par(mfrow=c(1,1))

step = 0.0001
n1 <- 5
k1 <- 1
n2 <- n1*5
k2 <- k1*5
theta0 <- 0.5
ll_ymin = -8
theta <- seq(step, 1, by = step)

likelihood2 <- choose(n2, k2)*theta^k2*(1-theta)^(n2-k2)
likelihood2.max <- choose(n2, k2) *(k2/n2)^k2 * (1 - k2/n2)^(n2 - k2)
likelihood2.H0 <- choose(n2, k2)*theta0^k2*(1-theta0)^(n2-k2)

plot(c(0,theta), c(ll_ymin*10, log(likelihood2)), type = "l", xaxt = "n",
     ylab = "", xlab = "", ylim = c(ll_ymin, max(log(likelihood2))), cex.axis = cex.axs)
axis(1, at = c(0, k2/n2, theta0, 1), 
     labels = c("0", expression(italic(hat(theta))), expression(theta[0]), 1), cex.axis = cex.axs)
title(xlab = expression(italic(theta)), cex.lab = cex.labs)
title(ylab = expression(italic(l(theta))), cex.lab = cex.labs)
#arrows(k2/n2, log(likelihood2.max), k2/n2, log(likelihood2.H0))
arrows(k2/n2, log(likelihood2.H0), k2/n2, log(likelihood2.max))
arrows(theta0, log(likelihood2.H0), k2/n2, log(likelihood2.H0))
text(0.34, -6.2, "Wald", cex = cex.axs)
text(0.155, -4, "LRT", cex = cex.axs)

dev.off()

####################################################
####################################################
####################################################
#Chapter 10


pdf_or_tiff(pdf_ind, "fig10_1", width = figwidth, height = figheight)
par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")

set.seed(1010)
true.theta <- 0.6
true.sd <- .4
prior.thetamean <- 0
prior.thetasd <- .5
n <- 3
theta.d <- seq(-2, 3, length.out = 100000)
x <- rnorm(n, true.theta, true.sd)
#function to compute the likelihood.
like.theta <- function(theta, x, true.sd){
  lls <- log(dnorm(x, theta, true.sd))
  exp(sum(lls))
}

#likelihood, prior, and unscaled posterior
like.x <- apply(matrix(theta.d, ncol = 1), 1, like.theta, x = x, true.sd = true.sd)
prior.t <- dnorm(theta.d, prior.thetamean, prior.thetasd)
post.us <- like.x * prior.t

#Prior and likelihood unscaled
plot(theta.d, prior.t, type = "l", xlim = c(-1, 1.6), ylab = "", xlab = expression(italic(theta)), 
     lwd = 1.5, lty = 3, cex.axis = cex.axs, cex.lab = cex.labs)
lines(theta.d, like.x, lwd = 1.5, lty = 2)
lines(theta.d, post.us, lwd = 1.5, lty = 1)
rug(x)
legend("topright", lty = 3:1, lwd = 1.5, legend = c("Prior", "Likelihood", "Unscaled Posterior"), bty = "n")

dev.off()


#Figure 10-2
pdf_or_tiff(pdf_ind, "fig10_2", width = figwidth, height = figwidth)
par(mar = mar.fx, las = 1, family = fontfam, bty = "n")
par(mfrow=c(2,1))

plot(theta.d, prior.t, type = "n", xlim = c(-1, 1.6), ylab = "", 
     xlab = expression(italic(theta)), lwd = 1.5, lty = 3,
     cex.axis = cex.axs, cex.lab = cex.labs)

polygon(c(theta.d, rev(theta.d)), c(prior.t, rev(post.us)), col = "black", border = NA)
polygon(c(theta.d, rev(theta.d)), c(post.us, rep(0, length(post.us))), col = "gray", border = NA)
legend("topright", fill = c("black", "gray"), legend = c("Rejected Samples", "Accepted Samples"), border = NA, bty = "n")
mtext("Rejection sampling as in text", adj = 0, cex = cex.labs.fx)


post.s <- post.us / like.theta(mean(x),x, true.sd)
plot(theta.d, prior.t, type = "n", xlim = c(-1, 1.6), ylab = "", 
     xlab = expression(italic(theta)), lwd = 1.5, lty = 3,
     cex.axis = cex.axs, cex.lab = cex.labs)
polygon(c(theta.d, rev(theta.d)), c(prior.t, rev(post.us)), col = "black", border = NA)
polygon(c(theta.d, rev(theta.d)), c(post.s, rep(0, length(post.s))), col = "gray", border = NA)
legend("topright", fill = c("black", "gray"), legend = c("Rejected Samples", "Accepted Samples"), border = NA, bty = "n")
mtext("Rejection sampling after rescaling", adj = 0, cex = cex.labs.fx)

dev.off()


#Figure 10-3
sh <- 5
sc <- 1

pdf_or_tiff(pdf_ind, "fig10_3", width = figwidth, height = figheight)
par(mar = mar.df, mgp = mgp.set, las = 1, family = fontfam, bty = "n")

curve(dgamma(x, sh, sc), xlim = c(0, sh*sc*3), lwd = 1.5, bty = "n", xlab = expression(italic(theta)), 
      ylab = expression(italic(f[theta](theta *"|"* D*"="*d) ) ), cex.axis = cex.axs, cex.lab = cex.labs)
lines(x = rep(sh*sc, 2), y = c(0, 1), lty = 2)
lines(x = rep(qgamma(0.5, sh, sc), 2), y = c(0, 1), lty = 3)
lines(x = rep((sh-1)*sc, 2), y = c(0, 1), lty = 6)

legend("topright", lty = c(2,3,6), 
       legend = c("Posterior Mean","Posterior Median","Posterior Mode"), 
       bty = "n", cex = cex.axs)

dev.off()


pdf_or_tiff(pdf_ind, "fig10_4", width = figwidth, height = figheight*(4/3))
par(mar = mar.fx*c(.6,1,1,1), las = 1, family = fontfam, bty = "n")
par(mfrow=c(2,1))

curve(dgamma(x, sh, sc), xlim = c(0, sh*sc*3), lwd = 1.5, bty = "n", xlab = "", 
      ylab = expression(italic(f[theta](theta *"|"* D*"="*d) ) ), cex.axis = cex.axs, cex.lab = cex.labs)
title(xlab = expression(italic(theta)), mgp = c(2,1,0), cex.lab = cex.labs)
lines(rep(qgamma(0.05, sh, sc), 2), c(0,1), lty = 2)
lines(rep(qgamma(0.95, sh, sc), 2), c(0,1), lty = 2)
xl <- seq(0, qgamma(0.05, sh, sc), length.out = 1000)
xr <- seq(qgamma(0.95, sh, sc), sh*sc*3, length.out = 1000)
polygon(c(xl, rev(xl)), c(dgamma(xl, sh, sc), rep(0, length(xl))), col = "darkgrey", border = NA)
polygon(c(xr, rev(xr)), c(dgamma(xr, sh, sc), rep(0, length(xr))), col = "darkgrey", border = NA)
mtext("Quantile-based interval", adj = 0, cex = cex.labs.fx)

curve(dgamma(x, sh, sc), xlim = c(0, sh*sc*3), lwd = 1.5, bty = "n", xlab = "", 
      ylab = expression(italic(f[theta](theta *"|"* D*"="*d) ) ), cex.axis = cex.axs, cex.lab = cex.labs)
title(xlab = expression(italic(theta)), mgp = c(2,1,0), cex.lab = cex.labs)
lines(rep(1.51, 2), c(0,1), lty = 2)
lines(rep(8.36, 2), c(0,1), lty = 2)
xl <- seq(0, 1.51, length.out = 1000)
xr <- seq(8.36, sh*sc*3, length.out = 1000)
polygon(c(xl, rev(xl)), c(dgamma(xl, sh, sc), rep(0, length(xl))), col = "darkgrey", border = NA)
polygon(c(xr, rev(xr)), c(dgamma(xr, sh, sc), rep(0, length(xr))), col = "darkgrey", border = NA)
mtext("Highest-posterior-density interval", adj = 0, cex = cex.labs.fx)
dev.off()



####################################################
####################################################
####################################################
#Chapter Postlude

#Figure post_1
pdf_or_tiff(pdf_ind, "figPost_1", width = figwidth, height = figheight)
par(mar = mar.df * c(1,1,0,0), mgp = mgp.set, las = 1, family = fontfam)
par(mfrow = c(2,2))
lim.x <- c(4,20)
lim.y <- c(3,13)
plot(anscombe$x1, anscombe$y1, xlab = "x1", ylab = "y1", pch = 19,  
     cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs, bty = "n", xlim = lim.x, ylim = lim.y)
abline(lm(anscombe$y1 ~ anscombe$x1))
plot(anscombe$x2, anscombe$y2, xlab = "x2", ylab = "y2", pch = 19,  
     cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs, bty = "n", xlim = lim.x, ylim = lim.y)
abline(lm(anscombe$y2 ~ anscombe$x2))
plot(anscombe$x3, anscombe$y3, xlab = "x3", ylab = "y3", pch = 19,  
     cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs, bty = "n", xlim = lim.x, ylim = lim.y)
abline(lm(anscombe$y3 ~ anscombe$x3))
plot(anscombe$x4, anscombe$y4, xlab = "x4", ylab = "y4", pch = 19,  
     cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs, bty = "n", xlim = lim.x, ylim = lim.y)
abline(lm(anscombe$y4 ~ anscombe$x4))
dev.off()

#Figure post-2
pdf_or_tiff(pdf_ind, "figPost_2", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
x <- anscombe$x1
mod.poly <- lm(anscombe$y1 ~ poly(x, 10))
xp <- seq(3, 15, length.out = 1000)
y.pred <- predict(mod.poly, data.frame(x = xp) )

plot(anscombe$x1, anscombe$y1, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.axis = cex.axs,
     cex.lab = cex.labs, ylim = c(-1,15))
lines(xp, y.pred)
dev.off()


#Figure post-3. Multiple regression

if(!("rockchalk" %in% installed.packages())){install.packages("rockchalk")}
library(rockchalk)
set.seed(123456)
x1 <- rnorm(50)
x2 <- rnorm(50)
y <- 0.2*x1 + 0.3*x2 + rnorm(50, 0, sqrt(0.8))
mod <- lm(y ~ x1 + x2)
pdf_or_tiff(pdf_ind, "figPost_3", width = figwidth, height = figheight)
par(cex.lab = cex.labs.fx, mar = c(1,1,1,1), oma = c(1,1,1,1), las = 1, family = fontfam)
plotPlane(mod, plotx1 = "x1", plotx2 = "x2", pch = 20, pcol = "black", lcol = "darkgrey", phi = 20, theta = 30, border = NA)
dev.off()



#Figure post-4: GLM vs. linear prob model.
pdf_or_tiff(pdf_ind, "figPost_4", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
set.seed(8675309)
x <- rep(anscombe$x1, 5) + runif(length(anscombe$x1)*5, -.5, .5)
y <- (-2 + 0.3*x + rnorm(length(x), 0, 0.8)) > 0.5
lm.fit <- lm(y ~ x)
prob.fit <- glm(y ~ x, family = binomial("probit"))
logit.fit <- glm(y~x, family = binomial("logit"))
plot(x,y, ylim = c(-0.1,1.1),xlim = c(2,15), xlab = plotlabx, ylab = "Exports Grain (1 = Yes)", 
     pch = 19,  cex = cex.pts, cex.axis = cex.axs, cex.lab = cex.labs)
abline(lm.fit, lwd = 1.5, lty = 1)
x.pl <- seq(0, 20, length.out = 1000)
y.pl <- pnorm(prob.fit$coefficients[1] + prob.fit$coefficients[2]*x.pl)
lines(x.pl, y.pl, lwd = 2, lty = 2)
x.ll <- seq(0, 20, length.out = 1000)
y.ll <- pnorm(logit.fit$coefficients[1] + logit.fit$coefficients[2]*x.pl)
lines(x.ll, y.ll, lwd = 2, lty = 3)
legend(x = 2, y = 1, legend = c("linear fit", "probit fit", "logit fit"), lwd = 2, lty = c(1,2,3), bty = "n")
dev.off()




#Mixed model simulation
set.seed(8675309)
alpha <- 3
beta <- 1/2
eps.sd <- sqrt(1/2)
re.sd <- 1
yrs <- 10
x <- rep(anscombe$x1, 10)
rand.ints <- rnorm(length(anscombe$x1), 0, re.sd)
y <- alpha + beta*x + rep(rand.ints, 10) + rnorm(length(x), 0, eps.sd)


#Figure post-5. Mixed model
pdf_or_tiff(pdf_ind, "figPost_5", width = figwidth, height = figheight)
par(mar = mar.df, las = 1, family = fontfam, bty = "n", mgp = mgp.set)
plot(x, y, xlab = plotlabx, ylab = plotlaby, pch = 19,  cex = cex.pts, cex.lab = cex.labs, cex.axis = cex.axs)
dev.off()





####################################################
####################################################
####################################################
#Appendix A

#Figure A-1

#We want to plot the function f(x) = x^2. We begin by generating
#two vectors that contain some values of x and f(x) that we can plot.
#We will call f(x) "y."
x <- seq(-5,5, by = 0.01)
y <- x^2

#The point of the plot() command is just to draw the background. It contains several
#modifications from the default.
#The ylab and xlab specifications contain extra material that specifies some of the
#letters in the plot to be italic. pch is specified as "" because I do not want to
#draw any point symbols; instead, I will draw the function in the next command using
#lines(). Setting xaxt, yaxt, and bty to "n" stops R from drawing the x axis, y axis,
#and box surrounding the Figure according to its defaults. I do not want the box,
#and I will draw the axes in a different place.
pdf_or_tiff(pdf_ind, "figA_1", width = figwidth, height = figwidth)
par(mar = mar.df, mgp = mgp.set*c(.8,1,1), cex.lab = cex.labs*1.5, las = 1, family = fontfam)
plot(x,y, ylab = expression(paste(italic(f), "(", italic(x), ")", sep = "")), 
     xlab = expression(italic(x)), pch = "", xaxt = "n", yaxt = "n", bty = "n")

#This command draws in the function.
lines(x,y, lwd = 2)

#Draw the axes. Setting pos to 0 in both commands makes the axes cross
#at the origin.
axis(side = 1, pos = 0, at = seq(-5,5, by = 1), cex.axis = cex.axs*1.5)
axis(side = 2, pos = 0, cex.axis = cex.axs*1.5)
dev.off()



#Figure A-3

x <- seq(-5,5, by = 0.01)
y <- x^2
tan.line = 4*x - 4 

pdf_or_tiff(pdf_ind, "figA_3", width = figwidth, height = figwidth)
par(mar = mar.df, mgp = mgp.set*c(.8,1,1), cex.lab = cex.labs*1.5, las = 1, family = fontfam)
plot(x,y, ylab = expression(paste(italic(f), "(", italic(x), ")", sep = "")), 
     xlab = expression(italic(x)), pch = "", xaxt = "n", yaxt = "n", bty = "n")

#This command draws in the function.
lines(x,y, lwd = 2)

#Draw the axes. Setting pos to 0 in both commands makes the axes cross
#at the origin.
axis(side = 1, pos = 0, at = seq(-5,5, by = 1), cex.axis = cex.axs*1.5)
axis(side = 2, pos = 0, cex.axis = cex.axs*1.5)
lines(x,y, lwd = 2) #function
lines(x, tan.line, lwd = 2, lty = 2) #tangent line

dev.off()

#Figure A-4
#A more complicated function and its derivative.
x <- seq(-2,1, length.out = 1000)
y <- .25*x^4 + (2/3)*x^3 - x
dy <- x^3 + 2*x^2 - 1

pdf_or_tiff(pdf_ind, "figA_4", width = figwidth, height = figwidth)
par(mfrow = c(2,1),mar = mar.df*c(.7,.7,1.1,0), mgp = mgp.set*c(.5,1,1),  cex.lab = cex.labs.fx, las = 1, family = fontfam)
plot(x,y, type = "l", ylab = "f(x)", xaxt = "n", yaxt = "n", bty = "n")
axis(side = 1, pos = 0)
axis(side = 2, pos = 0)
lines(c(-1,-1),c(-10,10), lty = 2)
lines(c(-1/2-sqrt(5)/2,-1/2-sqrt(5)/2),c(-10,10), lty = 2)
lines(c(sqrt(5)/2-1/2,sqrt(5)/2-1/2),c(-10,10), lty = 2)
mtext("Original function", adj = 0, cex = cex.labs.fx)
plot(x,dy, type = "l", ylab = "df(x)/dx", xaxt = "n", yaxt = "n", bty = "n")
axis(side = 1, pos = 0)
axis(side = 2, pos = 0)
lines(c(-1,-1),c(-10,10), lty = 2)
lines(c(-1/2-sqrt(5)/2,-1/2-sqrt(5)/2),c(-10,10), lty = 2)
lines(c(sqrt(5)/2-1/2,sqrt(5)/2-1/2),c(-10,10), lty = 2)
mtext("Derivative", adj = 0, cex = cex.labs.fx)
dev.off()


#Figure A-5.

#We want to plot the function f(x) = 2x. We begin by generating
#two vectors that contain some values of x and f(x) that we can plot.
#We will call f(x) "y."
x <- seq(-5,5, by = 0.01)
y <- 2*x

pdf_or_tiff(pdf_ind, "figA_5", width = figwidth, height = figwidth)
par(mar = mar.df, mgp = mgp.set*c(.8,1,1), cex.lab = cex.labs*1.5, las = 1, family = fontfam)
plot(x,y, ylab = expression(paste(italic(f), "(", italic(x), ")", sep = "")), 
     xlab = expression(italic(x)), pch = "", xaxt = "n", yaxt = "n", bty = "n", xlim=c(-4,4), ylim = c(-4,4))
#draw in the function.
lines(x,y, lwd = 2)

#Draw the axes. Setting pos to 0 in both commands makes the axes cross
#at the origin.
axis(side = 1, pos = 0, at = seq(-5,5, by = 1), cex.axis = cex.axs*1.5)
axis(side = 2, pos = 0, cex.axis = cex.axs*1.5)
dev.off()

#Figure A-6, just Fig A-5 with a polygon showing a def integral area:

#We want to plot the function f(x) = 2x. We begin by generating
#two vectors that contain some values of x and f(x) that we can plot.
#We will call f(x) "y."
x <- seq(-5,5, by = 0.01)
y <- 2*x

pdf_or_tiff(pdf_ind, "figA_6", width = figwidth, height = figwidth)
par(mar = mar.df, mgp = mgp.set*c(.8,1,1), cex.lab = cex.labs*1.5, las = 1, family = fontfam)
plot(x,y, ylab = expression(paste(italic(f), "(", italic(x), ")", sep = "")), 
     xlab = expression(italic(x)), pch = "", xaxt = "n", yaxt = "n", bty = "n", xlim=c(-4,4), ylim = c(-4,4))

#This command draws in the function.
lines(x,y, lwd = 2)

#Draw the axes. Setting pos to 0 in both commands makes the axes cross
#at the origin.
axis(side = 1, pos = 0, at = seq(-5,5, by = 1), cex.axis = cex.axs*1.5)
axis(side = 2, pos = 0, cex.axis = cex.axs*1.5)
polygon(x = c(1,1,2,2),y = c(0,2,4,0), col = "grey") # the only new part.
dev.off()


#Figure A-8.

x <- seq(0,5, length.out = 1000)
fx <- x^2
w <- 1/4

pdf_or_tiff(pdf_ind, "figA_8", width = figwidth, height = figheight)
par(mar = mar.df*c(1,.5,1,1), mgp = mgp.set*c(.8,1,1), cex.lab = cex.labs*1.5, las = 1, family = fontfam)
plot(x,fx, ylab = "", xlab = expression(italic(x)), pch = "", xaxt = "n", yaxt = "n", bty = "n", xlim=c(0,4), ylim = c(0,12))
lines(x,fx, lwd = 2)
polygon(c(x[x<=3],3), c(fx[x<=3],0) , col = "darkgrey", border = NA)
axis(side = 1, pos = 0, cex.axis = cex.axs*1.5)
axis(side = 2, pos = 0, cex.axis = cex.axs*1.5)
polygon(c(3, 3, 3+w, 3+w), c(0, 3^2, 3^2, 0), col = "black", border = NA)
#polygon(c(x[x>=3 & x<=3+w], 3+w), c(fx[x>=3 & x<=3+w], 3^2), density =25, angle = 135, col = "black", border = NA)
polygon(c(x[x>=3 & x<=3+w], 3+w), c(fx[x>=3 & x<=3+w], 3^2), col = "lightgray", border = NA)
text(expression(italic("F(3)")), x = 2, y = 2, cex = cex.axs*1.5)
text(expression(italic("f(x)")), x = 1.7, y = 4, srt = 45, cex = cex.axs*1.5)
text("{", x = 3 + w/2, y = -0.2, srt = 90, cex = cex.axs*1.6, xpd = NA)
text(expression(italic("w")), y = -.6, x = 3 + w/2, cex = cex.axs*1.3, xpd = NA)
#text(expression(epsilon), y = (3^2 + (3 + w)^2)/2, x = 3 + w + .1, cex = cex.axs*1.3)
text(expression(epsilon), y = 9+2*w, x = 3 + 2*w/3, cex = cex.axs*1.3)
dev.off()







#tiffs embedded in Figure 8-5 illustrating bootstrapping.
#Illustrate sampling distribution of Gamma(2,2)
tiff("fig_gamma_cdf.tif", width = figwidth, height = figheight, units = "in", compression = "lzw"
     , res = 900)
par(mar = mar.fx*.6, las = 1, family = fontfam, bty = "n")
x <- seq(0,5, length.out = 1000)
Fx <- pgamma(x, 2, 2)
plot(x, Fx, type = "l", xlab = "", 
     ylab = "",  lwd = 2, xlim = c(0,4), ylim = c(0, 1),
     cex.axis = cex.labs.fx, cex.lab = cex.labs.fx)
dev.off()

set.seed(8675309)
n <- 50
stat.func <- function(x){mean(x^3)^(1/3)}
tiff("fig_gamma_ecdf.tif", width = figwidth, height = figheight, units = "in", compression = "lzw"
     , res = 900)
par(mar = mar.fx*.6, las = 1, family = fontfam, bty = "n")
gamsamp <- rgamma(n, 2, 2)
plot(x, Fx,  xlab = "", 
     ylab = "", xlim = c(0,4), ylim = c(0, 1), type = "n",
     cex.axis = cex.labs.fx, cex.lab = cex.labs.fx)
lines(ecdf(gamsamp), verticals = TRUE, do.points = FALSE, main = "", lty = 1, lwd = 2)
dev.off()


tiff("fig_gamma_samp.tif", width = figwidth, height = figheight, units = "in", compression = "lzw"
     , res = 900)
par(mar = mar.fx*.6, las = 1, family = fontfam, bty = "n")
set.seed(8675309)
mat.gam <- matrix(rgamma(n*10000, 2, 2), nrow = 10000)
gam.stats <- apply(mat.gam, 1, stat.func)
hist(gam.stats,  xlab = "", 
     ylab = "", xlim = c(0,4), main = "", freq = FALSE,
     cex.axis = cex.labs.fx, cex.lab = cex.labs.fx, col = "grey55", border = "white")
dev.off()


tiff("fig_gamma_bootstrap.tif", width = figwidth, height = figheight, units = "in", compression = "lzw"
     , res = 900)
par(mar = mar.fx*.6, las = 1, family = fontfam, bty = "n")
set.seed(8675309)
gamsamp <- rgamma(n, 2, 2)
bootmat <- matrix(nrow = 10000, ncol = 100)
for(i in 1:10000){
  bootmat[i,] <- sample(gamsamp, replace = TRUE)
}
boot.stats <- apply(bootmat, 1, stat.func)
hist(boot.stats,  xlab = "", 
     ylab = "", xlim = c(0,4), main = "", freq = FALSE,
     cex.axis = cex.labs.fx, cex.lab = cex.labs.fx, col = "grey55", border = "white")
dev.off()
