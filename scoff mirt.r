##Set working directory
##setwd("E:/Teaching(Short)/ED 6661 - Autumn 2022/R -SCOFF")

##Load Libraries
library(mirt)

##Read Data
data <- read.csv("scoff.csv", header=TRUE)
scoff <- (data[,1:5])

## This command fits a one-factor 2pl model
mod1 <- (mirt(data = scoff, model = 1, verbose = FALSE, SE = TRUE, quadpts = 61, 
              itemtype = c('2PL')))

## This command provides details about model fit
M2(mod1)

##These commands provide details about item parameters
##IRT parameterization with standard errors
coef(mod1, IRTpars = TRUE, printSE = TRUE)

##Factor analysis parameterization
summary(mod1)

##Item Fit Statistics
itemfit(mod1)

##IRT Reliability
marginal_rxx(mod1)

## Various plots
##Item characteristic curves
plot(mod1, type='trace', which.item = c(1,2,3,4,5), facet_items=TRUE, 
     as.table = TRUE,
     auto.key=list(points=F, lines=T, columns=3, space = 'top', cex = .8), 
     layout=c(3,2),
     theta_lim = c(-4, 4))

plot(mod1, type='trace', which.item = c(1,2,3,4,5), facet_items=FALSE, 
     as.table = TRUE,
     auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
     theta_lim = c(-4, 4))

##Item Information Curves
plot(mod1, type='infotrace', which.item = c(1,2,3,4,5), facet_items=TRUE, 
     as.table = TRUE,
     auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
     layout=c(3,2), 
     theta_lim = c(-4, 4))

plot(mod1, type='infotrace', which.item = c(1,2,3,4,5), facet_items=FALSE, 
     as.table = TRUE,
     auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
     theta_lim = c(-4, 4))

##Item Information Curves
plot(mod1, type='infotrace', which.item = c(1,2,3,4,5), facet_items=FALSE, 
     as.table = TRUE,
     auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
     theta_lim = c(-4, 4))       

##Test Information and Standard Errors
plot(mod1, type = 'infoSE', theta_lim = c(-4, 4))

##Standard Errors
plot(mod1, type = 'SE', theta_lim = c(-4, 4))

##Standard Errors with CIs
plot(mod1, type = 'SE', theta_lim = c(-4, 4), MI = 1000)

##Reliability
plot(mod1, type = 'rxx', theta_lim = c(-4, 4))

##Test Characteristic Curve
plot(mod1, type = 'score', theta_lim = c(-4, 4))

## Test characteristic curve (alternative approach)
options(max.print=10000) 
Theta <- matrix(seq(-4,4, .01))
expected <- expected.test(mod1, Theta)
describe(expected)
tail <- (cbind(Theta,expected))
tail
plot(Theta, expected, type = 'l', xlab = expression(~ theta), 
     ylab = "Expected True Score", 
     cex.lab = 1)
abline(h = 2, v = 1.14)

detach(scoff)

help(panel.abline, 
     package="lattice")
