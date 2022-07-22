#Set working directory
#setwd("I:/Articles/SCOFF - R")

##Load Libraries
library(ltm)

##Read Data
data <- read.csv("scoff.csv", header=TRUE) 
scoff <- (data[,1:5]) 

##Descriptive information
descript(scoff)
cronbach.alpha(scoff, CI = TRUE, B = 100)

##Fit 2PL Model
fit.2pl <- ltm(scoff ~ z1) 
summary(fit.2pl)
coef(fit.2pl, standardized = TRUE)

##2PL Item Trace Lines
plot(fit.2pl, items = c(1,2,3,4,5), zrange = c(-4,4), lwd = 1, lty=c(2,3,4,5,6), 
     col = "Black",
     legend = TRUE, main = "Item Trace Lines", xlab = expression(~ theta), cx="bottomright", 
     cex = .85, cex.lab = 1.2)

##Item Information Plots
plot(fit.2pl, type = "IIC", zrange = c(-4,4), items = c(1, 2, 3, 4, 5), lwd = 1, 
     lty=c(2,3,4,5,6), 
     col = "Black",legend = T, main = "Item Information Curves", xlab = expression(~ theta), 
     cex = .85, cex.lab = 1.2)

##Scale Information Curve
vals <- plot.ltm(fit.2pl, zrange = c(-4,4), lwd = 1, xlab = expression(~ theta), 
                 cex.lab = 1.2, 
                 ylab = "",type = "IIC",items = 0,
                 main="Scale Information and Standard Errors", yaxt = 'n')

##Information and standard error plots
lines(vals[,"z"], 1 / sqrt(vals[,"info"]), type = "l", lty = 2, lwd = 1)
legend("topright", c("Information", "SE"), lty=1:2, lwd = 1, col=1:1, bty="n", cex = .75)

##Factor scores (latent trait scores)
scores <- factor.scores(fit.2pl, method = c("EAP"))
scores
