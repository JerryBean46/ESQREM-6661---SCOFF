#Set working directory
setwd("E:/Teaching(Short)/ED 6661 - Autumn 2022/R -SCOFF")

##Load Libraries
library(mirt)

##Read Data
data <- read.csv("scoff.csv", header=TRUE)
scoff <- (data[,1:5]) #Define data

dim(scoff)
names(scoff)
head(scoff)
apply(scoff, 2, table)

## This command fits a one-factor 2pl model
mod1 <- (mirt(data = scoff, model = 1, verbose = FALSE, SE = TRUE, quadpts = 61, 
              itemtype = c('2PL')))
coef(mod1, simplify = TRUE, IRTpars = TRUE)
coef(mod1, simplify = TRUE, IRTpars = FALSE)
coef(mod1, IRTpars = TRUE, printSE = TRUE)
coef(mod1, IRTpars = TRUE)
summary(mod1)

## This command provides details about model fit
M2(mod1)

##Local Dependence Test - Sorted Cramer's V
rrr <- residuals(mod1, type = 'LD')
CV <- NULL
for (i in 1:nrow(rrr)) {
  for (j in i:ncol(rrr)) {
    if (i !=j) {CV <- rbind(CV, cbind(abs(rrr[i,j]),
                                      rrr[i,j], rownames(rrr)[i], colnames(rrr) [j]))}
  }
}
data.frame(CV[order(CV[,1], decreasing=TRUE),][,-1])

##Item Fit Statistics
itemfit(mod1)

##Scoring
fullscores <- fscores(mod1, method='EAP', full.scores=TRUE, full.scores.SE = TRUE)
fscores <- cbind(fullscores, data)
fscores
summary(fscores$F1)
empirical_rxx(fullscores)
marginal_rxx(mod1)
fullscores2 <- fscores(mod1, method='EAP', full.scores=FALSE, full.scores.SE = TRUE)
fullscores

## Various plots
##Item Probability
plot(mod1, type='trace', which.item = c(1,2,3,4,5), facet_items=T, as.table = TRUE,
              auto.key=list(points=F, lines=T, columns=3, space = 'top', cex = .8), 
              layout=c(3,2),
              theta_lim = c(-3, 3))

##Item Information Curves
plot(mod1, type='infotrace', which.item = c(1,2,3,4,5), facet_items=TRUE, as.table = TRUE,
     auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
     layout=c(3,2), 
     theta_lim = c(-3, 3))                                                                          

##Test Information and Standard Errors
plot(mod1, type = 'infoSE', theta_lim = c(-3, 3))

##Standard Errors
plot(mod1, type = 'SE', theta_lim = c(-3, 3))

##Standard Errors with CIs
plot(mod1, type = 'SE', theta_lim = c(-3, 3), , MI = 1000)

##Reliability
plot(mod1, type = 'rxx', theta_lim = c(-3, 3))

##Test Characteristic Curve
plot(mod1, type = 'score', theta_lim = c(-3, 3))



