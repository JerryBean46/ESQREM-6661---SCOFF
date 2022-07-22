##setwd("M:/SCOFF - difR/")
##setwd("E:/Teaching(short) - ED 6661 Archived/SCOFF - difR")

library(difR)
library(ltm)

data <- read.csv("scoff.csv", header=TRUE)

(difLogistic(data[,1:5], group=data[,6], focal.name=1, type = "udif"))
(difLogistic(data[,1:5], group=data[,6], focal.name=1, type = "nudif"))

(dichoDif(data[,1:5], group=data[,6], focal.name=1, 
          method=c("MH", "Logistic", "Std", "Lord", "Raju")))

lord <- (difLord(data[,1:5], group=data[,6], focal.name = 1, model="2PL", engine = "ltm"))
lord
