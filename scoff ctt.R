##Set working directory
##setwd("E:/Teaching(Short)/ED 6661 - Autumn 2022/R -SCOFF")

##Load Libraries
library(CTT)
library(psych)
library(summarytools)

##Read Data
data <- read.csv("scoff.csv", header=TRUE)
scoff <- (data[,1:5])
attach(scoff)

##Compute a scale(sum) score
sumScore <- rowSums(scoff)

##Item and Scale Frequencies
freq(Item.1)
freq(Item.2)
freq(Item.3)
freq(Item.4)
freq(Item.5)
freq(sumScore)
hist(sumScore, breaks = 5)
describe(sumScore)

##CTT Analysis
itemAnalysis(scoff)
iA = itemAnalysis(scoff, itemReport=TRUE, NA.Delete=TRUE)
iA$itemReport

##Compute a standard error of measurement
sem <- 1.18 * sqrt(1 - .669)
sem


