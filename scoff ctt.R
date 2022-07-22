#Set working directory
setwd("E:/Teaching(Short)/ED 6661 - Autumn 2022/R -SCOFF")

##Load Libraries
library(CTT)
library(psych)
library(sem)

##Read Data
data <- read.csv("scoff.csv", header=TRUE)
scoff <- (data[,1:5]) #Define data

##Descriptives
sumScore <- scoff$Item.1 + scoff$Item.2 + scoff$Item.3 + scoff$Item.4 + scoff$Item.5
describe(sumScore)

scoff.tab <- table(sumScore)
scoff.tab
scoff.pct <- prop.table(scoff.tab)*100
scoff.pct

##CTT Analysis
itemAnalysis(scoff)

options(digits = 3)
iA = itemAnalysis(scoff, itemReport=TRUE, NA.Delete=TRUE)
iA$itemReport

##Compute a standard error of measurement
sem <- 1.18 * sqrt(1 - .669)
sem

