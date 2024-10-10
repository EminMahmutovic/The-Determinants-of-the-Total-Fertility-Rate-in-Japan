setwd("C:/Users/Hampu/Desktop/NEK_III_Uppsats")

library(dplyr)
library(forecast)
library(car)
library(orcutt)
library(lmtest)
library(stats)
library(urca)
library(vars)
library(strucchange)
library(aTSA)
library(seasonal)
library(ggplot2)

TFRC <- read.table("TFRC.csv", header=TRUE, sep=",", dec=".")
rownames(TFRC) <- TFRC$Year

tsTFR <- ts(TFRC$TFR)

RegTFR <- lm(TFRC$TFR ~ TFRC$EducUni + I(TFRC$EducUni^2) + TFRC$MR + TFRC$UrbanPop)
dwtest(RegTFR)
RegTFRCO <- cochrane.orcutt(RegTFR)
summary(RegTFRCO)
bptest(RegTFRCO)

plot(RegTFRCO$fitted.values)
tsTFRCO <- ts(RegTFRCO$fitted.values)

### Graph
par(mfrow=c(2,2), oma=c(0,0,0,0))
##QQnorm
qqnorm(RegTFRCO$residuals)
qqline(RegTFRCO$residuals)
### Residuals on fitted values
plot(RegTFRCO$residuals)
abline(h=0)
### Histogram of residuals
hist(RegTFRCO$residuals)
#### Residuals by observation order
plot(RegTFRCO$residuals)
points(RegTFRCO$residuals)
abline(h=0)


### Test with imputation
TFRC$TFR[6] <- NA
TFRC$TFR <- na.interp(TFRC$TFR)
tsTFRIM <- ts(TFRC$TFR)
RegTFRIM <- lm(TFRC$TFR ~ TFRC$EducUni + I(TFRC$EducUni^2) + TFRC$MR + TFRC$UrbanPop)
dwtest(RegTFRIM)
RegTFRCOIM <- cochrane.orcutt(RegTFRIM)
summary(RegTFRCOIM)
bptest(RegTFRCOIM)
plot(RegTFRCOIM$fitted.values)

tsTFRCOIM <- ts(RegTFRCOIM$fitted.values)

plot(tsTFR)
lines(tsTFRCO, col="green")
lines(tsTFRCOIM, col="red")

plot(tsTFRIM, col="red")
lines(tsTFR)


### Test normal differencing and then make a regression
### IT was no good.
TFRdiff <- diff(TFRC$TFR)
Educdiff <- diff(TFRC$EducUni)
MRdiff <- diff(TFRC$MR)
UrbanPopdiff <- diff(TFRC$UrbanPop)

TestDiff <- cbind(TFRdiff, Educdiff, MRdiff, UrbanPopdiff)
View(TestDiff)
regTFRDiff <- lm(TestDiff[,1] ~ TestDiff[,2] + I(TestDiff[,2]^2) + TestDiff[,3] + TestDiff[,4])
summary(regTFRDiff)
regTFRdiffCO <- cochrane.orcutt(regTFRDiff)
summary(regTFRdiffCO)

plot(regTFRdiffCO$fitted.values)

#################
library(readxl)
Agedep <- read_excel("Desktop/Age dependency ratio Japan.xlsx")
View(Agedep)
plot(Agedep)
