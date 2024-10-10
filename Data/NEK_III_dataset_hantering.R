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

### Get the population density for japan sorted. 
### Data is from 1961 - 2020, annual data.
### https://databank.worldbank.org/source/world-development-indicators#advancedDownloadOptions
PopDen <- read.table("PopDenJapan.csv", header=TRUE, sep=",", dec=".")
(PopDen)
View(PopDen)
PopDen<-PopDen[-c(1,2)]
View(PopDen)
PopDen <- PopDen[-c(2,3,4,5,6),]
PopDen <- t(PopDen)
View(PopDen)
plot(PopDen[,1])

### Get the TFR data sorted
### Data is fro, 1960 - 2019, annual data. 
### https://databank.worldbank.org/source/world-development-indicators#advancedDownloadOptions
TFR <- read.table("TFR.csv", header=TRUE, sep=",", dec=".")
View(TFR)
TFR <- TFR[-c(1,2)]
TFR <- TFR[-c(2,3,4,5,6),]
TFR <- t(TFR)
View(TFR)
plot(TFR[,1])


##### Manually input the values for participation rates for females. 
##### They can either attend university or junior colleges of technology.
##### WE are interested in the variable EducUni but i wrote down EducJR by 
##### mistake, so keeping it aswell =)
##### THE VALUES HAVE TO BE DUBBELCHECKED

##### The variables will be called EducUni and EducJR
EducUni <- c(2.4, 2.3, 2.5, 2.4, 2.3, 2.5, 3.0, 3.3, 3.9, 5.1, 4.6,
             4.5, 4.9, 5.2, 5.8, 6.5, 7.9, 9.3, 10.6, 11.6, 12.7, 
             13.2, 12.6, 12.4, 12.2, 12.3, 12.2, 12.2, 12.2, 12.6, 
             13.7, 12.5, 13.6, 14.4, 14.7, 15.2, 16.1, 17.2, 19.0, 
             21.0, 22.9, 24.5, 26.0, 27.5, 29.4, 31.5, 32.6, 33.8, 
             34.4, 35.2, 36.8, 38.5, 40.5, 42.6, 44.2, 45.2, 45.8, 
             45.9, 45.7, 47.1, 47.5, 48.4)
EducUni <- as.vector(EducUni)

EducJR <- c(2.5, 2.6, 2.8, 2.7, 2.8, 3.0, 3.5, 4.0, 5.1, 6.4, 6.6, 
           7.2, 8.4, 8.2, 10.2, 11.2, 12.8, 14.3, 16.4, 18.1, 20.1, 
           20.9, 21.0, 20.7, 20.5, 19.9, 20.1, 20.8, 21.0, 21.5, 21.9, 
           22.1, 22.3, 23.2, 23.6, 24.5, 25.0, 24.8, 23.9, 23.1, 22.1, 20.5,
           17.5, 16.1, 14.9, 14.2, 13.8, 13.3, 12.7, 12.2, 11.8, 11.4, 11.1, 
           10.7, 10.1, 9.9, 9.8, 9.6, 9.3)
EducJR <- as.vector(EducJR)

plot(EducUni)
plot(EducJR)

##### Average age of firsta marriage. 
##### We have data for every 5th year starting 1975 going till 2015.
##### Dataset shows for men and women, only making a variable for women.

FMW <- c(25.2, 25.9, 26.4, 26.9, 27.3,
         28.2, 29.4, 30.3, 31.1)
plot(FMW)

#### Marriage rate per 1000 population. Dataset is from 1899 to 2020. 
#### We will only use data from 1960 - 2020
#### https://www.mhlw.go.jp/english/database/db-hw/populate/dl/E02.pdf
MR <- c(9.3, 9.4, 9.8, 9.7, 9.9, 9.7, 9.5, 9.6, 9.5, 9.6,
        10.0, 10.5, 10.4, 9.9, 9.1, 8.5, 7.8, 7.2, 6.9, 6.8, 
        6.7, 6.6, 6.6, 6.4, 6.2, 6.1, 5.9, 5.7, 5.8, 5.8, 
        5.9, 6.0, 6.1, 6.4, 6.3, 6.4, 6.4, 6.2, 6.3, 6.1, 
        6.4, 6.4, 6.0, 5.9, 5.7, 5.7, 5.8, 5.7, 5.8, 5.6, 
        5.5, 5.2, 5.3, 5.3, 5.1, 5.1, 5.0, 4.9, 4.7, 4.8, 4.3 )
MR <- as.vector(MR)
plot(MR)






#########################################################################
#########################################################################
#########################################################################

### Merge the datasets. remove the years with null observations. 
### We will first merge all the variables for which data is gathered annually, 
View(PopDen) ### Data from 1961 - 2020
View(TFR) ### Data from 1960 - 2019
View(EducUni) ### Data from 1955 -2015
View(MR) ### Data from 1960 - 2020
### We will create a dataset that goes from 1961 - 2015
### We will remove observations from the datasets so they match eachother in length. 
### THen we will merge them
PopDen <- PopDen[-c(1,57,58,59,60,61),]
TFR <- TFR[-c(1,57,58,59,60,61),]
EducUni <- EducUni[-c(1,2,3,4,5,6,62)]
MR <- MR[-c(1,57,58,59,60,61)]

PopDen<- as.data.frame(PopDen)
TFR<- as.data.frame(TFR)
EducUni<- as.data.frame(EducUni)
MR<- as.data.frame(MR)

### Now We add the variables to one dataset. We will call this dataset TFRC, TFR-continuous. 
TFRC<- TFR
TFRC$PopDen <- PopDen[,1]
TFRC$EducUni <- EducUni[,1]
TFRC$MR <- MR[,1]
View(TFRC)

### Now we save the dataset as TFRC.csv
### Do not run this code more then once unless you want to create multiple datasets.
### Only run the first line of code below. THe rest is just for testing it works. 
#write.table(TFRC, "TFRC.csv", row.names=FALSE, sep=",", dec=".")
#TFRC_TEST <- read.table("TFRC.csv", header=TRUE, sep=",", dec=".")
#View(TFRC_TEST)




### Now we will create datasets for the variables matching with the variable FMW. 
### While it may be useless we will also include the variable MR (why not? we dont have to use it if we dont want to.)
### We have data for the years:
### 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015
### A total of 9 observation points. To create the dataset we will just take the observed values of the specific year
### for the other variables. 

### Naming the variable TFRD for TFR-discrete.
TFRD <- c(1.909, 1.75, 1.76, 1.54, 1.422, 1.359, 1.26, 1.39, 1.45)
PopDenD <- c(304.4284, 318.8834, 331.4235, 338.667, 344.136, 347.9918, 350.5432, 351.358, 348.8093)
EducUniD <- c(12.7, 12.3, 13.7, 15.2, 22.9, 31.5, 36.8, 45.2, 47.5)
FMW <- c(25.2, 25.9, 26.4, 26.9, 27.3, 28.2, 29.4, 30.3, 31.1)
MRD <- c(8.5, 6.7, 6.1, 5.9, 6.4, 6.4, 5.7, 5.5, 5.1)

TFRD <- as.data.frame(TFRD)
PopDenD <- as.data.frame(PopDenD)
EducUniD <- as.data.frame(EducUniD)
FMW <- as.data.frame(FMW)
MRD <- as.data.frame(MRD)

TFRD$PopDen <- PopDenD[,1]
TFRD$EducUni <- EducUniD[,1]
TFRD$FMW <- FMW[,1]
TFRD$MRD <- MRD[,1]


#write.table(TFRD, "TFRD.csv", row.names=FALSE, sep=",", dec=".")
#TFRD_TEST <- read.table("TFRD.csv", header=TRUE, sep=",", dec=".")
#View(TFRD_TEST)
