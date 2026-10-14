# =============================================================================
# 03e_garch_ETC.R
#
# Purpose : ETC: return-series tests, ARMA(1,1)-EGARCH(1,1) with Norm/SST/SGED/JSU
#           errors on the full sample (with break dummies) and on SS1-SS3; Pearson IV
#           (PSIV) fitted to standardised residuals; rolling 1-day-ahead VaR
#           (hold-out: last 350 obs of full sample; last 60/200/100 obs of SS1/SS2/SS3); Kupiec, Christoffersen and
#           duration tests; VaR plots.
# Inputs  : data/Cryptocurrency6.csv; R/functions/strucbreak.R
# Outputs : CrCu_ETC_* objects; output/figures/{FS,SS1,SS2,SS3}VaR_ETC.eps
# Paper   : Tables 1, 3, A.2-A.7 (via 11)
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

#### ::::::::: Value at Risk for Cryptocurrencies ####
#### ::::::::: Objective is to find the 1-day ahead VaR for Cryptocurrencies ####
#### ::::::::: find structural breaks using tSNE and CUSUM based Binary Segmentation ####

## Load the following libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(fBasics)
library(dplyr) 
library(MLmetrics) # MAPE and MSE
library(forecast)
library(rugarch)
library(fGarch)
library(Johnson)
library(SuppDists)   # Johnson's SU distribution
library(PerformanceAnalytics) # For estimating the ES
library(xts)## Plot in ugarchroll works best with xts objects
library(cvar) ## Package for ES calculations
library(Dowd) ## Package for test of independence of VaR
library(tseries) ## For conducting basic tests

# :::::::::::: Read the file
# setwd() removed: paths are set in R/00_setup.R
# import dataset
Crypto1 <- read.csv(DATA_FILE, header = TRUE)
Crypto <- read.csv(DATA_FILE, header = TRUE)
Crypto_Date   <- as.Date.character(Crypto$Date,format = "%d/%m/%Y")

#### ********** B I T C O I N ********** ####
# Crypto1 - without date column #

Crypto1 <- data.frame(Crypto1$Bitcoin, Crypto1$Ethereum, Crypto1$Litecoin, Crypto1$Monero, Crypto1$Ethereum.Classic)
colnames(Crypto1) <- c("BTC", "ETH", "LTC", "XMR", "ETC")
View(Crypto1)
Crypto1$ETC = as.numeric(as.character(Crypto1$ETC))
CrCu_ETC <- data.frame(Crypto_Date, Crypto$Ethereum.Classic) # Ethereum Classic Data with Date
View(CrCu_ETC)
# ::::::::: Find structural breaks
# ::::::::: tSNE and CUSUM based Binary Segmentation
# ::::::::: TWO change points

source(file.path("R", "functions", "strucbreak.R"))
set.seed(1)
library(Rtsne)
Crypto1.cp <- CW.Anchal.multi(Crypto1, 100, 2)

CPPP <- data.frame(Crypto1.cp)
CPPP <- CPPP[,1]
CPPP <- sort(CPPP)

CrCu_ETC_samples <- CPPP # Structural break locations

####### :::::::::: Returns Series

Returns_CrCu_ETC <- log(Crypto1$ETC[-1]/Crypto1$ETC[1:(nrow(Crypto1)-1)]) # Returns series
Des_Returns_CrCu_ETC <- basicStats(Returns_CrCu_ETC) # Descriptive statistics
View(Returns_CrCu_ETC)

#### Convert into xts objects ####

CrCu_ETC_xts   <- xts(Returns_CrCu_ETC,order.by = Crypto_Date[-1])
names(CrCu_ETC_xts)   <- "Returns"
View(CrCu_ETC_xts)

# Time series properties of return series 
# H0: The series has unit root
adf_ETC <- adf.test(CrCu_ETC_xts) #p-value proves that returns series is stationary
# H0: The series follow normal distribution
jb_ETC <- jarque.bera.test(CrCu_ETC_xts)
# H0: Independence in the given series
Bt_ETC <- Box.test(CrCu_ETC_xts, type = "Ljung-Box", lag = 10) #pvalue says that there is significant autocorrelation
Bt_ETC_sq <- Box.test(CrCu_ETC_xts^2, type = "Ljung-Box", lag = 10) #pvalue confirms presence of GARCH effect
# H0: No ARCH effects
arch_ETC <- ArchTest(CrCu_ETC_xts, lags = 5) #pvalue confirms evidence of presence of ARCH effects

### Subsamples Dates
#SS1 - 2/12/2016 to 27/09/2017
#SS2 - 28/09/2017 to 23/06/2020
#SS3 - 24/06/2020 to 04/12/2021

### Subsamples XTS ###
CrCu_ETC_sub1    <- CrCu_ETC_xts[1:CrCu_ETC_samples[1]]
CrCu_ETC_sub2    <- CrCu_ETC_xts[(CrCu_ETC_samples[1]+1):CrCu_ETC_samples[2]]
CrCu_ETC_sub3    <- CrCu_ETC_xts[(CrCu_ETC_samples[2]+1):nrow(CrCu_ETC_xts)]

### Dummy variables for structural breaks (to be used in Garch modelling)####

CrCu_ETC_Dummy1 <- array(data=0,dim=length(CrCu_ETC_xts))
CrCu_ETC_Dummy2 <- array(data=0,dim=length(CrCu_ETC_xts))
CrCu_ETC_Dummy3 <- array(data=0,dim=length(CrCu_ETC_xts))

CrCu_ETC_Dummy1[1:CrCu_ETC_samples[1]]  <- 1
CrCu_ETC_Dummy2[(CrCu_ETC_samples[1]+1):CrCu_ETC_samples[2]] <- 1
CrCu_ETC_Dummy3[(CrCu_ETC_samples[2]+1):length(CrCu_ETC_xts)] <- 1

CrCu_ETC_Dummy <- cbind(CrCu_ETC_Dummy1,CrCu_ETC_Dummy2,CrCu_ETC_Dummy3)

## :::::::::::: check presence of ARCH effetc in Full Sample
library(FinTS)
ArchTest(CrCu_ETC_xts, lags = 1)
ArchTest(CrCu_ETC_xts, lags = 5)
ArchTest(CrCu_ETC_xts, lags = 10)
pacf((CrCu_ETC_xts)^2)
# H0: Series has no ARCH effects
# Null hypothesis is rejected, hence proves presence of ARCH effects

### Garch Models
# :::::::::With external regressors in Full Sample GARCH

#SST #SGED #JSU #Norm
CrCu_ETC_Spec1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETC_Spec2 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETC_Spec3 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETC_Spec4 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETC_Model1 <- ugarchfit(CrCu_ETC_Spec1,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model2 <- ugarchfit(CrCu_ETC_Spec2,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model3 <- ugarchfit(CrCu_ETC_Spec3,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model4 <- ugarchfit(CrCu_ETC_Spec4,CrCu_ETC_xts,out.sample = 350)

#### egarch models

CrCu_ETC_Spec5 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETC_Spec6 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETC_Spec7 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETC_Spec8 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETC_Model5 <- ugarchfit(CrCu_ETC_Spec5,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model6 <- ugarchfit(CrCu_ETC_Spec6,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model7 <- ugarchfit(CrCu_ETC_Spec7,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model8 <- ugarchfit(CrCu_ETC_Spec8,CrCu_ETC_xts,out.sample = 350)

### gjrgarch models

CrCu_ETC_Spec9  <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETC_Spec10 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETC_Spec11 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETC_Spec12 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_ETC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETC_Model9  <- ugarchfit(CrCu_ETC_Spec9,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model10 <- ugarchfit(CrCu_ETC_Spec10,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model11 <- ugarchfit(CrCu_ETC_Spec11,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model12 <- ugarchfit(CrCu_ETC_Spec12,CrCu_ETC_xts,out.sample = 350)
#uncmean(CrCu_ETC_Model5)
#uncvariance(CrCu_ETC_Model1)
#halfline(CrCu_ETC_Model5)
#plot.ts(sigma(CrCu_ETC_Model5), ylab="sigma(t)", col="blue")
#plot(CrCu_ETC_Model5)

## persistence

persis_ETC <- matrix(data=0, nrow = 12, ncol = 1)
persis_ETC[1,1] <- round(persistence(CrCu_ETC_Model1),4)
persis_ETC[2,1] <- round(persistence(CrCu_ETC_Model2),4)
persis_ETC[3,1] <- round(persistence(CrCu_ETC_Model3),4)
persis_ETC[4,1] <- round(persistence(CrCu_ETC_Model4),4)
persis_ETC[5,1] <- round(persistence(CrCu_ETC_Model5),4)
persis_ETC[6,1] <- round(persistence(CrCu_ETC_Model6),4)
persis_ETC[7,1] <- round(persistence(CrCu_ETC_Model7),4)
persis_ETC[8,1] <- round(persistence(CrCu_ETC_Model8),4)
persis_ETC[9,1] <- round(persistence(CrCu_ETC_Model9),4)
persis_ETC[10,1] <- round(persistence(CrCu_ETC_Model10),4)
persis_ETC[11,1] <- round(persistence(CrCu_ETC_Model11),4)
persis_ETC[12,1] <- round(persistence(CrCu_ETC_Model12),4)

persistance_ETC <- matrix(data=persis_ETC, nrow = 12, ncol = 1)
colnames(persistance_ETC) <- c("Persistence_ETC")
rownames(persistance_ETC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(data.frame(persistance_ETC))
#xtable(persistance_ETC, digits = 4)

### Store the loglikelihood values, AIC and BIC ####

Llhood_ETC  <- matrix(data=0,nrow=12,ncol=1)
colnames(Llhood_ETC) <- c("CrCu_ETC")
rownames(Llhood_ETC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
Llhood_ETC[1,1]  <-  CrCu_ETC_Model1@fit$LLH
Llhood_ETC[2,1]  <-  CrCu_ETC_Model2@fit$LLH
Llhood_ETC[3,1]  <-  CrCu_ETC_Model3@fit$LLH
Llhood_ETC[4,1]  <-  CrCu_ETC_Model4@fit$LLH
Llhood_ETC[5,1]  <-  CrCu_ETC_Model5@fit$LLH
Llhood_ETC[6,1]  <-  CrCu_ETC_Model6@fit$LLH
Llhood_ETC[7,1]  <-  CrCu_ETC_Model7@fit$LLH
Llhood_ETC[8,1]  <-  CrCu_ETC_Model8@fit$LLH
Llhood_ETC[9,1]  <-  CrCu_ETC_Model9@fit$LLH
Llhood_ETC[10,1] <- CrCu_ETC_Model10@fit$LLH
Llhood_ETC[11,1] <- CrCu_ETC_Model11@fit$LLH
Llhood_ETC[12,1] <- CrCu_ETC_Model12@fit$LLH

BIC_ETC  <- matrix(data=0,nrow=12,ncol=1)
colnames(BIC_ETC) <- c("CrCu_ETC")
rownames(BIC_ETC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
BIC_ETC[1,1]  <-  infocriteria(CrCu_ETC_Model1)[2]
BIC_ETC[2,1]  <-  infocriteria(CrCu_ETC_Model2)[2]
BIC_ETC[3,1]  <-  infocriteria(CrCu_ETC_Model3)[2]
BIC_ETC[4,1]  <-  infocriteria(CrCu_ETC_Model4)[2]
BIC_ETC[5,1]  <-  infocriteria(CrCu_ETC_Model5)[2]
BIC_ETC[6,1]  <-  infocriteria(CrCu_ETC_Model6)[2]
BIC_ETC[7,1]  <-  infocriteria(CrCu_ETC_Model7)[2]
BIC_ETC[8,1]  <-  infocriteria(CrCu_ETC_Model8)[2]
BIC_ETC[9,1]  <-  infocriteria(CrCu_ETC_Model9)[2]
BIC_ETC[10,1] <- infocriteria(CrCu_ETC_Model10)[2]
BIC_ETC[11,1] <- infocriteria(CrCu_ETC_Model11)[2]
BIC_ETC[12,1] <- infocriteria(CrCu_ETC_Model12)[2]

AIC_ETC  <- matrix(data=0,nrow=12,ncol=1)
colnames(AIC_ETC) <- c("CrCu_ETC")
rownames(AIC_ETC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
AIC_ETC[1,1]  <-  infocriteria(CrCu_ETC_Model1)[1]
AIC_ETC[2,1]  <-  infocriteria(CrCu_ETC_Model2)[1]
AIC_ETC[3,1]  <-  infocriteria(CrCu_ETC_Model3)[1]
AIC_ETC[4,1]  <-  infocriteria(CrCu_ETC_Model4)[1]
AIC_ETC[5,1]  <-  infocriteria(CrCu_ETC_Model5)[1]
AIC_ETC[6,1]  <-  infocriteria(CrCu_ETC_Model6)[1]
AIC_ETC[7,1]  <-  infocriteria(CrCu_ETC_Model7)[1]
AIC_ETC[8,1]  <-  infocriteria(CrCu_ETC_Model8)[1]
AIC_ETC[9,1]  <-  infocriteria(CrCu_ETC_Model9)[1]
AIC_ETC[10,1] <- infocriteria(CrCu_ETC_Model10)[1]
AIC_ETC[11,1] <- infocriteria(CrCu_ETC_Model11)[1]
AIC_ETC[12,1] <- infocriteria(CrCu_ETC_Model12)[1]

Infocrit_ETC  <- matrix(data=0,nrow=12,ncol=3)
colnames(Infocrit_ETC) <- c("Likelihood", "BIC", "AIC")
rownames(Infocrit_ETC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")

Infocrit_ETC[,1] = Llhood_ETC
Infocrit_ETC[,2] = BIC_ETC
Infocrit_ETC[,3] = AIC_ETC
View(data.frame(Infocrit_ETC))
xtable(Infocrit_ETC, caption = "Information Criteria for Ethereum Classic GARCH models - Full Sample", digits = 4)

## Conclusion: eGarch-JSU have the highest AIC and eGarch-SST have the second highest #####
## Model 7 and the Model 5

## coefficients

coef_ETC_1 <- round(coef(CrCu_ETC_Model1),4)
coef_ETC_2 <- round(coef(CrCu_ETC_Model2),4)
coef_ETC_3 <- round(coef(CrCu_ETC_Model3),4)
coef_ETC_4 <- round(coef(CrCu_ETC_Model4),4)
coef_ETC_5 <- round(coef(CrCu_ETC_Model5),4)
coef_ETC_6 <- round(coef(CrCu_ETC_Model6),4)
coef_ETC_7 <- round(coef(CrCu_ETC_Model7),4)
coef_ETC_8 <- round(coef(CrCu_ETC_Model8),4)
coef_ETC_9 <- round(coef(CrCu_ETC_Model9),4)
coef_ETC_10 <- round(coef(CrCu_ETC_Model10),4)
coef_ETC_11 <- round(coef(CrCu_ETC_Model11),4)
coef_ETC_12 <- round(coef(CrCu_ETC_Model12),4)

garch_coef_ETC <- bind_rows(coef_ETC_1, coef_ETC_2, coef_ETC_3, coef_ETC_4,coef_ETC_5, coef_ETC_6, coef_ETC_7, coef_ETC_8, coef_ETC_9, coef_ETC_10, coef_ETC_11, coef_ETC_12)

garch_coef_ETC1 <- as.data.frame(garch_coef_ETC)
rownames(garch_coef_ETC1) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(garch_coef_ETC1)
xtable(garch_coef_ETC1, caption = "Coefficients of Ethereum Classic GARCH models - Full Sample", digits = 4)

### Ljung-box test statistic
# Check standardized shocks

#CrCu_ETC5_LQ1  <- Box.test(CrCu_ETC_Model5@fit$residuals/CrCu_ETC_Model5@fit$sigma,lag=1,type="Lj")
#CrCu_ETC5_LQ5  <- Box.test(CrCu_ETC_Model5@fit$residuals/CrCu_ETC_Model5@fit$sigma,lag=5,type="Lj")
#CrCu_ETC5_LQ10 <- Box.test(CrCu_ETC_Model5@fit$residuals/CrCu_ETC_Model5@fit$sigma,lag=10,type="Lj")

#CrCu_ETC6_LQ1  <- Box.test(CrCu_ETC_Model6@fit$residuals/CrCu_ETC_Model6@fit$sigma,lag=1,type="Lj")
#CrCu_ETC6_LQ5  <- Box.test(CrCu_ETC_Model6@fit$residuals/CrCu_ETC_Model6@fit$sigma,lag=5,type="Lj")
#CrCu_ETC6_LQ10 <- Box.test(CrCu_ETC_Model6@fit$residuals/CrCu_ETC_Model6@fit$sigma,lag=10,type="Lj")

#CrCu_ETC7_LQ1  <- Box.test(CrCu_ETC_Model7@fit$residuals/CrCu_ETC_Model7@fit$sigma,lag=1,type="Lj")
#CrCu_ETC7_LQ5  <- Box.test(CrCu_ETC_Model7@fit$residuals/CrCu_ETC_Model7@fit$sigma,lag=5,type="Lj")
#CrCu_ETC7_LQ10 <- Box.test(CrCu_ETC_Model7@fit$residuals/CrCu_ETC_Model7@fit$sigma,lag=10,type="Lj")

#CrCu_ETC8_LQ1  <- Box.test(CrCu_ETC_Model8@fit$residuals/CrCu_ETC_Model8@fit$sigma,lag=1,type="Lj")
#CrCu_ETC8_LQ5  <- Box.test(CrCu_ETC_Model8@fit$residuals/CrCu_ETC_Model8@fit$sigma,lag=5,type="Lj")
#CrCu_ETC8_LQ10 <- Box.test(CrCu_ETC_Model8@fit$residuals/CrCu_ETC_Model8@fit$sigma,lag=10,type="Lj")

#CrCu_ETC5_LQ <- c(CrCu_ETC5_LQ1$statistic,CrCu_ETC5_LQ5$statistic,CrCu_ETC5_LQ10$statistic)
#CrCu_ETC6_LQ <- c(CrCu_ETC6_LQ1$statistic,CrCu_ETC6_LQ5$statistic,CrCu_ETC6_LQ10$statistic)
#CrCu_ETC7_LQ <- c(CrCu_ETC7_LQ1$statistic,CrCu_ETC7_LQ5$statistic,CrCu_ETC7_LQ10$statistic)
#CrCu_ETC8_LQ <- c(CrCu_ETC8_LQ1$statistic,CrCu_ETC8_LQ5$statistic,CrCu_ETC8_LQ10$statistic)

#CrCu_ETC_res_stat <-   cbind(CrCu_ETC5_LQ,CrCu_ETC6_LQ,CrCu_ETC7_LQ,CrCu_ETC8_LQ)
#rownames(CrCu_ETC_res_stat) <- c("LQ1","LQ5","LQ10")
#colnames(CrCu_ETC_res_stat) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

#### 1-day ahead Rolling Forecasts ####
## Full sample for roll i.e. last 350 points for forecast and rest for model
CrCu_ETC_Roll5 <-  ugarchroll(CrCu_ETC_Spec5,CrCu_ETC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll6 <-  ugarchroll(CrCu_ETC_Spec6,CrCu_ETC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll7 <-  ugarchroll(CrCu_ETC_Spec7,CrCu_ETC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll8 <-  ugarchroll(CrCu_ETC_Spec8,CrCu_ETC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model

garchsigSST_ETC <- CrCu_ETC_Roll5@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll5@forecast$density$Realized)
MSEvalSST_ETC <- MSE(garchsigSST_ETC, realval_ETC)
MAPEgvalSST_ETC <- MAPE(garchsigSST_ETC, realval_ETC)

garchsigSGED_ETC <- CrCu_ETC_Roll6@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll6@forecast$density$Realized)
MSEvalSGED_ETC <- MSE(garchsigSGED_ETC, realval_ETC)
MAPEgvalSGED_ETC <- MAPE(garchsigSGED_ETC, realval_ETC)

garchsigJSU_ETC <- CrCu_ETC_Roll7@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll7@forecast$density$Realized)
MSEvalJSU_ETC <- MSE(garchsigJSU_ETC, realval_ETC)
MAPEgvalJSU_ETC <- MAPE(garchsigJSU_ETC, realval_ETC)

garchsigNorm_ETC <- CrCu_ETC_Roll8@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll8@forecast$density$Realized)
MSEvalNorm_ETC <- MSE(garchsigNorm_ETC, realval_ETC)
MAPEgvalNorm_ETC <- MAPE(garchsigNorm_ETC, realval_ETC)

MAPEgval_ETC  <- matrix(data=0,nrow=4,ncol=2)
colnames(MAPEgval_ETC) <- c("MSE", "MAPE")
rownames(MAPEgval_ETC) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")

MAPEgval_ETC[,1] = cbind(MSEvalSST_ETC,MSEvalSGED_ETC,MSEvalJSU_ETC,MSEvalNorm_ETC)
MAPEgval_ETC[,2] = cbind(MAPEgvalSST_ETC,MAPEgvalSGED_ETC,MAPEgvalJSU_ETC,MAPEgvalNorm_ETC)

xtable(MAPEgval_ETC, caption = "MSE & MAPE for out of sample forecast of Ethereum Classic GARCH - Full Sample", digits = 4)

#### Store the VaR for the out sample using rolling window forecasts

CrCu_ETC_VaR_SST   <- CrCu_ETC_Roll5@forecast$VaR
CrCu_ETC_VaR_SGED  <- CrCu_ETC_Roll6@forecast$VaR
CrCu_ETC_VaR_JSU   <- CrCu_ETC_Roll7@forecast$VaR
CrCu_ETC_VaR_Norm  <- CrCu_ETC_Roll8@forecast$VaR

#### ::::::::::::: Repeating for P E A R S O N Distribution  #####

### Check the summary stats of the standardized residuals
CrCu_ETC_Model8_res <- CrCu_ETC_Model8@fit$residuals/CrCu_ETC_Model8@fit$sigma
CrCu_ETC_Descr8 <- basicStats(CrCu_ETC_Model8_res)

### Fitting distributions on the standardized residuals 

library(PearsonDS)
library(gsl)
CrCu_ETC_Dist8   <- pearsonFitML(CrCu_ETC_Model8_res)
CrCu_ETC_JSU_param <- JohnsonFit(CrCu_ETC_Model8_res,moment ="find")

### Store the parameters for different distributions  

CrCu_ETC_P4_Param    <- CrCu_ETC_Dist8[2:5]

### Checking the fit using KS tests

CrCu_ETC_test1 <- ks.test(CrCu_ETC_Model8_res,rpearson(length(CrCu_ETC_Model8_res),params = CrCu_ETC_Dist8))
#Null: True distribution is equal to the hypothesized distribution
#p-value is high, hence, we fail to reject H0.

### Calculate the quantiles

ETC_Exceed_Pearson    <- array(data=0,dim=length(signific_levels))
qpearson(signific_levels[2],CrCu_ETC_Dist8)
for(j in 1:3)
{
  Z_Pearson_ETC   <- qpearson(signific_levels[j],CrCu_ETC_Dist8)
  
  mu_ETC   <- array(data=0,dim=length(Returns_CrCu_ETC)-350)
  VaR_ETC   <- array(data=0,dim=length(Returns_CrCu_ETC)-351)
  
  for(i in 2:(length(Returns_CrCu_ETC)-350))
  {
    mu_ETC[i] <- CrCu_ETC_Model8@fit$coef[1]+CrCu_ETC_Model8@fit$coef[2]*Returns_CrCu_ETC[i-1]+CrCu_ETC_Model8@fit$coef[3]*CrCu_ETC_Model8_res[i-1]
    VaR_ETC[i-1] <- mu_ETC[i]+CrCu_ETC_Model8@fit$sigma[i]*Z_Pearson_ETC
  }
  #ETC_Violations[j]   <- VaRTest(signific_levels[j],Returns_CrCu_ETC[1:(length(VaR_ETC))],VaR_ETC)
  print(VaRTest(signific_levels[j],Returns_CrCu_ETC[1:(length(VaR_ETC))],VaR_ETC))
  #ETC_Exceed_Pearson[j] <- ETC_Violations$actual.exceed
}

#### Out of sample VaR forecast  #####

Holdout_Returns_ETC   <- Returns_CrCu_ETC[(length(Returns_CrCu_ETC)-349):length(Returns_CrCu_ETC)] 

Holdout_ETC_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_ETC_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_ETC_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_ETC_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_ETC_Pearson) <- c("1%","5%","10%")
rownames(Holdout_ETC_Pearson_pvalues) <- c("1%","5%","10%")

## Store the volatility and mu forecast
signific_levels <- c(0.1,0.05,0.01)

ETC_Holdout_Mu <- as.data.frame(CrCu_ETC_Roll8)[,'Mu']
ETC_Holdout_Sigma <- as.data.frame(CrCu_ETC_Roll8)[,'Sigma']

MP_ETC <- c(0 , 0, 0)

for(j in 1:3)
{
  
  Z_Pearson_ETC   <- qpearson(signific_levels[j],CrCu_ETC_Dist8)
  
  ETC_Holdout_VaR   <- ETC_Holdout_Mu + ETC_Holdout_Sigma*Z_Pearson_ETC
  ETC_Violations   <- BacktestVaR(Holdout_Returns_ETC[1:(length(Holdout_Returns_ETC)-1)],ETC_Holdout_VaR[-1],signific_levels[j])
  Holdout_ETC_Pearson[j,]   <- c(ETC_Violations$LRuc[1],ETC_Violations$LRcc[1],ETC_Violations$DQ$stat,ETC_Violations$AE,ETC_Violations$Loss$Loss)
  Holdout_ETC_Pearson_pvalues[j,] <- c(ETC_Violations$LRuc[2],ETC_Violations$LRcc[2],ETC_Violations$DQ$pvalue)
  MP_ETC[[j]] <- MAPE(ETC_Holdout_VaR, abs(CrCu_ETC_VaR_Norm$realized))
  print(ETC_Violations$AE)
  #print(ETC_Violations)
}

########## ::::::::::::::: MAPE ###########
# Forecast performance measure for full sample
# Value at Risk

m1_ETC <- MAPE(CrCu_ETC_VaR_SST$`alpha(1%)`, abs(CrCu_ETC_VaR_SST$realized))
m2_ETC <- MAPE(CrCu_ETC_VaR_SST$`alpha(5%)`, abs(CrCu_ETC_VaR_SST$realized))
m3_ETC <- MAPE(CrCu_ETC_VaR_SST$`alpha(10%)`, abs(CrCu_ETC_VaR_SST$realized))

m4_ETC <- MAPE(CrCu_ETC_VaR_SGED$`alpha(1%)`, abs(CrCu_ETC_VaR_SGED$realized))
m5_ETC <- MAPE(CrCu_ETC_VaR_SGED$`alpha(5%)`, abs(CrCu_ETC_VaR_SGED$realized))
m6_ETC <- MAPE(CrCu_ETC_VaR_SGED$`alpha(10%)`, abs(CrCu_ETC_VaR_SGED$realized))

m7_ETC <- MAPE(CrCu_ETC_VaR_JSU$`alpha(1%)`, abs(CrCu_ETC_VaR_JSU$realized))
m8_ETC <- MAPE(CrCu_ETC_VaR_JSU$`alpha(5%)`, abs(CrCu_ETC_VaR_JSU$realized))
m9_ETC <- MAPE(CrCu_ETC_VaR_JSU$`alpha(10%)`, abs(CrCu_ETC_VaR_JSU$realized))

m10_ETC <- MAPE(CrCu_ETC_VaR_Norm$`alpha(1%)`, abs(CrCu_ETC_VaR_Norm$realized))
m11_ETC <- MAPE(CrCu_ETC_VaR_Norm$`alpha(5%)`, abs(CrCu_ETC_VaR_Norm$realized))
m12_ETC <- MAPE(CrCu_ETC_VaR_Norm$`alpha(10%)`, abs(CrCu_ETC_VaR_Norm$realized))

m13_ETC <- MP_ETC[3]
m14_ETC <- MP_ETC[2]
m15_ETC <- MP_ETC[1]

MAPEval_ETC  <- matrix(data=0,nrow=5,ncol=3)
colnames(MAPEval_ETC) <- c("1%", "5%", "10%")
rownames(MAPEval_ETC) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm", "eGarch-Pearson")

MAPEval_ETC[,1] = cbind(m1_ETC,m4_ETC,m7_ETC,m10_ETC,m13_ETC)
MAPEval_ETC[,2] = cbind(m2_ETC,m5_ETC,m8_ETC,m11_ETC,m14_ETC)
MAPEval_ETC[,3] = cbind(m3_ETC,m6_ETC,m9_ETC,m12_ETC,m15_ETC)

xtable(MAPEval_ETC, caption = "MAPE for out of sample forecast of Ethereum Classic Value at Risk - Full Sample", digits = 4)

signific_levels <- c(0.01,0.05,0.10)

##### Backtesting
ETC_Test_SST_1  <- BacktestVaR(CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,1],signific_levels[1])
ETC_Test_SST_5  <- BacktestVaR(CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,2],signific_levels[2])
ETC_Test_SST_10 <- BacktestVaR(CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,3],signific_levels[3])

ETC_Test_SGED_1  <- BacktestVaR(CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,1],signific_levels[1])
ETC_Test_SGED_5  <- BacktestVaR(CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,2],signific_levels[2])
ETC_Test_SGED_10 <- BacktestVaR(CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,3],signific_levels[3])

ETC_Test_JSU_1  <- BacktestVaR(CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,1],signific_levels[1])
ETC_Test_JSU_5  <- BacktestVaR(CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,2],signific_levels[2])
ETC_Test_JSU_10 <- BacktestVaR(CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,3],signific_levels[3])

ETC_Test_Norm_1  <- BacktestVaR(CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,1],signific_levels[1])
ETC_Test_Norm_5  <- BacktestVaR(CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,2],signific_levels[2])
ETC_Test_Norm_10 <- BacktestVaR(CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,3],signific_levels[3])

#### Saving the test stats and pvalues for 1%, 5% and 10% VaR

## 1%

ETC_Backtest_teststat1  <- matrix(c(ETC_Test_SST_1$LRuc[1],ETC_Test_SST_1$LRcc[1],ETC_Test_SST_1$DQ$stat,
                                    ETC_Test_SGED_1$LRuc[1],ETC_Test_SGED_1$LRcc[1],ETC_Test_SGED_1$DQ$stat,
                                    ETC_Test_JSU_1$LRuc[1],ETC_Test_JSU_1$LRcc[1],ETC_Test_JSU_1$DQ$stat,
                                    ETC_Test_Norm_1$LRuc[1],ETC_Test_Norm_1$LRcc[1],ETC_Test_Norm_1$DQ$stat),nrow=4,byrow = TRUE)
rownames(ETC_Backtest_teststat1)  <- c("SST","SGED","JSU","Norm")
colnames(ETC_Backtest_teststat1)  <- c("Kupiec","Christoffersen","DQ")

ETC_Backtest_pvalues1  <- matrix(c(ETC_Test_SST_1$LRuc[2],ETC_Test_SST_1$LRcc[2],ETC_Test_SST_1$DQ$pvalue,
                                   ETC_Test_SGED_1$LRuc[2],ETC_Test_SGED_1$LRcc[2],ETC_Test_SGED_1$DQ$pvalue,
                                   ETC_Test_JSU_1$LRuc[2],ETC_Test_JSU_1$LRcc[2],ETC_Test_JSU_1$DQ$pvalue,
                                   ETC_Test_Norm_1$LRuc[2],ETC_Test_Norm_1$LRcc[2],ETC_Test_Norm_1$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(ETC_Backtest_pvalues1)  <- c("SST","SGED","JSU","Norm")
colnames(ETC_Backtest_pvalues1)  <- c("Kupiec","Christoffersen","DQ")

### 5%

ETC_Backtest_teststat5  <- matrix(c(ETC_Test_SST_5$LRuc[1],ETC_Test_SST_5$LRcc[1],ETC_Test_SST_5$DQ$stat,
                                    ETC_Test_SGED_5$LRuc[1],ETC_Test_SGED_5$LRcc[1],ETC_Test_SGED_5$DQ$stat,
                                    ETC_Test_JSU_5$LRuc[1],ETC_Test_JSU_5$LRcc[1],ETC_Test_JSU_5$DQ$stat,
                                    ETC_Test_Norm_5$LRuc[1],ETC_Test_Norm_5$LRcc[1],ETC_Test_Norm_5$DQ$stat),nrow=4,byrow = TRUE)
rownames(ETC_Backtest_teststat5)  <- c("SST","SGED","JSU","Norm")
colnames(ETC_Backtest_teststat5)  <- c("Kupiec","Christoffersen","DQ")

ETC_Backtest_pvalues5  <- matrix(c(ETC_Test_SST_5$LRuc[2],ETC_Test_SST_5$LRcc[2],ETC_Test_SST_5$DQ$pvalue,
                                   ETC_Test_SGED_5$LRuc[2],ETC_Test_SGED_5$LRcc[2],ETC_Test_SGED_5$DQ$pvalue,
                                   ETC_Test_JSU_5$LRuc[2],ETC_Test_JSU_5$LRcc[2],ETC_Test_JSU_5$DQ$pvalue,
                                   ETC_Test_Norm_5$LRuc[2],ETC_Test_Norm_5$LRcc[2],ETC_Test_Norm_5$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(ETC_Backtest_pvalues5)  <- c("SST","SGED","JSU","Norm")
colnames(ETC_Backtest_pvalues5)  <- c("Kupiec","Christoffersen","DQ")

### 10%

ETC_Backtest_teststat10  <- matrix(c(ETC_Test_SST_10$LRuc[1],ETC_Test_SST_10$LRcc[1],ETC_Test_SST_10$DQ$stat,
                                     ETC_Test_SGED_10$LRuc[1],ETC_Test_SGED_10$LRcc[1],ETC_Test_SGED_10$DQ$stat,
                                     ETC_Test_JSU_10$LRuc[1],ETC_Test_JSU_10$LRcc[1],ETC_Test_JSU_10$DQ$stat,
                                     ETC_Test_Norm_10$LRuc[1],ETC_Test_Norm_10$LRcc[1],ETC_Test_Norm_10$DQ$stat),nrow=4,byrow = TRUE)
rownames(ETC_Backtest_teststat10)  <- c("SST","SGED","JSU","Norm")
colnames(ETC_Backtest_teststat10)  <- c("Kupiec","Christoffersen","DQ")

ETC_Backtest_pvalues10  <- matrix(c(ETC_Test_SST_10$LRuc[2],ETC_Test_SST_10$LRcc[2],ETC_Test_SST_10$DQ$pvalue,
                                    ETC_Test_SGED_10$LRuc[2],ETC_Test_SGED_10$LRcc[2],ETC_Test_SGED_10$DQ$pvalue,
                                    ETC_Test_JSU_10$LRuc[2],ETC_Test_JSU_10$LRcc[2],ETC_Test_JSU_10$DQ$pvalue,
                                    ETC_Test_Norm_10$LRuc[2],ETC_Test_Norm_10$LRcc[2],ETC_Test_Norm_10$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(ETC_Backtest_pvalues10)  <- c("SST","SGED","JSU","Norm")
colnames(ETC_Backtest_pvalues10)  <- c("Kupiec","Christoffersen","DQ")

ETC_Backtest_pvalues1  <- round(ETC_Backtest_pvalues1,4)
ETC_Backtest_pvalues5  <- round(ETC_Backtest_pvalues5,4)
ETC_Backtest_pvalues10 <- round(ETC_Backtest_pvalues10,4)

ETC_Backtest_teststat1  <- round(ETC_Backtest_teststat1,4)
ETC_Backtest_teststat5  <- round(ETC_Backtest_teststat5,4)
ETC_Backtest_teststat10 <- round(ETC_Backtest_teststat10,4)

#####################################################################################
### Using VaRTest and VaRDuration Test individually ###
## VaRTest in R

CrCu_ETC_Norm_VaR_Test1  <- VaRTest(0.01,CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,1])
CrCu_ETC_Norm_VaR_Test5  <- VaRTest(0.05,CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,2])
CrCu_ETC_Norm_VaR_Test10 <- VaRTest(0.1,CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,3])

CrCu_ETC_JSU_VaR_Test1  <- VaRTest(0.01,CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,1])
CrCu_ETC_JSU_VaR_Test5  <- VaRTest(0.05,CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,2])
CrCu_ETC_JSU_VaR_Test10 <- VaRTest(0.1,CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,3])

CrCu_ETC_SGED_VaR_Test1  <- VaRTest(0.01,CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,1])
CrCu_ETC_SGED_VaR_Test5  <- VaRTest(0.05,CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,2])
CrCu_ETC_SGED_VaR_Test10 <- VaRTest(0.1,CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,3])

CrCu_ETC_SST_VaR_Test1  <- VaRTest(0.01,CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,1])
CrCu_ETC_SST_VaR_Test5  <- VaRTest(0.05,CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,2])
CrCu_ETC_SST_VaR_Test10 <- VaRTest(0.1,CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,3])

# ::::::::::: number of exceedances
Backtestres_ETC <- matrix(data = 0, nrow = 12, ncol = 2)

datavar_ETC <- c(CrCu_ETC_SST_VaR_Test1$expected.exceed, CrCu_ETC_SST_VaR_Test1$actual.exceed, CrCu_ETC_SST_VaR_Test5$expected.exceed, CrCu_ETC_SST_VaR_Test5$actual.exceed, CrCu_ETC_SST_VaR_Test10$expected.exceed, CrCu_ETC_SST_VaR_Test10$actual.exceed, 
                 CrCu_ETC_JSU_VaR_Test1$expected.exceed, CrCu_ETC_JSU_VaR_Test1$actual.exceed, CrCu_ETC_JSU_VaR_Test5$expected.exceed, CrCu_ETC_JSU_VaR_Test5$actual.exceed, CrCu_ETC_JSU_VaR_Test10$expected.exceed, CrCu_ETC_JSU_VaR_Test10$actual.exceed,
                 CrCu_ETC_SGED_VaR_Test1$expected.exceed, CrCu_ETC_SGED_VaR_Test1$actual.exceed, CrCu_ETC_SGED_VaR_Test5$expected.exceed, CrCu_ETC_SGED_VaR_Test5$actual.exceed, CrCu_ETC_SGED_VaR_Test10$expected.exceed, CrCu_ETC_SGED_VaR_Test10$actual.exceed,
                 CrCu_ETC_Norm_VaR_Test1$expected.exceed, CrCu_ETC_Norm_VaR_Test1$actual.exceed, CrCu_ETC_Norm_VaR_Test5$expected.exceed, CrCu_ETC_Norm_VaR_Test1$actual.exceed, CrCu_ETC_Norm_VaR_Test10$expected.exceed, CrCu_ETC_Norm_VaR_Test10$actual.exceed)

Backtestres_ETC <- matrix(data = datavar_ETC, nrow = 12, ncol = 2)
rownames(Backtestres_ETC) <- c("SST 1%", "SST 5%", "SST 10%", "JSU 1%", "JSU 5%", "JSU 10%", "SGED 1%", "SGED 5%", "SGED 10%", "Norm 1%", "Norm 5%", "Norm 10%")
colnames(Backtestres_ETC) <- c("Expected Exceedances", "Actual Exceedances")

xtable(Backtestres_ETC, caption = "Exceedances for Value at Risk Backtest for Ethereum Classic - Full Sample", digits = 0)

## Store the pvalues at a place

CrCu_ETC_VaR_Test_pvalues1 <- matrix(c(CrCu_ETC_SST_VaR_Test1$uc.LRp,CrCu_ETC_SST_VaR_Test1$cc.LRp,
                                       CrCu_ETC_SGED_VaR_Test1$uc.LRp,CrCu_ETC_SGED_VaR_Test1$cc.LRp,
                                       CrCu_ETC_JSU_VaR_Test1$uc.LRp,CrCu_ETC_JSU_VaR_Test1$cc.LRp,
                                       CrCu_ETC_Norm_VaR_Test1$uc.LRp,CrCu_ETC_Norm_VaR_Test1$cc.LRp),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_pvalues5 <- matrix(c(CrCu_ETC_SST_VaR_Test5$uc.LRp,CrCu_ETC_SST_VaR_Test5$cc.LRp,
                                       CrCu_ETC_SGED_VaR_Test5$uc.LRp,CrCu_ETC_SGED_VaR_Test5$cc.LRp,
                                       CrCu_ETC_JSU_VaR_Test5$uc.LRp,CrCu_ETC_JSU_VaR_Test5$cc.LRp,
                                       CrCu_ETC_Norm_VaR_Test5$uc.LRp,CrCu_ETC_Norm_VaR_Test5$cc.LRp),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_pvalues10 <- matrix(c(CrCu_ETC_SST_VaR_Test10$uc.LRp,CrCu_ETC_SST_VaR_Test10$cc.LRp,
                                        CrCu_ETC_SGED_VaR_Test10$uc.LRp,CrCu_ETC_SGED_VaR_Test10$cc.LRp,
                                        CrCu_ETC_JSU_VaR_Test10$uc.LRp,CrCu_ETC_JSU_VaR_Test10$cc.LRp,
                                        CrCu_ETC_Norm_VaR_Test10$uc.LRp,CrCu_ETC_Norm_VaR_Test10$cc.LRp),ncol=2,byrow = TRUE)

## Storing the test stats

CrCu_ETC_VaR_Test_teststat1 <- matrix(c(CrCu_ETC_SST_VaR_Test1$uc.LRstat,CrCu_ETC_SST_VaR_Test1$cc.LRstat,
                                        CrCu_ETC_SGED_VaR_Test1$uc.LRstat,CrCu_ETC_SGED_VaR_Test1$cc.LRstat,
                                        CrCu_ETC_JSU_VaR_Test1$uc.LRstat,CrCu_ETC_JSU_VaR_Test1$cc.LRstat,
                                        CrCu_ETC_Norm_VaR_Test1$uc.LRstat,CrCu_ETC_Norm_VaR_Test1$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_teststat5 <- matrix(c(CrCu_ETC_SST_VaR_Test5$uc.LRstat,CrCu_ETC_SST_VaR_Test5$cc.LRstat,
                                        CrCu_ETC_SGED_VaR_Test5$uc.LRstat,CrCu_ETC_SGED_VaR_Test5$cc.LRstat,
                                        CrCu_ETC_JSU_VaR_Test5$uc.LRstat,CrCu_ETC_JSU_VaR_Test5$cc.LRstat,
                                        CrCu_ETC_Norm_VaR_Test5$uc.LRstat,CrCu_ETC_Norm_VaR_Test5$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_teststat10 <- matrix(c(CrCu_ETC_SST_VaR_Test10$uc.LRstat,CrCu_ETC_SST_VaR_Test10$cc.LRstat,
                                         CrCu_ETC_SGED_VaR_Test10$uc.LRstat,CrCu_ETC_SGED_VaR_Test10$cc.LRstat,
                                         CrCu_ETC_JSU_VaR_Test10$uc.LRstat,CrCu_ETC_JSU_VaR_Test10$cc.LRstat,
                                         CrCu_ETC_Norm_VaR_Test10$uc.LRstat,CrCu_ETC_Norm_VaR_Test10$cc.LRstat),ncol=2,byrow = TRUE)
## Round them up to 4 decimals

CrCu_ETC_VaR_Test_pvalues1  <- round(CrCu_ETC_VaR_Test_pvalues1,4)
CrCu_ETC_VaR_Test_pvalues5  <- round(CrCu_ETC_VaR_Test_pvalues5,4)
CrCu_ETC_VaR_Test_pvalues10 <- round(CrCu_ETC_VaR_Test_pvalues10,4)

CrCu_ETC_VaR_Test_teststat1  <- round(CrCu_ETC_VaR_Test_teststat1,4)
CrCu_ETC_VaR_Test_teststat5  <- round(CrCu_ETC_VaR_Test_teststat5,4)
CrCu_ETC_VaR_Test_teststat10 <- round(CrCu_ETC_VaR_Test_teststat10,4)

rownames(CrCu_ETC_VaR_Test_pvalues1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_pvalues1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_ETC_VaR_Test_pvalues5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_pvalues5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_ETC_VaR_Test_pvalues10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_pvalues10)  <- c("Kupiec","Christoffersen")

rownames(CrCu_ETC_VaR_Test_teststat1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_teststat1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_ETC_VaR_Test_teststat5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_teststat5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_ETC_VaR_Test_teststat10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_teststat10)  <- c("Kupiec","Christoffersen")
# Kupiec H0: observed failure rate is equal to the failure suggested by tests
# Christoffersen H0: Exceedances are not clustering or no consecutive exceedances
# Fail to reject H0 is ideal
# Based on above, H0 is rejected only in case of Normal 1%, so good results

## VaR Duration test

CrCu_ETC_Norm_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,1])
CrCu_ETC_Norm_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,2])
CrCu_ETC_Norm_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,3])

CrCu_ETC_JSU_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,1])
CrCu_ETC_JSU_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,2])
CrCu_ETC_JSU_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,3])

CrCu_ETC_SGED_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,1])
CrCu_ETC_SGED_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,2])
CrCu_ETC_SGED_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,3])

CrCu_ETC_SST_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,1])
CrCu_ETC_SST_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,2])
CrCu_ETC_SST_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,3])

### Store the test results and p=values

CrCu_ETC_VaR_Dur_Test_pvalues1 <- matrix(c(CrCu_ETC_SST_VaR_Dur_Test1$LRp,CrCu_ETC_SGED_VaR_Dur_Test1$LRp,
                                           CrCu_ETC_JSU_VaR_Dur_Test1$LRp,CrCu_ETC_Norm_VaR_Dur_Test1$LRp),nrow=4,byrow = TRUE)
CrCu_ETC_VaR_Dur_Test_pvalues5 <- matrix(c(CrCu_ETC_SST_VaR_Dur_Test5$LRp,CrCu_ETC_SGED_VaR_Dur_Test5$LRp,
                                           CrCu_ETC_JSU_VaR_Dur_Test5$LRp,CrCu_ETC_Norm_VaR_Dur_Test5$LRp),nrow=4,byrow = TRUE)
CrCu_ETC_VaR_Dur_Test_pvalues10 <- matrix(c(CrCu_ETC_SST_VaR_Dur_Test10$LRp,CrCu_ETC_SGED_VaR_Dur_Test10$LRp,
                                            CrCu_ETC_JSU_VaR_Dur_Test10$LRp,CrCu_ETC_Norm_VaR_Dur_Test10$LRp),nrow=4,byrow = TRUE)

CrCu_ETC_VaR_Dur_Test_teststat1 <- matrix(c(CrCu_ETC_SST_VaR_Dur_Test1$uLL,CrCu_ETC_SST_VaR_Dur_Test1$rLL,
                                            CrCu_ETC_SGED_VaR_Dur_Test1$uLL,CrCu_ETC_SGED_VaR_Dur_Test1$rLL,
                                            CrCu_ETC_JSU_VaR_Dur_Test1$uLL,CrCu_ETC_JSU_VaR_Dur_Test1$rLL,
                                            CrCu_ETC_Norm_VaR_Dur_Test1$uLL,CrCu_ETC_Norm_VaR_Dur_Test1$rLL),nrow=4,byrow = TRUE)
CrCu_ETC_VaR_Dur_Test_teststat5 <- matrix(c(CrCu_ETC_SST_VaR_Dur_Test5$uLL,CrCu_ETC_SST_VaR_Dur_Test5$rLL,
                                            CrCu_ETC_SGED_VaR_Dur_Test5$uLL,CrCu_ETC_SGED_VaR_Dur_Test5$rLL,
                                            CrCu_ETC_JSU_VaR_Dur_Test5$uLL,CrCu_ETC_JSU_VaR_Dur_Test5$rLL,
                                            CrCu_ETC_Norm_VaR_Dur_Test5$uLL,CrCu_ETC_Norm_VaR_Dur_Test5$rLL),nrow=4,byrow = TRUE)
CrCu_ETC_VaR_Dur_Test_teststat10 <- matrix(c(CrCu_ETC_SST_VaR_Dur_Test10$uLL,CrCu_ETC_SST_VaR_Dur_Test10$rLL,
                                             CrCu_ETC_SGED_VaR_Dur_Test10$uLL,CrCu_ETC_SGED_VaR_Dur_Test10$rLL,
                                             CrCu_ETC_JSU_VaR_Dur_Test10$uLL,CrCu_ETC_JSU_VaR_Dur_Test10$rLL,
                                             CrCu_ETC_Norm_VaR_Dur_Test10$uLL,CrCu_ETC_Norm_VaR_Dur_Test10$rLL),nrow=4,byrow = TRUE)

## Round them up to 4 decimals

CrCu_ETC_VaR_Dur_Test_pvalues1  <- round(CrCu_ETC_VaR_Dur_Test_pvalues1,4)
CrCu_ETC_VaR_Dur_Test_pvalues5  <- round(CrCu_ETC_VaR_Dur_Test_pvalues5,4)
CrCu_ETC_VaR_Dur_Test_pvalues10 <- round(CrCu_ETC_VaR_Dur_Test_pvalues10,4)

CrCu_ETC_VaR_Dur_Test_teststat1  <- round(CrCu_ETC_VaR_Dur_Test_teststat1,4)
CrCu_ETC_VaR_Dur_Test_teststat5  <- round(CrCu_ETC_VaR_Dur_Test_teststat5,4)
CrCu_ETC_VaR_Dur_Test_teststat10 <- round(CrCu_ETC_VaR_Dur_Test_teststat10,4)

rownames(CrCu_ETC_VaR_Dur_Test_pvalues1)  <- c("SST","SGED","JSU","Norm")
rownames(CrCu_ETC_VaR_Dur_Test_pvalues5)  <- c("SST","SGED","JSU","Norm")
rownames(CrCu_ETC_VaR_Dur_Test_pvalues10)  <- c("SST","SGED","JSU","Norm")

rownames(CrCu_ETC_VaR_Dur_Test_teststat1)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_ETC_VaR_Dur_Test_teststat1)  <- c("Unrestricted","Restricted")
rownames(CrCu_ETC_VaR_Dur_Test_teststat5)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_ETC_VaR_Dur_Test_teststat5)  <- c("Unrestricted","Restricted")
rownames(CrCu_ETC_VaR_Dur_Test_teststat10)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_ETC_VaR_Dur_Test_teststat10)  <- c("Unrestricted","Restricted")
# H0: Correctly specified risk model
# H0: Duration Between Exceedances have no memory
# Duration of time between VaR violations should ideally be independent and not clustered
# Fail to reject H0 in all case, which is good

# ___________________________________________________________________________________________________ #

#### ::::::::::::::::: S U B S A M P L E Analysis ::::::::::::::::::::: ####

### ::::::::::::Descriptive Statistics and Basic Tests of **SUBSAMPLES** ::::::::::::::::: ###

Descr_CrCu_ETC   <- cbind(basicStats(CrCu_ETC_sub1),basicStats(CrCu_ETC_sub2),basicStats(CrCu_ETC_sub3))
# To check evidence of unit root
ADF_CrCu_ETC   <- cbind(adf.test(CrCu_ETC_sub1),adf.test(CrCu_ETC_sub2),adf.test(CrCu_ETC_sub3))
# To check evidence of unit root
pp_CrCu_ETC   <- cbind(pp.test(CrCu_ETC_sub1),pp.test(CrCu_ETC_sub2),pp.test(CrCu_ETC_sub3))
# To check assumption of normality
jb_CrCu_ETC   <- cbind(jarque.bera.test(CrCu_ETC_sub1),jarque.bera.test(CrCu_ETC_sub2),jarque.bera.test(CrCu_ETC_sub3))
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation
LQ_CrCu_ETC01   <- cbind(Box.test(CrCu_ETC_sub1^2,lag=1,type="Lj"),Box.test(CrCu_ETC_sub2^2,lag=1,type="Lj"),Box.test(CrCu_ETC_sub3^2,lag=1,type="Lj"))
LQ_CrCu_ETC05   <- cbind(Box.test(CrCu_ETC_sub1^2,lag=5,type="Lj"),Box.test(CrCu_ETC_sub2^2,lag=5,type="Lj"),Box.test(CrCu_ETC_sub3^2,lag=5,type="Lj"))
LQ_CrCu_ETC10   <- cbind(Box.test(CrCu_ETC_sub1^2,lag=10,type="Lj"),Box.test(CrCu_ETC_sub2^2,lag=10,type="Lj"),Box.test(CrCu_ETC_sub3^2,lag=10,type="Lj"))

## :::::::: Subsamples
ArchTest(CrCu_ETC_sub1, lags = 3)
ArchTest(CrCu_ETC_sub2, lags = 3)
ArchTest(CrCu_ETC_sub3, lags = 3)
# H0: No ARCH effect
# DEFINITELY ARCH EFFECTS EXIST
arch.test(CrCu_ETC_sub1, lags.single = 3)

## plot squared returns
par(mfrow = c(1,1))
pacf(CrCu_ETC_sub1^2)
pacf(CrCu_ETC_sub2^2)
pacf(CrCu_ETC_sub3^2)

acf(CrCu_ETC_sub1^2)
acf(CrCu_ETC_sub2^2)
acf(CrCu_ETC_sub3^2)

acf(CrCu_ETC_sub1)
acf(CrCu_ETC_sub2)
acf(CrCu_ETC_sub3)

#### JSU specification

CrCu_ETC_Spec_Sample1_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETC_Spec_Sample2_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETC_Spec_Sample3_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")

CrCu_ETC_Model_Sample_JSU1 <- ugarchfit(CrCu_ETC_Spec_Sample1_JSU,CrCu_ETC_sub1,out.sample = 60)
CrCu_ETC_Model_Sample_JSU2 <- ugarchfit(CrCu_ETC_Spec_Sample2_JSU,CrCu_ETC_sub2,out.sample = 200)
CrCu_ETC_Model_Sample_JSU3 <- ugarchfit(CrCu_ETC_Spec_Sample3_JSU,CrCu_ETC_sub3,out.sample = 100)

## Rollover Analysis

CrCu_ETC_Roll_JSU_Sample1 <-  ugarchroll(CrCu_ETC_Spec_Sample1_JSU,CrCu_ETC_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll_JSU_Sample2 <-  ugarchroll(CrCu_ETC_Spec_Sample2_JSU,CrCu_ETC_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll_JSU_Sample3 <-  ugarchroll(CrCu_ETC_Spec_Sample3_JSU,CrCu_ETC_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# ### SGED specification

CrCu_ETC_Spec_Sample1_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETC_Spec_Sample2_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETC_Spec_Sample3_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")

CrCu_ETC_Model_Sample_SGED1 <- ugarchfit(CrCu_ETC_Spec_Sample1_SGED,CrCu_ETC_sub1,out.sample = 60)
CrCu_ETC_Model_Sample_SGED2 <- ugarchfit(CrCu_ETC_Spec_Sample2_SGED,CrCu_ETC_sub2,out.sample = 200)
CrCu_ETC_Model_Sample_SGED3 <- ugarchfit(CrCu_ETC_Spec_Sample3_SGED,CrCu_ETC_sub3,out.sample = 100)

## Rollover Analysis

CrCu_ETC_Roll_SGED_Sample1 <-  ugarchroll(CrCu_ETC_Spec_Sample1_SGED,CrCu_ETC_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll_SGED_Sample2 <-  ugarchroll(CrCu_ETC_Spec_Sample2_SGED,CrCu_ETC_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll_SGED_Sample3 <-  ugarchroll(CrCu_ETC_Spec_Sample3_SGED,CrCu_ETC_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

### SST Specification

CrCu_ETC_Spec_Sample1_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETC_Spec_Sample2_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETC_Spec_Sample3_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")

CrCu_ETC_Model_Sample_SST1 <- ugarchfit(CrCu_ETC_Spec_Sample1_SST,CrCu_ETC_sub1,out.sample = 60)
CrCu_ETC_Model_Sample_SST2 <- ugarchfit(CrCu_ETC_Spec_Sample2_SST,CrCu_ETC_sub2,out.sample = 200)
CrCu_ETC_Model_Sample_SST3 <- ugarchfit(CrCu_ETC_Spec_Sample3_SST,CrCu_ETC_sub3,out.sample = 100)

## Rollover Analysis

CrCu_ETC_Roll_SST_Sample1 <-  ugarchroll(CrCu_ETC_Spec_Sample1_SST,CrCu_ETC_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll_SST_Sample2 <-  ugarchroll(CrCu_ETC_Spec_Sample2_SST,CrCu_ETC_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll_SST_Sample3 <-  ugarchroll(CrCu_ETC_Spec_Sample3_SST,CrCu_ETC_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# ### Normal Specification

CrCu_ETC_Spec_Sample1_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")
CrCu_ETC_Spec_Sample2_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")
CrCu_ETC_Spec_Sample3_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETC_Model_Sample_Normal1 <- ugarchfit(CrCu_ETC_Spec_Sample1_Normal,CrCu_ETC_sub1,out.sample = 60)
CrCu_ETC_Model_Sample_Normal2 <- ugarchfit(CrCu_ETC_Spec_Sample2_Normal,CrCu_ETC_sub2,out.sample = 200)
CrCu_ETC_Model_Sample_Normal3 <- ugarchfit(CrCu_ETC_Spec_Sample3_Normal,CrCu_ETC_sub3,out.sample = 100)

## Rollover Analysis

CrCu_ETC_Roll_Normal_Sample1 <-  ugarchroll(CrCu_ETC_Spec_Sample1_Normal,CrCu_ETC_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll_Normal_Sample2 <-  ugarchroll(CrCu_ETC_Spec_Sample2_Normal,CrCu_ETC_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll_Normal_Sample3 <-  ugarchroll(CrCu_ETC_Spec_Sample3_Normal,CrCu_ETC_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

## persistence

Persis_sub_ETC <- c(persistence(CrCu_ETC_Model_Sample_JSU1), persistence(CrCu_ETC_Model_Sample_JSU2), persistence(CrCu_ETC_Model_Sample_JSU3),
                    persistence(CrCu_ETC_Model_Sample_SGED1), persistence(CrCu_ETC_Model_Sample_SGED2), persistence(CrCu_ETC_Model_Sample_SGED3),
                    persistence(CrCu_ETC_Model_Sample_SST1), persistence(CrCu_ETC_Model_Sample_SST2), persistence(CrCu_ETC_Model_Sample_SST3),
                    persistence(CrCu_ETC_Model_Sample_Normal1), persistence(CrCu_ETC_Model_Sample_Normal2), persistence(CrCu_ETC_Model_Sample_Normal3))

persis_ETC_sub <- matrix(data=Persis_sub_ETC, nrow = 3, ncol = 4)
colnames(persis_ETC_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(persis_ETC_sub) <- c("SubSample1", "SubSample2", "SubSample3")
xtable(persis_ETC_sub, caption = "Persistence of Ethereum Classic SubSamples", digits = 4)

### Store the loglikelihood values, AIC and BIC ####

Likelihood_sub_ETC <- c(CrCu_ETC_Model_Sample_JSU1@fit$LLH, CrCu_ETC_Model_Sample_JSU2@fit$LLH, CrCu_ETC_Model_Sample_JSU3@fit$LLH,
                        CrCu_ETC_Model_Sample_SGED1@fit$LLH, CrCu_ETC_Model_Sample_SGED2@fit$LLH, CrCu_ETC_Model_Sample_SGED3@fit$LLH,
                        CrCu_ETC_Model_Sample_SST1@fit$LLH, CrCu_ETC_Model_Sample_SST2@fit$LLH, CrCu_ETC_Model_Sample_SST3@fit$LLH,
                        CrCu_ETC_Model_Sample_Normal1@fit$LLH, CrCu_ETC_Model_Sample_Normal2@fit$LLH, CrCu_ETC_Model_Sample_Normal3@fit$LLH)

Llhood_sub_ETC <- matrix(data = Likelihood_sub_ETC, nrow = 3, ncol = 4)
colnames(Llhood_sub_ETC) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(Llhood_sub_ETC) <- c("SubSample1", "SubSample2", "SubSample3")

bic_sub_ETC <- c(infocriteria(CrCu_ETC_Model_Sample_JSU1)[2], infocriteria(CrCu_ETC_Model_Sample_JSU2)[2], infocriteria(CrCu_ETC_Model_Sample_JSU3)[2],
                 infocriteria(CrCu_ETC_Model_Sample_SGED1)[2], infocriteria(CrCu_ETC_Model_Sample_SGED2)[2], infocriteria(CrCu_ETC_Model_Sample_SGED3)[2],
                 infocriteria(CrCu_ETC_Model_Sample_SST1)[2], infocriteria(CrCu_ETC_Model_Sample_SST2)[2], infocriteria(CrCu_ETC_Model_Sample_SST3)[2],
                 infocriteria(CrCu_ETC_Model_Sample_Normal1)[2], infocriteria(CrCu_ETC_Model_Sample_Normal2)[2], infocriteria(CrCu_ETC_Model_Sample_Normal3)[2])

bic_ETC_sub <- matrix(data=bic_sub_ETC, nrow = 3, ncol = 4)
colnames(bic_ETC_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(bic_ETC_sub) <- c("SubSample1", "SubSample2", "SubSample3")

aic_sub_ETC <- c(infocriteria(CrCu_ETC_Model_Sample_JSU1)[1], infocriteria(CrCu_ETC_Model_Sample_JSU2)[1], infocriteria(CrCu_ETC_Model_Sample_JSU3)[1],
                 infocriteria(CrCu_ETC_Model_Sample_SGED1)[1], infocriteria(CrCu_ETC_Model_Sample_SGED2)[1], infocriteria(CrCu_ETC_Model_Sample_SGED3)[1],
                 infocriteria(CrCu_ETC_Model_Sample_SST1)[1], infocriteria(CrCu_ETC_Model_Sample_SST2)[1], infocriteria(CrCu_ETC_Model_Sample_SST3)[1],
                 infocriteria(CrCu_ETC_Model_Sample_Normal1)[1], infocriteria(CrCu_ETC_Model_Sample_Normal2)[1], infocriteria(CrCu_ETC_Model_Sample_Normal3)[1])

aic_ETC_sub <- matrix(data=aic_sub_ETC, nrow = 3, ncol = 4)
colnames(aic_ETC_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(aic_ETC_sub) <- c("SubSample1", "SubSample2", "SubSample3")

Infocrit_sub_ETC  <- cbind(Llhood_sub_ETC, bic_ETC_sub, aic_ETC_sub)
colnames(Infocrit_sub_ETC) <- c("Llhood:eGarch-JSU","Llhood:eGarch-SGED","Llhood:eGarch-SST","Llhood:eGarch-Normal", 
                                "BIC:eGarch-JSU","BIC:eGarch-SGED","BIC:eGarch-SST","BIC:eGarch-Normal", 
                                "AIC:eGarch-JSU","AIC:eGarch-SGED","AIC:eGarch-SST","AIC:eGarch-Normal")
Infocrit_sub1_ETC <- t(Infocrit_sub_ETC)
View(data.frame(Infocrit_sub1_ETC))
xtable(Infocrit_sub1_ETC, caption = "Information Criteria for Ethereum Classic GARCH models - SubSamples", digits = 4)

## Conclusion: SS1 - eGarch SGED, SS2 - eGarch SST, SS3 - eGarch SST##

## coefficients

# Sample 1
coef_ETC_sub1_SGED <- round(coef(CrCu_ETC_Model_Sample_SGED1),4) # SS1 SELECTED
coef_ETC_sub1_JSU <- round(coef(CrCu_ETC_Model_Sample_JSU1),4) # SS1
coef_ETC_sub1_SST <- round(coef(CrCu_ETC_Model_Sample_SST1),4) # SS1
coef_ETC_sub1_Normal <- round(coef(CrCu_ETC_Model_Sample_Normal1),4) # SS1

# Sample 2
coef_ETC_sub2_SGED <- round(coef(CrCu_ETC_Model_Sample_SGED2),4) # SS2
coef_ETC_sub2_JSU <- round(coef(CrCu_ETC_Model_Sample_JSU2),4) # SS2 
coef_ETC_sub2_SST <- round(coef(CrCu_ETC_Model_Sample_SST2),4) # SS2 SELECTED
coef_ETC_sub2_Normal <- round(coef(CrCu_ETC_Model_Sample_Normal2),4) # SS2

# Sample 3
coef_ETC_sub3_SGED <- round(coef(CrCu_ETC_Model_Sample_SGED3),4) # SS3
coef_ETC_sub3_JSU <- round(coef(CrCu_ETC_Model_Sample_JSU3),4) # SS3
coef_ETC_sub3_SST <- round(coef(CrCu_ETC_Model_Sample_SST3),4) # SS3 SELECTED
coef_ETC_sub3_Normal <- round(coef(CrCu_ETC_Model_Sample_Normal3),4) # SS3

garch_coef_ETC_fsub <- bind_rows(coef_ETC_7, coef_ETC_sub1_SGED, coef_ETC_sub2_SST, coef_ETC_sub3_SST)

garch_coef_ETC1_fsub <- as.data.frame(garch_coef_ETC_fsub)
rownames(garch_coef_ETC1_fsub) <- c("FullSample-eGarchJSU","SS1-eGarchSGED","SS2-eGarchSST","SS3-eGarchSST")
garch_coef_ETC1_fsubt <- (t(garch_coef_ETC1_fsub))
View(t(garch_coef_ETC1_fsub))
xtable(garch_coef_ETC1_fsubt, caption = "Coefficients of Ethereum Classic GARCH models", digits = 4)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model

# Sample 1

garchsig_SGED1_ETC <- CrCu_ETC_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_ETC <- abs(CrCu_ETC_Roll_SGED_Sample1@forecast$density$Realized)
MSEval_SGED1_ETC <- MSE(garchsig_SGED1_ETC, realval_SGED1_ETC)
MAPEval_SGED1_ETC <- MAPE(garchsig_SGED1_ETC, realval_SGED1_ETC)

garchsig_JSU1_ETC <- CrCu_ETC_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_ETC <- abs(CrCu_ETC_Roll_JSU_Sample1@forecast$density$Realized)
MSEval_JSU1_ETC <- MSE(garchsig_JSU1_ETC, realval_JSU1_ETC)
MAPEval_JSU1_ETC <- MAPE(garchsig_JSU1_ETC, realval_JSU1_ETC)

garchsig_SST1_ETC <- CrCu_ETC_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_ETC <- abs(CrCu_ETC_Roll_SST_Sample1@forecast$density$Realized)
MSEval_SST1_ETC <- MSE(garchsig_SST1_ETC, realval_SST1_ETC)
MAPEval_SST1_ETC <- MAPE(garchsig_SST1_ETC, realval_SST1_ETC)

garchsig_Normal1_ETC <- CrCu_ETC_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_ETC <- abs(CrCu_ETC_Roll_Normal_Sample1@forecast$density$Realized)
MSEval_Normal1_ETC <- MSE(garchsig_Normal1_ETC, realval_Normal1_ETC)
MAPEval_Normal1_ETC <- MAPE(garchsig_Normal1_ETC, realval_Normal1_ETC)

# Sample 2

garchsig_SGED2_ETC <- CrCu_ETC_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_ETC <- abs(CrCu_ETC_Roll_SGED_Sample2@forecast$density$Realized)
MSEval_SGED2_ETC <- MSE(garchsig_SGED2_ETC, realval_SGED2_ETC)
MAPEval_SGED2_ETC <- MAPE(garchsig_SGED2_ETC, realval_SGED2_ETC)

garchsig_JSU2_ETC <- CrCu_ETC_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_ETC <- abs(CrCu_ETC_Roll_JSU_Sample2@forecast$density$Realized)
MSEval_JSU2_ETC <- MSE(garchsig_JSU2_ETC, realval_JSU2_ETC)
MAPEval_JSU2_ETC <- MAPE(garchsig_JSU2_ETC, realval_JSU2_ETC)

garchsig_SST2_ETC <- CrCu_ETC_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_ETC <- abs(CrCu_ETC_Roll_SST_Sample2@forecast$density$Realized)
MSEval_SST2_ETC <- MSE(garchsig_SST2_ETC, realval_SST2_ETC)
MAPEval_SST2_ETC <- MAPE(garchsig_SST2_ETC, realval_SST2_ETC)

garchsig_Normal2_ETC <- CrCu_ETC_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_ETC <- abs(CrCu_ETC_Roll_Normal_Sample2@forecast$density$Realized)
MSEval_Normal2_ETC <- MSE(garchsig_Normal2_ETC, realval_Normal2_ETC)
MAPEval_Normal2_ETC <- MAPE(garchsig_Normal2_ETC, realval_Normal2_ETC)

# Sample 3

garchsig_SGED3_ETC <- CrCu_ETC_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_ETC <- abs(CrCu_ETC_Roll_SGED_Sample3@forecast$density$Realized)
MSEval_SGED3_ETC <- MSE(garchsig_SGED3_ETC, realval_SGED3_ETC)
MAPEval_SGED3_ETC <- MAPE(garchsig_SGED3_ETC, realval_SGED3_ETC)

garchsig_JSU3_ETC <- CrCu_ETC_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_ETC <- abs(CrCu_ETC_Roll_JSU_Sample3@forecast$density$Realized)
MSEval_JSU3_ETC <- MSE(garchsig_JSU3_ETC, realval_JSU3_ETC)
MAPEval_JSU3_ETC <- MAPE(garchsig_JSU3_ETC, realval_JSU3_ETC)

garchsig_SST3_ETC <- CrCu_ETC_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_ETC <- abs(CrCu_ETC_Roll_SST_Sample3@forecast$density$Realized)
MSEval_SST3_ETC <- MSE(garchsig_SST3_ETC, realval_SST3_ETC)
MAPEval_SST3_ETC <- MAPE(garchsig_SST3_ETC, realval_SST3_ETC)

garchsig_Normal3_ETC <- CrCu_ETC_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_ETC <- abs(CrCu_ETC_Roll_Normal_Sample3@forecast$density$Realized)
MSEval_Normal3_ETC <- MSE(garchsig_Normal3_ETC, realval_Normal3_ETC)
MAPEval_Normal3_ETC <- MAPE(garchsig_Normal3_ETC, realval_Normal3_ETC)


MAPEgval_ETC_fsub  <- matrix(data=0,nrow=12,ncol=2)
colnames(MAPEgval_ETC_fsub) <- c("MSE", "MAPE")
rownames(MAPEgval_ETC_fsub) <- c("SS1-eGarchSGED","SS1-eGarchJSU","SS1-eGarchSST","SS1-eGarchNormal",
                                 "SS2-eGarchSGED","SS2-eGarchJSU","SS2-eGarchSST","SS2-eGarchNormal",
                                 "SS3-eGarchSGED","SS3-eGarchJSU","SS3-eGarchSST","SS3-eGarchNormal")

MAPEgval_ETC_fsub[,1] = cbind(MSEval_SGED1_ETC,MSEval_JSU1_ETC,MSEval_SST1_ETC,MSEval_Normal1_ETC,
                              MSEval_SGED2_ETC,MSEval_JSU2_ETC,MSEval_SST2_ETC,MSEval_Normal2_ETC,
                              MSEval_SGED3_ETC,MSEval_JSU3_ETC,MSEval_SST3_ETC,MSEval_Normal3_ETC)
MAPEgval_ETC_fsub[,2] = cbind(MAPEval_SGED1_ETC,MAPEval_JSU1_ETC,MAPEval_SST1_ETC,MAPEval_Normal1_ETC,
                              MAPEval_SGED2_ETC,MAPEval_JSU2_ETC,MAPEval_SST2_ETC,MAPEval_Normal2_ETC,
                              MAPEval_SGED3_ETC,MAPEval_JSU3_ETC,MAPEval_SST3_ETC,MAPEval_Normal3_ETC)

xtable(MAPEgval_ETC_fsub, caption = "MSE & MAPE for out of sample forecast for subsamples of Ethereum Classic GARCH", digits = 4)

#### Store the VaR for the out sample using rolling window forecasts

# Sample 1 
CrCu_ETC_VaR_SGED1   <- CrCu_ETC_Roll_SGED_Sample1@forecast$VaR # SS1 SELECTED
CrCu_ETC_VaR_JSU1   <- CrCu_ETC_Roll_JSU_Sample1@forecast$VaR # SS1
CrCu_ETC_VaR_SST1   <- CrCu_ETC_Roll_SST_Sample1@forecast$VaR # SS1
CrCu_ETC_VaR_Normal1   <- CrCu_ETC_Roll_Normal_Sample1@forecast$VaR # SS1

# Sample 2
CrCu_ETC_VaR_SGED2   <- CrCu_ETC_Roll_SGED_Sample2@forecast$VaR # SS2 
CrCu_ETC_VaR_JSU2   <- CrCu_ETC_Roll_JSU_Sample2@forecast$VaR # SS2 SELECTED
CrCu_ETC_VaR_SST2   <- CrCu_ETC_Roll_SST_Sample2@forecast$VaR # SS2
CrCu_ETC_VaR_Normal2   <- CrCu_ETC_Roll_Normal_Sample2@forecast$VaR # SS2

# Sample 3
CrCu_ETC_VaR_SGED3   <- CrCu_ETC_Roll_SGED_Sample3@forecast$VaR # SS3 
CrCu_ETC_VaR_JSU3   <- CrCu_ETC_Roll_JSU_Sample3@forecast$VaR # SS3 
CrCu_ETC_VaR_SST3   <- CrCu_ETC_Roll_SST_Sample3@forecast$VaR # SS3 SELECTED
CrCu_ETC_VaR_Normal3   <- CrCu_ETC_Roll_Normal_Sample3@forecast$VaR # SS3

### Ljung-box test statistic

# Sample 1

CrCu_ETC1_JSU1  <- Box.test(CrCu_ETC_Model_Sample_JSU1@fit$residuals/CrCu_ETC_Model_Sample_JSU1@fit$sigma,lag=1,type="Lj")
CrCu_ETC1_JSU5  <- Box.test(CrCu_ETC_Model_Sample_JSU1@fit$residuals/CrCu_ETC_Model_Sample_JSU1@fit$sigma,lag=5,type="Lj")
CrCu_ETC1_JSU10  <- Box.test(CrCu_ETC_Model_Sample_JSU1@fit$residuals/CrCu_ETC_Model_Sample_JSU1@fit$sigma,lag=10,type="Lj")

CrCu_ETC1_SST1  <- Box.test(CrCu_ETC_Model_Sample_SST1@fit$residuals/CrCu_ETC_Model_Sample_SST1@fit$sigma,lag=1,type="Lj")
CrCu_ETC1_SST5  <- Box.test(CrCu_ETC_Model_Sample_SST1@fit$residuals/CrCu_ETC_Model_Sample_SST1@fit$sigma,lag=5,type="Lj")
CrCu_ETC1_SST10  <- Box.test(CrCu_ETC_Model_Sample_SST1@fit$residuals/CrCu_ETC_Model_Sample_SST1@fit$sigma,lag=10,type="Lj")

CrCu_ETC1_SGED1  <- Box.test(CrCu_ETC_Model_Sample_SGED1@fit$residuals/CrCu_ETC_Model_Sample_SGED1@fit$sigma,lag=1,type="Lj")
CrCu_ETC1_SGED5  <- Box.test(CrCu_ETC_Model_Sample_SGED1@fit$residuals/CrCu_ETC_Model_Sample_SGED1@fit$sigma,lag=5,type="Lj")
CrCu_ETC1_SGED10  <- Box.test(CrCu_ETC_Model_Sample_SGED1@fit$residuals/CrCu_ETC_Model_Sample_SGED1@fit$sigma,lag=10,type="Lj")

CrCu_ETC1_Normal1  <- Box.test(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma,lag=1,type="Lj")
CrCu_ETC1_Normal5  <- Box.test(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma,lag=5,type="Lj")
CrCu_ETC1_Normal10  <- Box.test(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma,lag=10,type="Lj")

CrCu_ETC1_LQ_JSU <- c(CrCu_ETC1_JSU1$statistic,CrCu_ETC1_JSU5$statistic,CrCu_ETC1_JSU10$statistic)
CrCu_ETC1_LQ_SST <- c(CrCu_ETC1_SST1$statistic,CrCu_ETC1_SST5$statistic,CrCu_ETC1_SST10$statistic)
CrCu_ETC1_LQ_SGED <- c(CrCu_ETC1_SGED1$statistic,CrCu_ETC1_SGED5$statistic,CrCu_ETC1_SGED10$statistic)
CrCu_ETC1_LQ_Normal <- c(CrCu_ETC1_Normal1$statistic,CrCu_ETC1_Normal5$statistic,CrCu_ETC1_Normal10$statistic)

CrCu_ETC1_res_stat <-   cbind(CrCu_ETC1_LQ_JSU,CrCu_ETC1_LQ_SST,CrCu_ETC1_LQ_SGED,CrCu_ETC1_LQ_Normal)
rownames(CrCu_ETC1_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_ETC1_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Sample 2

CrCu_ETC2_JSU1  <- Box.test(CrCu_ETC_Model_Sample_JSU2@fit$residuals/CrCu_ETC_Model_Sample_JSU2@fit$sigma,lag=1,type="Lj")
CrCu_ETC2_JSU5  <- Box.test(CrCu_ETC_Model_Sample_JSU2@fit$residuals/CrCu_ETC_Model_Sample_JSU2@fit$sigma,lag=5,type="Lj")
CrCu_ETC2_JSU10  <- Box.test(CrCu_ETC_Model_Sample_JSU2@fit$residuals/CrCu_ETC_Model_Sample_JSU2@fit$sigma,lag=10,type="Lj")

CrCu_ETC2_SST1  <- Box.test(CrCu_ETC_Model_Sample_SST2@fit$residuals/CrCu_ETC_Model_Sample_SST2@fit$sigma,lag=1,type="Lj")
CrCu_ETC2_SST5  <- Box.test(CrCu_ETC_Model_Sample_SST2@fit$residuals/CrCu_ETC_Model_Sample_SST2@fit$sigma,lag=5,type="Lj")
CrCu_ETC2_SST10  <- Box.test(CrCu_ETC_Model_Sample_SST2@fit$residuals/CrCu_ETC_Model_Sample_SST2@fit$sigma,lag=10,type="Lj")

CrCu_ETC2_SGED1  <- Box.test(CrCu_ETC_Model_Sample_SGED2@fit$residuals/CrCu_ETC_Model_Sample_SGED2@fit$sigma,lag=1,type="Lj")
CrCu_ETC2_SGED5  <- Box.test(CrCu_ETC_Model_Sample_SGED2@fit$residuals/CrCu_ETC_Model_Sample_SGED2@fit$sigma,lag=5,type="Lj")
CrCu_ETC2_SGED10  <- Box.test(CrCu_ETC_Model_Sample_SGED2@fit$residuals/CrCu_ETC_Model_Sample_SGED2@fit$sigma,lag=10,type="Lj")

CrCu_ETC2_Normal1  <- Box.test(CrCu_ETC_Model_Sample_Normal2@fit$residuals/CrCu_ETC_Model_Sample_Normal2@fit$sigma,lag=1,type="Lj")
CrCu_ETC2_Normal5  <- Box.test(CrCu_ETC_Model_Sample_Normal2@fit$residuals/CrCu_ETC_Model_Sample_Normal2@fit$sigma,lag=5,type="Lj")
CrCu_ETC2_Normal10  <- Box.test(CrCu_ETC_Model_Sample_Normal2@fit$residuals/CrCu_ETC_Model_Sample_Normal2@fit$sigma,lag=10,type="Lj")

CrCu_ETC2_LQ_JSU <- c(CrCu_ETC2_JSU1$statistic,CrCu_ETC2_JSU5$statistic,CrCu_ETC2_JSU10$statistic)
CrCu_ETC2_LQ_SST <- c(CrCu_ETC2_SST1$statistic,CrCu_ETC2_SST5$statistic,CrCu_ETC2_SST10$statistic)
CrCu_ETC2_LQ_SGED <- c(CrCu_ETC2_SGED1$statistic,CrCu_ETC2_SGED5$statistic,CrCu_ETC2_SGED10$statistic)
CrCu_ETC2_LQ_Normal <- c(CrCu_ETC2_Normal1$statistic,CrCu_ETC2_Normal5$statistic,CrCu_ETC2_Normal10$statistic)

CrCu_ETC2_res_stat <-   cbind(CrCu_ETC2_LQ_JSU,CrCu_ETC2_LQ_SST,CrCu_ETC2_LQ_SGED,CrCu_ETC2_LQ_Normal)
rownames(CrCu_ETC2_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_ETC2_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Sample 3

CrCu_ETC3_JSU1  <- Box.test(CrCu_ETC_Model_Sample_JSU3@fit$residuals/CrCu_ETC_Model_Sample_JSU3@fit$sigma,lag=1,type="Lj")
CrCu_ETC3_JSU5  <- Box.test(CrCu_ETC_Model_Sample_JSU3@fit$residuals/CrCu_ETC_Model_Sample_JSU3@fit$sigma,lag=5,type="Lj")
CrCu_ETC3_JSU10  <- Box.test(CrCu_ETC_Model_Sample_JSU3@fit$residuals/CrCu_ETC_Model_Sample_JSU3@fit$sigma,lag=10,type="Lj")

CrCu_ETC3_SST1  <- Box.test(CrCu_ETC_Model_Sample_SST3@fit$residuals/CrCu_ETC_Model_Sample_SST3@fit$sigma,lag=1,type="Lj")
CrCu_ETC3_SST5  <- Box.test(CrCu_ETC_Model_Sample_SST3@fit$residuals/CrCu_ETC_Model_Sample_SST3@fit$sigma,lag=5,type="Lj")
CrCu_ETC3_SST10  <- Box.test(CrCu_ETC_Model_Sample_SST3@fit$residuals/CrCu_ETC_Model_Sample_SST3@fit$sigma,lag=10,type="Lj")

CrCu_ETC3_SGED1  <- Box.test(CrCu_ETC_Model_Sample_SGED3@fit$residuals/CrCu_ETC_Model_Sample_SGED3@fit$sigma,lag=1,type="Lj")
CrCu_ETC3_SGED5  <- Box.test(CrCu_ETC_Model_Sample_SGED3@fit$residuals/CrCu_ETC_Model_Sample_SGED3@fit$sigma,lag=5,type="Lj")
CrCu_ETC3_SGED10  <- Box.test(CrCu_ETC_Model_Sample_SGED3@fit$residuals/CrCu_ETC_Model_Sample_SGED3@fit$sigma,lag=10,type="Lj")

CrCu_ETC3_Normal1  <- Box.test(CrCu_ETC_Model_Sample_Normal3@fit$residuals/CrCu_ETC_Model_Sample_Normal3@fit$sigma,lag=1,type="Lj")
CrCu_ETC3_Normal5  <- Box.test(CrCu_ETC_Model_Sample_Normal3@fit$residuals/CrCu_ETC_Model_Sample_Normal3@fit$sigma,lag=5,type="Lj")
CrCu_ETC3_Normal10  <- Box.test(CrCu_ETC_Model_Sample_Normal3@fit$residuals/CrCu_ETC_Model_Sample_Normal3@fit$sigma,lag=10,type="Lj")

CrCu_ETC3_LQ_JSU <- c(CrCu_ETC3_JSU1$statistic,CrCu_ETC3_JSU5$statistic,CrCu_ETC3_JSU10$statistic)
CrCu_ETC3_LQ_SST <- c(CrCu_ETC3_SST1$statistic,CrCu_ETC3_SST5$statistic,CrCu_ETC3_SST10$statistic)
CrCu_ETC3_LQ_SGED <- c(CrCu_ETC3_SGED1$statistic,CrCu_ETC3_SGED5$statistic,CrCu_ETC3_SGED10$statistic)
CrCu_ETC3_LQ_Normal <- c(CrCu_ETC3_Normal1$statistic,CrCu_ETC3_Normal5$statistic,CrCu_ETC3_Normal10$statistic)

CrCu_ETC3_res_stat <-   cbind(CrCu_ETC3_LQ_JSU,CrCu_ETC3_LQ_SST,CrCu_ETC3_LQ_SGED,CrCu_ETC3_LQ_Normal)
rownames(CrCu_ETC3_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_ETC3_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

######## ::::::::::::::::: for P E A R S O N  TYPE IV ##########

### Sample Residuals Statistics

ETC_Res_Sample1 <- c(normalTest(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma,mETCod="jb")@test$statistic,normalTest(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma,mETCod="sw")@test$statistic,normalTest(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma,mETCod="ks")@test$statistic)
ETC_Res_Sample2 <- c(normalTest(CrCu_ETC_Model_Sample_Normal2@fit$residuals/CrCu_ETC_Model_Sample_Normal2@fit$sigma,mETCod="jb")@test$statistic,normalTest(CrCu_ETC_Model_Sample_Normal2@fit$residuals/CrCu_ETC_Model_Sample_Normal2@fit$sigma,mETCod="sw")@test$statistic,normalTest(CrCu_ETC_Model_Sample_Normal2@fit$residuals/CrCu_ETC_Model_Sample_Normal2@fit$sigma,mETCod="ks")@test$statistic)
ETC_Res_Sample3 <- c(normalTest(CrCu_ETC_Model_Sample_Normal3@fit$residuals/CrCu_ETC_Model_Sample_Normal3@fit$sigma,mETCod="jb")@test$statistic,normalTest(CrCu_ETC_Model_Sample_Normal3@fit$residuals/CrCu_ETC_Model_Sample_Normal3@fit$sigma,mETCod="sw")@test$statistic,normalTest(CrCu_ETC_Model_Sample_Normal3@fit$residuals/CrCu_ETC_Model_Sample_Normal3@fit$sigma,mETCod="ks")@test$statistic)

#jarqueberaTest(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma)

ETC_Res_Sample_Test   <- rbind(ETC_Res_Sample1,ETC_Res_Sample2,ETC_Res_Sample3)
rownames(ETC_Res_Sample_Test) <- c("Sample1","Sample2","Sample3")
colnames(ETC_Res_Sample_Test) <- c("JB","SW","KS") 

### Pearson Specification
library(gsl)
ETC_Param_Pearson_Sample1 <- pearsonFitML(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma)
ETC_Param_Pearson_Sample2 <- pearsonFitML(CrCu_ETC_Model_Sample_Normal2@fit$residuals/CrCu_ETC_Model_Sample_Normal2@fit$sigma)
ETC_Param_Pearson_Sample3 <- pearsonFitML(CrCu_ETC_Model_Sample_Normal3@fit$residuals/CrCu_ETC_Model_Sample_Normal3@fit$sigma)

ETC_Param_Pearson_Subsample <- rbind(ETC_Param_Pearson_Sample1,ETC_Param_Pearson_Sample2,ETC_Param_Pearson_Sample3)
rownames(ETC_Param_Pearson_Subsample) <- c("Sample1","Sample2","Sample3")

### johnson SU parameter
ETC_Param_Johnson_Sample1 <- JohnsonFit(CrCu_ETC_Model_Sample_Normal1@fit$residuals/CrCu_ETC_Model_Sample_Normal1@fit$sigma)
ETC_Param_Johnson_Sample2 <- JohnsonFit(CrCu_ETC_Model_Sample_Normal2@fit$residuals/CrCu_ETC_Model_Sample_Normal2@fit$sigma)
ETC_Param_Johnson_Sample3 <- JohnsonFit(CrCu_ETC_Model_Sample_Normal3@fit$residuals/CrCu_ETC_Model_Sample_Normal3@fit$sigma)

ETC_Param_Johnson_Subsample <- rbind(ETC_Param_Johnson_Sample1,ETC_Param_Johnson_Sample2,ETC_Param_Johnson_Sample3)
rownames(ETC_Param_Johnson_Subsample) <- c("Sample1","Sample2","Sample3")

# coefficients for Normal eGarch
coef_ETC_sub1_Normal <- round(coef(CrCu_ETC_Model_Sample_Normal1),4) # SS1
coef_ETC_sub2_Normal <- round(coef(CrCu_ETC_Model_Sample_Normal2),4) # SS2
coef_ETC_sub3_Normal <- round(coef(CrCu_ETC_Model_Sample_Normal3),4) # SS3
# Value at Risk
CrCu_ETC_VaR_Normal1   <- CrCu_ETC_Roll_Normal_Sample1@forecast$VaR
CrCu_ETC_VaR_Normal2   <- CrCu_ETC_Roll_Normal_Sample2@forecast$VaR
CrCu_ETC_VaR_Normal3   <- CrCu_ETC_Roll_Normal_Sample3@forecast$VaR

#### Repeating for Pearson Distribution  #####
signific_levels <- c(0.1,0.05,0.01)

## Store the volatility and mu forecast for Sample 1

Holdout_Sample1_ETC_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample1_ETC_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample1_ETC_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample1_ETC_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample1_ETC_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample1_ETC_Pearson_pvalues) <- c("1%","5%","10%")

Sample1_ETC_Holdout_Mu <- as.data.frame(CrCu_ETC_Roll_Normal_Sample1)[,'Mu']
Sample1_ETC_Holdout_Sigma <- as.data.frame(CrCu_ETC_Roll_Normal_Sample1)[,'Sigma']

for(j in 1:3)
{
  
  Z_Pearson_Sample1_ETC   <- qpearson(signific_levels[j],ETC_Param_Pearson_Sample1)
  
  Sample1_ETC_Holdout_VaR   <- Sample1_ETC_Holdout_Mu + Sample1_ETC_Holdout_Sigma*Z_Pearson_Sample1_ETC
  Sample1_ETC_Violations   <- BacktestVaR(Returns_CrCu_ETC[(CrCu_ETC_samples[1]-60+1):(CrCu_ETC_samples[1]-1)],Sample1_ETC_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample1_ETC_Pearson[j,]   <- c(Sample1_ETC_Violations$LRuc[1],Sample1_ETC_Violations$LRcc[1],Sample1_ETC_Violations$DQ$stat,Sample1_ETC_Violations$AE,Sample1_ETC_Violations$Loss$Loss)
  Holdout_Sample1_ETC_Pearson_pvalues[j,] <- c(Sample1_ETC_Violations$LRuc[2],Sample1_ETC_Violations$LRcc[2],Sample1_ETC_Violations$DQ$pvalue)
}

## Store the volatility and mu forecast for Sample 2

Holdout_Sample2_ETC_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample2_ETC_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample2_ETC_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample2_ETC_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample2_ETC_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample2_ETC_Pearson_pvalues) <- c("1%","5%","10%")

Sample2_ETC_Holdout_Mu <- as.data.frame(CrCu_ETC_Roll_Normal_Sample2)[,'Mu']
Sample2_ETC_Holdout_Sigma <- as.data.frame(CrCu_ETC_Roll_Normal_Sample2)[,'Sigma']

for(j in 1:3)
{
  
  Z_Pearson_Sample2_ETC   <- qpearson(signific_levels[j],ETC_Param_Pearson_Sample1)
  
  Sample2_ETC_Holdout_VaR   <- Sample2_ETC_Holdout_Mu + Sample2_ETC_Holdout_Sigma*Z_Pearson_Sample2_ETC
  Sample2_ETC_Violations   <- BacktestVaR(Returns_CrCu_ETC[(CrCu_ETC_samples[2]-200+1):(CrCu_ETC_samples[2]-1)],Sample2_ETC_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample2_ETC_Pearson[j,]   <- c(Sample2_ETC_Violations$LRuc[1],Sample2_ETC_Violations$LRcc[1],Sample2_ETC_Violations$DQ$stat,Sample2_ETC_Violations$AE,Sample2_ETC_Violations$Loss$Loss)
  Holdout_Sample2_ETC_Pearson_pvalues[j,] <- c(Sample2_ETC_Violations$LRuc[2],Sample2_ETC_Violations$LRcc[2],Sample2_ETC_Violations$DQ$pvalue)
}

## Store the volatility and mu forecast for Sample 3

Holdout_Sample3_ETC_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample3_ETC_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample3_ETC_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample3_ETC_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample3_ETC_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample3_ETC_Pearson_pvalues) <- c("1%","5%","10%")

Sample3_ETC_Holdout_Mu <- as.data.frame(CrCu_ETC_Roll_Normal_Sample3)[,'Mu']
Sample3_ETC_Holdout_Sigma <- as.data.frame(CrCu_ETC_Roll_Normal_Sample3)[,'Sigma']

for(j in 1:3)
{
  
  Z_Pearson_Sample3_ETC   <- qpearson(signific_levels[j],ETC_Param_Pearson_Sample3)
  
  Sample3_ETC_Holdout_VaR   <- Sample3_ETC_Holdout_Mu + Sample3_ETC_Holdout_Sigma*Z_Pearson_Sample3_ETC
  Sample3_ETC_Violations   <- BacktestVaR(Returns_CrCu_ETC[(length(Returns_CrCu_ETC)-100+1):(length(Returns_CrCu_ETC)-1)],Sample3_ETC_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample3_ETC_Pearson[j,]   <- c(Sample3_ETC_Violations$LRuc[1],Sample3_ETC_Violations$LRcc[1],Sample3_ETC_Violations$DQ$stat,Sample3_ETC_Violations$AE,Sample3_ETC_Violations$Loss$Loss)
  Holdout_Sample3_ETC_Pearson_pvalues[j,] <- c(Sample3_ETC_Violations$LRuc[2],Sample3_ETC_Violations$LRcc[2],Sample3_ETC_Violations$DQ$pvalue)
}

Holdout_ETC_Pearson_FT <- cbind(Holdout_Sample1_ETC_Pearson[,4], Holdout_Sample2_ETC_Pearson[,4],Holdout_Sample3_ETC_Pearson[,4])
colnames(Holdout_ETC_Pearson_FT) <- c("SubSample1","SubSample2","SubSample3")

## Storing Pearson subsample results ###

ETC_subsample_1pc   <- rbind(Holdout_Sample1_ETC_Pearson[1,],Holdout_Sample2_ETC_Pearson[1,],Holdout_Sample3_ETC_Pearson[1,])

ETC_subsample_5pc   <- rbind(Holdout_Sample1_ETC_Pearson[2,],Holdout_Sample2_ETC_Pearson[2,],Holdout_Sample3_ETC_Pearson[2,])

ETC_subsample_10pc   <- rbind(Holdout_Sample1_ETC_Pearson[3,],Holdout_Sample2_ETC_Pearson[3,],Holdout_Sample3_ETC_Pearson[3,])

ETC_subsample_pvalues_1pc   <- rbind(Holdout_Sample1_ETC_Pearson_pvalues[1,],Holdout_Sample2_ETC_Pearson_pvalues[1,],Holdout_Sample3_ETC_Pearson_pvalues[1,])

ETC_subsample_pvalues_5pc   <- rbind(Holdout_Sample1_ETC_Pearson_pvalues[2,],Holdout_Sample2_ETC_Pearson_pvalues[2,],Holdout_Sample3_ETC_Pearson_pvalues[2,])

ETC_subsample_pvalues_10pc   <- rbind(Holdout_Sample1_ETC_Pearson_pvalues[3,],Holdout_Sample2_ETC_Pearson_pvalues[3,],Holdout_Sample3_ETC_Pearson_pvalues[3,])

## Store the volatility and mu forecast for Sample 1
### Plotting the VaR for subsamples

CrCu_ETC_Sample1_Dates <- format(CrCu_ETC$Crypto_Date[(CrCu_ETC_samples[1]-59):CrCu_ETC_samples[1]],"%b-%y")
CrCu_ETC_Sample2_Dates <- format(CrCu_ETC$Crypto_Date[(CrCu_ETC_samples[2]-199):CrCu_ETC_samples[2]],"%b-%y")
CrCu_ETC_Sample3_Dates <- format(CrCu_ETC$Crypto_Date[(length(CrCu_ETC_xts)-99):length(CrCu_ETC_xts)],"%b-%y")

# Sub Sample 1 # best is SGED based on garch

Z_Pearson_Sample1_ETC   <- qpearson(0.05,ETC_Param_Pearson_Sample1)
Sample1_ETC_Holdout_VaR   <- Sample1_ETC_Holdout_Mu + Sample1_ETC_Holdout_Sigma*Z_Pearson_Sample1_ETC

VR11_ETC <- data.frame(CrCu_ETC_VaR_Normal1)
VR12_ETC <- data.frame(CrCu_ETC_VaR_SST1)
VR13_ETC <- data.frame(CrCu_ETC_VaR_SGED1)
VR14_ETC <- data.frame(CrCu_ETC_VaR_JSU1)
VR15_ETC <- data.frame(Sample1_ETC_Holdout_VaR)
VR_Date1_ETC <- CrCu_ETC$Crypto_Date[(CrCu_ETC_samples[1]-59):CrCu_ETC_samples[1]]

SS1VaR_ETC <- ggplot(data.frame(VR11_ETC[,4]), aes(x= VR_Date1_ETC, y = VR11_ETC[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date1_ETC, y = VR11_ETC[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date1_ETC, y = VR12_ETC[,2], color = "SST")) +
  geom_line(aes(x = VR_Date1_ETC, y = VR13_ETC[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date1_ETC, y = VR14_ETC[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date1_ETC, y = VR15_ETC[,1], color = "PSIV")) +
  xlab ("30 July 2017 to 27 September 2017") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Ethereum Classic SubSample 1") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5, color ="blue")) +
  scale_color_manual(name = "Models", 
                     labels = c("Returns","Norm", "SST", "SGED", "JSU", "PSIV"),
                     values = c("Returns" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PSIV" = "black")) +
  theme(legend.title = element_blank(),
        legend.key = element_rect(),
        legend.position = "bottom",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9)) 

ggexport(SS1VaR_ETC, filename = file.path(FIG_DIR, "SS1VaR_ETC.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

# Sub Sample 2

Z_Pearson_Sample2_ETC   <- qpearson(0.05,ETC_Param_Pearson_Sample2)
Sample2_ETC_Holdout_VaR   <- Sample2_ETC_Holdout_Mu + Sample2_ETC_Holdout_Sigma*Z_Pearson_Sample2_ETC

VR21_ETC <- data.frame(CrCu_ETC_VaR_Normal2)
VR22_ETC <- data.frame(CrCu_ETC_VaR_SST2)
VR23_ETC <- data.frame(CrCu_ETC_VaR_SGED2)
VR24_ETC <- data.frame(CrCu_ETC_VaR_JSU2)
VR25_ETC <- data.frame(Sample2_ETC_Holdout_VaR)
VR_Date2_ETC <- CrCu_ETC$Crypto_Date[(CrCu_ETC_samples[2]-199):CrCu_ETC_samples[2]]

SS2VaR_ETC <- ggplot(data.frame(VR21_ETC[,4]), aes(x= VR_Date2_ETC, y = VR21_ETC[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date2_ETC, y = VR21_ETC[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date2_ETC, y = VR22_ETC[,2], color = "SST")) +
  geom_line(aes(x = VR_Date2_ETC, y = VR23_ETC[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date2_ETC, y = VR24_ETC[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date2_ETC, y = VR25_ETC[,1], color = "PSIV")) +
  xlab ("07 December 2019 to 23 June 2020") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Ethereum Classic SubSample 2") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5, color ="blue"))+
  scale_color_manual(name = "Models", 
                     labels = c("Returns","Norm", "SST", "SGED", "JSU", "PSIV"),
                     values = c("Returns" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PSIV" = "black"))+
  theme(legend.title = element_blank(),
        legend.key = element_rect(),
        legend.position = "bottom",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9))

ggexport(SS2VaR_ETC, filename = file.path(FIG_DIR, "SS2VaR_ETC.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

# Sub Sample 3

Z_Pearson_Sample3_ETC   <- qpearson(0.05,ETC_Param_Pearson_Sample3)
Sample3_ETC_Holdout_VaR   <- Sample3_ETC_Holdout_Mu + Sample3_ETC_Holdout_Sigma*Z_Pearson_Sample3_ETC

VR31_ETC <- data.frame(CrCu_ETC_VaR_Normal3)
VR32_ETC <- data.frame(CrCu_ETC_VaR_SST3)
VR33_ETC <- data.frame(CrCu_ETC_VaR_SGED3)
VR34_ETC <- data.frame(CrCu_ETC_VaR_JSU3)
VR35_ETC <- data.frame(Sample3_ETC_Holdout_VaR)
VR_Date3_ETC <- CrCu_ETC$Crypto_Date[(length(CrCu_ETC_xts)-99):length(CrCu_ETC_xts)]

SS3VaR_ETC <- ggplot(data.frame(VR31_ETC[,4]), aes(x= VR_Date3_ETC, y = VR31_ETC[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date3_ETC, y = VR31_ETC[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date3_ETC, y = VR32_ETC[,2], color = "SST")) +
  geom_line(aes(x = VR_Date3_ETC, y = VR33_ETC[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date3_ETC, y = VR34_ETC[,2], color = "JSU")) +
  geom_line(aes(x = VR_Date3_ETC, y = VR35_ETC[,1], color = "PSIV")) +
  xlab ("26 August 2021 to 04 December 2021") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Ethereum Classic SubSample 3") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5, color ="blue"))+
  scale_color_manual(name = "Models", 
                     labels = c("Returns","Norm", "SST", "SGED", "JSU", "PSIV"),
                     values = c("Returns" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PSIV" = "black"))+
  theme(legend.title = element_blank(),
        legend.key = element_rect(),
        legend.position = "bottom",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9))

ggexport(SS3VaR_ETC, filename = file.path(FIG_DIR, "SS3VaR_ETC.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

#Full Data

Z_Pearson_ETC   <- qpearson(0.05,CrCu_ETC_Dist8)
ETC_Holdout_VaR   <- ETC_Holdout_Mu + ETC_Holdout_Sigma*Z_Pearson_ETC

VR1_ETC <- data.frame(CrCu_ETC_Roll8@forecast$VaR)
VR2_ETC <- data.frame(CrCu_ETC_Roll5@forecast$VaR)
VR3_ETC <- data.frame(CrCu_ETC_Roll6@forecast$VaR)
VR4_ETC <- data.frame(CrCu_ETC_Roll7@forecast$VaR)
VR5_ETC <- data.frame(ETC_Holdout_VaR)
VR_Date_ETC <- CrCu_ETC$Crypto_Date[(length(CrCu_ETC_xts)-349):length(CrCu_ETC_xts)]

FSVaR_ETC <- ggplot(data.frame(VR1_ETC[,4]), aes(x= VR_Date_ETC, y = VR1_ETC[,4])) +
  geom_line(aes(colour = "Return"))  +
  geom_line(aes(x = VR_Date_ETC, y = VR1_ETC[,2], colour = "Norm")) +
  geom_line(aes(x = VR_Date_ETC, y = VR2_ETC[,2], color = "SST")) +
  geom_line(aes(x = VR_Date_ETC, y = VR3_ETC[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date_ETC, y = VR4_ETC[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date_ETC, y = VR5_ETC[,1], color = "PS")) +
  xlab ("19 Dec 2020 to 04 Dec 2021") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Ethereum Classic") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5, color ="blue"))+
  scale_color_manual(name = "Models", 
                     labels = c("Return","Norm", "SST", "SGED", "JSU", "PS"),
                     values = c("Return" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PS" = "black"))+
  theme(legend.title = element_blank(),
        legend.key = element_rect(),
        legend.position = "bottom",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9))

ggexport(FSVaR_ETC, filename = file.path(FIG_DIR, "FSVaR_ETC.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

########## MAPE ###########
# Forecast performance measure for sub samples
# Value at Risk

# Sample 1

VaR_s1_01_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED1$`alpha(1%)`, abs(CrCu_ETC_VaR_SGED1$realized))
VaR_s1_05_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED1$`alpha(5%)`, abs(CrCu_ETC_VaR_SGED1$realized))
VaR_s1_10_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED1$`alpha(10%)`, abs(CrCu_ETC_VaR_SGED1$realized))

VaR_s1_01_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU1$`alpha(1%)`, abs(CrCu_ETC_VaR_JSU1$realized))
VaR_s1_05_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU1$`alpha(5%)`, abs(CrCu_ETC_VaR_JSU1$realized))
VaR_s1_10_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU1$`alpha(10%)`, abs(CrCu_ETC_VaR_JSU1$realized))

VaR_s1_01_ETC_SST <- MAPE(CrCu_ETC_VaR_SST1$`alpha(1%)`, abs(CrCu_ETC_VaR_SST1$realized))
VaR_s1_05_ETC_SST <- MAPE(CrCu_ETC_VaR_SST1$`alpha(5%)`, abs(CrCu_ETC_VaR_SST1$realized))
VaR_s1_10_ETC_SST <- MAPE(CrCu_ETC_VaR_SST1$`alpha(10%)`, abs(CrCu_ETC_VaR_SST1$realized))

VaR_s1_01_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal1$`alpha(1%)`, abs(CrCu_ETC_VaR_Normal1$realized))
VaR_s1_05_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal1$`alpha(5%)`, abs(CrCu_ETC_VaR_Normal1$realized))
VaR_s1_10_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal1$`alpha(10%)`, abs(CrCu_ETC_VaR_Normal1$realized))

Z_Pearson_Sample1_ETC_01   <- qpearson(0.01,ETC_Param_Pearson_Sample1)
Sample1_ETC_Holdout_VaR_01   <- Sample1_ETC_Holdout_Mu + Sample1_ETC_Holdout_Sigma*Z_Pearson_Sample1_ETC_01
Z_Pearson_Sample1_ETC_05   <- qpearson(0.05,ETC_Param_Pearson_Sample1)
Sample1_ETC_Holdout_VaR_05   <- Sample1_ETC_Holdout_Mu + Sample1_ETC_Holdout_Sigma*Z_Pearson_Sample1_ETC_05
Z_Pearson_Sample1_ETC_10   <- qpearson(0.10,ETC_Param_Pearson_Sample1)
Sample1_ETC_Holdout_VaR_10   <- Sample1_ETC_Holdout_Mu + Sample1_ETC_Holdout_Sigma*Z_Pearson_Sample1_ETC_10

VaR_s1_01_ETC_PS <- MAPE(Sample1_ETC_Holdout_VaR_01, abs(CrCu_ETC_VaR_SGED1$realized))
VaR_s1_05_ETC_PS <- MAPE(Sample1_ETC_Holdout_VaR_05, abs(CrCu_ETC_VaR_SGED1$realized))
VaR_s1_10_ETC_PS <- MAPE(Sample1_ETC_Holdout_VaR_10, abs(CrCu_ETC_VaR_SGED1$realized))


MAPEval_fsub1_ETC  <- matrix(data=0,nrow=5,ncol=3)
colnames(MAPEval_fsub1_ETC) <- c("SS1 - 1%", "SS1 - 5%", "SS1 - 10%")
rownames(MAPEval_fsub1_ETC) <- c("SGED","JSU","SST","Normal", "PSIV")

MAPEval_fsub1_ETC[,1] = rbind(VaR_s1_01_ETC_SGED,VaR_s1_01_ETC_JSU,VaR_s1_01_ETC_SST,VaR_s1_01_ETC_Normal,VaR_s1_01_ETC_PS )
MAPEval_fsub1_ETC[,2] = rbind(VaR_s1_05_ETC_SGED,VaR_s1_05_ETC_JSU,VaR_s1_05_ETC_SST,VaR_s1_05_ETC_Normal,VaR_s1_05_ETC_PS)
MAPEval_fsub1_ETC[,3] = rbind(VaR_s1_10_ETC_SGED,VaR_s1_10_ETC_JSU,VaR_s1_10_ETC_SST,VaR_s1_10_ETC_Normal,VaR_s1_10_ETC_PS)

# Sample 2

VaR_s2_01_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED2$`alpha(1%)`, abs(CrCu_ETC_VaR_SGED2$realized))
VaR_s2_05_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED2$`alpha(5%)`, abs(CrCu_ETC_VaR_SGED2$realized))
VaR_s2_10_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED2$`alpha(10%)`, abs(CrCu_ETC_VaR_SGED2$realized))

VaR_s2_01_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU2$`alpha(1%)`, abs(CrCu_ETC_VaR_JSU2$realized))
VaR_s2_05_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU2$`alpha(5%)`, abs(CrCu_ETC_VaR_JSU2$realized))
VaR_s2_10_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU2$`alpha(10%)`, abs(CrCu_ETC_VaR_JSU2$realized))

VaR_s2_01_ETC_SST <- MAPE(CrCu_ETC_VaR_SST2$`alpha(1%)`, abs(CrCu_ETC_VaR_SST2$realized))
VaR_s2_05_ETC_SST <- MAPE(CrCu_ETC_VaR_SST2$`alpha(5%)`, abs(CrCu_ETC_VaR_SST2$realized))
VaR_s2_10_ETC_SST <- MAPE(CrCu_ETC_VaR_SST2$`alpha(10%)`, abs(CrCu_ETC_VaR_SST2$realized))

VaR_s2_01_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal2$`alpha(1%)`, abs(CrCu_ETC_VaR_Normal2$realized))
VaR_s2_05_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal2$`alpha(5%)`, abs(CrCu_ETC_VaR_Normal2$realized))
VaR_s2_10_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal2$`alpha(10%)`, abs(CrCu_ETC_VaR_Normal2$realized))

Z_Pearson_Sample2_ETC_01   <- qpearson(0.01,ETC_Param_Pearson_Sample2)
Sample2_ETC_Holdout_VaR_01   <- Sample2_ETC_Holdout_Mu + Sample2_ETC_Holdout_Sigma*Z_Pearson_Sample2_ETC_01
Z_Pearson_Sample2_ETC_05   <- qpearson(0.05,ETC_Param_Pearson_Sample2)
Sample2_ETC_Holdout_VaR_05   <- Sample2_ETC_Holdout_Mu + Sample2_ETC_Holdout_Sigma*Z_Pearson_Sample2_ETC_05
Z_Pearson_Sample2_ETC_10   <- qpearson(0.10,ETC_Param_Pearson_Sample2)
Sample2_ETC_Holdout_VaR_10   <- Sample2_ETC_Holdout_Mu + Sample2_ETC_Holdout_Sigma*Z_Pearson_Sample2_ETC_10

VaR_s2_01_ETC_PS <- MAPE(Sample2_ETC_Holdout_VaR_01, abs(CrCu_ETC_VaR_SGED2$realized))
VaR_s2_05_ETC_PS <- MAPE(Sample2_ETC_Holdout_VaR_05, abs(CrCu_ETC_VaR_SGED2$realized))
VaR_s2_10_ETC_PS <- MAPE(Sample2_ETC_Holdout_VaR_10, abs(CrCu_ETC_VaR_SGED2$realized))


MAPEval_fsub2_ETC  <- matrix(data=0,nrow=5,ncol=3)
colnames(MAPEval_fsub2_ETC) <- c("SS2 - 1%", "SS2 - 5%", "SS2 - 10%")
rownames(MAPEval_fsub2_ETC) <- c("SGED","JSU","SST","Normal","PSIV")

MAPEval_fsub2_ETC[,1] = rbind(VaR_s2_01_ETC_SGED,VaR_s2_01_ETC_JSU,VaR_s2_01_ETC_SST,VaR_s2_01_ETC_Normal,VaR_s2_01_ETC_PS)
MAPEval_fsub2_ETC[,2] = rbind(VaR_s2_05_ETC_SGED,VaR_s2_05_ETC_JSU,VaR_s2_05_ETC_SST,VaR_s2_05_ETC_Normal,VaR_s2_05_ETC_PS)
MAPEval_fsub2_ETC[,3] = rbind(VaR_s2_10_ETC_SGED,VaR_s2_10_ETC_JSU,VaR_s2_10_ETC_SST,VaR_s2_10_ETC_Normal,VaR_s2_10_ETC_PS)

# Sample 3

VaR_s3_01_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED3$`alpha(1%)`, abs(CrCu_ETC_VaR_SGED3$realized))
VaR_s3_05_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED3$`alpha(5%)`, abs(CrCu_ETC_VaR_SGED3$realized))
VaR_s3_10_ETC_SGED <- MAPE(CrCu_ETC_VaR_SGED3$`alpha(10%)`, abs(CrCu_ETC_VaR_SGED3$realized))

VaR_s3_01_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU3$`alpha(1%)`, abs(CrCu_ETC_VaR_JSU3$realized))
VaR_s3_05_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU3$`alpha(5%)`, abs(CrCu_ETC_VaR_JSU3$realized))
VaR_s3_10_ETC_JSU <- MAPE(CrCu_ETC_VaR_JSU3$`alpha(10%)`, abs(CrCu_ETC_VaR_JSU3$realized))

VaR_s3_01_ETC_SST <- MAPE(CrCu_ETC_VaR_SST3$`alpha(1%)`, abs(CrCu_ETC_VaR_SST3$realized))
VaR_s3_05_ETC_SST <- MAPE(CrCu_ETC_VaR_SST3$`alpha(5%)`, abs(CrCu_ETC_VaR_SST3$realized))
VaR_s3_10_ETC_SST <- MAPE(CrCu_ETC_VaR_SST3$`alpha(10%)`, abs(CrCu_ETC_VaR_SST3$realized))

VaR_s3_01_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal3$`alpha(1%)`, abs(CrCu_ETC_VaR_Normal3$realized))
VaR_s3_05_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal3$`alpha(5%)`, abs(CrCu_ETC_VaR_Normal3$realized))
VaR_s3_10_ETC_Normal <- MAPE(CrCu_ETC_VaR_Normal3$`alpha(10%)`, abs(CrCu_ETC_VaR_Normal3$realized))

Z_Pearson_Sample3_ETC_01   <- qpearson(0.01,ETC_Param_Pearson_Sample3)
Sample3_ETC_Holdout_VaR_01   <- Sample3_ETC_Holdout_Mu + Sample3_ETC_Holdout_Sigma*Z_Pearson_Sample3_ETC_01
Z_Pearson_Sample3_ETC_05   <- qpearson(0.05,ETC_Param_Pearson_Sample3)
Sample3_ETC_Holdout_VaR_05   <- Sample3_ETC_Holdout_Mu + Sample3_ETC_Holdout_Sigma*Z_Pearson_Sample3_ETC_05
Z_Pearson_Sample3_ETC_10   <- qpearson(0.10,ETC_Param_Pearson_Sample3)
Sample3_ETC_Holdout_VaR_10   <- Sample3_ETC_Holdout_Mu + Sample3_ETC_Holdout_Sigma*Z_Pearson_Sample3_ETC_10

VaR_s3_01_ETC_PS <- MAPE(Sample3_ETC_Holdout_VaR_01, abs(CrCu_ETC_VaR_SGED3$realized))
VaR_s3_05_ETC_PS <- MAPE(Sample3_ETC_Holdout_VaR_05, abs(CrCu_ETC_VaR_SGED3$realized))
VaR_s3_10_ETC_PS <- MAPE(Sample3_ETC_Holdout_VaR_10, abs(CrCu_ETC_VaR_SGED3$realized))

MAPEval_fsub3_ETC  <- matrix(data=0,nrow=5,ncol=3)
colnames(MAPEval_fsub3_ETC) <- c("SS3 - 1%", "SS3 - 5%", "SS3 - 10%")
rownames(MAPEval_fsub3_ETC) <- c("SGED","JSU","SST","Normal","PSIV")

MAPEval_fsub3_ETC[,1] = rbind(VaR_s3_01_ETC_SGED,VaR_s3_01_ETC_JSU,VaR_s3_01_ETC_SST,VaR_s3_01_ETC_Normal,VaR_s3_01_ETC_PS)
MAPEval_fsub3_ETC[,2] = rbind(VaR_s3_05_ETC_SGED,VaR_s3_05_ETC_JSU,VaR_s3_05_ETC_SST,VaR_s3_05_ETC_Normal,VaR_s3_05_ETC_PS)
MAPEval_fsub3_ETC[,3] = rbind(VaR_s3_10_ETC_SGED,VaR_s3_10_ETC_JSU,VaR_s3_10_ETC_SST,VaR_s3_10_ETC_Normal,VaR_s3_10_ETC_PS)

rbind("SS1",MAPEval_fsub1_ETC, "SS2",MAPEval_fsub2_ETC,"SS3", MAPEval_fsub3_ETC)
xtable(MAPEval_fsub1_ETC, caption = "MAPE for out of sample forecast of Ethereum Classic Value at Risk - Sub Sample 1", digits = 4)

xtable(MAPEval_fsub2_ETC, caption = "MAPE for out of sample forecast of Ethereum Classic Value at Risk - Sub Sample 2", digits = 4)

xtable(MAPEval_fsub3_ETC, caption = "MAPE for out of sample forecast of Ethereum Classic Value at Risk - Sub Sample 3", digits = 4)

#### Backtesting and Storing the results

# Sample 1
library(GAS)
ETC_Test_SST1_Sample1 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,1],0.01)
ETC_Test_SST5_Sample1 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,2],0.05)
ETC_Test_SST10_Sample1 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,3],0.1)

ETC_Test_SGED1_Sample1 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,1],0.01)
ETC_Test_SGED5_Sample1 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,2],0.05)
ETC_Test_SGED10_Sample1 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,3],0.1)

ETC_Test_JSU1_Sample1 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,1],0.01)
ETC_Test_JSU5_Sample1 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,2],0.05)
ETC_Test_JSU10_Sample1 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,3],0.1)

ETC_Test_Normal1_Sample1 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,1],0.01)
ETC_Test_Normal5_Sample1 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,2],0.05)
ETC_Test_Normal10_Sample1 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,3],0.1)

##### Sample 2

ETC_Test_SST1_Sample2 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,1],0.01)
ETC_Test_SST5_Sample2 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,2],0.05)
ETC_Test_SST10_Sample2 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,3],0.1)

ETC_Test_SGED1_Sample2 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,1],0.01)
ETC_Test_SGED5_Sample2 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,2],0.05)
ETC_Test_SGED10_Sample2 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,3],0.1)

ETC_Test_JSU1_Sample2 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,1],0.01)
ETC_Test_JSU5_Sample2 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,2],0.05)
ETC_Test_JSU10_Sample2 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,3],0.1)

ETC_Test_Normal1_Sample2 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,1],0.01)
ETC_Test_Normal5_Sample2 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,2],0.05)
ETC_Test_Normal10_Sample2 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,3],0.1)

### Sample3

ETC_Test_SST1_Sample3 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,1],0.01)
ETC_Test_SST5_Sample3 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,2],0.05)
ETC_Test_SST10_Sample3 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,3],0.1)

ETC_Test_SGED1_Sample3 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,1],0.01)
ETC_Test_SGED5_Sample3 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,2],0.05)
ETC_Test_SGED10_Sample3 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,3],0.1)

ETC_Test_JSU1_Sample3 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,1],0.01)
ETC_Test_JSU5_Sample3 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,2],0.05)
ETC_Test_JSU10_Sample3 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,3],0.1)

ETC_Test_Normal1_Sample3 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,1],0.01)
ETC_Test_Normal5_Sample3 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,2],0.05)
ETC_Test_Normal10_Sample3 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,3],0.1)

ETC_Sample1 <- matrix(c(ETC_Test_Normal1_Sample1$LRuc[1],ETC_Test_SST1_Sample1$LRuc[1],ETC_Test_SGED1_Sample1$LRuc[1],ETC_Test_JSU1_Sample1$LRuc[1],
                        ETC_Test_Normal1_Sample1$LRcc[1],ETC_Test_SST1_Sample1$LRcc[1],ETC_Test_SGED1_Sample1$LRcc[1],ETC_Test_JSU1_Sample1$LRcc[1],
                        ETC_Test_Normal1_Sample1$DQ$stat,ETC_Test_SST1_Sample1$DQ$stat,ETC_Test_SGED1_Sample1$DQ$stat,ETC_Test_JSU1_Sample1$DQ$stat,
                        ETC_Test_Normal1_Sample1$AE,ETC_Test_SST1_Sample1$AE,ETC_Test_SGED1_Sample1$AE,ETC_Test_JSU1_Sample1$AE,
                        (ETC_Test_Normal1_Sample1$Loss$Loss/ETC_Test_Normal1_Sample1$Loss$Loss),(ETC_Test_SST1_Sample1$Loss$Loss/ETC_Test_Normal1_Sample1$Loss$Loss),
                        (ETC_Test_SGED1_Sample1$Loss$Loss/ETC_Test_Normal1_Sample1$Loss$Loss),(ETC_Test_JSU1_Sample1$Loss$Loss/ETC_Test_Normal1_Sample1$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(ETC_Sample1) <- c("Normal","SST","SGED","JSU")    
rownames(ETC_Sample1) <- c("UC","CC","DQ","AE","QL")    

ETC_Sample2 <- matrix(c(ETC_Test_Normal1_Sample2$LRuc[1],ETC_Test_SST1_Sample2$LRuc[1],ETC_Test_SGED1_Sample2$LRuc[1],ETC_Test_JSU1_Sample2$LRuc[1],
                        ETC_Test_Normal1_Sample2$LRcc[1],ETC_Test_SST1_Sample2$LRcc[1],ETC_Test_SGED1_Sample2$LRcc[1],ETC_Test_JSU1_Sample2$LRcc[1],
                        ETC_Test_Normal1_Sample2$DQ$stat,ETC_Test_SST1_Sample2$DQ$stat,ETC_Test_SGED1_Sample2$DQ$stat,ETC_Test_JSU1_Sample2$DQ$stat,
                        ETC_Test_Normal1_Sample2$AE,ETC_Test_SST1_Sample2$AE,ETC_Test_SGED1_Sample2$AE,ETC_Test_JSU1_Sample2$AE,
                        (ETC_Test_Normal1_Sample2$Loss$Loss/ETC_Test_Normal1_Sample2$Loss$Loss),(ETC_Test_SST1_Sample2$Loss$Loss/ETC_Test_Normal1_Sample2$Loss$Loss),
                        (ETC_Test_SGED1_Sample2$Loss$Loss/ETC_Test_Normal1_Sample2$Loss$Loss),(ETC_Test_JSU1_Sample2$Loss$Loss/ETC_Test_Normal1_Sample2$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(ETC_Sample2) <- c("Normal","SST","SGED","JSU")    
rownames(ETC_Sample2) <- c("UC","CC","DQ","AE","QL")    

ETC_Sample3 <- matrix(c(ETC_Test_Normal1_Sample3$LRuc[1],ETC_Test_SST1_Sample3$LRuc[1],ETC_Test_SGED1_Sample3$LRuc[1],ETC_Test_JSU1_Sample3$LRuc[1],
                        ETC_Test_Normal1_Sample3$LRcc[1],ETC_Test_SST1_Sample3$LRcc[1],ETC_Test_SGED1_Sample3$LRcc[1],ETC_Test_JSU1_Sample3$LRcc[1],
                        ETC_Test_Normal1_Sample3$DQ$stat,ETC_Test_SST1_Sample3$DQ$stat,ETC_Test_SGED1_Sample3$DQ$stat,ETC_Test_JSU1_Sample3$DQ$stat,
                        ETC_Test_Normal1_Sample3$AE,ETC_Test_SST1_Sample3$AE,ETC_Test_SGED1_Sample3$AE,ETC_Test_JSU1_Sample3$AE,
                        (ETC_Test_Normal1_Sample3$Loss$Loss/ETC_Test_Normal1_Sample3$Loss$Loss),(ETC_Test_SST1_Sample3$Loss$Loss/ETC_Test_Normal1_Sample3$Loss$Loss),
                        (ETC_Test_SGED1_Sample3$Loss$Loss/ETC_Test_Normal1_Sample3$Loss$Loss),(ETC_Test_JSU1_Sample3$Loss$Loss/ETC_Test_Normal1_Sample3$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(ETC_Sample3) <- c("Normal","SST","SGED","JSU")    
rownames(ETC_Sample3) <- c("UC","CC","DQ","AE","QL")    


### Storing the p-values for subsamples 

# 1 percent

ETC_pvalues_1pc_Sample1 <- matrix(c(ETC_Test_Normal1_Sample1$LRuc[2],ETC_Test_SST1_Sample1$LRuc[2],ETC_Test_SGED1_Sample1$LRuc[2],ETC_Test_JSU1_Sample1$LRuc[2],
                                    ETC_Test_Normal1_Sample1$LRcc[2],ETC_Test_SST1_Sample1$LRcc[2],ETC_Test_SGED1_Sample1$LRcc[2],ETC_Test_JSU1_Sample1$LRcc[2],
                                    ETC_Test_Normal1_Sample1$DQ$pvalue,ETC_Test_SST1_Sample1$DQ$pvalue,ETC_Test_SGED1_Sample1$DQ$pvalue,ETC_Test_JSU1_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

ETC_pvalues_1pc_Sample2 <- matrix(c(ETC_Test_Normal1_Sample2$LRuc[2],ETC_Test_SST1_Sample2$LRuc[2],ETC_Test_SGED1_Sample2$LRuc[2],ETC_Test_JSU1_Sample2$LRuc[2],
                                    ETC_Test_Normal1_Sample2$LRcc[2],ETC_Test_SST1_Sample2$LRcc[2],ETC_Test_SGED1_Sample2$LRcc[2],ETC_Test_JSU1_Sample2$LRcc[2],
                                    ETC_Test_Normal1_Sample2$DQ$pvalue,ETC_Test_SST1_Sample2$DQ$pvalue,ETC_Test_SGED1_Sample2$DQ$pvalue,ETC_Test_JSU1_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

ETC_pvalues_1pc_Sample3 <- matrix(c(ETC_Test_Normal1_Sample3$LRuc[2],ETC_Test_SST1_Sample3$LRuc[2],ETC_Test_SGED1_Sample3$LRuc[2],ETC_Test_JSU1_Sample3$LRuc[2],
                                    ETC_Test_Normal1_Sample3$LRcc[2],ETC_Test_SST1_Sample3$LRcc[2],ETC_Test_SGED1_Sample3$LRcc[2],ETC_Test_JSU1_Sample3$LRcc[2],
                                    ETC_Test_Normal1_Sample3$DQ$pvalue,ETC_Test_SST1_Sample3$DQ$pvalue,ETC_Test_SGED1_Sample3$DQ$pvalue,ETC_Test_JSU1_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

# 5 percent

ETC_pvalues_5pc_Sample1 <- matrix(c(ETC_Test_Normal5_Sample1$LRuc[2],ETC_Test_SST5_Sample1$LRuc[2],ETC_Test_SGED5_Sample1$LRuc[2],ETC_Test_JSU5_Sample1$LRuc[2],
                                    ETC_Test_Normal5_Sample1$LRcc[2],ETC_Test_SST5_Sample1$LRcc[2],ETC_Test_SGED5_Sample1$LRcc[2],ETC_Test_JSU5_Sample1$LRcc[2],
                                    ETC_Test_Normal5_Sample1$DQ$pvalue,ETC_Test_SST5_Sample1$DQ$pvalue,ETC_Test_SGED5_Sample1$DQ$pvalue,ETC_Test_JSU5_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

ETC_pvalues_5pc_Sample2 <- matrix(c(ETC_Test_Normal5_Sample2$LRuc[2],ETC_Test_SST5_Sample2$LRuc[2],ETC_Test_SGED5_Sample2$LRuc[2],ETC_Test_JSU5_Sample2$LRuc[2],
                                    ETC_Test_Normal5_Sample2$LRcc[2],ETC_Test_SST5_Sample2$LRcc[2],ETC_Test_SGED5_Sample2$LRcc[2],ETC_Test_JSU5_Sample2$LRcc[2],
                                    ETC_Test_Normal5_Sample2$DQ$pvalue,ETC_Test_SST5_Sample2$DQ$pvalue,ETC_Test_SGED5_Sample2$DQ$pvalue,ETC_Test_JSU5_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

ETC_pvalues_5pc_Sample3 <- matrix(c(ETC_Test_Normal5_Sample3$LRuc[2],ETC_Test_SST5_Sample3$LRuc[2],ETC_Test_SGED5_Sample3$LRuc[2],ETC_Test_JSU5_Sample3$LRuc[2],
                                    ETC_Test_Normal5_Sample3$LRcc[2],ETC_Test_SST5_Sample3$LRcc[2],ETC_Test_SGED5_Sample3$LRcc[2],ETC_Test_JSU5_Sample3$LRcc[2],
                                    ETC_Test_Normal5_Sample3$DQ$pvalue,ETC_Test_SST5_Sample3$DQ$pvalue,ETC_Test_SGED5_Sample3$DQ$pvalue,ETC_Test_JSU5_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

# 10 percent

ETC_pvalues_10pc_Sample1 <- matrix(c(ETC_Test_Normal10_Sample1$LRuc[2],ETC_Test_SST10_Sample1$LRuc[2],ETC_Test_SGED10_Sample1$LRuc[2],ETC_Test_JSU10_Sample1$LRuc[2],
                                     ETC_Test_Normal10_Sample1$LRcc[2],ETC_Test_SST10_Sample1$LRcc[2],ETC_Test_SGED10_Sample1$LRcc[2],ETC_Test_JSU10_Sample1$LRcc[2],
                                     ETC_Test_Normal10_Sample1$DQ$pvalue,ETC_Test_SST10_Sample1$DQ$pvalue,ETC_Test_SGED10_Sample1$DQ$pvalue,ETC_Test_JSU10_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

ETC_pvalues_10pc_Sample2 <- matrix(c(ETC_Test_Normal10_Sample2$LRuc[2],ETC_Test_SST10_Sample2$LRuc[2],ETC_Test_SGED10_Sample2$LRuc[2],ETC_Test_JSU10_Sample2$LRuc[2],
                                     ETC_Test_Normal10_Sample2$LRcc[2],ETC_Test_SST10_Sample2$LRcc[2],ETC_Test_SGED10_Sample2$LRcc[2],ETC_Test_JSU10_Sample2$LRcc[2],
                                     ETC_Test_Normal10_Sample2$DQ$pvalue,ETC_Test_SST10_Sample2$DQ$pvalue,ETC_Test_SGED10_Sample2$DQ$pvalue,ETC_Test_JSU10_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

ETC_pvalues_10pc_Sample3 <- matrix(c(ETC_Test_Normal10_Sample3$LRuc[2],ETC_Test_SST10_Sample3$LRuc[2],ETC_Test_SGED10_Sample3$LRuc[2],ETC_Test_JSU10_Sample3$LRuc[2],
                                     ETC_Test_Normal10_Sample3$LRcc[2],ETC_Test_SST10_Sample3$LRcc[2],ETC_Test_SGED10_Sample3$LRcc[2],ETC_Test_JSU10_Sample3$LRcc[2],
                                     ETC_Test_Normal10_Sample3$DQ$pvalue,ETC_Test_SST10_Sample3$DQ$pvalue,ETC_Test_SGED10_Sample3$DQ$pvalue,ETC_Test_JSU10_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

ETC_pvalues_1pc   <- rbind(ETC_pvalues_1pc_Sample1,ETC_pvalues_1pc_Sample2,ETC_pvalues_1pc_Sample3)
ETC_pvalues_5pc   <- rbind(ETC_pvalues_5pc_Sample1,ETC_pvalues_5pc_Sample2,ETC_pvalues_5pc_Sample3)
ETC_pvalues_10pc  <- rbind(ETC_pvalues_10pc_Sample1,ETC_pvalues_10pc_Sample2,ETC_pvalues_10pc_Sample3)

rownames(ETC_pvalues_1pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(ETC_pvalues_1pc) <- c("Normal","SST","SGED","JSU")

rownames(ETC_pvalues_5pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(ETC_pvalues_5pc) <- c("Normal","SST","SGED","JSU")

rownames(ETC_pvalues_10pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(ETC_pvalues_10pc) <- c("Normal","SST","SGED","JSU")

### Using VaRTest and VaRDuration Test individually ###
## VaRTest in R

CrCu_ETC_SGED1_VaR_Test1  <- VaRTest(0.01,CrCu_ETC_VaR_SGED1[,4],CrCu_ETC_VaR_SGED1[,1])
CrCu_ETC_SGED1_VaR_Test5  <- VaRTest(0.05,CrCu_ETC_VaR_SGED1[,4],CrCu_ETC_VaR_SGED1[,2])
CrCu_ETC_SGED1_VaR_Test10 <- VaRTest(0.1,CrCu_ETC_VaR_SGED1[,4],CrCu_ETC_VaR_SGED1[,3])

CrCu_ETC_JSU2_VaR_Test1  <- VaRTest(0.01,CrCu_ETC_VaR_JSU2[,4],CrCu_ETC_VaR_JSU2[,1])
CrCu_ETC_JSU2_VaR_Test5  <- VaRTest(0.05,CrCu_ETC_VaR_JSU2[,4],CrCu_ETC_VaR_JSU2[,2])
CrCu_ETC_JSU2_VaR_Test10 <- VaRTest(0.1,CrCu_ETC_VaR_JSU2[,4],CrCu_ETC_VaR_JSU2[,3])

CrCu_ETC_SST3_VaR_Test1  <- VaRTest(0.01,CrCu_ETC_VaR_SST3[,4],CrCu_ETC_VaR_SST3[,1])
CrCu_ETC_SST3_VaR_Test5  <- VaRTest(0.05,CrCu_ETC_VaR_SST3[,4],CrCu_ETC_VaR_SST3[,2])
CrCu_ETC_SST3_VaR_Test10 <- VaRTest(0.1,CrCu_ETC_VaR_SST3[,4],CrCu_ETC_VaR_SST3[,3])

# ::::::::::: number of exceedances
Backtestres_sub_ETC <- matrix(data = 0, nrow = 8, ncol = 3)

datavar_sub_ETC <- c(CrCu_ETC_SGED1_VaR_Test5$expected.exceed, CrCu_ETC_SGED1_VaR_Test10$expected.exceed, 
                     CrCu_ETC_JSU2_VaR_Test1$expected.exceed, CrCu_ETC_JSU2_VaR_Test5$expected.exceed, CrCu_ETC_JSU2_VaR_Test10$expected.exceed,
                     CrCu_ETC_SST3_VaR_Test1$expected.exceed, CrCu_ETC_SST3_VaR_Test5$expected.exceed, CrCu_ETC_SST3_VaR_Test10$expected.exceed,
                     CrCu_ETC_SGED1_VaR_Test5$actual.exceed, CrCu_ETC_SGED1_VaR_Test10$actual.exceed,
                     CrCu_ETC_JSU2_VaR_Test1$actual.exceed, CrCu_ETC_JSU2_VaR_Test5$actual.exceed, CrCu_ETC_JSU2_VaR_Test10$actual.exceed,
                     CrCu_ETC_SST3_VaR_Test1$actual.exceed, CrCu_ETC_SST3_VaR_Test5$actual.exceed, CrCu_ETC_SST3_VaR_Test10$actual.exceed
)

Backtestres_sub_ETC <- matrix(data = datavar_sub_ETC, ncol = 2, nrow = 8)
rownames(Backtestres_sub_ETC) <- c("SGED1 5%", "SGED1 10%","JSU2 1%", "JSU2 5%", "JSU2 10%", "SST3 1%", "SST3 5%", "SST3 10%")
colnames(Backtestres_sub_ETC) <- c("Expected Exceedances", "Actual Exceedances")

###### :::::::::::::::plot number of exceedances
fillwith_ETC <-  c(CrCu_ETC_SGED1_VaR_Test5$expected.exceed, CrCu_ETC_SGED1_VaR_Test5$actual.exceed, CrCu_ETC_SGED1_VaR_Test10$expected.exceed, CrCu_ETC_SGED1_VaR_Test10$actual.exceed,
                   CrCu_ETC_JSU2_VaR_Test1$expected.exceed, CrCu_ETC_JSU2_VaR_Test1$actual.exceed, CrCu_ETC_JSU2_VaR_Test5$expected.exceed, CrCu_ETC_JSU2_VaR_Test5$actual.exceed, CrCu_ETC_JSU2_VaR_Test10$expected.exceed, CrCu_ETC_JSU2_VaR_Test10$actual.exceed,
                   CrCu_ETC_SST3_VaR_Test1$expected.exceed, CrCu_ETC_SST3_VaR_Test1$actual.exceed, CrCu_ETC_SST3_VaR_Test5$expected.exceed, CrCu_ETC_SST3_VaR_Test5$actual.exceed, CrCu_ETC_SST3_VaR_Test10$expected.exceed, CrCu_ETC_SST3_VaR_Test10$actual.exceed)

df_ETC <- data.frame(distalpha = rep(c("SGED1 5%", "SGED1 10%","JSU2 1%", "JSU2 5%", "JSU2 10%", "SST3 1%", "SST3 5%", "SST3 10%"), each = 2), 
                     exceed = rep(c("Expected.Exceedances", "Actual.Exceedances"), times = 8), 
                     numb = fillwith_ETC)

ggplot(df_ETC, aes(fill=exceed, y=numb, x=distalpha)) +
  geom_bar(position='dodge', stat='identity') +
  ggtitle('# times actual loss is greater than VaR vs expected exceedances') +
  xlab('Distributions with confidence level') +
  ylab('Exceedances') +
  scale_fill_manual('Exceedances', values=c('light blue','gray')) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5, color ="black")) +
  geom_text(aes(label=fillwith_ETC), position = position_dodge(0.9), vjust=2, size = 4)


xtable(Backtestres_sub_ETC, caption = "Exceedances for Value at Risk Backtest for Ethereum Classic - SubSamples", digits = 0)

## Table for AE
fillwith1_ETC <-  c(CrCu_ETC_SGED1_VaR_Test5$actual.exceed/CrCu_ETC_SGED1_VaR_Test5$expected.exceed, CrCu_ETC_SGED1_VaR_Test10$actual.exceed/CrCu_ETC_SGED1_VaR_Test10$expected.exceed,
                    CrCu_ETC_JSU2_VaR_Test1$actual.exceed/CrCu_ETC_JSU2_VaR_Test1$expected.exceed, CrCu_ETC_JSU2_VaR_Test5$actual.exceed/CrCu_ETC_JSU2_VaR_Test5$expected.exceed, CrCu_ETC_JSU2_VaR_Test10$actual.exceed/CrCu_ETC_JSU2_VaR_Test10$expected.exceed,
                    CrCu_ETC_SST3_VaR_Test1$actual.exceed/CrCu_ETC_SST3_VaR_Test1$expected.exceed, CrCu_ETC_SST3_VaR_Test5$actual.exceed/CrCu_ETC_SST3_VaR_Test5$expected.exceed, CrCu_ETC_SST3_VaR_Test10$actual.exceed/CrCu_ETC_SST3_VaR_Test10$expected.exceed)


Backtestres_sub_ETC1 <- matrix(data = fillwith1_ETC, ncol = 1, nrow = 8)
rownames(Backtestres_sub_ETC1) <- c("SGED1 5%", "SGED1 10%","JSU2 1%", "JSU2 5%", "JSU2 10%", "SST3 1%", "SST3 5%", "SST3 10%")
colnames(Backtestres_sub_ETC1) <- c("Actual Exceedances/Expected Exceedances")

## Store the pvalues at a place

CrCu_ETC_VaR_Test_pvalues1 <- matrix(c(CrCu_ETC_SST_VaR_Test1$uc.LRp,CrCu_ETC_SST_VaR_Test1$cc.LRp,
                                       CrCu_ETC_SGED_VaR_Test1$uc.LRp,CrCu_ETC_SGED_VaR_Test1$cc.LRp,
                                       CrCu_ETC_JSU_VaR_Test1$uc.LRp,CrCu_ETC_JSU_VaR_Test1$cc.LRp,
                                       CrCu_ETC_Norm_VaR_Test1$uc.LRp,CrCu_ETC_Norm_VaR_Test1$cc.LRp),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_pvalues5 <- matrix(c(CrCu_ETC_SST_VaR_Test5$uc.LRp,CrCu_ETC_SST_VaR_Test5$cc.LRp,
                                       CrCu_ETC_SGED_VaR_Test5$uc.LRp,CrCu_ETC_SGED_VaR_Test5$cc.LRp,
                                       CrCu_ETC_JSU_VaR_Test5$uc.LRp,CrCu_ETC_JSU_VaR_Test5$cc.LRp,
                                       CrCu_ETC_Norm_VaR_Test5$uc.LRp,CrCu_ETC_Norm_VaR_Test5$cc.LRp),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_pvalues10 <- matrix(c(CrCu_ETC_SST_VaR_Test10$uc.LRp,CrCu_ETC_SST_VaR_Test10$cc.LRp,
                                        CrCu_ETC_SGED_VaR_Test10$uc.LRp,CrCu_ETC_SGED_VaR_Test10$cc.LRp,
                                        CrCu_ETC_JSU_VaR_Test10$uc.LRp,CrCu_ETC_JSU_VaR_Test10$cc.LRp,
                                        CrCu_ETC_Norm_VaR_Test10$uc.LRp,CrCu_ETC_Norm_VaR_Test10$cc.LRp),ncol=2,byrow = TRUE)

## Storing the test stats

CrCu_ETC_VaR_Test_teststat1 <- matrix(c(CrCu_ETC_SST_VaR_Test1$uc.LRstat,CrCu_ETC_SST_VaR_Test1$cc.LRstat,
                                        CrCu_ETC_SGED_VaR_Test1$uc.LRstat,CrCu_ETC_SGED_VaR_Test1$cc.LRstat,
                                        CrCu_ETC_JSU_VaR_Test1$uc.LRstat,CrCu_ETC_JSU_VaR_Test1$cc.LRstat,
                                        CrCu_ETC_Norm_VaR_Test1$uc.LRstat,CrCu_ETC_Norm_VaR_Test1$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_teststat5 <- matrix(c(CrCu_ETC_SST_VaR_Test5$uc.LRstat,CrCu_ETC_SST_VaR_Test5$cc.LRstat,
                                        CrCu_ETC_SGED_VaR_Test5$uc.LRstat,CrCu_ETC_SGED_VaR_Test5$cc.LRstat,
                                        CrCu_ETC_JSU_VaR_Test5$uc.LRstat,CrCu_ETC_JSU_VaR_Test5$cc.LRstat,
                                        CrCu_ETC_Norm_VaR_Test5$uc.LRstat,CrCu_ETC_Norm_VaR_Test5$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_teststat10 <- matrix(c(CrCu_ETC_SST_VaR_Test10$uc.LRstat,CrCu_ETC_SST_VaR_Test10$cc.LRstat,
                                         CrCu_ETC_SGED_VaR_Test10$uc.LRstat,CrCu_ETC_SGED_VaR_Test10$cc.LRstat,
                                         CrCu_ETC_JSU_VaR_Test10$uc.LRstat,CrCu_ETC_JSU_VaR_Test10$cc.LRstat,
                                         CrCu_ETC_Norm_VaR_Test10$uc.LRstat,CrCu_ETC_Norm_VaR_Test10$cc.LRstat),ncol=2,byrow = TRUE)
## Round them up to 4 decimals

CrCu_ETC_VaR_Test_pvalues1  <- round(CrCu_ETC_VaR_Test_pvalues1,4)
CrCu_ETC_VaR_Test_pvalues5  <- round(CrCu_ETC_VaR_Test_pvalues5,4)
CrCu_ETC_VaR_Test_pvalues10 <- round(CrCu_ETC_VaR_Test_pvalues10,4)

CrCu_ETC_VaR_Test_teststat1  <- round(CrCu_ETC_VaR_Test_teststat1,4)
CrCu_ETC_VaR_Test_teststat5  <- round(CrCu_ETC_VaR_Test_teststat5,4)
CrCu_ETC_VaR_Test_teststat10 <- round(CrCu_ETC_VaR_Test_teststat10,4)

rownames(CrCu_ETC_VaR_Test_pvalues1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_pvalues1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_ETC_VaR_Test_pvalues5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_pvalues5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_ETC_VaR_Test_pvalues10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_pvalues10)  <- c("Kupiec","Christoffersen")

rownames(CrCu_ETC_VaR_Test_teststat1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_teststat1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_ETC_VaR_Test_teststat5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_teststat5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_ETC_VaR_Test_teststat10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_ETC_VaR_Test_teststat10)  <- c("Kupiec","Christoffersen")
# Kupiec H0: observed failure rate is equal to the failure suggested by tests
# Christoffersen H0: Exceedances are not clustering or no consecutive exceedances
# Fail to reject H0 is ideal
# Based on above, H0 is rejected only in case of Normal 1%, so good results

#### VaRDuration Test for subsamples

CrCu_ETC_Dur_Normal_Sample1 <- VaRDurTest(0.10,CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,3])
CrCu_ETC_Dur_Normal_Sample2 <- VaRDurTest(0.10,CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,3])
CrCu_ETC_Dur_Normal_Sample3 <- VaRDurTest(0.10,CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,3])

CrCu_ETC_Dur_SST_Sample1 <- VaRDurTest(0.10,CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,3])
CrCu_ETC_Dur_SST_Sample2 <- VaRDurTest(0.10,CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,3])
CrCu_ETC_Dur_SST_Sample3 <- VaRDurTest(0.10,CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,3])

CrCu_ETC_Dur_SGED_Sample1 <- VaRDurTest(0.10,CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,3])
CrCu_ETC_Dur_SGED_Sample2 <- VaRDurTest(0.10,CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,3])
CrCu_ETC_Dur_SGED_Sample3 <- VaRDurTest(0.10,CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,3])

CrCu_ETC_Dur_JSU_Sample1 <- VaRDurTest(0.10,CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,3])
CrCu_ETC_Dur_JSU_Sample2 <- VaRDurTest(0.10,CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,3])
CrCu_ETC_Dur_JSU_Sample3 <- VaRDurTest(0.10,CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,3])

## Store the results

CrCu_ETC_Dur_Subsample_stat <- matrix(c(CrCu_ETC_Dur_Normal_Sample1$rLL,
                                        CrCu_ETC_Dur_SST_Sample1$rLL,
                                        CrCu_ETC_Dur_SGED_Sample1$rLL,
                                        CrCu_ETC_Dur_JSU_Sample1$rLL,
                                        CrCu_ETC_Dur_Normal_Sample2$rLL,
                                        CrCu_ETC_Dur_SST_Sample2$rLL,
                                        CrCu_ETC_Dur_SGED_Sample2$rLL,
                                        CrCu_ETC_Dur_JSU_Sample2$rLL,
                                        CrCu_ETC_Dur_Normal_Sample3$rLL,
                                        CrCu_ETC_Dur_SST_Sample3$rLL,
                                        CrCu_ETC_Dur_SGED_Sample3$rLL,
                                        CrCu_ETC_Dur_JSU_Sample3$rLL),nrow=4)

CrCu_ETC_Dur_Subsample_pvalues <- matrix(c(CrCu_ETC_Dur_Normal_Sample1$LRp,
                                           CrCu_ETC_Dur_SST_Sample1$LRp,
                                           CrCu_ETC_Dur_SGED_Sample1$LRp,
                                           CrCu_ETC_Dur_JSU_Sample1$LRp,
                                           CrCu_ETC_Dur_Normal_Sample2$LRp,
                                           CrCu_ETC_Dur_SST_Sample2$LRp,
                                           CrCu_ETC_Dur_SGED_Sample2$LRp,
                                           CrCu_ETC_Dur_JSU_Sample2$LRp,
                                           CrCu_ETC_Dur_Normal_Sample3$LRp,
                                           CrCu_ETC_Dur_SST_Sample3$LRp,
                                           CrCu_ETC_Dur_SGED_Sample3$LRp,
                                           CrCu_ETC_Dur_JSU_Sample3$LRp),nrow=4)

### Expected Shortfall Calculations ####

## Assuming Normal Distribution

CrCu_ETC_index_normal1 <- which(CrCu_ETC_Roll8@forecast$VaR[,1]>CrCu_ETC_Roll8@forecast$VaR[,4])
CrCu_ETC_VaR_values_normal1 <- CrCu_ETC_Roll8@forecast$VaR[CrCu_ETC_index_normal1,1]
CrCu_ETC_return_values_normal1 <- CrCu_ETC_Roll8@forecast$VaR[CrCu_ETC_index_normal1,4]
CrCu_ETC_ES_normal1 <- (CrCu_ETC_return_values_normal1/CrCu_ETC_VaR_values_normal1)
CrCu_ETC_ES_Measure1_normal1 <- mean(CrCu_ETC_ES_normal1)
CrCu_ETC_ES_Measure2_normal1 <- max(CrCu_ETC_ES_normal1)

CrCu_ETC_index_normal5 <- which(CrCu_ETC_Roll8@forecast$VaR[,2]>CrCu_ETC_Roll8@forecast$VaR[,4])
CrCu_ETC_VaR_values_normal5 <- CrCu_ETC_Roll8@forecast$VaR[CrCu_ETC_index_normal5,2]
CrCu_ETC_return_values_normal5 <- CrCu_ETC_Roll8@forecast$VaR[CrCu_ETC_index_normal5,4]
CrCu_ETC_ES_normal5 <- (CrCu_ETC_return_values_normal5/CrCu_ETC_VaR_values_normal5)
CrCu_ETC_ES_Measure1_normal5 <- mean(CrCu_ETC_ES_normal5)
CrCu_ETC_ES_Measure2_normal5 <- max(CrCu_ETC_ES_normal5)

CrCu_ETC_index_normal10 <- which(CrCu_ETC_Roll8@forecast$VaR[,3]>CrCu_ETC_Roll8@forecast$VaR[,4])
CrCu_ETC_VaR_values_normal10 <- CrCu_ETC_Roll8@forecast$VaR[CrCu_ETC_index_normal10,3]
CrCu_ETC_return_values_normal10 <- CrCu_ETC_Roll8@forecast$VaR[CrCu_ETC_index_normal10,4]
CrCu_ETC_ES_normal10 <- (CrCu_ETC_return_values_normal10/CrCu_ETC_VaR_values_normal10)
CrCu_ETC_ES_Measure1_normal10 <- mean(CrCu_ETC_ES_normal10)
CrCu_ETC_ES_Measure2_normal10 <- max(CrCu_ETC_ES_normal10)

## Assuming SST Distribution

CrCu_ETC_index_SST1 <- which(CrCu_ETC_Roll5@forecast$VaR[,1]>CrCu_ETC_Roll5@forecast$VaR[,4])
CrCu_ETC_VaR_values_SST1 <- CrCu_ETC_Roll5@forecast$VaR[CrCu_ETC_index_SST1,1]
CrCu_ETC_return_values_SST1 <- CrCu_ETC_Roll5@forecast$VaR[CrCu_ETC_index_SST1,4]
CrCu_ETC_ES_SST1 <- (CrCu_ETC_return_values_SST1/CrCu_ETC_VaR_values_SST1)
CrCu_ETC_ES_Measure1_SST1 <- mean(CrCu_ETC_ES_SST1)
CrCu_ETC_ES_Measure2_SST1 <- max(CrCu_ETC_ES_SST1)

CrCu_ETC_index_SST5 <- which(CrCu_ETC_Roll5@forecast$VaR[,2]>CrCu_ETC_Roll5@forecast$VaR[,4])
CrCu_ETC_VaR_values_SST5 <- CrCu_ETC_Roll5@forecast$VaR[CrCu_ETC_index_SST5,2]
CrCu_ETC_return_values_SST5 <- CrCu_ETC_Roll5@forecast$VaR[CrCu_ETC_index_SST5,4]
CrCu_ETC_ES_SST5 <- (CrCu_ETC_return_values_SST5/CrCu_ETC_VaR_values_SST5)
CrCu_ETC_ES_Measure1_SST5 <- mean(CrCu_ETC_ES_SST5)
CrCu_ETC_ES_Measure2_SST5 <- max(CrCu_ETC_ES_SST5)

CrCu_ETC_index_SST10 <- which(CrCu_ETC_Roll5@forecast$VaR[,3]>CrCu_ETC_Roll5@forecast$VaR[,4])
CrCu_ETC_VaR_values_SST10 <- CrCu_ETC_Roll5@forecast$VaR[CrCu_ETC_index_SST10,3]
CrCu_ETC_return_values_SST10 <- CrCu_ETC_Roll5@forecast$VaR[CrCu_ETC_index_SST10,4]
CrCu_ETC_ES_SST10 <- (CrCu_ETC_return_values_SST10/CrCu_ETC_VaR_values_SST10)
CrCu_ETC_ES_Measure1_SST10 <- mean(CrCu_ETC_ES_SST10)
CrCu_ETC_ES_Measure2_SST10 <- max(CrCu_ETC_ES_SST10)

## Assuming SGED Distribution

CrCu_ETC_index_SGED1 <- which(CrCu_ETC_Roll6@forecast$VaR[,1]>CrCu_ETC_Roll6@forecast$VaR[,4])
CrCu_ETC_VaR_values_SGED1 <- CrCu_ETC_Roll6@forecast$VaR[CrCu_ETC_index_SGED1,1]
CrCu_ETC_return_values_SGED1 <- CrCu_ETC_Roll6@forecast$VaR[CrCu_ETC_index_SGED1,4]
CrCu_ETC_ES_SGED1 <- (CrCu_ETC_return_values_SGED1/CrCu_ETC_VaR_values_SGED1)
CrCu_ETC_ES_Measure1_SGED1 <- mean(CrCu_ETC_ES_SGED1)
CrCu_ETC_ES_Measure2_SGED1 <- max(CrCu_ETC_ES_SGED1)

CrCu_ETC_index_SGED5 <- which(CrCu_ETC_Roll6@forecast$VaR[,2]>CrCu_ETC_Roll6@forecast$VaR[,4])
CrCu_ETC_VaR_values_SGED5 <- CrCu_ETC_Roll6@forecast$VaR[CrCu_ETC_index_SGED5,2]
CrCu_ETC_return_values_SGED5 <- CrCu_ETC_Roll6@forecast$VaR[CrCu_ETC_index_SGED5,4]
CrCu_ETC_ES_SGED5 <- (CrCu_ETC_return_values_SGED5/CrCu_ETC_VaR_values_SGED5)
CrCu_ETC_ES_Measure1_SGED5 <- mean(CrCu_ETC_ES_SGED5)
CrCu_ETC_ES_Measure2_SGED5 <- max(CrCu_ETC_ES_SGED5)

CrCu_ETC_index_SGED10 <- which(CrCu_ETC_Roll6@forecast$VaR[,3]>CrCu_ETC_Roll6@forecast$VaR[,4])
CrCu_ETC_VaR_values_SGED10 <- CrCu_ETC_Roll6@forecast$VaR[CrCu_ETC_index_SGED10,3]
CrCu_ETC_return_values_SGED10 <- CrCu_ETC_Roll6@forecast$VaR[CrCu_ETC_index_SGED10,4]
CrCu_ETC_ES_SGED10 <- (CrCu_ETC_return_values_SGED10/CrCu_ETC_VaR_values_SGED10)
CrCu_ETC_ES_Measure1_SGED10 <- mean(CrCu_ETC_ES_SGED10)
CrCu_ETC_ES_Measure2_SGED10 <- max(CrCu_ETC_ES_SGED10)

## Assuming JSU Distribution

CrCu_ETC_index_JSU1 <- which(CrCu_ETC_Roll7@forecast$VaR[,1]>CrCu_ETC_Roll7@forecast$VaR[,4])
CrCu_ETC_VaR_values_JSU1 <- CrCu_ETC_Roll7@forecast$VaR[CrCu_ETC_index_JSU1,1]
CrCu_ETC_return_values_JSU1 <- CrCu_ETC_Roll7@forecast$VaR[CrCu_ETC_index_JSU1,4]
CrCu_ETC_ES_JSU1 <- (CrCu_ETC_return_values_JSU1/CrCu_ETC_VaR_values_JSU1)
CrCu_ETC_ES_Measure1_JSU1 <- mean(CrCu_ETC_ES_JSU1)
CrCu_ETC_ES_Measure2_JSU1 <- max(CrCu_ETC_ES_JSU1)

CrCu_ETC_index_JSU5 <- which(CrCu_ETC_Roll7@forecast$VaR[,2]>CrCu_ETC_Roll7@forecast$VaR[,4])
CrCu_ETC_VaR_values_JSU5 <- CrCu_ETC_Roll7@forecast$VaR[CrCu_ETC_index_JSU5,2]
CrCu_ETC_return_values_JSU5 <- CrCu_ETC_Roll7@forecast$VaR[CrCu_ETC_index_JSU5,4]
CrCu_ETC_ES_JSU5 <- (CrCu_ETC_return_values_JSU5/CrCu_ETC_VaR_values_JSU5)
CrCu_ETC_ES_Measure1_JSU5 <- mean(CrCu_ETC_ES_JSU5)
CrCu_ETC_ES_Measure2_JSU5 <- max(CrCu_ETC_ES_JSU5)

CrCu_ETC_index_JSU10 <- which(CrCu_ETC_Roll7@forecast$VaR[,3]>CrCu_ETC_Roll7@forecast$VaR[,4])
CrCu_ETC_VaR_values_JSU10 <- CrCu_ETC_Roll7@forecast$VaR[CrCu_ETC_index_JSU10,3]
CrCu_ETC_return_values_JSU10 <- CrCu_ETC_Roll7@forecast$VaR[CrCu_ETC_index_JSU10,4]
CrCu_ETC_ES_JSU10 <- (CrCu_ETC_return_values_JSU10/CrCu_ETC_VaR_values_JSU10)
CrCu_ETC_ES_Measure1_JSU10 <- mean(CrCu_ETC_ES_JSU10)
CrCu_ETC_ES_Measure2_JSU10 <- max(CrCu_ETC_ES_JSU10)

### Assuming Pearson Distribution

Z_Pearson_ETC_01   <- qpearson(0.01,CrCu_ETC_Dist8)
ETC_Holdout_VaR_01   <- ETC_Holdout_Mu + ETC_Holdout_Sigma*Z_Pearson_ETC_01

ETC_index_Pearson1 <- which(ETC_Holdout_VaR_01[-1]>CrCu_ETC_Roll7@forecast$VaR[1:349,4])
ETC_VaR_values_Pearson1 <- ETC_Holdout_VaR_01[ETC_index_Pearson1]
ETC_return_values_Pearson1 <- CrCu_ETC_Roll7@forecast$VaR[ETC_index_Pearson1,4]
ETC_ES_Pearson1 <- (ETC_return_values_Pearson1/ETC_VaR_values_Pearson1)
ETC_ES_Measure1_Pearson1 <- mean(ETC_ES_Pearson1)
ETC_ES_Measure2_Pearson1 <- max(ETC_ES_Pearson1)

Z_Pearson_ETC_05   <- qpearson(0.05,CrCu_ETC_Dist8)
ETC_Holdout_VaR_05   <- ETC_Holdout_Mu + ETC_Holdout_Sigma*Z_Pearson_ETC_05

CrCu_ETC_index_Pearson5 <- which(ETC_Holdout_VaR_05[-1]>CrCu_ETC_Roll7@forecast$VaR[1:349,4])
CrCu_ETC_VaR_values_Pearson5 <- ETC_Holdout_VaR_05[CrCu_ETC_index_Pearson5]
CrCu_ETC_return_values_Pearson5 <- CrCu_ETC_Roll7@forecast$VaR[CrCu_ETC_index_Pearson5,4]
CrCu_ETC_ES_Pearson5 <- (CrCu_ETC_return_values_Pearson5/CrCu_ETC_VaR_values_Pearson5)
CrCu_ETC_ES_Measure1_Pearson5 <- mean(CrCu_ETC_ES_Pearson5)
CrCu_ETC_ES_Measure2_Pearson5 <- max(CrCu_ETC_ES_Pearson5)

Z_Pearson_ETC_10   <- qpearson(0.10,CrCu_ETC_Dist8)
ETC_Holdout_VaR_10   <- ETC_Holdout_Mu + ETC_Holdout_Sigma*Z_Pearson_ETC_10

CrCu_ETC_index_Pearson10 <- which(ETC_Holdout_VaR_10[-1]>CrCu_ETC_Roll7@forecast$VaR[1:349,4])
CrCu_ETC_VaR_values_Pearson10 <- ETC_Holdout_VaR_10[CrCu_ETC_index_Pearson10]
CrCu_ETC_return_values_Pearson10 <- CrCu_ETC_Roll7@forecast$VaR[CrCu_ETC_index_Pearson10,4]
CrCu_ETC_ES_Pearson10 <- (CrCu_ETC_return_values_Pearson10/CrCu_ETC_VaR_values_Pearson10)
CrCu_ETC_ES_Measure1_Pearson10 <- mean(CrCu_ETC_ES_Pearson10)
CrCu_ETC_ES_Measure2_Pearson10 <- max(CrCu_ETC_ES_Pearson10)

### Store for Pearson

Pearson_Full_ES_Measure1_ETC <- matrix(c(ETC_ES_Measure1_Pearson1,CrCu_ETC_ES_Measure1_Pearson5,CrCu_ETC_ES_Measure1_Pearson10),nrow=3)

Pearson_Full_ES_Measure2_ETC <- matrix(c(ETC_ES_Measure2_Pearson1,CrCu_ETC_ES_Measure2_Pearson5,CrCu_ETC_ES_Measure2_Pearson10),nrow=3)

rownames(Pearson_Full_ES_Measure1_ETC) <- c("1%","5%","10%")
rownames(Pearson_Full_ES_Measure2_ETC) <- c("1%","5%","10%")
colnames(Pearson_Full_ES_Measure1_ETC) <- c("ETC")
colnames(Pearson_Full_ES_Measure2_ETC) <- c("ETC")

#### Store the values
CrCu_ETC_Full_ES_Measure1 <- matrix(c(CrCu_ETC_ES_Measure1_normal1,CrCu_ETC_ES_Measure1_SST1,CrCu_ETC_ES_Measure1_SGED1,CrCu_ETC_ES_Measure1_JSU1,
                                      CrCu_ETC_ES_Measure1_normal5,CrCu_ETC_ES_Measure1_SST5,CrCu_ETC_ES_Measure1_SGED5,CrCu_ETC_ES_Measure1_JSU5,
                                      CrCu_ETC_ES_Measure1_normal10,CrCu_ETC_ES_Measure1_SST10,CrCu_ETC_ES_Measure1_SGED10,CrCu_ETC_ES_Measure1_JSU10),nrow=3,byrow=TRUE)

CrCu_ETC_Full_ES_Measure2 <- matrix(c(CrCu_ETC_ES_Measure2_normal1,CrCu_ETC_ES_Measure2_SST1,CrCu_ETC_ES_Measure2_SGED1,CrCu_ETC_ES_Measure2_JSU1,
                                      CrCu_ETC_ES_Measure2_normal5,CrCu_ETC_ES_Measure2_SST5,CrCu_ETC_ES_Measure2_SGED5,CrCu_ETC_ES_Measure2_JSU5,
                                      CrCu_ETC_ES_Measure2_normal10,CrCu_ETC_ES_Measure2_SST10,CrCu_ETC_ES_Measure2_SGED10,CrCu_ETC_ES_Measure2_JSU10),nrow=3,byrow=TRUE)

rownames(CrCu_ETC_Full_ES_Measure1) <- c("1%","5%","10%")
rownames(CrCu_ETC_Full_ES_Measure2) <- c("1%","5%","10%")

colnames(CrCu_ETC_Full_ES_Measure1) <- c("Normal","SST","SGED","JSU")
colnames(CrCu_ETC_Full_ES_Measure2) <- c("Normal","SST","SGED","JSU")

#### End of all the code/lines ###### 