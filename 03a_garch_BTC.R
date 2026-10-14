# =============================================================================
# 03a_garch_BTC.R
#
# Purpose : BTC: return-series tests, ARMA(1,1)-EGARCH(1,1) with Norm/SST/SGED/JSU
#           errors on the full sample (with break dummies) and on SS1-SS3; Pearson IV
#           (PSIV) fitted to standardised residuals; rolling 1-day-ahead VaR
#           (hold-out: last 350 obs of full sample; last 60/200/100 obs of SS1/SS2/SS3); Kupiec, Christoffersen and
#           duration tests; VaR plots.
# Inputs  : data/Cryptocurrency6.csv; R/functions/strucbreak.R
# Outputs : CrCu_BTC_* objects; output/figures/{FS,SS1,SS2,SS3}VaR_BTC.eps
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
library(Dowd) ## Package for test of independence of VaR
library(tseries) ## For conducting basic tests

# :::::::::::: Read the file
# setwd() removed: paths are set in R/00_setup.R
# import dataset
Crypto1 <- read.csv(DATA_FILE, header = TRUE)
Crypto <- read.csv(DATA_FILE, header = TRUE)
Crypto_Date   <- as.Date.character(Crypto1$Date,format = "%d/%m/%Y")

#### ********** B I T C O I N ********** ####
# Crypto1 - without date column #

Crypto1 <- data.frame(Crypto1$Bitcoin, Crypto1$Ethereum, Crypto1$Litecoin, Crypto1$Monero, Crypto1$Ethereum.Classic)
colnames(Crypto1) <- c("BTC", "ETH", "LTC", "XMR", "ETC")
View(Crypto1)
Crypto1$BTC = as.numeric(as.character(Crypto1$BTC))
CrCu_BTC <- data.frame(Crypto_Date, Crypto$Bitcoin) # Bitcoin Data with Date
View(CrCu_BTC)
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

CrCu_BTC_samples <- CPPP # Structural break locations

####### :::::::::: Returns Series

Returns_CrCu_BTC <- log(Crypto1$BTC[-1]/Crypto1$BTC[1:(nrow(Crypto1)-1)]) # Returns series
Des_Returns_CrCu_BTC <- basicStats(Returns_CrCu_BTC) # Descriptive statistics
View(Returns_CrCu_BTC)

#### Convert into xts objects ####

CrCu_BTC_xts   <- xts(Returns_CrCu_BTC,order.by = Crypto_Date[-1])
names(CrCu_BTC_xts)   <- "Returns"
View(CrCu_BTC_xts)

# Time series properties of return series 
# H0: The series has unit root
adf_BTC <- adf.test(CrCu_BTC_xts) #p-value proves that returns series is stationary
# H0: The series follow normal distribution
jb_BTC <- jarque.bera.test(CrCu_BTC_xts)
# H0: Independence in the given series
Bt_BTC <- Box.test(CrCu_BTC_xts, type = "Ljung-Box", lag = 10) #pvalue says that there is significant autocorrelation
Bt_BTC_sq <- Box.test(CrCu_BTC_xts^2, type = "Ljung-Box", lag = 10) #pvalue confirms presence of GARCH effect
# H0: No ARCH effects
arch_BTC <- ArchTest(CrCu_BTC_xts, lags = 5) #pvalue confirms evidence of presence of ARCH effects

### Subsamples Dates
#SS1 - 2/12/2016 to 27/09/2017
#SS2 - 28/09/2017 to 23/06/2020
#SS3 - 24/06/2020 to 04/12/2021

### Subsamples XTS ###
CrCu_BTC_sub1    <- CrCu_BTC_xts[1:CrCu_BTC_samples[1]]
CrCu_BTC_sub2    <- CrCu_BTC_xts[(CrCu_BTC_samples[1]+1):CrCu_BTC_samples[2]]
CrCu_BTC_sub3    <- CrCu_BTC_xts[(CrCu_BTC_samples[2]+1):nrow(CrCu_BTC_xts)]

### Dummy variables for structural breaks (to be used in Garch modelling)####

CrCu_BTC_Dummy1 <- array(data=0,dim=length(CrCu_BTC_xts))
CrCu_BTC_Dummy2 <- array(data=0,dim=length(CrCu_BTC_xts))
CrCu_BTC_Dummy3 <- array(data=0,dim=length(CrCu_BTC_xts))

CrCu_BTC_Dummy1[1:CrCu_BTC_samples[1]]  <- 1
CrCu_BTC_Dummy2[(CrCu_BTC_samples[1]+1):CrCu_BTC_samples[2]] <- 1
CrCu_BTC_Dummy3[(CrCu_BTC_samples[2]+1):length(CrCu_BTC_xts)] <- 1

CrCu_BTC_Dummy <- cbind(CrCu_BTC_Dummy1,CrCu_BTC_Dummy2,CrCu_BTC_Dummy3)

## :::::::::::: check presence of ARCH effetc in Full Sample
library(FinTS)
ArchTest(CrCu_BTC_xts, lags = 1)
ArchTest(CrCu_BTC_xts, lags = 5)
ArchTest(CrCu_BTC_xts, lags = 10)
pacf((CrCu_BTC_xts)^2)
# H0: Series has no ARCH effects
# Null hypothesis is rejected, hence proves presence of ARCH effects

### Garch Models
# :::::::::With external regressors in Full Sample GARCH

#SST #SGED #JSU #Norm
CrCu_BTC_Spec1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_BTC_Spec2 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_BTC_Spec3 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_BTC_Spec4 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_BTC_Model1 <- ugarchfit(CrCu_BTC_Spec1,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model2 <- ugarchfit(CrCu_BTC_Spec2,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model3 <- ugarchfit(CrCu_BTC_Spec3,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model4 <- ugarchfit(CrCu_BTC_Spec4,CrCu_BTC_xts,out.sample = 350)

#### egarch models

CrCu_BTC_Spec5 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_BTC_Spec6 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_BTC_Spec7 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_BTC_Spec8 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_BTC_Model5 <- ugarchfit(CrCu_BTC_Spec5,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model6 <- ugarchfit(CrCu_BTC_Spec6,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model7 <- ugarchfit(CrCu_BTC_Spec7,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model8 <- ugarchfit(CrCu_BTC_Spec8,CrCu_BTC_xts,out.sample = 350)

### gjrgarch models

CrCu_BTC_Spec9  <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_BTC_Spec10 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_BTC_Spec11 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_BTC_Spec12 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_BTC_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_BTC_Model9  <- ugarchfit(CrCu_BTC_Spec9,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model10 <- ugarchfit(CrCu_BTC_Spec10,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model11 <- ugarchfit(CrCu_BTC_Spec11,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model12 <- ugarchfit(CrCu_BTC_Spec12,CrCu_BTC_xts,out.sample = 350)

## persistence

persis_BTC <- matrix(data=0, nrow = 12, ncol = 1)
persis_BTC[1,1] <- round(persistence(CrCu_BTC_Model1),4)
persis_BTC[2,1] <- round(persistence(CrCu_BTC_Model2),4)
persis_BTC[3,1] <- round(persistence(CrCu_BTC_Model3),4)
persis_BTC[4,1] <- round(persistence(CrCu_BTC_Model4),4)
persis_BTC[5,1] <- round(persistence(CrCu_BTC_Model5),4)
persis_BTC[6,1] <- round(persistence(CrCu_BTC_Model6),4)
persis_BTC[7,1] <- round(persistence(CrCu_BTC_Model7),4)
persis_BTC[8,1] <- round(persistence(CrCu_BTC_Model8),4)
persis_BTC[9,1] <- round(persistence(CrCu_BTC_Model9),4)
persis_BTC[10,1] <- round(persistence(CrCu_BTC_Model10),4)
persis_BTC[11,1] <- round(persistence(CrCu_BTC_Model11),4)
persis_BTC[12,1] <- round(persistence(CrCu_BTC_Model12),4)

persistance_BTC <- matrix(data=persis_BTC, nrow = 12, ncol = 1)
colnames(persistance_BTC) <- c("Persistence_BTC")
rownames(persistance_BTC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(data.frame(persistance_BTC))

### Store the loglikelihood values, AIC and BIC ####

Llhood_BTC  <- matrix(data=0,nrow=12,ncol=1)
colnames(Llhood_BTC) <- c("CrCu_BTC")
rownames(Llhood_BTC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
Llhood_BTC[1,1]  <-  CrCu_BTC_Model1@fit$LLH
Llhood_BTC[2,1]  <-  CrCu_BTC_Model2@fit$LLH
Llhood_BTC[3,1]  <-  CrCu_BTC_Model3@fit$LLH
Llhood_BTC[4,1]  <-  CrCu_BTC_Model4@fit$LLH
Llhood_BTC[5,1]  <-  CrCu_BTC_Model5@fit$LLH
Llhood_BTC[6,1]  <-  CrCu_BTC_Model6@fit$LLH
Llhood_BTC[7,1]  <-  CrCu_BTC_Model7@fit$LLH
Llhood_BTC[8,1]  <-  CrCu_BTC_Model8@fit$LLH
Llhood_BTC[9,1]  <-  CrCu_BTC_Model9@fit$LLH
Llhood_BTC[10,1] <- CrCu_BTC_Model10@fit$LLH
Llhood_BTC[11,1] <- CrCu_BTC_Model11@fit$LLH
Llhood_BTC[12,1] <- CrCu_BTC_Model12@fit$LLH

BIC_BTC  <- matrix(data=0,nrow=12,ncol=1)
colnames(BIC_BTC) <- c("CrCu_BTC")
rownames(BIC_BTC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
BIC_BTC[1,1]  <-  infocriteria(CrCu_BTC_Model1)[2]
BIC_BTC[2,1]  <-  infocriteria(CrCu_BTC_Model2)[2]
BIC_BTC[3,1]  <-  infocriteria(CrCu_BTC_Model3)[2]
BIC_BTC[4,1]  <-  infocriteria(CrCu_BTC_Model4)[2]
BIC_BTC[5,1]  <-  infocriteria(CrCu_BTC_Model5)[2]
BIC_BTC[6,1]  <-  infocriteria(CrCu_BTC_Model6)[2]
BIC_BTC[7,1]  <-  infocriteria(CrCu_BTC_Model7)[2]
BIC_BTC[8,1]  <-  infocriteria(CrCu_BTC_Model8)[2]
BIC_BTC[9,1]  <-  infocriteria(CrCu_BTC_Model9)[2]
BIC_BTC[10,1] <- infocriteria(CrCu_BTC_Model10)[2]
BIC_BTC[11,1] <- infocriteria(CrCu_BTC_Model11)[2]
BIC_BTC[12,1] <- infocriteria(CrCu_BTC_Model12)[2]

AIC_BTC  <- matrix(data=0,nrow=12,ncol=1)
colnames(AIC_BTC) <- c("CrCu_BTC")
rownames(AIC_BTC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
AIC_BTC[1,1]  <-  infocriteria(CrCu_BTC_Model1)[1]
AIC_BTC[2,1]  <-  infocriteria(CrCu_BTC_Model2)[1]
AIC_BTC[3,1]  <-  infocriteria(CrCu_BTC_Model3)[1]
AIC_BTC[4,1]  <-  infocriteria(CrCu_BTC_Model4)[1]
AIC_BTC[5,1]  <-  infocriteria(CrCu_BTC_Model5)[1]
AIC_BTC[6,1]  <-  infocriteria(CrCu_BTC_Model6)[1]
AIC_BTC[7,1]  <-  infocriteria(CrCu_BTC_Model7)[1]
AIC_BTC[8,1]  <-  infocriteria(CrCu_BTC_Model8)[1]
AIC_BTC[9,1]  <-  infocriteria(CrCu_BTC_Model9)[1]
AIC_BTC[10,1] <- infocriteria(CrCu_BTC_Model10)[1]
AIC_BTC[11,1] <- infocriteria(CrCu_BTC_Model11)[1]
AIC_BTC[12,1] <- infocriteria(CrCu_BTC_Model12)[1]

Infocrit_BTC  <- matrix(data=0,nrow=12,ncol=3)
colnames(Infocrit_BTC) <- c("Likelihood", "BIC", "AIC")
rownames(Infocrit_BTC) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")

Infocrit_BTC[,1] = Llhood_BTC
Infocrit_BTC[,2] = BIC_BTC
Infocrit_BTC[,3] = AIC_BTC
View(data.frame(Infocrit_BTC))
xtable(Infocrit_BTC, caption = "Information Criteria for Bitcoin GARCH models - Full Sample", digits = 4)

## coefficients

coef_BTC_1 <- round(coef(CrCu_BTC_Model1),4)
coef_BTC_2 <- round(coef(CrCu_BTC_Model2),4)
coef_BTC_3 <- round(coef(CrCu_BTC_Model3),4)
coef_BTC_4 <- round(coef(CrCu_BTC_Model4),4)
coef_BTC_5 <- round(coef(CrCu_BTC_Model5),4)
coef_BTC_6 <- round(coef(CrCu_BTC_Model6),4)
coef_BTC_7 <- round(coef(CrCu_BTC_Model7),4)
coef_BTC_8 <- round(coef(CrCu_BTC_Model8),4)
coef_BTC_9 <- round(coef(CrCu_BTC_Model9),4)
coef_BTC_10 <- round(coef(CrCu_BTC_Model10),4)
coef_BTC_11 <- round(coef(CrCu_BTC_Model11),4)
coef_BTC_12 <- round(coef(CrCu_BTC_Model12),4)

garch_coef_BTC <- bind_rows(coef_BTC_1, coef_BTC_2, coef_BTC_3, coef_BTC_4,coef_BTC_5, coef_BTC_6, coef_BTC_7, coef_BTC_8, coef_BTC_9, coef_BTC_10, coef_BTC_11, coef_BTC_12)

garch_coef_BTC1 <- as.data.frame(garch_coef_BTC)
rownames(garch_coef_BTC1) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(garch_coef_BTC1)
xtable(garch_coef_BTC1, caption = "Coefficients of Bitcoin GARCH models - Full Sample", digits = 4)

### Ljung-box test statistic
# Check standardized shocks

#CrCu_BTC5_LQ1  <- Box.test(CrCu_BTC_Model5@fit$residuals/CrCu_BTC_Model5@fit$sigma,lag=1,type="Lj")
#CrCu_BTC5_LQ5  <- Box.test(CrCu_BTC_Model5@fit$residuals/CrCu_BTC_Model5@fit$sigma,lag=5,type="Lj")
#CrCu_BTC5_LQ10 <- Box.test(CrCu_BTC_Model5@fit$residuals/CrCu_BTC_Model5@fit$sigma,lag=10,type="Lj")

#CrCu_BTC6_LQ1  <- Box.test(CrCu_BTC_Model6@fit$residuals/CrCu_BTC_Model6@fit$sigma,lag=1,type="Lj")
#CrCu_BTC6_LQ5  <- Box.test(CrCu_BTC_Model6@fit$residuals/CrCu_BTC_Model6@fit$sigma,lag=5,type="Lj")
#CrCu_BTC6_LQ10 <- Box.test(CrCu_BTC_Model6@fit$residuals/CrCu_BTC_Model6@fit$sigma,lag=10,type="Lj")

#CrCu_BTC7_LQ1  <- Box.test(CrCu_BTC_Model7@fit$residuals/CrCu_BTC_Model7@fit$sigma,lag=1,type="Lj")
#CrCu_BTC7_LQ5  <- Box.test(CrCu_BTC_Model7@fit$residuals/CrCu_BTC_Model7@fit$sigma,lag=5,type="Lj")
#CrCu_BTC7_LQ10 <- Box.test(CrCu_BTC_Model7@fit$residuals/CrCu_BTC_Model7@fit$sigma,lag=10,type="Lj")

#CrCu_BTC8_LQ1  <- Box.test(CrCu_BTC_Model8@fit$residuals/CrCu_BTC_Model8@fit$sigma,lag=1,type="Lj")
#CrCu_BTC8_LQ5  <- Box.test(CrCu_BTC_Model8@fit$residuals/CrCu_BTC_Model8@fit$sigma,lag=5,type="Lj")
#CrCu_BTC8_LQ10 <- Box.test(CrCu_BTC_Model8@fit$residuals/CrCu_BTC_Model8@fit$sigma,lag=10,type="Lj")

#CrCu_BTC5_LQ <- c(CrCu_BTC5_LQ1$statistic,CrCu_BTC5_LQ5$statistic,CrCu_BTC5_LQ10$statistic)
#CrCu_BTC6_LQ <- c(CrCu_BTC6_LQ1$statistic,CrCu_BTC6_LQ5$statistic,CrCu_BTC6_LQ10$statistic)
#CrCu_BTC7_LQ <- c(CrCu_BTC7_LQ1$statistic,CrCu_BTC7_LQ5$statistic,CrCu_BTC7_LQ10$statistic)
#CrCu_BTC8_LQ <- c(CrCu_BTC8_LQ1$statistic,CrCu_BTC8_LQ5$statistic,CrCu_BTC8_LQ10$statistic)

#CrCu_BTC_res_stat <-   cbind(CrCu_BTC5_LQ,CrCu_BTC6_LQ,CrCu_BTC7_LQ,CrCu_BTC8_LQ)
#rownames(CrCu_BTC_res_stat) <- c("LQ1","LQ5","LQ10")
#colnames(CrCu_BTC_res_stat) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

#### 1-day ahead Rolling Forecasts ####
## Full sample for roll i.e. last 350 points for forecast and rest for model
CrCu_BTC_Roll5 <-  ugarchroll(CrCu_BTC_Spec5,CrCu_BTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll6 <-  ugarchroll(CrCu_BTC_Spec6,CrCu_BTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll7 <-  ugarchroll(CrCu_BTC_Spec7,CrCu_BTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll8 <-  ugarchroll(CrCu_BTC_Spec8,CrCu_BTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# :::::::::::::assess forecasting performance of volatility model
# MAPE and RMSE
# MAPE/RMSE for all five coins is computed once, after all coin scripts, in R/04_garch_mape_rmse.R

#### Store the VaR for the out sample using rolling window forecasts

CrCu_BTC_VaR_SST   <- CrCu_BTC_Roll5@forecast$VaR
CrCu_BTC_VaR_SGED  <- CrCu_BTC_Roll6@forecast$VaR
CrCu_BTC_VaR_JSU   <- CrCu_BTC_Roll7@forecast$VaR
CrCu_BTC_VaR_Norm  <- CrCu_BTC_Roll8@forecast$VaR

#### ::::::::::::: Repeating for P E A R S O N Distribution  #####

### Check the summary stats of the standardized residuals
CrCu_BTC_Model8_res <- CrCu_BTC_Model8@fit$residuals/CrCu_BTC_Model8@fit$sigma
CrCu_BTC_Descr8 <- basicStats(CrCu_BTC_Model8_res)

### Fitting distributions on the standardized residuals 

library(PearsonDS)
library(gsl)
CrCu_BTC_Dist8   <- pearsonFitML(CrCu_BTC_Model8_res)

### Store the parameters for different distributions  

CrCu_BTC_P4_Param    <- CrCu_BTC_Dist8[2:5]

### Checking the fit using KS tests

CrCu_BTC_test1 <- ks.test(CrCu_BTC_Model8_res,rpearson(length(CrCu_BTC_Model8_res),params = CrCu_BTC_Dist8))
#Null: True distribution is equal to the hypothesized distribution
#p-value is high, hence, we fail to reject H0.

### Calculate the quantiles
signific_levels <- c(0.01,0.05,0.1)
BTC_Exceed_Pearson    <- array(data=0,dim=length(signific_levels))

for(j in 1:3) {
  Z_Pearson_BTC   <- qpearson(signific_levels[j],CrCu_BTC_Dist8)
  
  mu_BTC   <- array(data=0,dim=length(Returns_CrCu_BTC)-350)
  VaR_BTC   <- array(data=0,dim=length(Returns_CrCu_BTC)-351)
  
  for(i in 2:(length(Returns_CrCu_BTC)-350))
  {
    mu_BTC[i] <- CrCu_BTC_Model8@fit$coef[1]+CrCu_BTC_Model8@fit$coef[2]*Returns_CrCu_BTC[i-1]+CrCu_BTC_Model8@fit$coef[3]*CrCu_BTC_Model8_res[i-1]
    VaR_BTC[i-1] <- mu_BTC[i]+CrCu_BTC_Model8@fit$sigma[i]*Z_Pearson_BTC
  }
  #BTC_Violations[j]   <- VaRTest(signific_levels[j],Returns_CrCu_BTC[1:(length(VaR_BTC))],VaR_BTC)
  print(VaRTest(signific_levels[j],Returns_CrCu_BTC[1:(length(VaR_BTC))],VaR_BTC))
  #BTC_Exceed_Pearson[j] <- BTC_Violations$actual.exceed
}

#### Out of sample VaR forecast  #####

Holdout_Returns_BTC   <- Returns_CrCu_BTC[(length(Returns_CrCu_BTC)-349):length(Returns_CrCu_BTC)] 

Holdout_BTC_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_BTC_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_BTC_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_BTC_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_BTC_Pearson) <- c("1%","5%","10%")
rownames(Holdout_BTC_Pearson_pvalues) <- c("1%","5%","10%")

## Store the volatility and mu forecast

BTC_Holdout_Mu <- as.data.frame(CrCu_BTC_Roll8)[,'Mu']
BTC_Holdout_Sigma <- as.data.frame(CrCu_BTC_Roll8)[,'Sigma']

Holdout_BTC_Pearson <- matrix(0, nrow = 3, ncol = 5)
Holdout_BTC_Pearson_pvalues <- matrix(0, nrow = 3, ncol = 3)

for(j in 1:3) {
  
  Z_Pearson_BTC   <- qpearson(signific_levels[j],CrCu_BTC_Dist8)
  
  BTC_Holdout_VaR   <- BTC_Holdout_Mu + BTC_Holdout_Sigma*Z_Pearson_BTC
  BTC_Violations   <- BacktestVaR(Holdout_Returns_BTC[1:(length(Holdout_Returns_BTC)-1)],BTC_Holdout_VaR[-1],signific_levels[j])
  Holdout_BTC_Pearson[j,]   <- c(BTC_Violations$LRuc[1],BTC_Violations$LRcc[1],BTC_Violations$DQ$stat,BTC_Violations$AE,BTC_Violations$Loss$Loss)
  Holdout_BTC_Pearson_pvalues[j,] <- c(BTC_Violations$LRuc[2],BTC_Violations$LRcc[2],BTC_Violations$DQ$pvalue)
  print(BTC_Violations$AE)
  #print(BTC_Violations)
}

##### Backtesting
BTC_Test_SST_1  <- BacktestVaR(CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,1],signific_levels[1])
BTC_Test_SST_5  <- BacktestVaR(CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,2],signific_levels[2])
BTC_Test_SST_10 <- BacktestVaR(CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,3],signific_levels[3])

BTC_Test_SGED_1  <- BacktestVaR(CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,1],signific_levels[1])
BTC_Test_SGED_5  <- BacktestVaR(CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,2],signific_levels[2])
BTC_Test_SGED_10 <- BacktestVaR(CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,3],signific_levels[3])

BTC_Test_JSU_1  <- BacktestVaR(CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,1],signific_levels[1])
BTC_Test_JSU_5  <- BacktestVaR(CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,2],signific_levels[2])
BTC_Test_JSU_10 <- BacktestVaR(CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,3],signific_levels[3])

BTC_Test_Norm_1  <- BacktestVaR(CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,1],signific_levels[1])
BTC_Test_Norm_5  <- BacktestVaR(CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,2],signific_levels[2])
BTC_Test_Norm_10 <- BacktestVaR(CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,3],signific_levels[3])

#### Saving the test stats and pvalues for 1%, 5% and 10% VaR

## 1%

BTC_Backtest_teststat1  <- matrix(c(BTC_Test_SST_1$LRuc[1],BTC_Test_SST_1$LRcc[1],BTC_Test_SST_1$DQ$stat,
                                    BTC_Test_SGED_1$LRuc[1],BTC_Test_SGED_1$LRcc[1],BTC_Test_SGED_1$DQ$stat,
                                    BTC_Test_JSU_1$LRuc[1],BTC_Test_JSU_1$LRcc[1],BTC_Test_JSU_1$DQ$stat,
                                    BTC_Test_Norm_1$LRuc[1],BTC_Test_Norm_1$LRcc[1],BTC_Test_Norm_1$DQ$stat),nrow=4,byrow = TRUE)
rownames(BTC_Backtest_teststat1)  <- c("SST","SGED","JSU","Norm")
colnames(BTC_Backtest_teststat1)  <- c("Kupiec","Christoffersen","DQ")

BTC_Backtest_pvalues1  <- matrix(c(BTC_Test_SST_1$LRuc[2],BTC_Test_SST_1$LRcc[2],BTC_Test_SST_1$DQ$pvalue,
                                   BTC_Test_SGED_1$LRuc[2],BTC_Test_SGED_1$LRcc[2],BTC_Test_SGED_1$DQ$pvalue,
                                   BTC_Test_JSU_1$LRuc[2],BTC_Test_JSU_1$LRcc[2],BTC_Test_JSU_1$DQ$pvalue,
                                   BTC_Test_Norm_1$LRuc[2],BTC_Test_Norm_1$LRcc[2],BTC_Test_Norm_1$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(BTC_Backtest_pvalues1)  <- c("SST","SGED","JSU","Norm")
colnames(BTC_Backtest_pvalues1)  <- c("Kupiec","Christoffersen","DQ")

### 5%

BTC_Backtest_teststat5  <- matrix(c(BTC_Test_SST_5$LRuc[1],BTC_Test_SST_5$LRcc[1],BTC_Test_SST_5$DQ$stat,
                                    BTC_Test_SGED_5$LRuc[1],BTC_Test_SGED_5$LRcc[1],BTC_Test_SGED_5$DQ$stat,
                                    BTC_Test_JSU_5$LRuc[1],BTC_Test_JSU_5$LRcc[1],BTC_Test_JSU_5$DQ$stat,
                                    BTC_Test_Norm_5$LRuc[1],BTC_Test_Norm_5$LRcc[1],BTC_Test_Norm_5$DQ$stat),nrow=4,byrow = TRUE)
rownames(BTC_Backtest_teststat5)  <- c("SST","SGED","JSU","Norm")
colnames(BTC_Backtest_teststat5)  <- c("Kupiec","Christoffersen","DQ")

BTC_Backtest_pvalues5  <- matrix(c(BTC_Test_SST_5$LRuc[2],BTC_Test_SST_5$LRcc[2],BTC_Test_SST_5$DQ$pvalue,
                                   BTC_Test_SGED_5$LRuc[2],BTC_Test_SGED_5$LRcc[2],BTC_Test_SGED_5$DQ$pvalue,
                                   BTC_Test_JSU_5$LRuc[2],BTC_Test_JSU_5$LRcc[2],BTC_Test_JSU_5$DQ$pvalue,
                                   BTC_Test_Norm_5$LRuc[2],BTC_Test_Norm_5$LRcc[2],BTC_Test_Norm_5$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(BTC_Backtest_pvalues5)  <- c("SST","SGED","JSU","Norm")
colnames(BTC_Backtest_pvalues5)  <- c("Kupiec","Christoffersen","DQ")

### 10%

BTC_Backtest_teststat10  <- matrix(c(BTC_Test_SST_10$LRuc[1],BTC_Test_SST_10$LRcc[1],BTC_Test_SST_10$DQ$stat,
                                     BTC_Test_SGED_10$LRuc[1],BTC_Test_SGED_10$LRcc[1],BTC_Test_SGED_10$DQ$stat,
                                     BTC_Test_JSU_10$LRuc[1],BTC_Test_JSU_10$LRcc[1],BTC_Test_JSU_10$DQ$stat,
                                     BTC_Test_Norm_10$LRuc[1],BTC_Test_Norm_10$LRcc[1],BTC_Test_Norm_10$DQ$stat),nrow=4,byrow = TRUE)
rownames(BTC_Backtest_teststat10)  <- c("SST","SGED","JSU","Norm")
colnames(BTC_Backtest_teststat10)  <- c("Kupiec","Christoffersen","DQ")

BTC_Backtest_pvalues10  <- matrix(c(BTC_Test_SST_10$LRuc[2],BTC_Test_SST_10$LRcc[2],BTC_Test_SST_10$DQ$pvalue,
                                    BTC_Test_SGED_10$LRuc[2],BTC_Test_SGED_10$LRcc[2],BTC_Test_SGED_10$DQ$pvalue,
                                    BTC_Test_JSU_10$LRuc[2],BTC_Test_JSU_10$LRcc[2],BTC_Test_JSU_10$DQ$pvalue,
                                    BTC_Test_Norm_10$LRuc[2],BTC_Test_Norm_10$LRcc[2],BTC_Test_Norm_10$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(BTC_Backtest_pvalues10)  <- c("SST","SGED","JSU","Norm")
colnames(BTC_Backtest_pvalues10)  <- c("Kupiec","Christoffersen","DQ")

BTC_Backtest_pvalues1  <- round(BTC_Backtest_pvalues1,4)
BTC_Backtest_pvalues5  <- round(BTC_Backtest_pvalues5,4)
BTC_Backtest_pvalues10 <- round(BTC_Backtest_pvalues10,4)

BTC_Backtest_teststat1  <- round(BTC_Backtest_teststat1,4)
BTC_Backtest_teststat5  <- round(BTC_Backtest_teststat5,4)
BTC_Backtest_teststat10 <- round(BTC_Backtest_teststat10,4)

#####################################################################################
### Using VaRTest and VaRDuration Test individually ###
## VaRTest in R

CrCu_BTC_Norm_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,1])
CrCu_BTC_Norm_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,2])
CrCu_BTC_Norm_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,3])

CrCu_BTC_JSU_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,1])
CrCu_BTC_JSU_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,2])
CrCu_BTC_JSU_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,3])

CrCu_BTC_SGED_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,1])
CrCu_BTC_SGED_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,2])
CrCu_BTC_SGED_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,3])

CrCu_BTC_SST_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,1])
CrCu_BTC_SST_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,2])
CrCu_BTC_SST_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,3])

## Store the pvalues at a place

CrCu_BTC_VaR_Test_pvalues1 <- matrix(c(CrCu_BTC_SST_VaR_Test1$uc.LRp,CrCu_BTC_SST_VaR_Test1$cc.LRp,
                                       CrCu_BTC_SGED_VaR_Test1$uc.LRp,CrCu_BTC_SGED_VaR_Test1$cc.LRp,
                                       CrCu_BTC_JSU_VaR_Test1$uc.LRp,CrCu_BTC_JSU_VaR_Test1$cc.LRp,
                                       CrCu_BTC_Norm_VaR_Test1$uc.LRp,CrCu_BTC_Norm_VaR_Test1$cc.LRp),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_pvalues5 <- matrix(c(CrCu_BTC_SST_VaR_Test5$uc.LRp,CrCu_BTC_SST_VaR_Test5$cc.LRp,
                                       CrCu_BTC_SGED_VaR_Test5$uc.LRp,CrCu_BTC_SGED_VaR_Test5$cc.LRp,
                                       CrCu_BTC_JSU_VaR_Test5$uc.LRp,CrCu_BTC_JSU_VaR_Test5$cc.LRp,
                                       CrCu_BTC_Norm_VaR_Test5$uc.LRp,CrCu_BTC_Norm_VaR_Test5$cc.LRp),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_pvalues10 <- matrix(c(CrCu_BTC_SST_VaR_Test10$uc.LRp,CrCu_BTC_SST_VaR_Test10$cc.LRp,
                                        CrCu_BTC_SGED_VaR_Test10$uc.LRp,CrCu_BTC_SGED_VaR_Test10$cc.LRp,
                                        CrCu_BTC_JSU_VaR_Test10$uc.LRp,CrCu_BTC_JSU_VaR_Test10$cc.LRp,
                                        CrCu_BTC_Norm_VaR_Test10$uc.LRp,CrCu_BTC_Norm_VaR_Test10$cc.LRp),ncol=2,byrow = TRUE)

## Storing the test stats

CrCu_BTC_VaR_Test_teststat1 <- matrix(c(CrCu_BTC_SST_VaR_Test1$uc.LRstat,CrCu_BTC_SST_VaR_Test1$cc.LRstat,
                                        CrCu_BTC_SGED_VaR_Test1$uc.LRstat,CrCu_BTC_SGED_VaR_Test1$cc.LRstat,
                                        CrCu_BTC_JSU_VaR_Test1$uc.LRstat,CrCu_BTC_JSU_VaR_Test1$cc.LRstat,
                                        CrCu_BTC_Norm_VaR_Test1$uc.LRstat,CrCu_BTC_Norm_VaR_Test1$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_teststat5 <- matrix(c(CrCu_BTC_SST_VaR_Test5$uc.LRstat,CrCu_BTC_SST_VaR_Test5$cc.LRstat,
                                        CrCu_BTC_SGED_VaR_Test5$uc.LRstat,CrCu_BTC_SGED_VaR_Test5$cc.LRstat,
                                        CrCu_BTC_JSU_VaR_Test5$uc.LRstat,CrCu_BTC_JSU_VaR_Test5$cc.LRstat,
                                        CrCu_BTC_Norm_VaR_Test5$uc.LRstat,CrCu_BTC_Norm_VaR_Test5$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_teststat10 <- matrix(c(CrCu_BTC_SST_VaR_Test10$uc.LRstat,CrCu_BTC_SST_VaR_Test10$cc.LRstat,
                                         CrCu_BTC_SGED_VaR_Test10$uc.LRstat,CrCu_BTC_SGED_VaR_Test10$cc.LRstat,
                                         CrCu_BTC_JSU_VaR_Test10$uc.LRstat,CrCu_BTC_JSU_VaR_Test10$cc.LRstat,
                                         CrCu_BTC_Norm_VaR_Test10$uc.LRstat,CrCu_BTC_Norm_VaR_Test10$cc.LRstat),ncol=2,byrow = TRUE)
## Round them up to 4 decimals

CrCu_BTC_VaR_Test_pvalues1  <- round(CrCu_BTC_VaR_Test_pvalues1,4)
CrCu_BTC_VaR_Test_pvalues5  <- round(CrCu_BTC_VaR_Test_pvalues5,4)
CrCu_BTC_VaR_Test_pvalues10 <- round(CrCu_BTC_VaR_Test_pvalues10,4)

CrCu_BTC_VaR_Test_teststat1  <- round(CrCu_BTC_VaR_Test_teststat1,4)
CrCu_BTC_VaR_Test_teststat5  <- round(CrCu_BTC_VaR_Test_teststat5,4)
CrCu_BTC_VaR_Test_teststat10 <- round(CrCu_BTC_VaR_Test_teststat10,4)

rownames(CrCu_BTC_VaR_Test_pvalues1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_pvalues1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_BTC_VaR_Test_pvalues5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_pvalues5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_BTC_VaR_Test_pvalues10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_pvalues10)  <- c("Kupiec","Christoffersen")

rownames(CrCu_BTC_VaR_Test_teststat1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_teststat1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_BTC_VaR_Test_teststat5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_teststat5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_BTC_VaR_Test_teststat10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_teststat10)  <- c("Kupiec","Christoffersen")
# Kupiec H0: observed failure rate is equal to the failure suggested by tests
# Christoffersen H0: Exceedances are not clustering or no consecutive exceedances
# Fail to reject H0 is ideal
# Based on above, H0 is rejected only in case of Normal 1%, so good results

## VaR Duration test

CrCu_BTC_Norm_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,1])
CrCu_BTC_Norm_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,2])
CrCu_BTC_Norm_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,3])

CrCu_BTC_JSU_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,1])
CrCu_BTC_JSU_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,2])
CrCu_BTC_JSU_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,3])

CrCu_BTC_SGED_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,1])
CrCu_BTC_SGED_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,2])
CrCu_BTC_SGED_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,3])

CrCu_BTC_SST_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,1])
CrCu_BTC_SST_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,2])
CrCu_BTC_SST_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,3])

### Store the test results and p=values

CrCu_BTC_VaR_Dur_Test_pvalues1 <- matrix(c(CrCu_BTC_SST_VaR_Dur_Test1$LRp,CrCu_BTC_SGED_VaR_Dur_Test1$LRp,
                                           CrCu_BTC_JSU_VaR_Dur_Test1$LRp,CrCu_BTC_Norm_VaR_Dur_Test1$LRp),nrow=4,byrow = TRUE)
CrCu_BTC_VaR_Dur_Test_pvalues5 <- matrix(c(CrCu_BTC_SST_VaR_Dur_Test5$LRp,CrCu_BTC_SGED_VaR_Dur_Test5$LRp,
                                           CrCu_BTC_JSU_VaR_Dur_Test5$LRp,CrCu_BTC_Norm_VaR_Dur_Test5$LRp),nrow=4,byrow = TRUE)
CrCu_BTC_VaR_Dur_Test_pvalues10 <- matrix(c(CrCu_BTC_SST_VaR_Dur_Test10$LRp,CrCu_BTC_SGED_VaR_Dur_Test10$LRp,
                                            CrCu_BTC_JSU_VaR_Dur_Test10$LRp,CrCu_BTC_Norm_VaR_Dur_Test10$LRp),nrow=4,byrow = TRUE)

CrCu_BTC_VaR_Dur_Test_teststat1 <- matrix(c(CrCu_BTC_SST_VaR_Dur_Test1$uLL,CrCu_BTC_SST_VaR_Dur_Test1$rLL,
                                            CrCu_BTC_SGED_VaR_Dur_Test1$uLL,CrCu_BTC_SGED_VaR_Dur_Test1$rLL,
                                            CrCu_BTC_JSU_VaR_Dur_Test1$uLL,CrCu_BTC_JSU_VaR_Dur_Test1$rLL,
                                            CrCu_BTC_Norm_VaR_Dur_Test1$uLL,CrCu_BTC_Norm_VaR_Dur_Test1$rLL),nrow=4,byrow = TRUE)
CrCu_BTC_VaR_Dur_Test_teststat5 <- matrix(c(CrCu_BTC_SST_VaR_Dur_Test5$uLL,CrCu_BTC_SST_VaR_Dur_Test5$rLL,
                                            CrCu_BTC_SGED_VaR_Dur_Test5$uLL,CrCu_BTC_SGED_VaR_Dur_Test5$rLL,
                                            CrCu_BTC_JSU_VaR_Dur_Test5$uLL,CrCu_BTC_JSU_VaR_Dur_Test5$rLL,
                                            CrCu_BTC_Norm_VaR_Dur_Test5$uLL,CrCu_BTC_Norm_VaR_Dur_Test5$rLL),nrow=4,byrow = TRUE)
CrCu_BTC_VaR_Dur_Test_teststat10 <- matrix(c(CrCu_BTC_SST_VaR_Dur_Test10$uLL,CrCu_BTC_SST_VaR_Dur_Test10$rLL,
                                             CrCu_BTC_SGED_VaR_Dur_Test10$uLL,CrCu_BTC_SGED_VaR_Dur_Test10$rLL,
                                             CrCu_BTC_JSU_VaR_Dur_Test10$uLL,CrCu_BTC_JSU_VaR_Dur_Test10$rLL,
                                             CrCu_BTC_Norm_VaR_Dur_Test10$uLL,CrCu_BTC_Norm_VaR_Dur_Test10$rLL),nrow=4,byrow = TRUE)
## Round them up to 4 decimals

CrCu_BTC_VaR_Dur_Test_pvalues1  <- round(CrCu_BTC_VaR_Dur_Test_pvalues1,4)
CrCu_BTC_VaR_Dur_Test_pvalues5  <- round(CrCu_BTC_VaR_Dur_Test_pvalues5,4)
CrCu_BTC_VaR_Dur_Test_pvalues10 <- round(CrCu_BTC_VaR_Dur_Test_pvalues10,4)

CrCu_BTC_VaR_Dur_Test_teststat1  <- round(CrCu_BTC_VaR_Dur_Test_teststat1,4)
CrCu_BTC_VaR_Dur_Test_teststat5  <- round(CrCu_BTC_VaR_Dur_Test_teststat5,4)
CrCu_BTC_VaR_Dur_Test_teststat10 <- round(CrCu_BTC_VaR_Dur_Test_teststat10,4)

rownames(CrCu_BTC_VaR_Dur_Test_pvalues1)  <- c("SST","SGED","JSU","Norm")
rownames(CrCu_BTC_VaR_Dur_Test_pvalues5)  <- c("SST","SGED","JSU","Norm")
rownames(CrCu_BTC_VaR_Dur_Test_pvalues10)  <- c("SST","SGED","JSU","Norm")

rownames(CrCu_BTC_VaR_Dur_Test_teststat1)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_BTC_VaR_Dur_Test_teststat1)  <- c("Unrestricted","Restricted")
rownames(CrCu_BTC_VaR_Dur_Test_teststat5)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_BTC_VaR_Dur_Test_teststat5)  <- c("Unrestricted","Restricted")
rownames(CrCu_BTC_VaR_Dur_Test_teststat10)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_BTC_VaR_Dur_Test_teststat10)  <- c("Unrestricted","Restricted")
# H0: Correctly specified risk model
# H0: Duration Between Exceedances have no memory
# Duration of time between VaR violations should ideally be independent and not clustered
# Fail to reject H0 in all case, which is good

# ____________________________________________________________________________________________ #

#### ::::::::::::::::: S U B S A M P L E Analysis ::::::::::::::::::::: ####

### ::::::::::::Descriptive Statistics and Basic Tests of **SUBSAMPLES** ::::::::::::::::: ###

Descr_CrCu_BTC   <- cbind(basicStats(CrCu_BTC_sub1),basicStats(CrCu_BTC_sub2),basicStats(CrCu_BTC_sub3))
# To check evidence of unit root
ADF_CrCu_BTC   <- cbind(adf.test(CrCu_BTC_sub1),adf.test(CrCu_BTC_sub2),adf.test(CrCu_BTC_sub3))
# To check evidence of unit root
pp_CrCu_BTC   <- cbind(pp.test(CrCu_BTC_sub1),pp.test(CrCu_BTC_sub2),pp.test(CrCu_BTC_sub3))
# To check assumption of normality
jb_CrCu_BTC   <- cbind(jarque.bera.test(CrCu_BTC_sub1),jarque.bera.test(CrCu_BTC_sub2),jarque.bera.test(CrCu_BTC_sub3))
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation
LQ_CrCu_BTC01   <- cbind(Box.test(CrCu_BTC_sub1^2,lag=1,type="Lj"),Box.test(CrCu_BTC_sub2^2,lag=1,type="Lj"),Box.test(CrCu_BTC_sub3^2,lag=1,type="Lj"))
LQ_CrCu_BTC05   <- cbind(Box.test(CrCu_BTC_sub1^2,lag=5,type="Lj"),Box.test(CrCu_BTC_sub2^2,lag=5,type="Lj"),Box.test(CrCu_BTC_sub3^2,lag=5,type="Lj"))
LQ_CrCu_BTC10   <- cbind(Box.test(CrCu_BTC_sub1^2,lag=10,type="Lj"),Box.test(CrCu_BTC_sub2^2,lag=10,type="Lj"),Box.test(CrCu_BTC_sub3^2,lag=10,type="Lj"))

## :::::::: Subsamples
ArchTest(CrCu_BTC_sub1, lags = 3)
ArchTest(CrCu_BTC_sub2, lags = 3)
ArchTest(CrCu_BTC_sub3, lags = 3)
# H0: No ARCH effect
# DEFINITELY ARCH EFFECTS EXIST
arch.test(CrCu_BTC_sub1, lags.single = 3)

#### JSU specification

CrCu_BTC_Spec_Sample1_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_BTC_Spec_Sample2_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_BTC_Spec_Sample3_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")

CrCu_BTC_Model_Sample_JSU1 <- ugarchfit(CrCu_BTC_Spec_Sample1_JSU,CrCu_BTC_sub1,out.sample = 60)
CrCu_BTC_Model_Sample_JSU2 <- ugarchfit(CrCu_BTC_Spec_Sample2_JSU,CrCu_BTC_sub2,out.sample = 200)
CrCu_BTC_Model_Sample_JSU3 <- ugarchfit(CrCu_BTC_Spec_Sample3_JSU,CrCu_BTC_sub3,out.sample = 100)

## Rollover Analysis

CrCu_BTC_Roll_JSU_Sample1 <-  ugarchroll(CrCu_BTC_Spec_Sample1_JSU,CrCu_BTC_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll_JSU_Sample2 <-  ugarchroll(CrCu_BTC_Spec_Sample2_JSU,CrCu_BTC_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll_JSU_Sample3 <-  ugarchroll(CrCu_BTC_Spec_Sample3_JSU,CrCu_BTC_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# ### SGED specification

CrCu_BTC_Spec_Sample1_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_BTC_Spec_Sample2_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_BTC_Spec_Sample3_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")

CrCu_BTC_Model_Sample_SGED1 <- ugarchfit(CrCu_BTC_Spec_Sample1_SGED,CrCu_BTC_sub1,out.sample = 60)
CrCu_BTC_Model_Sample_SGED2 <- ugarchfit(CrCu_BTC_Spec_Sample2_SGED,CrCu_BTC_sub2,out.sample = 200)
CrCu_BTC_Model_Sample_SGED3 <- ugarchfit(CrCu_BTC_Spec_Sample3_SGED,CrCu_BTC_sub3,out.sample = 100)

## Rollover Analysis

CrCu_BTC_Roll_SGED_Sample1 <-  ugarchroll(CrCu_BTC_Spec_Sample1_SGED,CrCu_BTC_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll_SGED_Sample2 <-  ugarchroll(CrCu_BTC_Spec_Sample2_SGED,CrCu_BTC_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll_SGED_Sample3 <-  ugarchroll(CrCu_BTC_Spec_Sample3_SGED,CrCu_BTC_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

### SST Specification

CrCu_BTC_Spec_Sample1_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_BTC_Spec_Sample2_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_BTC_Spec_Sample3_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")

CrCu_BTC_Model_Sample_SST1 <- ugarchfit(CrCu_BTC_Spec_Sample1_SST,CrCu_BTC_sub1,out.sample = 60)
CrCu_BTC_Model_Sample_SST2 <- ugarchfit(CrCu_BTC_Spec_Sample2_SST,CrCu_BTC_sub2,out.sample = 200)
CrCu_BTC_Model_Sample_SST3 <- ugarchfit(CrCu_BTC_Spec_Sample3_SST,CrCu_BTC_sub3,out.sample = 100)

## Rollover Analysis

CrCu_BTC_Roll_SST_Sample1 <-  ugarchroll(CrCu_BTC_Spec_Sample1_SST,CrCu_BTC_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll_SST_Sample2 <-  ugarchroll(CrCu_BTC_Spec_Sample2_SST,CrCu_BTC_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll_SST_Sample3 <-  ugarchroll(CrCu_BTC_Spec_Sample3_SST,CrCu_BTC_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# ### Normal Specification

CrCu_BTC_Spec_Sample1_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")
CrCu_BTC_Spec_Sample2_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")
CrCu_BTC_Spec_Sample3_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_BTC_Model_Sample_Normal1 <- ugarchfit(CrCu_BTC_Spec_Sample1_Normal,CrCu_BTC_sub1,out.sample = 60)
CrCu_BTC_Model_Sample_Normal2 <- ugarchfit(CrCu_BTC_Spec_Sample2_Normal,CrCu_BTC_sub2,out.sample = 200)
CrCu_BTC_Model_Sample_Normal3 <- ugarchfit(CrCu_BTC_Spec_Sample3_Normal,CrCu_BTC_sub3,out.sample = 100)

## Rollover Analysis

CrCu_BTC_Roll_Normal_Sample1 <-  ugarchroll(CrCu_BTC_Spec_Sample1_Normal,CrCu_BTC_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll_Normal_Sample2 <-  ugarchroll(CrCu_BTC_Spec_Sample2_Normal,CrCu_BTC_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll_Normal_Sample3 <-  ugarchroll(CrCu_BTC_Spec_Sample3_Normal,CrCu_BTC_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

## persistence

Persis_sub_BTC <- c(persistence(CrCu_BTC_Model_Sample_JSU1), persistence(CrCu_BTC_Model_Sample_JSU2), persistence(CrCu_BTC_Model_Sample_JSU3),
                    persistence(CrCu_BTC_Model_Sample_SGED1), persistence(CrCu_BTC_Model_Sample_SGED2), persistence(CrCu_BTC_Model_Sample_SGED3),
                    persistence(CrCu_BTC_Model_Sample_SST1), persistence(CrCu_BTC_Model_Sample_SST2), persistence(CrCu_BTC_Model_Sample_SST3),
                    persistence(CrCu_BTC_Model_Sample_Normal1), persistence(CrCu_BTC_Model_Sample_Normal2), persistence(CrCu_BTC_Model_Sample_Normal3))

persis_BTC_sub <- matrix(data=Persis_sub_BTC, nrow = 3, ncol = 4)
colnames(persis_BTC_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(persis_BTC_sub) <- c("SubSample1", "SubSample2", "SubSample3")
xtable(persis_BTC_sub, caption = "Persistence of Bitcoin SubSamples", digits = 4)

### Store the loglikelihood values, AIC and BIC ####

Likelihood_sub_BTC <- c(CrCu_BTC_Model_Sample_JSU1@fit$LLH, CrCu_BTC_Model_Sample_JSU2@fit$LLH, CrCu_BTC_Model_Sample_JSU3@fit$LLH,
                        CrCu_BTC_Model_Sample_SGED1@fit$LLH, CrCu_BTC_Model_Sample_SGED2@fit$LLH, CrCu_BTC_Model_Sample_SGED3@fit$LLH,
                        CrCu_BTC_Model_Sample_SST1@fit$LLH, CrCu_BTC_Model_Sample_SST2@fit$LLH, CrCu_BTC_Model_Sample_SST3@fit$LLH,
                        CrCu_BTC_Model_Sample_Normal1@fit$LLH, CrCu_BTC_Model_Sample_Normal2@fit$LLH, CrCu_BTC_Model_Sample_Normal3@fit$LLH)

Llhood_sub_BTC <- matrix(data = Likelihood_sub_BTC, nrow = 3, ncol = 4)
colnames(Llhood_sub_BTC) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(Llhood_sub_BTC) <- c("SubSample1", "SubSample2", "SubSample3")

bic_sub_BTC <- c(infocriteria(CrCu_BTC_Model_Sample_JSU1)[2], infocriteria(CrCu_BTC_Model_Sample_JSU2)[2], infocriteria(CrCu_BTC_Model_Sample_JSU3)[2],
                 infocriteria(CrCu_BTC_Model_Sample_SGED1)[2], infocriteria(CrCu_BTC_Model_Sample_SGED2)[2], infocriteria(CrCu_BTC_Model_Sample_SGED3)[2],
                 infocriteria(CrCu_BTC_Model_Sample_SST1)[2], infocriteria(CrCu_BTC_Model_Sample_SST2)[2], infocriteria(CrCu_BTC_Model_Sample_SST3)[2],
                 infocriteria(CrCu_BTC_Model_Sample_Normal1)[2], infocriteria(CrCu_BTC_Model_Sample_Normal2)[2], infocriteria(CrCu_BTC_Model_Sample_Normal3)[2])

bic_BTC_sub <- matrix(data=bic_sub_BTC, nrow = 3, ncol = 4)
colnames(bic_BTC_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(bic_BTC_sub) <- c("SubSample1", "SubSample2", "SubSample3")

aic_sub_BTC <- c(infocriteria(CrCu_BTC_Model_Sample_JSU1)[1], infocriteria(CrCu_BTC_Model_Sample_JSU2)[1], infocriteria(CrCu_BTC_Model_Sample_JSU3)[1],
                 infocriteria(CrCu_BTC_Model_Sample_SGED1)[1], infocriteria(CrCu_BTC_Model_Sample_SGED2)[1], infocriteria(CrCu_BTC_Model_Sample_SGED3)[1],
                 infocriteria(CrCu_BTC_Model_Sample_SST1)[1], infocriteria(CrCu_BTC_Model_Sample_SST2)[1], infocriteria(CrCu_BTC_Model_Sample_SST3)[1],
                 infocriteria(CrCu_BTC_Model_Sample_Normal1)[1], infocriteria(CrCu_BTC_Model_Sample_Normal2)[1], infocriteria(CrCu_BTC_Model_Sample_Normal3)[1])

aic_BTC_sub <- matrix(data=aic_sub_BTC, nrow = 3, ncol = 4)
colnames(aic_BTC_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(aic_BTC_sub) <- c("SubSample1", "SubSample2", "SubSample3")

Infocrit_sub_BTC  <- cbind(Llhood_sub_BTC, bic_BTC_sub, aic_BTC_sub)
colnames(Infocrit_sub_BTC) <- c("Llhood:eGarch-JSU","Llhood:eGarch-SGED","Llhood:eGarch-SST","Llhood:eGarch-Normal", 
                                "BIC:eGarch-JSU","BIC:eGarch-SGED","BIC:eGarch-SST","BIC:eGarch-Normal", 
                                "AIC:eGarch-JSU","AIC:eGarch-SGED","AIC:eGarch-SST","AIC:eGarch-Normal")
Infocrit_sub1_BTC <- t(Infocrit_sub_BTC)
View(data.frame(Infocrit_sub1_BTC))
xtable(Infocrit_sub1_BTC, caption = "Information Criteria for Bitcoin GARCH models - SubSamples", digits = 4)

## coefficients

# Sample 1
coef_BTC_sub1_SGED <- round(coef(CrCu_BTC_Model_Sample_SGED1),4) # SS1 
coef_BTC_sub1_JSU <- round(coef(CrCu_BTC_Model_Sample_JSU1),4) # SS1
coef_BTC_sub1_SST <- round(coef(CrCu_BTC_Model_Sample_SST1),4) # SS1
coef_BTC_sub1_Normal <- round(coef(CrCu_BTC_Model_Sample_Normal1),4) # SS1

# Sample 2
coef_BTC_sub2_SGED <- round(coef(CrCu_BTC_Model_Sample_SGED2),4) # SS2
coef_BTC_sub2_JSU <- round(coef(CrCu_BTC_Model_Sample_JSU2),4) # SS2 
coef_BTC_sub2_SST <- round(coef(CrCu_BTC_Model_Sample_SST2),4) # SS2
coef_BTC_sub2_Normal <- round(coef(CrCu_BTC_Model_Sample_Normal2),4) # SS2

# Sample 3
coef_BTC_sub3_SGED <- round(coef(CrCu_BTC_Model_Sample_SGED3),4) # SS3
coef_BTC_sub3_JSU <- round(coef(CrCu_BTC_Model_Sample_JSU3),4) # SS3
coef_BTC_sub3_SST <- round(coef(CrCu_BTC_Model_Sample_SST3),4) # SS3 
coef_BTC_sub3_Normal <- round(coef(CrCu_BTC_Model_Sample_Normal3),4) # SS3

garch_coef_BTC_fsub <- bind_rows(coef_BTC_7, coef_BTC_sub1_SGED, coef_BTC_sub2_JSU, coef_BTC_sub3_SST)

garch_coef_BTC1_fsub <- as.data.frame(garch_coef_BTC_fsub)
rownames(garch_coef_BTC1_fsub) <- c("FullSample-eGarchJSU","SS1-eGarchSGED","SS2-eGarchJSU","SS3-eGarchSST")
garch_coef_BTC1_fsubt <- (t(garch_coef_BTC1_fsub))
View(t(garch_coef_BTC1_fsub))
xtable(garch_coef_BTC1_fsubt, caption = "Coefficients of Bitcoin GARCH models", digits = 4)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model
# MAPE/RMSE for all five coins is computed once, after all coin scripts, in R/04_garch_mape_rmse.R

#### Store the VaR for the out sample using rolling window forecasts

# Sample 1 
CrCu_BTC_VaR_SGED1   <- CrCu_BTC_Roll_SGED_Sample1@forecast$VaR # SS1 
CrCu_BTC_VaR_JSU1   <- CrCu_BTC_Roll_JSU_Sample1@forecast$VaR # SS1
CrCu_BTC_VaR_SST1   <- CrCu_BTC_Roll_SST_Sample1@forecast$VaR # SS1
CrCu_BTC_VaR_Normal1   <- CrCu_BTC_Roll_Normal_Sample1@forecast$VaR # SS1

# Sample 2
CrCu_BTC_VaR_SGED2   <- CrCu_BTC_Roll_SGED_Sample2@forecast$VaR # SS2 
CrCu_BTC_VaR_JSU2   <- CrCu_BTC_Roll_JSU_Sample2@forecast$VaR # SS2 
CrCu_BTC_VaR_SST2   <- CrCu_BTC_Roll_SST_Sample2@forecast$VaR # SS2
CrCu_BTC_VaR_Normal2   <- CrCu_BTC_Roll_Normal_Sample2@forecast$VaR # SS2

# Sample 3
CrCu_BTC_VaR_SGED3   <- CrCu_BTC_Roll_SGED_Sample3@forecast$VaR # SS3 
CrCu_BTC_VaR_JSU3   <- CrCu_BTC_Roll_JSU_Sample3@forecast$VaR # SS3 
CrCu_BTC_VaR_SST3   <- CrCu_BTC_Roll_SST_Sample3@forecast$VaR # SS3
CrCu_BTC_VaR_Normal3   <- CrCu_BTC_Roll_Normal_Sample3@forecast$VaR # SS3

### Ljung-box test statistic

# Sample 1

CrCu_BTC1_JSU1  <- Box.test(CrCu_BTC_Model_Sample_JSU1@fit$residuals/CrCu_BTC_Model_Sample_JSU1@fit$sigma,lag=1,type="Lj")
CrCu_BTC1_JSU5  <- Box.test(CrCu_BTC_Model_Sample_JSU1@fit$residuals/CrCu_BTC_Model_Sample_JSU1@fit$sigma,lag=5,type="Lj")
CrCu_BTC1_JSU10  <- Box.test(CrCu_BTC_Model_Sample_JSU1@fit$residuals/CrCu_BTC_Model_Sample_JSU1@fit$sigma,lag=10,type="Lj")

CrCu_BTC1_SST1  <- Box.test(CrCu_BTC_Model_Sample_SST1@fit$residuals/CrCu_BTC_Model_Sample_SST1@fit$sigma,lag=1,type="Lj")
CrCu_BTC1_SST5  <- Box.test(CrCu_BTC_Model_Sample_SST1@fit$residuals/CrCu_BTC_Model_Sample_SST1@fit$sigma,lag=5,type="Lj")
CrCu_BTC1_SST10  <- Box.test(CrCu_BTC_Model_Sample_SST1@fit$residuals/CrCu_BTC_Model_Sample_SST1@fit$sigma,lag=10,type="Lj")

CrCu_BTC1_SGED1  <- Box.test(CrCu_BTC_Model_Sample_SGED1@fit$residuals/CrCu_BTC_Model_Sample_SGED1@fit$sigma,lag=1,type="Lj")
CrCu_BTC1_SGED5  <- Box.test(CrCu_BTC_Model_Sample_SGED1@fit$residuals/CrCu_BTC_Model_Sample_SGED1@fit$sigma,lag=5,type="Lj")
CrCu_BTC1_SGED10  <- Box.test(CrCu_BTC_Model_Sample_SGED1@fit$residuals/CrCu_BTC_Model_Sample_SGED1@fit$sigma,lag=10,type="Lj")

CrCu_BTC1_Normal1  <- Box.test(CrCu_BTC_Model_Sample_Normal1@fit$residuals/CrCu_BTC_Model_Sample_Normal1@fit$sigma,lag=1,type="Lj")
CrCu_BTC1_Normal5  <- Box.test(CrCu_BTC_Model_Sample_Normal1@fit$residuals/CrCu_BTC_Model_Sample_Normal1@fit$sigma,lag=5,type="Lj")
CrCu_BTC1_Normal10  <- Box.test(CrCu_BTC_Model_Sample_Normal1@fit$residuals/CrCu_BTC_Model_Sample_Normal1@fit$sigma,lag=10,type="Lj")

CrCu_BTC1_LQ_JSU <- c(CrCu_BTC1_JSU1$statistic,CrCu_BTC1_JSU5$statistic,CrCu_BTC1_JSU10$statistic)
CrCu_BTC1_LQ_SST <- c(CrCu_BTC1_SST1$statistic,CrCu_BTC1_SST5$statistic,CrCu_BTC1_SST10$statistic)
CrCu_BTC1_LQ_SGED <- c(CrCu_BTC1_SGED1$statistic,CrCu_BTC1_SGED5$statistic,CrCu_BTC1_SGED10$statistic)
CrCu_BTC1_LQ_Normal <- c(CrCu_BTC1_Normal1$statistic,CrCu_BTC1_Normal5$statistic,CrCu_BTC1_Normal10$statistic)

CrCu_BTC1_res_stat <-   cbind(CrCu_BTC1_LQ_JSU,CrCu_BTC1_LQ_SST,CrCu_BTC1_LQ_SGED,CrCu_BTC1_LQ_Normal)
rownames(CrCu_BTC1_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_BTC1_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Sample 2

CrCu_BTC2_JSU1  <- Box.test(CrCu_BTC_Model_Sample_JSU2@fit$residuals/CrCu_BTC_Model_Sample_JSU2@fit$sigma,lag=1,type="Lj")
CrCu_BTC2_JSU5  <- Box.test(CrCu_BTC_Model_Sample_JSU2@fit$residuals/CrCu_BTC_Model_Sample_JSU2@fit$sigma,lag=5,type="Lj")
CrCu_BTC2_JSU10  <- Box.test(CrCu_BTC_Model_Sample_JSU2@fit$residuals/CrCu_BTC_Model_Sample_JSU2@fit$sigma,lag=10,type="Lj")

CrCu_BTC2_SST1  <- Box.test(CrCu_BTC_Model_Sample_SST2@fit$residuals/CrCu_BTC_Model_Sample_SST2@fit$sigma,lag=1,type="Lj")
CrCu_BTC2_SST5  <- Box.test(CrCu_BTC_Model_Sample_SST2@fit$residuals/CrCu_BTC_Model_Sample_SST2@fit$sigma,lag=5,type="Lj")
CrCu_BTC2_SST10  <- Box.test(CrCu_BTC_Model_Sample_SST2@fit$residuals/CrCu_BTC_Model_Sample_SST2@fit$sigma,lag=10,type="Lj")

CrCu_BTC2_SGED1  <- Box.test(CrCu_BTC_Model_Sample_SGED2@fit$residuals/CrCu_BTC_Model_Sample_SGED2@fit$sigma,lag=1,type="Lj")
CrCu_BTC2_SGED5  <- Box.test(CrCu_BTC_Model_Sample_SGED2@fit$residuals/CrCu_BTC_Model_Sample_SGED2@fit$sigma,lag=5,type="Lj")
CrCu_BTC2_SGED10  <- Box.test(CrCu_BTC_Model_Sample_SGED2@fit$residuals/CrCu_BTC_Model_Sample_SGED2@fit$sigma,lag=10,type="Lj")

CrCu_BTC2_Normal1  <- Box.test(CrCu_BTC_Model_Sample_Normal2@fit$residuals/CrCu_BTC_Model_Sample_Normal2@fit$sigma,lag=1,type="Lj")
CrCu_BTC2_Normal5  <- Box.test(CrCu_BTC_Model_Sample_Normal2@fit$residuals/CrCu_BTC_Model_Sample_Normal2@fit$sigma,lag=5,type="Lj")
CrCu_BTC2_Normal10  <- Box.test(CrCu_BTC_Model_Sample_Normal2@fit$residuals/CrCu_BTC_Model_Sample_Normal2@fit$sigma,lag=10,type="Lj")

CrCu_BTC2_LQ_JSU <- c(CrCu_BTC2_JSU1$statistic,CrCu_BTC2_JSU5$statistic,CrCu_BTC2_JSU10$statistic)
CrCu_BTC2_LQ_SST <- c(CrCu_BTC2_SST1$statistic,CrCu_BTC2_SST5$statistic,CrCu_BTC2_SST10$statistic)
CrCu_BTC2_LQ_SGED <- c(CrCu_BTC2_SGED1$statistic,CrCu_BTC2_SGED5$statistic,CrCu_BTC2_SGED10$statistic)
CrCu_BTC2_LQ_Normal <- c(CrCu_BTC2_Normal1$statistic,CrCu_BTC2_Normal5$statistic,CrCu_BTC2_Normal10$statistic)

CrCu_BTC2_res_stat <-   cbind(CrCu_BTC2_LQ_JSU,CrCu_BTC2_LQ_SST,CrCu_BTC2_LQ_SGED,CrCu_BTC2_LQ_Normal)
rownames(CrCu_BTC2_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_BTC2_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Sample 3

CrCu_BTC3_JSU1  <- Box.test(CrCu_BTC_Model_Sample_JSU3@fit$residuals/CrCu_BTC_Model_Sample_JSU3@fit$sigma,lag=1,type="Lj")
CrCu_BTC3_JSU5  <- Box.test(CrCu_BTC_Model_Sample_JSU3@fit$residuals/CrCu_BTC_Model_Sample_JSU3@fit$sigma,lag=5,type="Lj")
CrCu_BTC3_JSU10  <- Box.test(CrCu_BTC_Model_Sample_JSU3@fit$residuals/CrCu_BTC_Model_Sample_JSU3@fit$sigma,lag=10,type="Lj")

CrCu_BTC3_SST1  <- Box.test(CrCu_BTC_Model_Sample_SST3@fit$residuals/CrCu_BTC_Model_Sample_SST3@fit$sigma,lag=1,type="Lj")
CrCu_BTC3_SST5  <- Box.test(CrCu_BTC_Model_Sample_SST3@fit$residuals/CrCu_BTC_Model_Sample_SST3@fit$sigma,lag=5,type="Lj")
CrCu_BTC3_SST10  <- Box.test(CrCu_BTC_Model_Sample_SST3@fit$residuals/CrCu_BTC_Model_Sample_SST3@fit$sigma,lag=10,type="Lj")

CrCu_BTC3_SGED1  <- Box.test(CrCu_BTC_Model_Sample_SGED3@fit$residuals/CrCu_BTC_Model_Sample_SGED3@fit$sigma,lag=1,type="Lj")
CrCu_BTC3_SGED5  <- Box.test(CrCu_BTC_Model_Sample_SGED3@fit$residuals/CrCu_BTC_Model_Sample_SGED3@fit$sigma,lag=5,type="Lj")
CrCu_BTC3_SGED10  <- Box.test(CrCu_BTC_Model_Sample_SGED3@fit$residuals/CrCu_BTC_Model_Sample_SGED3@fit$sigma,lag=10,type="Lj")

CrCu_BTC3_Normal1  <- Box.test(CrCu_BTC_Model_Sample_Normal3@fit$residuals/CrCu_BTC_Model_Sample_Normal3@fit$sigma,lag=1,type="Lj")
CrCu_BTC3_Normal5  <- Box.test(CrCu_BTC_Model_Sample_Normal3@fit$residuals/CrCu_BTC_Model_Sample_Normal3@fit$sigma,lag=5,type="Lj")
CrCu_BTC3_Normal10  <- Box.test(CrCu_BTC_Model_Sample_Normal3@fit$residuals/CrCu_BTC_Model_Sample_Normal3@fit$sigma,lag=10,type="Lj")

CrCu_BTC3_LQ_JSU <- c(CrCu_BTC3_JSU1$statistic,CrCu_BTC3_JSU5$statistic,CrCu_BTC3_JSU10$statistic)
CrCu_BTC3_LQ_SST <- c(CrCu_BTC3_SST1$statistic,CrCu_BTC3_SST5$statistic,CrCu_BTC3_SST10$statistic)
CrCu_BTC3_LQ_SGED <- c(CrCu_BTC3_SGED1$statistic,CrCu_BTC3_SGED5$statistic,CrCu_BTC3_SGED10$statistic)
CrCu_BTC3_LQ_Normal <- c(CrCu_BTC3_Normal1$statistic,CrCu_BTC3_Normal5$statistic,CrCu_BTC3_Normal10$statistic)

CrCu_BTC3_res_stat <-   cbind(CrCu_BTC3_LQ_JSU,CrCu_BTC3_LQ_SST,CrCu_BTC3_LQ_SGED,CrCu_BTC3_LQ_Normal)
rownames(CrCu_BTC3_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_BTC3_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

######## ::::::::::::::::: for P E A R S O N  TYPE IV ##########

### Sample Residuals Statistics

BTC_Res_Sample1 <- c(normalTest(CrCu_BTC_Model_Sample_Normal1@fit$residuals/CrCu_BTC_Model_Sample_Normal1@fit$sigma,method="jb")@test$statistic,normalTest(CrCu_BTC_Model_Sample_Normal1@fit$residuals/CrCu_BTC_Model_Sample_Normal1@fit$sigma,method="sw")@test$statistic,normalTest(CrCu_BTC_Model_Sample_Normal1@fit$residuals/CrCu_BTC_Model_Sample_Normal1@fit$sigma,method="ks")@test$statistic)
BTC_Res_Sample2 <- c(normalTest(CrCu_BTC_Model_Sample_Normal2@fit$residuals/CrCu_BTC_Model_Sample_Normal2@fit$sigma,method="jb")@test$statistic,normalTest(CrCu_BTC_Model_Sample_Normal2@fit$residuals/CrCu_BTC_Model_Sample_Normal2@fit$sigma,method="sw")@test$statistic,normalTest(CrCu_BTC_Model_Sample_Normal2@fit$residuals/CrCu_BTC_Model_Sample_Normal2@fit$sigma,method="ks")@test$statistic)
BTC_Res_Sample3 <- c(normalTest(CrCu_BTC_Model_Sample_Normal3@fit$residuals/CrCu_BTC_Model_Sample_Normal3@fit$sigma,method="jb")@test$statistic,normalTest(CrCu_BTC_Model_Sample_Normal3@fit$residuals/CrCu_BTC_Model_Sample_Normal3@fit$sigma,method="sw")@test$statistic,normalTest(CrCu_BTC_Model_Sample_Normal3@fit$residuals/CrCu_BTC_Model_Sample_Normal3@fit$sigma,method="ks")@test$statistic)

#jarqueberaTest(CrCu_BTC_Model_Sample_Normal1@fit$residuals/CrCu_BTC_Model_Sample_Normal1@fit$sigma)

BTC_Res_Sample_Test   <- rbind(BTC_Res_Sample1,BTC_Res_Sample2,BTC_Res_Sample3)
rownames(BTC_Res_Sample_Test) <- c("Sample1","Sample2","Sample3")
colnames(BTC_Res_Sample_Test) <- c("JB","SW","KS") 

### Pearson Specification

BTC_Param_Pearson_Sample1 <- pearsonFitML(CrCu_BTC_Model_Sample_Normal1@fit$residuals/CrCu_BTC_Model_Sample_Normal1@fit$sigma)
BTC_Param_Pearson_Sample2 <- pearsonFitML(CrCu_BTC_Model_Sample_Normal2@fit$residuals/CrCu_BTC_Model_Sample_Normal2@fit$sigma)
BTC_Param_Pearson_Sample3 <- pearsonFitML(CrCu_BTC_Model_Sample_Normal3@fit$residuals/CrCu_BTC_Model_Sample_Normal3@fit$sigma)

BTC_Param_Pearson_Subsample <- rbind(BTC_Param_Pearson_Sample1,BTC_Param_Pearson_Sample2,BTC_Param_Pearson_Sample3)
rownames(BTC_Param_Pearson_Subsample) <- c("Sample1","Sample2","Sample3")

# coefficients for Normal eGarch
coef_BTC_sub1_Normal <- round(coef(CrCu_BTC_Model_Sample_Normal1),4) # SS1
coef_BTC_sub2_Normal <- round(coef(CrCu_BTC_Model_Sample_Normal2),4) # SS2
coef_BTC_sub3_Normal <- round(coef(CrCu_BTC_Model_Sample_Normal3),4) # SS3

#### Repeating for Pearson Distribution  #####

## Store the volatility and mu forecast for Sample 1

Holdout_Sample1_BTC_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample1_BTC_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample1_BTC_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample1_BTC_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample1_BTC_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample1_BTC_Pearson_pvalues) <- c("1%","5%","10%")

Sample1_BTC_Holdout_Mu <- as.data.frame(CrCu_BTC_Roll_Normal_Sample1)[,'Mu']
Sample1_BTC_Holdout_Sigma <- as.data.frame(CrCu_BTC_Roll_Normal_Sample1)[,'Sigma']

for(j in 1:3) {
  
  Z_Pearson_Sample1_BTC   <- qpearson(signific_levels[j],BTC_Param_Pearson_Sample1)
  
  Sample1_BTC_Holdout_VaR   <- Sample1_BTC_Holdout_Mu + Sample1_BTC_Holdout_Sigma*Z_Pearson_Sample1_BTC
  Sample1_BTC_Violations   <- BacktestVaR(Returns_CrCu_BTC[(CrCu_BTC_samples[1]-60+1):(CrCu_BTC_samples[1]-1)],Sample1_BTC_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample1_BTC_Pearson[j,]   <- c(Sample1_BTC_Violations$LRuc[1],Sample1_BTC_Violations$LRcc[1],Sample1_BTC_Violations$DQ$stat,Sample1_BTC_Violations$AE,Sample1_BTC_Violations$Loss$Loss)
  Holdout_Sample1_BTC_Pearson_pvalues[j,] <- c(Sample1_BTC_Violations$LRuc[2],Sample1_BTC_Violations$LRcc[2],Sample1_BTC_Violations$DQ$pvalue)
}

## Store the volatility and mu forecast for Sample 2

Holdout_Sample2_BTC_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample2_BTC_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample2_BTC_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample2_BTC_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample2_BTC_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample2_BTC_Pearson_pvalues) <- c("1%","5%","10%")

Sample2_BTC_Holdout_Mu <- as.data.frame(CrCu_BTC_Roll_Normal_Sample2)[,'Mu']
Sample2_BTC_Holdout_Sigma <- as.data.frame(CrCu_BTC_Roll_Normal_Sample2)[,'Sigma']

for(j in 1:3) {
  
  Z_Pearson_Sample2_BTC   <- qpearson(signific_levels[j],BTC_Param_Pearson_Sample1)
  
  Sample2_BTC_Holdout_VaR   <- Sample2_BTC_Holdout_Mu + Sample2_BTC_Holdout_Sigma*Z_Pearson_Sample2_BTC
  Sample2_BTC_Violations   <- BacktestVaR(Returns_CrCu_BTC[(CrCu_BTC_samples[2]-200+1):(CrCu_BTC_samples[2]-1)],Sample2_BTC_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample2_BTC_Pearson[j,]   <- c(Sample2_BTC_Violations$LRuc[1],Sample2_BTC_Violations$LRcc[1],Sample2_BTC_Violations$DQ$stat,Sample2_BTC_Violations$AE,Sample2_BTC_Violations$Loss$Loss)
  Holdout_Sample2_BTC_Pearson_pvalues[j,] <- c(Sample2_BTC_Violations$LRuc[2],Sample2_BTC_Violations$LRcc[2],Sample2_BTC_Violations$DQ$pvalue)
}

## Store the volatility and mu forecast for Sample 3

Holdout_Sample3_BTC_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample3_BTC_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample3_BTC_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample3_BTC_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample3_BTC_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample3_BTC_Pearson_pvalues) <- c("1%","5%","10%")

Sample3_BTC_Holdout_Mu <- as.data.frame(CrCu_BTC_Roll_Normal_Sample3)[,'Mu']
Sample3_BTC_Holdout_Sigma <- as.data.frame(CrCu_BTC_Roll_Normal_Sample3)[,'Sigma']

for(j in 1:3){
  
  Z_Pearson_Sample3_BTC   <- qpearson(signific_levels[j],BTC_Param_Pearson_Sample3)
  
  Sample3_BTC_Holdout_VaR   <- Sample3_BTC_Holdout_Mu + Sample3_BTC_Holdout_Sigma*Z_Pearson_Sample3_BTC
  Sample3_BTC_Violations   <- BacktestVaR(Returns_CrCu_BTC[(length(Returns_CrCu_BTC)-100+1):(length(Returns_CrCu_BTC)-1)],Sample3_BTC_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample3_BTC_Pearson[j,]   <- c(Sample3_BTC_Violations$LRuc[1],Sample3_BTC_Violations$LRcc[1],Sample3_BTC_Violations$DQ$stat,Sample3_BTC_Violations$AE,Sample3_BTC_Violations$Loss$Loss)
  Holdout_Sample3_BTC_Pearson_pvalues[j,] <- c(Sample3_BTC_Violations$LRuc[2],Sample3_BTC_Violations$LRcc[2],Sample3_BTC_Violations$DQ$pvalue)
}

Holdout_BTC_Pearson_FT <- cbind(Holdout_Sample1_BTC_Pearson[,4], Holdout_Sample2_BTC_Pearson[,4],Holdout_Sample3_BTC_Pearson[,4])
colnames(Holdout_BTC_Pearson_FT) <- c("SubSample1","SubSample2","SubSample3")

## Storing Pearson subsample results ###

BTC_subsample_1pc   <- rbind(Holdout_Sample1_BTC_Pearson[1,],Holdout_Sample2_BTC_Pearson[1,],Holdout_Sample3_BTC_Pearson[1,])

BTC_subsample_5pc   <- rbind(Holdout_Sample1_BTC_Pearson[2,],Holdout_Sample2_BTC_Pearson[2,],Holdout_Sample3_BTC_Pearson[2,])

BTC_subsample_10pc   <- rbind(Holdout_Sample1_BTC_Pearson[3,],Holdout_Sample2_BTC_Pearson[3,],Holdout_Sample3_BTC_Pearson[3,])

BTC_subsample_pvalues_1pc   <- rbind(Holdout_Sample1_BTC_Pearson_pvalues[1,],Holdout_Sample2_BTC_Pearson_pvalues[1,],Holdout_Sample3_BTC_Pearson_pvalues[1,])

BTC_subsample_pvalues_5pc   <- rbind(Holdout_Sample1_BTC_Pearson_pvalues[2,],Holdout_Sample2_BTC_Pearson_pvalues[2,],Holdout_Sample3_BTC_Pearson_pvalues[2,])

BTC_subsample_pvalues_10pc   <- rbind(Holdout_Sample1_BTC_Pearson_pvalues[3,],Holdout_Sample2_BTC_Pearson_pvalues[3,],Holdout_Sample3_BTC_Pearson_pvalues[3,])

## Store the volatility and mu forecast for Sample 1
### Plotting the VaR for subsamples

CrCu_BTC_Sample1_Dates <- format(CrCu_BTC$Crypto_Date[(CrCu_BTC_samples[1]-59):CrCu_BTC_samples[1]],"%b-%y")
CrCu_BTC_Sample2_Dates <- format(CrCu_BTC$Crypto_Date[(CrCu_BTC_samples[2]-199):CrCu_BTC_samples[2]],"%b-%y")
CrCu_BTC_Sample3_Dates <- format(CrCu_BTC$Crypto_Date[(length(CrCu_BTC_xts)-99):length(CrCu_BTC_xts)],"%b-%y")
## For 5%
Z_Pearson_Sample1_BTC   <- qpearson(0.05,BTC_Param_Pearson_Sample1)
Sample1_BTC_Holdout_VaR   <- Sample1_BTC_Holdout_Mu + Sample1_BTC_Holdout_Sigma*Z_Pearson_Sample1_BTC

VR11_BTC <- data.frame(CrCu_BTC_VaR_Normal1)
VR12_BTC <- data.frame(CrCu_BTC_VaR_SST1)
VR13_BTC <- data.frame(CrCu_BTC_VaR_SGED1)
VR14_BTC <- data.frame(CrCu_BTC_VaR_JSU1)
VR15_BTC <- data.frame(Sample1_BTC_Holdout_VaR)
VR_Date1_BTC <- CrCu_BTC$Crypto_Date[(CrCu_BTC_samples[1]-59):CrCu_BTC_samples[1]]

SS1VaR_BTC <- ggplot(data.frame(VR11_BTC[,4]), aes(x= VR_Date1_BTC, y = VR11_BTC[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date1_BTC, y = VR11_BTC[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date1_BTC, y = VR12_BTC[,2], color = "SST")) +
  geom_line(aes(x = VR_Date1_BTC, y = VR13_BTC[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date1_BTC, y = VR14_BTC[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date1_BTC, y = VR15_BTC[,1], color = "PSIV")) +
  xlab ("30 July 2017 to 27 September 2017") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Bitcoin SubSample 1") +
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

ggexport(SS1VaR_BTC, filename = file.path(FIG_DIR, "SS1VaR_BTC.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

# Sub Sample 2

Z_Pearson_Sample2_BTC   <- qpearson(0.05,BTC_Param_Pearson_Sample2)
Sample2_BTC_Holdout_VaR   <- Sample2_BTC_Holdout_Mu + Sample2_BTC_Holdout_Sigma*Z_Pearson_Sample2_BTC

VR21_BTC <- data.frame(CrCu_BTC_VaR_Normal2)
VR22_BTC <- data.frame(CrCu_BTC_VaR_SST2)
VR23_BTC <- data.frame(CrCu_BTC_VaR_SGED2)
VR24_BTC <- data.frame(CrCu_BTC_VaR_JSU2)
VR25_BTC <- data.frame(Sample2_BTC_Holdout_VaR)
VR_Date2_BTC <- CrCu_BTC$Crypto_Date[(CrCu_BTC_samples[2]-199):CrCu_BTC_samples[2]]

SS2VaR_BTC <- ggplot(data.frame(VR21_BTC[,4]), aes(x= VR_Date2_BTC, y = VR21_BTC[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date2_BTC, y = VR21_BTC[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date2_BTC, y = VR22_BTC[,2], color = "SST")) +
  geom_line(aes(x = VR_Date2_BTC, y = VR23_BTC[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date2_BTC, y = VR24_BTC[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date2_BTC, y = VR25_BTC[,1], color = "PSIV")) +
  xlab ("07 December 2019 to 23 June 2020") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Bitcoin SubSample 2") +
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

ggexport(SS2VaR_BTC, filename = file.path(FIG_DIR, "SS2VaR_BTC.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

# Sub Sample 3

Z_Pearson_Sample3_BTC   <- qpearson(0.05,BTC_Param_Pearson_Sample3)
Sample3_BTC_Holdout_VaR   <- Sample3_BTC_Holdout_Mu + Sample3_BTC_Holdout_Sigma*Z_Pearson_Sample3_BTC

VR31_BTC <- data.frame(CrCu_BTC_VaR_Normal3)
VR32_BTC <- data.frame(CrCu_BTC_VaR_SST3)
VR33_BTC <- data.frame(CrCu_BTC_VaR_SGED3)
VR34_BTC <- data.frame(CrCu_BTC_VaR_JSU3)
VR35_BTC <- data.frame(Sample3_BTC_Holdout_VaR)
VR_Date3_BTC <- CrCu_BTC$Crypto_Date[(length(CrCu_BTC_xts)-99):length(CrCu_BTC_xts)]

SS3VaR_BTC <- ggplot(data.frame(VR31_BTC[,4]), aes(x= VR_Date3_BTC, y = VR31_BTC[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date3_BTC, y = VR31_BTC[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date3_BTC, y = VR32_BTC[,2], color = "SST")) +
  geom_line(aes(x = VR_Date3_BTC, y = VR33_BTC[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date3_BTC, y = VR34_BTC[,2], color = "JSU")) +
  geom_line(aes(x = VR_Date3_BTC, y = VR35_BTC[,1], color = "PSIV")) +
  xlab ("26 August 2021 to 04 December 2021") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Bitcoin SubSample 3") +
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

ggexport(SS3VaR_BTC, filename = file.path(FIG_DIR, "SS3VaR_BTC.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

#Full Data

Z_Pearson_BTC   <- qpearson(0.05,CrCu_BTC_Dist8)
BTC_Holdout_VaR   <- BTC_Holdout_Mu + BTC_Holdout_Sigma*Z_Pearson_BTC

VR1_BTC <- data.frame(CrCu_BTC_Roll8@forecast$VaR)
VR2_BTC <- data.frame(CrCu_BTC_Roll5@forecast$VaR)
VR3_BTC <- data.frame(CrCu_BTC_Roll6@forecast$VaR)
VR4_BTC <- data.frame(CrCu_BTC_Roll7@forecast$VaR)
VR5_BTC <- data.frame(BTC_Holdout_VaR)
VR_Date_BTC <- CrCu_BTC$Crypto_Date[(length(CrCu_BTC_xts)-349):length(CrCu_BTC_xts)]

FSVaR_BTC <- ggplot(data.frame(VR1_BTC[,4]), aes(x= VR_Date_BTC, y = VR1_BTC[,4])) +
  geom_line(aes(colour = "Return"))  +
  geom_line(aes(x = VR_Date_BTC, y = VR1_BTC[,2], colour = "Norm")) +
  geom_line(aes(x = VR_Date_BTC, y = VR2_BTC[,2], color = "SST")) +
  geom_line(aes(x = VR_Date_BTC, y = VR3_BTC[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date_BTC, y = VR4_BTC[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date_BTC, y = VR5_BTC[,1], color = "PS")) +
  xlab ("19 Dec 2020 to 04 Dec 2021") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Bitcoin") +
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

ggexport(FSVaR_BTC, filename = file.path(FIG_DIR, "FSVaR_BTC.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

# Sample 1

Z_Pearson_Sample1_BTC_01   <- qpearson(0.01,BTC_Param_Pearson_Sample1)
Sample1_BTC_Holdout_VaR_01   <- Sample1_BTC_Holdout_Mu + Sample1_BTC_Holdout_Sigma*Z_Pearson_Sample1_BTC_01
Z_Pearson_Sample1_BTC_05   <- qpearson(0.05,BTC_Param_Pearson_Sample1)
Sample1_BTC_Holdout_VaR_05   <- Sample1_BTC_Holdout_Mu + Sample1_BTC_Holdout_Sigma*Z_Pearson_Sample1_BTC_05
Z_Pearson_Sample1_BTC_10   <- qpearson(0.10,BTC_Param_Pearson_Sample1)
Sample1_BTC_Holdout_VaR_10   <- Sample1_BTC_Holdout_Mu + Sample1_BTC_Holdout_Sigma*Z_Pearson_Sample1_BTC_10

# Sample 2

Z_Pearson_Sample2_BTC_01   <- qpearson(0.01,BTC_Param_Pearson_Sample2)
Sample2_BTC_Holdout_VaR_01   <- Sample2_BTC_Holdout_Mu + Sample2_BTC_Holdout_Sigma*Z_Pearson_Sample2_BTC_01
Z_Pearson_Sample2_BTC_05   <- qpearson(0.05,BTC_Param_Pearson_Sample2)
Sample2_BTC_Holdout_VaR_05   <- Sample2_BTC_Holdout_Mu + Sample2_BTC_Holdout_Sigma*Z_Pearson_Sample2_BTC_05
Z_Pearson_Sample2_BTC_10   <- qpearson(0.10,BTC_Param_Pearson_Sample2)
Sample2_BTC_Holdout_VaR_10   <- Sample2_BTC_Holdout_Mu + Sample2_BTC_Holdout_Sigma*Z_Pearson_Sample2_BTC_10

# Sample 3

Z_Pearson_Sample3_BTC_01   <- qpearson(0.01,BTC_Param_Pearson_Sample3)
Sample3_BTC_Holdout_VaR_01   <- Sample3_BTC_Holdout_Mu + Sample3_BTC_Holdout_Sigma*Z_Pearson_Sample3_BTC_01
Z_Pearson_Sample3_BTC_05   <- qpearson(0.05,BTC_Param_Pearson_Sample3)
Sample3_BTC_Holdout_VaR_05   <- Sample3_BTC_Holdout_Mu + Sample3_BTC_Holdout_Sigma*Z_Pearson_Sample3_BTC_05
Z_Pearson_Sample3_BTC_10   <- qpearson(0.10,BTC_Param_Pearson_Sample3)
Sample3_BTC_Holdout_VaR_10   <- Sample3_BTC_Holdout_Mu + Sample3_BTC_Holdout_Sigma*Z_Pearson_Sample3_BTC_10

#### Backtesting and Storing the results

# Sample 1
library(GAS)
BTC_Test_SST1_Sample1 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,1],0.01)
BTC_Test_SST5_Sample1 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,2],0.05)
BTC_Test_SST10_Sample1 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,3],0.1)

BTC_Test_SGED1_Sample1 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,1],0.01)
BTC_Test_SGED5_Sample1 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,2],0.05)
BTC_Test_SGED10_Sample1 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,3],0.1)

BTC_Test_JSU1_Sample1 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,1],0.01)
BTC_Test_JSU5_Sample1 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,2],0.05)
BTC_Test_JSU10_Sample1 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,3],0.1)

BTC_Test_Normal1_Sample1 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,1],0.01)
BTC_Test_Normal5_Sample1 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,2],0.05)
BTC_Test_Normal10_Sample1 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,3],0.1)

##### Sample 2

BTC_Test_SST1_Sample2 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,1],0.01)
BTC_Test_SST5_Sample2 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,2],0.05)
BTC_Test_SST10_Sample2 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,3],0.1)

BTC_Test_SGED1_Sample2 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,1],0.01)
BTC_Test_SGED5_Sample2 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,2],0.05)
BTC_Test_SGED10_Sample2 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,3],0.1)

BTC_Test_JSU1_Sample2 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,1],0.01)
BTC_Test_JSU5_Sample2 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,2],0.05)
BTC_Test_JSU10_Sample2 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,3],0.1)

BTC_Test_Normal1_Sample2 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,1],0.01)
BTC_Test_Normal5_Sample2 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,2],0.05)
BTC_Test_Normal10_Sample2 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,3],0.1)

### Sample3

BTC_Test_SST1_Sample3 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,1],0.01)
BTC_Test_SST5_Sample3 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,2],0.05)
BTC_Test_SST10_Sample3 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,3],0.1)

BTC_Test_SGED1_Sample3 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,1],0.01)
BTC_Test_SGED5_Sample3 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,2],0.05)
BTC_Test_SGED10_Sample3 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,3],0.1)

BTC_Test_JSU1_Sample3 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,1],0.01)
BTC_Test_JSU5_Sample3 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,2],0.05)
BTC_Test_JSU10_Sample3 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,3],0.1)

BTC_Test_Normal1_Sample3 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,1],0.01)
BTC_Test_Normal5_Sample3 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,2],0.05)
BTC_Test_Normal10_Sample3 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,3],0.1)

BTC_Sample1 <- matrix(c(BTC_Test_Normal1_Sample1$LRuc[1],BTC_Test_SST1_Sample1$LRuc[1],BTC_Test_SGED1_Sample1$LRuc[1],BTC_Test_JSU1_Sample1$LRuc[1],BTC_Test_PSIV1_Sample1$LRuc[1],
                        BTC_Test_Normal1_Sample1$LRcc[1],BTC_Test_SST1_Sample1$LRcc[1],BTC_Test_SGED1_Sample1$LRcc[1],BTC_Test_JSU1_Sample1$LRcc[1],BTC_Test_PSIV1_Sample1$LRcc[1],
                        BTC_Test_Normal1_Sample1$DQ$stat,BTC_Test_SST1_Sample1$DQ$stat,BTC_Test_SGED1_Sample1$DQ$stat,BTC_Test_JSU1_Sample1$DQ$stat,BTC_Test_PSIV1_Sample1$DQ$stat,
                        BTC_Test_Normal1_Sample1$AE,BTC_Test_SST1_Sample1$AE,BTC_Test_SGED1_Sample1$AE,BTC_Test_JSU1_Sample1$AE,BTC_Test_PSIV1_Sample1$AE,
                        (BTC_Test_Normal1_Sample1$Loss$Loss/BTC_Test_Normal1_Sample1$Loss$Loss),(BTC_Test_SST1_Sample1$Loss$Loss/BTC_Test_Normal1_Sample1$Loss$Loss),
                        (BTC_Test_SGED1_Sample1$Loss$Loss/BTC_Test_Normal1_Sample1$Loss$Loss),(BTC_Test_JSU1_Sample1$Loss$Loss/BTC_Test_Normal1_Sample1$Loss$Loss),(BTC_Test_PSIV1_Sample1$Loss$Loss/BTC_Test_Normal1_Sample1$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(BTC_Sample1) <- c("Normal","SST","SGED","JSU","PSIV")    
rownames(BTC_Sample1) <- c("UC","CC","DQ","AE","QL")    

BTC_Sample2 <- matrix(c(BTC_Test_Normal1_Sample2$LRuc[1],BTC_Test_SST1_Sample2$LRuc[1],BTC_Test_SGED1_Sample2$LRuc[1],BTC_Test_JSU1_Sample2$LRuc[1],
                        BTC_Test_Normal1_Sample2$LRcc[1],BTC_Test_SST1_Sample2$LRcc[1],BTC_Test_SGED1_Sample2$LRcc[1],BTC_Test_JSU1_Sample2$LRcc[1],
                        BTC_Test_Normal1_Sample2$DQ$stat,BTC_Test_SST1_Sample2$DQ$stat,BTC_Test_SGED1_Sample2$DQ$stat,BTC_Test_JSU1_Sample2$DQ$stat,
                        BTC_Test_Normal1_Sample2$AE,BTC_Test_SST1_Sample2$AE,BTC_Test_SGED1_Sample2$AE,BTC_Test_JSU1_Sample2$AE,
                        (BTC_Test_Normal1_Sample2$Loss$Loss/BTC_Test_Normal1_Sample2$Loss$Loss),(BTC_Test_SST1_Sample2$Loss$Loss/BTC_Test_Normal1_Sample2$Loss$Loss),
                        (BTC_Test_SGED1_Sample2$Loss$Loss/BTC_Test_Normal1_Sample2$Loss$Loss),(BTC_Test_JSU1_Sample2$Loss$Loss/BTC_Test_Normal1_Sample2$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(BTC_Sample2) <- c("Normal","SST","SGED","JSU")    
rownames(BTC_Sample2) <- c("UC","CC","DQ","AE","QL")    

BTC_Sample3 <- matrix(c(BTC_Test_Normal1_Sample3$LRuc[1],BTC_Test_SST1_Sample3$LRuc[1],BTC_Test_SGED1_Sample3$LRuc[1],BTC_Test_JSU1_Sample3$LRuc[1],
                        BTC_Test_Normal1_Sample3$LRcc[1],BTC_Test_SST1_Sample3$LRcc[1],BTC_Test_SGED1_Sample3$LRcc[1],BTC_Test_JSU1_Sample3$LRcc[1],
                        BTC_Test_Normal1_Sample3$DQ$stat,BTC_Test_SST1_Sample3$DQ$stat,BTC_Test_SGED1_Sample3$DQ$stat,BTC_Test_JSU1_Sample3$DQ$stat,
                        BTC_Test_Normal1_Sample3$AE,BTC_Test_SST1_Sample3$AE,BTC_Test_SGED1_Sample3$AE,BTC_Test_JSU1_Sample3$AE,
                        (BTC_Test_Normal1_Sample3$Loss$Loss/BTC_Test_Normal1_Sample3$Loss$Loss),(BTC_Test_SST1_Sample3$Loss$Loss/BTC_Test_Normal1_Sample3$Loss$Loss),
                        (BTC_Test_SGED1_Sample3$Loss$Loss/BTC_Test_Normal1_Sample3$Loss$Loss),(BTC_Test_JSU1_Sample3$Loss$Loss/BTC_Test_Normal1_Sample3$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(BTC_Sample3) <- c("Normal","SST","SGED","JSU")    
rownames(BTC_Sample3) <- c("UC","CC","DQ","AE","QL")    

### Storing the p-values for subsamples 

# 1 percent

BTC_pvalues_1pc_Sample1 <- matrix(c(BTC_Test_Normal1_Sample1$LRuc[2],BTC_Test_SST1_Sample1$LRuc[2],BTC_Test_SGED1_Sample1$LRuc[2],BTC_Test_JSU1_Sample1$LRuc[2],
                                    BTC_Test_Normal1_Sample1$LRcc[2],BTC_Test_SST1_Sample1$LRcc[2],BTC_Test_SGED1_Sample1$LRcc[2],BTC_Test_JSU1_Sample1$LRcc[2],
                                    BTC_Test_Normal1_Sample1$DQ$pvalue,BTC_Test_SST1_Sample1$DQ$pvalue,BTC_Test_SGED1_Sample1$DQ$pvalue,BTC_Test_JSU1_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

BTC_pvalues_1pc_Sample2 <- matrix(c(BTC_Test_Normal1_Sample2$LRuc[2],BTC_Test_SST1_Sample2$LRuc[2],BTC_Test_SGED1_Sample2$LRuc[2],BTC_Test_JSU1_Sample2$LRuc[2],
                                    BTC_Test_Normal1_Sample2$LRcc[2],BTC_Test_SST1_Sample2$LRcc[2],BTC_Test_SGED1_Sample2$LRcc[2],BTC_Test_JSU1_Sample2$LRcc[2],
                                    BTC_Test_Normal1_Sample2$DQ$pvalue,BTC_Test_SST1_Sample2$DQ$pvalue,BTC_Test_SGED1_Sample2$DQ$pvalue,BTC_Test_JSU1_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

BTC_pvalues_1pc_Sample3 <- matrix(c(BTC_Test_Normal1_Sample3$LRuc[2],BTC_Test_SST1_Sample3$LRuc[2],BTC_Test_SGED1_Sample3$LRuc[2],BTC_Test_JSU1_Sample3$LRuc[2],
                                    BTC_Test_Normal1_Sample3$LRcc[2],BTC_Test_SST1_Sample3$LRcc[2],BTC_Test_SGED1_Sample3$LRcc[2],BTC_Test_JSU1_Sample3$LRcc[2],
                                    BTC_Test_Normal1_Sample3$DQ$pvalue,BTC_Test_SST1_Sample3$DQ$pvalue,BTC_Test_SGED1_Sample3$DQ$pvalue,BTC_Test_JSU1_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

# 5 percent

BTC_pvalues_5pc_Sample1 <- matrix(c(BTC_Test_Normal5_Sample1$LRuc[2],BTC_Test_SST5_Sample1$LRuc[2],BTC_Test_SGED5_Sample1$LRuc[2],BTC_Test_JSU5_Sample1$LRuc[2],
                                    BTC_Test_Normal5_Sample1$LRcc[2],BTC_Test_SST5_Sample1$LRcc[2],BTC_Test_SGED5_Sample1$LRcc[2],BTC_Test_JSU5_Sample1$LRcc[2],
                                    BTC_Test_Normal5_Sample1$DQ$pvalue,BTC_Test_SST5_Sample1$DQ$pvalue,BTC_Test_SGED5_Sample1$DQ$pvalue,BTC_Test_JSU5_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

BTC_pvalues_5pc_Sample2 <- matrix(c(BTC_Test_Normal5_Sample2$LRuc[2],BTC_Test_SST5_Sample2$LRuc[2],BTC_Test_SGED5_Sample2$LRuc[2],BTC_Test_JSU5_Sample2$LRuc[2],
                                    BTC_Test_Normal5_Sample2$LRcc[2],BTC_Test_SST5_Sample2$LRcc[2],BTC_Test_SGED5_Sample2$LRcc[2],BTC_Test_JSU5_Sample2$LRcc[2],
                                    BTC_Test_Normal5_Sample2$DQ$pvalue,BTC_Test_SST5_Sample2$DQ$pvalue,BTC_Test_SGED5_Sample2$DQ$pvalue,BTC_Test_JSU5_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

BTC_pvalues_5pc_Sample3 <- matrix(c(BTC_Test_Normal5_Sample3$LRuc[2],BTC_Test_SST5_Sample3$LRuc[2],BTC_Test_SGED5_Sample3$LRuc[2],BTC_Test_JSU5_Sample3$LRuc[2],
                                    BTC_Test_Normal5_Sample3$LRcc[2],BTC_Test_SST5_Sample3$LRcc[2],BTC_Test_SGED5_Sample3$LRcc[2],BTC_Test_JSU5_Sample3$LRcc[2],
                                    BTC_Test_Normal5_Sample3$DQ$pvalue,BTC_Test_SST5_Sample3$DQ$pvalue,BTC_Test_SGED5_Sample3$DQ$pvalue,BTC_Test_JSU5_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

# 10 percent

BTC_pvalues_10pc_Sample1 <- matrix(c(BTC_Test_Normal10_Sample1$LRuc[2],BTC_Test_SST10_Sample1$LRuc[2],BTC_Test_SGED10_Sample1$LRuc[2],BTC_Test_JSU10_Sample1$LRuc[2],
                                     BTC_Test_Normal10_Sample1$LRcc[2],BTC_Test_SST10_Sample1$LRcc[2],BTC_Test_SGED10_Sample1$LRcc[2],BTC_Test_JSU10_Sample1$LRcc[2],
                                     BTC_Test_Normal10_Sample1$DQ$pvalue,BTC_Test_SST10_Sample1$DQ$pvalue,BTC_Test_SGED10_Sample1$DQ$pvalue,BTC_Test_JSU10_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

BTC_pvalues_10pc_Sample2 <- matrix(c(BTC_Test_Normal10_Sample2$LRuc[2],BTC_Test_SST10_Sample2$LRuc[2],BTC_Test_SGED10_Sample2$LRuc[2],BTC_Test_JSU10_Sample2$LRuc[2],
                                     BTC_Test_Normal10_Sample2$LRcc[2],BTC_Test_SST10_Sample2$LRcc[2],BTC_Test_SGED10_Sample2$LRcc[2],BTC_Test_JSU10_Sample2$LRcc[2],
                                     BTC_Test_Normal10_Sample2$DQ$pvalue,BTC_Test_SST10_Sample2$DQ$pvalue,BTC_Test_SGED10_Sample2$DQ$pvalue,BTC_Test_JSU10_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

BTC_pvalues_10pc_Sample3 <- matrix(c(BTC_Test_Normal10_Sample3$LRuc[2],BTC_Test_SST10_Sample3$LRuc[2],BTC_Test_SGED10_Sample3$LRuc[2],BTC_Test_JSU10_Sample3$LRuc[2],
                                     BTC_Test_Normal10_Sample3$LRcc[2],BTC_Test_SST10_Sample3$LRcc[2],BTC_Test_SGED10_Sample3$LRcc[2],BTC_Test_JSU10_Sample3$LRcc[2],
                                     BTC_Test_Normal10_Sample3$DQ$pvalue,BTC_Test_SST10_Sample3$DQ$pvalue,BTC_Test_SGED10_Sample3$DQ$pvalue,BTC_Test_JSU10_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

BTC_pvalues_1pc   <- rbind(BTC_pvalues_1pc_Sample1,BTC_pvalues_1pc_Sample2,BTC_pvalues_1pc_Sample3)
BTC_pvalues_5pc   <- rbind(BTC_pvalues_5pc_Sample1,BTC_pvalues_5pc_Sample2,BTC_pvalues_5pc_Sample3)
BTC_pvalues_10pc  <- rbind(BTC_pvalues_10pc_Sample1,BTC_pvalues_10pc_Sample2,BTC_pvalues_10pc_Sample3)

rownames(BTC_pvalues_1pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(BTC_pvalues_1pc) <- c("Normal","SST","SGED","JSU")

rownames(BTC_pvalues_5pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(BTC_pvalues_5pc) <- c("Normal","SST","SGED","JSU")

rownames(BTC_pvalues_10pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(BTC_pvalues_10pc) <- c("Normal","SST","SGED","JSU")

### Using VaRTest and VaRDuration Test individually ###
## VaRTest in R

# Sub sample 1

CrCu_BTC_SGED1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,1])
CrCu_BTC_SGED1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,2])
CrCu_BTC_SGED1_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,3])

CrCu_BTC_JSU1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_JSU1[,4],CrCu_BTC_VaR_JSU1[,1])
CrCu_BTC_JSU1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_JSU1[,4],CrCu_BTC_VaR_JSU1[,2])
CrCu_BTC_JSU1_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_JSU1[,4],CrCu_BTC_VaR_JSU1[,3])

CrCu_BTC_SGED1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,1])
CrCu_BTC_SGED1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,2])
CrCu_BTC_SGED1_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,3])

CrCu_BTC_SGED1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,1])
CrCu_BTC_SGED1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,2])
CrCu_BTC_SGED1_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,3])

CrCu_BTC_JSU2_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_JSU2[,4],CrCu_BTC_VaR_JSU2[,1])
CrCu_BTC_JSU2_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_JSU2[,4],CrCu_BTC_VaR_JSU2[,2])
CrCu_BTC_JSU2_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_JSU2[,4],CrCu_BTC_VaR_JSU2[,3])

CrCu_BTC_SST3_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SST3[,4],CrCu_BTC_VaR_SST3[,1])
CrCu_BTC_SST3_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SST3[,4],CrCu_BTC_VaR_SST3[,2])
CrCu_BTC_SST3_VaR_Test10 <- VaRTest(0.1,CrCu_BTC_VaR_SST3[,4],CrCu_BTC_VaR_SST3[,3])

CrCu_BTC_Normal1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_Normal1[,4],CrCu_BTC_VaR_Normal1[,1])

CrCu_BTC_SGED1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,1])
CrCu_BTC_SST1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SST1[,4],CrCu_BTC_VaR_SST1[,1])
CrCu_BTC_JSU1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_JSU1[,4],CrCu_BTC_VaR_JSU1[,1])
CrCu_BTC_Normal1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_Normal1[,4],CrCu_BTC_VaR_Normal1[,1])
CrCu_BTC_PSIV1_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED1[,4],Sample1_BTC_Holdout_VaR_01)

CrCu_BTC_SGED1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SGED1[,4],CrCu_BTC_VaR_SGED1[,2])
CrCu_BTC_SST1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SST1[,4],CrCu_BTC_VaR_SST1[,2])
CrCu_BTC_JSU1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_JSU1[,4],CrCu_BTC_VaR_JSU1[,2])
CrCu_BTC_Normal1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_Normal1[,4],CrCu_BTC_VaR_Normal1[,2])
CrCu_BTC_PSIV1_VaR_Test5  <- VaRTest(0.05,CrCu_BTC_VaR_SGED1[,4],Sample1_BTC_Holdout_VaR_05)

CrCu_BTC_SGED2_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED2[,4],CrCu_BTC_VaR_SGED2[,1])
CrCu_BTC_SST2_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SST2[,4],CrCu_BTC_VaR_SST2[,1])
CrCu_BTC_JSU2_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_JSU2[,4],CrCu_BTC_VaR_JSU2[,1])
CrCu_BTC_Normal2_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_Normal2[,4],CrCu_BTC_VaR_Normal2[,1])
CrCu_BTC_PSIV2_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED2[,4],Sample2_BTC_Holdout_VaR_01)

CrCu_BTC_SGED3_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED3[,4],CrCu_BTC_VaR_SGED3[,1])
CrCu_BTC_SST3_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SST3[,4],CrCu_BTC_VaR_SST3[,1])
CrCu_BTC_JSU3_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_JSU3[,4],CrCu_BTC_VaR_JSU3[,1])
CrCu_BTC_Normal3_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_Normal3[,4],CrCu_BTC_VaR_Normal3[,1])
CrCu_BTC_PSIV3_VaR_Test1  <- VaRTest(0.01,CrCu_BTC_VaR_SGED3[,4],Sample3_BTC_Holdout_VaR_01)

## Store the pvalues at a place

CrCu_BTC_VaR_Test_pvalues1 <- matrix(c(CrCu_BTC_SST_VaR_Test1$uc.LRp,CrCu_BTC_SST_VaR_Test1$cc.LRp,
                                       CrCu_BTC_SGED_VaR_Test1$uc.LRp,CrCu_BTC_SGED_VaR_Test1$cc.LRp,
                                       CrCu_BTC_JSU_VaR_Test1$uc.LRp,CrCu_BTC_JSU_VaR_Test1$cc.LRp,
                                       CrCu_BTC_Norm_VaR_Test1$uc.LRp,CrCu_BTC_Norm_VaR_Test1$cc.LRp),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_pvalues5 <- matrix(c(CrCu_BTC_SST_VaR_Test5$uc.LRp,CrCu_BTC_SST_VaR_Test5$cc.LRp,
                                       CrCu_BTC_SGED_VaR_Test5$uc.LRp,CrCu_BTC_SGED_VaR_Test5$cc.LRp,
                                       CrCu_BTC_JSU_VaR_Test5$uc.LRp,CrCu_BTC_JSU_VaR_Test5$cc.LRp,
                                       CrCu_BTC_Norm_VaR_Test5$uc.LRp,CrCu_BTC_Norm_VaR_Test5$cc.LRp),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_pvalues10 <- matrix(c(CrCu_BTC_SST_VaR_Test10$uc.LRp,CrCu_BTC_SST_VaR_Test10$cc.LRp,
                                        CrCu_BTC_SGED_VaR_Test10$uc.LRp,CrCu_BTC_SGED_VaR_Test10$cc.LRp,
                                        CrCu_BTC_JSU_VaR_Test10$uc.LRp,CrCu_BTC_JSU_VaR_Test10$cc.LRp,
                                        CrCu_BTC_Norm_VaR_Test10$uc.LRp,CrCu_BTC_Norm_VaR_Test10$cc.LRp),ncol=2,byrow = TRUE)

## Storing the test stats

CrCu_BTC_VaR_Test_teststat1 <- matrix(c(CrCu_BTC_SST_VaR_Test1$uc.LRstat,CrCu_BTC_SST_VaR_Test1$cc.LRstat,
                                        CrCu_BTC_SGED_VaR_Test1$uc.LRstat,CrCu_BTC_SGED_VaR_Test1$cc.LRstat,
                                        CrCu_BTC_JSU_VaR_Test1$uc.LRstat,CrCu_BTC_JSU_VaR_Test1$cc.LRstat,
                                        CrCu_BTC_Norm_VaR_Test1$uc.LRstat,CrCu_BTC_Norm_VaR_Test1$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_teststat5 <- matrix(c(CrCu_BTC_SST_VaR_Test5$uc.LRstat,CrCu_BTC_SST_VaR_Test5$cc.LRstat,
                                        CrCu_BTC_SGED_VaR_Test5$uc.LRstat,CrCu_BTC_SGED_VaR_Test5$cc.LRstat,
                                        CrCu_BTC_JSU_VaR_Test5$uc.LRstat,CrCu_BTC_JSU_VaR_Test5$cc.LRstat,
                                        CrCu_BTC_Norm_VaR_Test5$uc.LRstat,CrCu_BTC_Norm_VaR_Test5$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_teststat10 <- matrix(c(CrCu_BTC_SST_VaR_Test10$uc.LRstat,CrCu_BTC_SST_VaR_Test10$cc.LRstat,
                                         CrCu_BTC_SGED_VaR_Test10$uc.LRstat,CrCu_BTC_SGED_VaR_Test10$cc.LRstat,
                                         CrCu_BTC_JSU_VaR_Test10$uc.LRstat,CrCu_BTC_JSU_VaR_Test10$cc.LRstat,
                                         CrCu_BTC_Norm_VaR_Test10$uc.LRstat,CrCu_BTC_Norm_VaR_Test10$cc.LRstat),ncol=2,byrow = TRUE)
## Round them up to 4 decimals

CrCu_BTC_VaR_Test_pvalues1  <- round(CrCu_BTC_VaR_Test_pvalues1,4)
CrCu_BTC_VaR_Test_pvalues5  <- round(CrCu_BTC_VaR_Test_pvalues5,4)
CrCu_BTC_VaR_Test_pvalues10 <- round(CrCu_BTC_VaR_Test_pvalues10,4)

CrCu_BTC_VaR_Test_teststat1  <- round(CrCu_BTC_VaR_Test_teststat1,4)
CrCu_BTC_VaR_Test_teststat5  <- round(CrCu_BTC_VaR_Test_teststat5,4)
CrCu_BTC_VaR_Test_teststat10 <- round(CrCu_BTC_VaR_Test_teststat10,4)

rownames(CrCu_BTC_VaR_Test_pvalues1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_pvalues1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_BTC_VaR_Test_pvalues5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_pvalues5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_BTC_VaR_Test_pvalues10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_pvalues10)  <- c("Kupiec","Christoffersen")

rownames(CrCu_BTC_VaR_Test_teststat1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_teststat1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_BTC_VaR_Test_teststat5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_teststat5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_BTC_VaR_Test_teststat10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_BTC_VaR_Test_teststat10)  <- c("Kupiec","Christoffersen")
# Kupiec H0: observed failure rate is equal to the failure suggested by tests
# Christoffersen H0: Exceedances are not clustering or no consecutive exceedances
# Fail to reject H0 is ideal
# Based on above, H0 is rejected only in case of Normal 1%, so good results

#### VaRDuration Test for subsamples

CrCu_BTC_Dur_Normal_Sample1 <- VaRDurTest(0.10,CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,3])
CrCu_BTC_Dur_Normal_Sample2 <- VaRDurTest(0.10,CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,3])
CrCu_BTC_Dur_Normal_Sample3 <- VaRDurTest(0.10,CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,3])

CrCu_BTC_Dur_SST_Sample1 <- VaRDurTest(0.10,CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,3])
CrCu_BTC_Dur_SST_Sample2 <- VaRDurTest(0.10,CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,3])
CrCu_BTC_Dur_SST_Sample3 <- VaRDurTest(0.10,CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,3])

CrCu_BTC_Dur_SGED_Sample1 <- VaRDurTest(0.10,CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,3])
CrCu_BTC_Dur_SGED_Sample2 <- VaRDurTest(0.10,CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,3])
CrCu_BTC_Dur_SGED_Sample3 <- VaRDurTest(0.10,CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,3])

CrCu_BTC_Dur_JSU_Sample1 <- VaRDurTest(0.10,CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,3])
CrCu_BTC_Dur_JSU_Sample2 <- VaRDurTest(0.10,CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,3])
CrCu_BTC_Dur_JSU_Sample3 <- VaRDurTest(0.10,CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,3])

## Store the results

CrCu_BTC_Dur_Subsample_stat <- matrix(c(CrCu_BTC_Dur_Normal_Sample1$rLL,
                                        CrCu_BTC_Dur_SST_Sample1$rLL,
                                        CrCu_BTC_Dur_SGED_Sample1$rLL,
                                        CrCu_BTC_Dur_JSU_Sample1$rLL,
                                        CrCu_BTC_Dur_Normal_Sample2$rLL,
                                        CrCu_BTC_Dur_SST_Sample2$rLL,
                                        CrCu_BTC_Dur_SGED_Sample2$rLL,
                                        CrCu_BTC_Dur_JSU_Sample2$rLL,
                                        CrCu_BTC_Dur_Normal_Sample3$rLL,
                                        CrCu_BTC_Dur_SST_Sample3$rLL,
                                        CrCu_BTC_Dur_SGED_Sample3$rLL,
                                        CrCu_BTC_Dur_JSU_Sample3$rLL),nrow=4)

CrCu_BTC_Dur_Subsample_pvalues <- matrix(c(CrCu_BTC_Dur_Normal_Sample1$LRp,
                                           CrCu_BTC_Dur_SST_Sample1$LRp,
                                           CrCu_BTC_Dur_SGED_Sample1$LRp,
                                           CrCu_BTC_Dur_JSU_Sample1$LRp,
                                           CrCu_BTC_Dur_Normal_Sample2$LRp,
                                           CrCu_BTC_Dur_SST_Sample2$LRp,
                                           CrCu_BTC_Dur_SGED_Sample2$LRp,
                                           CrCu_BTC_Dur_JSU_Sample2$LRp,
                                           CrCu_BTC_Dur_Normal_Sample3$LRp,
                                           CrCu_BTC_Dur_SST_Sample3$LRp,
                                           CrCu_BTC_Dur_SGED_Sample3$LRp,
                                           CrCu_BTC_Dur_JSU_Sample3$LRp),nrow=4)

### Expected Shortfall Calculations ####

## Assuming Normal Distribution

CrCu_BTC_index_normal1 <- which(CrCu_BTC_Roll8@forecast$VaR[,1]>CrCu_BTC_Roll8@forecast$VaR[,4])
CrCu_BTC_VaR_values_normal1 <- CrCu_BTC_Roll8@forecast$VaR[CrCu_BTC_index_normal1,1]
CrCu_BTC_return_values_normal1 <- CrCu_BTC_Roll8@forecast$VaR[CrCu_BTC_index_normal1,4]
CrCu_BTC_ES_normal1 <- (CrCu_BTC_return_values_normal1/CrCu_BTC_VaR_values_normal1)
CrCu_BTC_ES_Measure1_normal1 <- mean(CrCu_BTC_ES_normal1)
CrCu_BTC_ES_Measure2_normal1 <- max(CrCu_BTC_ES_normal1)

CrCu_BTC_index_normal5 <- which(CrCu_BTC_Roll8@forecast$VaR[,2]>CrCu_BTC_Roll8@forecast$VaR[,4])
CrCu_BTC_VaR_values_normal5 <- CrCu_BTC_Roll8@forecast$VaR[CrCu_BTC_index_normal5,2]
CrCu_BTC_return_values_normal5 <- CrCu_BTC_Roll8@forecast$VaR[CrCu_BTC_index_normal5,4]
CrCu_BTC_ES_normal5 <- (CrCu_BTC_return_values_normal5/CrCu_BTC_VaR_values_normal5)
CrCu_BTC_ES_Measure1_normal5 <- mean(CrCu_BTC_ES_normal5)
CrCu_BTC_ES_Measure2_normal5 <- max(CrCu_BTC_ES_normal5)

CrCu_BTC_index_normal10 <- which(CrCu_BTC_Roll8@forecast$VaR[,3]>CrCu_BTC_Roll8@forecast$VaR[,4])
CrCu_BTC_VaR_values_normal10 <- CrCu_BTC_Roll8@forecast$VaR[CrCu_BTC_index_normal10,3]
CrCu_BTC_return_values_normal10 <- CrCu_BTC_Roll8@forecast$VaR[CrCu_BTC_index_normal10,4]
CrCu_BTC_ES_normal10 <- (CrCu_BTC_return_values_normal10/CrCu_BTC_VaR_values_normal10)
CrCu_BTC_ES_Measure1_normal10 <- mean(CrCu_BTC_ES_normal10)
CrCu_BTC_ES_Measure2_normal10 <- max(CrCu_BTC_ES_normal10)

## Assuming SST Distribution

CrCu_BTC_index_SST1 <- which(CrCu_BTC_Roll5@forecast$VaR[,1]>CrCu_BTC_Roll5@forecast$VaR[,4])
CrCu_BTC_VaR_values_SST1 <- CrCu_BTC_Roll5@forecast$VaR[CrCu_BTC_index_SST1,1]
CrCu_BTC_return_values_SST1 <- CrCu_BTC_Roll5@forecast$VaR[CrCu_BTC_index_SST1,4]
CrCu_BTC_ES_SST1 <- (CrCu_BTC_return_values_SST1/CrCu_BTC_VaR_values_SST1)
CrCu_BTC_ES_Measure1_SST1 <- mean(CrCu_BTC_ES_SST1)
CrCu_BTC_ES_Measure2_SST1 <- max(CrCu_BTC_ES_SST1)

CrCu_BTC_index_SST5 <- which(CrCu_BTC_Roll5@forecast$VaR[,2]>CrCu_BTC_Roll5@forecast$VaR[,4])
CrCu_BTC_VaR_values_SST5 <- CrCu_BTC_Roll5@forecast$VaR[CrCu_BTC_index_SST5,2]
CrCu_BTC_return_values_SST5 <- CrCu_BTC_Roll5@forecast$VaR[CrCu_BTC_index_SST5,4]
CrCu_BTC_ES_SST5 <- (CrCu_BTC_return_values_SST5/CrCu_BTC_VaR_values_SST5)
CrCu_BTC_ES_Measure1_SST5 <- mean(CrCu_BTC_ES_SST5)
CrCu_BTC_ES_Measure2_SST5 <- max(CrCu_BTC_ES_SST5)

CrCu_BTC_index_SST10 <- which(CrCu_BTC_Roll5@forecast$VaR[,3]>CrCu_BTC_Roll5@forecast$VaR[,4])
CrCu_BTC_VaR_values_SST10 <- CrCu_BTC_Roll5@forecast$VaR[CrCu_BTC_index_SST10,3]
CrCu_BTC_return_values_SST10 <- CrCu_BTC_Roll5@forecast$VaR[CrCu_BTC_index_SST10,4]
CrCu_BTC_ES_SST10 <- (CrCu_BTC_return_values_SST10/CrCu_BTC_VaR_values_SST10)
CrCu_BTC_ES_Measure1_SST10 <- mean(CrCu_BTC_ES_SST10)
CrCu_BTC_ES_Measure2_SST10 <- max(CrCu_BTC_ES_SST10)

## Assuming SGED Distribution

CrCu_BTC_index_SGED1 <- which(CrCu_BTC_Roll6@forecast$VaR[,1]>CrCu_BTC_Roll6@forecast$VaR[,4])
CrCu_BTC_VaR_values_SGED1 <- CrCu_BTC_Roll6@forecast$VaR[CrCu_BTC_index_SGED1,1]
CrCu_BTC_return_values_SGED1 <- CrCu_BTC_Roll6@forecast$VaR[CrCu_BTC_index_SGED1,4]
CrCu_BTC_ES_SGED1 <- (CrCu_BTC_return_values_SGED1/CrCu_BTC_VaR_values_SGED1)
CrCu_BTC_ES_Measure1_SGED1 <- mean(CrCu_BTC_ES_SGED1)
CrCu_BTC_ES_Measure2_SGED1 <- max(CrCu_BTC_ES_SGED1)

CrCu_BTC_index_SGED5 <- which(CrCu_BTC_Roll6@forecast$VaR[,2]>CrCu_BTC_Roll6@forecast$VaR[,4])
CrCu_BTC_VaR_values_SGED5 <- CrCu_BTC_Roll6@forecast$VaR[CrCu_BTC_index_SGED5,2]
CrCu_BTC_return_values_SGED5 <- CrCu_BTC_Roll6@forecast$VaR[CrCu_BTC_index_SGED5,4]
CrCu_BTC_ES_SGED5 <- (CrCu_BTC_return_values_SGED5/CrCu_BTC_VaR_values_SGED5)
CrCu_BTC_ES_Measure1_SGED5 <- mean(CrCu_BTC_ES_SGED5)
CrCu_BTC_ES_Measure2_SGED5 <- max(CrCu_BTC_ES_SGED5)

CrCu_BTC_index_SGED10 <- which(CrCu_BTC_Roll6@forecast$VaR[,3]>CrCu_BTC_Roll6@forecast$VaR[,4])
CrCu_BTC_VaR_values_SGED10 <- CrCu_BTC_Roll6@forecast$VaR[CrCu_BTC_index_SGED10,3]
CrCu_BTC_return_values_SGED10 <- CrCu_BTC_Roll6@forecast$VaR[CrCu_BTC_index_SGED10,4]
CrCu_BTC_ES_SGED10 <- (CrCu_BTC_return_values_SGED10/CrCu_BTC_VaR_values_SGED10)
CrCu_BTC_ES_Measure1_SGED10 <- mean(CrCu_BTC_ES_SGED10)
CrCu_BTC_ES_Measure2_SGED10 <- max(CrCu_BTC_ES_SGED10)

## Assuming JSU Distribution

CrCu_BTC_index_JSU1 <- which(CrCu_BTC_Roll7@forecast$VaR[,1]>CrCu_BTC_Roll7@forecast$VaR[,4])
CrCu_BTC_VaR_values_JSU1 <- CrCu_BTC_Roll7@forecast$VaR[CrCu_BTC_index_JSU1,1]
CrCu_BTC_return_values_JSU1 <- CrCu_BTC_Roll7@forecast$VaR[CrCu_BTC_index_JSU1,4]
CrCu_BTC_ES_JSU1 <- (CrCu_BTC_return_values_JSU1/CrCu_BTC_VaR_values_JSU1)
CrCu_BTC_ES_Measure1_JSU1 <- mean(CrCu_BTC_ES_JSU1)
CrCu_BTC_ES_Measure2_JSU1 <- max(CrCu_BTC_ES_JSU1)

CrCu_BTC_index_JSU5 <- which(CrCu_BTC_Roll7@forecast$VaR[,2]>CrCu_BTC_Roll7@forecast$VaR[,4])
CrCu_BTC_VaR_values_JSU5 <- CrCu_BTC_Roll7@forecast$VaR[CrCu_BTC_index_JSU5,2]
CrCu_BTC_return_values_JSU5 <- CrCu_BTC_Roll7@forecast$VaR[CrCu_BTC_index_JSU5,4]
CrCu_BTC_ES_JSU5 <- (CrCu_BTC_return_values_JSU5/CrCu_BTC_VaR_values_JSU5)
CrCu_BTC_ES_Measure1_JSU5 <- mean(CrCu_BTC_ES_JSU5)
CrCu_BTC_ES_Measure2_JSU5 <- max(CrCu_BTC_ES_JSU5)

CrCu_BTC_index_JSU10 <- which(CrCu_BTC_Roll7@forecast$VaR[,3]>CrCu_BTC_Roll7@forecast$VaR[,4])
CrCu_BTC_VaR_values_JSU10 <- CrCu_BTC_Roll7@forecast$VaR[CrCu_BTC_index_JSU10,3]
CrCu_BTC_return_values_JSU10 <- CrCu_BTC_Roll7@forecast$VaR[CrCu_BTC_index_JSU10,4]
CrCu_BTC_ES_JSU10 <- (CrCu_BTC_return_values_JSU10/CrCu_BTC_VaR_values_JSU10)
CrCu_BTC_ES_Measure1_JSU10 <- mean(CrCu_BTC_ES_JSU10)
CrCu_BTC_ES_Measure2_JSU10 <- max(CrCu_BTC_ES_JSU10)

### Assuming Pearson Distribution

Z_Pearson_BTC_01   <- qpearson(0.01,CrCu_BTC_Dist8)
BTC_Holdout_VaR_01   <- BTC_Holdout_Mu + BTC_Holdout_Sigma*Z_Pearson_BTC_01

BTC_index_Pearson1 <- which(BTC_Holdout_VaR_01[-1]>CrCu_BTC_Roll7@forecast$VaR[1:349,4])
BTC_VaR_values_Pearson1 <- BTC_Holdout_VaR_01[BTC_index_Pearson1]
BTC_return_values_Pearson1 <- CrCu_BTC_Roll7@forecast$VaR[BTC_index_Pearson1,4]
BTC_ES_Pearson1 <- (BTC_return_values_Pearson1/BTC_VaR_values_Pearson1)
BTC_ES_Measure1_Pearson1 <- mean(BTC_ES_Pearson1)
BTC_ES_Measure2_Pearson1 <- max(BTC_ES_Pearson1)

Z_Pearson_BTC_05   <- qpearson(0.05,CrCu_BTC_Dist8)
BTC_Holdout_VaR_05   <- BTC_Holdout_Mu + BTC_Holdout_Sigma*Z_Pearson_BTC_05

CrCu_BTC_index_Pearson5 <- which(BTC_Holdout_VaR_05[-1]>CrCu_BTC_Roll7@forecast$VaR[1:349,4])
CrCu_BTC_VaR_values_Pearson5 <- BTC_Holdout_VaR_05[CrCu_BTC_index_Pearson5]
CrCu_BTC_return_values_Pearson5 <- CrCu_BTC_Roll7@forecast$VaR[CrCu_BTC_index_Pearson5,4]
CrCu_BTC_ES_Pearson5 <- (CrCu_BTC_return_values_Pearson5/CrCu_BTC_VaR_values_Pearson5)
CrCu_BTC_ES_Measure1_Pearson5 <- mean(CrCu_BTC_ES_Pearson5)
CrCu_BTC_ES_Measure2_Pearson5 <- max(CrCu_BTC_ES_Pearson5)

Z_Pearson_BTC_10   <- qpearson(0.10,CrCu_BTC_Dist8)
BTC_Holdout_VaR_10   <- BTC_Holdout_Mu + BTC_Holdout_Sigma*Z_Pearson_BTC_10

CrCu_BTC_index_Pearson10 <- which(BTC_Holdout_VaR_10[-1]>CrCu_BTC_Roll7@forecast$VaR[1:349,4])
CrCu_BTC_VaR_values_Pearson10 <- BTC_Holdout_VaR_10[CrCu_BTC_index_Pearson10]
CrCu_BTC_return_values_Pearson10 <- CrCu_BTC_Roll7@forecast$VaR[CrCu_BTC_index_Pearson10,4]
CrCu_BTC_ES_Pearson10 <- (CrCu_BTC_return_values_Pearson10/CrCu_BTC_VaR_values_Pearson10)
CrCu_BTC_ES_Measure1_Pearson10 <- mean(CrCu_BTC_ES_Pearson10)
CrCu_BTC_ES_Measure2_Pearson10 <- max(CrCu_BTC_ES_Pearson10)

### Store for Pearson

Pearson_Full_ES_Measure1_BTC <- matrix(c(BTC_ES_Measure1_Pearson1,CrCu_BTC_ES_Measure1_Pearson5,CrCu_BTC_ES_Measure1_Pearson10),nrow=3)

Pearson_Full_ES_Measure2_BTC <- matrix(c(BTC_ES_Measure2_Pearson1,CrCu_BTC_ES_Measure2_Pearson5,CrCu_BTC_ES_Measure2_Pearson10),nrow=3)

rownames(Pearson_Full_ES_Measure1_BTC) <- c("1%","5%","10%")
rownames(Pearson_Full_ES_Measure2_BTC) <- c("1%","5%","10%")
colnames(Pearson_Full_ES_Measure1_BTC) <- c("BTC")
colnames(Pearson_Full_ES_Measure2_BTC) <- c("BTC")

#### Store the values
CrCu_BTC_Full_ES_Measure1 <- matrix(c(CrCu_BTC_ES_Measure1_normal1,CrCu_BTC_ES_Measure1_SST1,CrCu_BTC_ES_Measure1_SGED1,CrCu_BTC_ES_Measure1_JSU1,
                                      CrCu_BTC_ES_Measure1_normal5,CrCu_BTC_ES_Measure1_SST5,CrCu_BTC_ES_Measure1_SGED5,CrCu_BTC_ES_Measure1_JSU5,
                                      CrCu_BTC_ES_Measure1_normal10,CrCu_BTC_ES_Measure1_SST10,CrCu_BTC_ES_Measure1_SGED10,CrCu_BTC_ES_Measure1_JSU10),nrow=3,byrow=TRUE)

CrCu_BTC_Full_ES_Measure2 <- matrix(c(CrCu_BTC_ES_Measure2_normal1,CrCu_BTC_ES_Measure2_SST1,CrCu_BTC_ES_Measure2_SGED1,CrCu_BTC_ES_Measure2_JSU1,
                                      CrCu_BTC_ES_Measure2_normal5,CrCu_BTC_ES_Measure2_SST5,CrCu_BTC_ES_Measure2_SGED5,CrCu_BTC_ES_Measure2_JSU5,
                                      CrCu_BTC_ES_Measure2_normal10,CrCu_BTC_ES_Measure2_SST10,CrCu_BTC_ES_Measure2_SGED10,CrCu_BTC_ES_Measure2_JSU10),nrow=3,byrow=TRUE)

rownames(CrCu_BTC_Full_ES_Measure1) <- c("1%","5%","10%")
rownames(CrCu_BTC_Full_ES_Measure2) <- c("1%","5%","10%")

colnames(CrCu_BTC_Full_ES_Measure1) <- c("Normal","SST","SGED","JSU")
colnames(CrCu_BTC_Full_ES_Measure2) <- c("Normal","SST","SGED","JSU")

#### End of all the code/lines ###### 




