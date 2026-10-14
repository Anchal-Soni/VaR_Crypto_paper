# =============================================================================
# 03d_garch_XMR.R
#
# Purpose : XMR: return-series tests, ARMA(1,1)-EGARCH(1,1) with Norm/SST/SGED/JSU
#           errors on the full sample (with break dummies) and on SS1-SS3; Pearson IV
#           (PSIV) fitted to standardised residuals; rolling 1-day-ahead VaR
#           (hold-out: last 350 obs of full sample; last 60/200/100 obs of SS1/SS2/SS3); Kupiec, Christoffersen and
#           duration tests; VaR plots.
# Inputs  : data/Cryptocurrency6.csv; R/functions/strucbreak.R
# Outputs : CrCu_XMR_* objects; output/figures/{FS,SS1,SS2,SS3}VaR_XMR.eps
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
Crypto1$XMR = as.numeric(as.character(Crypto1$XMR))
CrCu_XMR <- data.frame(Crypto_Date, Crypto$Monero) # Monero Data with Date
View(CrCu_XMR)
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

CrCu_XMR_samples <- CPPP # Structural break locations

####### :::::::::: Returns Series

Returns_CrCu_XMR <- log(Crypto1$XMR[-1]/Crypto1$XMR[1:(nrow(Crypto1)-1)]) # Returns series
Des_Returns_CrCu_XMR <- basicStats(Returns_CrCu_XMR) # Descriptive statistics
View(Returns_CrCu_XMR)

#### Convert into xts objects ####

CrCu_XMR_xts   <- xts(Returns_CrCu_XMR,order.by = Crypto_Date[-1])
names(CrCu_XMR_xts)   <- "Returns"
View(CrCu_XMR_xts)

# Time series properties of return series 
# H0: The series has unit root
adf_XMR <- adf.test(CrCu_XMR_xts) #p-value proves that returns series is stationary
# H0: The series follow normal distribution
jb_XMR <- jarque.bera.test(CrCu_XMR_xts)
# H0: Independence in the given series
Bt_XMR <- Box.test(CrCu_XMR_xts, type = "Ljung-Box", lag = 10) #pvalue says that there is significant autocorrelation
Bt_XMR_sq <- Box.test(CrCu_XMR_xts^2, type = "Ljung-Box", lag = 10) #pvalue confirms presence of GARCH effect
# H0: No ARCH effects
arch_XMR <- ArchTest(CrCu_XMR_xts, lags = 5) #pvalue confirms evidence of presence of ARCH effects

### Subsamples Dates
#SS1 - 2/12/2016 to 27/09/2017
#SS2 - 28/09/2017 to 23/06/2020
#SS3 - 24/06/2020 to 04/12/2021

### Subsamples XTS ###
CrCu_XMR_sub1    <- CrCu_XMR_xts[1:CrCu_XMR_samples[1]]
CrCu_XMR_sub2    <- CrCu_XMR_xts[(CrCu_XMR_samples[1]+1):CrCu_XMR_samples[2]]
CrCu_XMR_sub3    <- CrCu_XMR_xts[(CrCu_XMR_samples[2]+1):nrow(CrCu_XMR_xts)]

### Dummy variables for structural breaks (to be used in Garch modelling)####

CrCu_XMR_Dummy1 <- array(data=0,dim=length(CrCu_XMR_xts))
CrCu_XMR_Dummy2 <- array(data=0,dim=length(CrCu_XMR_xts))
CrCu_XMR_Dummy3 <- array(data=0,dim=length(CrCu_XMR_xts))

CrCu_XMR_Dummy1[1:CrCu_XMR_samples[1]]  <- 1
CrCu_XMR_Dummy2[(CrCu_XMR_samples[1]+1):CrCu_XMR_samples[2]] <- 1
CrCu_XMR_Dummy3[(CrCu_XMR_samples[2]+1):length(CrCu_XMR_xts)] <- 1

CrCu_XMR_Dummy <- cbind(CrCu_XMR_Dummy1,CrCu_XMR_Dummy2,CrCu_XMR_Dummy3)

## :::::::::::: check presence of ARCH effetc in Full Sample
library(FinTS)
ArchTest(CrCu_XMR_xts, lags = 1)
ArchTest(CrCu_XMR_xts, lags = 5)
ArchTest(CrCu_XMR_xts, lags = 10)
pacf((CrCu_XMR_xts)^2)
# H0: Series has no ARCH effects
# Null hypothesis is rejected, hence proves presence of ARCH effects

### Garch Models
# :::::::::With external regressors in Full Sample GARCH

#SST #SGED #JSU #Norm
CrCu_XMR_Spec1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_XMR_Spec2 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_XMR_Spec3 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_XMR_Spec4 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_XMR_Model1 <- ugarchfit(CrCu_XMR_Spec1,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model2 <- ugarchfit(CrCu_XMR_Spec2,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model3 <- ugarchfit(CrCu_XMR_Spec3,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model4 <- ugarchfit(CrCu_XMR_Spec4,CrCu_XMR_xts,out.sample = 350)

#### egarch models

CrCu_XMR_Spec5 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_XMR_Spec6 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_XMR_Spec7 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_XMR_Spec8 <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_XMR_Model5 <- ugarchfit(CrCu_XMR_Spec5,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model6 <- ugarchfit(CrCu_XMR_Spec6,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model7 <- ugarchfit(CrCu_XMR_Spec7,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model8 <- ugarchfit(CrCu_XMR_Spec8,CrCu_XMR_xts,out.sample = 350)

### gjrgarch models

CrCu_XMR_Spec9  <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_XMR_Spec10 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_XMR_Spec11 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_XMR_Spec12 <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1),external.regressors=CrCu_XMR_Dummy),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_XMR_Model9  <- ugarchfit(CrCu_XMR_Spec9,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model10 <- ugarchfit(CrCu_XMR_Spec10,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model11 <- ugarchfit(CrCu_XMR_Spec11,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model12 <- ugarchfit(CrCu_XMR_Spec12,CrCu_XMR_xts,out.sample = 350)
#uncmean(CrCu_XMR_Model5)
#uncvariance(CrCu_XMR_Model1)
#halfline(CrCu_XMR_Model5)
#plot.ts(sigma(CrCu_XMR_Model5), ylab="sigma(t)", col="blue")
#plot(CrCu_XMR_Model5)

## persistence

persis_XMR <- matrix(data=0, nrow = 12, ncol = 1)
persis_XMR[1,1] <- round(persistence(CrCu_XMR_Model1),4)
persis_XMR[2,1] <- round(persistence(CrCu_XMR_Model2),4)
persis_XMR[3,1] <- round(persistence(CrCu_XMR_Model3),4)
persis_XMR[4,1] <- round(persistence(CrCu_XMR_Model4),4)
persis_XMR[5,1] <- round(persistence(CrCu_XMR_Model5),4)
persis_XMR[6,1] <- round(persistence(CrCu_XMR_Model6),4)
persis_XMR[7,1] <- round(persistence(CrCu_XMR_Model7),4)
persis_XMR[8,1] <- round(persistence(CrCu_XMR_Model8),4)
persis_XMR[9,1] <- round(persistence(CrCu_XMR_Model9),4)
persis_XMR[10,1] <- round(persistence(CrCu_XMR_Model10),4)
persis_XMR[11,1] <- round(persistence(CrCu_XMR_Model11),4)
persis_XMR[12,1] <- round(persistence(CrCu_XMR_Model12),4)

persistance_XMR <- matrix(data=persis_XMR, nrow = 12, ncol = 1)
colnames(persistance_XMR) <- c("Persistence_XMR")
rownames(persistance_XMR) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(data.frame(persistance_XMR))
#xtable(persistance_XMR, digits = 4)

### Store the loglikelihood values, AIC and BIC ####

Llhood_XMR  <- matrix(data=0,nrow=12,ncol=1)
colnames(Llhood_XMR) <- c("CrCu_XMR")
rownames(Llhood_XMR) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
Llhood_XMR[1,1]  <-  CrCu_XMR_Model1@fit$LLH
Llhood_XMR[2,1]  <-  CrCu_XMR_Model2@fit$LLH
Llhood_XMR[3,1]  <-  CrCu_XMR_Model3@fit$LLH
Llhood_XMR[4,1]  <-  CrCu_XMR_Model4@fit$LLH
Llhood_XMR[5,1]  <-  CrCu_XMR_Model5@fit$LLH
Llhood_XMR[6,1]  <-  CrCu_XMR_Model6@fit$LLH
Llhood_XMR[7,1]  <-  CrCu_XMR_Model7@fit$LLH
Llhood_XMR[8,1]  <-  CrCu_XMR_Model8@fit$LLH
Llhood_XMR[9,1]  <-  CrCu_XMR_Model9@fit$LLH
Llhood_XMR[10,1] <- CrCu_XMR_Model10@fit$LLH
Llhood_XMR[11,1] <- CrCu_XMR_Model11@fit$LLH
Llhood_XMR[12,1] <- CrCu_XMR_Model12@fit$LLH

BIC_XMR  <- matrix(data=0,nrow=12,ncol=1)
colnames(BIC_XMR) <- c("CrCu_XMR")
rownames(BIC_XMR) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
BIC_XMR[1,1]  <-  infocriteria(CrCu_XMR_Model1)[2]
BIC_XMR[2,1]  <-  infocriteria(CrCu_XMR_Model2)[2]
BIC_XMR[3,1]  <-  infocriteria(CrCu_XMR_Model3)[2]
BIC_XMR[4,1]  <-  infocriteria(CrCu_XMR_Model4)[2]
BIC_XMR[5,1]  <-  infocriteria(CrCu_XMR_Model5)[2]
BIC_XMR[6,1]  <-  infocriteria(CrCu_XMR_Model6)[2]
BIC_XMR[7,1]  <-  infocriteria(CrCu_XMR_Model7)[2]
BIC_XMR[8,1]  <-  infocriteria(CrCu_XMR_Model8)[2]
BIC_XMR[9,1]  <-  infocriteria(CrCu_XMR_Model9)[2]
BIC_XMR[10,1] <- infocriteria(CrCu_XMR_Model10)[2]
BIC_XMR[11,1] <- infocriteria(CrCu_XMR_Model11)[2]
BIC_XMR[12,1] <- infocriteria(CrCu_XMR_Model12)[2]

AIC_XMR  <- matrix(data=0,nrow=12,ncol=1)
colnames(AIC_XMR) <- c("CrCu_XMR")
rownames(AIC_XMR) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
AIC_XMR[1,1]  <-  infocriteria(CrCu_XMR_Model1)[1]
AIC_XMR[2,1]  <-  infocriteria(CrCu_XMR_Model2)[1]
AIC_XMR[3,1]  <-  infocriteria(CrCu_XMR_Model3)[1]
AIC_XMR[4,1]  <-  infocriteria(CrCu_XMR_Model4)[1]
AIC_XMR[5,1]  <-  infocriteria(CrCu_XMR_Model5)[1]
AIC_XMR[6,1]  <-  infocriteria(CrCu_XMR_Model6)[1]
AIC_XMR[7,1]  <-  infocriteria(CrCu_XMR_Model7)[1]
AIC_XMR[8,1]  <-  infocriteria(CrCu_XMR_Model8)[1]
AIC_XMR[9,1]  <-  infocriteria(CrCu_XMR_Model9)[1]
AIC_XMR[10,1] <- infocriteria(CrCu_XMR_Model10)[1]
AIC_XMR[11,1] <- infocriteria(CrCu_XMR_Model11)[1]
AIC_XMR[12,1] <- infocriteria(CrCu_XMR_Model12)[1]

Infocrit_XMR  <- matrix(data=0,nrow=12,ncol=3)
colnames(Infocrit_XMR) <- c("Likelihood", "BIC", "AIC")
rownames(Infocrit_XMR) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")

Infocrit_XMR[,1] = Llhood_XMR
Infocrit_XMR[,2] = BIC_XMR
Infocrit_XMR[,3] = AIC_XMR
View(data.frame(Infocrit_XMR))
xtable(Infocrit_XMR, caption = "Information Criteria for Monero GARCH models - Full Sample", digits = 4)

## Conclusion: eGarch-JSU have the highest AIC and eGarch-SST have the second highest #####
## Model 7 and the Model 5

## coefficients

coef_XMR_1 <- round(coef(CrCu_XMR_Model1),4)
coef_XMR_2 <- round(coef(CrCu_XMR_Model2),4)
coef_XMR_3 <- round(coef(CrCu_XMR_Model3),4)
coef_XMR_4 <- round(coef(CrCu_XMR_Model4),4)
coef_XMR_5 <- round(coef(CrCu_XMR_Model5),4)
coef_XMR_6 <- round(coef(CrCu_XMR_Model6),4)
coef_XMR_7 <- round(coef(CrCu_XMR_Model7),4)
coef_XMR_8 <- round(coef(CrCu_XMR_Model8),4)
coef_XMR_9 <- round(coef(CrCu_XMR_Model9),4)
coef_XMR_10 <- round(coef(CrCu_XMR_Model10),4)
coef_XMR_11 <- round(coef(CrCu_XMR_Model11),4)
coef_XMR_12 <- round(coef(CrCu_XMR_Model12),4)

garch_coef_XMR <- bind_rows(coef_XMR_1, coef_XMR_2, coef_XMR_3, coef_XMR_4,coef_XMR_5, coef_XMR_6, coef_XMR_7, coef_XMR_8, coef_XMR_9, coef_XMR_10, coef_XMR_11, coef_XMR_12)

garch_coef_XMR1 <- as.data.frame(garch_coef_XMR)
rownames(garch_coef_XMR1) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(garch_coef_XMR1)
xtable(garch_coef_XMR1, caption = "Coefficients of Monero GARCH models - Full Sample", digits = 4)

### Ljung-box test statistic
# Check standardized shocks

#CrCu_XMR5_LQ1  <- Box.test(CrCu_XMR_Model5@fit$residuals/CrCu_XMR_Model5@fit$sigma,lag=1,type="Lj")
#CrCu_XMR5_LQ5  <- Box.test(CrCu_XMR_Model5@fit$residuals/CrCu_XMR_Model5@fit$sigma,lag=5,type="Lj")
#CrCu_XMR5_LQ10 <- Box.test(CrCu_XMR_Model5@fit$residuals/CrCu_XMR_Model5@fit$sigma,lag=10,type="Lj")

#CrCu_XMR6_LQ1  <- Box.test(CrCu_XMR_Model6@fit$residuals/CrCu_XMR_Model6@fit$sigma,lag=1,type="Lj")
#CrCu_XMR6_LQ5  <- Box.test(CrCu_XMR_Model6@fit$residuals/CrCu_XMR_Model6@fit$sigma,lag=5,type="Lj")
#CrCu_XMR6_LQ10 <- Box.test(CrCu_XMR_Model6@fit$residuals/CrCu_XMR_Model6@fit$sigma,lag=10,type="Lj")

#CrCu_XMR7_LQ1  <- Box.test(CrCu_XMR_Model7@fit$residuals/CrCu_XMR_Model7@fit$sigma,lag=1,type="Lj")
#CrCu_XMR7_LQ5  <- Box.test(CrCu_XMR_Model7@fit$residuals/CrCu_XMR_Model7@fit$sigma,lag=5,type="Lj")
#CrCu_XMR7_LQ10 <- Box.test(CrCu_XMR_Model7@fit$residuals/CrCu_XMR_Model7@fit$sigma,lag=10,type="Lj")

#CrCu_XMR8_LQ1  <- Box.test(CrCu_XMR_Model8@fit$residuals/CrCu_XMR_Model8@fit$sigma,lag=1,type="Lj")
#CrCu_XMR8_LQ5  <- Box.test(CrCu_XMR_Model8@fit$residuals/CrCu_XMR_Model8@fit$sigma,lag=5,type="Lj")
#CrCu_XMR8_LQ10 <- Box.test(CrCu_XMR_Model8@fit$residuals/CrCu_XMR_Model8@fit$sigma,lag=10,type="Lj")

#CrCu_XMR5_LQ <- c(CrCu_XMR5_LQ1$statistic,CrCu_XMR5_LQ5$statistic,CrCu_XMR5_LQ10$statistic)
#CrCu_XMR6_LQ <- c(CrCu_XMR6_LQ1$statistic,CrCu_XMR6_LQ5$statistic,CrCu_XMR6_LQ10$statistic)
#CrCu_XMR7_LQ <- c(CrCu_XMR7_LQ1$statistic,CrCu_XMR7_LQ5$statistic,CrCu_XMR7_LQ10$statistic)
#CrCu_XMR8_LQ <- c(CrCu_XMR8_LQ1$statistic,CrCu_XMR8_LQ5$statistic,CrCu_XMR8_LQ10$statistic)

#CrCu_XMR_res_stat <-   cbind(CrCu_XMR5_LQ,CrCu_XMR6_LQ,CrCu_XMR7_LQ,CrCu_XMR8_LQ)
#rownames(CrCu_XMR_res_stat) <- c("LQ1","LQ5","LQ10")
#colnames(CrCu_XMR_res_stat) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

#### 1-day ahead Rolling Forecasts ####
## Full sample for roll i.e. last 350 points for forecast and rest for model
CrCu_XMR_Roll5 <-  ugarchroll(CrCu_XMR_Spec5,CrCu_XMR_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll6 <-  ugarchroll(CrCu_XMR_Spec6,CrCu_XMR_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll7 <-  ugarchroll(CrCu_XMR_Spec7,CrCu_XMR_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll8 <-  ugarchroll(CrCu_XMR_Spec8,CrCu_XMR_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model

garchsigSST_XMR <- CrCu_XMR_Roll5@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll5@forecast$density$Realized)
MSEvalSST_XMR <- MSE(garchsigSST_XMR, realval_XMR)
MAPEgvalSST_XMR <- MAPE(garchsigSST_XMR, realval_XMR)

garchsigSGED_XMR <- CrCu_XMR_Roll6@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll6@forecast$density$Realized)
MSEvalSGED_XMR <- MSE(garchsigSGED_XMR, realval_XMR)
MAPEgvalSGED_XMR <- MAPE(garchsigSGED_XMR, realval_XMR)

garchsigJSU_XMR <- CrCu_XMR_Roll7@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll7@forecast$density$Realized)
MSEvalJSU_XMR <- MSE(garchsigJSU_XMR, realval_XMR)
MAPEgvalJSU_XMR <- MAPE(garchsigJSU_XMR, realval_XMR)

garchsigNorm_XMR <- CrCu_XMR_Roll8@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll8@forecast$density$Realized)
MSEvalNorm_XMR <- MSE(garchsigNorm_XMR, realval_XMR)
MAPEgvalNorm_XMR <- MAPE(garchsigNorm_XMR, realval_XMR)

MAPEgval_XMR  <- matrix(data=0,nrow=4,ncol=2)
colnames(MAPEgval_XMR) <- c("MSE", "MAPE")
rownames(MAPEgval_XMR) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")

MAPEgval_XMR[,1] = cbind(MSEvalSST_XMR,MSEvalSGED_XMR,MSEvalJSU_XMR,MSEvalNorm_XMR)
MAPEgval_XMR[,2] = cbind(MAPEgvalSST_XMR,MAPEgvalSGED_XMR,MAPEgvalJSU_XMR,MAPEgvalNorm_XMR)

xtable(MAPEgval_XMR, caption = "MSE & MAPE for out of sample forecast of Monero GARCH - Full Sample", digits = 4)

#### Store the VaR for the out sample using rolling window forecasts

CrCu_XMR_VaR_SST   <- CrCu_XMR_Roll5@forecast$VaR
CrCu_XMR_VaR_SGED  <- CrCu_XMR_Roll6@forecast$VaR
CrCu_XMR_VaR_JSU   <- CrCu_XMR_Roll7@forecast$VaR
CrCu_XMR_VaR_Norm  <- CrCu_XMR_Roll8@forecast$VaR

#### ::::::::::::: Repeating for P E A R S O N Distribution  #####

### Check the summary stats of the standardized residuals
CrCu_XMR_Model8_res <- CrCu_XMR_Model8@fit$residuals/CrCu_XMR_Model8@fit$sigma
CrCu_XMR_Descr8 <- basicStats(CrCu_XMR_Model8_res)

### Fitting distributions on the standardized residuals 

library(PearsonDS)
library(gsl)
CrCu_XMR_Dist8   <- pearsonFitML(CrCu_XMR_Model8_res)
CrCu_XMR_JSU_param <- JohnsonFit(CrCu_XMR_Model8_res,moment ="find")

### Store the parameters for different distributions  

CrCu_XMR_P4_Param    <- CrCu_XMR_Dist8[2:5]

### Checking the fit using KS tests

CrCu_XMR_test1 <- ks.test(CrCu_XMR_Model8_res,rpearson(length(CrCu_XMR_Model8_res),params = CrCu_XMR_Dist8))
#Null: True distribution is equal to the hypothesized distribution
#p-value is high, hence, we fail to reject H0.

### Calculate the quantiles

XMR_Exceed_Pearson    <- array(data=0,dim=length(signific_levels))
qpearson(signific_levels[2],CrCu_XMR_Dist8)
for(j in 1:3)
{
  Z_Pearson_XMR   <- qpearson(signific_levels[j],CrCu_XMR_Dist8)
  
  mu_XMR   <- array(data=0,dim=length(Returns_CrCu_XMR)-350)
  VaR_XMR   <- array(data=0,dim=length(Returns_CrCu_XMR)-351)
  
  for(i in 2:(length(Returns_CrCu_XMR)-350))
  {
    mu_XMR[i] <- CrCu_XMR_Model8@fit$coef[1]+CrCu_XMR_Model8@fit$coef[2]*Returns_CrCu_XMR[i-1]+CrCu_XMR_Model8@fit$coef[3]*CrCu_XMR_Model8_res[i-1]
    VaR_XMR[i-1] <- mu_XMR[i]+CrCu_XMR_Model8@fit$sigma[i]*Z_Pearson_XMR
  }
  #XMR_Violations[j]   <- VaRTest(signific_levels[j],Returns_CrCu_XMR[1:(length(VaR_XMR))],VaR_XMR)
  print(VaRTest(signific_levels[j],Returns_CrCu_XMR[1:(length(VaR_XMR))],VaR_XMR))
  #XMR_Exceed_Pearson[j] <- XMR_Violations$actual.exceed
}

#### Out of sample VaR forecast  #####

Holdout_Returns_XMR   <- Returns_CrCu_XMR[(length(Returns_CrCu_XMR)-349):length(Returns_CrCu_XMR)] 

Holdout_XMR_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_XMR_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_XMR_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_XMR_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_XMR_Pearson) <- c("1%","5%","10%")
rownames(Holdout_XMR_Pearson_pvalues) <- c("1%","5%","10%")

## Store the volatility and mu forecast
signific_levels <- c(0.1,0.05,0.01)

XMR_Holdout_Mu <- as.data.frame(CrCu_XMR_Roll8)[,'Mu']
XMR_Holdout_Sigma <- as.data.frame(CrCu_XMR_Roll8)[,'Sigma']

MP_XMR <- c(0 , 0, 0)

for(j in 1:3)
{
  
  Z_Pearson_XMR   <- qpearson(signific_levels[j],CrCu_XMR_Dist8)
  
  XMR_Holdout_VaR   <- XMR_Holdout_Mu + XMR_Holdout_Sigma*Z_Pearson_XMR
  XMR_Violations   <- BacktestVaR(Holdout_Returns_XMR[1:(length(Holdout_Returns_XMR)-1)],XMR_Holdout_VaR[-1],signific_levels[j])
  Holdout_XMR_Pearson[j,]   <- c(XMR_Violations$LRuc[1],XMR_Violations$LRcc[1],XMR_Violations$DQ$stat,XMR_Violations$AE,XMR_Violations$Loss$Loss)
  Holdout_XMR_Pearson_pvalues[j,] <- c(XMR_Violations$LRuc[2],XMR_Violations$LRcc[2],XMR_Violations$DQ$pvalue)
  MP_XMR[[j]] <- MAPE(XMR_Holdout_VaR, abs(CrCu_XMR_VaR_Norm$realized))
  print(XMR_Violations$AE)
  #print(XMR_Violations)
}

########## ::::::::::::::: MAPE ###########
# Forecast performance measure for full sample
# Value at Risk

m1_XMR <- MAPE(CrCu_XMR_VaR_SST$`alpha(1%)`, abs(CrCu_XMR_VaR_SST$realized))
m2_XMR <- MAPE(CrCu_XMR_VaR_SST$`alpha(5%)`, abs(CrCu_XMR_VaR_SST$realized))
m3_XMR <- MAPE(CrCu_XMR_VaR_SST$`alpha(10%)`, abs(CrCu_XMR_VaR_SST$realized))

m4_XMR <- MAPE(CrCu_XMR_VaR_SGED$`alpha(1%)`, abs(CrCu_XMR_VaR_SGED$realized))
m5_XMR <- MAPE(CrCu_XMR_VaR_SGED$`alpha(5%)`, abs(CrCu_XMR_VaR_SGED$realized))
m6_XMR <- MAPE(CrCu_XMR_VaR_SGED$`alpha(10%)`, abs(CrCu_XMR_VaR_SGED$realized))

m7_XMR <- MAPE(CrCu_XMR_VaR_JSU$`alpha(1%)`, abs(CrCu_XMR_VaR_JSU$realized))
m8_XMR <- MAPE(CrCu_XMR_VaR_JSU$`alpha(5%)`, abs(CrCu_XMR_VaR_JSU$realized))
m9_XMR <- MAPE(CrCu_XMR_VaR_JSU$`alpha(10%)`, abs(CrCu_XMR_VaR_JSU$realized))

m10_XMR <- MAPE(CrCu_XMR_VaR_Norm$`alpha(1%)`, abs(CrCu_XMR_VaR_Norm$realized))
m11_XMR <- MAPE(CrCu_XMR_VaR_Norm$`alpha(5%)`, abs(CrCu_XMR_VaR_Norm$realized))
m12_XMR <- MAPE(CrCu_XMR_VaR_Norm$`alpha(10%)`, abs(CrCu_XMR_VaR_Norm$realized))

m13_XMR <- MP_XMR[3]
m14_XMR <- MP_XMR[2]
m15_XMR <- MP_XMR[1]

MAPEval_XMR  <- matrix(data=0,nrow=5,ncol=3)
colnames(MAPEval_XMR) <- c("1%", "5%", "10%")
rownames(MAPEval_XMR) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm", "eGarch-Pearson")

MAPEval_XMR[,1] = cbind(m1_XMR,m4_XMR,m7_XMR,m10_XMR,m13_XMR)
MAPEval_XMR[,2] = cbind(m2_XMR,m5_XMR,m8_XMR,m11_XMR,m14_XMR)
MAPEval_XMR[,3] = cbind(m3_XMR,m6_XMR,m9_XMR,m12_XMR,m15_XMR)

xtable(MAPEval_XMR, caption = "MAPE for out of sample forecast of Monero Value at Risk - Full Sample", digits = 4)

signific_levels <- c(0.01,0.05,0.10)

##### Backtesting
XMR_Test_SST_1  <- BacktestVaR(CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,1],signific_levels[1])
XMR_Test_SST_5  <- BacktestVaR(CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,2],signific_levels[2])
XMR_Test_SST_10 <- BacktestVaR(CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,3],signific_levels[3])

XMR_Test_SGED_1  <- BacktestVaR(CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,1],signific_levels[1])
XMR_Test_SGED_5  <- BacktestVaR(CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,2],signific_levels[2])
XMR_Test_SGED_10 <- BacktestVaR(CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,3],signific_levels[3])

XMR_Test_JSU_1  <- BacktestVaR(CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,1],signific_levels[1])
XMR_Test_JSU_5  <- BacktestVaR(CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,2],signific_levels[2])
XMR_Test_JSU_10 <- BacktestVaR(CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,3],signific_levels[3])

XMR_Test_Norm_1  <- BacktestVaR(CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,1],signific_levels[1])
XMR_Test_Norm_5  <- BacktestVaR(CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,2],signific_levels[2])
XMR_Test_Norm_10 <- BacktestVaR(CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,3],signific_levels[3])

#### Saving the test stats and pvalues for 1%, 5% and 10% VaR

## 1%

XMR_Backtest_teststat1  <- matrix(c(XMR_Test_SST_1$LRuc[1],XMR_Test_SST_1$LRcc[1],XMR_Test_SST_1$DQ$stat,
                                    XMR_Test_SGED_1$LRuc[1],XMR_Test_SGED_1$LRcc[1],XMR_Test_SGED_1$DQ$stat,
                                    XMR_Test_JSU_1$LRuc[1],XMR_Test_JSU_1$LRcc[1],XMR_Test_JSU_1$DQ$stat,
                                    XMR_Test_Norm_1$LRuc[1],XMR_Test_Norm_1$LRcc[1],XMR_Test_Norm_1$DQ$stat),nrow=4,byrow = TRUE)
rownames(XMR_Backtest_teststat1)  <- c("SST","SGED","JSU","Norm")
colnames(XMR_Backtest_teststat1)  <- c("Kupiec","Christoffersen","DQ")

XMR_Backtest_pvalues1  <- matrix(c(XMR_Test_SST_1$LRuc[2],XMR_Test_SST_1$LRcc[2],XMR_Test_SST_1$DQ$pvalue,
                                   XMR_Test_SGED_1$LRuc[2],XMR_Test_SGED_1$LRcc[2],XMR_Test_SGED_1$DQ$pvalue,
                                   XMR_Test_JSU_1$LRuc[2],XMR_Test_JSU_1$LRcc[2],XMR_Test_JSU_1$DQ$pvalue,
                                   XMR_Test_Norm_1$LRuc[2],XMR_Test_Norm_1$LRcc[2],XMR_Test_Norm_1$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(XMR_Backtest_pvalues1)  <- c("SST","SGED","JSU","Norm")
colnames(XMR_Backtest_pvalues1)  <- c("Kupiec","Christoffersen","DQ")

### 5%

XMR_Backtest_teststat5  <- matrix(c(XMR_Test_SST_5$LRuc[1],XMR_Test_SST_5$LRcc[1],XMR_Test_SST_5$DQ$stat,
                                    XMR_Test_SGED_5$LRuc[1],XMR_Test_SGED_5$LRcc[1],XMR_Test_SGED_5$DQ$stat,
                                    XMR_Test_JSU_5$LRuc[1],XMR_Test_JSU_5$LRcc[1],XMR_Test_JSU_5$DQ$stat,
                                    XMR_Test_Norm_5$LRuc[1],XMR_Test_Norm_5$LRcc[1],XMR_Test_Norm_5$DQ$stat),nrow=4,byrow = TRUE)
rownames(XMR_Backtest_teststat5)  <- c("SST","SGED","JSU","Norm")
colnames(XMR_Backtest_teststat5)  <- c("Kupiec","Christoffersen","DQ")

XMR_Backtest_pvalues5  <- matrix(c(XMR_Test_SST_5$LRuc[2],XMR_Test_SST_5$LRcc[2],XMR_Test_SST_5$DQ$pvalue,
                                   XMR_Test_SGED_5$LRuc[2],XMR_Test_SGED_5$LRcc[2],XMR_Test_SGED_5$DQ$pvalue,
                                   XMR_Test_JSU_5$LRuc[2],XMR_Test_JSU_5$LRcc[2],XMR_Test_JSU_5$DQ$pvalue,
                                   XMR_Test_Norm_5$LRuc[2],XMR_Test_Norm_5$LRcc[2],XMR_Test_Norm_5$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(XMR_Backtest_pvalues5)  <- c("SST","SGED","JSU","Norm")
colnames(XMR_Backtest_pvalues5)  <- c("Kupiec","Christoffersen","DQ")

### 10%

XMR_Backtest_teststat10  <- matrix(c(XMR_Test_SST_10$LRuc[1],XMR_Test_SST_10$LRcc[1],XMR_Test_SST_10$DQ$stat,
                                     XMR_Test_SGED_10$LRuc[1],XMR_Test_SGED_10$LRcc[1],XMR_Test_SGED_10$DQ$stat,
                                     XMR_Test_JSU_10$LRuc[1],XMR_Test_JSU_10$LRcc[1],XMR_Test_JSU_10$DQ$stat,
                                     XMR_Test_Norm_10$LRuc[1],XMR_Test_Norm_10$LRcc[1],XMR_Test_Norm_10$DQ$stat),nrow=4,byrow = TRUE)
rownames(XMR_Backtest_teststat10)  <- c("SST","SGED","JSU","Norm")
colnames(XMR_Backtest_teststat10)  <- c("Kupiec","Christoffersen","DQ")

XMR_Backtest_pvalues10  <- matrix(c(XMR_Test_SST_10$LRuc[2],XMR_Test_SST_10$LRcc[2],XMR_Test_SST_10$DQ$pvalue,
                                    XMR_Test_SGED_10$LRuc[2],XMR_Test_SGED_10$LRcc[2],XMR_Test_SGED_10$DQ$pvalue,
                                    XMR_Test_JSU_10$LRuc[2],XMR_Test_JSU_10$LRcc[2],XMR_Test_JSU_10$DQ$pvalue,
                                    XMR_Test_Norm_10$LRuc[2],XMR_Test_Norm_10$LRcc[2],XMR_Test_Norm_10$DQ$pvalue),nrow=4,byrow = TRUE)
rownames(XMR_Backtest_pvalues10)  <- c("SST","SGED","JSU","Norm")
colnames(XMR_Backtest_pvalues10)  <- c("Kupiec","Christoffersen","DQ")

XMR_Backtest_pvalues1  <- round(XMR_Backtest_pvalues1,4)
XMR_Backtest_pvalues5  <- round(XMR_Backtest_pvalues5,4)
XMR_Backtest_pvalues10 <- round(XMR_Backtest_pvalues10,4)

XMR_Backtest_teststat1  <- round(XMR_Backtest_teststat1,4)
XMR_Backtest_teststat5  <- round(XMR_Backtest_teststat5,4)
XMR_Backtest_teststat10 <- round(XMR_Backtest_teststat10,4)

#####################################################################################
### Using VaRTest and VaRDuration Test individually ###
## VaRTest in R

CrCu_XMR_Norm_VaR_Test1  <- VaRTest(0.01,CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,1])
CrCu_XMR_Norm_VaR_Test5  <- VaRTest(0.05,CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,2])
CrCu_XMR_Norm_VaR_Test10 <- VaRTest(0.1,CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,3])

CrCu_XMR_JSU_VaR_Test1  <- VaRTest(0.01,CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,1])
CrCu_XMR_JSU_VaR_Test5  <- VaRTest(0.05,CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,2])
CrCu_XMR_JSU_VaR_Test10 <- VaRTest(0.1,CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,3])

CrCu_XMR_SGED_VaR_Test1  <- VaRTest(0.01,CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,1])
CrCu_XMR_SGED_VaR_Test5  <- VaRTest(0.05,CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,2])
CrCu_XMR_SGED_VaR_Test10 <- VaRTest(0.1,CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,3])

CrCu_XMR_SST_VaR_Test1  <- VaRTest(0.01,CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,1])
CrCu_XMR_SST_VaR_Test5  <- VaRTest(0.05,CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,2])
CrCu_XMR_SST_VaR_Test10 <- VaRTest(0.1,CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,3])

# ::::::::::: number of exceedances
Backtestres_XMR <- matrix(data = 0, nrow = 12, ncol = 2)

datavar_XMR <- c(CrCu_XMR_SST_VaR_Test1$expected.exceed, CrCu_XMR_SST_VaR_Test1$actual.exceed, CrCu_XMR_SST_VaR_Test5$expected.exceed, CrCu_XMR_SST_VaR_Test5$actual.exceed, CrCu_XMR_SST_VaR_Test10$expected.exceed, CrCu_XMR_SST_VaR_Test10$actual.exceed, 
                 CrCu_XMR_JSU_VaR_Test1$expected.exceed, CrCu_XMR_JSU_VaR_Test1$actual.exceed, CrCu_XMR_JSU_VaR_Test5$expected.exceed, CrCu_XMR_JSU_VaR_Test5$actual.exceed, CrCu_XMR_JSU_VaR_Test10$expected.exceed, CrCu_XMR_JSU_VaR_Test10$actual.exceed,
                 CrCu_XMR_SGED_VaR_Test1$expected.exceed, CrCu_XMR_SGED_VaR_Test1$actual.exceed, CrCu_XMR_SGED_VaR_Test5$expected.exceed, CrCu_XMR_SGED_VaR_Test5$actual.exceed, CrCu_XMR_SGED_VaR_Test10$expected.exceed, CrCu_XMR_SGED_VaR_Test10$actual.exceed,
                 CrCu_XMR_Norm_VaR_Test1$expected.exceed, CrCu_XMR_Norm_VaR_Test1$actual.exceed, CrCu_XMR_Norm_VaR_Test5$expected.exceed, CrCu_XMR_Norm_VaR_Test1$actual.exceed, CrCu_XMR_Norm_VaR_Test10$expected.exceed, CrCu_XMR_Norm_VaR_Test10$actual.exceed)

Backtestres_XMR <- matrix(data = datavar_XMR, nrow = 12, ncol = 2)
rownames(Backtestres_XMR) <- c("SST 1%", "SST 5%", "SST 10%", "JSU 1%", "JSU 5%", "JSU 10%", "SGED 1%", "SGED 5%", "SGED 10%", "Norm 1%", "Norm 5%", "Norm 10%")
colnames(Backtestres_XMR) <- c("Expected Exceedances", "Actual Exceedances")

xtable(Backtestres_XMR, caption = "Exceedances for Value at Risk Backtest for Monero - Full Sample", digits = 0)

## Store the pvalues at a place

CrCu_XMR_VaR_Test_pvalues1 <- matrix(c(CrCu_XMR_SST_VaR_Test1$uc.LRp,CrCu_XMR_SST_VaR_Test1$cc.LRp,
                                       CrCu_XMR_SGED_VaR_Test1$uc.LRp,CrCu_XMR_SGED_VaR_Test1$cc.LRp,
                                       CrCu_XMR_JSU_VaR_Test1$uc.LRp,CrCu_XMR_JSU_VaR_Test1$cc.LRp,
                                       CrCu_XMR_Norm_VaR_Test1$uc.LRp,CrCu_XMR_Norm_VaR_Test1$cc.LRp),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_pvalues5 <- matrix(c(CrCu_XMR_SST_VaR_Test5$uc.LRp,CrCu_XMR_SST_VaR_Test5$cc.LRp,
                                       CrCu_XMR_SGED_VaR_Test5$uc.LRp,CrCu_XMR_SGED_VaR_Test5$cc.LRp,
                                       CrCu_XMR_JSU_VaR_Test5$uc.LRp,CrCu_XMR_JSU_VaR_Test5$cc.LRp,
                                       CrCu_XMR_Norm_VaR_Test5$uc.LRp,CrCu_XMR_Norm_VaR_Test5$cc.LRp),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_pvalues10 <- matrix(c(CrCu_XMR_SST_VaR_Test10$uc.LRp,CrCu_XMR_SST_VaR_Test10$cc.LRp,
                                        CrCu_XMR_SGED_VaR_Test10$uc.LRp,CrCu_XMR_SGED_VaR_Test10$cc.LRp,
                                        CrCu_XMR_JSU_VaR_Test10$uc.LRp,CrCu_XMR_JSU_VaR_Test10$cc.LRp,
                                        CrCu_XMR_Norm_VaR_Test10$uc.LRp,CrCu_XMR_Norm_VaR_Test10$cc.LRp),ncol=2,byrow = TRUE)

## Storing the test stats

CrCu_XMR_VaR_Test_teststat1 <- matrix(c(CrCu_XMR_SST_VaR_Test1$uc.LRstat,CrCu_XMR_SST_VaR_Test1$cc.LRstat,
                                        CrCu_XMR_SGED_VaR_Test1$uc.LRstat,CrCu_XMR_SGED_VaR_Test1$cc.LRstat,
                                        CrCu_XMR_JSU_VaR_Test1$uc.LRstat,CrCu_XMR_JSU_VaR_Test1$cc.LRstat,
                                        CrCu_XMR_Norm_VaR_Test1$uc.LRstat,CrCu_XMR_Norm_VaR_Test1$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_teststat5 <- matrix(c(CrCu_XMR_SST_VaR_Test5$uc.LRstat,CrCu_XMR_SST_VaR_Test5$cc.LRstat,
                                        CrCu_XMR_SGED_VaR_Test5$uc.LRstat,CrCu_XMR_SGED_VaR_Test5$cc.LRstat,
                                        CrCu_XMR_JSU_VaR_Test5$uc.LRstat,CrCu_XMR_JSU_VaR_Test5$cc.LRstat,
                                        CrCu_XMR_Norm_VaR_Test5$uc.LRstat,CrCu_XMR_Norm_VaR_Test5$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_teststat10 <- matrix(c(CrCu_XMR_SST_VaR_Test10$uc.LRstat,CrCu_XMR_SST_VaR_Test10$cc.LRstat,
                                         CrCu_XMR_SGED_VaR_Test10$uc.LRstat,CrCu_XMR_SGED_VaR_Test10$cc.LRstat,
                                         CrCu_XMR_JSU_VaR_Test10$uc.LRstat,CrCu_XMR_JSU_VaR_Test10$cc.LRstat,
                                         CrCu_XMR_Norm_VaR_Test10$uc.LRstat,CrCu_XMR_Norm_VaR_Test10$cc.LRstat),ncol=2,byrow = TRUE)
## Round them up to 4 decimals

CrCu_XMR_VaR_Test_pvalues1  <- round(CrCu_XMR_VaR_Test_pvalues1,4)
CrCu_XMR_VaR_Test_pvalues5  <- round(CrCu_XMR_VaR_Test_pvalues5,4)
CrCu_XMR_VaR_Test_pvalues10 <- round(CrCu_XMR_VaR_Test_pvalues10,4)

CrCu_XMR_VaR_Test_teststat1  <- round(CrCu_XMR_VaR_Test_teststat1,4)
CrCu_XMR_VaR_Test_teststat5  <- round(CrCu_XMR_VaR_Test_teststat5,4)
CrCu_XMR_VaR_Test_teststat10 <- round(CrCu_XMR_VaR_Test_teststat10,4)

rownames(CrCu_XMR_VaR_Test_pvalues1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_pvalues1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_XMR_VaR_Test_pvalues5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_pvalues5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_XMR_VaR_Test_pvalues10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_pvalues10)  <- c("Kupiec","Christoffersen")

rownames(CrCu_XMR_VaR_Test_teststat1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_teststat1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_XMR_VaR_Test_teststat5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_teststat5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_XMR_VaR_Test_teststat10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_teststat10)  <- c("Kupiec","Christoffersen")
# Kupiec H0: observed failure rate is equal to the failure suggested by tests
# Christoffersen H0: Exceedances are not clustering or no consecutive exceedances
# Fail to reject H0 is ideal
# Based on above, H0 is rejected only in case of Normal 1%, so good results

## VaR Duration test

CrCu_XMR_Norm_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,1])
CrCu_XMR_Norm_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,2])
CrCu_XMR_Norm_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,3])

CrCu_XMR_JSU_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,1])
CrCu_XMR_JSU_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,2])
CrCu_XMR_JSU_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,3])

CrCu_XMR_SGED_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,1])
CrCu_XMR_SGED_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,2])
CrCu_XMR_SGED_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,3])

CrCu_XMR_SST_VaR_Dur_Test1  <- VaRDurTest(0.01,CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,1])
CrCu_XMR_SST_VaR_Dur_Test5  <- VaRDurTest(0.05,CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,2])
CrCu_XMR_SST_VaR_Dur_Test10 <- VaRDurTest(0.1,CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,3])

### Store the test results and p=values

CrCu_XMR_VaR_Dur_Test_pvalues1 <- matrix(c(CrCu_XMR_SST_VaR_Dur_Test1$LRp,CrCu_XMR_SGED_VaR_Dur_Test1$LRp,
                                           CrCu_XMR_JSU_VaR_Dur_Test1$LRp,CrCu_XMR_Norm_VaR_Dur_Test1$LRp),nrow=4,byrow = TRUE)
CrCu_XMR_VaR_Dur_Test_pvalues5 <- matrix(c(CrCu_XMR_SST_VaR_Dur_Test5$LRp,CrCu_XMR_SGED_VaR_Dur_Test5$LRp,
                                           CrCu_XMR_JSU_VaR_Dur_Test5$LRp,CrCu_XMR_Norm_VaR_Dur_Test5$LRp),nrow=4,byrow = TRUE)
CrCu_XMR_VaR_Dur_Test_pvalues10 <- matrix(c(CrCu_XMR_SST_VaR_Dur_Test10$LRp,CrCu_XMR_SGED_VaR_Dur_Test10$LRp,
                                            CrCu_XMR_JSU_VaR_Dur_Test10$LRp,CrCu_XMR_Norm_VaR_Dur_Test10$LRp),nrow=4,byrow = TRUE)

CrCu_XMR_VaR_Dur_Test_teststat1 <- matrix(c(CrCu_XMR_SST_VaR_Dur_Test1$uLL,CrCu_XMR_SST_VaR_Dur_Test1$rLL,
                                            CrCu_XMR_SGED_VaR_Dur_Test1$uLL,CrCu_XMR_SGED_VaR_Dur_Test1$rLL,
                                            CrCu_XMR_JSU_VaR_Dur_Test1$uLL,CrCu_XMR_JSU_VaR_Dur_Test1$rLL,
                                            CrCu_XMR_Norm_VaR_Dur_Test1$uLL,CrCu_XMR_Norm_VaR_Dur_Test1$rLL),nrow=4,byrow = TRUE)
CrCu_XMR_VaR_Dur_Test_teststat5 <- matrix(c(CrCu_XMR_SST_VaR_Dur_Test5$uLL,CrCu_XMR_SST_VaR_Dur_Test5$rLL,
                                            CrCu_XMR_SGED_VaR_Dur_Test5$uLL,CrCu_XMR_SGED_VaR_Dur_Test5$rLL,
                                            CrCu_XMR_JSU_VaR_Dur_Test5$uLL,CrCu_XMR_JSU_VaR_Dur_Test5$rLL,
                                            CrCu_XMR_Norm_VaR_Dur_Test5$uLL,CrCu_XMR_Norm_VaR_Dur_Test5$rLL),nrow=4,byrow = TRUE)
CrCu_XMR_VaR_Dur_Test_teststat10 <- matrix(c(CrCu_XMR_SST_VaR_Dur_Test10$uLL,CrCu_XMR_SST_VaR_Dur_Test10$rLL,
                                             CrCu_XMR_SGED_VaR_Dur_Test10$uLL,CrCu_XMR_SGED_VaR_Dur_Test10$rLL,
                                             CrCu_XMR_JSU_VaR_Dur_Test10$uLL,CrCu_XMR_JSU_VaR_Dur_Test10$rLL,
                                             CrCu_XMR_Norm_VaR_Dur_Test10$uLL,CrCu_XMR_Norm_VaR_Dur_Test10$rLL),nrow=4,byrow = TRUE)

## Round them up to 4 decimals

CrCu_XMR_VaR_Dur_Test_pvalues1  <- round(CrCu_XMR_VaR_Dur_Test_pvalues1,4)
CrCu_XMR_VaR_Dur_Test_pvalues5  <- round(CrCu_XMR_VaR_Dur_Test_pvalues5,4)
CrCu_XMR_VaR_Dur_Test_pvalues10 <- round(CrCu_XMR_VaR_Dur_Test_pvalues10,4)

CrCu_XMR_VaR_Dur_Test_teststat1  <- round(CrCu_XMR_VaR_Dur_Test_teststat1,4)
CrCu_XMR_VaR_Dur_Test_teststat5  <- round(CrCu_XMR_VaR_Dur_Test_teststat5,4)
CrCu_XMR_VaR_Dur_Test_teststat10 <- round(CrCu_XMR_VaR_Dur_Test_teststat10,4)

rownames(CrCu_XMR_VaR_Dur_Test_pvalues1)  <- c("SST","SGED","JSU","Norm")
rownames(CrCu_XMR_VaR_Dur_Test_pvalues5)  <- c("SST","SGED","JSU","Norm")
rownames(CrCu_XMR_VaR_Dur_Test_pvalues10)  <- c("SST","SGED","JSU","Norm")

rownames(CrCu_XMR_VaR_Dur_Test_teststat1)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_XMR_VaR_Dur_Test_teststat1)  <- c("Unrestricted","Restricted")
rownames(CrCu_XMR_VaR_Dur_Test_teststat5)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_XMR_VaR_Dur_Test_teststat5)  <- c("Unrestricted","Restricted")
rownames(CrCu_XMR_VaR_Dur_Test_teststat10)  <- c("SST","SGED","JSU","Norm")
colnames(CrCu_XMR_VaR_Dur_Test_teststat10)  <- c("Unrestricted","Restricted")
# H0: Correctly specified risk model
# H0: Duration Between Exceedances have no memory
# Duration of time between VaR violations should ideally be independent and not clustered
# Fail to reject H0 in all case, which is good

# ___________________________________________________________________________________________________ #

#### ::::::::::::::::: S U B S A M P L E Analysis ::::::::::::::::::::: ####

### ::::::::::::Descriptive Statistics and Basic Tests of **SUBSAMPLES** ::::::::::::::::: ###

Descr_CrCu_XMR   <- cbind(basicStats(CrCu_XMR_sub1),basicStats(CrCu_XMR_sub2),basicStats(CrCu_XMR_sub3))
# To check evidence of unit root
ADF_CrCu_XMR   <- cbind(adf.test(CrCu_XMR_sub1),adf.test(CrCu_XMR_sub2),adf.test(CrCu_XMR_sub3))
# To check evidence of unit root
pp_CrCu_XMR   <- cbind(pp.test(CrCu_XMR_sub1),pp.test(CrCu_XMR_sub2),pp.test(CrCu_XMR_sub3))
# To check assumption of normality
jb_CrCu_XMR   <- cbind(jarque.bera.test(CrCu_XMR_sub1),jarque.bera.test(CrCu_XMR_sub2),jarque.bera.test(CrCu_XMR_sub3))
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation
LQ_CrCu_XMR01   <- cbind(Box.test(CrCu_XMR_sub1^2,lag=1,type="Lj"),Box.test(CrCu_XMR_sub2^2,lag=1,type="Lj"),Box.test(CrCu_XMR_sub3^2,lag=1,type="Lj"))
LQ_CrCu_XMR05   <- cbind(Box.test(CrCu_XMR_sub1^2,lag=5,type="Lj"),Box.test(CrCu_XMR_sub2^2,lag=5,type="Lj"),Box.test(CrCu_XMR_sub3^2,lag=5,type="Lj"))
LQ_CrCu_XMR10   <- cbind(Box.test(CrCu_XMR_sub1^2,lag=10,type="Lj"),Box.test(CrCu_XMR_sub2^2,lag=10,type="Lj"),Box.test(CrCu_XMR_sub3^2,lag=10,type="Lj"))

## :::::::: Subsamples
ArchTest(CrCu_XMR_sub1, lags = 3)
ArchTest(CrCu_XMR_sub2, lags = 3)
ArchTest(CrCu_XMR_sub3, lags = 3)
# H0: No ARCH effect
# DEFINITELY ARCH EFFECTS EXIST
arch.test(CrCu_XMR_sub1, lags.single = 3)

## plot squared returns
par(mfrow = c(1,1))
pacf(CrCu_XMR_sub1^2)
pacf(CrCu_XMR_sub2^2)
pacf(CrCu_XMR_sub3^2)

acf(CrCu_XMR_sub1^2)
acf(CrCu_XMR_sub2^2)
acf(CrCu_XMR_sub3^2)

acf(CrCu_XMR_sub1)
acf(CrCu_XMR_sub2)
acf(CrCu_XMR_sub3)

#### JSU specification

CrCu_XMR_Spec_Sample1_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_XMR_Spec_Sample2_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_XMR_Spec_Sample3_JSU <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")

CrCu_XMR_Model_Sample_JSU1 <- ugarchfit(CrCu_XMR_Spec_Sample1_JSU,CrCu_XMR_sub1,out.sample = 60)
CrCu_XMR_Model_Sample_JSU2 <- ugarchfit(CrCu_XMR_Spec_Sample2_JSU,CrCu_XMR_sub2,out.sample = 200)
CrCu_XMR_Model_Sample_JSU3 <- ugarchfit(CrCu_XMR_Spec_Sample3_JSU,CrCu_XMR_sub3,out.sample = 100)

## Rollover Analysis

CrCu_XMR_Roll_JSU_Sample1 <-  ugarchroll(CrCu_XMR_Spec_Sample1_JSU,CrCu_XMR_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll_JSU_Sample2 <-  ugarchroll(CrCu_XMR_Spec_Sample2_JSU,CrCu_XMR_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll_JSU_Sample3 <-  ugarchroll(CrCu_XMR_Spec_Sample3_JSU,CrCu_XMR_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# ### SGED specification

CrCu_XMR_Spec_Sample1_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_XMR_Spec_Sample2_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_XMR_Spec_Sample3_SGED <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")

CrCu_XMR_Model_Sample_SGED1 <- ugarchfit(CrCu_XMR_Spec_Sample1_SGED,CrCu_XMR_sub1,out.sample = 60)
CrCu_XMR_Model_Sample_SGED2 <- ugarchfit(CrCu_XMR_Spec_Sample2_SGED,CrCu_XMR_sub2,out.sample = 200)
CrCu_XMR_Model_Sample_SGED3 <- ugarchfit(CrCu_XMR_Spec_Sample3_SGED,CrCu_XMR_sub3,out.sample = 100)

## Rollover Analysis

CrCu_XMR_Roll_SGED_Sample1 <-  ugarchroll(CrCu_XMR_Spec_Sample1_SGED,CrCu_XMR_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll_SGED_Sample2 <-  ugarchroll(CrCu_XMR_Spec_Sample2_SGED,CrCu_XMR_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll_SGED_Sample3 <-  ugarchroll(CrCu_XMR_Spec_Sample3_SGED,CrCu_XMR_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

### SST Specification

CrCu_XMR_Spec_Sample1_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_XMR_Spec_Sample2_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_XMR_Spec_Sample3_SST <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")

CrCu_XMR_Model_Sample_SST1 <- ugarchfit(CrCu_XMR_Spec_Sample1_SST,CrCu_XMR_sub1,out.sample = 60)
CrCu_XMR_Model_Sample_SST2 <- ugarchfit(CrCu_XMR_Spec_Sample2_SST,CrCu_XMR_sub2,out.sample = 200)
CrCu_XMR_Model_Sample_SST3 <- ugarchfit(CrCu_XMR_Spec_Sample3_SST,CrCu_XMR_sub3,out.sample = 100)

## Rollover Analysis

CrCu_XMR_Roll_SST_Sample1 <-  ugarchroll(CrCu_XMR_Spec_Sample1_SST,CrCu_XMR_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll_SST_Sample2 <-  ugarchroll(CrCu_XMR_Spec_Sample2_SST,CrCu_XMR_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll_SST_Sample3 <-  ugarchroll(CrCu_XMR_Spec_Sample3_SST,CrCu_XMR_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# ### Normal Specification

CrCu_XMR_Spec_Sample1_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")
CrCu_XMR_Spec_Sample2_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")
CrCu_XMR_Spec_Sample3_Normal <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_XMR_Model_Sample_Normal1 <- ugarchfit(CrCu_XMR_Spec_Sample1_Normal,CrCu_XMR_sub1,out.sample = 60)
CrCu_XMR_Model_Sample_Normal2 <- ugarchfit(CrCu_XMR_Spec_Sample2_Normal,CrCu_XMR_sub2,out.sample = 200)
CrCu_XMR_Model_Sample_Normal3 <- ugarchfit(CrCu_XMR_Spec_Sample3_Normal,CrCu_XMR_sub3,out.sample = 100)

## Rollover Analysis

CrCu_XMR_Roll_Normal_Sample1 <-  ugarchroll(CrCu_XMR_Spec_Sample1_Normal,CrCu_XMR_sub1,forecast.length = 60,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll_Normal_Sample2 <-  ugarchroll(CrCu_XMR_Spec_Sample2_Normal,CrCu_XMR_sub2,forecast.length = 200,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll_Normal_Sample3 <-  ugarchroll(CrCu_XMR_Spec_Sample3_Normal,CrCu_XMR_sub3,forecast.length = 100,refit.every = 20,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

## persistence

Persis_sub_XMR <- c(persistence(CrCu_XMR_Model_Sample_JSU1), persistence(CrCu_XMR_Model_Sample_JSU2), persistence(CrCu_XMR_Model_Sample_JSU3),
                    persistence(CrCu_XMR_Model_Sample_SGED1), persistence(CrCu_XMR_Model_Sample_SGED2), persistence(CrCu_XMR_Model_Sample_SGED3),
                    persistence(CrCu_XMR_Model_Sample_SST1), persistence(CrCu_XMR_Model_Sample_SST2), persistence(CrCu_XMR_Model_Sample_SST3),
                    persistence(CrCu_XMR_Model_Sample_Normal1), persistence(CrCu_XMR_Model_Sample_Normal2), persistence(CrCu_XMR_Model_Sample_Normal3))

persis_XMR_sub <- matrix(data=Persis_sub_XMR, nrow = 3, ncol = 4)
colnames(persis_XMR_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(persis_XMR_sub) <- c("SubSample1", "SubSample2", "SubSample3")
xtable(persis_XMR_sub, caption = "Persistence of Monero SubSamples", digits = 4)

### Store the loglikelihood values, AIC and BIC ####

Likelihood_sub_XMR <- c(CrCu_XMR_Model_Sample_JSU1@fit$LLH, CrCu_XMR_Model_Sample_JSU2@fit$LLH, CrCu_XMR_Model_Sample_JSU3@fit$LLH,
                        CrCu_XMR_Model_Sample_SGED1@fit$LLH, CrCu_XMR_Model_Sample_SGED2@fit$LLH, CrCu_XMR_Model_Sample_SGED3@fit$LLH,
                        CrCu_XMR_Model_Sample_SST1@fit$LLH, CrCu_XMR_Model_Sample_SST2@fit$LLH, CrCu_XMR_Model_Sample_SST3@fit$LLH,
                        CrCu_XMR_Model_Sample_Normal1@fit$LLH, CrCu_XMR_Model_Sample_Normal2@fit$LLH, CrCu_XMR_Model_Sample_Normal3@fit$LLH)

Llhood_sub_XMR <- matrix(data = Likelihood_sub_XMR, nrow = 3, ncol = 4)
colnames(Llhood_sub_XMR) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(Llhood_sub_XMR) <- c("SubSample1", "SubSample2", "SubSample3")

bic_sub_XMR <- c(infocriteria(CrCu_XMR_Model_Sample_JSU1)[2], infocriteria(CrCu_XMR_Model_Sample_JSU2)[2], infocriteria(CrCu_XMR_Model_Sample_JSU3)[2],
                 infocriteria(CrCu_XMR_Model_Sample_SGED1)[2], infocriteria(CrCu_XMR_Model_Sample_SGED2)[2], infocriteria(CrCu_XMR_Model_Sample_SGED3)[2],
                 infocriteria(CrCu_XMR_Model_Sample_SST1)[2], infocriteria(CrCu_XMR_Model_Sample_SST2)[2], infocriteria(CrCu_XMR_Model_Sample_SST3)[2],
                 infocriteria(CrCu_XMR_Model_Sample_Normal1)[2], infocriteria(CrCu_XMR_Model_Sample_Normal2)[2], infocriteria(CrCu_XMR_Model_Sample_Normal3)[2])

bic_XMR_sub <- matrix(data=bic_sub_XMR, nrow = 3, ncol = 4)
colnames(bic_XMR_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(bic_XMR_sub) <- c("SubSample1", "SubSample2", "SubSample3")

aic_sub_XMR <- c(infocriteria(CrCu_XMR_Model_Sample_JSU1)[1], infocriteria(CrCu_XMR_Model_Sample_JSU2)[1], infocriteria(CrCu_XMR_Model_Sample_JSU3)[1],
                 infocriteria(CrCu_XMR_Model_Sample_SGED1)[1], infocriteria(CrCu_XMR_Model_Sample_SGED2)[1], infocriteria(CrCu_XMR_Model_Sample_SGED3)[1],
                 infocriteria(CrCu_XMR_Model_Sample_SST1)[1], infocriteria(CrCu_XMR_Model_Sample_SST2)[1], infocriteria(CrCu_XMR_Model_Sample_SST3)[1],
                 infocriteria(CrCu_XMR_Model_Sample_Normal1)[1], infocriteria(CrCu_XMR_Model_Sample_Normal2)[1], infocriteria(CrCu_XMR_Model_Sample_Normal3)[1])

aic_XMR_sub <- matrix(data=aic_sub_XMR, nrow = 3, ncol = 4)
colnames(aic_XMR_sub) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
rownames(aic_XMR_sub) <- c("SubSample1", "SubSample2", "SubSample3")

Infocrit_sub_XMR  <- cbind(Llhood_sub_XMR, bic_XMR_sub, aic_XMR_sub)
colnames(Infocrit_sub_XMR) <- c("Llhood:eGarch-JSU","Llhood:eGarch-SGED","Llhood:eGarch-SST","Llhood:eGarch-Normal", 
                                "BIC:eGarch-JSU","BIC:eGarch-SGED","BIC:eGarch-SST","BIC:eGarch-Normal", 
                                "AIC:eGarch-JSU","AIC:eGarch-SGED","AIC:eGarch-SST","AIC:eGarch-Normal")
Infocrit_sub1_XMR <- t(Infocrit_sub_XMR)
View(data.frame(Infocrit_sub1_XMR))
xtable(Infocrit_sub1_XMR, caption = "Information Criteria for Monero GARCH models - SubSamples", digits = 4)

## Conclusion: SS1 - eGarch JSU, SS2 - eGarch SGED, SS3 - eGarch SST##

## coefficients

# Sample 1
coef_XMR_sub1_SGED <- round(coef(CrCu_XMR_Model_Sample_SGED1),4) # SS1 
coef_XMR_sub1_JSU <- round(coef(CrCu_XMR_Model_Sample_JSU1),4) # SS1 SELECTED
coef_XMR_sub1_SST <- round(coef(CrCu_XMR_Model_Sample_SST1),4) # SS1
coef_XMR_sub1_Normal <- round(coef(CrCu_XMR_Model_Sample_Normal1),4) # SS1

# Sample 2
coef_XMR_sub2_SGED <- round(coef(CrCu_XMR_Model_Sample_SGED2),4) # SS2 SELECTED
coef_XMR_sub2_JSU <- round(coef(CrCu_XMR_Model_Sample_JSU2),4) # SS2 
coef_XMR_sub2_SST <- round(coef(CrCu_XMR_Model_Sample_SST2),4) # SS2
coef_XMR_sub2_Normal <- round(coef(CrCu_XMR_Model_Sample_Normal2),4) # SS2

# Sample 3
coef_XMR_sub3_SGED <- round(coef(CrCu_XMR_Model_Sample_SGED3),4) # SS3
coef_XMR_sub3_JSU <- round(coef(CrCu_XMR_Model_Sample_JSU3),4) # SS3
coef_XMR_sub3_SST <- round(coef(CrCu_XMR_Model_Sample_SST3),4) # SS3 SELECTED
coef_XMR_sub3_Normal <- round(coef(CrCu_XMR_Model_Sample_Normal3),4) # SS3

garch_coef_XMR_fsub <- bind_rows(coef_XMR_7, coef_XMR_sub1_JSU, coef_XMR_sub2_SGED, coef_XMR_sub3_SST)

garch_coef_XMR1_fsub <- as.data.frame(garch_coef_XMR_fsub)
rownames(garch_coef_XMR1_fsub) <- c("FullSample-eGarchJSU","SS1-eGarchJSU","SS2-eGarchSGED","SS3-eGarchSST")
garch_coef_XMR1_fsubt <- (t(garch_coef_XMR1_fsub))
View(t(garch_coef_XMR1_fsub))
xtable(garch_coef_XMR1_fsubt, caption = "Coefficients of Monero GARCH models", digits = 4)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model

# Sample 1

garchsig_SGED1_XMR <- CrCu_XMR_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_XMR <- abs(CrCu_XMR_Roll_SGED_Sample1@forecast$density$Realized)
MSEval_SGED1_XMR <- MSE(garchsig_SGED1_XMR, realval_SGED1_XMR)
MAPEval_SGED1_XMR <- MAPE(garchsig_SGED1_XMR, realval_SGED1_XMR)

garchsig_JSU1_XMR <- CrCu_XMR_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_XMR <- abs(CrCu_XMR_Roll_JSU_Sample1@forecast$density$Realized)
MSEval_JSU1_XMR <- MSE(garchsig_JSU1_XMR, realval_JSU1_XMR)
MAPEval_JSU1_XMR <- MAPE(garchsig_JSU1_XMR, realval_JSU1_XMR)

garchsig_SST1_XMR <- CrCu_XMR_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_XMR <- abs(CrCu_XMR_Roll_SST_Sample1@forecast$density$Realized)
MSEval_SST1_XMR <- MSE(garchsig_SST1_XMR, realval_SST1_XMR)
MAPEval_SST1_XMR <- MAPE(garchsig_SST1_XMR, realval_SST1_XMR)

garchsig_Normal1_XMR <- CrCu_XMR_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_XMR <- abs(CrCu_XMR_Roll_Normal_Sample1@forecast$density$Realized)
MSEval_Normal1_XMR <- MSE(garchsig_Normal1_XMR, realval_Normal1_XMR)
MAPEval_Normal1_XMR <- MAPE(garchsig_Normal1_XMR, realval_Normal1_XMR)

# Sample 2

garchsig_SGED2_XMR <- CrCu_XMR_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_XMR <- abs(CrCu_XMR_Roll_SGED_Sample2@forecast$density$Realized)
MSEval_SGED2_XMR <- MSE(garchsig_SGED2_XMR, realval_SGED2_XMR)
MAPEval_SGED2_XMR <- MAPE(garchsig_SGED2_XMR, realval_SGED2_XMR)

garchsig_JSU2_XMR <- CrCu_XMR_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_XMR <- abs(CrCu_XMR_Roll_JSU_Sample2@forecast$density$Realized)
MSEval_JSU2_XMR <- MSE(garchsig_JSU2_XMR, realval_JSU2_XMR)
MAPEval_JSU2_XMR <- MAPE(garchsig_JSU2_XMR, realval_JSU2_XMR)

garchsig_SST2_XMR <- CrCu_XMR_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_XMR <- abs(CrCu_XMR_Roll_SST_Sample2@forecast$density$Realized)
MSEval_SST2_XMR <- MSE(garchsig_SST2_XMR, realval_SST2_XMR)
MAPEval_SST2_XMR <- MAPE(garchsig_SST2_XMR, realval_SST2_XMR)

garchsig_Normal2_XMR <- CrCu_XMR_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_XMR <- abs(CrCu_XMR_Roll_Normal_Sample2@forecast$density$Realized)
MSEval_Normal2_XMR <- MSE(garchsig_Normal2_XMR, realval_Normal2_XMR)
MAPEval_Normal2_XMR <- MAPE(garchsig_Normal2_XMR, realval_Normal2_XMR)

# Sample 3

garchsig_SGED3_XMR <- CrCu_XMR_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_XMR <- abs(CrCu_XMR_Roll_SGED_Sample3@forecast$density$Realized)
MSEval_SGED3_XMR <- MSE(garchsig_SGED3_XMR, realval_SGED3_XMR)
MAPEval_SGED3_XMR <- MAPE(garchsig_SGED3_XMR, realval_SGED3_XMR)

garchsig_JSU3_XMR <- CrCu_XMR_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_XMR <- abs(CrCu_XMR_Roll_JSU_Sample3@forecast$density$Realized)
MSEval_JSU3_XMR <- MSE(garchsig_JSU3_XMR, realval_JSU3_XMR)
MAPEval_JSU3_XMR <- MAPE(garchsig_JSU3_XMR, realval_JSU3_XMR)

garchsig_SST3_XMR <- CrCu_XMR_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_XMR <- abs(CrCu_XMR_Roll_SST_Sample3@forecast$density$Realized)
MSEval_SST3_XMR <- MSE(garchsig_SST3_XMR, realval_SST3_XMR)
MAPEval_SST3_XMR <- MAPE(garchsig_SST3_XMR, realval_SST3_XMR)

garchsig_Normal3_XMR <- CrCu_XMR_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_XMR <- abs(CrCu_XMR_Roll_Normal_Sample3@forecast$density$Realized)
MSEval_Normal3_XMR <- MSE(garchsig_Normal3_XMR, realval_Normal3_XMR)
MAPEval_Normal3_XMR <- MAPE(garchsig_Normal3_XMR, realval_Normal3_XMR)


MAPEgval_XMR_fsub  <- matrix(data=0,nrow=12,ncol=2)
colnames(MAPEgval_XMR_fsub) <- c("MSE", "MAPE")
rownames(MAPEgval_XMR_fsub) <- c("SS1-eGarchSGED","SS1-eGarchJSU","SS1-eGarchSST","SS1-eGarchNormal",
                                 "SS2-eGarchSGED","SS2-eGarchJSU","SS2-eGarchSST","SS2-eGarchNormal",
                                 "SS3-eGarchSGED","SS3-eGarchJSU","SS3-eGarchSST","SS3-eGarchNormal")

MAPEgval_XMR_fsub[,1] = cbind(MSEval_SGED1_XMR,MSEval_JSU1_XMR,MSEval_SST1_XMR,MSEval_Normal1_XMR,
                              MSEval_SGED2_XMR,MSEval_JSU2_XMR,MSEval_SST2_XMR,MSEval_Normal2_XMR,
                              MSEval_SGED3_XMR,MSEval_JSU3_XMR,MSEval_SST3_XMR,MSEval_Normal3_XMR)
MAPEgval_XMR_fsub[,2] = cbind(MAPEval_SGED1_XMR,MAPEval_JSU1_XMR,MAPEval_SST1_XMR,MAPEval_Normal1_XMR,
                              MAPEval_SGED2_XMR,MAPEval_JSU2_XMR,MAPEval_SST2_XMR,MAPEval_Normal2_XMR,
                              MAPEval_SGED3_XMR,MAPEval_JSU3_XMR,MAPEval_SST3_XMR,MAPEval_Normal3_XMR)

xtable(MAPEgval_XMR_fsub, caption = "MSE & MAPE for out of sample forecast for subsamples of Monero GARCH", digits = 4)

#### Store the VaR for the out sample using rolling window forecasts

# Sample 1 
CrCu_XMR_VaR_SGED1   <- CrCu_XMR_Roll_SGED_Sample1@forecast$VaR # SS1 SELECTED
CrCu_XMR_VaR_JSU1   <- CrCu_XMR_Roll_JSU_Sample1@forecast$VaR # SS1
CrCu_XMR_VaR_SST1   <- CrCu_XMR_Roll_SST_Sample1@forecast$VaR # SS1
CrCu_XMR_VaR_Normal1   <- CrCu_XMR_Roll_Normal_Sample1@forecast$VaR # SS1

# Sample 2
CrCu_XMR_VaR_SGED2   <- CrCu_XMR_Roll_SGED_Sample2@forecast$VaR # SS2 
CrCu_XMR_VaR_JSU2   <- CrCu_XMR_Roll_JSU_Sample2@forecast$VaR # SS2 SELECTED
CrCu_XMR_VaR_SST2   <- CrCu_XMR_Roll_SST_Sample2@forecast$VaR # SS2
CrCu_XMR_VaR_Normal2   <- CrCu_XMR_Roll_Normal_Sample2@forecast$VaR # SS2

# Sample 3
CrCu_XMR_VaR_SGED3   <- CrCu_XMR_Roll_SGED_Sample3@forecast$VaR # SS3 
CrCu_XMR_VaR_JSU3   <- CrCu_XMR_Roll_JSU_Sample3@forecast$VaR # SS3 
CrCu_XMR_VaR_SST3   <- CrCu_XMR_Roll_SST_Sample3@forecast$VaR # SS3 SELECTED
CrCu_XMR_VaR_Normal3   <- CrCu_XMR_Roll_Normal_Sample3@forecast$VaR # SS3

### Ljung-box test statistic

# Sample 1

CrCu_XMR1_JSU1  <- Box.test(CrCu_XMR_Model_Sample_JSU1@fit$residuals/CrCu_XMR_Model_Sample_JSU1@fit$sigma,lag=1,type="Lj")
CrCu_XMR1_JSU5  <- Box.test(CrCu_XMR_Model_Sample_JSU1@fit$residuals/CrCu_XMR_Model_Sample_JSU1@fit$sigma,lag=5,type="Lj")
CrCu_XMR1_JSU10  <- Box.test(CrCu_XMR_Model_Sample_JSU1@fit$residuals/CrCu_XMR_Model_Sample_JSU1@fit$sigma,lag=10,type="Lj")

CrCu_XMR1_SST1  <- Box.test(CrCu_XMR_Model_Sample_SST1@fit$residuals/CrCu_XMR_Model_Sample_SST1@fit$sigma,lag=1,type="Lj")
CrCu_XMR1_SST5  <- Box.test(CrCu_XMR_Model_Sample_SST1@fit$residuals/CrCu_XMR_Model_Sample_SST1@fit$sigma,lag=5,type="Lj")
CrCu_XMR1_SST10  <- Box.test(CrCu_XMR_Model_Sample_SST1@fit$residuals/CrCu_XMR_Model_Sample_SST1@fit$sigma,lag=10,type="Lj")

CrCu_XMR1_SGED1  <- Box.test(CrCu_XMR_Model_Sample_SGED1@fit$residuals/CrCu_XMR_Model_Sample_SGED1@fit$sigma,lag=1,type="Lj")
CrCu_XMR1_SGED5  <- Box.test(CrCu_XMR_Model_Sample_SGED1@fit$residuals/CrCu_XMR_Model_Sample_SGED1@fit$sigma,lag=5,type="Lj")
CrCu_XMR1_SGED10  <- Box.test(CrCu_XMR_Model_Sample_SGED1@fit$residuals/CrCu_XMR_Model_Sample_SGED1@fit$sigma,lag=10,type="Lj")

CrCu_XMR1_Normal1  <- Box.test(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma,lag=1,type="Lj")
CrCu_XMR1_Normal5  <- Box.test(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma,lag=5,type="Lj")
CrCu_XMR1_Normal10  <- Box.test(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma,lag=10,type="Lj")

CrCu_XMR1_LQ_JSU <- c(CrCu_XMR1_JSU1$statistic,CrCu_XMR1_JSU5$statistic,CrCu_XMR1_JSU10$statistic)
CrCu_XMR1_LQ_SST <- c(CrCu_XMR1_SST1$statistic,CrCu_XMR1_SST5$statistic,CrCu_XMR1_SST10$statistic)
CrCu_XMR1_LQ_SGED <- c(CrCu_XMR1_SGED1$statistic,CrCu_XMR1_SGED5$statistic,CrCu_XMR1_SGED10$statistic)
CrCu_XMR1_LQ_Normal <- c(CrCu_XMR1_Normal1$statistic,CrCu_XMR1_Normal5$statistic,CrCu_XMR1_Normal10$statistic)

CrCu_XMR1_res_stat <-   cbind(CrCu_XMR1_LQ_JSU,CrCu_XMR1_LQ_SST,CrCu_XMR1_LQ_SGED,CrCu_XMR1_LQ_Normal)
rownames(CrCu_XMR1_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_XMR1_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Sample 2

CrCu_XMR2_JSU1  <- Box.test(CrCu_XMR_Model_Sample_JSU2@fit$residuals/CrCu_XMR_Model_Sample_JSU2@fit$sigma,lag=1,type="Lj")
CrCu_XMR2_JSU5  <- Box.test(CrCu_XMR_Model_Sample_JSU2@fit$residuals/CrCu_XMR_Model_Sample_JSU2@fit$sigma,lag=5,type="Lj")
CrCu_XMR2_JSU10  <- Box.test(CrCu_XMR_Model_Sample_JSU2@fit$residuals/CrCu_XMR_Model_Sample_JSU2@fit$sigma,lag=10,type="Lj")

CrCu_XMR2_SST1  <- Box.test(CrCu_XMR_Model_Sample_SST2@fit$residuals/CrCu_XMR_Model_Sample_SST2@fit$sigma,lag=1,type="Lj")
CrCu_XMR2_SST5  <- Box.test(CrCu_XMR_Model_Sample_SST2@fit$residuals/CrCu_XMR_Model_Sample_SST2@fit$sigma,lag=5,type="Lj")
CrCu_XMR2_SST10  <- Box.test(CrCu_XMR_Model_Sample_SST2@fit$residuals/CrCu_XMR_Model_Sample_SST2@fit$sigma,lag=10,type="Lj")

CrCu_XMR2_SGED1  <- Box.test(CrCu_XMR_Model_Sample_SGED2@fit$residuals/CrCu_XMR_Model_Sample_SGED2@fit$sigma,lag=1,type="Lj")
CrCu_XMR2_SGED5  <- Box.test(CrCu_XMR_Model_Sample_SGED2@fit$residuals/CrCu_XMR_Model_Sample_SGED2@fit$sigma,lag=5,type="Lj")
CrCu_XMR2_SGED10  <- Box.test(CrCu_XMR_Model_Sample_SGED2@fit$residuals/CrCu_XMR_Model_Sample_SGED2@fit$sigma,lag=10,type="Lj")

CrCu_XMR2_Normal1  <- Box.test(CrCu_XMR_Model_Sample_Normal2@fit$residuals/CrCu_XMR_Model_Sample_Normal2@fit$sigma,lag=1,type="Lj")
CrCu_XMR2_Normal5  <- Box.test(CrCu_XMR_Model_Sample_Normal2@fit$residuals/CrCu_XMR_Model_Sample_Normal2@fit$sigma,lag=5,type="Lj")
CrCu_XMR2_Normal10  <- Box.test(CrCu_XMR_Model_Sample_Normal2@fit$residuals/CrCu_XMR_Model_Sample_Normal2@fit$sigma,lag=10,type="Lj")

CrCu_XMR2_LQ_JSU <- c(CrCu_XMR2_JSU1$statistic,CrCu_XMR2_JSU5$statistic,CrCu_XMR2_JSU10$statistic)
CrCu_XMR2_LQ_SST <- c(CrCu_XMR2_SST1$statistic,CrCu_XMR2_SST5$statistic,CrCu_XMR2_SST10$statistic)
CrCu_XMR2_LQ_SGED <- c(CrCu_XMR2_SGED1$statistic,CrCu_XMR2_SGED5$statistic,CrCu_XMR2_SGED10$statistic)
CrCu_XMR2_LQ_Normal <- c(CrCu_XMR2_Normal1$statistic,CrCu_XMR2_Normal5$statistic,CrCu_XMR2_Normal10$statistic)

CrCu_XMR2_res_stat <-   cbind(CrCu_XMR2_LQ_JSU,CrCu_XMR2_LQ_SST,CrCu_XMR2_LQ_SGED,CrCu_XMR2_LQ_Normal)
rownames(CrCu_XMR2_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_XMR2_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Sample 3

CrCu_XMR3_JSU1  <- Box.test(CrCu_XMR_Model_Sample_JSU3@fit$residuals/CrCu_XMR_Model_Sample_JSU3@fit$sigma,lag=1,type="Lj")
CrCu_XMR3_JSU5  <- Box.test(CrCu_XMR_Model_Sample_JSU3@fit$residuals/CrCu_XMR_Model_Sample_JSU3@fit$sigma,lag=5,type="Lj")
CrCu_XMR3_JSU10  <- Box.test(CrCu_XMR_Model_Sample_JSU3@fit$residuals/CrCu_XMR_Model_Sample_JSU3@fit$sigma,lag=10,type="Lj")

CrCu_XMR3_SST1  <- Box.test(CrCu_XMR_Model_Sample_SST3@fit$residuals/CrCu_XMR_Model_Sample_SST3@fit$sigma,lag=1,type="Lj")
CrCu_XMR3_SST5  <- Box.test(CrCu_XMR_Model_Sample_SST3@fit$residuals/CrCu_XMR_Model_Sample_SST3@fit$sigma,lag=5,type="Lj")
CrCu_XMR3_SST10  <- Box.test(CrCu_XMR_Model_Sample_SST3@fit$residuals/CrCu_XMR_Model_Sample_SST3@fit$sigma,lag=10,type="Lj")

CrCu_XMR3_SGED1  <- Box.test(CrCu_XMR_Model_Sample_SGED3@fit$residuals/CrCu_XMR_Model_Sample_SGED3@fit$sigma,lag=1,type="Lj")
CrCu_XMR3_SGED5  <- Box.test(CrCu_XMR_Model_Sample_SGED3@fit$residuals/CrCu_XMR_Model_Sample_SGED3@fit$sigma,lag=5,type="Lj")
CrCu_XMR3_SGED10  <- Box.test(CrCu_XMR_Model_Sample_SGED3@fit$residuals/CrCu_XMR_Model_Sample_SGED3@fit$sigma,lag=10,type="Lj")

CrCu_XMR3_Normal1  <- Box.test(CrCu_XMR_Model_Sample_Normal3@fit$residuals/CrCu_XMR_Model_Sample_Normal3@fit$sigma,lag=1,type="Lj")
CrCu_XMR3_Normal5  <- Box.test(CrCu_XMR_Model_Sample_Normal3@fit$residuals/CrCu_XMR_Model_Sample_Normal3@fit$sigma,lag=5,type="Lj")
CrCu_XMR3_Normal10  <- Box.test(CrCu_XMR_Model_Sample_Normal3@fit$residuals/CrCu_XMR_Model_Sample_Normal3@fit$sigma,lag=10,type="Lj")

CrCu_XMR3_LQ_JSU <- c(CrCu_XMR3_JSU1$statistic,CrCu_XMR3_JSU5$statistic,CrCu_XMR3_JSU10$statistic)
CrCu_XMR3_LQ_SST <- c(CrCu_XMR3_SST1$statistic,CrCu_XMR3_SST5$statistic,CrCu_XMR3_SST10$statistic)
CrCu_XMR3_LQ_SGED <- c(CrCu_XMR3_SGED1$statistic,CrCu_XMR3_SGED5$statistic,CrCu_XMR3_SGED10$statistic)
CrCu_XMR3_LQ_Normal <- c(CrCu_XMR3_Normal1$statistic,CrCu_XMR3_Normal5$statistic,CrCu_XMR3_Normal10$statistic)

CrCu_XMR3_res_stat <-   cbind(CrCu_XMR3_LQ_JSU,CrCu_XMR3_LQ_SST,CrCu_XMR3_LQ_SGED,CrCu_XMR3_LQ_Normal)
rownames(CrCu_XMR3_res_stat) <- c("LQ1","LQ5","LQ10")
colnames(CrCu_XMR3_res_stat) <- c("eGarch-JSU","eGarch-SST","eGarch-SGED","eGarch-Normal")

# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

######## ::::::::::::::::: for P E A R S O N  TYPE IV ##########

### Sample Residuals Statistics

XMR_Res_Sample1 <- c(normalTest(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma,mXMRod="jb")@test$statistic,normalTest(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma,mXMRod="sw")@test$statistic,normalTest(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma,mXMRod="ks")@test$statistic)
XMR_Res_Sample2 <- c(normalTest(CrCu_XMR_Model_Sample_Normal2@fit$residuals/CrCu_XMR_Model_Sample_Normal2@fit$sigma,mXMRod="jb")@test$statistic,normalTest(CrCu_XMR_Model_Sample_Normal2@fit$residuals/CrCu_XMR_Model_Sample_Normal2@fit$sigma,mXMRod="sw")@test$statistic,normalTest(CrCu_XMR_Model_Sample_Normal2@fit$residuals/CrCu_XMR_Model_Sample_Normal2@fit$sigma,mXMRod="ks")@test$statistic)
XMR_Res_Sample3 <- c(normalTest(CrCu_XMR_Model_Sample_Normal3@fit$residuals/CrCu_XMR_Model_Sample_Normal3@fit$sigma,mXMRod="jb")@test$statistic,normalTest(CrCu_XMR_Model_Sample_Normal3@fit$residuals/CrCu_XMR_Model_Sample_Normal3@fit$sigma,mXMRod="sw")@test$statistic,normalTest(CrCu_XMR_Model_Sample_Normal3@fit$residuals/CrCu_XMR_Model_Sample_Normal3@fit$sigma,mXMRod="ks")@test$statistic)

#jarqueberaTest(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma)

XMR_Res_Sample_Test   <- rbind(XMR_Res_Sample1,XMR_Res_Sample2,XMR_Res_Sample3)
rownames(XMR_Res_Sample_Test) <- c("Sample1","Sample2","Sample3")
colnames(XMR_Res_Sample_Test) <- c("JB","SW","KS") 

### Pearson Specification
library(gsl)
XMR_Param_Pearson_Sample1 <- pearsonFitML(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma)
XMR_Param_Pearson_Sample2 <- pearsonFitML(CrCu_XMR_Model_Sample_Normal2@fit$residuals/CrCu_XMR_Model_Sample_Normal2@fit$sigma)
XMR_Param_Pearson_Sample3 <- pearsonFitML(CrCu_XMR_Model_Sample_Normal3@fit$residuals/CrCu_XMR_Model_Sample_Normal3@fit$sigma)

XMR_Param_Pearson_Subsample <- rbind(XMR_Param_Pearson_Sample1,XMR_Param_Pearson_Sample2,XMR_Param_Pearson_Sample3)
rownames(XMR_Param_Pearson_Subsample) <- c("Sample1","Sample2","Sample3")

### johnson SU parameter
XMR_Param_Johnson_Sample1 <- JohnsonFit(CrCu_XMR_Model_Sample_Normal1@fit$residuals/CrCu_XMR_Model_Sample_Normal1@fit$sigma)
XMR_Param_Johnson_Sample2 <- JohnsonFit(CrCu_XMR_Model_Sample_Normal2@fit$residuals/CrCu_XMR_Model_Sample_Normal2@fit$sigma)
XMR_Param_Johnson_Sample3 <- JohnsonFit(CrCu_XMR_Model_Sample_Normal3@fit$residuals/CrCu_XMR_Model_Sample_Normal3@fit$sigma)

XMR_Param_Johnson_Subsample <- rbind(XMR_Param_Johnson_Sample1,XMR_Param_Johnson_Sample2,XMR_Param_Johnson_Sample3)
rownames(XMR_Param_Johnson_Subsample) <- c("Sample1","Sample2","Sample3")

# coefficients for Normal eGarch
coef_XMR_sub1_Normal <- round(coef(CrCu_XMR_Model_Sample_Normal1),4) # SS1
coef_XMR_sub2_Normal <- round(coef(CrCu_XMR_Model_Sample_Normal2),4) # SS2
coef_XMR_sub3_Normal <- round(coef(CrCu_XMR_Model_Sample_Normal3),4) # SS3
# Value at Risk
CrCu_XMR_VaR_Normal1   <- CrCu_XMR_Roll_Normal_Sample1@forecast$VaR
CrCu_XMR_VaR_Normal2   <- CrCu_XMR_Roll_Normal_Sample2@forecast$VaR
CrCu_XMR_VaR_Normal3   <- CrCu_XMR_Roll_Normal_Sample3@forecast$VaR

#### Repeating for Pearson Distribution  #####
signific_levels <- c(0.1,0.05,0.01)

## Store the volatility and mu forecast for Sample 1

Holdout_Sample1_XMR_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample1_XMR_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample1_XMR_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample1_XMR_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample1_XMR_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample1_XMR_Pearson_pvalues) <- c("1%","5%","10%")

Sample1_XMR_Holdout_Mu <- as.data.frame(CrCu_XMR_Roll_Normal_Sample1)[,'Mu']
Sample1_XMR_Holdout_Sigma <- as.data.frame(CrCu_XMR_Roll_Normal_Sample1)[,'Sigma']

for(j in 1:3)
{
  
  Z_Pearson_Sample1_XMR   <- qpearson(signific_levels[j],XMR_Param_Pearson_Sample1)
  
  Sample1_XMR_Holdout_VaR   <- Sample1_XMR_Holdout_Mu + Sample1_XMR_Holdout_Sigma*Z_Pearson_Sample1_XMR
  Sample1_XMR_Violations   <- BacktestVaR(Returns_CrCu_XMR[(CrCu_XMR_samples[1]-60+1):(CrCu_XMR_samples[1]-1)],Sample1_XMR_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample1_XMR_Pearson[j,]   <- c(Sample1_XMR_Violations$LRuc[1],Sample1_XMR_Violations$LRcc[1],Sample1_XMR_Violations$DQ$stat,Sample1_XMR_Violations$AE,Sample1_XMR_Violations$Loss$Loss)
  Holdout_Sample1_XMR_Pearson_pvalues[j,] <- c(Sample1_XMR_Violations$LRuc[2],Sample1_XMR_Violations$LRcc[2],Sample1_XMR_Violations$DQ$pvalue)
}

## Store the volatility and mu forecast for Sample 2

Holdout_Sample2_XMR_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample2_XMR_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample2_XMR_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample2_XMR_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample2_XMR_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample2_XMR_Pearson_pvalues) <- c("1%","5%","10%")

Sample2_XMR_Holdout_Mu <- as.data.frame(CrCu_XMR_Roll_Normal_Sample2)[,'Mu']
Sample2_XMR_Holdout_Sigma <- as.data.frame(CrCu_XMR_Roll_Normal_Sample2)[,'Sigma']

for(j in 1:3)
{
  
  Z_Pearson_Sample2_XMR   <- qpearson(signific_levels[j],XMR_Param_Pearson_Sample1)
  
  Sample2_XMR_Holdout_VaR   <- Sample2_XMR_Holdout_Mu + Sample2_XMR_Holdout_Sigma*Z_Pearson_Sample2_XMR
  Sample2_XMR_Violations   <- BacktestVaR(Returns_CrCu_XMR[(CrCu_XMR_samples[2]-200+1):(CrCu_XMR_samples[2]-1)],Sample2_XMR_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample2_XMR_Pearson[j,]   <- c(Sample2_XMR_Violations$LRuc[1],Sample2_XMR_Violations$LRcc[1],Sample2_XMR_Violations$DQ$stat,Sample2_XMR_Violations$AE,Sample2_XMR_Violations$Loss$Loss)
  Holdout_Sample2_XMR_Pearson_pvalues[j,] <- c(Sample2_XMR_Violations$LRuc[2],Sample2_XMR_Violations$LRcc[2],Sample2_XMR_Violations$DQ$pvalue)
}

## Store the volatility and mu forecast for Sample 3

Holdout_Sample3_XMR_Pearson    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_Sample3_XMR_Pearson_pvalues    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_Sample3_XMR_Pearson)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_Sample3_XMR_Pearson_pvalues)   <- c("UC","CC","DQ")
rownames(Holdout_Sample3_XMR_Pearson) <- c("1%","5%","10%")
rownames(Holdout_Sample3_XMR_Pearson_pvalues) <- c("1%","5%","10%")

Sample3_XMR_Holdout_Mu <- as.data.frame(CrCu_XMR_Roll_Normal_Sample3)[,'Mu']
Sample3_XMR_Holdout_Sigma <- as.data.frame(CrCu_XMR_Roll_Normal_Sample3)[,'Sigma']

for(j in 1:3)
{
  
  Z_Pearson_Sample3_XMR   <- qpearson(signific_levels[j],XMR_Param_Pearson_Sample3)
  
  Sample3_XMR_Holdout_VaR   <- Sample3_XMR_Holdout_Mu + Sample3_XMR_Holdout_Sigma*Z_Pearson_Sample3_XMR
  Sample3_XMR_Violations   <- BacktestVaR(Returns_CrCu_XMR[(length(Returns_CrCu_XMR)-100+1):(length(Returns_CrCu_XMR)-1)],Sample3_XMR_Holdout_VaR[-1],signific_levels[j])
  Holdout_Sample3_XMR_Pearson[j,]   <- c(Sample3_XMR_Violations$LRuc[1],Sample3_XMR_Violations$LRcc[1],Sample3_XMR_Violations$DQ$stat,Sample3_XMR_Violations$AE,Sample3_XMR_Violations$Loss$Loss)
  Holdout_Sample3_XMR_Pearson_pvalues[j,] <- c(Sample3_XMR_Violations$LRuc[2],Sample3_XMR_Violations$LRcc[2],Sample3_XMR_Violations$DQ$pvalue)
}

Holdout_XMR_Pearson_FT <- cbind(Holdout_Sample1_XMR_Pearson[,4], Holdout_Sample2_XMR_Pearson[,4],Holdout_Sample3_XMR_Pearson[,4])
colnames(Holdout_XMR_Pearson_FT) <- c("SubSample1","SubSample2","SubSample3")

## Storing Pearson subsample results ###

XMR_subsample_1pc   <- rbind(Holdout_Sample1_XMR_Pearson[1,],Holdout_Sample2_XMR_Pearson[1,],Holdout_Sample3_XMR_Pearson[1,])

XMR_subsample_5pc   <- rbind(Holdout_Sample1_XMR_Pearson[2,],Holdout_Sample2_XMR_Pearson[2,],Holdout_Sample3_XMR_Pearson[2,])

XMR_subsample_10pc   <- rbind(Holdout_Sample1_XMR_Pearson[3,],Holdout_Sample2_XMR_Pearson[3,],Holdout_Sample3_XMR_Pearson[3,])

XMR_subsample_pvalues_1pc   <- rbind(Holdout_Sample1_XMR_Pearson_pvalues[1,],Holdout_Sample2_XMR_Pearson_pvalues[1,],Holdout_Sample3_XMR_Pearson_pvalues[1,])

XMR_subsample_pvalues_5pc   <- rbind(Holdout_Sample1_XMR_Pearson_pvalues[2,],Holdout_Sample2_XMR_Pearson_pvalues[2,],Holdout_Sample3_XMR_Pearson_pvalues[2,])

XMR_subsample_pvalues_10pc   <- rbind(Holdout_Sample1_XMR_Pearson_pvalues[3,],Holdout_Sample2_XMR_Pearson_pvalues[3,],Holdout_Sample3_XMR_Pearson_pvalues[3,])

## Store the volatility and mu forecast for Sample 1
### Plotting the VaR for subsamples

CrCu_XMR_Sample1_Dates <- format(CrCu_XMR$Crypto_Date[(CrCu_XMR_samples[1]-59):CrCu_XMR_samples[1]],"%b-%y")
CrCu_XMR_Sample2_Dates <- format(CrCu_XMR$Crypto_Date[(CrCu_XMR_samples[2]-199):CrCu_XMR_samples[2]],"%b-%y")
CrCu_XMR_Sample3_Dates <- format(CrCu_XMR$Crypto_Date[(length(CrCu_XMR_xts)-99):length(CrCu_XMR_xts)],"%b-%y")

# Sub Sample 1 # best is SGED based on garch

Z_Pearson_Sample1_XMR   <- qpearson(0.05,XMR_Param_Pearson_Sample1)
Sample1_XMR_Holdout_VaR   <- Sample1_XMR_Holdout_Mu + Sample1_XMR_Holdout_Sigma*Z_Pearson_Sample1_XMR

VR11_XMR <- data.frame(CrCu_XMR_VaR_Normal1)
VR12_XMR <- data.frame(CrCu_XMR_VaR_SST1)
VR13_XMR <- data.frame(CrCu_XMR_VaR_SGED1)
VR14_XMR <- data.frame(CrCu_XMR_VaR_JSU1)
VR15_XMR <- data.frame(Sample1_XMR_Holdout_VaR)
VR_Date1_XMR <- CrCu_XMR$Crypto_Date[(CrCu_XMR_samples[1]-59):CrCu_XMR_samples[1]]

SS1VaR_XMR <- ggplot(data.frame(VR11_XMR[,4]), aes(x= VR_Date1_XMR, y = VR11_XMR[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date1_XMR, y = VR11_XMR[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date1_XMR, y = VR12_XMR[,2], color = "SST")) +
  geom_line(aes(x = VR_Date1_XMR, y = VR13_XMR[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date1_XMR, y = VR14_XMR[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date1_XMR, y = VR15_XMR[,1], color = "PSIV")) +
  xlab ("30 July 2017 to 27 September 2017") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Monero SubSample 1") +
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

ggexport(SS1VaR_XMR, filename = file.path(FIG_DIR, "SS1VaR_XMR.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

# Sub Sample 2

Z_Pearson_Sample2_XMR   <- qpearson(0.05,XMR_Param_Pearson_Sample2)
Sample2_XMR_Holdout_VaR   <- Sample2_XMR_Holdout_Mu + Sample2_XMR_Holdout_Sigma*Z_Pearson_Sample2_XMR

VR21_XMR <- data.frame(CrCu_XMR_VaR_Normal2)
VR22_XMR <- data.frame(CrCu_XMR_VaR_SST2)
VR23_XMR <- data.frame(CrCu_XMR_VaR_SGED2)
VR24_XMR <- data.frame(CrCu_XMR_VaR_JSU2)
VR25_XMR <- data.frame(Sample2_XMR_Holdout_VaR)
VR_Date2_XMR <- CrCu_XMR$Crypto_Date[(CrCu_XMR_samples[2]-199):CrCu_XMR_samples[2]]

SS2VaR_XMR <- ggplot(data.frame(VR21_XMR[,4]), aes(x= VR_Date2_XMR, y = VR21_XMR[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date2_XMR, y = VR21_XMR[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date2_XMR, y = VR22_XMR[,2], color = "SST")) +
  geom_line(aes(x = VR_Date2_XMR, y = VR23_XMR[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date2_XMR, y = VR24_XMR[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date2_XMR, y = VR25_XMR[,1], color = "PSIV")) +
  xlab ("07 December 2019 to 23 June 2020") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Monero SubSample 2") +
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

ggexport(SS2VaR_XMR, filename = file.path(FIG_DIR, "SS2VaR_XMR.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

# Sub Sample 3

Z_Pearson_Sample3_XMR   <- qpearson(0.05,XMR_Param_Pearson_Sample3)
Sample3_XMR_Holdout_VaR   <- Sample3_XMR_Holdout_Mu + Sample3_XMR_Holdout_Sigma*Z_Pearson_Sample3_XMR

VR31_XMR <- data.frame(CrCu_XMR_VaR_Normal3)
VR32_XMR <- data.frame(CrCu_XMR_VaR_SST3)
VR33_XMR <- data.frame(CrCu_XMR_VaR_SGED3)
VR34_XMR <- data.frame(CrCu_XMR_VaR_JSU3)
VR35_XMR <- data.frame(Sample3_XMR_Holdout_VaR)
VR_Date3_XMR <- CrCu_XMR$Crypto_Date[(length(CrCu_XMR_xts)-99):length(CrCu_XMR_xts)]

SS3VaR_XMR <- ggplot(data.frame(VR31_XMR[,4]), aes(x= VR_Date3_XMR, y = VR31_XMR[,4])) +
  geom_line(aes(colour = "Returns"))  +
  geom_line(aes(x = VR_Date3_XMR, y = VR31_XMR[,2], color = "Norm")) +
  geom_line(aes(x = VR_Date3_XMR, y = VR32_XMR[,2], color = "SST")) +
  geom_line(aes(x = VR_Date3_XMR, y = VR33_XMR[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date3_XMR, y = VR34_XMR[,2], color = "JSU")) +
  geom_line(aes(x = VR_Date3_XMR, y = VR35_XMR[,1], color = "PSIV")) +
  xlab ("26 August 2021 to 04 December 2021") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Monero SubSample 3") +
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

ggexport(SS3VaR_XMR, filename = file.path(FIG_DIR, "SS3VaR_XMR.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

#Full Data

Z_Pearson_XMR   <- qpearson(0.05,CrCu_XMR_Dist8)
XMR_Holdout_VaR   <- XMR_Holdout_Mu + XMR_Holdout_Sigma*Z_Pearson_XMR

VR1_XMR <- data.frame(CrCu_XMR_Roll8@forecast$VaR)
VR2_XMR <- data.frame(CrCu_XMR_Roll5@forecast$VaR)
VR3_XMR <- data.frame(CrCu_XMR_Roll6@forecast$VaR)
VR4_XMR <- data.frame(CrCu_XMR_Roll7@forecast$VaR)
VR5_XMR <- data.frame(XMR_Holdout_VaR)
VR_Date_XMR <- CrCu_XMR$Crypto_Date[(length(CrCu_XMR_xts)-349):length(CrCu_XMR_xts)]

FSVaR_XMR <- ggplot(data.frame(VR1_XMR[,4]), aes(x= VR_Date_XMR, y = VR1_XMR[,4])) +
  geom_line(aes(colour = "Return"))  +
  geom_line(aes(x = VR_Date_XMR, y = VR1_XMR[,2], colour = "Norm")) +
  geom_line(aes(x = VR_Date_XMR, y = VR2_XMR[,2], color = "SST")) +
  geom_line(aes(x = VR_Date_XMR, y = VR3_XMR[,2], color = "SGED")) +
  geom_line(aes(x = VR_Date_XMR, y = VR4_XMR[,2], color = "JSU")) + 
  geom_line(aes(x = VR_Date_XMR, y = VR5_XMR[,1], color = "PS")) +
  xlab ("19 Dec 2020 to 04 Dec 2021") + 
  ylab("Log Returns") +
  ggtitle("Out of sample forecast for VaR for Monero") +
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

ggexport(FSVaR_XMR, filename = file.path(FIG_DIR, "FSVaR_XMR.eps"), width = 300,   
         height = 200,
         dpi = 800,
         units = "mm")

########## MAPE ###########
# Forecast performance measure for sub samples
# Value at Risk

# Sample 1

VaR_s1_01_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED1$`alpha(1%)`, abs(CrCu_XMR_VaR_SGED1$realized))
VaR_s1_05_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED1$`alpha(5%)`, abs(CrCu_XMR_VaR_SGED1$realized))
VaR_s1_10_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED1$`alpha(10%)`, abs(CrCu_XMR_VaR_SGED1$realized))

VaR_s1_01_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU1$`alpha(1%)`, abs(CrCu_XMR_VaR_JSU1$realized))
VaR_s1_05_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU1$`alpha(5%)`, abs(CrCu_XMR_VaR_JSU1$realized))
VaR_s1_10_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU1$`alpha(10%)`, abs(CrCu_XMR_VaR_JSU1$realized))

VaR_s1_01_XMR_SST <- MAPE(CrCu_XMR_VaR_SST1$`alpha(1%)`, abs(CrCu_XMR_VaR_SST1$realized))
VaR_s1_05_XMR_SST <- MAPE(CrCu_XMR_VaR_SST1$`alpha(5%)`, abs(CrCu_XMR_VaR_SST1$realized))
VaR_s1_10_XMR_SST <- MAPE(CrCu_XMR_VaR_SST1$`alpha(10%)`, abs(CrCu_XMR_VaR_SST1$realized))

VaR_s1_01_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal1$`alpha(1%)`, abs(CrCu_XMR_VaR_Normal1$realized))
VaR_s1_05_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal1$`alpha(5%)`, abs(CrCu_XMR_VaR_Normal1$realized))
VaR_s1_10_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal1$`alpha(10%)`, abs(CrCu_XMR_VaR_Normal1$realized))

Z_Pearson_Sample1_XMR_01   <- qpearson(0.01,XMR_Param_Pearson_Sample1)
Sample1_XMR_Holdout_VaR_01   <- Sample1_XMR_Holdout_Mu + Sample1_XMR_Holdout_Sigma*Z_Pearson_Sample1_XMR_01
Z_Pearson_Sample1_XMR_05   <- qpearson(0.05,XMR_Param_Pearson_Sample1)
Sample1_XMR_Holdout_VaR_05   <- Sample1_XMR_Holdout_Mu + Sample1_XMR_Holdout_Sigma*Z_Pearson_Sample1_XMR_05
Z_Pearson_Sample1_XMR_10   <- qpearson(0.10,XMR_Param_Pearson_Sample1)
Sample1_XMR_Holdout_VaR_10   <- Sample1_XMR_Holdout_Mu + Sample1_XMR_Holdout_Sigma*Z_Pearson_Sample1_XMR_10

VaR_s1_01_XMR_PS <- MAPE(Sample1_XMR_Holdout_VaR_01, abs(CrCu_XMR_VaR_SGED1$realized))
VaR_s1_05_XMR_PS <- MAPE(Sample1_XMR_Holdout_VaR_05, abs(CrCu_XMR_VaR_SGED1$realized))
VaR_s1_10_XMR_PS <- MAPE(Sample1_XMR_Holdout_VaR_10, abs(CrCu_XMR_VaR_SGED1$realized))


MAPEval_fsub1_XMR  <- matrix(data=0,nrow=5,ncol=3)
colnames(MAPEval_fsub1_XMR) <- c("SS1 - 1%", "SS1 - 5%", "SS1 - 10%")
rownames(MAPEval_fsub1_XMR) <- c("SGED","JSU","SST","Normal", "PSIV")

MAPEval_fsub1_XMR[,1] = rbind(VaR_s1_01_XMR_SGED,VaR_s1_01_XMR_JSU,VaR_s1_01_XMR_SST,VaR_s1_01_XMR_Normal,VaR_s1_01_XMR_PS )
MAPEval_fsub1_XMR[,2] = rbind(VaR_s1_05_XMR_SGED,VaR_s1_05_XMR_JSU,VaR_s1_05_XMR_SST,VaR_s1_05_XMR_Normal,VaR_s1_05_XMR_PS)
MAPEval_fsub1_XMR[,3] = rbind(VaR_s1_10_XMR_SGED,VaR_s1_10_XMR_JSU,VaR_s1_10_XMR_SST,VaR_s1_10_XMR_Normal,VaR_s1_10_XMR_PS)

# Sample 2

VaR_s2_01_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED2$`alpha(1%)`, abs(CrCu_XMR_VaR_SGED2$realized))
VaR_s2_05_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED2$`alpha(5%)`, abs(CrCu_XMR_VaR_SGED2$realized))
VaR_s2_10_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED2$`alpha(10%)`, abs(CrCu_XMR_VaR_SGED2$realized))

VaR_s2_01_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU2$`alpha(1%)`, abs(CrCu_XMR_VaR_JSU2$realized))
VaR_s2_05_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU2$`alpha(5%)`, abs(CrCu_XMR_VaR_JSU2$realized))
VaR_s2_10_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU2$`alpha(10%)`, abs(CrCu_XMR_VaR_JSU2$realized))

VaR_s2_01_XMR_SST <- MAPE(CrCu_XMR_VaR_SST2$`alpha(1%)`, abs(CrCu_XMR_VaR_SST2$realized))
VaR_s2_05_XMR_SST <- MAPE(CrCu_XMR_VaR_SST2$`alpha(5%)`, abs(CrCu_XMR_VaR_SST2$realized))
VaR_s2_10_XMR_SST <- MAPE(CrCu_XMR_VaR_SST2$`alpha(10%)`, abs(CrCu_XMR_VaR_SST2$realized))

VaR_s2_01_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal2$`alpha(1%)`, abs(CrCu_XMR_VaR_Normal2$realized))
VaR_s2_05_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal2$`alpha(5%)`, abs(CrCu_XMR_VaR_Normal2$realized))
VaR_s2_10_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal2$`alpha(10%)`, abs(CrCu_XMR_VaR_Normal2$realized))

Z_Pearson_Sample2_XMR_01   <- qpearson(0.01,XMR_Param_Pearson_Sample2)
Sample2_XMR_Holdout_VaR_01   <- Sample2_XMR_Holdout_Mu + Sample2_XMR_Holdout_Sigma*Z_Pearson_Sample2_XMR_01
Z_Pearson_Sample2_XMR_05   <- qpearson(0.05,XMR_Param_Pearson_Sample2)
Sample2_XMR_Holdout_VaR_05   <- Sample2_XMR_Holdout_Mu + Sample2_XMR_Holdout_Sigma*Z_Pearson_Sample2_XMR_05
Z_Pearson_Sample2_XMR_10   <- qpearson(0.10,XMR_Param_Pearson_Sample2)
Sample2_XMR_Holdout_VaR_10   <- Sample2_XMR_Holdout_Mu + Sample2_XMR_Holdout_Sigma*Z_Pearson_Sample2_XMR_10

VaR_s2_01_XMR_PS <- MAPE(Sample2_XMR_Holdout_VaR_01, abs(CrCu_XMR_VaR_SGED2$realized))
VaR_s2_05_XMR_PS <- MAPE(Sample2_XMR_Holdout_VaR_05, abs(CrCu_XMR_VaR_SGED2$realized))
VaR_s2_10_XMR_PS <- MAPE(Sample2_XMR_Holdout_VaR_10, abs(CrCu_XMR_VaR_SGED2$realized))


MAPEval_fsub2_XMR  <- matrix(data=0,nrow=5,ncol=3)
colnames(MAPEval_fsub2_XMR) <- c("SS2 - 1%", "SS2 - 5%", "SS2 - 10%")
rownames(MAPEval_fsub2_XMR) <- c("SGED","JSU","SST","Normal","PSIV")

MAPEval_fsub2_XMR[,1] = rbind(VaR_s2_01_XMR_SGED,VaR_s2_01_XMR_JSU,VaR_s2_01_XMR_SST,VaR_s2_01_XMR_Normal,VaR_s2_01_XMR_PS)
MAPEval_fsub2_XMR[,2] = rbind(VaR_s2_05_XMR_SGED,VaR_s2_05_XMR_JSU,VaR_s2_05_XMR_SST,VaR_s2_05_XMR_Normal,VaR_s2_05_XMR_PS)
MAPEval_fsub2_XMR[,3] = rbind(VaR_s2_10_XMR_SGED,VaR_s2_10_XMR_JSU,VaR_s2_10_XMR_SST,VaR_s2_10_XMR_Normal,VaR_s2_10_XMR_PS)

# Sample 3

VaR_s3_01_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED3$`alpha(1%)`, abs(CrCu_XMR_VaR_SGED3$realized))
VaR_s3_05_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED3$`alpha(5%)`, abs(CrCu_XMR_VaR_SGED3$realized))
VaR_s3_10_XMR_SGED <- MAPE(CrCu_XMR_VaR_SGED3$`alpha(10%)`, abs(CrCu_XMR_VaR_SGED3$realized))

VaR_s3_01_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU3$`alpha(1%)`, abs(CrCu_XMR_VaR_JSU3$realized))
VaR_s3_05_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU3$`alpha(5%)`, abs(CrCu_XMR_VaR_JSU3$realized))
VaR_s3_10_XMR_JSU <- MAPE(CrCu_XMR_VaR_JSU3$`alpha(10%)`, abs(CrCu_XMR_VaR_JSU3$realized))

VaR_s3_01_XMR_SST <- MAPE(CrCu_XMR_VaR_SST3$`alpha(1%)`, abs(CrCu_XMR_VaR_SST3$realized))
VaR_s3_05_XMR_SST <- MAPE(CrCu_XMR_VaR_SST3$`alpha(5%)`, abs(CrCu_XMR_VaR_SST3$realized))
VaR_s3_10_XMR_SST <- MAPE(CrCu_XMR_VaR_SST3$`alpha(10%)`, abs(CrCu_XMR_VaR_SST3$realized))

VaR_s3_01_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal3$`alpha(1%)`, abs(CrCu_XMR_VaR_Normal3$realized))
VaR_s3_05_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal3$`alpha(5%)`, abs(CrCu_XMR_VaR_Normal3$realized))
VaR_s3_10_XMR_Normal <- MAPE(CrCu_XMR_VaR_Normal3$`alpha(10%)`, abs(CrCu_XMR_VaR_Normal3$realized))

Z_Pearson_Sample3_XMR_01   <- qpearson(0.01,XMR_Param_Pearson_Sample3)
Sample3_XMR_Holdout_VaR_01   <- Sample3_XMR_Holdout_Mu + Sample3_XMR_Holdout_Sigma*Z_Pearson_Sample3_XMR_01
Z_Pearson_Sample3_XMR_05   <- qpearson(0.05,XMR_Param_Pearson_Sample3)
Sample3_XMR_Holdout_VaR_05   <- Sample3_XMR_Holdout_Mu + Sample3_XMR_Holdout_Sigma*Z_Pearson_Sample3_XMR_05
Z_Pearson_Sample3_XMR_10   <- qpearson(0.10,XMR_Param_Pearson_Sample3)
Sample3_XMR_Holdout_VaR_10   <- Sample3_XMR_Holdout_Mu + Sample3_XMR_Holdout_Sigma*Z_Pearson_Sample3_XMR_10

VaR_s3_01_XMR_PS <- MAPE(Sample3_XMR_Holdout_VaR_01, abs(CrCu_XMR_VaR_SGED3$realized))
VaR_s3_05_XMR_PS <- MAPE(Sample3_XMR_Holdout_VaR_05, abs(CrCu_XMR_VaR_SGED3$realized))
VaR_s3_10_XMR_PS <- MAPE(Sample3_XMR_Holdout_VaR_10, abs(CrCu_XMR_VaR_SGED3$realized))

MAPEval_fsub3_XMR  <- matrix(data=0,nrow=5,ncol=3)
colnames(MAPEval_fsub3_XMR) <- c("SS3 - 1%", "SS3 - 5%", "SS3 - 10%")
rownames(MAPEval_fsub3_XMR) <- c("SGED","JSU","SST","Normal","PSIV")

MAPEval_fsub3_XMR[,1] = rbind(VaR_s3_01_XMR_SGED,VaR_s3_01_XMR_JSU,VaR_s3_01_XMR_SST,VaR_s3_01_XMR_Normal,VaR_s3_01_XMR_PS)
MAPEval_fsub3_XMR[,2] = rbind(VaR_s3_05_XMR_SGED,VaR_s3_05_XMR_JSU,VaR_s3_05_XMR_SST,VaR_s3_05_XMR_Normal,VaR_s3_05_XMR_PS)
MAPEval_fsub3_XMR[,3] = rbind(VaR_s3_10_XMR_SGED,VaR_s3_10_XMR_JSU,VaR_s3_10_XMR_SST,VaR_s3_10_XMR_Normal,VaR_s3_10_XMR_PS)

rbind("SS1",MAPEval_fsub1_XMR, "SS2",MAPEval_fsub2_XMR,"SS3", MAPEval_fsub3_XMR)
xtable(MAPEval_fsub1_XMR, caption = "MAPE for out of sample forecast of Monero Value at Risk - Sub Sample 1", digits = 4)

xtable(MAPEval_fsub2_XMR, caption = "MAPE for out of sample forecast of Monero Value at Risk - Sub Sample 2", digits = 4)

xtable(MAPEval_fsub3_XMR, caption = "MAPE for out of sample forecast of Monero Value at Risk - Sub Sample 3", digits = 4)

#### Backtesting and Storing the results

# Sample 1
library(GAS)
XMR_Test_SST1_Sample1 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,1],0.01)
XMR_Test_SST5_Sample1 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,2],0.05)
XMR_Test_SST10_Sample1 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,3],0.1)

XMR_Test_SGED1_Sample1 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,1],0.01)
XMR_Test_SGED5_Sample1 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,2],0.05)
XMR_Test_SGED10_Sample1 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,3],0.1)

XMR_Test_JSU1_Sample1 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,1],0.01)
XMR_Test_JSU5_Sample1 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,2],0.05)
XMR_Test_JSU10_Sample1 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,3],0.1)

XMR_Test_Normal1_Sample1 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,1],0.01)
XMR_Test_Normal5_Sample1 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,2],0.05)
XMR_Test_Normal10_Sample1 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,3],0.1)

##### Sample 2

XMR_Test_SST1_Sample2 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,1],0.01)
XMR_Test_SST5_Sample2 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,2],0.05)
XMR_Test_SST10_Sample2 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,3],0.1)

XMR_Test_SGED1_Sample2 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,1],0.01)
XMR_Test_SGED5_Sample2 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,2],0.05)
XMR_Test_SGED10_Sample2 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,3],0.1)

XMR_Test_JSU1_Sample2 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,1],0.01)
XMR_Test_JSU5_Sample2 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,2],0.05)
XMR_Test_JSU10_Sample2 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,3],0.1)

XMR_Test_Normal1_Sample2 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,1],0.01)
XMR_Test_Normal5_Sample2 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,2],0.05)
XMR_Test_Normal10_Sample2 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,3],0.1)

### Sample3

XMR_Test_SST1_Sample3 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,1],0.01)
XMR_Test_SST5_Sample3 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,2],0.05)
XMR_Test_SST10_Sample3 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,3],0.1)

XMR_Test_SGED1_Sample3 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,1],0.01)
XMR_Test_SGED5_Sample3 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,2],0.05)
XMR_Test_SGED10_Sample3 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,3],0.1)

XMR_Test_JSU1_Sample3 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,1],0.01)
XMR_Test_JSU5_Sample3 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,2],0.05)
XMR_Test_JSU10_Sample3 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,3],0.1)

XMR_Test_Normal1_Sample3 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,1],0.01)
XMR_Test_Normal5_Sample3 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,2],0.05)
XMR_Test_Normal10_Sample3 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,3],0.1)

XMR_Sample1 <- matrix(c(XMR_Test_Normal1_Sample1$LRuc[1],XMR_Test_SST1_Sample1$LRuc[1],XMR_Test_SGED1_Sample1$LRuc[1],XMR_Test_JSU1_Sample1$LRuc[1],
                        XMR_Test_Normal1_Sample1$LRcc[1],XMR_Test_SST1_Sample1$LRcc[1],XMR_Test_SGED1_Sample1$LRcc[1],XMR_Test_JSU1_Sample1$LRcc[1],
                        XMR_Test_Normal1_Sample1$DQ$stat,XMR_Test_SST1_Sample1$DQ$stat,XMR_Test_SGED1_Sample1$DQ$stat,XMR_Test_JSU1_Sample1$DQ$stat,
                        XMR_Test_Normal1_Sample1$AE,XMR_Test_SST1_Sample1$AE,XMR_Test_SGED1_Sample1$AE,XMR_Test_JSU1_Sample1$AE,
                        (XMR_Test_Normal1_Sample1$Loss$Loss/XMR_Test_Normal1_Sample1$Loss$Loss),(XMR_Test_SST1_Sample1$Loss$Loss/XMR_Test_Normal1_Sample1$Loss$Loss),
                        (XMR_Test_SGED1_Sample1$Loss$Loss/XMR_Test_Normal1_Sample1$Loss$Loss),(XMR_Test_JSU1_Sample1$Loss$Loss/XMR_Test_Normal1_Sample1$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(XMR_Sample1) <- c("Normal","SST","SGED","JSU")    
rownames(XMR_Sample1) <- c("UC","CC","DQ","AE","QL")    

XMR_Sample2 <- matrix(c(XMR_Test_Normal1_Sample2$LRuc[1],XMR_Test_SST1_Sample2$LRuc[1],XMR_Test_SGED1_Sample2$LRuc[1],XMR_Test_JSU1_Sample2$LRuc[1],
                        XMR_Test_Normal1_Sample2$LRcc[1],XMR_Test_SST1_Sample2$LRcc[1],XMR_Test_SGED1_Sample2$LRcc[1],XMR_Test_JSU1_Sample2$LRcc[1],
                        XMR_Test_Normal1_Sample2$DQ$stat,XMR_Test_SST1_Sample2$DQ$stat,XMR_Test_SGED1_Sample2$DQ$stat,XMR_Test_JSU1_Sample2$DQ$stat,
                        XMR_Test_Normal1_Sample2$AE,XMR_Test_SST1_Sample2$AE,XMR_Test_SGED1_Sample2$AE,XMR_Test_JSU1_Sample2$AE,
                        (XMR_Test_Normal1_Sample2$Loss$Loss/XMR_Test_Normal1_Sample2$Loss$Loss),(XMR_Test_SST1_Sample2$Loss$Loss/XMR_Test_Normal1_Sample2$Loss$Loss),
                        (XMR_Test_SGED1_Sample2$Loss$Loss/XMR_Test_Normal1_Sample2$Loss$Loss),(XMR_Test_JSU1_Sample2$Loss$Loss/XMR_Test_Normal1_Sample2$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(XMR_Sample2) <- c("Normal","SST","SGED","JSU")    
rownames(XMR_Sample2) <- c("UC","CC","DQ","AE","QL")    

XMR_Sample3 <- matrix(c(XMR_Test_Normal1_Sample3$LRuc[1],XMR_Test_SST1_Sample3$LRuc[1],XMR_Test_SGED1_Sample3$LRuc[1],XMR_Test_JSU1_Sample3$LRuc[1],
                        XMR_Test_Normal1_Sample3$LRcc[1],XMR_Test_SST1_Sample3$LRcc[1],XMR_Test_SGED1_Sample3$LRcc[1],XMR_Test_JSU1_Sample3$LRcc[1],
                        XMR_Test_Normal1_Sample3$DQ$stat,XMR_Test_SST1_Sample3$DQ$stat,XMR_Test_SGED1_Sample3$DQ$stat,XMR_Test_JSU1_Sample3$DQ$stat,
                        XMR_Test_Normal1_Sample3$AE,XMR_Test_SST1_Sample3$AE,XMR_Test_SGED1_Sample3$AE,XMR_Test_JSU1_Sample3$AE,
                        (XMR_Test_Normal1_Sample3$Loss$Loss/XMR_Test_Normal1_Sample3$Loss$Loss),(XMR_Test_SST1_Sample3$Loss$Loss/XMR_Test_Normal1_Sample3$Loss$Loss),
                        (XMR_Test_SGED1_Sample3$Loss$Loss/XMR_Test_Normal1_Sample3$Loss$Loss),(XMR_Test_JSU1_Sample3$Loss$Loss/XMR_Test_Normal1_Sample3$Loss$Loss)),nrow=5,byrow = TRUE)
colnames(XMR_Sample3) <- c("Normal","SST","SGED","JSU")    
rownames(XMR_Sample3) <- c("UC","CC","DQ","AE","QL")    


### Storing the p-values for subsamples 

# 1 percent

XMR_pvalues_1pc_Sample1 <- matrix(c(XMR_Test_Normal1_Sample1$LRuc[2],XMR_Test_SST1_Sample1$LRuc[2],XMR_Test_SGED1_Sample1$LRuc[2],XMR_Test_JSU1_Sample1$LRuc[2],
                                    XMR_Test_Normal1_Sample1$LRcc[2],XMR_Test_SST1_Sample1$LRcc[2],XMR_Test_SGED1_Sample1$LRcc[2],XMR_Test_JSU1_Sample1$LRcc[2],
                                    XMR_Test_Normal1_Sample1$DQ$pvalue,XMR_Test_SST1_Sample1$DQ$pvalue,XMR_Test_SGED1_Sample1$DQ$pvalue,XMR_Test_JSU1_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

XMR_pvalues_1pc_Sample2 <- matrix(c(XMR_Test_Normal1_Sample2$LRuc[2],XMR_Test_SST1_Sample2$LRuc[2],XMR_Test_SGED1_Sample2$LRuc[2],XMR_Test_JSU1_Sample2$LRuc[2],
                                    XMR_Test_Normal1_Sample2$LRcc[2],XMR_Test_SST1_Sample2$LRcc[2],XMR_Test_SGED1_Sample2$LRcc[2],XMR_Test_JSU1_Sample2$LRcc[2],
                                    XMR_Test_Normal1_Sample2$DQ$pvalue,XMR_Test_SST1_Sample2$DQ$pvalue,XMR_Test_SGED1_Sample2$DQ$pvalue,XMR_Test_JSU1_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

XMR_pvalues_1pc_Sample3 <- matrix(c(XMR_Test_Normal1_Sample3$LRuc[2],XMR_Test_SST1_Sample3$LRuc[2],XMR_Test_SGED1_Sample3$LRuc[2],XMR_Test_JSU1_Sample3$LRuc[2],
                                    XMR_Test_Normal1_Sample3$LRcc[2],XMR_Test_SST1_Sample3$LRcc[2],XMR_Test_SGED1_Sample3$LRcc[2],XMR_Test_JSU1_Sample3$LRcc[2],
                                    XMR_Test_Normal1_Sample3$DQ$pvalue,XMR_Test_SST1_Sample3$DQ$pvalue,XMR_Test_SGED1_Sample3$DQ$pvalue,XMR_Test_JSU1_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

# 5 percent

XMR_pvalues_5pc_Sample1 <- matrix(c(XMR_Test_Normal5_Sample1$LRuc[2],XMR_Test_SST5_Sample1$LRuc[2],XMR_Test_SGED5_Sample1$LRuc[2],XMR_Test_JSU5_Sample1$LRuc[2],
                                    XMR_Test_Normal5_Sample1$LRcc[2],XMR_Test_SST5_Sample1$LRcc[2],XMR_Test_SGED5_Sample1$LRcc[2],XMR_Test_JSU5_Sample1$LRcc[2],
                                    XMR_Test_Normal5_Sample1$DQ$pvalue,XMR_Test_SST5_Sample1$DQ$pvalue,XMR_Test_SGED5_Sample1$DQ$pvalue,XMR_Test_JSU5_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

XMR_pvalues_5pc_Sample2 <- matrix(c(XMR_Test_Normal5_Sample2$LRuc[2],XMR_Test_SST5_Sample2$LRuc[2],XMR_Test_SGED5_Sample2$LRuc[2],XMR_Test_JSU5_Sample2$LRuc[2],
                                    XMR_Test_Normal5_Sample2$LRcc[2],XMR_Test_SST5_Sample2$LRcc[2],XMR_Test_SGED5_Sample2$LRcc[2],XMR_Test_JSU5_Sample2$LRcc[2],
                                    XMR_Test_Normal5_Sample2$DQ$pvalue,XMR_Test_SST5_Sample2$DQ$pvalue,XMR_Test_SGED5_Sample2$DQ$pvalue,XMR_Test_JSU5_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

XMR_pvalues_5pc_Sample3 <- matrix(c(XMR_Test_Normal5_Sample3$LRuc[2],XMR_Test_SST5_Sample3$LRuc[2],XMR_Test_SGED5_Sample3$LRuc[2],XMR_Test_JSU5_Sample3$LRuc[2],
                                    XMR_Test_Normal5_Sample3$LRcc[2],XMR_Test_SST5_Sample3$LRcc[2],XMR_Test_SGED5_Sample3$LRcc[2],XMR_Test_JSU5_Sample3$LRcc[2],
                                    XMR_Test_Normal5_Sample3$DQ$pvalue,XMR_Test_SST5_Sample3$DQ$pvalue,XMR_Test_SGED5_Sample3$DQ$pvalue,XMR_Test_JSU5_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

# 10 percent

XMR_pvalues_10pc_Sample1 <- matrix(c(XMR_Test_Normal10_Sample1$LRuc[2],XMR_Test_SST10_Sample1$LRuc[2],XMR_Test_SGED10_Sample1$LRuc[2],XMR_Test_JSU10_Sample1$LRuc[2],
                                     XMR_Test_Normal10_Sample1$LRcc[2],XMR_Test_SST10_Sample1$LRcc[2],XMR_Test_SGED10_Sample1$LRcc[2],XMR_Test_JSU10_Sample1$LRcc[2],
                                     XMR_Test_Normal10_Sample1$DQ$pvalue,XMR_Test_SST10_Sample1$DQ$pvalue,XMR_Test_SGED10_Sample1$DQ$pvalue,XMR_Test_JSU10_Sample1$DQ$pvalue),ncol=4,byrow = TRUE)

XMR_pvalues_10pc_Sample2 <- matrix(c(XMR_Test_Normal10_Sample2$LRuc[2],XMR_Test_SST10_Sample2$LRuc[2],XMR_Test_SGED10_Sample2$LRuc[2],XMR_Test_JSU10_Sample2$LRuc[2],
                                     XMR_Test_Normal10_Sample2$LRcc[2],XMR_Test_SST10_Sample2$LRcc[2],XMR_Test_SGED10_Sample2$LRcc[2],XMR_Test_JSU10_Sample2$LRcc[2],
                                     XMR_Test_Normal10_Sample2$DQ$pvalue,XMR_Test_SST10_Sample2$DQ$pvalue,XMR_Test_SGED10_Sample2$DQ$pvalue,XMR_Test_JSU10_Sample2$DQ$pvalue),ncol=4,byrow = TRUE)

XMR_pvalues_10pc_Sample3 <- matrix(c(XMR_Test_Normal10_Sample3$LRuc[2],XMR_Test_SST10_Sample3$LRuc[2],XMR_Test_SGED10_Sample3$LRuc[2],XMR_Test_JSU10_Sample3$LRuc[2],
                                     XMR_Test_Normal10_Sample3$LRcc[2],XMR_Test_SST10_Sample3$LRcc[2],XMR_Test_SGED10_Sample3$LRcc[2],XMR_Test_JSU10_Sample3$LRcc[2],
                                     XMR_Test_Normal10_Sample3$DQ$pvalue,XMR_Test_SST10_Sample3$DQ$pvalue,XMR_Test_SGED10_Sample3$DQ$pvalue,XMR_Test_JSU10_Sample3$DQ$pvalue),ncol=4,byrow = TRUE)

XMR_pvalues_1pc   <- rbind(XMR_pvalues_1pc_Sample1,XMR_pvalues_1pc_Sample2,XMR_pvalues_1pc_Sample3)
XMR_pvalues_5pc   <- rbind(XMR_pvalues_5pc_Sample1,XMR_pvalues_5pc_Sample2,XMR_pvalues_5pc_Sample3)
XMR_pvalues_10pc  <- rbind(XMR_pvalues_10pc_Sample1,XMR_pvalues_10pc_Sample2,XMR_pvalues_10pc_Sample3)

rownames(XMR_pvalues_1pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(XMR_pvalues_1pc) <- c("Normal","SST","SGED","JSU")

rownames(XMR_pvalues_5pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(XMR_pvalues_5pc) <- c("Normal","SST","SGED","JSU")

rownames(XMR_pvalues_10pc) <- c(rep(c("UC","CC","DQ"),4))
colnames(XMR_pvalues_10pc) <- c("Normal","SST","SGED","JSU")

### Using VaRTest and VaRDuration Test individually ###
## VaRTest in R

CrCu_XMR_SGED1_VaR_Test1  <- VaRTest(0.01,CrCu_XMR_VaR_SGED1[,4],CrCu_XMR_VaR_SGED1[,1])
CrCu_XMR_SGED1_VaR_Test5  <- VaRTest(0.05,CrCu_XMR_VaR_SGED1[,4],CrCu_XMR_VaR_SGED1[,2])
CrCu_XMR_SGED1_VaR_Test10 <- VaRTest(0.1,CrCu_XMR_VaR_SGED1[,4],CrCu_XMR_VaR_SGED1[,3])

CrCu_XMR_JSU2_VaR_Test1  <- VaRTest(0.01,CrCu_XMR_VaR_JSU2[,4],CrCu_XMR_VaR_JSU2[,1])
CrCu_XMR_JSU2_VaR_Test5  <- VaRTest(0.05,CrCu_XMR_VaR_JSU2[,4],CrCu_XMR_VaR_JSU2[,2])
CrCu_XMR_JSU2_VaR_Test10 <- VaRTest(0.1,CrCu_XMR_VaR_JSU2[,4],CrCu_XMR_VaR_JSU2[,3])

CrCu_XMR_SST3_VaR_Test1  <- VaRTest(0.01,CrCu_XMR_VaR_SST3[,4],CrCu_XMR_VaR_SST3[,1])
CrCu_XMR_SST3_VaR_Test5  <- VaRTest(0.05,CrCu_XMR_VaR_SST3[,4],CrCu_XMR_VaR_SST3[,2])
CrCu_XMR_SST3_VaR_Test10 <- VaRTest(0.1,CrCu_XMR_VaR_SST3[,4],CrCu_XMR_VaR_SST3[,3])

# ::::::::::: number of exceedances
Backtestres_sub_XMR <- matrix(data = 0, nrow = 8, ncol = 3)

datavar_sub_XMR <- c(CrCu_XMR_SGED1_VaR_Test5$expected.exceed, CrCu_XMR_SGED1_VaR_Test10$expected.exceed, 
                     CrCu_XMR_JSU2_VaR_Test1$expected.exceed, CrCu_XMR_JSU2_VaR_Test5$expected.exceed, CrCu_XMR_JSU2_VaR_Test10$expected.exceed,
                     CrCu_XMR_SST3_VaR_Test1$expected.exceed, CrCu_XMR_SST3_VaR_Test5$expected.exceed, CrCu_XMR_SST3_VaR_Test10$expected.exceed,
                     CrCu_XMR_SGED1_VaR_Test5$actual.exceed, CrCu_XMR_SGED1_VaR_Test10$actual.exceed,
                     CrCu_XMR_JSU2_VaR_Test1$actual.exceed, CrCu_XMR_JSU2_VaR_Test5$actual.exceed, CrCu_XMR_JSU2_VaR_Test10$actual.exceed,
                     CrCu_XMR_SST3_VaR_Test1$actual.exceed, CrCu_XMR_SST3_VaR_Test5$actual.exceed, CrCu_XMR_SST3_VaR_Test10$actual.exceed
)

Backtestres_sub_XMR <- matrix(data = datavar_sub_XMR, ncol = 2, nrow = 8)
rownames(Backtestres_sub_XMR) <- c("SGED1 5%", "SGED1 10%","JSU2 1%", "JSU2 5%", "JSU2 10%", "SST3 1%", "SST3 5%", "SST3 10%")
colnames(Backtestres_sub_XMR) <- c("Expected Exceedances", "Actual Exceedances")

###### :::::::::::::::plot number of exceedances
fillwith_XMR <-  c(CrCu_XMR_SGED1_VaR_Test5$expected.exceed, CrCu_XMR_SGED1_VaR_Test5$actual.exceed, CrCu_XMR_SGED1_VaR_Test10$expected.exceed, CrCu_XMR_SGED1_VaR_Test10$actual.exceed,
                   CrCu_XMR_JSU2_VaR_Test1$expected.exceed, CrCu_XMR_JSU2_VaR_Test1$actual.exceed, CrCu_XMR_JSU2_VaR_Test5$expected.exceed, CrCu_XMR_JSU2_VaR_Test5$actual.exceed, CrCu_XMR_JSU2_VaR_Test10$expected.exceed, CrCu_XMR_JSU2_VaR_Test10$actual.exceed,
                   CrCu_XMR_SST3_VaR_Test1$expected.exceed, CrCu_XMR_SST3_VaR_Test1$actual.exceed, CrCu_XMR_SST3_VaR_Test5$expected.exceed, CrCu_XMR_SST3_VaR_Test5$actual.exceed, CrCu_XMR_SST3_VaR_Test10$expected.exceed, CrCu_XMR_SST3_VaR_Test10$actual.exceed)

df_XMR <- data.frame(distalpha = rep(c("SGED1 5%", "SGED1 10%","JSU2 1%", "JSU2 5%", "JSU2 10%", "SST3 1%", "SST3 5%", "SST3 10%"), each = 2), 
                     exceed = rep(c("Expected.Exceedances", "Actual.Exceedances"), times = 8), 
                     numb = fillwith_XMR)

ggplot(df_XMR, aes(fill=exceed, y=numb, x=distalpha)) +
  geom_bar(position='dodge', stat='identity') +
  ggtitle('# times actual loss is greater than VaR vs expected exceedances') +
  xlab('Distributions with confidence level') +
  ylab('Exceedances') +
  scale_fill_manual('Exceedances', values=c('light blue','gray')) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5, color ="black")) +
  geom_text(aes(label=fillwith_XMR), position = position_dodge(0.9), vjust=2, size = 4)


xtable(Backtestres_sub_XMR, caption = "Exceedances for Value at Risk Backtest for Monero - SubSamples", digits = 0)

## Table for AE
fillwith1_XMR <-  c(CrCu_XMR_SGED1_VaR_Test5$actual.exceed/CrCu_XMR_SGED1_VaR_Test5$expected.exceed, CrCu_XMR_SGED1_VaR_Test10$actual.exceed/CrCu_XMR_SGED1_VaR_Test10$expected.exceed,
                    CrCu_XMR_JSU2_VaR_Test1$actual.exceed/CrCu_XMR_JSU2_VaR_Test1$expected.exceed, CrCu_XMR_JSU2_VaR_Test5$actual.exceed/CrCu_XMR_JSU2_VaR_Test5$expected.exceed, CrCu_XMR_JSU2_VaR_Test10$actual.exceed/CrCu_XMR_JSU2_VaR_Test10$expected.exceed,
                    CrCu_XMR_SST3_VaR_Test1$actual.exceed/CrCu_XMR_SST3_VaR_Test1$expected.exceed, CrCu_XMR_SST3_VaR_Test5$actual.exceed/CrCu_XMR_SST3_VaR_Test5$expected.exceed, CrCu_XMR_SST3_VaR_Test10$actual.exceed/CrCu_XMR_SST3_VaR_Test10$expected.exceed)


Backtestres_sub_XMR1 <- matrix(data = fillwith1_XMR, ncol = 1, nrow = 8)
rownames(Backtestres_sub_XMR1) <- c("SGED1 5%", "SGED1 10%","JSU2 1%", "JSU2 5%", "JSU2 10%", "SST3 1%", "SST3 5%", "SST3 10%")
colnames(Backtestres_sub_XMR1) <- c("Actual Exceedances/Expected Exceedances")

## Store the pvalues at a place

CrCu_XMR_VaR_Test_pvalues1 <- matrix(c(CrCu_XMR_SST_VaR_Test1$uc.LRp,CrCu_XMR_SST_VaR_Test1$cc.LRp,
                                       CrCu_XMR_SGED_VaR_Test1$uc.LRp,CrCu_XMR_SGED_VaR_Test1$cc.LRp,
                                       CrCu_XMR_JSU_VaR_Test1$uc.LRp,CrCu_XMR_JSU_VaR_Test1$cc.LRp,
                                       CrCu_XMR_Norm_VaR_Test1$uc.LRp,CrCu_XMR_Norm_VaR_Test1$cc.LRp),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_pvalues5 <- matrix(c(CrCu_XMR_SST_VaR_Test5$uc.LRp,CrCu_XMR_SST_VaR_Test5$cc.LRp,
                                       CrCu_XMR_SGED_VaR_Test5$uc.LRp,CrCu_XMR_SGED_VaR_Test5$cc.LRp,
                                       CrCu_XMR_JSU_VaR_Test5$uc.LRp,CrCu_XMR_JSU_VaR_Test5$cc.LRp,
                                       CrCu_XMR_Norm_VaR_Test5$uc.LRp,CrCu_XMR_Norm_VaR_Test5$cc.LRp),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_pvalues10 <- matrix(c(CrCu_XMR_SST_VaR_Test10$uc.LRp,CrCu_XMR_SST_VaR_Test10$cc.LRp,
                                        CrCu_XMR_SGED_VaR_Test10$uc.LRp,CrCu_XMR_SGED_VaR_Test10$cc.LRp,
                                        CrCu_XMR_JSU_VaR_Test10$uc.LRp,CrCu_XMR_JSU_VaR_Test10$cc.LRp,
                                        CrCu_XMR_Norm_VaR_Test10$uc.LRp,CrCu_XMR_Norm_VaR_Test10$cc.LRp),ncol=2,byrow = TRUE)

## Storing the test stats

CrCu_XMR_VaR_Test_teststat1 <- matrix(c(CrCu_XMR_SST_VaR_Test1$uc.LRstat,CrCu_XMR_SST_VaR_Test1$cc.LRstat,
                                        CrCu_XMR_SGED_VaR_Test1$uc.LRstat,CrCu_XMR_SGED_VaR_Test1$cc.LRstat,
                                        CrCu_XMR_JSU_VaR_Test1$uc.LRstat,CrCu_XMR_JSU_VaR_Test1$cc.LRstat,
                                        CrCu_XMR_Norm_VaR_Test1$uc.LRstat,CrCu_XMR_Norm_VaR_Test1$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_teststat5 <- matrix(c(CrCu_XMR_SST_VaR_Test5$uc.LRstat,CrCu_XMR_SST_VaR_Test5$cc.LRstat,
                                        CrCu_XMR_SGED_VaR_Test5$uc.LRstat,CrCu_XMR_SGED_VaR_Test5$cc.LRstat,
                                        CrCu_XMR_JSU_VaR_Test5$uc.LRstat,CrCu_XMR_JSU_VaR_Test5$cc.LRstat,
                                        CrCu_XMR_Norm_VaR_Test5$uc.LRstat,CrCu_XMR_Norm_VaR_Test5$cc.LRstat),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_teststat10 <- matrix(c(CrCu_XMR_SST_VaR_Test10$uc.LRstat,CrCu_XMR_SST_VaR_Test10$cc.LRstat,
                                         CrCu_XMR_SGED_VaR_Test10$uc.LRstat,CrCu_XMR_SGED_VaR_Test10$cc.LRstat,
                                         CrCu_XMR_JSU_VaR_Test10$uc.LRstat,CrCu_XMR_JSU_VaR_Test10$cc.LRstat,
                                         CrCu_XMR_Norm_VaR_Test10$uc.LRstat,CrCu_XMR_Norm_VaR_Test10$cc.LRstat),ncol=2,byrow = TRUE)
## Round them up to 4 decimals

CrCu_XMR_VaR_Test_pvalues1  <- round(CrCu_XMR_VaR_Test_pvalues1,4)
CrCu_XMR_VaR_Test_pvalues5  <- round(CrCu_XMR_VaR_Test_pvalues5,4)
CrCu_XMR_VaR_Test_pvalues10 <- round(CrCu_XMR_VaR_Test_pvalues10,4)

CrCu_XMR_VaR_Test_teststat1  <- round(CrCu_XMR_VaR_Test_teststat1,4)
CrCu_XMR_VaR_Test_teststat5  <- round(CrCu_XMR_VaR_Test_teststat5,4)
CrCu_XMR_VaR_Test_teststat10 <- round(CrCu_XMR_VaR_Test_teststat10,4)

rownames(CrCu_XMR_VaR_Test_pvalues1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_pvalues1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_XMR_VaR_Test_pvalues5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_pvalues5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_XMR_VaR_Test_pvalues10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_pvalues10)  <- c("Kupiec","Christoffersen")

rownames(CrCu_XMR_VaR_Test_teststat1)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_teststat1)  <- c("Kupiec","Christoffersen")
rownames(CrCu_XMR_VaR_Test_teststat5)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_teststat5)  <- c("Kupiec","Christoffersen")
rownames(CrCu_XMR_VaR_Test_teststat10)  <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
colnames(CrCu_XMR_VaR_Test_teststat10)  <- c("Kupiec","Christoffersen")
# Kupiec H0: observed failure rate is equal to the failure suggested by tests
# Christoffersen H0: Exceedances are not clustering or no consecutive exceedances
# Fail to reject H0 is ideal
# Based on above, H0 is rejected only in case of Normal 1%, so good results

#### VaRDuration Test for subsamples

CrCu_XMR_Dur_Normal_Sample1 <- VaRDurTest(0.10,CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,3])
CrCu_XMR_Dur_Normal_Sample2 <- VaRDurTest(0.10,CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,3])
CrCu_XMR_Dur_Normal_Sample3 <- VaRDurTest(0.10,CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,3])

CrCu_XMR_Dur_SST_Sample1 <- VaRDurTest(0.10,CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,3])
CrCu_XMR_Dur_SST_Sample2 <- VaRDurTest(0.10,CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,3])
CrCu_XMR_Dur_SST_Sample3 <- VaRDurTest(0.10,CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,3])

CrCu_XMR_Dur_SGED_Sample1 <- VaRDurTest(0.10,CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,3])
CrCu_XMR_Dur_SGED_Sample2 <- VaRDurTest(0.10,CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,3])
CrCu_XMR_Dur_SGED_Sample3 <- VaRDurTest(0.10,CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,3])

CrCu_XMR_Dur_JSU_Sample1 <- VaRDurTest(0.10,CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,3])
CrCu_XMR_Dur_JSU_Sample2 <- VaRDurTest(0.10,CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,3])
CrCu_XMR_Dur_JSU_Sample3 <- VaRDurTest(0.10,CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,3])

## Store the results

CrCu_XMR_Dur_Subsample_stat <- matrix(c(CrCu_XMR_Dur_Normal_Sample1$rLL,
                                        CrCu_XMR_Dur_SST_Sample1$rLL,
                                        CrCu_XMR_Dur_SGED_Sample1$rLL,
                                        CrCu_XMR_Dur_JSU_Sample1$rLL,
                                        CrCu_XMR_Dur_Normal_Sample2$rLL,
                                        CrCu_XMR_Dur_SST_Sample2$rLL,
                                        CrCu_XMR_Dur_SGED_Sample2$rLL,
                                        CrCu_XMR_Dur_JSU_Sample2$rLL,
                                        CrCu_XMR_Dur_Normal_Sample3$rLL,
                                        CrCu_XMR_Dur_SST_Sample3$rLL,
                                        CrCu_XMR_Dur_SGED_Sample3$rLL,
                                        CrCu_XMR_Dur_JSU_Sample3$rLL),nrow=4)

CrCu_XMR_Dur_Subsample_pvalues <- matrix(c(CrCu_XMR_Dur_Normal_Sample1$LRp,
                                           CrCu_XMR_Dur_SST_Sample1$LRp,
                                           CrCu_XMR_Dur_SGED_Sample1$LRp,
                                           CrCu_XMR_Dur_JSU_Sample1$LRp,
                                           CrCu_XMR_Dur_Normal_Sample2$LRp,
                                           CrCu_XMR_Dur_SST_Sample2$LRp,
                                           CrCu_XMR_Dur_SGED_Sample2$LRp,
                                           CrCu_XMR_Dur_JSU_Sample2$LRp,
                                           CrCu_XMR_Dur_Normal_Sample3$LRp,
                                           CrCu_XMR_Dur_SST_Sample3$LRp,
                                           CrCu_XMR_Dur_SGED_Sample3$LRp,
                                           CrCu_XMR_Dur_JSU_Sample3$LRp),nrow=4)

### Expected Shortfall Calculations ####

## Assuming Normal Distribution

CrCu_XMR_index_normal1 <- which(CrCu_XMR_Roll8@forecast$VaR[,1]>CrCu_XMR_Roll8@forecast$VaR[,4])
CrCu_XMR_VaR_values_normal1 <- CrCu_XMR_Roll8@forecast$VaR[CrCu_XMR_index_normal1,1]
CrCu_XMR_return_values_normal1 <- CrCu_XMR_Roll8@forecast$VaR[CrCu_XMR_index_normal1,4]
CrCu_XMR_ES_normal1 <- (CrCu_XMR_return_values_normal1/CrCu_XMR_VaR_values_normal1)
CrCu_XMR_ES_Measure1_normal1 <- mean(CrCu_XMR_ES_normal1)
CrCu_XMR_ES_Measure2_normal1 <- max(CrCu_XMR_ES_normal1)

CrCu_XMR_index_normal5 <- which(CrCu_XMR_Roll8@forecast$VaR[,2]>CrCu_XMR_Roll8@forecast$VaR[,4])
CrCu_XMR_VaR_values_normal5 <- CrCu_XMR_Roll8@forecast$VaR[CrCu_XMR_index_normal5,2]
CrCu_XMR_return_values_normal5 <- CrCu_XMR_Roll8@forecast$VaR[CrCu_XMR_index_normal5,4]
CrCu_XMR_ES_normal5 <- (CrCu_XMR_return_values_normal5/CrCu_XMR_VaR_values_normal5)
CrCu_XMR_ES_Measure1_normal5 <- mean(CrCu_XMR_ES_normal5)
CrCu_XMR_ES_Measure2_normal5 <- max(CrCu_XMR_ES_normal5)

CrCu_XMR_index_normal10 <- which(CrCu_XMR_Roll8@forecast$VaR[,3]>CrCu_XMR_Roll8@forecast$VaR[,4])
CrCu_XMR_VaR_values_normal10 <- CrCu_XMR_Roll8@forecast$VaR[CrCu_XMR_index_normal10,3]
CrCu_XMR_return_values_normal10 <- CrCu_XMR_Roll8@forecast$VaR[CrCu_XMR_index_normal10,4]
CrCu_XMR_ES_normal10 <- (CrCu_XMR_return_values_normal10/CrCu_XMR_VaR_values_normal10)
CrCu_XMR_ES_Measure1_normal10 <- mean(CrCu_XMR_ES_normal10)
CrCu_XMR_ES_Measure2_normal10 <- max(CrCu_XMR_ES_normal10)

## Assuming SST Distribution

CrCu_XMR_index_SST1 <- which(CrCu_XMR_Roll5@forecast$VaR[,1]>CrCu_XMR_Roll5@forecast$VaR[,4])
CrCu_XMR_VaR_values_SST1 <- CrCu_XMR_Roll5@forecast$VaR[CrCu_XMR_index_SST1,1]
CrCu_XMR_return_values_SST1 <- CrCu_XMR_Roll5@forecast$VaR[CrCu_XMR_index_SST1,4]
CrCu_XMR_ES_SST1 <- (CrCu_XMR_return_values_SST1/CrCu_XMR_VaR_values_SST1)
CrCu_XMR_ES_Measure1_SST1 <- mean(CrCu_XMR_ES_SST1)
CrCu_XMR_ES_Measure2_SST1 <- max(CrCu_XMR_ES_SST1)

CrCu_XMR_index_SST5 <- which(CrCu_XMR_Roll5@forecast$VaR[,2]>CrCu_XMR_Roll5@forecast$VaR[,4])
CrCu_XMR_VaR_values_SST5 <- CrCu_XMR_Roll5@forecast$VaR[CrCu_XMR_index_SST5,2]
CrCu_XMR_return_values_SST5 <- CrCu_XMR_Roll5@forecast$VaR[CrCu_XMR_index_SST5,4]
CrCu_XMR_ES_SST5 <- (CrCu_XMR_return_values_SST5/CrCu_XMR_VaR_values_SST5)
CrCu_XMR_ES_Measure1_SST5 <- mean(CrCu_XMR_ES_SST5)
CrCu_XMR_ES_Measure2_SST5 <- max(CrCu_XMR_ES_SST5)

CrCu_XMR_index_SST10 <- which(CrCu_XMR_Roll5@forecast$VaR[,3]>CrCu_XMR_Roll5@forecast$VaR[,4])
CrCu_XMR_VaR_values_SST10 <- CrCu_XMR_Roll5@forecast$VaR[CrCu_XMR_index_SST10,3]
CrCu_XMR_return_values_SST10 <- CrCu_XMR_Roll5@forecast$VaR[CrCu_XMR_index_SST10,4]
CrCu_XMR_ES_SST10 <- (CrCu_XMR_return_values_SST10/CrCu_XMR_VaR_values_SST10)
CrCu_XMR_ES_Measure1_SST10 <- mean(CrCu_XMR_ES_SST10)
CrCu_XMR_ES_Measure2_SST10 <- max(CrCu_XMR_ES_SST10)

## Assuming SGED Distribution

CrCu_XMR_index_SGED1 <- which(CrCu_XMR_Roll6@forecast$VaR[,1]>CrCu_XMR_Roll6@forecast$VaR[,4])
CrCu_XMR_VaR_values_SGED1 <- CrCu_XMR_Roll6@forecast$VaR[CrCu_XMR_index_SGED1,1]
CrCu_XMR_return_values_SGED1 <- CrCu_XMR_Roll6@forecast$VaR[CrCu_XMR_index_SGED1,4]
CrCu_XMR_ES_SGED1 <- (CrCu_XMR_return_values_SGED1/CrCu_XMR_VaR_values_SGED1)
CrCu_XMR_ES_Measure1_SGED1 <- mean(CrCu_XMR_ES_SGED1)
CrCu_XMR_ES_Measure2_SGED1 <- max(CrCu_XMR_ES_SGED1)

CrCu_XMR_index_SGED5 <- which(CrCu_XMR_Roll6@forecast$VaR[,2]>CrCu_XMR_Roll6@forecast$VaR[,4])
CrCu_XMR_VaR_values_SGED5 <- CrCu_XMR_Roll6@forecast$VaR[CrCu_XMR_index_SGED5,2]
CrCu_XMR_return_values_SGED5 <- CrCu_XMR_Roll6@forecast$VaR[CrCu_XMR_index_SGED5,4]
CrCu_XMR_ES_SGED5 <- (CrCu_XMR_return_values_SGED5/CrCu_XMR_VaR_values_SGED5)
CrCu_XMR_ES_Measure1_SGED5 <- mean(CrCu_XMR_ES_SGED5)
CrCu_XMR_ES_Measure2_SGED5 <- max(CrCu_XMR_ES_SGED5)

CrCu_XMR_index_SGED10 <- which(CrCu_XMR_Roll6@forecast$VaR[,3]>CrCu_XMR_Roll6@forecast$VaR[,4])
CrCu_XMR_VaR_values_SGED10 <- CrCu_XMR_Roll6@forecast$VaR[CrCu_XMR_index_SGED10,3]
CrCu_XMR_return_values_SGED10 <- CrCu_XMR_Roll6@forecast$VaR[CrCu_XMR_index_SGED10,4]
CrCu_XMR_ES_SGED10 <- (CrCu_XMR_return_values_SGED10/CrCu_XMR_VaR_values_SGED10)
CrCu_XMR_ES_Measure1_SGED10 <- mean(CrCu_XMR_ES_SGED10)
CrCu_XMR_ES_Measure2_SGED10 <- max(CrCu_XMR_ES_SGED10)

## Assuming JSU Distribution

CrCu_XMR_index_JSU1 <- which(CrCu_XMR_Roll7@forecast$VaR[,1]>CrCu_XMR_Roll7@forecast$VaR[,4])
CrCu_XMR_VaR_values_JSU1 <- CrCu_XMR_Roll7@forecast$VaR[CrCu_XMR_index_JSU1,1]
CrCu_XMR_return_values_JSU1 <- CrCu_XMR_Roll7@forecast$VaR[CrCu_XMR_index_JSU1,4]
CrCu_XMR_ES_JSU1 <- (CrCu_XMR_return_values_JSU1/CrCu_XMR_VaR_values_JSU1)
CrCu_XMR_ES_Measure1_JSU1 <- mean(CrCu_XMR_ES_JSU1)
CrCu_XMR_ES_Measure2_JSU1 <- max(CrCu_XMR_ES_JSU1)

CrCu_XMR_index_JSU5 <- which(CrCu_XMR_Roll7@forecast$VaR[,2]>CrCu_XMR_Roll7@forecast$VaR[,4])
CrCu_XMR_VaR_values_JSU5 <- CrCu_XMR_Roll7@forecast$VaR[CrCu_XMR_index_JSU5,2]
CrCu_XMR_return_values_JSU5 <- CrCu_XMR_Roll7@forecast$VaR[CrCu_XMR_index_JSU5,4]
CrCu_XMR_ES_JSU5 <- (CrCu_XMR_return_values_JSU5/CrCu_XMR_VaR_values_JSU5)
CrCu_XMR_ES_Measure1_JSU5 <- mean(CrCu_XMR_ES_JSU5)
CrCu_XMR_ES_Measure2_JSU5 <- max(CrCu_XMR_ES_JSU5)

CrCu_XMR_index_JSU10 <- which(CrCu_XMR_Roll7@forecast$VaR[,3]>CrCu_XMR_Roll7@forecast$VaR[,4])
CrCu_XMR_VaR_values_JSU10 <- CrCu_XMR_Roll7@forecast$VaR[CrCu_XMR_index_JSU10,3]
CrCu_XMR_return_values_JSU10 <- CrCu_XMR_Roll7@forecast$VaR[CrCu_XMR_index_JSU10,4]
CrCu_XMR_ES_JSU10 <- (CrCu_XMR_return_values_JSU10/CrCu_XMR_VaR_values_JSU10)
CrCu_XMR_ES_Measure1_JSU10 <- mean(CrCu_XMR_ES_JSU10)
CrCu_XMR_ES_Measure2_JSU10 <- max(CrCu_XMR_ES_JSU10)

### Assuming Pearson Distribution

Z_Pearson_XMR_01   <- qpearson(0.01,CrCu_XMR_Dist8)
XMR_Holdout_VaR_01   <- XMR_Holdout_Mu + XMR_Holdout_Sigma*Z_Pearson_XMR_01

XMR_index_Pearson1 <- which(XMR_Holdout_VaR_01[-1]>CrCu_XMR_Roll7@forecast$VaR[1:349,4])
XMR_VaR_values_Pearson1 <- XMR_Holdout_VaR_01[XMR_index_Pearson1]
XMR_return_values_Pearson1 <- CrCu_XMR_Roll7@forecast$VaR[XMR_index_Pearson1,4]
XMR_ES_Pearson1 <- (XMR_return_values_Pearson1/XMR_VaR_values_Pearson1)
XMR_ES_Measure1_Pearson1 <- mean(XMR_ES_Pearson1)
XMR_ES_Measure2_Pearson1 <- max(XMR_ES_Pearson1)

Z_Pearson_XMR_05   <- qpearson(0.05,CrCu_XMR_Dist8)
XMR_Holdout_VaR_05   <- XMR_Holdout_Mu + XMR_Holdout_Sigma*Z_Pearson_XMR_05

CrCu_XMR_index_Pearson5 <- which(XMR_Holdout_VaR_05[-1]>CrCu_XMR_Roll7@forecast$VaR[1:349,4])
CrCu_XMR_VaR_values_Pearson5 <- XMR_Holdout_VaR_05[CrCu_XMR_index_Pearson5]
CrCu_XMR_return_values_Pearson5 <- CrCu_XMR_Roll7@forecast$VaR[CrCu_XMR_index_Pearson5,4]
CrCu_XMR_ES_Pearson5 <- (CrCu_XMR_return_values_Pearson5/CrCu_XMR_VaR_values_Pearson5)
CrCu_XMR_ES_Measure1_Pearson5 <- mean(CrCu_XMR_ES_Pearson5)
CrCu_XMR_ES_Measure2_Pearson5 <- max(CrCu_XMR_ES_Pearson5)

Z_Pearson_XMR_10   <- qpearson(0.10,CrCu_XMR_Dist8)
XMR_Holdout_VaR_10   <- XMR_Holdout_Mu + XMR_Holdout_Sigma*Z_Pearson_XMR_10

CrCu_XMR_index_Pearson10 <- which(XMR_Holdout_VaR_10[-1]>CrCu_XMR_Roll7@forecast$VaR[1:349,4])
CrCu_XMR_VaR_values_Pearson10 <- XMR_Holdout_VaR_10[CrCu_XMR_index_Pearson10]
CrCu_XMR_return_values_Pearson10 <- CrCu_XMR_Roll7@forecast$VaR[CrCu_XMR_index_Pearson10,4]
CrCu_XMR_ES_Pearson10 <- (CrCu_XMR_return_values_Pearson10/CrCu_XMR_VaR_values_Pearson10)
CrCu_XMR_ES_Measure1_Pearson10 <- mean(CrCu_XMR_ES_Pearson10)
CrCu_XMR_ES_Measure2_Pearson10 <- max(CrCu_XMR_ES_Pearson10)

### Store for Pearson

Pearson_Full_ES_Measure1_XMR <- matrix(c(XMR_ES_Measure1_Pearson1,CrCu_XMR_ES_Measure1_Pearson5,CrCu_XMR_ES_Measure1_Pearson10),nrow=3)

Pearson_Full_ES_Measure2_XMR <- matrix(c(XMR_ES_Measure2_Pearson1,CrCu_XMR_ES_Measure2_Pearson5,CrCu_XMR_ES_Measure2_Pearson10),nrow=3)

rownames(Pearson_Full_ES_Measure1_XMR) <- c("1%","5%","10%")
rownames(Pearson_Full_ES_Measure2_XMR) <- c("1%","5%","10%")
colnames(Pearson_Full_ES_Measure1_XMR) <- c("XMR")
colnames(Pearson_Full_ES_Measure2_XMR) <- c("XMR")

#### Store the values
CrCu_XMR_Full_ES_Measure1 <- matrix(c(CrCu_XMR_ES_Measure1_normal1,CrCu_XMR_ES_Measure1_SST1,CrCu_XMR_ES_Measure1_SGED1,CrCu_XMR_ES_Measure1_JSU1,
                                      CrCu_XMR_ES_Measure1_normal5,CrCu_XMR_ES_Measure1_SST5,CrCu_XMR_ES_Measure1_SGED5,CrCu_XMR_ES_Measure1_JSU5,
                                      CrCu_XMR_ES_Measure1_normal10,CrCu_XMR_ES_Measure1_SST10,CrCu_XMR_ES_Measure1_SGED10,CrCu_XMR_ES_Measure1_JSU10),nrow=3,byrow=TRUE)

CrCu_XMR_Full_ES_Measure2 <- matrix(c(CrCu_XMR_ES_Measure2_normal1,CrCu_XMR_ES_Measure2_SST1,CrCu_XMR_ES_Measure2_SGED1,CrCu_XMR_ES_Measure2_JSU1,
                                      CrCu_XMR_ES_Measure2_normal5,CrCu_XMR_ES_Measure2_SST5,CrCu_XMR_ES_Measure2_SGED5,CrCu_XMR_ES_Measure2_JSU5,
                                      CrCu_XMR_ES_Measure2_normal10,CrCu_XMR_ES_Measure2_SST10,CrCu_XMR_ES_Measure2_SGED10,CrCu_XMR_ES_Measure2_JSU10),nrow=3,byrow=TRUE)

rownames(CrCu_XMR_Full_ES_Measure1) <- c("1%","5%","10%")
rownames(CrCu_XMR_Full_ES_Measure2) <- c("1%","5%","10%")

colnames(CrCu_XMR_Full_ES_Measure1) <- c("Normal","SST","SGED","JSU")
colnames(CrCu_XMR_Full_ES_Measure2) <- c("Normal","SST","SGED","JSU")

#### End of all the code/lines ###### 