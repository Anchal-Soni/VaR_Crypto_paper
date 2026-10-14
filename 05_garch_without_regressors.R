# =============================================================================
# 05_garch_without_regressors.R
#
# Purpose : Full-sample EGARCH models WITHOUT structural-break dummies (comparison to
#           the break-adjusted models): AIC, VaR, backtests, MAPE/RMSE, VaR plot.
# Inputs  : data/Cryptocurrency6.csv
# Outputs : VR*_<coin>_wo objects; output/figures/FSVaR_G_01_wo.eps
# Paper   : comparison rows 'FS_WO' in the QPS/AE tables
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

######### Without external regressors
## GARCH models, AIC, VaR, Backtest, MAPE, RMSE, VaR plots

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

# Crypto1 - without date column #

Crypto1 <- data.frame(Crypto1$Bitcoin, Crypto1$Ethereum, Crypto1$Litecoin, Crypto1$Monero, Crypto1$Ethereum.Classic)
colnames(Crypto1) <- c("BTC", "ETH", "LTC", "XMR", "ETC")
View(Crypto1)
#### ********** B I T C O I N ********** ####
Crypto1$BTC = as.numeric(as.character(Crypto1$BTC))
CrCu_BTC <- data.frame(Crypto_Date, Crypto$Bitcoin) # Bitcoin Data with Date
View(CrCu_BTC)

####### :::::::::: Returns Series

Returns_CrCu_BTC <- log(Crypto1$BTC[-1]/Crypto1$BTC[1:(nrow(Crypto1)-1)]) # Returns series
Des_Returns_CrCu_BTC <- basicStats(Returns_CrCu_BTC) # Descriptive statistics
View(Returns_CrCu_BTC)

#### Convert into xts objects ####

CrCu_BTC_xts   <- xts(Returns_CrCu_BTC,order.by = Crypto_Date[-1])
names(CrCu_BTC_xts)   <- "Returns"
View(CrCu_BTC_xts)

## :::::::::::: check presence of ARCH effect in Full Sample
library(FinTS)
ArchTest(CrCu_BTC_xts, lags = 1)
ArchTest(CrCu_BTC_xts, lags = 5)
ArchTest(CrCu_BTC_xts, lags = 10)

# H0: Series has no ARCH effects
# Null hypothesis is rejected, hence proves presence of ARCH effects

### Garch Models
# :::::::::Without external regressors in Full Sample GARCH

#SST #SGED #JSU #Norm
CrCu_BTC_Spec1_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_BTC_Spec2_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_BTC_Spec3_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_BTC_Spec4_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_BTC_Model1_wo <- ugarchfit(CrCu_BTC_Spec1_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model2_wo <- ugarchfit(CrCu_BTC_Spec2_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model3_wo <- ugarchfit(CrCu_BTC_Spec3_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model4_wo <- ugarchfit(CrCu_BTC_Spec4_wo,CrCu_BTC_xts,out.sample = 350)

#### egarch models

CrCu_BTC_Spec5_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_BTC_Spec6_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_BTC_Spec7_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_BTC_Spec8_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_BTC_Model5_wo <- ugarchfit(CrCu_BTC_Spec5_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model6_wo <- ugarchfit(CrCu_BTC_Spec6_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model7_wo <- ugarchfit(CrCu_BTC_Spec7_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model8_wo <- ugarchfit(CrCu_BTC_Spec8_wo,CrCu_BTC_xts,out.sample = 350)

### gjrgarch models

CrCu_BTC_Spec9_wo  <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_BTC_Spec10_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_BTC_Spec11_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_BTC_Spec12_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_BTC_Model9_wo  <- ugarchfit(CrCu_BTC_Spec9_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model10_wo <- ugarchfit(CrCu_BTC_Spec10_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model11_wo <- ugarchfit(CrCu_BTC_Spec11_wo,CrCu_BTC_xts,out.sample = 350)
CrCu_BTC_Model12_wo <- ugarchfit(CrCu_BTC_Spec12_wo,CrCu_BTC_xts,out.sample = 350)

## persistence

persis_BTC_wo <- matrix(data=0, nrow = 12, ncol = 1)
persis_BTC_wo[1,1] <- round(persistence(CrCu_BTC_Model1_wo),4)
persis_BTC_wo[2,1] <- round(persistence(CrCu_BTC_Model2_wo),4)
persis_BTC_wo[3,1] <- round(persistence(CrCu_BTC_Model3_wo),4)
persis_BTC_wo[4,1] <- round(persistence(CrCu_BTC_Model4_wo),4)
persis_BTC_wo[5,1] <- round(persistence(CrCu_BTC_Model5_wo),4)
persis_BTC_wo[6,1] <- round(persistence(CrCu_BTC_Model6_wo),4)
persis_BTC_wo[7,1] <- round(persistence(CrCu_BTC_Model7_wo),4)
persis_BTC_wo[8,1] <- round(persistence(CrCu_BTC_Model8_wo),4)
persis_BTC_wo[9,1] <- round(persistence(CrCu_BTC_Model9_wo),4)
persis_BTC_wo[10,1] <- round(persistence(CrCu_BTC_Model10_wo),4)
persis_BTC_wo[11,1] <- round(persistence(CrCu_BTC_Model11_wo),4)
persis_BTC_wo[12,1] <- round(persistence(CrCu_BTC_Model12_wo),4)

persistance_BTC_wo <- matrix(data=persis_BTC_wo, nrow = 12, ncol = 1)
colnames(persistance_BTC_wo) <- c("Persistence_BTC")
rownames(persistance_BTC_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(data.frame(persistance_BTC_wo))
#xtable(persistance_BTC, digits = 4)

### Store the AIC values  ####

AIC_BTC_wo  <- matrix(data=0,nrow=12,ncol=1)
colnames(AIC_BTC_wo) <- c("CrCu_BTC")
rownames(AIC_BTC_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
AIC_BTC_wo[1,1]  <-  infocriteria(CrCu_BTC_Model1_wo)[1]
AIC_BTC_wo[2,1]  <-  infocriteria(CrCu_BTC_Model2_wo)[1]
AIC_BTC_wo[3,1]  <-  infocriteria(CrCu_BTC_Model3_wo)[1]
AIC_BTC_wo[4,1]  <-  infocriteria(CrCu_BTC_Model4_wo)[1]
AIC_BTC_wo[5,1]  <-  infocriteria(CrCu_BTC_Model5_wo)[1]
AIC_BTC_wo[6,1]  <-  infocriteria(CrCu_BTC_Model6_wo)[1]
AIC_BTC_wo[7,1]  <-  infocriteria(CrCu_BTC_Model7_wo)[1]
AIC_BTC_wo[8,1]  <-  infocriteria(CrCu_BTC_Model8_wo)[1]
AIC_BTC_wo[9,1]  <-  infocriteria(CrCu_BTC_Model9_wo)[1]
AIC_BTC_wo[10,1] <- infocriteria(CrCu_BTC_Model10_wo)[1]
AIC_BTC_wo[11,1] <- infocriteria(CrCu_BTC_Model11_wo)[1]
AIC_BTC_wo[12,1] <- infocriteria(CrCu_BTC_Model12_wo)[1]
View(AIC_BTC_wo)
xtable(AIC_BTC_wo, caption = "Information Criteria for Bitcoin GARCH models - Full Sample", digits = 4)

## Conclusion: eGarch-JSU have the minimum AIC and eGarch-SST have the second minimum #####
## Model 7 and the Model 5

## coefficients

coef_BTC_wo_1 <- round(coef(CrCu_BTC_Model1_wo),4)
coef_BTC_wo_2 <- round(coef(CrCu_BTC_Model2_wo),4)
coef_BTC_wo_3 <- round(coef(CrCu_BTC_Model3_wo),4)
coef_BTC_wo_4 <- round(coef(CrCu_BTC_Model4_wo),4)
coef_BTC_wo_5 <- round(coef(CrCu_BTC_Model5_wo),4)
coef_BTC_wo_6 <- round(coef(CrCu_BTC_Model6_wo),4)
coef_BTC_wo_7 <- round(coef(CrCu_BTC_Model7_wo),4)
coef_BTC_wo_8 <- round(coef(CrCu_BTC_Model8_wo),4)
coef_BTC_wo_9 <- round(coef(CrCu_BTC_Model9_wo),4)
coef_BTC_wo_10 <- round(coef(CrCu_BTC_Model10_wo),4)
coef_BTC_wo_11 <- round(coef(CrCu_BTC_Model11_wo),4)
coef_BTC_wo_12 <- round(coef(CrCu_BTC_Model12_wo),4)

garch_coef_BTC_wo <- bind_rows(coef_BTC_wo_1, coef_BTC_wo_2, coef_BTC_wo_3, coef_BTC_wo_4,coef_BTC_wo_5, coef_BTC_wo_6, coef_BTC_wo_7, coef_BTC_wo_8, coef_BTC_wo_9, coef_BTC_wo_10, coef_BTC_wo_11, coef_BTC_wo_12)

garch_coef_BTC_wo1 <- as.data.frame(garch_coef_BTC_wo)
rownames(garch_coef_BTC_wo1) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(garch_coef_BTC_wo1)
xtable(garch_coef_BTC_wo1, caption = "Coefficients of Bitcoin GARCH models - Full Sample", digits = 4)

### Ljung-box test statistic
# Check standardized shocks

#CrCu_BTC5_LQ1_wo  <- Box.test(CrCu_BTC_Model5_wo@fit$residuals/CrCu_BTC_Model5_wo@fit$sigma,lag=1,type="Lj")
#CrCu_BTC5_LQ5_wo  <- Box.test(CrCu_BTC_Model5_wo@fit$residuals/CrCu_BTC_Model5_wo@fit$sigma,lag=5,type="Lj")
#CrCu_BTC5_LQ10_wo <- Box.test(CrCu_BTC_Model5_wo@fit$residuals/CrCu_BTC_Model5_wo@fit$sigma,lag=10,type="Lj")

#CrCu_BTC6_LQ1_wo  <- Box.test(CrCu_BTC_Model6_wo@fit$residuals/CrCu_BTC_Model6_wo@fit$sigma,lag=1,type="Lj")
#CrCu_BTC6_LQ5_wo  <- Box.test(CrCu_BTC_Model6_wo@fit$residuals/CrCu_BTC_Model6_wo@fit$sigma,lag=5,type="Lj")
#CrCu_BTC6_LQ10_wo <- Box.test(CrCu_BTC_Model6_wo@fit$residuals/CrCu_BTC_Model6_wo@fit$sigma,lag=10,type="Lj")

#CrCu_BTC7_LQ1_wo  <- Box.test(CrCu_BTC_Model7_wo@fit$residuals/CrCu_BTC_Model7_wo@fit$sigma,lag=1,type="Lj")
#CrCu_BTC7_LQ5_wo  <- Box.test(CrCu_BTC_Model7_wo@fit$residuals/CrCu_BTC_Model7_wo@fit$sigma,lag=5,type="Lj")
#CrCu_BTC7_LQ10_wo <- Box.test(CrCu_BTC_Model7_wo@fit$residuals/CrCu_BTC_Model7_wo@fit$sigma,lag=10,type="Lj")

#CrCu_BTC8_LQ1_wo  <- Box.test(CrCu_BTC_Model8_wo@fit$residuals/CrCu_BTC_Model8_wo@fit$sigma,lag=1,type="Lj")
#CrCu_BTC8_LQ5_wo  <- Box.test(CrCu_BTC_Model8_wo@fit$residuals/CrCu_BTC_Model8_wo@fit$sigma,lag=5,type="Lj")
#CrCu_BTC8_LQ10_wo <- Box.test(CrCu_BTC_Model8_wo@fit$residuals/CrCu_BTC_Model8_wo@fit$sigma,lag=10,type="Lj")

#CrCu_BTC5_LQ_wo <- c(CrCu_BTC5_LQ1_wo$statistic,CrCu_BTC5_LQ5_wo$statistic,CrCu_BTC5_LQ10_wo$statistic)
#CrCu_BTC6_LQ_wo <- c(CrCu_BTC6_LQ1_wo$statistic,CrCu_BTC6_LQ5_wo$statistic,CrCu_BTC6_LQ10_wo$statistic)
#CrCu_BTC7_LQ_wo <- c(CrCu_BTC7_LQ1_wo$statistic,CrCu_BTC7_LQ5_wo$statistic,CrCu_BTC7_LQ10_wo$statistic)
#CrCu_BTC8_LQ_wo <- c(CrCu_BTC8_LQ1_wo$statistic,CrCu_BTC8_LQ5_wo$statistic,CrCu_BTC8_LQ10_wo$statistic)

#CrCu_BTC_res_stat_wo <-   cbind(CrCu_BTC5_LQ_wo,CrCu_BTC6_LQ_wo,CrCu_BTC7_LQ_wo,CrCu_BTC8_LQ_wo)
#rownames(CrCu_BTC_res_stat_wo) <- c("LQ1","LQ5","LQ10")
#colnames(CrCu_BTC_res_stat_wo) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

#### 1-day ahead Rolling Forecasts ####
## Full sample for roll i.e. last 350 points for forecast and rest for model
CrCu_BTC_Roll5_wo <-  ugarchroll(CrCu_BTC_Spec5_wo,CrCu_BTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll6_wo <-  ugarchroll(CrCu_BTC_Spec6_wo,CrCu_BTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll7_wo <-  ugarchroll(CrCu_BTC_Spec7_wo,CrCu_BTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_BTC_Roll8_wo <-  ugarchroll(CrCu_BTC_Spec8_wo,CrCu_BTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model
# Check in the last few lines of the code

#### Store the VaR for the out sample using rolling window forecasts

CrCu_BTC_VaR_SST_wo   <- CrCu_BTC_Roll5_wo@forecast$VaR
CrCu_BTC_VaR_SGED_wo  <- CrCu_BTC_Roll6_wo@forecast$VaR
CrCu_BTC_VaR_JSU_wo   <- CrCu_BTC_Roll7_wo@forecast$VaR
CrCu_BTC_VaR_Norm_wo  <- CrCu_BTC_Roll8_wo@forecast$VaR

#### ::::::::::::: Repeating for P E A R S O N Distribution  #####

### Check the summary stats of the standardized residuals
CrCu_BTC_Model8_res_wo <- CrCu_BTC_Model8_wo@fit$residuals/CrCu_BTC_Model8_wo@fit$sigma
CrCu_BTC_Descr8_wo <- basicStats(CrCu_BTC_Model8_res_wo)

### Fitting distributions on the standardized residuals 

library(PearsonDS)
library(gsl)
CrCu_BTC_Dist8_wo   <- pearsonFitML(CrCu_BTC_Model8_res_wo)

### Store the parameters for different distributions  

CrCu_BTC_P4_Param_wo    <- CrCu_BTC_Dist8_wo[2:5]

### Checking the fit using KS tests

CrCu_BTC_test1_wo <- ks.test(CrCu_BTC_Model8_res_wo,rpearson(length(CrCu_BTC_Model8_res_wo),params = CrCu_BTC_Dist8_wo))
#Null: True distribution is equal to the hypothesized distribution
#p-value is high, hence, we fail to reject H0.

### Calculate the quantiles

BTC_Exceed_Pearson_wo    <- array(data=0,dim=length(signific_levels))
#qpearson(signific_levels[2],CrCu_BTC_Dist8_wo)
for(j in 1:3) {
  Z_Pearson_BTC_wo   <- qpearson(signific_levels[j],CrCu_BTC_Dist8_wo)
  
  mu_BTC_wo   <- array(data=0,dim=length(Returns_CrCu_BTC)-350)
  VaR_BTC_wo   <- array(data=0,dim=length(Returns_CrCu_BTC)-351)
  
  for(i in 2:(length(Returns_CrCu_BTC)-350))
  {
    mu_BTC_wo[i] <- CrCu_BTC_Model8_wo@fit$coef[1]+CrCu_BTC_Model8_wo@fit$coef[2]*Returns_CrCu_BTC[i-1]+CrCu_BTC_Model8_wo@fit$coef[3]*CrCu_BTC_Model8_res_wo[i-1]
    VaR_BTC_wo[i-1] <- mu_BTC_wo[i]+CrCu_BTC_Model8_wo@fit$sigma[i]*Z_Pearson_BTC_wo
  }
  print(mu_BTC_wo)
  print(CrCu_BTC_Model8_wo@fit$sigma*Z_Pearson_BTC_wo)
  #BTC_Violations_wo[j]   <- VaRTest(signific_levels[j],Returns_CrCu_BTC[1:(length(VaR_BTC_wo))],VaR_BTC_wo)
  #print(VaRTest(signific_levels[j],Returns_CrCu_BTC[1:(length(VaR_BTC_wo))],VaR_BTC_wo))
  #BTC_Exceed_Pearson_wo[j] <- BTC_Violations_wo$actual.exceed
}

#### Out of sample VaR forecast  #####

Holdout_Returns_BTC_wo   <- Returns_CrCu_BTC[(length(Returns_CrCu_BTC)-349):length(Returns_CrCu_BTC)] 

Holdout_BTC_Pearson_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_BTC_Pearson_pvalues_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_BTC_Pearson_wo)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_BTC_Pearson_pvalues_wo)   <- c("UC","CC","DQ")
rownames(Holdout_BTC_Pearson_wo) <- c("1%","5%","10%")
rownames(Holdout_BTC_Pearson_pvalues_wo) <- c("1%","5%","10%")

## Store the volatility and mu forecast
signific_levels <- c(0.1,0.05,0.01)

BTC_Holdout_Mu_wo <- as.data.frame(CrCu_BTC_Roll8_wo)[,'Mu']
BTC_Holdout_Sigma_wo <- as.data.frame(CrCu_BTC_Roll8_wo)[,'Sigma']

MP_BTC_wo <- c(0 , 0, 0)
Holdout_BTC_Pearson_wo <- matrix(0, nrow = 3, ncol = 5)
Holdout_BTC_Pearson_pvalues_wo <- matrix(0, nrow = 3, ncol = 3)

for(j in 1:3) {
  
  Z_Pearson_BTC_wo   <- qpearson(signific_levels[j],CrCu_BTC_Dist8_wo)
  
  BTC_Holdout_VaR_wo   <- BTC_Holdout_Mu_wo + BTC_Holdout_Sigma_wo*Z_Pearson_BTC_wo
  BTC_Violations_wo   <- BacktestVaR(Holdout_Returns_BTC_wo[1:(length(Holdout_Returns_BTC_wo)-1)],BTC_Holdout_VaR_wo[-1],signific_levels[j])
  Holdout_BTC_Pearson_wo[j,]   <- c(BTC_Violations_wo$LRuc[1],BTC_Violations_wo$LRcc[1],BTC_Violations_wo$DQ$stat,BTC_Violations_wo$AE,BTC_Violations_wo$Loss$Loss)
  Holdout_BTC_Pearson_pvalues_wo[j,] <- c(BTC_Violations_wo$LRuc[2],BTC_Violations_wo$LRcc[2],BTC_Violations_wo$DQ$pvalue)
  MP_BTC_wo[j] <- MAPE(BTC_Holdout_Sigma_wo*Z_Pearson_BTC_wo, abs(CrCu_BTC_Roll7_wo@forecast$density$Realized))
  #print(BTC_Violations_wo$AE)
  #print(BTC_Violations)
  print(MP_BTC_wo)
}

#####################################################################################
### Using VaR Test individually ###
## VaRTest in R

CrCu_BTC_Norm_VaR_Test1_wo  <- VaRTest(0.01,CrCu_BTC_VaR_Norm_wo[,4],CrCu_BTC_VaR_Norm_wo[,1])
CrCu_BTC_Norm_VaR_Test5_wo  <- VaRTest(0.05,CrCu_BTC_VaR_Norm_wo[,4],CrCu_BTC_VaR_Norm_wo[,2])
CrCu_BTC_Norm_VaR_Test10_wo <- VaRTest(0.1,CrCu_BTC_VaR_Norm_wo[,4],CrCu_BTC_VaR_Norm_wo[,3])

CrCu_BTC_JSU_VaR_Test1_wo  <- VaRTest(0.01,CrCu_BTC_VaR_JSU_wo[,4],CrCu_BTC_VaR_JSU_wo[,1])
CrCu_BTC_JSU_VaR_Test5_wo  <- VaRTest(0.05,CrCu_BTC_VaR_JSU_wo[,4],CrCu_BTC_VaR_JSU_wo[,2])
CrCu_BTC_JSU_VaR_Test10_wo <- VaRTest(0.1,CrCu_BTC_VaR_JSU_wo[,4],CrCu_BTC_VaR_JSU_wo[,3])

CrCu_BTC_SGED_VaR_Test1_wo  <- VaRTest(0.01,CrCu_BTC_VaR_SGED_wo[,4],CrCu_BTC_VaR_SGED_wo[,1])
CrCu_BTC_SGED_VaR_Test5_wo  <- VaRTest(0.05,CrCu_BTC_VaR_SGED_wo[,4],CrCu_BTC_VaR_SGED_wo[,2])
CrCu_BTC_SGED_VaR_Test10_wo <- VaRTest(0.1,CrCu_BTC_VaR_SGED_wo[,4],CrCu_BTC_VaR_SGED_wo[,3])

CrCu_BTC_SST_VaR_Test1_wo  <- VaRTest(0.01,CrCu_BTC_VaR_SST_wo[,4],CrCu_BTC_VaR_SST_wo[,1])
CrCu_BTC_SST_VaR_Test5_wo  <- VaRTest(0.05,CrCu_BTC_VaR_SST_wo[,4],CrCu_BTC_VaR_SST_wo[,2])
CrCu_BTC_SST_VaR_Test10_wo <- VaRTest(0.1,CrCu_BTC_VaR_SST_wo[,4],CrCu_BTC_VaR_SST_wo[,3])

CrCu_BTC_VaR_Test_pvalues1_wo <- matrix(c(CrCu_BTC_SST_VaR_Test1_wo$uc.LRp,CrCu_BTC_SST_VaR_Test1_wo$cc.LRp,
                                       CrCu_BTC_SGED_VaR_Test1_wo$uc.LRp,CrCu_BTC_SGED_VaR_Test1_wo$cc.LRp,
                                       CrCu_BTC_JSU_VaR_Test1_wo$uc.LRp,CrCu_BTC_JSU_VaR_Test1_wo$cc.LRp,
                                       CrCu_BTC_Norm_VaR_Test1_wo$uc.LRp,CrCu_BTC_Norm_VaR_Test1_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_pvalues5_wo <- matrix(c(CrCu_BTC_SST_VaR_Test5_wo$uc.LRp,CrCu_BTC_SST_VaR_Test5_wo$cc.LRp,
                                       CrCu_BTC_SGED_VaR_Test5_wo$uc.LRp,CrCu_BTC_SGED_VaR_Test5_wo$cc.LRp,
                                       CrCu_BTC_JSU_VaR_Test5_wo$uc.LRp,CrCu_BTC_JSU_VaR_Test5_wo$cc.LRp,
                                       CrCu_BTC_Norm_VaR_Test5_wo$uc.LRp,CrCu_BTC_Norm_VaR_Test5_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_BTC_VaR_Test_pvalues10_wo <- matrix(c(CrCu_BTC_SST_VaR_Test10_wo$uc.LRp,CrCu_BTC_SST_VaR_Test10_wo$cc.LRp,
                                        CrCu_BTC_SGED_VaR_Test10_wo$uc.LRp,CrCu_BTC_SGED_VaR_Test10_wo$cc.LRp,
                                        CrCu_BTC_JSU_VaR_Test10_wo$uc.LRp,CrCu_BTC_JSU_VaR_Test10_wo$cc.LRp,
                                        CrCu_BTC_Norm_VaR_Test10_wo$uc.LRp,CrCu_BTC_Norm_VaR_Test10_wo$cc.LRp),ncol=2,byrow = TRUE)
###################:::::::::::::::::::::::::::::::::::::::::############################

#### ********** E T H E R E U M ********** ####
Crypto1$ETH = as.numeric(as.character(Crypto1$ETH))
CrCu_ETH <- data.frame(Crypto_Date, Crypto$Bitcoin) # Bitcoin Data with Date
View(CrCu_ETH)

####### :::::::::: Returns Series

Returns_CrCu_ETH <- log(Crypto1$ETH[-1]/Crypto1$ETH[1:(nrow(Crypto1)-1)]) # Returns series
Des_Returns_CrCu_ETH <- basicStats(Returns_CrCu_ETH) # Descriptive statistics
View(Returns_CrCu_ETH)

#### Convert into xts objects ####

CrCu_ETH_xts   <- xts(Returns_CrCu_ETH,order.by = Crypto_Date[-1])
names(CrCu_ETH_xts)   <- "Returns"
View(CrCu_ETH_xts)

## :::::::::::: check presence of ARCH effect in Full Sample
library(FinTS)
ArchTest(CrCu_ETH_xts, lags = 1)
ArchTest(CrCu_ETH_xts, lags = 5)
ArchTest(CrCu_ETH_xts, lags = 10)

# H0: Series has no ARCH effects
# Null hypothesis is rejected, hence proves presence of ARCH effects

### Garch Models
# :::::::::Without external regressors in Full Sample GARCH

#SST #SGED #JSU #Norm
CrCu_ETH_Spec1_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETH_Spec2_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETH_Spec3_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETH_Spec4_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETH_Model1_wo <- ugarchfit(CrCu_ETH_Spec1_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model2_wo <- ugarchfit(CrCu_ETH_Spec2_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model3_wo <- ugarchfit(CrCu_ETH_Spec3_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model4_wo <- ugarchfit(CrCu_ETH_Spec4_wo,CrCu_ETH_xts,out.sample = 350)

#### egarch models

CrCu_ETH_Spec5_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETH_Spec6_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETH_Spec7_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETH_Spec8_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETH_Model5_wo <- ugarchfit(CrCu_ETH_Spec5_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model6_wo <- ugarchfit(CrCu_ETH_Spec6_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model7_wo <- ugarchfit(CrCu_ETH_Spec7_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model8_wo <- ugarchfit(CrCu_ETH_Spec8_wo,CrCu_ETH_xts,out.sample = 350)

### gjrgarch models

CrCu_ETH_Spec9_wo  <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETH_Spec10_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETH_Spec11_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETH_Spec12_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETH_Model9_wo  <- ugarchfit(CrCu_ETH_Spec9_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model10_wo <- ugarchfit(CrCu_ETH_Spec10_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model11_wo <- ugarchfit(CrCu_ETH_Spec11_wo,CrCu_ETH_xts,out.sample = 350)
CrCu_ETH_Model12_wo <- ugarchfit(CrCu_ETH_Spec12_wo,CrCu_ETH_xts,out.sample = 350)

## persistence

persis_ETH_wo <- matrix(data=0, nrow = 12, ncol = 1)
persis_ETH_wo[1,1] <- round(persistence(CrCu_ETH_Model1_wo),4)
persis_ETH_wo[2,1] <- round(persistence(CrCu_ETH_Model2_wo),4)
persis_ETH_wo[3,1] <- round(persistence(CrCu_ETH_Model3_wo),4)
persis_ETH_wo[4,1] <- round(persistence(CrCu_ETH_Model4_wo),4)
persis_ETH_wo[5,1] <- round(persistence(CrCu_ETH_Model5_wo),4)
persis_ETH_wo[6,1] <- round(persistence(CrCu_ETH_Model6_wo),4)
persis_ETH_wo[7,1] <- round(persistence(CrCu_ETH_Model7_wo),4)
persis_ETH_wo[8,1] <- round(persistence(CrCu_ETH_Model8_wo),4)
persis_ETH_wo[9,1] <- round(persistence(CrCu_ETH_Model9_wo),4)
persis_ETH_wo[10,1] <- round(persistence(CrCu_ETH_Model10_wo),4)
persis_ETH_wo[11,1] <- round(persistence(CrCu_ETH_Model11_wo),4)
persis_ETH_wo[12,1] <- round(persistence(CrCu_ETH_Model12_wo),4)

persistance_ETH_wo <- matrix(data=persis_ETH_wo, nrow = 12, ncol = 1)
colnames(persistance_ETH_wo) <- c("Persistence_ETH")
rownames(persistance_ETH_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(data.frame(persistance_ETH_wo))
#xtable(persistance_ETH, digits = 4)

### Store the AIC values  ####

AIC_ETH_wo  <- matrix(data=0,nrow=12,ncol=1)
colnames(AIC_ETH_wo) <- c("CrCu_ETH")
rownames(AIC_ETH_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
AIC_ETH_wo[1,1]  <-  infocriteria(CrCu_ETH_Model1_wo)[1]
AIC_ETH_wo[2,1]  <-  infocriteria(CrCu_ETH_Model2_wo)[1]
AIC_ETH_wo[3,1]  <-  infocriteria(CrCu_ETH_Model3_wo)[1]
AIC_ETH_wo[4,1]  <-  infocriteria(CrCu_ETH_Model4_wo)[1]
AIC_ETH_wo[5,1]  <-  infocriteria(CrCu_ETH_Model5_wo)[1]
AIC_ETH_wo[6,1]  <-  infocriteria(CrCu_ETH_Model6_wo)[1]
AIC_ETH_wo[7,1]  <-  infocriteria(CrCu_ETH_Model7_wo)[1]
AIC_ETH_wo[8,1]  <-  infocriteria(CrCu_ETH_Model8_wo)[1]
AIC_ETH_wo[9,1]  <-  infocriteria(CrCu_ETH_Model9_wo)[1]
AIC_ETH_wo[10,1] <- infocriteria(CrCu_ETH_Model10_wo)[1]
AIC_ETH_wo[11,1] <- infocriteria(CrCu_ETH_Model11_wo)[1]
AIC_ETH_wo[12,1] <- infocriteria(CrCu_ETH_Model12_wo)[1]
View(AIC_ETH_wo)
xtable(AIC_ETH_wo, caption = "Information Criteria for Bitcoin GARCH models - Full Sample", digits = 4)

## Conclusion: Garch-SGED have the minimum AIC and eGarch-SGED is the next#####

## coefficients

coef_ETH_wo_1 <- round(coef(CrCu_ETH_Model1_wo),4)
coef_ETH_wo_2 <- round(coef(CrCu_ETH_Model2_wo),4)
coef_ETH_wo_3 <- round(coef(CrCu_ETH_Model3_wo),4)
coef_ETH_wo_4 <- round(coef(CrCu_ETH_Model4_wo),4)
coef_ETH_wo_5 <- round(coef(CrCu_ETH_Model5_wo),4)
coef_ETH_wo_6 <- round(coef(CrCu_ETH_Model6_wo),4)
coef_ETH_wo_7 <- round(coef(CrCu_ETH_Model7_wo),4)
coef_ETH_wo_8 <- round(coef(CrCu_ETH_Model8_wo),4)
coef_ETH_wo_9 <- round(coef(CrCu_ETH_Model9_wo),4)
coef_ETH_wo_10 <- round(coef(CrCu_ETH_Model10_wo),4)
coef_ETH_wo_11 <- round(coef(CrCu_ETH_Model11_wo),4)
coef_ETH_wo_12 <- round(coef(CrCu_ETH_Model12_wo),4)

garch_coef_ETH_wo <- bind_rows(coef_ETH_wo_1, coef_ETH_wo_2, coef_ETH_wo_3, coef_ETH_wo_4,coef_ETH_wo_5, coef_ETH_wo_6, coef_ETH_wo_7, coef_ETH_wo_8, coef_ETH_wo_9, coef_ETH_wo_10, coef_ETH_wo_11, coef_ETH_wo_12)

garch_coef_ETH_wo1 <- as.data.frame(garch_coef_ETH_wo)
rownames(garch_coef_ETH_wo1) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(garch_coef_ETH_wo1)
xtable(garch_coef_ETH_wo1, caption = "Coefficients of Bitcoin GARCH models - Full Sample", digits = 4)

### Ljung-box test statistic
# Check standardized shocks

#CrCu_ETH5_LQ1_wo  <- Box.test(CrCu_ETH_Model5_wo@fit$residuals/CrCu_ETH_Model5_wo@fit$sigma,lag=1,type="Lj")
#CrCu_ETH5_LQ5_wo  <- Box.test(CrCu_ETH_Model5_wo@fit$residuals/CrCu_ETH_Model5_wo@fit$sigma,lag=5,type="Lj")
#CrCu_ETH5_LQ10_wo <- Box.test(CrCu_ETH_Model5_wo@fit$residuals/CrCu_ETH_Model5_wo@fit$sigma,lag=10,type="Lj")

#CrCu_ETH6_LQ1_wo  <- Box.test(CrCu_ETH_Model6_wo@fit$residuals/CrCu_ETH_Model6_wo@fit$sigma,lag=1,type="Lj")
#CrCu_ETH6_LQ5_wo  <- Box.test(CrCu_ETH_Model6_wo@fit$residuals/CrCu_ETH_Model6_wo@fit$sigma,lag=5,type="Lj")
#CrCu_ETH6_LQ10_wo <- Box.test(CrCu_ETH_Model6_wo@fit$residuals/CrCu_ETH_Model6_wo@fit$sigma,lag=10,type="Lj")

#CrCu_ETH7_LQ1_wo  <- Box.test(CrCu_ETH_Model7_wo@fit$residuals/CrCu_ETH_Model7_wo@fit$sigma,lag=1,type="Lj")
#CrCu_ETH7_LQ5_wo  <- Box.test(CrCu_ETH_Model7_wo@fit$residuals/CrCu_ETH_Model7_wo@fit$sigma,lag=5,type="Lj")
#CrCu_ETH7_LQ10_wo <- Box.test(CrCu_ETH_Model7_wo@fit$residuals/CrCu_ETH_Model7_wo@fit$sigma,lag=10,type="Lj")

#CrCu_ETH8_LQ1_wo  <- Box.test(CrCu_ETH_Model8_wo@fit$residuals/CrCu_ETH_Model8_wo@fit$sigma,lag=1,type="Lj")
#CrCu_ETH8_LQ5_wo  <- Box.test(CrCu_ETH_Model8_wo@fit$residuals/CrCu_ETH_Model8_wo@fit$sigma,lag=5,type="Lj")
#CrCu_ETH8_LQ10_wo <- Box.test(CrCu_ETH_Model8_wo@fit$residuals/CrCu_ETH_Model8_wo@fit$sigma,lag=10,type="Lj")

#CrCu_ETH5_LQ_wo <- c(CrCu_ETH5_LQ1_wo$statistic,CrCu_ETH5_LQ5_wo$statistic,CrCu_ETH5_LQ10_wo$statistic)
#CrCu_ETH6_LQ_wo <- c(CrCu_ETH6_LQ1_wo$statistic,CrCu_ETH6_LQ5_wo$statistic,CrCu_ETH6_LQ10_wo$statistic)
#CrCu_ETH7_LQ_wo <- c(CrCu_ETH7_LQ1_wo$statistic,CrCu_ETH7_LQ5_wo$statistic,CrCu_ETH7_LQ10_wo$statistic)
#CrCu_ETH8_LQ_wo <- c(CrCu_ETH8_LQ1_wo$statistic,CrCu_ETH8_LQ5_wo$statistic,CrCu_ETH8_LQ10_wo$statistic)

#CrCu_ETH_res_stat_wo <-   cbind(CrCu_ETH5_LQ_wo,CrCu_ETH6_LQ_wo,CrCu_ETH7_LQ_wo,CrCu_ETH8_LQ_wo)
#rownames(CrCu_ETH_res_stat_wo) <- c("LQ1","LQ5","LQ10")
#colnames(CrCu_ETH_res_stat_wo) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

#### 1-day ahead Rolling Forecasts ####
## Full sample for roll i.e. last 350 points for forecast and rest for model
CrCu_ETH_Roll5_wo <-  ugarchroll(CrCu_ETH_Spec5_wo,CrCu_ETH_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETH_Roll6_wo <-  ugarchroll(CrCu_ETH_Spec6_wo,CrCu_ETH_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETH_Roll7_wo <-  ugarchroll(CrCu_ETH_Spec7_wo,CrCu_ETH_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETH_Roll8_wo <-  ugarchroll(CrCu_ETH_Spec8_wo,CrCu_ETH_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model
# Check in the last few lines of the code

#### Store the VaR for the out sample using rolling window forecasts

CrCu_ETH_VaR_SST_wo   <- CrCu_ETH_Roll5_wo@forecast$VaR
CrCu_ETH_VaR_SGED_wo  <- CrCu_ETH_Roll6_wo@forecast$VaR
CrCu_ETH_VaR_JSU_wo   <- CrCu_ETH_Roll7_wo@forecast$VaR
CrCu_ETH_VaR_Norm_wo  <- CrCu_ETH_Roll8_wo@forecast$VaR

#### ::::::::::::: Repeating for P E A R S O N Distribution  #####

### Check the summary stats of the standardized residuals
CrCu_ETH_Model8_res_wo <- CrCu_ETH_Model8_wo@fit$residuals/CrCu_ETH_Model8_wo@fit$sigma
CrCu_ETH_Descr8_wo <- basicStats(CrCu_ETH_Model8_res_wo)

### Fitting distributions on the standardized residuals 

library(PearsonDS)
library(gsl)
CrCu_ETH_Dist8_wo   <- pearsonFitML(CrCu_ETH_Model8_res_wo)

### Store the parameters for different distributions  

CrCu_ETH_P4_Param_wo    <- CrCu_ETH_Dist8_wo[2:5]

### Checking the fit using KS tests

CrCu_ETH_test1_wo <- ks.test(CrCu_ETH_Model8_res_wo,rpearson(length(CrCu_ETH_Model8_res_wo),params = CrCu_ETH_Dist8_wo))
#Null: True distribution is equal to the hypothesized distribution
#p-value is high, hence, we fail to reject H0.

### Calculate the quantiles

ETH_Exceed_Pearson_wo    <- array(data=0,dim=length(signific_levels))
#qpearson(signific_levels[2],CrCu_ETH_Dist8_wo)
for(j in 1:3) {
  Z_Pearson_ETH_wo   <- qpearson(signific_levels[j],CrCu_ETH_Dist8_wo)
  
  mu_ETH_wo   <- array(data=0,dim=length(Returns_CrCu_ETH)-350)
  VaR_ETH_wo   <- array(data=0,dim=length(Returns_CrCu_ETH)-351)
  
  for(i in 2:(length(Returns_CrCu_ETH)-350))
  {
    mu_ETH_wo[i] <- CrCu_ETH_Model8_wo@fit$coef[1]+CrCu_ETH_Model8_wo@fit$coef[2]*Returns_CrCu_ETH[i-1]+CrCu_ETH_Model8_wo@fit$coef[3]*CrCu_ETH_Model8_res_wo[i-1]
    VaR_ETH_wo[i-1] <- mu_ETH_wo[i]+CrCu_ETH_Model8_wo@fit$sigma[i]*Z_Pearson_ETH_wo
  }
  print(mu_ETH_wo)
  print(CrCu_ETH_Model8_wo@fit$sigma*Z_Pearson_ETH_wo)
  #ETH_Violations_wo[j]   <- VaRTest(signific_levels[j],Returns_CrCu_ETH[1:(length(VaR_ETH_wo))],VaR_ETH_wo)
  #print(VaRTest(signific_levels[j],Returns_CrCu_ETH[1:(length(VaR_ETH_wo))],VaR_ETH_wo))
  #ETH_Exceed_Pearson_wo[j] <- ETH_Violations_wo$actual.exceed
}

#### Out of sample VaR forecast  #####

Holdout_Returns_ETH_wo   <- Returns_CrCu_ETH[(length(Returns_CrCu_ETH)-349):length(Returns_CrCu_ETH)] 

Holdout_ETH_Pearson_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_ETH_Pearson_pvalues_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_ETH_Pearson_wo)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_ETH_Pearson_pvalues_wo)   <- c("UC","CC","DQ")
rownames(Holdout_ETH_Pearson_wo) <- c("1%","5%","10%")
rownames(Holdout_ETH_Pearson_pvalues_wo) <- c("1%","5%","10%")

## Store the volatility and mu forecast
signific_levels <- c(0.1,0.05,0.01)

ETH_Holdout_Mu_wo <- as.data.frame(CrCu_ETH_Roll8_wo)[,'Mu']
ETH_Holdout_Sigma_wo <- as.data.frame(CrCu_ETH_Roll8_wo)[,'Sigma']

MP_ETH_wo <- c(0 , 0, 0)
Holdout_ETH_Pearson_wo <- matrix(0, nrow = 3, ncol = 5)
Holdout_ETH_Pearson_pvalues_wo <- matrix(0, nrow = 3, ncol = 3)

for(j in 1:3) {
  
  Z_Pearson_ETH_wo   <- qpearson(signific_levels[j],CrCu_ETH_Dist8_wo)
  
  ETH_Holdout_VaR_wo   <- ETH_Holdout_Mu_wo + ETH_Holdout_Sigma_wo*Z_Pearson_ETH_wo
  ETH_Violations_wo   <- BacktestVaR(Holdout_Returns_ETH_wo[1:(length(Holdout_Returns_ETH_wo)-1)],ETH_Holdout_VaR_wo[-1],signific_levels[j])
  Holdout_ETH_Pearson_wo[j,]   <- c(ETH_Violations_wo$LRuc[1],ETH_Violations_wo$LRcc[1],ETH_Violations_wo$DQ$stat,ETH_Violations_wo$AE,ETH_Violations_wo$Loss$Loss)
  Holdout_ETH_Pearson_pvalues_wo[j,] <- c(ETH_Violations_wo$LRuc[2],ETH_Violations_wo$LRcc[2],ETH_Violations_wo$DQ$pvalue)
  MP_ETH_wo[j] <- MAPE(ETH_Holdout_Sigma_wo*Z_Pearson_ETH_wo, abs(CrCu_ETH_Roll7_wo@forecast$density$Realized))
  #print(ETH_Violations_wo$AE)
  #print(ETH_Violations)
  print(MP_ETH_wo)
}

#####################################################################################
### Using VaR Test individually ###
## VaRTest in R

CrCu_ETH_Norm_VaR_Test1_wo  <- VaRTest(0.01,CrCu_ETH_VaR_Norm_wo[,4],CrCu_ETH_VaR_Norm_wo[,1])
CrCu_ETH_Norm_VaR_Test5_wo  <- VaRTest(0.05,CrCu_ETH_VaR_Norm_wo[,4],CrCu_ETH_VaR_Norm_wo[,2])
CrCu_ETH_Norm_VaR_Test10_wo <- VaRTest(0.1,CrCu_ETH_VaR_Norm_wo[,4],CrCu_ETH_VaR_Norm_wo[,3])

CrCu_ETH_JSU_VaR_Test1_wo  <- VaRTest(0.01,CrCu_ETH_VaR_JSU_wo[,4],CrCu_ETH_VaR_JSU_wo[,1])
CrCu_ETH_JSU_VaR_Test5_wo  <- VaRTest(0.05,CrCu_ETH_VaR_JSU_wo[,4],CrCu_ETH_VaR_JSU_wo[,2])
CrCu_ETH_JSU_VaR_Test10_wo <- VaRTest(0.1,CrCu_ETH_VaR_JSU_wo[,4],CrCu_ETH_VaR_JSU_wo[,3])

CrCu_ETH_SGED_VaR_Test1_wo  <- VaRTest(0.01,CrCu_ETH_VaR_SGED_wo[,4],CrCu_ETH_VaR_SGED_wo[,1])
CrCu_ETH_SGED_VaR_Test5_wo  <- VaRTest(0.05,CrCu_ETH_VaR_SGED_wo[,4],CrCu_ETH_VaR_SGED_wo[,2])
CrCu_ETH_SGED_VaR_Test10_wo <- VaRTest(0.1,CrCu_ETH_VaR_SGED_wo[,4],CrCu_ETH_VaR_SGED_wo[,3])

CrCu_ETH_SST_VaR_Test1_wo  <- VaRTest(0.01,CrCu_ETH_VaR_SST_wo[,4],CrCu_ETH_VaR_SST_wo[,1])
CrCu_ETH_SST_VaR_Test5_wo  <- VaRTest(0.05,CrCu_ETH_VaR_SST_wo[,4],CrCu_ETH_VaR_SST_wo[,2])
CrCu_ETH_SST_VaR_Test10_wo <- VaRTest(0.1,CrCu_ETH_VaR_SST_wo[,4],CrCu_ETH_VaR_SST_wo[,3])

CrCu_ETH_VaR_Test_pvalues1_wo <- matrix(c(CrCu_ETH_SST_VaR_Test1_wo$uc.LRp,CrCu_ETH_SST_VaR_Test1_wo$cc.LRp,
                                          CrCu_ETH_SGED_VaR_Test1_wo$uc.LRp,CrCu_ETH_SGED_VaR_Test1_wo$cc.LRp,
                                          CrCu_ETH_JSU_VaR_Test1_wo$uc.LRp,CrCu_ETH_JSU_VaR_Test1_wo$cc.LRp,
                                          CrCu_ETH_Norm_VaR_Test1_wo$uc.LRp,CrCu_ETH_Norm_VaR_Test1_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_ETH_VaR_Test_pvalues5_wo <- matrix(c(CrCu_ETH_SST_VaR_Test5_wo$uc.LRp,CrCu_ETH_SST_VaR_Test5_wo$cc.LRp,
                                          CrCu_ETH_SGED_VaR_Test5_wo$uc.LRp,CrCu_ETH_SGED_VaR_Test5_wo$cc.LRp,
                                          CrCu_ETH_JSU_VaR_Test5_wo$uc.LRp,CrCu_ETH_JSU_VaR_Test5_wo$cc.LRp,
                                          CrCu_ETH_Norm_VaR_Test5_wo$uc.LRp,CrCu_ETH_Norm_VaR_Test5_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_ETH_VaR_Test_pvalues10_wo <- matrix(c(CrCu_ETH_SST_VaR_Test10_wo$uc.LRp,CrCu_ETH_SST_VaR_Test10_wo$cc.LRp,
                                           CrCu_ETH_SGED_VaR_Test10_wo$uc.LRp,CrCu_ETH_SGED_VaR_Test10_wo$cc.LRp,
                                           CrCu_ETH_JSU_VaR_Test10_wo$uc.LRp,CrCu_ETH_JSU_VaR_Test10_wo$cc.LRp,
                                           CrCu_ETH_Norm_VaR_Test10_wo$uc.LRp,CrCu_ETH_Norm_VaR_Test10_wo$cc.LRp),ncol=2,byrow = TRUE)
###################:::::::::::::::::::::::::::::::::::::::::############################
#### ********** L I T E C O I N ********** ####
Crypto1$LTC = as.numeric(as.character(Crypto1$LTC))
CrCu_LTC <- data.frame(Crypto_Date, Crypto$Bitcoin) # Bitcoin Data with Date
View(CrCu_LTC)

####### :::::::::: Returns Series

Returns_CrCu_LTC <- log(Crypto1$LTC[-1]/Crypto1$LTC[1:(nrow(Crypto1)-1)]) # Returns series
Des_Returns_CrCu_LTC <- basicStats(Returns_CrCu_LTC) # Descriptive statistics
View(Returns_CrCu_LTC)

#### Convert into xts objects ####

CrCu_LTC_xts   <- xts(Returns_CrCu_LTC,order.by = Crypto_Date[-1])
names(CrCu_LTC_xts)   <- "Returns"
View(CrCu_LTC_xts)

## :::::::::::: check presence of ARCH effect in Full Sample
library(FinTS)
ArchTest(CrCu_LTC_xts, lags = 1)
ArchTest(CrCu_LTC_xts, lags = 5)
ArchTest(CrCu_LTC_xts, lags = 10)

# H0: Series has no ARCH effects
# Null hypothesis is rejected, hence proves presence of ARCH effects

### Garch Models
# :::::::::Without external regressors in Full Sample GARCH

#SST #SGED #JSU #Norm
CrCu_LTC_Spec1_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_LTC_Spec2_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_LTC_Spec3_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_LTC_Spec4_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_LTC_Model1_wo <- ugarchfit(CrCu_LTC_Spec1_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model2_wo <- ugarchfit(CrCu_LTC_Spec2_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model3_wo <- ugarchfit(CrCu_LTC_Spec3_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model4_wo <- ugarchfit(CrCu_LTC_Spec4_wo,CrCu_LTC_xts,out.sample = 350)

#### egarch models

CrCu_LTC_Spec5_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_LTC_Spec6_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_LTC_Spec7_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_LTC_Spec8_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_LTC_Model5_wo <- ugarchfit(CrCu_LTC_Spec5_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model6_wo <- ugarchfit(CrCu_LTC_Spec6_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model7_wo <- ugarchfit(CrCu_LTC_Spec7_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model8_wo <- ugarchfit(CrCu_LTC_Spec8_wo,CrCu_LTC_xts,out.sample = 350)

### gjrgarch models

CrCu_LTC_Spec9_wo  <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_LTC_Spec10_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_LTC_Spec11_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_LTC_Spec12_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_LTC_Model9_wo  <- ugarchfit(CrCu_LTC_Spec9_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model10_wo <- ugarchfit(CrCu_LTC_Spec10_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model11_wo <- ugarchfit(CrCu_LTC_Spec11_wo,CrCu_LTC_xts,out.sample = 350)
CrCu_LTC_Model12_wo <- ugarchfit(CrCu_LTC_Spec12_wo,CrCu_LTC_xts,out.sample = 350)

## persistence

persis_LTC_wo <- matrix(data=0, nrow = 12, ncol = 1)
persis_LTC_wo[1,1] <- round(persistence(CrCu_LTC_Model1_wo),4)
persis_LTC_wo[2,1] <- round(persistence(CrCu_LTC_Model2_wo),4)
persis_LTC_wo[3,1] <- round(persistence(CrCu_LTC_Model3_wo),4)
persis_LTC_wo[4,1] <- round(persistence(CrCu_LTC_Model4_wo),4)
persis_LTC_wo[5,1] <- round(persistence(CrCu_LTC_Model5_wo),4)
persis_LTC_wo[6,1] <- round(persistence(CrCu_LTC_Model6_wo),4)
persis_LTC_wo[7,1] <- round(persistence(CrCu_LTC_Model7_wo),4)
persis_LTC_wo[8,1] <- round(persistence(CrCu_LTC_Model8_wo),4)
persis_LTC_wo[9,1] <- round(persistence(CrCu_LTC_Model9_wo),4)
persis_LTC_wo[10,1] <- round(persistence(CrCu_LTC_Model10_wo),4)
persis_LTC_wo[11,1] <- round(persistence(CrCu_LTC_Model11_wo),4)
persis_LTC_wo[12,1] <- round(persistence(CrCu_LTC_Model12_wo),4)

persistance_LTC_wo <- matrix(data=persis_LTC_wo, nrow = 12, ncol = 1)
colnames(persistance_LTC_wo) <- c("Persistence_LTC")
rownames(persistance_LTC_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(data.frame(persistance_LTC_wo))
#xtable(persistance_LTC, digits = 4)

### Store the AIC values  ####

AIC_LTC_wo  <- matrix(data=0,nrow=12,ncol=1)
colnames(AIC_LTC_wo) <- c("CrCu_LTC")
rownames(AIC_LTC_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
AIC_LTC_wo[1,1]  <-  infocriteria(CrCu_LTC_Model1_wo)[1]
AIC_LTC_wo[2,1]  <-  infocriteria(CrCu_LTC_Model2_wo)[1]
AIC_LTC_wo[3,1]  <-  infocriteria(CrCu_LTC_Model3_wo)[1]
AIC_LTC_wo[4,1]  <-  infocriteria(CrCu_LTC_Model4_wo)[1]
AIC_LTC_wo[5,1]  <-  infocriteria(CrCu_LTC_Model5_wo)[1]
AIC_LTC_wo[6,1]  <-  infocriteria(CrCu_LTC_Model6_wo)[1]
AIC_LTC_wo[7,1]  <-  infocriteria(CrCu_LTC_Model7_wo)[1]
AIC_LTC_wo[8,1]  <-  infocriteria(CrCu_LTC_Model8_wo)[1]
AIC_LTC_wo[9,1]  <-  infocriteria(CrCu_LTC_Model9_wo)[1]
AIC_LTC_wo[10,1] <- infocriteria(CrCu_LTC_Model10_wo)[1]
AIC_LTC_wo[11,1] <- infocriteria(CrCu_LTC_Model11_wo)[1]
AIC_LTC_wo[12,1] <- infocriteria(CrCu_LTC_Model12_wo)[1]
View(AIC_LTC_wo)
xtable(AIC_LTC_wo, caption = "Information Criteria for Bitcoin GARCH models - Full Sample", digits = 4)

## Conclusion: eGarch-SST have the minimum AIC and eGarch-JSU is the next best #####
## Model 5 and the Model 7

## coefficients

coef_LTC_wo_1 <- round(coef(CrCu_LTC_Model1_wo),4)
coef_LTC_wo_2 <- round(coef(CrCu_LTC_Model2_wo),4)
coef_LTC_wo_3 <- round(coef(CrCu_LTC_Model3_wo),4)
coef_LTC_wo_4 <- round(coef(CrCu_LTC_Model4_wo),4)
coef_LTC_wo_5 <- round(coef(CrCu_LTC_Model5_wo),4)
coef_LTC_wo_6 <- round(coef(CrCu_LTC_Model6_wo),4)
coef_LTC_wo_7 <- round(coef(CrCu_LTC_Model7_wo),4)
coef_LTC_wo_8 <- round(coef(CrCu_LTC_Model8_wo),4)
coef_LTC_wo_9 <- round(coef(CrCu_LTC_Model9_wo),4)
coef_LTC_wo_10 <- round(coef(CrCu_LTC_Model10_wo),4)
coef_LTC_wo_11 <- round(coef(CrCu_LTC_Model11_wo),4)
coef_LTC_wo_12 <- round(coef(CrCu_LTC_Model12_wo),4)

garch_coef_LTC_wo <- bind_rows(coef_LTC_wo_1, coef_LTC_wo_2, coef_LTC_wo_3, coef_LTC_wo_4,coef_LTC_wo_5, coef_LTC_wo_6, coef_LTC_wo_7, coef_LTC_wo_8, coef_LTC_wo_9, coef_LTC_wo_10, coef_LTC_wo_11, coef_LTC_wo_12)

garch_coef_LTC_wo1 <- as.data.frame(garch_coef_LTC_wo)
rownames(garch_coef_LTC_wo1) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(garch_coef_LTC_wo1)
xtable(garch_coef_LTC_wo1, caption = "Coefficients of Bitcoin GARCH models - Full Sample", digits = 4)

### Ljung-box test statistic
# Check standardized shocks

#CrCu_LTC5_LQ1_wo  <- Box.test(CrCu_LTC_Model5_wo@fit$residuals/CrCu_LTC_Model5_wo@fit$sigma,lag=1,type="Lj")
#CrCu_LTC5_LQ5_wo  <- Box.test(CrCu_LTC_Model5_wo@fit$residuals/CrCu_LTC_Model5_wo@fit$sigma,lag=5,type="Lj")
#CrCu_LTC5_LQ10_wo <- Box.test(CrCu_LTC_Model5_wo@fit$residuals/CrCu_LTC_Model5_wo@fit$sigma,lag=10,type="Lj")

#CrCu_LTC6_LQ1_wo  <- Box.test(CrCu_LTC_Model6_wo@fit$residuals/CrCu_LTC_Model6_wo@fit$sigma,lag=1,type="Lj")
#CrCu_LTC6_LQ5_wo  <- Box.test(CrCu_LTC_Model6_wo@fit$residuals/CrCu_LTC_Model6_wo@fit$sigma,lag=5,type="Lj")
#CrCu_LTC6_LQ10_wo <- Box.test(CrCu_LTC_Model6_wo@fit$residuals/CrCu_LTC_Model6_wo@fit$sigma,lag=10,type="Lj")

#CrCu_LTC7_LQ1_wo  <- Box.test(CrCu_LTC_Model7_wo@fit$residuals/CrCu_LTC_Model7_wo@fit$sigma,lag=1,type="Lj")
#CrCu_LTC7_LQ5_wo  <- Box.test(CrCu_LTC_Model7_wo@fit$residuals/CrCu_LTC_Model7_wo@fit$sigma,lag=5,type="Lj")
#CrCu_LTC7_LQ10_wo <- Box.test(CrCu_LTC_Model7_wo@fit$residuals/CrCu_LTC_Model7_wo@fit$sigma,lag=10,type="Lj")

#CrCu_LTC8_LQ1_wo  <- Box.test(CrCu_LTC_Model8_wo@fit$residuals/CrCu_LTC_Model8_wo@fit$sigma,lag=1,type="Lj")
#CrCu_LTC8_LQ5_wo  <- Box.test(CrCu_LTC_Model8_wo@fit$residuals/CrCu_LTC_Model8_wo@fit$sigma,lag=5,type="Lj")
#CrCu_LTC8_LQ10_wo <- Box.test(CrCu_LTC_Model8_wo@fit$residuals/CrCu_LTC_Model8_wo@fit$sigma,lag=10,type="Lj")

#CrCu_LTC5_LQ_wo <- c(CrCu_LTC5_LQ1_wo$statistic,CrCu_LTC5_LQ5_wo$statistic,CrCu_LTC5_LQ10_wo$statistic)
#CrCu_LTC6_LQ_wo <- c(CrCu_LTC6_LQ1_wo$statistic,CrCu_LTC6_LQ5_wo$statistic,CrCu_LTC6_LQ10_wo$statistic)
#CrCu_LTC7_LQ_wo <- c(CrCu_LTC7_LQ1_wo$statistic,CrCu_LTC7_LQ5_wo$statistic,CrCu_LTC7_LQ10_wo$statistic)
#CrCu_LTC8_LQ_wo <- c(CrCu_LTC8_LQ1_wo$statistic,CrCu_LTC8_LQ5_wo$statistic,CrCu_LTC8_LQ10_wo$statistic)

#CrCu_LTC_res_stat_wo <-   cbind(CrCu_LTC5_LQ_wo,CrCu_LTC6_LQ_wo,CrCu_LTC7_LQ_wo,CrCu_LTC8_LQ_wo)
#rownames(CrCu_LTC_res_stat_wo) <- c("LQ1","LQ5","LQ10")
#colnames(CrCu_LTC_res_stat_wo) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

#### 1-day ahead Rolling Forecasts ####
## Full sample for roll i.e. last 350 points for forecast and rest for model
CrCu_LTC_Roll5_wo <-  ugarchroll(CrCu_LTC_Spec5_wo,CrCu_LTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_LTC_Roll6_wo <-  ugarchroll(CrCu_LTC_Spec6_wo,CrCu_LTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_LTC_Roll7_wo <-  ugarchroll(CrCu_LTC_Spec7_wo,CrCu_LTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_LTC_Roll8_wo <-  ugarchroll(CrCu_LTC_Spec8_wo,CrCu_LTC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model
# Check in the last few lines of the code

#### Store the VaR for the out sample using rolling window forecasts

CrCu_LTC_VaR_SST_wo   <- CrCu_LTC_Roll5_wo@forecast$VaR
CrCu_LTC_VaR_SGED_wo  <- CrCu_LTC_Roll6_wo@forecast$VaR
CrCu_LTC_VaR_JSU_wo   <- CrCu_LTC_Roll7_wo@forecast$VaR
CrCu_LTC_VaR_Norm_wo  <- CrCu_LTC_Roll8_wo@forecast$VaR

#### ::::::::::::: Repeating for P E A R S O N Distribution  #####

### Check the summary stats of the standardized residuals
CrCu_LTC_Model8_res_wo <- CrCu_LTC_Model8_wo@fit$residuals/CrCu_LTC_Model8_wo@fit$sigma
CrCu_LTC_Descr8_wo <- basicStats(CrCu_LTC_Model8_res_wo)

### Fitting distributions on the standardized residuals 

library(PearsonDS)
library(gsl)
CrCu_LTC_Dist8_wo   <- pearsonFitML(CrCu_LTC_Model8_res_wo)

### Store the parameters for different distributions  

CrCu_LTC_P4_Param_wo    <- CrCu_LTC_Dist8_wo[2:5]

### Checking the fit using KS tests

CrCu_LTC_test1_wo <- ks.test(CrCu_LTC_Model8_res_wo,rpearson(length(CrCu_LTC_Model8_res_wo),params = CrCu_LTC_Dist8_wo))
#Null: True distribution is equal to the hypothesized distribution
#p-value is high, hence, we fail to reject H0.

### Calculate the quantiles

LTC_Exceed_Pearson_wo    <- array(data=0,dim=length(signific_levels))
#qpearson(signific_levels[2],CrCu_LTC_Dist8_wo)
for(j in 1:3) {
  Z_Pearson_LTC_wo   <- qpearson(signific_levels[j],CrCu_LTC_Dist8_wo)
  
  mu_LTC_wo   <- array(data=0,dim=length(Returns_CrCu_LTC)-350)
  VaR_LTC_wo   <- array(data=0,dim=length(Returns_CrCu_LTC)-351)
  
  for(i in 2:(length(Returns_CrCu_LTC)-350))
  {
    mu_LTC_wo[i] <- CrCu_LTC_Model8_wo@fit$coef[1]+CrCu_LTC_Model8_wo@fit$coef[2]*Returns_CrCu_LTC[i-1]+CrCu_LTC_Model8_wo@fit$coef[3]*CrCu_LTC_Model8_res_wo[i-1]
    VaR_LTC_wo[i-1] <- mu_LTC_wo[i]+CrCu_LTC_Model8_wo@fit$sigma[i]*Z_Pearson_LTC_wo
  }
  print(mu_LTC_wo)
  print(CrCu_LTC_Model8_wo@fit$sigma*Z_Pearson_LTC_wo)
  #LTC_Violations_wo[j]   <- VaRTest(signific_levels[j],Returns_CrCu_LTC[1:(length(VaR_LTC_wo))],VaR_LTC_wo)
  #print(VaRTest(signific_levels[j],Returns_CrCu_LTC[1:(length(VaR_LTC_wo))],VaR_LTC_wo))
  #LTC_Exceed_Pearson_wo[j] <- LTC_Violations_wo$actual.exceed
}

#### Out of sample VaR forecast  #####

Holdout_Returns_LTC_wo   <- Returns_CrCu_LTC[(length(Returns_CrCu_LTC)-349):length(Returns_CrCu_LTC)] 

Holdout_LTC_Pearson_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_LTC_Pearson_pvalues_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_LTC_Pearson_wo)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_LTC_Pearson_pvalues_wo)   <- c("UC","CC","DQ")
rownames(Holdout_LTC_Pearson_wo) <- c("1%","5%","10%")
rownames(Holdout_LTC_Pearson_pvalues_wo) <- c("1%","5%","10%")

## Store the volatility and mu forecast
signific_levels <- c(0.1,0.05,0.01)

LTC_Holdout_Mu_wo <- as.data.frame(CrCu_LTC_Roll8_wo)[,'Mu']
LTC_Holdout_Sigma_wo <- as.data.frame(CrCu_LTC_Roll8_wo)[,'Sigma']

MP_LTC_wo <- c(0 , 0, 0)
Holdout_LTC_Pearson_wo <- matrix(0, nrow = 3, ncol = 5)
Holdout_LTC_Pearson_pvalues_wo <- matrix(0, nrow = 3, ncol = 3)

for(j in 1:3) {
  
  Z_Pearson_LTC_wo   <- qpearson(signific_levels[j],CrCu_LTC_Dist8_wo)
  
  LTC_Holdout_VaR_wo   <- LTC_Holdout_Mu_wo + LTC_Holdout_Sigma_wo*Z_Pearson_LTC_wo
  LTC_Violations_wo   <- BacktestVaR(Holdout_Returns_LTC_wo[1:(length(Holdout_Returns_LTC_wo)-1)],LTC_Holdout_VaR_wo[-1],signific_levels[j])
  Holdout_LTC_Pearson_wo[j,]   <- c(LTC_Violations_wo$LRuc[1],LTC_Violations_wo$LRcc[1],LTC_Violations_wo$DQ$stat,LTC_Violations_wo$AE,LTC_Violations_wo$Loss$Loss)
  Holdout_LTC_Pearson_pvalues_wo[j,] <- c(LTC_Violations_wo$LRuc[2],LTC_Violations_wo$LRcc[2],LTC_Violations_wo$DQ$pvalue)
  MP_LTC_wo[j] <- MAPE(LTC_Holdout_Sigma_wo*Z_Pearson_LTC_wo, abs(CrCu_LTC_Roll7_wo@forecast$density$Realized))
  #print(LTC_Violations_wo$AE)
  #print(LTC_Violations)
  print(MP_LTC_wo)
}

#####################################################################################
### Using VaR Test individually ###
## VaRTest in R

CrCu_LTC_Norm_VaR_Test1_wo  <- VaRTest(0.01,CrCu_LTC_VaR_Norm_wo[,4],CrCu_LTC_VaR_Norm_wo[,1])
CrCu_LTC_Norm_VaR_Test5_wo  <- VaRTest(0.05,CrCu_LTC_VaR_Norm_wo[,4],CrCu_LTC_VaR_Norm_wo[,2])
CrCu_LTC_Norm_VaR_Test10_wo <- VaRTest(0.1,CrCu_LTC_VaR_Norm_wo[,4],CrCu_LTC_VaR_Norm_wo[,3])

CrCu_LTC_JSU_VaR_Test1_wo  <- VaRTest(0.01,CrCu_LTC_VaR_JSU_wo[,4],CrCu_LTC_VaR_JSU_wo[,1])
CrCu_LTC_JSU_VaR_Test5_wo  <- VaRTest(0.05,CrCu_LTC_VaR_JSU_wo[,4],CrCu_LTC_VaR_JSU_wo[,2])
CrCu_LTC_JSU_VaR_Test10_wo <- VaRTest(0.1,CrCu_LTC_VaR_JSU_wo[,4],CrCu_LTC_VaR_JSU_wo[,3])

CrCu_LTC_SGED_VaR_Test1_wo  <- VaRTest(0.01,CrCu_LTC_VaR_SGED_wo[,4],CrCu_LTC_VaR_SGED_wo[,1])
CrCu_LTC_SGED_VaR_Test5_wo  <- VaRTest(0.05,CrCu_LTC_VaR_SGED_wo[,4],CrCu_LTC_VaR_SGED_wo[,2])
CrCu_LTC_SGED_VaR_Test10_wo <- VaRTest(0.1,CrCu_LTC_VaR_SGED_wo[,4],CrCu_LTC_VaR_SGED_wo[,3])

CrCu_LTC_SST_VaR_Test1_wo  <- VaRTest(0.01,CrCu_LTC_VaR_SST_wo[,4],CrCu_LTC_VaR_SST_wo[,1])
CrCu_LTC_SST_VaR_Test5_wo  <- VaRTest(0.05,CrCu_LTC_VaR_SST_wo[,4],CrCu_LTC_VaR_SST_wo[,2])
CrCu_LTC_SST_VaR_Test10_wo <- VaRTest(0.1,CrCu_LTC_VaR_SST_wo[,4],CrCu_LTC_VaR_SST_wo[,3])

CrCu_LTC_VaR_Test_pvalues1_wo <- matrix(c(CrCu_LTC_SST_VaR_Test1_wo$uc.LRp,CrCu_LTC_SST_VaR_Test1_wo$cc.LRp,
                                          CrCu_LTC_SGED_VaR_Test1_wo$uc.LRp,CrCu_LTC_SGED_VaR_Test1_wo$cc.LRp,
                                          CrCu_LTC_JSU_VaR_Test1_wo$uc.LRp,CrCu_LTC_JSU_VaR_Test1_wo$cc.LRp,
                                          CrCu_LTC_Norm_VaR_Test1_wo$uc.LRp,CrCu_LTC_Norm_VaR_Test1_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_LTC_VaR_Test_pvalues5_wo <- matrix(c(CrCu_LTC_SST_VaR_Test5_wo$uc.LRp,CrCu_LTC_SST_VaR_Test5_wo$cc.LRp,
                                          CrCu_LTC_SGED_VaR_Test5_wo$uc.LRp,CrCu_LTC_SGED_VaR_Test5_wo$cc.LRp,
                                          CrCu_LTC_JSU_VaR_Test5_wo$uc.LRp,CrCu_LTC_JSU_VaR_Test5_wo$cc.LRp,
                                          CrCu_LTC_Norm_VaR_Test5_wo$uc.LRp,CrCu_LTC_Norm_VaR_Test5_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_LTC_VaR_Test_pvalues10_wo <- matrix(c(CrCu_LTC_SST_VaR_Test10_wo$uc.LRp,CrCu_LTC_SST_VaR_Test10_wo$cc.LRp,
                                           CrCu_LTC_SGED_VaR_Test10_wo$uc.LRp,CrCu_LTC_SGED_VaR_Test10_wo$cc.LRp,
                                           CrCu_LTC_JSU_VaR_Test10_wo$uc.LRp,CrCu_LTC_JSU_VaR_Test10_wo$cc.LRp,
                                           CrCu_LTC_Norm_VaR_Test10_wo$uc.LRp,CrCu_LTC_Norm_VaR_Test10_wo$cc.LRp),ncol=2,byrow = TRUE)
###################:::::::::::::::::::::::::::::::::::::::::############################

#### ********** M O N E R O ********** ####
Crypto1$XMR = as.numeric(as.character(Crypto1$XMR))
CrCu_XMR <- data.frame(Crypto_Date, Crypto$Bitcoin) # Bitcoin Data with Date
View(CrCu_XMR)

####### :::::::::: Returns Series

Returns_CrCu_XMR <- log(Crypto1$XMR[-1]/Crypto1$XMR[1:(nrow(Crypto1)-1)]) # Returns series
Des_Returns_CrCu_XMR <- basicStats(Returns_CrCu_XMR) # Descriptive statistics
View(Returns_CrCu_XMR)

#### Convert into xts objects ####

CrCu_XMR_xts   <- xts(Returns_CrCu_XMR,order.by = Crypto_Date[-1])
names(CrCu_XMR_xts)   <- "Returns"
View(CrCu_XMR_xts)

## :::::::::::: check presence of ARCH effect in Full Sample
library(FinTS)
ArchTest(CrCu_XMR_xts, lags = 1)
ArchTest(CrCu_XMR_xts, lags = 5)
ArchTest(CrCu_XMR_xts, lags = 10)

# H0: Series has no ARCH effects
# Null hypothesis is rejected, hence proves presence of ARCH effects

### Garch Models
# :::::::::Without external regressors in Full Sample GARCH

#SST #SGED #JSU #Norm
CrCu_XMR_Spec1_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_XMR_Spec2_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_XMR_Spec3_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_XMR_Spec4_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_XMR_Model1_wo <- ugarchfit(CrCu_XMR_Spec1_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model2_wo <- ugarchfit(CrCu_XMR_Spec2_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model3_wo <- ugarchfit(CrCu_XMR_Spec3_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model4_wo <- ugarchfit(CrCu_XMR_Spec4_wo,CrCu_XMR_xts,out.sample = 350)

#### egarch models

CrCu_XMR_Spec5_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_XMR_Spec6_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_XMR_Spec7_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_XMR_Spec8_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_XMR_Model5_wo <- ugarchfit(CrCu_XMR_Spec5_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model6_wo <- ugarchfit(CrCu_XMR_Spec6_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model7_wo <- ugarchfit(CrCu_XMR_Spec7_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model8_wo <- ugarchfit(CrCu_XMR_Spec8_wo,CrCu_XMR_xts,out.sample = 350)

### gjrgarch models

CrCu_XMR_Spec9_wo  <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_XMR_Spec10_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_XMR_Spec11_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_XMR_Spec12_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_XMR_Model9_wo  <- ugarchfit(CrCu_XMR_Spec9_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model10_wo <- ugarchfit(CrCu_XMR_Spec10_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model11_wo <- ugarchfit(CrCu_XMR_Spec11_wo,CrCu_XMR_xts,out.sample = 350)
CrCu_XMR_Model12_wo <- ugarchfit(CrCu_XMR_Spec12_wo,CrCu_XMR_xts,out.sample = 350)

## persistence

persis_XMR_wo <- matrix(data=0, nrow = 12, ncol = 1)
persis_XMR_wo[1,1] <- round(persistence(CrCu_XMR_Model1_wo),4)
persis_XMR_wo[2,1] <- round(persistence(CrCu_XMR_Model2_wo),4)
persis_XMR_wo[3,1] <- round(persistence(CrCu_XMR_Model3_wo),4)
persis_XMR_wo[4,1] <- round(persistence(CrCu_XMR_Model4_wo),4)
persis_XMR_wo[5,1] <- round(persistence(CrCu_XMR_Model5_wo),4)
persis_XMR_wo[6,1] <- round(persistence(CrCu_XMR_Model6_wo),4)
persis_XMR_wo[7,1] <- round(persistence(CrCu_XMR_Model7_wo),4)
persis_XMR_wo[8,1] <- round(persistence(CrCu_XMR_Model8_wo),4)
persis_XMR_wo[9,1] <- round(persistence(CrCu_XMR_Model9_wo),4)
persis_XMR_wo[10,1] <- round(persistence(CrCu_XMR_Model10_wo),4)
persis_XMR_wo[11,1] <- round(persistence(CrCu_XMR_Model11_wo),4)
persis_XMR_wo[12,1] <- round(persistence(CrCu_XMR_Model12_wo),4)

persistance_XMR_wo <- matrix(data=persis_XMR_wo, nrow = 12, ncol = 1)
colnames(persistance_XMR_wo) <- c("Persistence_XMR")
rownames(persistance_XMR_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(data.frame(persistance_XMR_wo))
#xtable(persistance_XMR, digits = 4)

### Store the AIC values  ####

AIC_XMR_wo  <- matrix(data=0,nrow=12,ncol=1)
colnames(AIC_XMR_wo) <- c("CrCu_XMR")
rownames(AIC_XMR_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
AIC_XMR_wo[1,1]  <-  infocriteria(CrCu_XMR_Model1_wo)[1]
AIC_XMR_wo[2,1]  <-  infocriteria(CrCu_XMR_Model2_wo)[1]
AIC_XMR_wo[3,1]  <-  infocriteria(CrCu_XMR_Model3_wo)[1]
AIC_XMR_wo[4,1]  <-  infocriteria(CrCu_XMR_Model4_wo)[1]
AIC_XMR_wo[5,1]  <-  infocriteria(CrCu_XMR_Model5_wo)[1]
AIC_XMR_wo[6,1]  <-  infocriteria(CrCu_XMR_Model6_wo)[1]
AIC_XMR_wo[7,1]  <-  infocriteria(CrCu_XMR_Model7_wo)[1]
AIC_XMR_wo[8,1]  <-  infocriteria(CrCu_XMR_Model8_wo)[1]
AIC_XMR_wo[9,1]  <-  infocriteria(CrCu_XMR_Model9_wo)[1]
AIC_XMR_wo[10,1] <- infocriteria(CrCu_XMR_Model10_wo)[1]
AIC_XMR_wo[11,1] <- infocriteria(CrCu_XMR_Model11_wo)[1]
AIC_XMR_wo[12,1] <- infocriteria(CrCu_XMR_Model12_wo)[1]
View(AIC_XMR_wo)
xtable(AIC_XMR_wo, caption = "Information Criteria for Bitcoin GARCH models - Full Sample", digits = 4)

## Conclusion: eGarch-JSU have the minimum AIC and eGarch-SST is the next best #####
## Model 7 and the Model 5

## coefficients

coef_XMR_wo_1 <- round(coef(CrCu_XMR_Model1_wo),4)
coef_XMR_wo_2 <- round(coef(CrCu_XMR_Model2_wo),4)
coef_XMR_wo_3 <- round(coef(CrCu_XMR_Model3_wo),4)
coef_XMR_wo_4 <- round(coef(CrCu_XMR_Model4_wo),4)
coef_XMR_wo_5 <- round(coef(CrCu_XMR_Model5_wo),4)
coef_XMR_wo_6 <- round(coef(CrCu_XMR_Model6_wo),4)
coef_XMR_wo_7 <- round(coef(CrCu_XMR_Model7_wo),4)
coef_XMR_wo_8 <- round(coef(CrCu_XMR_Model8_wo),4)
coef_XMR_wo_9 <- round(coef(CrCu_XMR_Model9_wo),4)
coef_XMR_wo_10 <- round(coef(CrCu_XMR_Model10_wo),4)
coef_XMR_wo_11 <- round(coef(CrCu_XMR_Model11_wo),4)
coef_XMR_wo_12 <- round(coef(CrCu_XMR_Model12_wo),4)

garch_coef_XMR_wo <- bind_rows(coef_XMR_wo_1, coef_XMR_wo_2, coef_XMR_wo_3, coef_XMR_wo_4,coef_XMR_wo_5, coef_XMR_wo_6, coef_XMR_wo_7, coef_XMR_wo_8, coef_XMR_wo_9, coef_XMR_wo_10, coef_XMR_wo_11, coef_XMR_wo_12)

garch_coef_XMR_wo1 <- as.data.frame(garch_coef_XMR_wo)
rownames(garch_coef_XMR_wo1) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(garch_coef_XMR_wo1)
xtable(garch_coef_XMR_wo1, caption = "Coefficients of Bitcoin GARCH models - Full Sample", digits = 4)

### Ljung-box test statistic
# Check standardized shocks

#CrCu_XMR5_LQ1_wo  <- Box.test(CrCu_XMR_Model5_wo@fit$residuals/CrCu_XMR_Model5_wo@fit$sigma,lag=1,type="Lj")
#CrCu_XMR5_LQ5_wo  <- Box.test(CrCu_XMR_Model5_wo@fit$residuals/CrCu_XMR_Model5_wo@fit$sigma,lag=5,type="Lj")
#CrCu_XMR5_LQ10_wo <- Box.test(CrCu_XMR_Model5_wo@fit$residuals/CrCu_XMR_Model5_wo@fit$sigma,lag=10,type="Lj")

#CrCu_XMR6_LQ1_wo  <- Box.test(CrCu_XMR_Model6_wo@fit$residuals/CrCu_XMR_Model6_wo@fit$sigma,lag=1,type="Lj")
#CrCu_XMR6_LQ5_wo  <- Box.test(CrCu_XMR_Model6_wo@fit$residuals/CrCu_XMR_Model6_wo@fit$sigma,lag=5,type="Lj")
#CrCu_XMR6_LQ10_wo <- Box.test(CrCu_XMR_Model6_wo@fit$residuals/CrCu_XMR_Model6_wo@fit$sigma,lag=10,type="Lj")

#CrCu_XMR7_LQ1_wo  <- Box.test(CrCu_XMR_Model7_wo@fit$residuals/CrCu_XMR_Model7_wo@fit$sigma,lag=1,type="Lj")
#CrCu_XMR7_LQ5_wo  <- Box.test(CrCu_XMR_Model7_wo@fit$residuals/CrCu_XMR_Model7_wo@fit$sigma,lag=5,type="Lj")
#CrCu_XMR7_LQ10_wo <- Box.test(CrCu_XMR_Model7_wo@fit$residuals/CrCu_XMR_Model7_wo@fit$sigma,lag=10,type="Lj")

#CrCu_XMR8_LQ1_wo  <- Box.test(CrCu_XMR_Model8_wo@fit$residuals/CrCu_XMR_Model8_wo@fit$sigma,lag=1,type="Lj")
#CrCu_XMR8_LQ5_wo  <- Box.test(CrCu_XMR_Model8_wo@fit$residuals/CrCu_XMR_Model8_wo@fit$sigma,lag=5,type="Lj")
#CrCu_XMR8_LQ10_wo <- Box.test(CrCu_XMR_Model8_wo@fit$residuals/CrCu_XMR_Model8_wo@fit$sigma,lag=10,type="Lj")

#CrCu_XMR5_LQ_wo <- c(CrCu_XMR5_LQ1_wo$statistic,CrCu_XMR5_LQ5_wo$statistic,CrCu_XMR5_LQ10_wo$statistic)
#CrCu_XMR6_LQ_wo <- c(CrCu_XMR6_LQ1_wo$statistic,CrCu_XMR6_LQ5_wo$statistic,CrCu_XMR6_LQ10_wo$statistic)
#CrCu_XMR7_LQ_wo <- c(CrCu_XMR7_LQ1_wo$statistic,CrCu_XMR7_LQ5_wo$statistic,CrCu_XMR7_LQ10_wo$statistic)
#CrCu_XMR8_LQ_wo <- c(CrCu_XMR8_LQ1_wo$statistic,CrCu_XMR8_LQ5_wo$statistic,CrCu_XMR8_LQ10_wo$statistic)

#CrCu_XMR_res_stat_wo <-   cbind(CrCu_XMR5_LQ_wo,CrCu_XMR6_LQ_wo,CrCu_XMR7_LQ_wo,CrCu_XMR8_LQ_wo)
#rownames(CrCu_XMR_res_stat_wo) <- c("LQ1","LQ5","LQ10")
#colnames(CrCu_XMR_res_stat_wo) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

#### 1-day ahead Rolling Forecasts ####
## Full sample for roll i.e. last 350 points for forecast and rest for model
CrCu_XMR_Roll5_wo <-  ugarchroll(CrCu_XMR_Spec5_wo,CrCu_XMR_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll6_wo <-  ugarchroll(CrCu_XMR_Spec6_wo,CrCu_XMR_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll7_wo <-  ugarchroll(CrCu_XMR_Spec7_wo,CrCu_XMR_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_XMR_Roll8_wo <-  ugarchroll(CrCu_XMR_Spec8_wo,CrCu_XMR_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model
# Check in the last few lines of the code

#### Store the VaR for the out sample using rolling window forecasts

CrCu_XMR_VaR_SST_wo   <- CrCu_XMR_Roll5_wo@forecast$VaR
CrCu_XMR_VaR_SGED_wo  <- CrCu_XMR_Roll6_wo@forecast$VaR
CrCu_XMR_VaR_JSU_wo   <- CrCu_XMR_Roll7_wo@forecast$VaR
CrCu_XMR_VaR_Norm_wo  <- CrCu_XMR_Roll8_wo@forecast$VaR

#### ::::::::::::: Repeating for P E A R S O N Distribution  #####

### Check the summary stats of the standardized residuals
CrCu_XMR_Model8_res_wo <- CrCu_XMR_Model8_wo@fit$residuals/CrCu_XMR_Model8_wo@fit$sigma
CrCu_XMR_Descr8_wo <- basicStats(CrCu_XMR_Model8_res_wo)

### Fitting distributions on the standardized residuals 

library(PearsonDS)
library(gsl)
CrCu_XMR_Dist8_wo   <- pearsonFitML(CrCu_XMR_Model8_res_wo)

### Store the parameters for different distributions  

CrCu_XMR_P4_Param_wo    <- CrCu_XMR_Dist8_wo[2:5]

### Checking the fit using KS tests

CrCu_XMR_test1_wo <- ks.test(CrCu_XMR_Model8_res_wo,rpearson(length(CrCu_XMR_Model8_res_wo),params = CrCu_XMR_Dist8_wo))
#Null: True distribution is equal to the hypothesized distribution
#p-value is high, hence, we fail to reject H0.

### Calculate the quantiles

XMR_Exceed_Pearson_wo    <- array(data=0,dim=length(signific_levels))
#qpearson(signific_levels[2],CrCu_XMR_Dist8_wo)
for(j in 1:3) {
  Z_Pearson_XMR_wo   <- qpearson(signific_levels[j],CrCu_XMR_Dist8_wo)
  
  mu_XMR_wo   <- array(data=0,dim=length(Returns_CrCu_XMR)-350)
  VaR_XMR_wo   <- array(data=0,dim=length(Returns_CrCu_XMR)-351)
  
  for(i in 2:(length(Returns_CrCu_XMR)-350))
  {
    mu_XMR_wo[i] <- CrCu_XMR_Model8_wo@fit$coef[1]+CrCu_XMR_Model8_wo@fit$coef[2]*Returns_CrCu_XMR[i-1]+CrCu_XMR_Model8_wo@fit$coef[3]*CrCu_XMR_Model8_res_wo[i-1]
    VaR_XMR_wo[i-1] <- mu_XMR_wo[i]+CrCu_XMR_Model8_wo@fit$sigma[i]*Z_Pearson_XMR_wo
  }
  print(mu_XMR_wo)
  print(CrCu_XMR_Model8_wo@fit$sigma*Z_Pearson_XMR_wo)
  #XMR_Violations_wo[j]   <- VaRTest(signific_levels[j],Returns_CrCu_XMR[1:(length(VaR_XMR_wo))],VaR_XMR_wo)
  #print(VaRTest(signific_levels[j],Returns_CrCu_XMR[1:(length(VaR_XMR_wo))],VaR_XMR_wo))
  #XMR_Exceed_Pearson_wo[j] <- XMR_Violations_wo$actual.exceed
}

#### Out of sample VaR forecast  #####

Holdout_Returns_XMR_wo   <- Returns_CrCu_XMR[(length(Returns_CrCu_XMR)-349):length(Returns_CrCu_XMR)] 

Holdout_XMR_Pearson_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_XMR_Pearson_pvalues_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_XMR_Pearson_wo)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_XMR_Pearson_pvalues_wo)   <- c("UC","CC","DQ")
rownames(Holdout_XMR_Pearson_wo) <- c("1%","5%","10%")
rownames(Holdout_XMR_Pearson_pvalues_wo) <- c("1%","5%","10%")

## Store the volatility and mu forecast
signific_levels <- c(0.1,0.05,0.01)

XMR_Holdout_Mu_wo <- as.data.frame(CrCu_XMR_Roll8_wo)[,'Mu']
XMR_Holdout_Sigma_wo <- as.data.frame(CrCu_XMR_Roll8_wo)[,'Sigma']

MP_XMR_wo <- c(0 , 0, 0)
Holdout_XMR_Pearson_wo <- matrix(0, nrow = 3, ncol = 5)
Holdout_XMR_Pearson_pvalues_wo <- matrix(0, nrow = 3, ncol = 3)

for(j in 1:3) {
  
  Z_Pearson_XMR_wo   <- qpearson(signific_levels[j],CrCu_XMR_Dist8_wo)
  
  XMR_Holdout_VaR_wo   <- XMR_Holdout_Mu_wo + XMR_Holdout_Sigma_wo*Z_Pearson_XMR_wo
  XMR_Violations_wo   <- BacktestVaR(Holdout_Returns_XMR_wo[1:(length(Holdout_Returns_XMR_wo)-1)],XMR_Holdout_VaR_wo[-1],signific_levels[j])
  Holdout_XMR_Pearson_wo[j,]   <- c(XMR_Violations_wo$LRuc[1],XMR_Violations_wo$LRcc[1],XMR_Violations_wo$DQ$stat,XMR_Violations_wo$AE,XMR_Violations_wo$Loss$Loss)
  Holdout_XMR_Pearson_pvalues_wo[j,] <- c(XMR_Violations_wo$LRuc[2],XMR_Violations_wo$LRcc[2],XMR_Violations_wo$DQ$pvalue)
  MP_XMR_wo[j] <- MAPE(XMR_Holdout_Sigma_wo*Z_Pearson_XMR_wo, abs(CrCu_XMR_Roll7_wo@forecast$density$Realized))
  #print(XMR_Violations_wo$AE)
  #print(XMR_Violations)
  print(MP_XMR_wo)
}

#####################################################################################
### Using VaR Test individually ###
## VaRTest in R

CrCu_XMR_Norm_VaR_Test1_wo  <- VaRTest(0.01,CrCu_XMR_VaR_Norm_wo[,4],CrCu_XMR_VaR_Norm_wo[,1])
CrCu_XMR_Norm_VaR_Test5_wo  <- VaRTest(0.05,CrCu_XMR_VaR_Norm_wo[,4],CrCu_XMR_VaR_Norm_wo[,2])
CrCu_XMR_Norm_VaR_Test10_wo <- VaRTest(0.1,CrCu_XMR_VaR_Norm_wo[,4],CrCu_XMR_VaR_Norm_wo[,3])

CrCu_XMR_JSU_VaR_Test1_wo  <- VaRTest(0.01,CrCu_XMR_VaR_JSU_wo[,4],CrCu_XMR_VaR_JSU_wo[,1])
CrCu_XMR_JSU_VaR_Test5_wo  <- VaRTest(0.05,CrCu_XMR_VaR_JSU_wo[,4],CrCu_XMR_VaR_JSU_wo[,2])
CrCu_XMR_JSU_VaR_Test10_wo <- VaRTest(0.1,CrCu_XMR_VaR_JSU_wo[,4],CrCu_XMR_VaR_JSU_wo[,3])

CrCu_XMR_SGED_VaR_Test1_wo  <- VaRTest(0.01,CrCu_XMR_VaR_SGED_wo[,4],CrCu_XMR_VaR_SGED_wo[,1])
CrCu_XMR_SGED_VaR_Test5_wo  <- VaRTest(0.05,CrCu_XMR_VaR_SGED_wo[,4],CrCu_XMR_VaR_SGED_wo[,2])
CrCu_XMR_SGED_VaR_Test10_wo <- VaRTest(0.1,CrCu_XMR_VaR_SGED_wo[,4],CrCu_XMR_VaR_SGED_wo[,3])

CrCu_XMR_SST_VaR_Test1_wo  <- VaRTest(0.01,CrCu_XMR_VaR_SST_wo[,4],CrCu_XMR_VaR_SST_wo[,1])
CrCu_XMR_SST_VaR_Test5_wo  <- VaRTest(0.05,CrCu_XMR_VaR_SST_wo[,4],CrCu_XMR_VaR_SST_wo[,2])
CrCu_XMR_SST_VaR_Test10_wo <- VaRTest(0.1,CrCu_XMR_VaR_SST_wo[,4],CrCu_XMR_VaR_SST_wo[,3])

CrCu_XMR_VaR_Test_pvalues1_wo <- matrix(c(CrCu_XMR_SST_VaR_Test1_wo$uc.LRp,CrCu_XMR_SST_VaR_Test1_wo$cc.LRp,
                                          CrCu_XMR_SGED_VaR_Test1_wo$uc.LRp,CrCu_XMR_SGED_VaR_Test1_wo$cc.LRp,
                                          CrCu_XMR_JSU_VaR_Test1_wo$uc.LRp,CrCu_XMR_JSU_VaR_Test1_wo$cc.LRp,
                                          CrCu_XMR_Norm_VaR_Test1_wo$uc.LRp,CrCu_XMR_Norm_VaR_Test1_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_pvalues5_wo <- matrix(c(CrCu_XMR_SST_VaR_Test5_wo$uc.LRp,CrCu_XMR_SST_VaR_Test5_wo$cc.LRp,
                                          CrCu_XMR_SGED_VaR_Test5_wo$uc.LRp,CrCu_XMR_SGED_VaR_Test5_wo$cc.LRp,
                                          CrCu_XMR_JSU_VaR_Test5_wo$uc.LRp,CrCu_XMR_JSU_VaR_Test5_wo$cc.LRp,
                                          CrCu_XMR_Norm_VaR_Test5_wo$uc.LRp,CrCu_XMR_Norm_VaR_Test5_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_XMR_VaR_Test_pvalues10_wo <- matrix(c(CrCu_XMR_SST_VaR_Test10_wo$uc.LRp,CrCu_XMR_SST_VaR_Test10_wo$cc.LRp,
                                           CrCu_XMR_SGED_VaR_Test10_wo$uc.LRp,CrCu_XMR_SGED_VaR_Test10_wo$cc.LRp,
                                           CrCu_XMR_JSU_VaR_Test10_wo$uc.LRp,CrCu_XMR_JSU_VaR_Test10_wo$cc.LRp,
                                           CrCu_XMR_Norm_VaR_Test10_wo$uc.LRp,CrCu_XMR_Norm_VaR_Test10_wo$cc.LRp),ncol=2,byrow = TRUE)
###################:::::::::::::::::::::::::::::::::::::::::############################

#### ********** E T H E R E U M  C L A S S I C ********** ####
Crypto1$ETC = as.numeric(as.character(Crypto1$ETC))
CrCu_ETC <- data.frame(Crypto_Date, Crypto$Bitcoin) # Bitcoin Data with Date
View(CrCu_ETC)

####### :::::::::: Returns Series

Returns_CrCu_ETC <- log(Crypto1$ETC[-1]/Crypto1$ETC[1:(nrow(Crypto1)-1)]) # Returns series
Des_Returns_CrCu_ETC <- basicStats(Returns_CrCu_ETC) # Descriptive statistics
View(Returns_CrCu_ETC)

#### Convert into xts objects ####

CrCu_ETC_xts   <- xts(Returns_CrCu_ETC,order.by = Crypto_Date[-1])
names(CrCu_ETC_xts)   <- "Returns"
View(CrCu_ETC_xts)

## :::::::::::: check presence of ARCH effect in Full Sample
library(FinTS)
ArchTest(CrCu_ETC_xts, lags = 1)
ArchTest(CrCu_ETC_xts, lags = 5)
ArchTest(CrCu_ETC_xts, lags = 10)

# H0: Series has no ARCH effects
# Null hypothesis is rejected, hence proves presence of ARCH effects

### Garch Models
# :::::::::Without external regressors in Full Sample GARCH

#SST #SGED #JSU #Norm
CrCu_ETC_Spec1_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETC_Spec2_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETC_Spec3_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETC_Spec4_wo <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETC_Model1_wo <- ugarchfit(CrCu_ETC_Spec1_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model2_wo <- ugarchfit(CrCu_ETC_Spec2_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model3_wo <- ugarchfit(CrCu_ETC_Spec3_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model4_wo <- ugarchfit(CrCu_ETC_Spec4_wo,CrCu_ETC_xts,out.sample = 350)

#### egarch models

CrCu_ETC_Spec5_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETC_Spec6_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETC_Spec7_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETC_Spec8_wo <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETC_Model5_wo <- ugarchfit(CrCu_ETC_Spec5_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model6_wo <- ugarchfit(CrCu_ETC_Spec6_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model7_wo <- ugarchfit(CrCu_ETC_Spec7_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model8_wo <- ugarchfit(CrCu_ETC_Spec8_wo,CrCu_ETC_xts,out.sample = 350)

### gjrgarch models

CrCu_ETC_Spec9_wo  <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sstd")
CrCu_ETC_Spec10_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="sged")
CrCu_ETC_Spec11_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="jsu")
CrCu_ETC_Spec12_wo <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="norm")

CrCu_ETC_Model9_wo  <- ugarchfit(CrCu_ETC_Spec9_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model10_wo <- ugarchfit(CrCu_ETC_Spec10_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model11_wo <- ugarchfit(CrCu_ETC_Spec11_wo,CrCu_ETC_xts,out.sample = 350)
CrCu_ETC_Model12_wo <- ugarchfit(CrCu_ETC_Spec12_wo,CrCu_ETC_xts,out.sample = 350)

## persistence

persis_ETC_wo <- matrix(data=0, nrow = 12, ncol = 1)
persis_ETC_wo[1,1] <- round(persistence(CrCu_ETC_Model1_wo),4)
persis_ETC_wo[2,1] <- round(persistence(CrCu_ETC_Model2_wo),4)
persis_ETC_wo[3,1] <- round(persistence(CrCu_ETC_Model3_wo),4)
persis_ETC_wo[4,1] <- round(persistence(CrCu_ETC_Model4_wo),4)
persis_ETC_wo[5,1] <- round(persistence(CrCu_ETC_Model5_wo),4)
persis_ETC_wo[6,1] <- round(persistence(CrCu_ETC_Model6_wo),4)
persis_ETC_wo[7,1] <- round(persistence(CrCu_ETC_Model7_wo),4)
persis_ETC_wo[8,1] <- round(persistence(CrCu_ETC_Model8_wo),4)
persis_ETC_wo[9,1] <- round(persistence(CrCu_ETC_Model9_wo),4)
persis_ETC_wo[10,1] <- round(persistence(CrCu_ETC_Model10_wo),4)
persis_ETC_wo[11,1] <- round(persistence(CrCu_ETC_Model11_wo),4)
persis_ETC_wo[12,1] <- round(persistence(CrCu_ETC_Model12_wo),4)

persistance_ETC_wo <- matrix(data=persis_ETC_wo, nrow = 12, ncol = 1)
colnames(persistance_ETC_wo) <- c("Persistence_ETC")
rownames(persistance_ETC_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(data.frame(persistance_ETC_wo))
#xtable(persistance_ETC, digits = 4)

### Store the AIC values  ####

AIC_ETC_wo  <- matrix(data=0,nrow=12,ncol=1)
colnames(AIC_ETC_wo) <- c("CrCu_ETC")
rownames(AIC_ETC_wo) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
AIC_ETC_wo[1,1]  <-  infocriteria(CrCu_ETC_Model1_wo)[1]
AIC_ETC_wo[2,1]  <-  infocriteria(CrCu_ETC_Model2_wo)[1]
AIC_ETC_wo[3,1]  <-  infocriteria(CrCu_ETC_Model3_wo)[1]
AIC_ETC_wo[4,1]  <-  infocriteria(CrCu_ETC_Model4_wo)[1]
AIC_ETC_wo[5,1]  <-  infocriteria(CrCu_ETC_Model5_wo)[1]
AIC_ETC_wo[6,1]  <-  infocriteria(CrCu_ETC_Model6_wo)[1]
AIC_ETC_wo[7,1]  <-  infocriteria(CrCu_ETC_Model7_wo)[1]
AIC_ETC_wo[8,1]  <-  infocriteria(CrCu_ETC_Model8_wo)[1]
AIC_ETC_wo[9,1]  <-  infocriteria(CrCu_ETC_Model9_wo)[1]
AIC_ETC_wo[10,1] <- infocriteria(CrCu_ETC_Model10_wo)[1]
AIC_ETC_wo[11,1] <- infocriteria(CrCu_ETC_Model11_wo)[1]
AIC_ETC_wo[12,1] <- infocriteria(CrCu_ETC_Model12_wo)[1]
View(AIC_ETC_wo)
xtable(AIC_ETC_wo, caption = "Information Criteria for Bitcoin GARCH models - Full Sample", digits = 4)

## Conclusion: eGarch-JSU have the minimum AIC and eGarch-SST is the next best #####
## Model 7 and the Model 5

## coefficients

coef_ETC_wo_1 <- round(coef(CrCu_ETC_Model1_wo),4)
coef_ETC_wo_2 <- round(coef(CrCu_ETC_Model2_wo),4)
coef_ETC_wo_3 <- round(coef(CrCu_ETC_Model3_wo),4)
coef_ETC_wo_4 <- round(coef(CrCu_ETC_Model4_wo),4)
coef_ETC_wo_5 <- round(coef(CrCu_ETC_Model5_wo),4)
coef_ETC_wo_6 <- round(coef(CrCu_ETC_Model6_wo),4)
coef_ETC_wo_7 <- round(coef(CrCu_ETC_Model7_wo),4)
coef_ETC_wo_8 <- round(coef(CrCu_ETC_Model8_wo),4)
coef_ETC_wo_9 <- round(coef(CrCu_ETC_Model9_wo),4)
coef_ETC_wo_10 <- round(coef(CrCu_ETC_Model10_wo),4)
coef_ETC_wo_11 <- round(coef(CrCu_ETC_Model11_wo),4)
coef_ETC_wo_12 <- round(coef(CrCu_ETC_Model12_wo),4)

garch_coef_ETC_wo <- bind_rows(coef_ETC_wo_1, coef_ETC_wo_2, coef_ETC_wo_3, coef_ETC_wo_4,coef_ETC_wo_5, coef_ETC_wo_6, coef_ETC_wo_7, coef_ETC_wo_8, coef_ETC_wo_9, coef_ETC_wo_10, coef_ETC_wo_11, coef_ETC_wo_12)

garch_coef_ETC_wo1 <- as.data.frame(garch_coef_ETC_wo)
rownames(garch_coef_ETC_wo1) <- c("Garch-SST","Garch-SGED","Garch-JSU","Garch-Normal", "eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Normal", "GJRGarch-SST","GJRGarch-SGED","GJRGarch-JSU","GJRGarch-Normal")
View(garch_coef_ETC_wo1)
xtable(garch_coef_ETC_wo1, caption = "Coefficients of Bitcoin GARCH models - Full Sample", digits = 4)

### Ljung-box test statistic
# Check standardized shocks

#CrCu_ETC5_LQ1_wo  <- Box.test(CrCu_ETC_Model5_wo@fit$residuals/CrCu_ETC_Model5_wo@fit$sigma,lag=1,type="Lj")
#CrCu_ETC5_LQ5_wo  <- Box.test(CrCu_ETC_Model5_wo@fit$residuals/CrCu_ETC_Model5_wo@fit$sigma,lag=5,type="Lj")
#CrCu_ETC5_LQ10_wo <- Box.test(CrCu_ETC_Model5_wo@fit$residuals/CrCu_ETC_Model5_wo@fit$sigma,lag=10,type="Lj")

#CrCu_ETC6_LQ1_wo  <- Box.test(CrCu_ETC_Model6_wo@fit$residuals/CrCu_ETC_Model6_wo@fit$sigma,lag=1,type="Lj")
#CrCu_ETC6_LQ5_wo  <- Box.test(CrCu_ETC_Model6_wo@fit$residuals/CrCu_ETC_Model6_wo@fit$sigma,lag=5,type="Lj")
#CrCu_ETC6_LQ10_wo <- Box.test(CrCu_ETC_Model6_wo@fit$residuals/CrCu_ETC_Model6_wo@fit$sigma,lag=10,type="Lj")

#CrCu_ETC7_LQ1_wo  <- Box.test(CrCu_ETC_Model7_wo@fit$residuals/CrCu_ETC_Model7_wo@fit$sigma,lag=1,type="Lj")
#CrCu_ETC7_LQ5_wo  <- Box.test(CrCu_ETC_Model7_wo@fit$residuals/CrCu_ETC_Model7_wo@fit$sigma,lag=5,type="Lj")
#CrCu_ETC7_LQ10_wo <- Box.test(CrCu_ETC_Model7_wo@fit$residuals/CrCu_ETC_Model7_wo@fit$sigma,lag=10,type="Lj")

#CrCu_ETC8_LQ1_wo  <- Box.test(CrCu_ETC_Model8_wo@fit$residuals/CrCu_ETC_Model8_wo@fit$sigma,lag=1,type="Lj")
#CrCu_ETC8_LQ5_wo  <- Box.test(CrCu_ETC_Model8_wo@fit$residuals/CrCu_ETC_Model8_wo@fit$sigma,lag=5,type="Lj")
#CrCu_ETC8_LQ10_wo <- Box.test(CrCu_ETC_Model8_wo@fit$residuals/CrCu_ETC_Model8_wo@fit$sigma,lag=10,type="Lj")

#CrCu_ETC5_LQ_wo <- c(CrCu_ETC5_LQ1_wo$statistic,CrCu_ETC5_LQ5_wo$statistic,CrCu_ETC5_LQ10_wo$statistic)
#CrCu_ETC6_LQ_wo <- c(CrCu_ETC6_LQ1_wo$statistic,CrCu_ETC6_LQ5_wo$statistic,CrCu_ETC6_LQ10_wo$statistic)
#CrCu_ETC7_LQ_wo <- c(CrCu_ETC7_LQ1_wo$statistic,CrCu_ETC7_LQ5_wo$statistic,CrCu_ETC7_LQ10_wo$statistic)
#CrCu_ETC8_LQ_wo <- c(CrCu_ETC8_LQ1_wo$statistic,CrCu_ETC8_LQ5_wo$statistic,CrCu_ETC8_LQ10_wo$statistic)

#CrCu_ETC_res_stat_wo <-   cbind(CrCu_ETC5_LQ_wo,CrCu_ETC6_LQ_wo,CrCu_ETC7_LQ_wo,CrCu_ETC8_LQ_wo)
#rownames(CrCu_ETC_res_stat_wo) <- c("LQ1","LQ5","LQ10")
#colnames(CrCu_ETC_res_stat_wo) <- c("eGarch-SST","eGarch-SGED","eGarch-JSU","eGarch-Norm")
# Serial correlation of data
# H0: Given time series is independent/ absence of serial correlation upto lag k
# if p value is less than 0.05, then reject null, and hence, dependent time series and presence of serial corrlelation

#### 1-day ahead Rolling Forecasts ####
## Full sample for roll i.e. last 350 points for forecast and rest for model
CrCu_ETC_Roll5_wo <-  ugarchroll(CrCu_ETC_Spec5_wo,CrCu_ETC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll6_wo <-  ugarchroll(CrCu_ETC_Spec6_wo,CrCu_ETC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll7_wo <-  ugarchroll(CrCu_ETC_Spec7_wo,CrCu_ETC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)
CrCu_ETC_Roll8_wo <-  ugarchroll(CrCu_ETC_Spec8_wo,CrCu_ETC_xts,forecast.length = 350,refit.every = 50,solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = c(0.01,0.05,0.10),keep.coef = TRUE)

# :::::::::::::assess forecasting performance of volatility model
### Out of sample forecast check for GARCH model
# Check in the last few lines of the code

#### Store the VaR for the out sample using rolling window forecasts

CrCu_ETC_VaR_SST_wo   <- CrCu_ETC_Roll5_wo@forecast$VaR
CrCu_ETC_VaR_SGED_wo  <- CrCu_ETC_Roll6_wo@forecast$VaR
CrCu_ETC_VaR_JSU_wo   <- CrCu_ETC_Roll7_wo@forecast$VaR
CrCu_ETC_VaR_Norm_wo  <- CrCu_ETC_Roll8_wo@forecast$VaR

#### ::::::::::::: Repeating for P E A R S O N Distribution  #####

### Check the summary stats of the standardized residuals
CrCu_ETC_Model8_res_wo <- CrCu_ETC_Model8_wo@fit$residuals/CrCu_ETC_Model8_wo@fit$sigma
CrCu_ETC_Descr8_wo <- basicStats(CrCu_ETC_Model8_res_wo)

### Fitting distributions on the standardized residuals 

library(PearsonDS)
library(gsl)
CrCu_ETC_Dist8_wo   <- pearsonFitML(CrCu_ETC_Model8_res_wo)

### Store the parameters for different distributions  

CrCu_ETC_P4_Param_wo    <- CrCu_ETC_Dist8_wo[2:5]

### Checking the fit using KS tests

CrCu_ETC_test1_wo <- ks.test(CrCu_ETC_Model8_res_wo,rpearson(length(CrCu_ETC_Model8_res_wo),params = CrCu_ETC_Dist8_wo))
#Null: True distribution is equal to the hypothesized distribution
#p-value is high, hence, we fail to reject H0.

### Calculate the quantiles

ETC_Exceed_Pearson_wo    <- array(data=0,dim=length(signific_levels))
#qpearson(signific_levels[2],CrCu_ETC_Dist8_wo)
for(j in 1:3) {
  Z_Pearson_ETC_wo   <- qpearson(signific_levels[j],CrCu_ETC_Dist8_wo)
  
  mu_ETC_wo   <- array(data=0,dim=length(Returns_CrCu_ETC)-350)
  VaR_ETC_wo   <- array(data=0,dim=length(Returns_CrCu_ETC)-351)
  
  for(i in 2:(length(Returns_CrCu_ETC)-350))
  {
    mu_ETC_wo[i] <- CrCu_ETC_Model8_wo@fit$coef[1]+CrCu_ETC_Model8_wo@fit$coef[2]*Returns_CrCu_ETC[i-1]+CrCu_ETC_Model8_wo@fit$coef[3]*CrCu_ETC_Model8_res_wo[i-1]
    VaR_ETC_wo[i-1] <- mu_ETC_wo[i]+CrCu_ETC_Model8_wo@fit$sigma[i]*Z_Pearson_ETC_wo
  }
  print(mu_ETC_wo)
  print(CrCu_ETC_Model8_wo@fit$sigma*Z_Pearson_ETC_wo)
  #ETC_Violations_wo[j]   <- VaRTest(signific_levels[j],Returns_CrCu_ETC[1:(length(VaR_ETC_wo))],VaR_ETC_wo)
  #print(VaRTest(signific_levels[j],Returns_CrCu_ETC[1:(length(VaR_ETC_wo))],VaR_ETC_wo))
  #ETC_Exceed_Pearson_wo[j] <- ETC_Violations_wo$actual.exceed
}

#### Out of sample VaR forecast  #####

Holdout_Returns_ETC_wo   <- Returns_CrCu_ETC[(length(Returns_CrCu_ETC)-349):length(Returns_CrCu_ETC)] 

Holdout_ETC_Pearson_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=5)
Holdout_ETC_Pearson_pvalues_wo    <- matrix(data=0,nrow=length(signific_levels),ncol=3)
colnames(Holdout_ETC_Pearson_wo)   <- c("UC","CC","DQ","AE","QL")
colnames(Holdout_ETC_Pearson_pvalues_wo)   <- c("UC","CC","DQ")
rownames(Holdout_ETC_Pearson_wo) <- c("1%","5%","10%")
rownames(Holdout_ETC_Pearson_pvalues_wo) <- c("1%","5%","10%")

## Store the volatility and mu forecast
signific_levels <- c(0.1,0.05,0.01)

ETC_Holdout_Mu_wo <- as.data.frame(CrCu_ETC_Roll8_wo)[,'Mu']
ETC_Holdout_Sigma_wo <- as.data.frame(CrCu_ETC_Roll8_wo)[,'Sigma']

MP_ETC_wo <- c(0 , 0, 0)
Holdout_ETC_Pearson_wo <- matrix(0, nrow = 3, ncol = 5)
Holdout_ETC_Pearson_pvalues_wo <- matrix(0, nrow = 3, ncol = 3)

for(j in 1:3) {
  
  Z_Pearson_ETC_wo   <- qpearson(signific_levels[j],CrCu_ETC_Dist8_wo)
  
  ETC_Holdout_VaR_wo   <- ETC_Holdout_Mu_wo + ETC_Holdout_Sigma_wo*Z_Pearson_ETC_wo
  ETC_Violations_wo   <- BacktestVaR(Holdout_Returns_ETC_wo[1:(length(Holdout_Returns_ETC_wo)-1)],ETC_Holdout_VaR_wo[-1],signific_levels[j])
  Holdout_ETC_Pearson_wo[j,]   <- c(ETC_Violations_wo$LRuc[1],ETC_Violations_wo$LRcc[1],ETC_Violations_wo$DQ$stat,ETC_Violations_wo$AE,ETC_Violations_wo$Loss$Loss)
  Holdout_ETC_Pearson_pvalues_wo[j,] <- c(ETC_Violations_wo$LRuc[2],ETC_Violations_wo$LRcc[2],ETC_Violations_wo$DQ$pvalue)
  MP_ETC_wo[j] <- MAPE(ETC_Holdout_Sigma_wo*Z_Pearson_ETC_wo, abs(CrCu_ETC_Roll7_wo@forecast$density$Realized))
  #print(ETC_Violations_wo$AE)
  #print(ETC_Violations)
  print(MP_ETC_wo)
}

#####################################################################################
### Using VaR Test individually ###
## VaRTest in R

CrCu_ETC_Norm_VaR_Test1_wo  <- VaRTest(0.01,CrCu_ETC_VaR_Norm_wo[,4],CrCu_ETC_VaR_Norm_wo[,1])
CrCu_ETC_Norm_VaR_Test5_wo  <- VaRTest(0.05,CrCu_ETC_VaR_Norm_wo[,4],CrCu_ETC_VaR_Norm_wo[,2])
CrCu_ETC_Norm_VaR_Test10_wo <- VaRTest(0.1,CrCu_ETC_VaR_Norm_wo[,4],CrCu_ETC_VaR_Norm_wo[,3])

CrCu_ETC_JSU_VaR_Test1_wo  <- VaRTest(0.01,CrCu_ETC_VaR_JSU_wo[,4],CrCu_ETC_VaR_JSU_wo[,1])
CrCu_ETC_JSU_VaR_Test5_wo  <- VaRTest(0.05,CrCu_ETC_VaR_JSU_wo[,4],CrCu_ETC_VaR_JSU_wo[,2])
CrCu_ETC_JSU_VaR_Test10_wo <- VaRTest(0.1,CrCu_ETC_VaR_JSU_wo[,4],CrCu_ETC_VaR_JSU_wo[,3])

CrCu_ETC_SGED_VaR_Test1_wo  <- VaRTest(0.01,CrCu_ETC_VaR_SGED_wo[,4],CrCu_ETC_VaR_SGED_wo[,1])
CrCu_ETC_SGED_VaR_Test5_wo  <- VaRTest(0.05,CrCu_ETC_VaR_SGED_wo[,4],CrCu_ETC_VaR_SGED_wo[,2])
CrCu_ETC_SGED_VaR_Test10_wo <- VaRTest(0.1,CrCu_ETC_VaR_SGED_wo[,4],CrCu_ETC_VaR_SGED_wo[,3])

CrCu_ETC_SST_VaR_Test1_wo  <- VaRTest(0.01,CrCu_ETC_VaR_SST_wo[,4],CrCu_ETC_VaR_SST_wo[,1])
CrCu_ETC_SST_VaR_Test5_wo  <- VaRTest(0.05,CrCu_ETC_VaR_SST_wo[,4],CrCu_ETC_VaR_SST_wo[,2])
CrCu_ETC_SST_VaR_Test10_wo <- VaRTest(0.1,CrCu_ETC_VaR_SST_wo[,4],CrCu_ETC_VaR_SST_wo[,3])

CrCu_ETC_VaR_Test_pvalues1_wo <- matrix(c(CrCu_ETC_SST_VaR_Test1_wo$uc.LRp,CrCu_ETC_SST_VaR_Test1_wo$cc.LRp,
                                          CrCu_ETC_SGED_VaR_Test1_wo$uc.LRp,CrCu_ETC_SGED_VaR_Test1_wo$cc.LRp,
                                          CrCu_ETC_JSU_VaR_Test1_wo$uc.LRp,CrCu_ETC_JSU_VaR_Test1_wo$cc.LRp,
                                          CrCu_ETC_Norm_VaR_Test1_wo$uc.LRp,CrCu_ETC_Norm_VaR_Test1_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_pvalues5_wo <- matrix(c(CrCu_ETC_SST_VaR_Test5_wo$uc.LRp,CrCu_ETC_SST_VaR_Test5_wo$cc.LRp,
                                          CrCu_ETC_SGED_VaR_Test5_wo$uc.LRp,CrCu_ETC_SGED_VaR_Test5_wo$cc.LRp,
                                          CrCu_ETC_JSU_VaR_Test5_wo$uc.LRp,CrCu_ETC_JSU_VaR_Test5_wo$cc.LRp,
                                          CrCu_ETC_Norm_VaR_Test5_wo$uc.LRp,CrCu_ETC_Norm_VaR_Test5_wo$cc.LRp),ncol=2,byrow = TRUE)

CrCu_ETC_VaR_Test_pvalues10_wo <- matrix(c(CrCu_ETC_SST_VaR_Test10_wo$uc.LRp,CrCu_ETC_SST_VaR_Test10_wo$cc.LRp,
                                           CrCu_ETC_SGED_VaR_Test10_wo$uc.LRp,CrCu_ETC_SGED_VaR_Test10_wo$cc.LRp,
                                           CrCu_ETC_JSU_VaR_Test10_wo$uc.LRp,CrCu_ETC_JSU_VaR_Test10_wo$cc.LRp,
                                           CrCu_ETC_Norm_VaR_Test10_wo$uc.LRp,CrCu_ETC_Norm_VaR_Test10_wo$cc.LRp),ncol=2,byrow = TRUE)
###################:::::::::::::::::::::::::::::::::::::::::############################

#####################################:::::::::::::::::::::::::::::
### MAPE - Out of sample forecast
###############::::::::::::::::: B I T C O I N ::::::::##########
# Full Sample

garchsigSGED_BTC_wo <- CrCu_BTC_Roll6_wo@forecast$density$Sigma
realval_BTC_wo <- abs(CrCu_BTC_Roll6_wo@forecast$density$Realized)
MAPEgvalSGED_BTC_wo <- MAPE(garchsigSGED_BTC_wo, realval_BTC_wo)

garchsigSST_BTC_wo <- CrCu_BTC_Roll5_wo@forecast$density$Sigma
realval_BTC_wo <- abs(CrCu_BTC_Roll5_wo@forecast$density$Realized)
MAPEgvalSST_BTC_wo <- MAPE(garchsigSST_BTC_wo, realval_BTC_wo)

garchsigJSU_BTC_wo <- CrCu_BTC_Roll7_wo@forecast$density$Sigma
realval_BTC_wo <- abs(CrCu_BTC_Roll7_wo@forecast$density$Realized)
MAPEgvalJSU_BTC_wo <- MAPE(garchsigJSU_BTC_wo, realval_BTC_wo)

garchsigNorm_BTC_wo <- CrCu_BTC_Roll8_wo@forecast$density$Sigma
realval_BTC_wo <- abs(CrCu_BTC_Roll8_wo@forecast$density$Realized)
MAPEgvalNorm_BTC_wo <- MAPE(garchsigNorm_BTC_wo, realval_BTC_wo)

MAPE_BTC_wo <- c(MAPEgvalSGED_BTC_wo, MAPEgvalSST_BTC_wo, MAPEgvalJSU_BTC_wo, MAPEgvalNorm_BTC_wo)

MAPE_BTC_wo <- matrix(data = MAPE_BTC_wo, nrow = 4)
colnames(MAPE_BTC_wo) <- c("Full Sample")
rownames(MAPE_BTC_wo) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: E T H E R E U M ::::::::##########
garchsigSGED_ETH_wo <- CrCu_ETH_Roll6_wo@forecast$density$Sigma
realval_ETH_wo <- abs(CrCu_ETH_Roll6_wo@forecast$density$Realized)
MAPEgvalSGED_ETH_wo <- MAPE(garchsigSGED_ETH_wo, realval_ETH_wo)

garchsigSST_ETH_wo <- CrCu_ETH_Roll5_wo@forecast$density$Sigma
realval_ETH_wo <- abs(CrCu_ETH_Roll5_wo@forecast$density$Realized)
MAPEgvalSST_ETH_wo <- MAPE(garchsigSST_ETH_wo, realval_ETH_wo)

garchsigJSU_ETH_wo <- CrCu_ETH_Roll7_wo@forecast$density$Sigma
realval_ETH_wo <- abs(CrCu_ETH_Roll7_wo@forecast$density$Realized)
MAPEgvalJSU_ETH_wo <- MAPE(garchsigJSU_ETH_wo, realval_ETH_wo)

garchsigNorm_ETH_wo <- CrCu_ETH_Roll8_wo@forecast$density$Sigma
realval_ETH_wo <- abs(CrCu_ETH_Roll8_wo@forecast$density$Realized)
MAPEgvalNorm_ETH_wo <- MAPE(garchsigNorm_ETH_wo, realval_ETH_wo)

MAPE_ETH_wo <- c(MAPEgvalSGED_ETH_wo, MAPEgvalSST_ETH_wo, MAPEgvalJSU_ETH_wo, MAPEgvalNorm_ETH_wo)

MAPE_ETH_wo <- matrix(data = MAPE_ETH_wo, nrow = 4)
colnames(MAPE_ETH_wo) <- c("Full Sample")
rownames(MAPE_ETH_wo) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: L I T E C O I N ::::::::##########

garchsigSGED_LTC_wo <- CrCu_LTC_Roll6_wo@forecast$density$Sigma
realval_LTC_wo <- abs(CrCu_LTC_Roll6_wo@forecast$density$Realized)
MAPEgvalSGED_LTC_wo <- MAPE(garchsigSGED_LTC_wo, realval_LTC_wo)

garchsigSST_LTC_wo <- CrCu_LTC_Roll5_wo@forecast$density$Sigma
realval_LTC_wo <- abs(CrCu_LTC_Roll5_wo@forecast$density$Realized)
MAPEgvalSST_LTC_wo <- MAPE(garchsigSST_LTC_wo, realval_LTC_wo)

garchsigJSU_LTC_wo <- CrCu_LTC_Roll7_wo@forecast$density$Sigma
realval_LTC_wo <- abs(CrCu_LTC_Roll7_wo@forecast$density$Realized)
MAPEgvalJSU_LTC_wo <- MAPE(garchsigJSU_LTC_wo, realval_LTC_wo)

garchsigNorm_LTC_wo <- CrCu_LTC_Roll8_wo@forecast$density$Sigma
realval_LTC_wo <- abs(CrCu_LTC_Roll8_wo@forecast$density$Realized)
MAPEgvalNorm_LTC_wo <- MAPE(garchsigNorm_LTC_wo, realval_LTC_wo)

MAPE_LTC_wo <- c(MAPEgvalSGED_LTC_wo, MAPEgvalSST_LTC_wo, MAPEgvalJSU_LTC_wo, MAPEgvalNorm_LTC_wo)

MAPE_LTC_wo <- matrix(data = MAPE_LTC_wo, nrow = 4)
colnames(MAPE_LTC_wo) <- c("Full Sample")
rownames(MAPE_LTC_wo) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: M O N E R O ::::::::##########

garchsigSGED_XMR_wo <- CrCu_XMR_Roll6_wo@forecast$density$Sigma
realval_XMR_wo <- abs(CrCu_XMR_Roll6_wo@forecast$density$Realized)
MAPEgvalSGED_XMR_wo <- MAPE(garchsigSGED_XMR_wo, realval_XMR_wo)

garchsigSST_XMR_wo <- CrCu_XMR_Roll5_wo@forecast$density$Sigma
realval_XMR_wo <- abs(CrCu_XMR_Roll5_wo@forecast$density$Realized)
MAPEgvalSST_XMR_wo <- MAPE(garchsigSST_XMR_wo, realval_XMR_wo)

garchsigJSU_XMR_wo <- CrCu_XMR_Roll7_wo@forecast$density$Sigma
realval_XMR_wo <- abs(CrCu_XMR_Roll7_wo@forecast$density$Realized)
MAPEgvalJSU_XMR_wo <- MAPE(garchsigJSU_XMR_wo, realval_XMR_wo)

garchsigNorm_XMR_wo <- CrCu_XMR_Roll8_wo@forecast$density$Sigma
realval_XMR_wo <- abs(CrCu_XMR_Roll8_wo@forecast$density$Realized)
MAPEgvalNorm_XMR_wo <- MAPE(garchsigNorm_XMR_wo, realval_XMR_wo)

MAPE_XMR_wo <- c(MAPEgvalSGED_XMR_wo, MAPEgvalSST_XMR_wo, MAPEgvalJSU_XMR_wo, MAPEgvalNorm_XMR_wo)

MAPE_XMR_wo <- matrix(data = MAPE_XMR_wo, nrow = 4)
colnames(MAPE_XMR_wo) <- c("Full Sample")
rownames(MAPE_XMR_wo) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: E T H E R E U M  C L A S S I C ::::::::##########

garchsigSGED_ETC_wo <- CrCu_ETC_Roll6_wo@forecast$density$Sigma
realval_ETC_wo <- abs(CrCu_ETC_Roll6_wo@forecast$density$Realized)
MAPEgvalSGED_ETC_wo <- MAPE(garchsigSGED_ETC_wo, realval_ETC_wo)

garchsigSST_ETC_wo <- CrCu_ETC_Roll5_wo@forecast$density$Sigma
realval_ETC_wo <- abs(CrCu_ETC_Roll5_wo@forecast$density$Realized)
MAPEgvalSST_ETC_wo <- MAPE(garchsigSST_ETC_wo, realval_ETC_wo)

garchsigJSU_ETC_wo <- CrCu_ETC_Roll7_wo@forecast$density$Sigma
realval_ETC_wo <- abs(CrCu_ETC_Roll7_wo@forecast$density$Realized)
MAPEgvalJSU_ETC_wo <- MAPE(garchsigJSU_ETC_wo, realval_ETC_wo)

garchsigNorm_ETC_wo <- CrCu_ETC_Roll8_wo@forecast$density$Sigma
realval_ETC_wo <- abs(CrCu_ETC_Roll8_wo@forecast$density$Realized)
MAPEgvalNorm_ETC_wo <- MAPE(garchsigNorm_ETC_wo, realval_ETC_wo)

MAPE_ETC_wo <- c(MAPEgvalSGED_ETC_wo, MAPEgvalSST_ETC_wo, MAPEgvalJSU_ETC_wo, MAPEgvalNorm_ETC_wo)

MAPE_ETC_wo <- matrix(data = MAPE_ETC_wo, nrow = 4)
colnames(MAPE_ETC_wo) <- c("Full Sample")
rownames(MAPE_ETC_wo) <- c("SGED", "SST", "JSU", "Normal")

MAPE_crypto_wo <- rbind("BTC", round(MAPE_BTC_wo,4),
                     "ETH", round(MAPE_ETH_wo,4),
                     "LTC", round(MAPE_LTC_wo,4),
                     "XMR", round(MAPE_XMR_wo,4),
                     "ETC", round(MAPE_ETC_wo,4))

View(MAPE_crypto_wo)

xtable(MAPE_crypto_wo)

#####################################:::::::::::::::::::::::::::::
### RMSE - Out of sample forecast
###############::::::::::::::::: B I T C O I N ::::::::##########
# Full Sample

garchsigSGED_BTC_wo <- CrCu_BTC_Roll6_wo@forecast$density$Sigma
realval_BTC_wo <- abs(CrCu_BTC_Roll6_wo@forecast$density$Realized)
RMSEgvalSGED_BTC_wo <- RMSE(garchsigSGED_BTC_wo, realval_BTC_wo)

garchsigSST_BTC_wo <- CrCu_BTC_Roll5_wo@forecast$density$Sigma
realval_BTC_wo <- abs(CrCu_BTC_Roll5_wo@forecast$density$Realized)
RMSEgvalSST_BTC_wo <- RMSE(garchsigSST_BTC_wo, realval_BTC_wo)

garchsigJSU_BTC_wo <- CrCu_BTC_Roll7_wo@forecast$density$Sigma
realval_BTC_wo <- abs(CrCu_BTC_Roll7_wo@forecast$density$Realized)
RMSEgvalJSU_BTC_wo <- RMSE(garchsigJSU_BTC_wo, realval_BTC_wo)

garchsigNorm_BTC_wo <- CrCu_BTC_Roll8_wo@forecast$density$Sigma
realval_BTC_wo <- abs(CrCu_BTC_Roll8_wo@forecast$density$Realized)
RMSEgvalNorm_BTC_wo <- RMSE(garchsigNorm_BTC_wo, realval_BTC_wo)

RMSE_BTC_wo <- c(RMSEgvalSGED_BTC_wo, RMSEgvalSST_BTC_wo, RMSEgvalJSU_BTC_wo, RMSEgvalNorm_BTC_wo)

RMSE_BTC_wo <- matrix(data = RMSE_BTC_wo, nrow = 4)
colnames(RMSE_BTC_wo) <- c("Full Sample")
rownames(RMSE_BTC_wo) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: E T H E R E U M ::::::::##########
garchsigSGED_ETH_wo <- CrCu_ETH_Roll6_wo@forecast$density$Sigma
realval_ETH_wo <- abs(CrCu_ETH_Roll6_wo@forecast$density$Realized)
RMSEgvalSGED_ETH_wo <- RMSE(garchsigSGED_ETH_wo, realval_ETH_wo)

garchsigSST_ETH_wo <- CrCu_ETH_Roll5_wo@forecast$density$Sigma
realval_ETH_wo <- abs(CrCu_ETH_Roll5_wo@forecast$density$Realized)
RMSEgvalSST_ETH_wo <- RMSE(garchsigSST_ETH_wo, realval_ETH_wo)

garchsigJSU_ETH_wo <- CrCu_ETH_Roll7_wo@forecast$density$Sigma
realval_ETH_wo <- abs(CrCu_ETH_Roll7_wo@forecast$density$Realized)
RMSEgvalJSU_ETH_wo <- RMSE(garchsigJSU_ETH_wo, realval_ETH_wo)

garchsigNorm_ETH_wo <- CrCu_ETH_Roll8_wo@forecast$density$Sigma
realval_ETH_wo <- abs(CrCu_ETH_Roll8_wo@forecast$density$Realized)
RMSEgvalNorm_ETH_wo <- RMSE(garchsigNorm_ETH_wo, realval_ETH_wo)

RMSE_ETH_wo <- c(RMSEgvalSGED_ETH_wo, RMSEgvalSST_ETH_wo, RMSEgvalJSU_ETH_wo, RMSEgvalNorm_ETH_wo)

RMSE_ETH_wo <- matrix(data = RMSE_ETH_wo, nrow = 4)
colnames(RMSE_ETH_wo) <- c("Full Sample")
rownames(RMSE_ETH_wo) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: L I T E C O I N ::::::::##########

garchsigSGED_LTC_wo <- CrCu_LTC_Roll6_wo@forecast$density$Sigma
realval_LTC_wo <- abs(CrCu_LTC_Roll6_wo@forecast$density$Realized)
RMSEgvalSGED_LTC_wo <- RMSE(garchsigSGED_LTC_wo, realval_LTC_wo)

garchsigSST_LTC_wo <- CrCu_LTC_Roll5_wo@forecast$density$Sigma
realval_LTC_wo <- abs(CrCu_LTC_Roll5_wo@forecast$density$Realized)
RMSEgvalSST_LTC_wo <- RMSE(garchsigSST_LTC_wo, realval_LTC_wo)

garchsigJSU_LTC_wo <- CrCu_LTC_Roll7_wo@forecast$density$Sigma
realval_LTC_wo <- abs(CrCu_LTC_Roll7_wo@forecast$density$Realized)
RMSEgvalJSU_LTC_wo <- RMSE(garchsigJSU_LTC_wo, realval_LTC_wo)

garchsigNorm_LTC_wo <- CrCu_LTC_Roll8_wo@forecast$density$Sigma
realval_LTC_wo <- abs(CrCu_LTC_Roll8_wo@forecast$density$Realized)
RMSEgvalNorm_LTC_wo <- RMSE(garchsigNorm_LTC_wo, realval_LTC_wo)

RMSE_LTC_wo <- c(RMSEgvalSGED_LTC_wo, RMSEgvalSST_LTC_wo, RMSEgvalJSU_LTC_wo, RMSEgvalNorm_LTC_wo)

RMSE_LTC_wo <- matrix(data = RMSE_LTC_wo, nrow = 4)
colnames(RMSE_LTC_wo) <- c("Full Sample")
rownames(RMSE_LTC_wo) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: M O N E R O ::::::::##########

garchsigSGED_XMR_wo <- CrCu_XMR_Roll6_wo@forecast$density$Sigma
realval_XMR_wo <- abs(CrCu_XMR_Roll6_wo@forecast$density$Realized)
RMSEgvalSGED_XMR_wo <- RMSE(garchsigSGED_XMR_wo, realval_XMR_wo)

garchsigSST_XMR_wo <- CrCu_XMR_Roll5_wo@forecast$density$Sigma
realval_XMR_wo <- abs(CrCu_XMR_Roll5_wo@forecast$density$Realized)
RMSEgvalSST_XMR_wo <- RMSE(garchsigSST_XMR_wo, realval_XMR_wo)

garchsigJSU_XMR_wo <- CrCu_XMR_Roll7_wo@forecast$density$Sigma
realval_XMR_wo <- abs(CrCu_XMR_Roll7_wo@forecast$density$Realized)
RMSEgvalJSU_XMR_wo <- RMSE(garchsigJSU_XMR_wo, realval_XMR_wo)

garchsigNorm_XMR_wo <- CrCu_XMR_Roll8_wo@forecast$density$Sigma
realval_XMR_wo <- abs(CrCu_XMR_Roll8_wo@forecast$density$Realized)
RMSEgvalNorm_XMR_wo <- RMSE(garchsigNorm_XMR_wo, realval_XMR_wo)

RMSE_XMR_wo <- c(RMSEgvalSGED_XMR_wo, RMSEgvalSST_XMR_wo, RMSEgvalJSU_XMR_wo, RMSEgvalNorm_XMR_wo)

RMSE_XMR_wo <- matrix(data = RMSE_XMR_wo, nrow = 4)
colnames(RMSE_XMR_wo) <- c("Full Sample")
rownames(RMSE_XMR_wo) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: E T H E R E U M  C L A S S I C ::::::::##########

garchsigSGED_ETC_wo <- CrCu_ETC_Roll6_wo@forecast$density$Sigma
realval_ETC_wo <- abs(CrCu_ETC_Roll6_wo@forecast$density$Realized)
RMSEgvalSGED_ETC_wo <- RMSE(garchsigSGED_ETC_wo, realval_ETC_wo)

garchsigSST_ETC_wo <- CrCu_ETC_Roll5_wo@forecast$density$Sigma
realval_ETC_wo <- abs(CrCu_ETC_Roll5_wo@forecast$density$Realized)
RMSEgvalSST_ETC_wo <- RMSE(garchsigSST_ETC_wo, realval_ETC_wo)

garchsigJSU_ETC_wo <- CrCu_ETC_Roll7_wo@forecast$density$Sigma
realval_ETC_wo <- abs(CrCu_ETC_Roll7_wo@forecast$density$Realized)
RMSEgvalJSU_ETC_wo <- RMSE(garchsigJSU_ETC_wo, realval_ETC_wo)

garchsigNorm_ETC_wo <- CrCu_ETC_Roll8_wo@forecast$density$Sigma
realval_ETC_wo <- abs(CrCu_ETC_Roll8_wo@forecast$density$Realized)
RMSEgvalNorm_ETC_wo <- RMSE(garchsigNorm_ETC_wo, realval_ETC_wo)

RMSE_ETC_wo <- c(RMSEgvalSGED_ETC_wo, RMSEgvalSST_ETC_wo, RMSEgvalJSU_ETC_wo, RMSEgvalNorm_ETC_wo)

RMSE_ETC_wo <- matrix(data = RMSE_ETC_wo, nrow = 4)
colnames(RMSE_ETC_wo) <- c("Full Sample")
rownames(RMSE_ETC_wo) <- c("SGED", "SST", "JSU", "Normal")

RMSE_crypto_wo <- rbind("BTC", round(RMSE_BTC_wo,4),
                        "ETH", round(RMSE_ETH_wo,4),
                        "LTC", round(RMSE_LTC_wo,4),
                        "XMR", round(RMSE_XMR_wo,4),
                        "ETC", round(RMSE_ETC_wo,4))

View(RMSE_crypto_wo)

xtable(RMSE_crypto_wo)

##### :::::::: Plot Value at Risk ::::::: ######
#Full Sample

Z_Pearson_BTC_01_wo   <- qpearson(0.01,CrCu_BTC_Dist8_wo)
BTC_Holdout_VaR_01_wo   <- BTC_Holdout_Mu_wo + BTC_Holdout_Sigma_wo*Z_Pearson_BTC_01_wo

VR1_BTC_wo <- data.frame(CrCu_BTC_Roll8_wo@forecast$VaR)
VR2_BTC_wo <- data.frame(CrCu_BTC_Roll5_wo@forecast$VaR)
VR3_BTC_wo <- data.frame(CrCu_BTC_Roll6_wo@forecast$VaR)
VR4_BTC_wo <- data.frame(CrCu_BTC_Roll7_wo@forecast$VaR)
VR5_BTC_wo <- data.frame(BTC_Holdout_VaR_01_wo)
VR_Date_BTC_wo <- CrCu_BTC$Crypto_Date[(length(CrCu_BTC_xts)-349):length(CrCu_BTC_xts)]

Z_Pearson_ETH_01_wo   <- qpearson(0.01,CrCu_ETH_Dist8_wo)
ETH_Holdout_VaR_01_wo   <- ETH_Holdout_Mu_wo + ETH_Holdout_Sigma_wo*Z_Pearson_ETH_01_wo

VR1_ETH_wo <- data.frame(CrCu_ETH_Roll8_wo@forecast$VaR)
VR2_ETH_wo <- data.frame(CrCu_ETH_Roll5_wo@forecast$VaR)
VR3_ETH_wo <- data.frame(CrCu_ETH_Roll6_wo@forecast$VaR)
VR4_ETH_wo <- data.frame(CrCu_ETH_Roll7_wo@forecast$VaR)
VR5_ETH_wo <- data.frame(ETH_Holdout_VaR_01_wo)
VR_Date_ETH_wo <- CrCu_ETH$Crypto_Date[(length(CrCu_ETH_xts)-349):length(CrCu_ETH_xts)]

Z_Pearson_LTC_01_wo   <- qpearson(0.01,CrCu_LTC_Dist8_wo)
LTC_Holdout_VaR_01_wo   <- LTC_Holdout_Mu_wo + LTC_Holdout_Sigma_wo*Z_Pearson_LTC_01_wo

VR1_LTC_wo <- data.frame(CrCu_LTC_Roll8_wo@forecast$VaR)
VR2_LTC_wo <- data.frame(CrCu_LTC_Roll5_wo@forecast$VaR)
VR3_LTC_wo <- data.frame(CrCu_LTC_Roll6_wo@forecast$VaR)
VR4_LTC_wo <- data.frame(CrCu_LTC_Roll7_wo@forecast$VaR)
VR5_LTC_wo <- data.frame(LTC_Holdout_VaR_01_wo)
VR_Date_LTC_wo <- CrCu_LTC$Crypto_Date[(length(CrCu_LTC_xts)-349):length(CrCu_LTC_xts)]

Z_Pearson_XMR_01_wo   <- qpearson(0.01,CrCu_XMR_Dist8_wo)
XMR_Holdout_VaR_01_wo   <- XMR_Holdout_Mu_wo + XMR_Holdout_Sigma_wo*Z_Pearson_XMR_01_wo

VR1_XMR_wo <- data.frame(CrCu_XMR_Roll8_wo@forecast$VaR)
VR2_XMR_wo <- data.frame(CrCu_XMR_Roll5_wo@forecast$VaR)
VR3_XMR_wo <- data.frame(CrCu_XMR_Roll6_wo@forecast$VaR)
VR4_XMR_wo <- data.frame(CrCu_XMR_Roll7_wo@forecast$VaR)
VR5_XMR_wo <- data.frame(XMR_Holdout_VaR_01_wo)
VR_Date_XMR_wo <- CrCu_XMR$Crypto_Date[(length(CrCu_XMR_xts)-349):length(CrCu_XMR_xts)]

Z_Pearson_ETC_01_wo   <- qpearson(0.01,CrCu_ETC_Dist8_wo)
ETC_Holdout_VaR_01_wo   <- ETC_Holdout_Mu_wo + ETC_Holdout_Sigma_wo*Z_Pearson_ETC_01_wo

VR1_ETC_wo <- data.frame(CrCu_ETC_Roll8_wo@forecast$VaR)
VR2_ETC_wo <- data.frame(CrCu_ETC_Roll5_wo@forecast$VaR)
VR3_ETC_wo <- data.frame(CrCu_ETC_Roll6_wo@forecast$VaR)
VR4_ETC_wo <- data.frame(CrCu_ETC_Roll7_wo@forecast$VaR)
VR5_ETC_wo <- data.frame(ETC_Holdout_VaR_01_wo)
VR_Date_ETC_wo <- CrCu_ETC$Crypto_Date[(length(CrCu_ETC_xts)-349):length(CrCu_ETC_xts)]

plotdata_VaR_BTC_01_wo <- data.frame(VR_Date_BTC_wo, "BTC", VR1_BTC_wo[,4], VR1_BTC_wo[,1], VR2_BTC_wo[,1], VR3_BTC_wo[,1], VR4_BTC_wo[,1], VR5_BTC_wo[,1])
colnames(plotdata_VaR_BTC_01_wo) <- c("Date", "Currency", "WReturns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_ETH_01_wo <- data.frame(VR_Date_ETH_wo, "ETH", VR1_ETH_wo[,4], VR1_ETH_wo[,1], VR2_ETH_wo[,1], VR3_ETH_wo[,1], VR4_ETH_wo[,1], VR5_ETH_wo[,1])
colnames(plotdata_VaR_ETH_01_wo) <- c("Date", "Currency", "WReturns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_XMR_01_wo <- data.frame(VR_Date_XMR_wo, "XMR", VR1_XMR_wo[,4], VR1_XMR_wo[,1], VR2_XMR_wo[,1], VR3_XMR_wo[,1], VR4_XMR_wo[,1], VR5_XMR_wo[,1])
colnames(plotdata_VaR_XMR_01_wo) <- c("Date", "Currency", "WReturns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_LTC_01_wo <- data.frame(VR_Date_LTC_wo, "LTC", VR1_LTC_wo[,4], VR1_LTC_wo[,1], VR2_LTC_wo[,1], VR3_LTC_wo[,1], VR4_LTC_wo[,1], VR5_LTC_wo[,1])
colnames(plotdata_VaR_LTC_01_wo) <- c("Date", "Currency", "WReturns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_ETC_01_wo <- data.frame(VR_Date_ETC_wo, "ETC", VR1_ETC_wo[,4], VR1_ETC_wo[,1], VR2_ETC_wo[,1], VR3_ETC_wo[,1], VR4_ETC_wo[,1], VR5_ETC_wo[,1])
colnames(plotdata_VaR_ETC_01_wo) <- c("Date", "Currency", "WReturns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_S_01_wo <- rbind(plotdata_VaR_BTC_01_wo, plotdata_VaR_ETH_01_wo, plotdata_VaR_XMR_01_wo, plotdata_VaR_LTC_01_wo, plotdata_VaR_ETC_01_wo)
View(plotdata_VaR_S_01_wo)

FSVaR_G_01_wo <- ggplot(plotdata_VaR_S_01_wo, aes(x = plotdata_VaR_S_01_wo$Date)) +
  geom_line(aes(y = WReturns, colour = "Return")) +
  geom_line(aes(x = Date, y = Norm, colour = "Norm")) +
  geom_line(aes(x = Date, y = SST, color = "SST")) +
  geom_line(aes(x = Date, y = SGED, color = "SGED")) +
  geom_line(aes(x = Date, y = JSU, color = "JSU")) + 
  geom_line(aes(x = Date, y = PSIV, color = "PSIV")) +
  facet_grid(vars(plotdata_VaR_S_01_wo$Currency),scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~.) +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Norm", "SST", "SGED", "JSU", "PSIV"),
                     values = c("Return" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PSIV" = "black")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for entire data: without external regressors") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.key = element_rect(),
        legend.text = element_text(size = 15),
        legend.position = "bottom", 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 1))

ggsave(filename = file.path(FIG_DIR, "FSVaR_G_01_wo.eps"),
       plot = FSVaR_G_01_wo,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")








