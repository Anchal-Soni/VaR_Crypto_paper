# =============================================================================
# 06_gas_models.R
#
# Purpose : GAS models (Norm, ST, SST, AST, AST1) for full sample and SS1-SS3: fit,
#           rolling 1-day-ahead forecasts, 1% VaR, Kupiec/Christoffersen backtests,
#           MAPE/RMSE.
# Inputs  : CrCu_<coin>_xts, subsample objects and realised values from 03a-04
# Outputs : Fit_*, Roll_*, VaR_*, VaRBacktest_* objects
# Paper   : Table 4, Tables A.9-A.10 (via 12)
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

library(GAS)
### GAS specification ::::::::::::
#### ::::::::::::::: BTC ::::::::::::::: ####
###### Full Sample
DistInfo(DistLabel = NULL, N = 1, FULL = TRUE)
# NORM #
GASSpec_BTC_norm0 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_norm0 <- UniGASFit(GASSpec_BTC_norm0, CrCu_BTC_xts)
Forecast_BTC_norm0 <- UniGASFor(Fit_BTC_norm0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_norm0 <- UniGASRoll(CrCu_BTC_xts, GASSpec_BTC_norm0, ForecastLength = 350, RefitEvery = 50)
#Roll_BTC_norm0@Forecast$PointForecast[,2]
#Roll_BTC_norm0@Forecast$Coef
#Roll_BTC_norm0@Testing$PitTest
sigma_BTC_norm0 <- Roll_BTC_norm0@Forecast$PointForecast[,2]
MAPE_BTC_norm0 <- MAPE(sigma_BTC_norm0,realval_BTC)
alpha <- 0.01
VaR_BTC_norm0 <- quantile(Roll_BTC_norm0, probs = alpha) 
ES_BTC_norm0 <- ES(Roll_BTC_norm0, probs = alpha)
VaRBacktest_BTC_norm0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_norm0, alpha = alpha)
QL_BTC_norm0 <- round(VaRBacktest_BTC_norm0$Loss$Loss / VaRBacktest_BTC_norm0$Loss$Loss, 2)
#plot(Roll_BTC_norm0)

# STD #
GASSpec_BTC_std0 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_std0 <- UniGASFit(GASSpec_BTC_std0, CrCu_BTC_xts)
Forecast_BTC_std0 <- UniGASFor(Fit_BTC_std0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_std0 <- UniGASRoll(CrCu_BTC_xts, GASSpec_BTC_std0, ForecastLength = 350, RefitEvery = 50)
#Roll_BTC_std0@Forecast$PointForecast[,2]
#Roll_BTC_std0@Forecast$Coef
#Roll_BTC_std0@Testing$PitTest
sigma_BTC_std0 <- Roll_BTC_std0@Forecast$PointForecast[,2]
MAPE_BTC_std0 <- MAPE(sigma_BTC_std0,realval_BTC)
alpha <- 0.01
VaR_BTC_std0 <- quantile(Roll_BTC_std0, probs = alpha) 
ES_BTC_std0 <- ES(Roll_BTC_std0, probs = alpha)
VaRBacktest_BTC_std0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_std0, alpha = alpha)
QL_BTC_std0 <- round(VaRBacktest_BTC_std0$Loss$Loss / VaRBacktest_BTC_norm0$Loss$Loss, 2)
#plot(Roll_BTC_std0)

# SSTD #
GASSpec_BTC_sstd0 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = TRUE))
Fit_BTC_sstd0 <- UniGASFit(GASSpec_BTC_sstd0, CrCu_BTC_xts)
Forecast_BTC_sstd0 <- UniGASFor(Fit_BTC_sstd0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_sstd0 <- UniGASRoll(CrCu_BTC_xts, GASSpec_BTC_sstd0, ForecastLength = 350, RefitEvery = 50)
#Roll_BTC_sstd0@Forecast$PointForecast[,2]
#Roll_BTC_sstd0@Forecast$Coef
#Roll_BTC_sstd0@Testing$PitTest
sigma_BTC_sstd0 <- Roll_BTC_sstd0@Forecast$PointForecast[,2]
MAPE_BTC_sstd0 <- MAPE(sigma_BTC_sstd0,realval_BTC)
alpha <- 0.01
VaR_BTC_sstd0 <- quantile(Roll_BTC_norm0, probs = alpha) 
ES_BTC_sstd0 <- ES(Roll_BTC_norm0, probs = alpha)
VaRBacktest_BTC_sstd0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_sstd0, alpha = alpha)
QL_BTC_sstd0 <- round(VaRBacktest_BTC_sstd0$Loss$Loss / VaRBacktest_BTC_norm0$Loss$Loss, 2)
#plot(Roll_BTC_sstd0)

# AST #
GASSpec_BTC_ast0 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_ast0 <- UniGASFit(GASSpec_BTC_ast0, CrCu_BTC_xts)
#Forecast_BTC_ast0 <- UniGASFor(Fit_BTC_ast0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_ast0 <- UniGASRoll(CrCu_BTC_xts, GASSpec_BTC_ast0, ForecastLength = 350, RefitEvery = 50)
#Roll_BTC_ast0@Forecast$PointForecast[,2]
#Roll_BTC_ast0@Forecast$Coef
#Roll_BTC_ast0@Testing$PitTest
sigma_BTC_ast0 <- Roll_BTC_ast0@Forecast$PointForecast[,2]
MAPE_BTC_ast0 <- MAPE(sigma_BTC_ast0,realval_BTC)
alpha <- 0.01
VaR_BTC_ast0 <- quantile(Roll_BTC_ast0, probs = alpha) 
ES_BTC_ast0 <- ES(Roll_BTC_ast0, probs = alpha)
VaRBacktest_BTC_ast0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_ast0, alpha = alpha)
QL_BTC_ast0 <- round(VaRBacktest_BTC_ast0$Loss$Loss / VaRBacktest_BTC_norm0$Loss$Loss, 2)
#plot(Roll_BTC_ast0)

# AST ONE #
GASSpec_BTC_astone0 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_astone0 <- UniGASFit(GASSpec_BTC_astone0, CrCu_BTC_xts)
#Forecast_BTC_astone0 <- UniGASFor(Fit_BTC_astone0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_astone0 <- UniGASRoll(CrCu_BTC_xts, GASSpec_BTC_astone0, ForecastLength = 350, RefitEvery = 50)
#Roll_BTC_astone0@Forecast$PointForecast[,2]
#Roll_BTC_astone0@Forecast$Coef
#Roll_BTC_astone0@Testing$PitTest
sigma_BTC_astone0 <- Roll_BTC_astone0@Forecast$PointForecast[,2]
MAPE_BTC_astone0 <- MAPE(sigma_BTC_astone0,realval_BTC)

alpha <- 0.01
VaR_BTC_astone0 <- quantile(Roll_BTC_astone0, probs = alpha) 
ES_BTC_astone0 <- ES(Roll_BTC_astone0, probs = alpha)
VaRBacktest_BTC_astone0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_astone0, alpha = alpha)
QL_BTC_astone0 <- round(VaRBacktest_BTC_astone0$Loss$Loss / VaRBacktest_BTC_norm0$Loss$Loss, 2)
#plot(Roll_BTC_astone0)

############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 1 ######

# NORM #
GASSpec_BTC_norm1 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_norm1 <- UniGASFit(GASSpec_BTC_norm1, CrCu_BTC_sub1)
Forecast_BTC_norm1 <- UniGASFor(Fit_BTC_norm1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_norm1 <- UniGASRoll(CrCu_BTC_sub1, GASSpec_BTC_norm1, ForecastLength = 60, RefitEvery = 20)
#Roll_BTC_norm1@Forecast$PointForecast[,2]
#Roll_BTC_norm1@Forecast$Coef
#Roll_BTC_norm1@Testing$PitTest
sigma_BTC_norm1 <- Roll_BTC_norm1@Forecast$PointForecast[,2]
MAPE_BTC_norm1 <- MAPE(sigma_BTC_norm1,realval_SGED1_BTC)
alpha <- 0.01
VaR_BTC_norm1 <- quantile(Roll_BTC_norm1, probs = alpha) 
ES_BTC_norm1 <- ES(Roll_BTC_norm1, probs = alpha)
VaRBacktest_BTC_norm1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_norm1, alpha = alpha)
QL_BTC_norm1 <- round(VaRBacktest_BTC_norm1$Loss$Loss / VaRBacktest_BTC_norm1$Loss$Loss, 2)
#plot(Roll_BTC_norm1)

# STD #
GASSpec_BTC_std1 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_std1 <- UniGASFit(GASSpec_BTC_std1, CrCu_BTC_sub1)
Forecast_BTC_std1 <- UniGASFor(Fit_BTC_std1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_std1 <- UniGASRoll(CrCu_BTC_sub1, GASSpec_BTC_std1, ForecastLength = 60, RefitEvery = 20)
#Roll_BTC_std1@Forecast$PointForecast[,2]
#Roll_BTC_std1@Forecast$Coef
#Roll_BTC_std1@Testing$PitTest
sigma_BTC_std1 <- Roll_BTC_std1@Forecast$PointForecast[,2]
MAPE_BTC_std1 <- MAPE(sigma_BTC_std1,realval_SGED1_BTC)
alpha <- 0.01
VaR_BTC_std1 <- quantile(Roll_BTC_std1, probs = alpha) 
ES_BTC_std1 <- ES(Roll_BTC_std1, probs = alpha)
VaRBacktest_BTC_std1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_std1, alpha = alpha)
QL_BTC_std1 <- round(VaRBacktest_BTC_std1$Loss$Loss / VaRBacktest_BTC_norm1$Loss$Loss, 2)
#plot(Roll_BTC_std1)

# SSTD #
GASSpec_BTC_sstd1 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_sstd1 <- UniGASFit(GASSpec_BTC_sstd1, CrCu_BTC_sub1)
Forecast_BTC_sstd1 <- UniGASFor(Fit_BTC_sstd1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_sstd1 <- UniGASRoll(CrCu_BTC_sub1, GASSpec_BTC_sstd1, ForecastLength = 60, RefitEvery = 20)
#Roll_BTC_sstd1@Forecast$PointForecast[,2]
#Roll_BTC_sstd1@Forecast$Coef
#Roll_BTC_sstd1@Testing$PitTest
sigma_BTC_sstd1 <- Roll_BTC_sstd1@Forecast$PointForecast[,2]
MAPE_BTC_sstd1 <- MAPE(sigma_BTC_sstd1,realval_SGED1_BTC)
alpha <- 0.01
VaR_BTC_sstd1 <- quantile(Roll_BTC_sstd1, probs = alpha) 
ES_BTC_sstd1 <- ES(Roll_BTC_sstd1, probs = alpha)
VaRBacktest_BTC_sstd1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_sstd1, alpha = alpha)
QL_BTC_sstd1 <- round(VaRBacktest_BTC_sstd1$Loss$Loss / VaRBacktest_BTC_norm1$Loss$Loss, 2)
#plot(Roll_BTC_sstd1)

# AST #
GASSpec_BTC_ast1 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_ast1 <- UniGASFit(GASSpec_BTC_ast1, CrCu_BTC_sub1)
Forecast_BTC_ast1 <- UniGASFor(Fit_BTC_ast1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_ast1 <- UniGASRoll(CrCu_BTC_sub1, GASSpec_BTC_ast1, ForecastLength = 60, RefitEvery = 20)
#Roll_BTC_ast1@Forecast$PointForecast[,2]
#Roll_BTC_ast1@Forecast$Coef
#Roll_BTC_ast1@Testing$PitTest
sigma_BTC_ast1 <- Roll_BTC_ast1@Forecast$PointForecast[,2]
MAPE_BTC_ast1 <- MAPE(sigma_BTC_ast1,realval_SGED1_BTC)
alpha <- 0.01
VaR_BTC_ast1 <- quantile(Roll_BTC_ast1, probs = alpha) 
ES_BTC_ast1 <- ES(Roll_BTC_ast1, probs = alpha)
VaRBacktest_BTC_ast1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_ast1, alpha = alpha)
QL_BTC_ast1 <- round(VaRBacktest_BTC_ast1$Loss$Loss / VaRBacktest_BTC_norm1$Loss$Loss, 2)
#plot(Roll_BTC_ast1)

# AST ONE#
GASSpec_BTC_astone1 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_astone1 <- UniGASFit(GASSpec_BTC_astone1, CrCu_BTC_sub1)
Forecast_BTC_astone1 <- UniGASFor(Fit_BTC_astone1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_astone1 <- UniGASRoll(CrCu_BTC_sub1, GASSpec_BTC_astone1, ForecastLength = 60, RefitEvery = 20)
#Roll_BTC_astone1@Forecast$PointForecast[,2]
#Roll_BTC_astone1@Forecast$Coef
#Roll_BTC_astone1@Testing$PitTest
sigma_BTC_astone1 <- Roll_BTC_astone1@Forecast$PointForecast[,2]
MAPE_BTC_astone1 <- MAPE(sigma_BTC_astone1,realval_SGED1_BTC)
alpha <- 0.01
VaR_BTC_astone1 <- quantile(Roll_BTC_astone1, probs = alpha) 
ES_BTC_astone1 <- ES(Roll_BTC_astone1, probs = alpha)
VaRBacktest_BTC_astone1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_astone1, alpha = alpha)
QL_BTC_astone1 <- round(VaRBacktest_BTC_astone1$Loss$Loss / VaRBacktest_BTC_norm1$Loss$Loss, 2)
#plot(Roll_BTC_astone1)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 2 ######

# NORM #
GASSpec_BTC_norm2 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_norm2 <- UniGASFit(GASSpec_BTC_norm2, CrCu_BTC_sub2)
Forecast_BTC_norm2 <- UniGASFor(Fit_BTC_norm2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_norm2 <- UniGASRoll(CrCu_BTC_sub2, GASSpec_BTC_norm2, ForecastLength = 200, RefitEvery = 20)
#Roll_BTC_norm2@Forecast$PointForecast[,2]
#Roll_BTC_norm2@Forecast$Coef
#Roll_BTC_norm2@Testing$PitTest
sigma_BTC_norm2 <- Roll_BTC_norm2@Forecast$PointForecast[,2]
MAPE_BTC_norm2 <- MAPE(sigma_BTC_norm2,realval_SGED2_BTC)
alpha <- 0.01
VaR_BTC_norm2 <- quantile(Roll_BTC_norm2, probs = alpha) 
ES_BTC_norm2 <- ES(Roll_BTC_norm2, probs = alpha)
VaRBacktest_BTC_norm2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_norm2, alpha = alpha)
QL_BTC_norm2 <- round(VaRBacktest_BTC_norm2$Loss$Loss / VaRBacktest_BTC_norm2$Loss$Loss, 2)
#plot(Roll_BTC_norm2)

# STD #
GASSpec_BTC_std2 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_std2 <- UniGASFit(GASSpec_BTC_std2, CrCu_BTC_sub2)
Forecast_BTC_std2 <- UniGASFor(Fit_BTC_std2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_std2 <- UniGASRoll(CrCu_BTC_sub2, GASSpec_BTC_std2, ForecastLength = 200, RefitEvery = 20)
#Roll_BTC_std2@Forecast$PointForecast[,2]
#Roll_BTC_std2@Forecast$Coef
#Roll_BTC_std2@Testing$PitTest
sigma_BTC_std2 <- Roll_BTC_std2@Forecast$PointForecast[,2]
MAPE_BTC_std2 <- MAPE(sigma_BTC_std2,realval_SGED2_BTC)
alpha <- 0.01
VaR_BTC_std2 <- quantile(Roll_BTC_std2, probs = alpha) 
ES_BTC_std2 <- ES(Roll_BTC_std2, probs = alpha)
VaRBacktest_BTC_std2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_std2, alpha = alpha)
QL_BTC_std2 <- round(VaRBacktest_BTC_std2$Loss$Loss / VaRBacktest_BTC_norm2$Loss$Loss, 2)
#plot(Roll_BTC_std2)

# SSTD #
GASSpec_BTC_sstd2 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_sstd2 <- UniGASFit(GASSpec_BTC_sstd2, CrCu_BTC_sub2)
Forecast_BTC_sstd2 <- UniGASFor(Fit_BTC_sstd2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_sstd2 <- UniGASRoll(CrCu_BTC_sub2, GASSpec_BTC_sstd2, ForecastLength = 200, RefitEvery = 20)
#Roll_BTC_sstd2@Forecast$PointForecast[,2]
#Roll_BTC_sstd2@Forecast$Coef
#Roll_BTC_sstd2@Testing$PitTest
sigma_BTC_sstd2 <- Roll_BTC_sstd2@Forecast$PointForecast[,2]
MAPE_BTC_sstd2 <- MAPE(sigma_BTC_sstd2,realval_SGED2_BTC)
alpha <- 0.01
VaR_BTC_sstd2 <- quantile(Roll_BTC_sstd2, probs = alpha) 
ES_BTC_sstd2 <- ES(Roll_BTC_sstd2, probs = alpha)
VaRBacktest_BTC_sstd2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_sstd2, alpha = alpha)
QL_BTC_sstd2 <- round(VaRBacktest_BTC_sstd2$Loss$Loss / VaRBacktest_BTC_norm2$Loss$Loss, 2)
#plot(Roll_BTC_sstd2)

# AST #
GASSpec_BTC_ast2 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_ast2 <- UniGASFit(GASSpec_BTC_ast2, CrCu_BTC_sub2)
Forecast_BTC_ast2 <- UniGASFor(Fit_BTC_ast2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_ast2 <- UniGASRoll(CrCu_BTC_sub2, GASSpec_BTC_ast2, ForecastLength = 200, RefitEvery = 20)
#Roll_BTC_ast2@Forecast$PointForecast[,2]
#Roll_BTC_ast2@Forecast$Coef
#Roll_BTC_ast2@Testing$PitTest
sigma_BTC_ast2 <- Roll_BTC_ast2@Forecast$PointForecast[,2]
MAPE_BTC_ast2 <- MAPE(sigma_BTC_ast2,realval_SGED2_BTC)
alpha <- 0.01
VaR_BTC_ast2 <- quantile(Roll_BTC_ast2, probs = alpha) 
ES_BTC_ast2 <- ES(Roll_BTC_ast2, probs = alpha)
VaRBacktest_BTC_ast2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_ast2, alpha = alpha)
QL_BTC_ast2 <- round(VaRBacktest_BTC_ast2$Loss$Loss / VaRBacktest_BTC_norm2$Loss$Loss, 2)
#plot(Roll_BTC_ast2)

# AST ONE#
GASSpec_BTC_astone2 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_astone2 <- UniGASFit(GASSpec_BTC_astone2, CrCu_BTC_sub2)
Forecast_BTC_astone2 <- UniGASFor(Fit_BTC_astone2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_astone2 <- UniGASRoll(CrCu_BTC_sub2, GASSpec_BTC_astone2, ForecastLength = 200, RefitEvery = 20)
#Roll_BTC_astone2@Forecast$PointForecast[,2]
#Roll_BTC_astone2@Forecast$Coef
#Roll_BTC_astone2@Testing$PitTest
sigma_BTC_astone2 <- Roll_BTC_astone2@Forecast$PointForecast[,2]
MAPE_BTC_astone2 <- MAPE(sigma_BTC_astone2,realval_SGED2_BTC)
alpha <- 0.01
VaR_BTC_astone2 <- quantile(Roll_BTC_astone2, probs = alpha) 
ES_BTC_astone2 <- ES(Roll_BTC_astone2, probs = alpha)
VaRBacktest_BTC_astone2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_astone2, alpha = alpha)
QL_BTC_astone2 <- round(VaRBacktest_BTC_astone2$Loss$Loss / VaRBacktest_BTC_norm2$Loss$Loss, 2)
#plot(Roll_BTC_astone2)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 3 ######

# NORM #
GASSpec_BTC_norm3 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_norm3 <- UniGASFit(GASSpec_BTC_norm3, CrCu_BTC_sub3)
Forecast_BTC_norm3 <- UniGASFor(Fit_BTC_norm3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_norm3 <- UniGASRoll(CrCu_BTC_sub3, GASSpec_BTC_norm3, ForecastLength = 100, RefitEvery = 20)
#Roll_BTC_norm3@Forecast$PointForecast[,2]
#Roll_BTC_norm3@Forecast$Coef
#Roll_BTC_norm3@Testing$PitTest
sigma_BTC_norm3 <- Roll_BTC_norm3@Forecast$PointForecast[,2]
MAPE_BTC_norm3 <- MAPE(sigma_BTC_norm3,realval_SGED3_BTC)
alpha <- 0.01
VaR_BTC_norm3 <- quantile(Roll_BTC_norm3, probs = alpha) 
ES_BTC_norm3 <- ES(Roll_BTC_norm3, probs = alpha)
VaRBacktest_BTC_norm3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_norm3, alpha = alpha)
QL_BTC_norm3 <- round(VaRBacktest_BTC_norm3$Loss$Loss / VaRBacktest_BTC_norm3$Loss$Loss, 2)
#plot(Roll_BTC_norm3)

# STD #
GASSpec_BTC_std3 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_std3 <- UniGASFit(GASSpec_BTC_std3, CrCu_BTC_sub3)
Forecast_BTC_std3 <- UniGASFor(Fit_BTC_std3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_std3 <- UniGASRoll(CrCu_BTC_sub3, GASSpec_BTC_std3, ForecastLength = 100, RefitEvery = 20)
#Roll_BTC_std3@Forecast$PointForecast[,2]
#Roll_BTC_std3@Forecast$Coef
#Roll_BTC_std3@Testing$PitTest
sigma_BTC_std3 <- Roll_BTC_std3@Forecast$PointForecast[,2]
MAPE_BTC_std3 <- MAPE(sigma_BTC_std3,realval_SGED3_BTC)
alpha <- 0.01
VaR_BTC_std3 <- quantile(Roll_BTC_std3, probs = alpha) 
ES_BTC_std3 <- ES(Roll_BTC_std3, probs = alpha)
VaRBacktest_BTC_std3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_std3, alpha = alpha)
QL_BTC_std3 <- round(VaRBacktest_BTC_std3$Loss$Loss / VaRBacktest_BTC_norm3$Loss$Loss, 2)
#plot(Roll_BTC_std3)

# SSTD #
GASSpec_BTC_sstd3 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_sstd3 <- UniGASFit(GASSpec_BTC_sstd3, CrCu_BTC_sub3)
Forecast_BTC_sstd3 <- UniGASFor(Fit_BTC_sstd3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_sstd3 <- UniGASRoll(CrCu_BTC_sub3, GASSpec_BTC_sstd3, ForecastLength = 100, RefitEvery = 20)
#Roll_BTC_sstd3@Forecast$PointForecast[,2]
#Roll_BTC_sstd3@Forecast$Coef
#Roll_BTC_sstd3@Testing$PitTest
sigma_BTC_sstd3 <- Roll_BTC_sstd3@Forecast$PointForecast[,2]
MAPE_BTC_sstd3 <- MAPE(sigma_BTC_sstd3,realval_SGED3_BTC)
alpha <- 0.01
VaR_BTC_sstd3 <- quantile(Roll_BTC_sstd3, probs = alpha) 
ES_BTC_sstd3 <- ES(Roll_BTC_sstd3, probs = alpha)
VaRBacktest_BTC_sstd3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_sstd3, alpha = alpha)
QL_BTC_sstd3 <- round(VaRBacktest_BTC_sstd3$Loss$Loss / VaRBacktest_BTC_norm3$Loss$Loss, 2)
#plot(Roll_BTC_sstd3)

# AST #
GASSpec_BTC_ast3 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_ast3 <- UniGASFit(GASSpec_BTC_ast3, CrCu_BTC_sub3)
Forecast_BTC_ast3 <- UniGASFor(Fit_BTC_ast3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_ast3 <- UniGASRoll(CrCu_BTC_sub3, GASSpec_BTC_ast3, ForecastLength = 100, RefitEvery = 20)
#Roll_BTC_ast3@Forecast$PointForecast[,2]
#Roll_BTC_ast3@Forecast$Coef
#Roll_BTC_ast3@Testing$PitTest
sigma_BTC_ast3 <- Roll_BTC_ast3@Forecast$PointForecast[,2]
MAPE_BTC_ast3 <- MAPE(sigma_BTC_ast3,realval_SGED3_BTC)
alpha <- 0.01
VaR_BTC_ast3 <- quantile(Roll_BTC_ast3, probs = alpha) 
ES_BTC_ast3 <- ES(Roll_BTC_ast3, probs = alpha)
VaRBacktest_BTC_ast3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_ast3, alpha = alpha)
QL_BTC_ast3 <- round(VaRBacktest_BTC_ast3$Loss$Loss / VaRBacktest_BTC_norm3$Loss$Loss, 2)
#plot(Roll_BTC_ast3)

# AST ONE#
GASSpec_BTC_astone3 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_BTC_astone3 <- UniGASFit(GASSpec_BTC_astone3, CrCu_BTC_sub3)
Forecast_BTC_astone3 <- UniGASFor(Fit_BTC_astone3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_BTC_astone3 <- UniGASRoll(CrCu_BTC_sub3, GASSpec_BTC_astone3, ForecastLength = 100, RefitEvery = 20)
#Roll_BTC_astone3@Forecast$PointForecast[,2]
#Roll_BTC_astone3@Forecast$Coef
#Roll_BTC_astone3@Testing$PitTest
sigma_BTC_astone3 <- Roll_BTC_astone3@Forecast$PointForecast[,2]
MAPE_BTC_astone3 <- MAPE(sigma_BTC_astone3,realval_SGED3_BTC)
alpha <- 0.01
VaR_BTC_astone3 <- quantile(Roll_BTC_astone3, probs = alpha) 
ES_BTC_astone3 <- ES(Roll_BTC_astone3, probs = alpha)
VaRBacktest_BTC_astone3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_astone3, alpha = alpha)
QL_BTC_astone3 <- round(VaRBacktest_BTC_astone3$Loss$Loss / VaRBacktest_BTC_norm3$Loss$Loss, 2)
#plot(Roll_BTC_astone3)




#### ::::::::::::::: ETH ::::::::::::::: ####
###### Full Sample

# NORM #
GASSpec_ETH_norm0 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_norm0 <- UniGASFit(GASSpec_ETH_norm0, CrCu_ETH_xts)
Forecast_ETH_norm0 <- UniGASFor(Fit_ETH_norm0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_norm0 <- UniGASRoll(CrCu_ETH_xts, GASSpec_ETH_norm0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETH_norm0@Forecast$PointForecast[,2]
#Roll_ETH_norm0@Forecast$Coef
#Roll_ETH_norm0@Testing$PitTest
sigma_ETH_norm0 <- Roll_ETH_norm0@Forecast$PointForecast[,2]
MAPE_ETH_norm0 <- MAPE(sigma_ETH_norm0,realval_ETH)
alpha <- 0.01
VaR_ETH_norm0 <- quantile(Roll_ETH_norm0, probs = alpha) 
ES_ETH_norm0 <- ES(Roll_ETH_norm0, probs = alpha)
VaRBacktest_ETH_norm0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_norm0, alpha = alpha)
QL_ETH_norm0 <- round(VaRBacktest_ETH_norm0$Loss$Loss / VaRBacktest_ETH_norm0$Loss$Loss, 2)
#plot(Roll_ETH_norm0)

# STD #
GASSpec_ETH_std0 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_std0 <- UniGASFit(GASSpec_ETH_std0, CrCu_ETH_xts)
Forecast_ETH_std0 <- UniGASFor(Fit_ETH_std0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_std0 <- UniGASRoll(CrCu_ETH_xts, GASSpec_ETH_std0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETH_std0@Forecast$PointForecast[,2]
#Roll_ETH_std0@Forecast$Coef
#Roll_ETH_std0@Testing$PitTest
sigma_ETH_std0 <- Roll_ETH_std0@Forecast$PointForecast[,2]
MAPE_ETH_std0 <- MAPE(sigma_ETH_std0,realval_ETH)
alpha <- 0.01
VaR_ETH_std0 <- quantile(Roll_ETH_std0, probs = alpha) 
ES_ETH_std0 <- ES(Roll_ETH_std0, probs = alpha)
VaRBacktest_ETH_std0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_std0, alpha = alpha)
QL_ETH_std0 <- round(VaRBacktest_ETH_std0$Loss$Loss / VaRBacktest_ETH_norm0$Loss$Loss, 2)
#plot(Roll_ETH_std0)

# SSTD #
GASSpec_ETH_sstd0 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_sstd0 <- UniGASFit(GASSpec_ETH_sstd0, CrCu_ETH_xts)
Forecast_ETH_sstd0 <- UniGASFor(Fit_ETH_sstd0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_sstd0 <- UniGASRoll(CrCu_ETH_xts, GASSpec_ETH_sstd0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETH_sstd0@Forecast$PointForecast[,2]
#Roll_ETH_sstd0@Forecast$Coef
#Roll_ETH_sstd0@Testing$PitTest
sigma_ETH_sstd0 <- Roll_ETH_sstd0@Forecast$PointForecast[,2]
MAPE_ETH_sstd0 <- MAPE(sigma_ETH_sstd0,realval_ETH)
alpha <- 0.01
VaR_ETH_sstd0 <- quantile(Roll_ETH_norm0, probs = alpha) 
ES_ETH_sstd0 <- ES(Roll_ETH_norm0, probs = alpha)
VaRBacktest_ETH_sstd0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_sstd0, alpha = alpha)
QL_ETH_sstd0 <- round(VaRBacktest_ETH_sstd0$Loss$Loss / VaRBacktest_ETH_norm0$Loss$Loss, 2)
#plot(Roll_ETH_sstd0)

# AST #
GASSpec_ETH_ast0 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_ast0 <- UniGASFit(GASSpec_ETH_ast0, CrCu_ETH_xts)
#Forecast_ETH_ast0 <- UniGASFor(Fit_ETH_ast0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_ast0 <- UniGASRoll(CrCu_ETH_xts, GASSpec_ETH_ast0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETH_ast0@Forecast$PointForecast[,2]
#Roll_ETH_ast0@Forecast$Coef
#Roll_ETH_ast0@Testing$PitTest
sigma_ETH_ast0 <- Roll_ETH_ast0@Forecast$PointForecast[,2]
MAPE_ETH_ast0 <- MAPE(sigma_ETH_ast0,realval_ETH)
alpha <- 0.01
VaR_ETH_ast0 <- quantile(Roll_ETH_ast0, probs = alpha) 
ES_ETH_ast0 <- ES(Roll_ETH_ast0, probs = alpha)
VaRBacktest_ETH_ast0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_ast0, alpha = alpha)
QL_ETH_ast0 <- round(VaRBacktest_ETH_ast0$Loss$Loss / VaRBacktest_ETH_norm0$Loss$Loss, 2)
#plot(Roll_ETH_ast0)

# AST ONE #
GASSpec_ETH_astone0 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_astone0 <- UniGASFit(GASSpec_ETH_astone0, CrCu_ETH_xts)
#Forecast_ETH_astone0 <- UniGASFor(Fit_ETH_astone0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_astone0 <- UniGASRoll(CrCu_ETH_xts, GASSpec_ETH_astone0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETH_astone0@Forecast$PointForecast[,2]
#Roll_ETH_astone0@Forecast$Coef
#Roll_ETH_astone0@Testing$PitTest
sigma_ETH_astone0 <- Roll_ETH_astone0@Forecast$PointForecast[,2]
MAPE_ETH_astone0 <- MAPE(sigma_ETH_astone0,realval_ETH)
alpha <- 0.01
VaR_ETH_astone0 <- quantile(Roll_ETH_astone0, probs = alpha) 
ES_ETH_astone0 <- ES(Roll_ETH_astone0, probs = alpha)
VaRBacktest_ETH_astone0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_astone0, alpha = alpha)
QL_ETH_astone0 <- round(VaRBacktest_ETH_astone0$Loss$Loss / VaRBacktest_ETH_norm0$Loss$Loss, 2)
#plot(Roll_ETH_astone0)

############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 1 ######

# NORM #
GASSpec_ETH_norm1 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_norm1 <- UniGASFit(GASSpec_ETH_norm1, CrCu_ETH_sub1)
Forecast_ETH_norm1 <- UniGASFor(Fit_ETH_norm1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_norm1 <- UniGASRoll(CrCu_ETH_sub1, GASSpec_ETH_norm1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETH_norm1@Forecast$PointForecast[,2]
#Roll_ETH_norm1@Forecast$Coef
#Roll_ETH_norm1@Testing$PitTest
sigma_ETH_norm1 <- Roll_ETH_norm1@Forecast$PointForecast[,2]
MAPE_ETH_norm1 <- MAPE(sigma_ETH_norm1,realval_SGED1_ETH)
alpha <- 0.01
VaR_ETH_norm1 <- quantile(Roll_ETH_norm1, probs = alpha) 
ES_ETH_norm1 <- ES(Roll_ETH_norm1, probs = alpha)
VaRBacktest_ETH_norm1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_norm1, alpha = alpha)
QL_ETH_norm1 <- round(VaRBacktest_ETH_norm1$Loss$Loss / VaRBacktest_ETH_norm1$Loss$Loss, 2)
#plot(Roll_ETH_norm1)

# STD #
GASSpec_ETH_std1 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_std1 <- UniGASFit(GASSpec_ETH_std1, CrCu_ETH_sub1)
Forecast_ETH_std1 <- UniGASFor(Fit_ETH_std1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_std1 <- UniGASRoll(CrCu_ETH_sub1, GASSpec_ETH_std1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETH_std1@Forecast$PointForecast[,2]
#Roll_ETH_std1@Forecast$Coef
#Roll_ETH_std1@Testing$PitTest
sigma_ETH_std1 <- Roll_ETH_std1@Forecast$PointForecast[,2]
MAPE_ETH_std1 <- MAPE(sigma_ETH_std1,realval_SGED1_ETH)
alpha <- 0.01
VaR_ETH_std1 <- quantile(Roll_ETH_std1, probs = alpha) 
ES_ETH_std1 <- ES(Roll_ETH_std1, probs = alpha)
VaRBacktest_ETH_std1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_std1, alpha = alpha)
QL_ETH_std1 <- round(VaRBacktest_ETH_std1$Loss$Loss / VaRBacktest_ETH_norm1$Loss$Loss, 2)
#plot(Roll_ETH_std1)

# SSTD #
GASSpec_ETH_sstd1 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_sstd1 <- UniGASFit(GASSpec_ETH_sstd1, CrCu_ETH_sub1)
Forecast_ETH_sstd1 <- UniGASFor(Fit_ETH_sstd1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_sstd1 <- UniGASRoll(CrCu_ETH_sub1, GASSpec_ETH_sstd1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETH_sstd1@Forecast$PointForecast[,2]
#Roll_ETH_sstd1@Forecast$Coef
#Roll_ETH_sstd1@Testing$PitTest
sigma_ETH_sstd1 <- Roll_ETH_sstd1@Forecast$PointForecast[,2]
MAPE_ETH_sstd1 <- MAPE(sigma_ETH_sstd1,realval_SGED1_ETH)
alpha <- 0.01
VaR_ETH_sstd1 <- quantile(Roll_ETH_sstd1, probs = alpha) 
ES_ETH_sstd1 <- ES(Roll_ETH_sstd1, probs = alpha)
VaRBacktest_ETH_sstd1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_sstd1, alpha = alpha)
QL_ETH_sstd1 <- round(VaRBacktest_ETH_sstd1$Loss$Loss / VaRBacktest_ETH_norm1$Loss$Loss, 2)
#plot(Roll_ETH_sstd1)

# AST #
GASSpec_ETH_ast1 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_ast1 <- UniGASFit(GASSpec_ETH_ast1, CrCu_ETH_sub1)
Forecast_ETH_ast1 <- UniGASFor(Fit_ETH_ast1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_ast1 <- UniGASRoll(CrCu_ETH_sub1, GASSpec_ETH_ast1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETH_ast1@Forecast$PointForecast[,2]
#Roll_ETH_ast1@Forecast$Coef
#Roll_ETH_ast1@Testing$PitTest
sigma_ETH_ast1 <- Roll_ETH_ast1@Forecast$PointForecast[,2]
MAPE_ETH_ast1 <- MAPE(sigma_ETH_ast1,realval_SGED1_ETH)
alpha <- 0.01
VaR_ETH_ast1 <- quantile(Roll_ETH_ast1, probs = alpha) 
ES_ETH_ast1 <- ES(Roll_ETH_ast1, probs = alpha)
VaRBacktest_ETH_ast1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_ast1, alpha = alpha)
QL_ETH_ast1 <- round(VaRBacktest_ETH_ast1$Loss$Loss / VaRBacktest_ETH_norm1$Loss$Loss, 2)
#plot(Roll_ETH_ast1)

# AST ONE#
GASSpec_ETH_astone1 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_astone1 <- UniGASFit(GASSpec_ETH_astone1, CrCu_ETH_sub1)
Forecast_ETH_astone1 <- UniGASFor(Fit_ETH_astone1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_astone1 <- UniGASRoll(CrCu_ETH_sub1, GASSpec_ETH_astone1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETH_astone1@Forecast$PointForecast[,2]
#Roll_ETH_astone1@Forecast$Coef
#Roll_ETH_astone1@Testing$PitTest
sigma_ETH_astone1 <- Roll_ETH_astone1@Forecast$PointForecast[,2]
MAPE_ETH_astone1 <- MAPE(sigma_ETH_astone1,realval_SGED1_ETH)
alpha <- 0.01
VaR_ETH_astone1 <- quantile(Roll_ETH_astone1, probs = alpha) 
ES_ETH_astone1 <- ES(Roll_ETH_astone1, probs = alpha)
VaRBacktest_ETH_astone1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_astone1, alpha = alpha)
QL_ETH_astone1 <- round(VaRBacktest_ETH_astone1$Loss$Loss / VaRBacktest_ETH_norm1$Loss$Loss, 2)
#plot(Roll_ETH_astone1)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 2 ######

# NORM #
GASSpec_ETH_norm2 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_norm2 <- UniGASFit(GASSpec_ETH_norm2, CrCu_ETH_sub2)
Forecast_ETH_norm2 <- UniGASFor(Fit_ETH_norm2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_norm2 <- UniGASRoll(CrCu_ETH_sub2, GASSpec_ETH_norm2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETH_norm2@Forecast$PointForecast[,2]
#Roll_ETH_norm2@Forecast$Coef
#Roll_ETH_norm2@Testing$PitTest
sigma_ETH_norm2 <- Roll_ETH_norm2@Forecast$PointForecast[,2]
MAPE_ETH_norm2 <- MAPE(sigma_ETH_norm2,realval_SGED2_ETH)
alpha <- 0.01
VaR_ETH_norm2 <- quantile(Roll_ETH_norm2, probs = alpha) 
ES_ETH_norm2 <- ES(Roll_ETH_norm2, probs = alpha)
VaRBacktest_ETH_norm2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_norm2, alpha = alpha)
QL_ETH_norm2 <- round(VaRBacktest_ETH_norm2$Loss$Loss / VaRBacktest_ETH_norm2$Loss$Loss, 2)
#plot(Roll_ETH_norm2)

# STD #
GASSpec_ETH_std2 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_std2 <- UniGASFit(GASSpec_ETH_std2, CrCu_ETH_sub2)
Forecast_ETH_std2 <- UniGASFor(Fit_ETH_std2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_std2 <- UniGASRoll(CrCu_ETH_sub2, GASSpec_ETH_std2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETH_std2@Forecast$PointForecast[,2]
#Roll_ETH_std2@Forecast$Coef
#Roll_ETH_std2@Testing$PitTest
sigma_ETH_std2 <- Roll_ETH_std2@Forecast$PointForecast[,2]
MAPE_ETH_std2 <- MAPE(sigma_ETH_std2,realval_SGED2_ETH)
alpha <- 0.01
VaR_ETH_std2 <- quantile(Roll_ETH_std2, probs = alpha) 
ES_ETH_std2 <- ES(Roll_ETH_std2, probs = alpha)
VaRBacktest_ETH_std2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_std2, alpha = alpha)
QL_ETH_std2 <- round(VaRBacktest_ETH_std2$Loss$Loss / VaRBacktest_ETH_norm2$Loss$Loss, 2)
#plot(Roll_ETH_std2)

# SSTD #
GASSpec_ETH_sstd2 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_sstd2 <- UniGASFit(GASSpec_ETH_sstd2, CrCu_ETH_sub2)
Forecast_ETH_sstd2 <- UniGASFor(Fit_ETH_sstd2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_sstd2 <- UniGASRoll(CrCu_ETH_sub2, GASSpec_ETH_sstd2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETH_sstd2@Forecast$PointForecast[,2]
#Roll_ETH_sstd2@Forecast$Coef
#Roll_ETH_sstd2@Testing$PitTest
sigma_ETH_sstd2 <- Roll_ETH_sstd2@Forecast$PointForecast[,2]
MAPE_ETH_sstd2 <- MAPE(sigma_ETH_sstd2,realval_SGED2_ETH)
alpha <- 0.01
VaR_ETH_sstd2 <- quantile(Roll_ETH_sstd2, probs = alpha) 
ES_ETH_sstd2 <- ES(Roll_ETH_sstd2, probs = alpha)
VaRBacktest_ETH_sstd2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_sstd2, alpha = alpha)
QL_ETH_sstd2 <- round(VaRBacktest_ETH_sstd2$Loss$Loss / VaRBacktest_ETH_norm2$Loss$Loss, 2)
#plot(Roll_ETH_sstd2)

# AST #
GASSpec_ETH_ast2 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_ast2 <- UniGASFit(GASSpec_ETH_ast2, CrCu_ETH_sub2)
Forecast_ETH_ast2 <- UniGASFor(Fit_ETH_ast2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_ast2 <- UniGASRoll(CrCu_ETH_sub2, GASSpec_ETH_ast2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETH_ast2@Forecast$PointForecast[,2]
#Roll_ETH_ast2@Forecast$Coef
#Roll_ETH_ast2@Testing$PitTest
sigma_ETH_ast2 <- Roll_ETH_ast2@Forecast$PointForecast[,2]
MAPE_ETH_ast2 <- MAPE(sigma_ETH_ast2,realval_SGED2_ETH)
alpha <- 0.01
VaR_ETH_ast2 <- quantile(Roll_ETH_ast2, probs = alpha) 
ES_ETH_ast2 <- ES(Roll_ETH_ast2, probs = alpha)
VaRBacktest_ETH_ast2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_ast2, alpha = alpha)
QL_ETH_ast2 <- round(VaRBacktest_ETH_ast2$Loss$Loss / VaRBacktest_ETH_norm2$Loss$Loss, 2)
#plot(Roll_ETH_ast2)

# AST ONE#
GASSpec_ETH_astone2 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_astone2 <- UniGASFit(GASSpec_ETH_astone2, CrCu_ETH_sub2)
Forecast_ETH_astone2 <- UniGASFor(Fit_ETH_astone2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_astone2 <- UniGASRoll(CrCu_ETH_sub2, GASSpec_ETH_astone2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETH_astone2@Forecast$PointForecast[,2]
#Roll_ETH_astone2@Forecast$Coef
#Roll_ETH_astone2@Testing$PitTest
sigma_ETH_astone2 <- Roll_ETH_astone2@Forecast$PointForecast[,2]
MAPE_ETH_astone2 <- MAPE(sigma_ETH_astone2,realval_SGED2_ETH)
alpha <- 0.01
VaR_ETH_astone2 <- quantile(Roll_ETH_astone2, probs = alpha) 
ES_ETH_astone2 <- ES(Roll_ETH_astone2, probs = alpha)
VaRBacktest_ETH_astone2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_astone2, alpha = alpha)
QL_ETH_astone2 <- round(VaRBacktest_ETH_astone2$Loss$Loss / VaRBacktest_ETH_norm2$Loss$Loss, 2)
#plot(Roll_ETH_astone2)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 3 ######

# NORM #
GASSpec_ETH_norm3 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_norm3 <- UniGASFit(GASSpec_ETH_norm3, CrCu_ETH_sub3)
Forecast_ETH_norm3 <- UniGASFor(Fit_ETH_norm3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_norm3 <- UniGASRoll(CrCu_ETH_sub3, GASSpec_ETH_norm3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETH_norm3@Forecast$PointForecast[,2]
#Roll_ETH_norm3@Forecast$Coef
#Roll_ETH_norm3@Testing$PitTest
sigma_ETH_norm3 <- Roll_ETH_norm3@Forecast$PointForecast[,2]
MAPE_ETH_norm3 <- MAPE(sigma_ETH_norm3,realval_SGED3_ETH)
alpha <- 0.01
VaR_ETH_norm3 <- quantile(Roll_ETH_norm3, probs = alpha) 
ES_ETH_norm3 <- ES(Roll_ETH_norm3, probs = alpha)
VaRBacktest_ETH_norm3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_norm3, alpha = alpha)
QL_ETH_norm3 <- round(VaRBacktest_ETH_norm3$Loss$Loss / VaRBacktest_ETH_norm3$Loss$Loss, 2)
#plot(Roll_ETH_norm3)

# STD #
GASSpec_ETH_std3 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_std3 <- UniGASFit(GASSpec_ETH_std3, CrCu_ETH_sub3)
Forecast_ETH_std3 <- UniGASFor(Fit_ETH_std3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_std3 <- UniGASRoll(CrCu_ETH_sub3, GASSpec_ETH_std3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETH_std3@Forecast$PointForecast[,2]
#Roll_ETH_std3@Forecast$Coef
#Roll_ETH_std3@Testing$PitTest
sigma_ETH_std3 <- Roll_ETH_std3@Forecast$PointForecast[,2]
MAPE_ETH_std3 <- MAPE(sigma_ETH_std3,realval_SGED3_ETH)
alpha <- 0.01
VaR_ETH_std3 <- quantile(Roll_ETH_std3, probs = alpha) 
ES_ETH_std3 <- ES(Roll_ETH_std3, probs = alpha)
VaRBacktest_ETH_std3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_std3, alpha = alpha)
QL_ETH_std3 <- round(VaRBacktest_ETH_std3$Loss$Loss / VaRBacktest_ETH_norm3$Loss$Loss, 2)
#plot(Roll_ETH_std3)

# SSTD #
GASSpec_ETH_sstd3 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_sstd3 <- UniGASFit(GASSpec_ETH_sstd3, CrCu_ETH_sub3)
Forecast_ETH_sstd3 <- UniGASFor(Fit_ETH_sstd3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_sstd3 <- UniGASRoll(CrCu_ETH_sub3, GASSpec_ETH_sstd3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETH_sstd3@Forecast$PointForecast[,2]
#Roll_ETH_sstd3@Forecast$Coef
#Roll_ETH_sstd3@Testing$PitTest
sigma_ETH_sstd3 <- Roll_ETH_sstd3@Forecast$PointForecast[,2]
MAPE_ETH_sstd3 <- MAPE(sigma_ETH_sstd3,realval_SGED3_ETH)
alpha <- 0.01
VaR_ETH_sstd3 <- quantile(Roll_ETH_sstd3, probs = alpha) 
ES_ETH_sstd3 <- ES(Roll_ETH_sstd3, probs = alpha)
VaRBacktest_ETH_sstd3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_sstd3, alpha = alpha)
QL_ETH_sstd3 <- round(VaRBacktest_ETH_sstd3$Loss$Loss / VaRBacktest_ETH_norm3$Loss$Loss, 2)
#plot(Roll_ETH_sstd3)

# AST #
GASSpec_ETH_ast3 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_ast3 <- UniGASFit(GASSpec_ETH_ast3, CrCu_ETH_sub3)
Forecast_ETH_ast3 <- UniGASFor(Fit_ETH_ast3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_ast3 <- UniGASRoll(CrCu_ETH_sub3, GASSpec_ETH_ast3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETH_ast3@Forecast$PointForecast[,2]
#Roll_ETH_ast3@Forecast$Coef
#Roll_ETH_ast3@Testing$PitTest
sigma_ETH_ast3 <- Roll_ETH_ast3@Forecast$PointForecast[,2]
MAPE_ETH_ast3 <- MAPE(sigma_ETH_ast3,realval_SGED3_ETH)
alpha <- 0.01
VaR_ETH_ast3 <- quantile(Roll_ETH_ast3, probs = alpha) 
ES_ETH_ast3 <- ES(Roll_ETH_ast3, probs = alpha)
VaRBacktest_ETH_ast3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_ast3, alpha = alpha)
QL_ETH_ast3 <- round(VaRBacktest_ETH_ast3$Loss$Loss / VaRBacktest_ETH_norm3$Loss$Loss, 2)
#plot(Roll_ETH_ast3)

# AST ONE#
GASSpec_ETH_astone3 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETH_astone3 <- UniGASFit(GASSpec_ETH_astone3, CrCu_ETH_sub3)
Forecast_ETH_astone3 <- UniGASFor(Fit_ETH_astone3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETH_astone3 <- UniGASRoll(CrCu_ETH_sub3, GASSpec_ETH_astone3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETH_astone3@Forecast$PointForecast[,2]
#Roll_ETH_astone3@Forecast$Coef
#Roll_ETH_astone3@Testing$PitTest
sigma_ETH_astone3 <- Roll_ETH_astone3@Forecast$PointForecast[,2]
MAPE_ETH_astone3 <- MAPE(sigma_ETH_astone3,realval_SGED3_ETH)
alpha <- 0.01
VaR_ETH_astone3 <- quantile(Roll_ETH_astone3, probs = alpha) 
ES_ETH_astone3 <- ES(Roll_ETH_astone3, probs = alpha)
VaRBacktest_ETH_astone3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_astone3, alpha = alpha)
QL_ETH_astone3 <- round(VaRBacktest_ETH_astone3$Loss$Loss / VaRBacktest_ETH_norm3$Loss$Loss, 2)
#plot(Roll_ETH_astone3)



### ::::::::::::::: LTC ::::::::::::::: ###
###### Full Sample

# NORM #
GASSpec_LTC_norm0 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_norm0 <- UniGASFit(GASSpec_LTC_norm0, CrCu_LTC_xts)
Forecast_LTC_norm0 <- UniGASFor(Fit_LTC_norm0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_norm0 <- UniGASRoll(CrCu_LTC_xts, GASSpec_LTC_norm0, ForecastLength = 350, RefitEvery = 50)
#Roll_LTC_norm0@Forecast$PointForecast[,2]
#Roll_LTC_norm0@Forecast$Coef
#Roll_LTC_norm0@Testing$PitTest
sigma_LTC_norm0 <- Roll_LTC_norm0@Forecast$PointForecast[,2]
MAPE_LTC_norm0 <- MAPE(sigma_LTC_norm0,realval_LTC)
alpha <- 0.01
VaR_LTC_norm0 <- quantile(Roll_LTC_norm0, probs = alpha) 
ES_LTC_norm0 <- ES(Roll_LTC_norm0, probs = alpha)
VaRBacktest_LTC_norm0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_norm0, alpha = alpha)
QL_LTC_norm0 <- round(VaRBacktest_LTC_norm0$Loss$Loss / VaRBacktest_LTC_norm0$Loss$Loss, 2)
#plot(Roll_LTC_norm0)

# STD #
GASSpec_LTC_std0 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_std0 <- UniGASFit(GASSpec_LTC_std0, CrCu_LTC_xts)
Forecast_LTC_std0 <- UniGASFor(Fit_LTC_std0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_std0 <- UniGASRoll(CrCu_LTC_xts, GASSpec_LTC_std0, ForecastLength = 350, RefitEvery = 50)
#Roll_LTC_std0@Forecast$PointForecast[,2]
#Roll_LTC_std0@Forecast$Coef
#Roll_LTC_std0@Testing$PitTest
sigma_LTC_std0 <- Roll_LTC_std0@Forecast$PointForecast[,2]
MAPE_LTC_std0 <- MAPE(sigma_LTC_std0,realval_LTC)
alpha <- 0.01
VaR_LTC_std0 <- quantile(Roll_LTC_std0, probs = alpha) 
ES_LTC_std0 <- ES(Roll_LTC_std0, probs = alpha)
VaRBacktest_LTC_std0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_std0, alpha = alpha)
QL_LTC_std0 <- round(VaRBacktest_LTC_std0$Loss$Loss / VaRBacktest_LTC_norm0$Loss$Loss, 2)
#plot(Roll_LTC_std0)

# SSTD #
GASSpec_LTC_sstd0 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_sstd0 <- UniGASFit(GASSpec_LTC_sstd0, CrCu_LTC_xts)
Forecast_LTC_sstd0 <- UniGASFor(Fit_LTC_sstd0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_sstd0 <- UniGASRoll(CrCu_LTC_xts, GASSpec_LTC_sstd0, ForecastLength = 350, RefitEvery = 50)
#Roll_LTC_sstd0@Forecast$PointForecast[,2]
#Roll_LTC_sstd0@Forecast$Coef
#Roll_LTC_sstd0@Testing$PitTest
sigma_LTC_sstd0 <- Roll_LTC_sstd0@Forecast$PointForecast[,2]
MAPE_LTC_sstd0 <- MAPE(sigma_LTC_sstd0,realval_LTC)
alpha <- 0.01
VaR_LTC_sstd0 <- quantile(Roll_LTC_norm0, probs = alpha) 
ES_LTC_sstd0 <- ES(Roll_LTC_norm0, probs = alpha)
VaRBacktest_LTC_sstd0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_sstd0, alpha = alpha)
QL_LTC_sstd0 <- round(VaRBacktest_LTC_sstd0$Loss$Loss / VaRBacktest_LTC_norm0$Loss$Loss, 2)
#plot(Roll_LTC_sstd0)

# AST #
GASSpec_LTC_ast0 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_ast0 <- UniGASFit(GASSpec_LTC_ast0, CrCu_LTC_xts)
#Forecast_LTC_ast0 <- UniGASFor(Fit_LTC_ast0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_ast0 <- UniGASRoll(CrCu_LTC_xts, GASSpec_LTC_ast0, ForecastLength = 350, RefitEvery = 50)
#Roll_LTC_ast0@Forecast$PointForecast[,2]
#Roll_LTC_ast0@Forecast$Coef
#Roll_LTC_ast0@Testing$PitTest
sigma_LTC_ast0 <- Roll_LTC_ast0@Forecast$PointForecast[,2]
MAPE_LTC_ast0 <- MAPE(sigma_LTC_ast0,realval_LTC)
alpha <- 0.01
VaR_LTC_ast0 <- quantile(Roll_LTC_ast0, probs = alpha) 
ES_LTC_ast0 <- ES(Roll_LTC_ast0, probs = alpha)
VaRBacktest_LTC_ast0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_ast0, alpha = alpha)
QL_LTC_ast0 <- round(VaRBacktest_LTC_ast0$Loss$Loss / VaRBacktest_LTC_norm0$Loss$Loss, 2)
#plot(Roll_LTC_ast0)

# AST ONE #
GASSpec_LTC_astone0 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_astone0 <- UniGASFit(GASSpec_LTC_astone0, CrCu_LTC_xts)
#Forecast_LTC_astone0 <- UniGASFor(Fit_LTC_astone0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_astone0 <- UniGASRoll(CrCu_LTC_xts, GASSpec_LTC_astone0, ForecastLength = 350, RefitEvery = 50)
#Roll_LTC_astone0@Forecast$PointForecast[,2]
#Roll_LTC_astone0@Forecast$Coef
#Roll_LTC_astone0@Testing$PitTest
sigma_LTC_astone0 <- Roll_LTC_astone0@Forecast$PointForecast[,2]
MAPE_LTC_astone0 <- MAPE(sigma_LTC_astone0,realval_LTC)
alpha <- 0.01
VaR_LTC_astone0 <- quantile(Roll_LTC_astone0, probs = alpha) 
ES_LTC_astone0 <- ES(Roll_LTC_astone0, probs = alpha)
VaRBacktest_LTC_astone0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_astone0, alpha = alpha)
QL_LTC_astone0 <- round(VaRBacktest_LTC_astone0$Loss$Loss / VaRBacktest_LTC_norm0$Loss$Loss, 2)
#plot(Roll_LTC_astone0)

############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 1 ######

# NORM #
GASSpec_LTC_norm1 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_norm1 <- UniGASFit(GASSpec_LTC_norm1, CrCu_LTC_sub1)
Forecast_LTC_norm1 <- UniGASFor(Fit_LTC_norm1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_norm1 <- UniGASRoll(CrCu_LTC_sub1, GASSpec_LTC_norm1, ForecastLength = 60, RefitEvery = 20)
#Roll_LTC_norm1@Forecast$PointForecast[,2]
#Roll_LTC_norm1@Forecast$Coef
#Roll_LTC_norm1@Testing$PitTest
sigma_LTC_norm1 <- Roll_LTC_norm1@Forecast$PointForecast[,2]
MAPE_LTC_norm1 <- MAPE(sigma_LTC_norm1,realval_SGED1_LTC)
alpha <- 0.01
VaR_LTC_norm1 <- quantile(Roll_LTC_norm1, probs = alpha) 
ES_LTC_norm1 <- ES(Roll_LTC_norm1, probs = alpha)
VaRBacktest_LTC_norm1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_norm1, alpha = alpha)
QL_LTC_norm1 <- round(VaRBacktest_LTC_norm1$Loss$Loss / VaRBacktest_LTC_norm1$Loss$Loss, 2)
#plot(Roll_LTC_norm1)

# STD #
GASSpec_LTC_std1 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_std1 <- UniGASFit(GASSpec_LTC_std1, CrCu_LTC_sub1)
Forecast_LTC_std1 <- UniGASFor(Fit_LTC_std1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_std1 <- UniGASRoll(CrCu_LTC_sub1, GASSpec_LTC_std1, ForecastLength = 60, RefitEvery = 20)
#Roll_LTC_std1@Forecast$PointForecast[,2]
#Roll_LTC_std1@Forecast$Coef
#Roll_LTC_std1@Testing$PitTest
sigma_LTC_std1 <- Roll_LTC_std1@Forecast$PointForecast[,2]
MAPE_LTC_std1 <- MAPE(sigma_LTC_std1,realval_SGED1_LTC)
alpha <- 0.01
VaR_LTC_std1 <- quantile(Roll_LTC_std1, probs = alpha) 
ES_LTC_std1 <- ES(Roll_LTC_std1, probs = alpha)
VaRBacktest_LTC_std1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_std1, alpha = alpha)
QL_LTC_std1 <- round(VaRBacktest_LTC_std1$Loss$Loss / VaRBacktest_LTC_norm1$Loss$Loss, 2)
#plot(Roll_LTC_std1)

# SSTD #
GASSpec_LTC_sstd1 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_sstd1 <- UniGASFit(GASSpec_LTC_sstd1, CrCu_LTC_sub1)
Forecast_LTC_sstd1 <- UniGASFor(Fit_LTC_sstd1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_sstd1 <- UniGASRoll(CrCu_LTC_sub1, GASSpec_LTC_sstd1, ForecastLength = 60, RefitEvery = 20)
#Roll_LTC_sstd1@Forecast$PointForecast[,2]
#Roll_LTC_sstd1@Forecast$Coef
#Roll_LTC_sstd1@Testing$PitTest
sigma_LTC_sstd1 <- Roll_LTC_sstd1@Forecast$PointForecast[,2]
MAPE_LTC_sstd1 <- MAPE(sigma_LTC_sstd1,realval_SGED1_LTC)
alpha <- 0.01
VaR_LTC_sstd1 <- quantile(Roll_LTC_sstd1, probs = alpha) 
ES_LTC_sstd1 <- ES(Roll_LTC_sstd1, probs = alpha)
VaRBacktest_LTC_sstd1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_sstd1, alpha = alpha)
QL_LTC_sstd1 <- round(VaRBacktest_LTC_sstd1$Loss$Loss / VaRBacktest_LTC_norm1$Loss$Loss, 2)
#plot(Roll_LTC_sstd1)

# AST #
GASSpec_LTC_ast1 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_ast1 <- UniGASFit(GASSpec_LTC_ast1, CrCu_LTC_sub1)
Forecast_LTC_ast1 <- UniGASFor(Fit_LTC_ast1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_ast1 <- UniGASRoll(CrCu_LTC_sub1, GASSpec_LTC_ast1, ForecastLength = 60, RefitEvery = 20)
#Roll_LTC_ast1@Forecast$PointForecast[,2]
#Roll_LTC_ast1@Forecast$Coef
#Roll_LTC_ast1@Testing$PitTest
sigma_LTC_ast1 <- Roll_LTC_ast1@Forecast$PointForecast[,2]
MAPE_LTC_ast1 <- MAPE(sigma_LTC_ast1,realval_SGED1_LTC)
alpha <- 0.01
VaR_LTC_ast1 <- quantile(Roll_LTC_ast1, probs = alpha) 
ES_LTC_ast1 <- ES(Roll_LTC_ast1, probs = alpha)
VaRBacktest_LTC_ast1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_ast1, alpha = alpha)
QL_LTC_ast1 <- round(VaRBacktest_LTC_ast1$Loss$Loss / VaRBacktest_LTC_norm1$Loss$Loss, 2)
#plot(Roll_LTC_ast1)

# AST ONE#
GASSpec_LTC_astone1 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_astone1 <- UniGASFit(GASSpec_LTC_astone1, CrCu_LTC_sub1)
Forecast_LTC_astone1 <- UniGASFor(Fit_LTC_astone1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_astone1 <- UniGASRoll(CrCu_LTC_sub1, GASSpec_LTC_astone1, ForecastLength = 60, RefitEvery = 20)
#Roll_LTC_astone1@Forecast$PointForecast[,2]
#Roll_LTC_astone1@Forecast$Coef
#Roll_LTC_astone1@Testing$PitTest
sigma_LTC_astone1 <- Roll_LTC_astone1@Forecast$PointForecast[,2]
MAPE_LTC_astone1 <- MAPE(sigma_LTC_astone1,realval_SGED1_LTC)
alpha <- 0.01
VaR_LTC_astone1 <- quantile(Roll_LTC_astone1, probs = alpha) 
ES_LTC_astone1 <- ES(Roll_LTC_astone1, probs = alpha)
VaRBacktest_LTC_astone1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_astone1, alpha = alpha)
QL_LTC_astone1 <- round(VaRBacktest_LTC_astone1$Loss$Loss / VaRBacktest_LTC_norm1$Loss$Loss, 2)
#plot(Roll_LTC_astone1)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 2 ######

# NORM #
GASSpec_LTC_norm2 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_norm2 <- UniGASFit(GASSpec_LTC_norm2, CrCu_LTC_sub2)
Forecast_LTC_norm2 <- UniGASFor(Fit_LTC_norm2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_norm2 <- UniGASRoll(CrCu_LTC_sub2, GASSpec_LTC_norm2, ForecastLength = 200, RefitEvery = 20)
#Roll_LTC_norm2@Forecast$PointForecast[,2]
#Roll_LTC_norm2@Forecast$Coef
#Roll_LTC_norm2@Testing$PitTest
sigma_LTC_norm2 <- Roll_LTC_norm2@Forecast$PointForecast[,2]
MAPE_LTC_norm2 <- MAPE(sigma_LTC_norm2,realval_SGED2_LTC)
alpha <- 0.01
VaR_LTC_norm2 <- quantile(Roll_LTC_norm2, probs = alpha) 
ES_LTC_norm2 <- ES(Roll_LTC_norm2, probs = alpha)
VaRBacktest_LTC_norm2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_norm2, alpha = alpha)
QL_LTC_norm2 <- round(VaRBacktest_LTC_norm2$Loss$Loss / VaRBacktest_LTC_norm2$Loss$Loss, 2)
#plot(Roll_LTC_norm2)

# STD #
GASSpec_LTC_std2 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_std2 <- UniGASFit(GASSpec_LTC_std2, CrCu_LTC_sub2)
Forecast_LTC_std2 <- UniGASFor(Fit_LTC_std2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_std2 <- UniGASRoll(CrCu_LTC_sub2, GASSpec_LTC_std2, ForecastLength = 200, RefitEvery = 20)
#Roll_LTC_std2@Forecast$PointForecast[,2]
#Roll_LTC_std2@Forecast$Coef
#Roll_LTC_std2@Testing$PitTest
sigma_LTC_std2 <- Roll_LTC_std2@Forecast$PointForecast[,2]
MAPE_LTC_std2 <- MAPE(sigma_LTC_std2,realval_SGED2_LTC)
alpha <- 0.01
VaR_LTC_std2 <- quantile(Roll_LTC_std2, probs = alpha) 
ES_LTC_std2 <- ES(Roll_LTC_std2, probs = alpha)
VaRBacktest_LTC_std2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_std2, alpha = alpha)
QL_LTC_std2 <- round(VaRBacktest_LTC_std2$Loss$Loss / VaRBacktest_LTC_norm2$Loss$Loss, 2)
#plot(Roll_LTC_std2)

# SSTD #
GASSpec_LTC_sstd2 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_sstd2 <- UniGASFit(GASSpec_LTC_sstd2, CrCu_LTC_sub2)
Forecast_LTC_sstd2 <- UniGASFor(Fit_LTC_sstd2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_sstd2 <- UniGASRoll(CrCu_LTC_sub2, GASSpec_LTC_sstd2, ForecastLength = 200, RefitEvery = 20)
#Roll_LTC_sstd2@Forecast$PointForecast[,2]
#Roll_LTC_sstd2@Forecast$Coef
#Roll_LTC_sstd2@Testing$PitTest
sigma_LTC_sstd2 <- Roll_LTC_sstd2@Forecast$PointForecast[,2]
MAPE_LTC_sstd2 <- MAPE(sigma_LTC_sstd2,realval_SGED2_LTC)
alpha <- 0.01
VaR_LTC_sstd2 <- quantile(Roll_LTC_sstd2, probs = alpha) 
ES_LTC_sstd2 <- ES(Roll_LTC_sstd2, probs = alpha)
VaRBacktest_LTC_sstd2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_sstd2, alpha = alpha)
QL_LTC_sstd2 <- round(VaRBacktest_LTC_sstd2$Loss$Loss / VaRBacktest_LTC_norm2$Loss$Loss, 2)
#plot(Roll_LTC_sstd2)

# AST #
GASSpec_LTC_ast2 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_ast2 <- UniGASFit(GASSpec_LTC_ast2, CrCu_LTC_sub2)
Forecast_LTC_ast2 <- UniGASFor(Fit_LTC_ast2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_ast2 <- UniGASRoll(CrCu_LTC_sub2, GASSpec_LTC_ast2, ForecastLength = 200, RefitEvery = 20)
#Roll_LTC_ast2@Forecast$PointForecast[,2]
#Roll_LTC_ast2@Forecast$Coef
#Roll_LTC_ast2@Testing$PitTest
sigma_LTC_ast2 <- Roll_LTC_ast2@Forecast$PointForecast[,2]
MAPE_LTC_ast2 <- MAPE(sigma_LTC_ast2,realval_SGED2_LTC)
alpha <- 0.01
VaR_LTC_ast2 <- quantile(Roll_LTC_ast2, probs = alpha) 
ES_LTC_ast2 <- ES(Roll_LTC_ast2, probs = alpha)
VaRBacktest_LTC_ast2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_ast2, alpha = alpha)
QL_LTC_ast2 <- round(VaRBacktest_LTC_ast2$Loss$Loss / VaRBacktest_LTC_norm2$Loss$Loss, 2)
#plot(Roll_LTC_ast2)

# AST ONE#
GASSpec_LTC_astone2 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_astone2 <- UniGASFit(GASSpec_LTC_astone2, CrCu_LTC_sub2)
Forecast_LTC_astone2 <- UniGASFor(Fit_LTC_astone2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_astone2 <- UniGASRoll(CrCu_LTC_sub2, GASSpec_LTC_astone2, ForecastLength = 200, RefitEvery = 20)
#Roll_LTC_astone2@Forecast$PointForecast[,2]
#Roll_LTC_astone2@Forecast$Coef
#Roll_LTC_astone2@Testing$PitTest
sigma_LTC_astone2 <- Roll_LTC_astone2@Forecast$PointForecast[,2]
MAPE_LTC_astone2 <- MAPE(sigma_LTC_astone2,realval_SGED2_LTC)
alpha <- 0.01
VaR_LTC_astone2 <- quantile(Roll_LTC_astone2, probs = alpha) 
ES_LTC_astone2 <- ES(Roll_LTC_astone2, probs = alpha)
VaRBacktest_LTC_astone2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_astone2, alpha = alpha)
QL_LTC_astone2 <- round(VaRBacktest_LTC_astone2$Loss$Loss / VaRBacktest_LTC_norm2$Loss$Loss, 2)
#plot(Roll_LTC_astone2)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 3 ######

# NORM #
GASSpec_LTC_norm3 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_norm3 <- UniGASFit(GASSpec_LTC_norm3, CrCu_LTC_sub3)
Forecast_LTC_norm3 <- UniGASFor(Fit_LTC_norm3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_norm3 <- UniGASRoll(CrCu_LTC_sub3, GASSpec_LTC_norm3, ForecastLength = 100, RefitEvery = 20)
#Roll_LTC_norm3@Forecast$PointForecast[,2]
#Roll_LTC_norm3@Forecast$Coef
#Roll_LTC_norm3@Testing$PitTest
sigma_LTC_norm3 <- Roll_LTC_norm3@Forecast$PointForecast[,2]
MAPE_LTC_norm3 <- MAPE(sigma_LTC_norm3,realval_SGED3_LTC)
alpha <- 0.01
VaR_LTC_norm3 <- quantile(Roll_LTC_norm3, probs = alpha) 
ES_LTC_norm3 <- ES(Roll_LTC_norm3, probs = alpha)
VaRBacktest_LTC_norm3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_norm3, alpha = alpha)
QL_LTC_norm3 <- round(VaRBacktest_LTC_norm3$Loss$Loss / VaRBacktest_LTC_norm3$Loss$Loss, 2)
#plot(Roll_LTC_norm3)

# STD #
GASSpec_LTC_std3 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_std3 <- UniGASFit(GASSpec_LTC_std3, CrCu_LTC_sub3)
Forecast_LTC_std3 <- UniGASFor(Fit_LTC_std3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_std3 <- UniGASRoll(CrCu_LTC_sub3, GASSpec_LTC_std3, ForecastLength = 100, RefitEvery = 20)
#Roll_LTC_std3@Forecast$PointForecast[,2]
#Roll_LTC_std3@Forecast$Coef
#Roll_LTC_std3@Testing$PitTest
sigma_LTC_std3 <- Roll_LTC_std3@Forecast$PointForecast[,2]
MAPE_LTC_std3 <- MAPE(sigma_LTC_std3,realval_SGED3_LTC)
alpha <- 0.01
VaR_LTC_std3 <- quantile(Roll_LTC_std3, probs = alpha) 
ES_LTC_std3 <- ES(Roll_LTC_std3, probs = alpha)
VaRBacktest_LTC_std3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_std3, alpha = alpha)
QL_LTC_std3 <- round(VaRBacktest_LTC_std3$Loss$Loss / VaRBacktest_LTC_norm3$Loss$Loss, 2)
#plot(Roll_LTC_std3)

# SSTD #
GASSpec_LTC_sstd3 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_sstd3 <- UniGASFit(GASSpec_LTC_sstd3, CrCu_LTC_sub3)
Forecast_LTC_sstd3 <- UniGASFor(Fit_LTC_sstd3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_sstd3 <- UniGASRoll(CrCu_LTC_sub3, GASSpec_LTC_sstd3, ForecastLength = 100, RefitEvery = 20)
#Roll_LTC_sstd3@Forecast$PointForecast[,2]
#Roll_LTC_sstd3@Forecast$Coef
#Roll_LTC_sstd3@Testing$PitTest
sigma_LTC_sstd3 <- Roll_LTC_sstd3@Forecast$PointForecast[,2]
MAPE_LTC_sstd3 <- MAPE(sigma_LTC_sstd3,realval_SGED3_LTC)
alpha <- 0.01
VaR_LTC_sstd3 <- quantile(Roll_LTC_sstd3, probs = alpha) 
ES_LTC_sstd3 <- ES(Roll_LTC_sstd3, probs = alpha)
VaRBacktest_LTC_sstd3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_sstd3, alpha = alpha)
QL_LTC_sstd3 <- round(VaRBacktest_LTC_sstd3$Loss$Loss / VaRBacktest_LTC_norm3$Loss$Loss, 2)
#plot(Roll_LTC_sstd3)

# AST #
GASSpec_LTC_ast3 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_ast3 <- UniGASFit(GASSpec_LTC_ast3, CrCu_LTC_sub3)
Forecast_LTC_ast3 <- UniGASFor(Fit_LTC_ast3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_ast3 <- UniGASRoll(CrCu_LTC_sub3, GASSpec_LTC_ast3, ForecastLength = 100, RefitEvery = 20)
#Roll_LTC_ast3@Forecast$PointForecast[,2]
#Roll_LTC_ast3@Forecast$Coef
#Roll_LTC_ast3@Testing$PitTest
sigma_LTC_ast3 <- Roll_LTC_ast3@Forecast$PointForecast[,2]
MAPE_LTC_ast3 <- MAPE(sigma_LTC_ast3,realval_SGED3_LTC)
alpha <- 0.01
VaR_LTC_ast3 <- quantile(Roll_LTC_ast3, probs = alpha) 
ES_LTC_ast3 <- ES(Roll_LTC_ast3, probs = alpha)
VaRBacktest_LTC_ast3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_ast3, alpha = alpha)
QL_LTC_ast3 <- round(VaRBacktest_LTC_ast3$Loss$Loss / VaRBacktest_LTC_norm3$Loss$Loss, 2)
#plot(Roll_LTC_ast3)

# AST ONE#
GASSpec_LTC_astone3 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_LTC_astone3 <- UniGASFit(GASSpec_LTC_astone3, CrCu_LTC_sub3)
Forecast_LTC_astone3 <- UniGASFor(Fit_LTC_astone3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_LTC_astone3 <- UniGASRoll(CrCu_LTC_sub3, GASSpec_LTC_astone3, ForecastLength = 100, RefitEvery = 20)
#Roll_LTC_astone3@Forecast$PointForecast[,2]
#Roll_LTC_astone3@Forecast$Coef
#Roll_LTC_astone3@Testing$PitTest
sigma_LTC_astone3 <- Roll_LTC_astone3@Forecast$PointForecast[,2]
MAPE_LTC_astone3 <- MAPE(sigma_LTC_astone3,realval_SGED3_LTC)
alpha <- 0.01
VaR_LTC_astone3 <- quantile(Roll_LTC_astone3, probs = alpha) 
ES_LTC_astone3 <- ES(Roll_LTC_astone3, probs = alpha)
VaRBacktest_LTC_astone3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_astone3, alpha = alpha)
QL_LTC_astone3 <- round(VaRBacktest_LTC_astone3$Loss$Loss / VaRBacktest_LTC_norm3$Loss$Loss, 2)
#plot(Roll_LTC_astone3)



#### ::::::::::::::: XMR ::::::::::::::: ####
###### Full Sample

# NORM #
GASSpec_XMR_norm0 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_norm0 <- UniGASFit(GASSpec_XMR_norm0, CrCu_XMR_xts)
Forecast_XMR_norm0 <- UniGASFor(Fit_XMR_norm0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_norm0 <- UniGASRoll(CrCu_XMR_xts, GASSpec_XMR_norm0, ForecastLength = 350, RefitEvery = 50)
#Roll_XMR_norm0@Forecast$PointForecast[,2]
#Roll_XMR_norm0@Forecast$Coef
#Roll_XMR_norm0@Testing$PitTest
sigma_XMR_norm0 <- Roll_XMR_norm0@Forecast$PointForecast[,2]
MAPE_XMR_norm0 <- MAPE(sigma_XMR_norm0,realval_XMR)
alpha <- 0.01
VaR_XMR_norm0 <- quantile(Roll_XMR_norm0, probs = alpha) 
ES_XMR_norm0 <- ES(Roll_XMR_norm0, probs = alpha)
VaRBacktest_XMR_norm0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_norm0, alpha = alpha)
QL_XMR_norm0 <- round(VaRBacktest_XMR_norm0$Loss$Loss / VaRBacktest_XMR_norm0$Loss$Loss, 2)
#plot(Roll_XMR_norm0)

# STD #
GASSpec_XMR_std0 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_std0 <- UniGASFit(GASSpec_XMR_std0, CrCu_XMR_xts)
Forecast_XMR_std0 <- UniGASFor(Fit_XMR_std0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_std0 <- UniGASRoll(CrCu_XMR_xts, GASSpec_XMR_std0, ForecastLength = 350, RefitEvery = 50)
#Roll_XMR_std0@Forecast$PointForecast[,2]
#Roll_XMR_std0@Forecast$Coef
#Roll_XMR_std0@Testing$PitTest
sigma_XMR_std0 <- Roll_XMR_std0@Forecast$PointForecast[,2]
MAPE_XMR_std0 <- MAPE(sigma_XMR_std0,realval_XMR)
alpha <- 0.01
VaR_XMR_std0 <- quantile(Roll_XMR_std0, probs = alpha) 
ES_XMR_std0 <- ES(Roll_XMR_std0, probs = alpha)
VaRBacktest_XMR_std0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_std0, alpha = alpha)
QL_XMR_std0 <- round(VaRBacktest_XMR_std0$Loss$Loss / VaRBacktest_XMR_norm0$Loss$Loss, 2)
#plot(Roll_XMR_std0)

# SSTD #
GASSpec_XMR_sstd0 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_sstd0 <- UniGASFit(GASSpec_XMR_sstd0, CrCu_XMR_xts)
Forecast_XMR_sstd0 <- UniGASFor(Fit_XMR_sstd0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_sstd0 <- UniGASRoll(CrCu_XMR_xts, GASSpec_XMR_sstd0, ForecastLength = 350, RefitEvery = 50)
#Roll_XMR_sstd0@Forecast$PointForecast[,2]
#Roll_XMR_sstd0@Forecast$Coef
#Roll_XMR_sstd0@Testing$PitTest
sigma_XMR_sstd0 <- Roll_XMR_sstd0@Forecast$PointForecast[,2]
MAPE_XMR_sstd0 <- MAPE(sigma_XMR_sstd0,realval_XMR)
alpha <- 0.01
VaR_XMR_sstd0 <- quantile(Roll_XMR_norm0, probs = alpha) 
ES_XMR_sstd0 <- ES(Roll_XMR_norm0, probs = alpha)
VaRBacktest_XMR_sstd0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_sstd0, alpha = alpha)
QL_XMR_sstd0 <- round(VaRBacktest_XMR_sstd0$Loss$Loss / VaRBacktest_XMR_norm0$Loss$Loss, 2)
#plot(Roll_XMR_sstd0)

# AST #
GASSpec_XMR_ast0 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_ast0 <- UniGASFit(GASSpec_XMR_ast0, CrCu_XMR_xts)
#Forecast_XMR_ast0 <- UniGASFor(Fit_XMR_ast0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_ast0 <- UniGASRoll(CrCu_XMR_xts, GASSpec_XMR_ast0, ForecastLength = 350, RefitEvery = 50)
#Roll_XMR_ast0@Forecast$PointForecast[,2]
#Roll_XMR_ast0@Forecast$Coef
#Roll_XMR_ast0@Testing$PitTest
sigma_XMR_ast0 <- Roll_XMR_ast0@Forecast$PointForecast[,2]
MAPE_XMR_ast0 <- MAPE(sigma_XMR_ast0,realval_XMR)
alpha <- 0.01
VaR_XMR_ast0 <- quantile(Roll_XMR_ast0, probs = alpha) 
ES_XMR_ast0 <- ES(Roll_XMR_ast0, probs = alpha)
VaRBacktest_XMR_ast0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_ast0, alpha = alpha)
QL_XMR_ast0 <- round(VaRBacktest_XMR_ast0$Loss$Loss / VaRBacktest_XMR_norm0$Loss$Loss, 2)
#plot(Roll_XMR_ast0)

# AST ONE #
GASSpec_XMR_astone0 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_astone0 <- UniGASFit(GASSpec_XMR_astone0, CrCu_XMR_xts)
#Forecast_XMR_astone0 <- UniGASFor(Fit_XMR_astone0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_astone0 <- UniGASRoll(CrCu_XMR_xts, GASSpec_XMR_astone0, ForecastLength = 350, RefitEvery = 50)
#Roll_XMR_astone0@Forecast$PointForecast[,2]
#Roll_XMR_astone0@Forecast$Coef
#Roll_XMR_astone0@Testing$PitTest
sigma_XMR_astone0 <- Roll_XMR_astone0@Forecast$PointForecast[,2]
MAPE_XMR_astone0 <- MAPE(sigma_XMR_astone0,realval_XMR)
alpha <- 0.01
VaR_XMR_astone0 <- quantile(Roll_XMR_astone0, probs = alpha) 
ES_XMR_astone0 <- ES(Roll_XMR_astone0, probs = alpha)
VaRBacktest_XMR_astone0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_astone0, alpha = alpha)
QL_XMR_astone0 <- round(VaRBacktest_XMR_astone0$Loss$Loss / VaRBacktest_XMR_norm0$Loss$Loss, 2)
#plot(Roll_XMR_astone0)

############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 1 ######

# NORM #
GASSpec_XMR_norm1 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_norm1 <- UniGASFit(GASSpec_XMR_norm1, CrCu_XMR_sub1)
Forecast_XMR_norm1 <- UniGASFor(Fit_XMR_norm1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_norm1 <- UniGASRoll(CrCu_XMR_sub1, GASSpec_XMR_norm1, ForecastLength = 60, RefitEvery = 20)
#Roll_XMR_norm1@Forecast$PointForecast[,2]
#Roll_XMR_norm1@Forecast$Coef
#Roll_XMR_norm1@Testing$PitTest
sigma_XMR_norm1 <- Roll_XMR_norm1@Forecast$PointForecast[,2]
MAPE_XMR_norm1 <- MAPE(sigma_XMR_norm1,realval_SGED1_XMR)
alpha <- 0.01
VaR_XMR_norm1 <- quantile(Roll_XMR_norm1, probs = alpha) 
ES_XMR_norm1 <- ES(Roll_XMR_norm1, probs = alpha)
VaRBacktest_XMR_norm1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_norm1, alpha = alpha)
QL_XMR_norm1 <- round(VaRBacktest_XMR_norm1$Loss$Loss / VaRBacktest_XMR_norm1$Loss$Loss, 2)
#plot(Roll_XMR_norm1)

# STD #
GASSpec_XMR_std1 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_std1 <- UniGASFit(GASSpec_XMR_std1, CrCu_XMR_sub1)
Forecast_XMR_std1 <- UniGASFor(Fit_XMR_std1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_std1 <- UniGASRoll(CrCu_XMR_sub1, GASSpec_XMR_std1, ForecastLength = 60, RefitEvery = 20)
#Roll_XMR_std1@Forecast$PointForecast[,2]
#Roll_XMR_std1@Forecast$Coef
#Roll_XMR_std1@Testing$PitTest
sigma_XMR_std1 <- Roll_XMR_std1@Forecast$PointForecast[,2]
MAPE_XMR_std1 <- MAPE(sigma_XMR_std1,realval_SGED1_XMR)
alpha <- 0.01
VaR_XMR_std1 <- quantile(Roll_XMR_std1, probs = alpha) 
ES_XMR_std1 <- ES(Roll_XMR_std1, probs = alpha)
VaRBacktest_XMR_std1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_std1, alpha = alpha)
QL_XMR_std1 <- round(VaRBacktest_XMR_std1$Loss$Loss / VaRBacktest_XMR_norm1$Loss$Loss, 2)
#plot(Roll_XMR_std1)

# SSTD #
GASSpec_XMR_sstd1 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_sstd1 <- UniGASFit(GASSpec_XMR_sstd1, CrCu_XMR_sub1)
Forecast_XMR_sstd1 <- UniGASFor(Fit_XMR_sstd1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_sstd1 <- UniGASRoll(CrCu_XMR_sub1, GASSpec_XMR_sstd1, ForecastLength = 60, RefitEvery = 20)
#Roll_XMR_sstd1@Forecast$PointForecast[,2]
#Roll_XMR_sstd1@Forecast$Coef
#Roll_XMR_sstd1@Testing$PitTest
sigma_XMR_sstd1 <- Roll_XMR_sstd1@Forecast$PointForecast[,2]
MAPE_XMR_sstd1 <- MAPE(sigma_XMR_sstd1,realval_SGED1_XMR)
alpha <- 0.01
VaR_XMR_sstd1 <- quantile(Roll_XMR_sstd1, probs = alpha) 
ES_XMR_sstd1 <- ES(Roll_XMR_sstd1, probs = alpha)
VaRBacktest_XMR_sstd1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_sstd1, alpha = alpha)
QL_XMR_sstd1 <- round(VaRBacktest_XMR_sstd1$Loss$Loss / VaRBacktest_XMR_norm1$Loss$Loss, 2)
#plot(Roll_XMR_sstd1)

# AST #
GASSpec_XMR_ast1 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_ast1 <- UniGASFit(GASSpec_XMR_ast1, CrCu_XMR_sub1)
Forecast_XMR_ast1 <- UniGASFor(Fit_XMR_ast1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_ast1 <- UniGASRoll(CrCu_XMR_sub1, GASSpec_XMR_ast1, ForecastLength = 60, RefitEvery = 20)
#Roll_XMR_ast1@Forecast$PointForecast[,2]
#Roll_XMR_ast1@Forecast$Coef
#Roll_XMR_ast1@Testing$PitTest
sigma_XMR_ast1 <- Roll_XMR_ast1@Forecast$PointForecast[,2]
MAPE_XMR_ast1 <- MAPE(sigma_XMR_ast1,realval_SGED1_XMR)
alpha <- 0.01
VaR_XMR_ast1 <- quantile(Roll_XMR_ast1, probs = alpha) 
ES_XMR_ast1 <- ES(Roll_XMR_ast1, probs = alpha)
VaRBacktest_XMR_ast1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_ast1, alpha = alpha)
QL_XMR_ast1 <- round(VaRBacktest_XMR_ast1$Loss$Loss / VaRBacktest_XMR_norm1$Loss$Loss, 2)
#plot(Roll_XMR_ast1)

# AST ONE#
GASSpec_XMR_astone1 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_astone1 <- UniGASFit(GASSpec_XMR_astone1, CrCu_XMR_sub1)
Forecast_XMR_astone1 <- UniGASFor(Fit_XMR_astone1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_astone1 <- UniGASRoll(CrCu_XMR_sub1, GASSpec_XMR_astone1, ForecastLength = 60, RefitEvery = 20)
#Roll_XMR_astone1@Forecast$PointForecast[,2]
#Roll_XMR_astone1@Forecast$Coef
#Roll_XMR_astone1@Testing$PitTest
sigma_XMR_astone1 <- Roll_XMR_astone1@Forecast$PointForecast[,2]
MAPE_XMR_astone1 <- MAPE(sigma_XMR_astone1,realval_SGED1_XMR)
alpha <- 0.01
VaR_XMR_astone1 <- quantile(Roll_XMR_astone1, probs = alpha) 
ES_XMR_astone1 <- ES(Roll_XMR_astone1, probs = alpha)
VaRBacktest_XMR_astone1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_astone1, alpha = alpha)
QL_XMR_astone1 <- round(VaRBacktest_XMR_astone1$Loss$Loss / VaRBacktest_XMR_norm1$Loss$Loss, 2)
#plot(Roll_XMR_astone1)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 2 ######

# NORM #
GASSpec_XMR_norm2 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_norm2 <- UniGASFit(GASSpec_XMR_norm2, CrCu_XMR_sub2)
Forecast_XMR_norm2 <- UniGASFor(Fit_XMR_norm2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_norm2 <- UniGASRoll(CrCu_XMR_sub2, GASSpec_XMR_norm2, ForecastLength = 200, RefitEvery = 20)
#Roll_XMR_norm2@Forecast$PointForecast[,2]
#Roll_XMR_norm2@Forecast$Coef
#Roll_XMR_norm2@Testing$PitTest
sigma_XMR_norm2 <- Roll_XMR_norm2@Forecast$PointForecast[,2]
MAPE_XMR_norm2 <- MAPE(sigma_XMR_norm2,realval_SGED2_XMR)
alpha <- 0.01
VaR_XMR_norm2 <- quantile(Roll_XMR_norm2, probs = alpha) 
ES_XMR_norm2 <- ES(Roll_XMR_norm2, probs = alpha)
VaRBacktest_XMR_norm2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_norm2, alpha = alpha)
QL_XMR_norm2 <- round(VaRBacktest_XMR_norm2$Loss$Loss / VaRBacktest_XMR_norm2$Loss$Loss, 2)
#plot(Roll_XMR_norm2)

# STD #
GASSpec_XMR_std2 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_std2 <- UniGASFit(GASSpec_XMR_std2, CrCu_XMR_sub2)
Forecast_XMR_std2 <- UniGASFor(Fit_XMR_std2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_std2 <- UniGASRoll(CrCu_XMR_sub2, GASSpec_XMR_std2, ForecastLength = 200, RefitEvery = 20)
#Roll_XMR_std2@Forecast$PointForecast[,2]
#Roll_XMR_std2@Forecast$Coef
#Roll_XMR_std2@Testing$PitTest
sigma_XMR_std2 <- Roll_XMR_std2@Forecast$PointForecast[,2]
MAPE_XMR_std2 <- MAPE(sigma_XMR_std2,realval_SGED2_XMR)
alpha <- 0.01
VaR_XMR_std2 <- quantile(Roll_XMR_std2, probs = alpha) 
ES_XMR_std2 <- ES(Roll_XMR_std2, probs = alpha)
VaRBacktest_XMR_std2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_std2, alpha = alpha)
QL_XMR_std2 <- round(VaRBacktest_XMR_std2$Loss$Loss / VaRBacktest_XMR_norm2$Loss$Loss, 2)
#plot(Roll_XMR_std2)

# SSTD #
GASSpec_XMR_sstd2 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_sstd2 <- UniGASFit(GASSpec_XMR_sstd2, CrCu_XMR_sub2)
Forecast_XMR_sstd2 <- UniGASFor(Fit_XMR_sstd2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_sstd2 <- UniGASRoll(CrCu_XMR_sub2, GASSpec_XMR_sstd2, ForecastLength = 200, RefitEvery = 20)
#Roll_XMR_sstd2@Forecast$PointForecast[,2]
#Roll_XMR_sstd2@Forecast$Coef
#Roll_XMR_sstd2@Testing$PitTest
sigma_XMR_sstd2 <- Roll_XMR_sstd2@Forecast$PointForecast[,2]
MAPE_XMR_sstd2 <- MAPE(sigma_XMR_sstd2,realval_SGED2_XMR)
alpha <- 0.01
VaR_XMR_sstd2 <- quantile(Roll_XMR_sstd2, probs = alpha) 
ES_XMR_sstd2 <- ES(Roll_XMR_sstd2, probs = alpha)
VaRBacktest_XMR_sstd2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_sstd2, alpha = alpha)
QL_XMR_sstd2 <- round(VaRBacktest_XMR_sstd2$Loss$Loss / VaRBacktest_XMR_norm2$Loss$Loss, 2)
#plot(Roll_XMR_sstd2)

# AST #
GASSpec_XMR_ast2 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_ast2 <- UniGASFit(GASSpec_XMR_ast2, CrCu_XMR_sub2)
Forecast_XMR_ast2 <- UniGASFor(Fit_XMR_ast2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_ast2 <- UniGASRoll(CrCu_XMR_sub2, GASSpec_XMR_ast2, ForecastLength = 200, RefitEvery = 20)
#Roll_XMR_ast2@Forecast$PointForecast[,2]
#Roll_XMR_ast2@Forecast$Coef
#Roll_XMR_ast2@Testing$PitTest
sigma_XMR_ast2 <- Roll_XMR_ast2@Forecast$PointForecast[,2]
MAPE_XMR_ast2 <- MAPE(sigma_XMR_ast2,realval_SGED2_XMR)
alpha <- 0.01
VaR_XMR_ast2 <- quantile(Roll_XMR_ast2, probs = alpha) 
ES_XMR_ast2 <- ES(Roll_XMR_ast2, probs = alpha)
VaRBacktest_XMR_ast2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_ast2, alpha = alpha)
QL_XMR_ast2 <- round(VaRBacktest_XMR_ast2$Loss$Loss / VaRBacktest_XMR_norm2$Loss$Loss, 2)
#plot(Roll_XMR_ast2)

# AST ONE#
GASSpec_XMR_astone2 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_astone2 <- UniGASFit(GASSpec_XMR_astone2, CrCu_XMR_sub2)
Forecast_XMR_astone2 <- UniGASFor(Fit_XMR_astone2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_astone2 <- UniGASRoll(CrCu_XMR_sub2, GASSpec_XMR_astone2, ForecastLength = 200, RefitEvery = 20)
#Roll_XMR_astone2@Forecast$PointForecast[,2]
#Roll_XMR_astone2@Forecast$Coef
#Roll_XMR_astone2@Testing$PitTest
sigma_XMR_astone2 <- Roll_XMR_astone2@Forecast$PointForecast[,2]
MAPE_XMR_astone2 <- MAPE(sigma_XMR_astone2,realval_SGED2_XMR)
alpha <- 0.01
VaR_XMR_astone2 <- quantile(Roll_XMR_astone2, probs = alpha) 
ES_XMR_astone2 <- ES(Roll_XMR_astone2, probs = alpha)
VaRBacktest_XMR_astone2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_astone2, alpha = alpha)
QL_XMR_astone2 <- round(VaRBacktest_XMR_astone2$Loss$Loss / VaRBacktest_XMR_norm2$Loss$Loss, 2)
#plot(Roll_XMR_astone2)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 3 ######

# NORM #
GASSpec_XMR_norm3 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_norm3 <- UniGASFit(GASSpec_XMR_norm3, CrCu_XMR_sub3)
Forecast_XMR_norm3 <- UniGASFor(Fit_XMR_norm3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_norm3 <- UniGASRoll(CrCu_XMR_sub3, GASSpec_XMR_norm3, ForecastLength = 100, RefitEvery = 20)
#Roll_XMR_norm3@Forecast$PointForecast[,2]
#Roll_XMR_norm3@Forecast$Coef
#Roll_XMR_norm3@Testing$PitTest
sigma_XMR_norm3 <- Roll_XMR_norm3@Forecast$PointForecast[,2]
MAPE_XMR_norm3 <- MAPE(sigma_XMR_norm3,realval_SGED3_XMR)
alpha <- 0.01
VaR_XMR_norm3 <- quantile(Roll_XMR_norm3, probs = alpha) 
ES_XMR_norm3 <- ES(Roll_XMR_norm3, probs = alpha)
VaRBacktest_XMR_norm3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_norm3, alpha = alpha)
QL_XMR_norm3 <- round(VaRBacktest_XMR_norm3$Loss$Loss / VaRBacktest_XMR_norm3$Loss$Loss, 2)
#plot(Roll_XMR_norm3)

# STD #
GASSpec_XMR_std3 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_std3 <- UniGASFit(GASSpec_XMR_std3, CrCu_XMR_sub3)
Forecast_XMR_std3 <- UniGASFor(Fit_XMR_std3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_std3 <- UniGASRoll(CrCu_XMR_sub3, GASSpec_XMR_std3, ForecastLength = 100, RefitEvery = 20)
#Roll_XMR_std3@Forecast$PointForecast[,2]
#Roll_XMR_std3@Forecast$Coef
#Roll_XMR_std3@Testing$PitTest
sigma_XMR_std3 <- Roll_XMR_std3@Forecast$PointForecast[,2]
MAPE_XMR_std3 <- MAPE(sigma_XMR_std3,realval_SGED3_XMR)
alpha <- 0.01
VaR_XMR_std3 <- quantile(Roll_XMR_std3, probs = alpha) 
ES_XMR_std3 <- ES(Roll_XMR_std3, probs = alpha)
VaRBacktest_XMR_std3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_std3, alpha = alpha)
QL_XMR_std3 <- round(VaRBacktest_XMR_std3$Loss$Loss / VaRBacktest_XMR_norm3$Loss$Loss, 2)
#plot(Roll_XMR_std3)

# SSTD #
GASSpec_XMR_sstd3 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_sstd3 <- UniGASFit(GASSpec_XMR_sstd3, CrCu_XMR_sub3)
Forecast_XMR_sstd3 <- UniGASFor(Fit_XMR_sstd3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_sstd3 <- UniGASRoll(CrCu_XMR_sub3, GASSpec_XMR_sstd3, ForecastLength = 100, RefitEvery = 20)
#Roll_XMR_sstd3@Forecast$PointForecast[,2]
#Roll_XMR_sstd3@Forecast$Coef
#Roll_XMR_sstd3@Testing$PitTest
sigma_XMR_sstd3 <- Roll_XMR_sstd3@Forecast$PointForecast[,2]
MAPE_XMR_sstd3 <- MAPE(sigma_XMR_sstd3,realval_SGED3_XMR)
alpha <- 0.01
VaR_XMR_sstd3 <- quantile(Roll_XMR_sstd3, probs = alpha) 
ES_XMR_sstd3 <- ES(Roll_XMR_sstd3, probs = alpha)
VaRBacktest_XMR_sstd3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_sstd3, alpha = alpha)
QL_XMR_sstd3 <- round(VaRBacktest_XMR_sstd3$Loss$Loss / VaRBacktest_XMR_norm3$Loss$Loss, 2)
#plot(Roll_XMR_sstd3)

# AST #
GASSpec_XMR_ast3 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_ast3 <- UniGASFit(GASSpec_XMR_ast3, CrCu_XMR_sub3)
Forecast_XMR_ast3 <- UniGASFor(Fit_XMR_ast3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_ast3 <- UniGASRoll(CrCu_XMR_sub3, GASSpec_XMR_ast3, ForecastLength = 100, RefitEvery = 20)
#Roll_XMR_ast3@Forecast$PointForecast[,2]
#Roll_XMR_ast3@Forecast$Coef
#Roll_XMR_ast3@Testing$PitTest
sigma_XMR_ast3 <- Roll_XMR_ast3@Forecast$PointForecast[,2]
MAPE_XMR_ast3 <- MAPE(sigma_XMR_ast3,realval_SGED3_XMR)
alpha <- 0.01
VaR_XMR_ast3 <- quantile(Roll_XMR_ast3, probs = alpha) 
ES_XMR_ast3 <- ES(Roll_XMR_ast3, probs = alpha)
VaRBacktest_XMR_ast3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_ast3, alpha = alpha)
QL_XMR_ast3 <- round(VaRBacktest_XMR_ast3$Loss$Loss / VaRBacktest_XMR_norm3$Loss$Loss, 2)
#plot(Roll_XMR_ast3)

# AST ONE#
GASSpec_XMR_astone3 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_XMR_astone3 <- UniGASFit(GASSpec_XMR_astone3, CrCu_XMR_sub3)
Forecast_XMR_astone3 <- UniGASFor(Fit_XMR_astone3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_XMR_astone3 <- UniGASRoll(CrCu_XMR_sub3, GASSpec_XMR_astone3, ForecastLength = 100, RefitEvery = 20)
#Roll_XMR_astone3@Forecast$PointForecast[,2]
#Roll_XMR_astone3@Forecast$Coef
#Roll_XMR_astone3@Testing$PitTest
sigma_XMR_astone3 <- Roll_XMR_astone3@Forecast$PointForecast[,2]
MAPE_XMR_astone3 <- MAPE(sigma_XMR_astone3,realval_SGED3_XMR)
alpha <- 0.01
VaR_XMR_astone3 <- quantile(Roll_XMR_astone3, probs = alpha) 
ES_XMR_astone3 <- ES(Roll_XMR_astone3, probs = alpha)
VaRBacktest_XMR_astone3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_astone3, alpha = alpha)
QL_XMR_astone3 <- round(VaRBacktest_XMR_astone3$Loss$Loss / VaRBacktest_XMR_norm3$Loss$Loss, 2)
#plot(Roll_XMR_astone3)


#### ::::::::::::::: ETC ::::::::::::::: ####
###### Full Sample

# NORM #
GASSpec_ETC_norm0 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_norm0 <- UniGASFit(GASSpec_ETC_norm0, CrCu_ETC_xts)
Forecast_ETC_norm0 <- UniGASFor(Fit_ETC_norm0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_norm0 <- UniGASRoll(CrCu_ETC_xts, GASSpec_ETC_norm0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETC_norm0@Forecast$PointForecast[,2]
#Roll_ETC_norm0@Forecast$Coef
#Roll_ETC_norm0@Testing$PitTest
sigma_ETC_norm0 <- Roll_ETC_norm0@Forecast$PointForecast[,2]
MAPE_ETC_norm0 <- MAPE(sigma_ETC_norm0,realval_ETC)
alpha <- 0.01
VaR_ETC_norm0 <- quantile(Roll_ETC_norm0, probs = alpha) 
ES_ETC_norm0 <- ES(Roll_ETC_norm0, probs = alpha)
VaRBacktest_ETC_norm0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_norm0, alpha = alpha)
QL_ETC_norm0 <- round(VaRBacktest_ETC_norm0$Loss$Loss / VaRBacktest_ETC_norm0$Loss$Loss, 2)
#plot(Roll_ETC_norm0)

# STD #
GASSpec_ETC_std0 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_std0 <- UniGASFit(GASSpec_ETC_std0, CrCu_ETC_xts)
Forecast_ETC_std0 <- UniGASFor(Fit_ETC_std0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_std0 <- UniGASRoll(CrCu_ETC_xts, GASSpec_ETC_std0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETC_std0@Forecast$PointForecast[,2]
#Roll_ETC_std0@Forecast$Coef
#Roll_ETC_std0@Testing$PitTest
sigma_ETC_std0 <- Roll_ETC_std0@Forecast$PointForecast[,2]
MAPE_ETC_std0 <- MAPE(sigma_ETC_std0,realval_ETC)
alpha <- 0.01
VaR_ETC_std0 <- quantile(Roll_ETC_std0, probs = alpha) 
ES_ETC_std0 <- ES(Roll_ETC_std0, probs = alpha)
VaRBacktest_ETC_std0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_std0, alpha = alpha)
QL_ETC_std0 <- round(VaRBacktest_ETC_std0$Loss$Loss / VaRBacktest_ETC_norm0$Loss$Loss, 2)
#plot(Roll_ETC_std0)

# SSTD #
GASSpec_ETC_sstd0 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_sstd0 <- UniGASFit(GASSpec_ETC_sstd0, CrCu_ETC_xts)
Forecast_ETC_sstd0 <- UniGASFor(Fit_ETC_sstd0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_sstd0 <- UniGASRoll(CrCu_ETC_xts, GASSpec_ETC_sstd0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETC_sstd0@Forecast$PointForecast[,2]
#Roll_ETC_sstd0@Forecast$Coef
#Roll_ETC_sstd0@Testing$PitTest
sigma_ETC_sstd0 <- Roll_ETC_sstd0@Forecast$PointForecast[,2]
MAPE_ETC_sstd0 <- MAPE(sigma_ETC_sstd0,realval_ETC)
alpha <- 0.01
VaR_ETC_sstd0 <- quantile(Roll_ETC_norm0, probs = alpha) 
ES_ETC_sstd0 <- ES(Roll_ETC_norm0, probs = alpha)
VaRBacktest_ETC_sstd0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_sstd0, alpha = alpha)
QL_ETC_sstd0 <- round(VaRBacktest_ETC_sstd0$Loss$Loss / VaRBacktest_ETC_norm0$Loss$Loss, 2)
#plot(Roll_ETC_sstd0)

# AST #
GASSpec_ETC_ast0 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_ast0 <- UniGASFit(GASSpec_ETC_ast0, CrCu_ETC_xts)
#Forecast_ETC_ast0 <- UniGASFor(Fit_ETC_ast0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_ast0 <- UniGASRoll(CrCu_ETC_xts, GASSpec_ETC_ast0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETC_ast0@Forecast$PointForecast[,2]
#Roll_ETC_ast0@Forecast$Coef
#Roll_ETC_ast0@Testing$PitTest
sigma_ETC_ast0 <- Roll_ETC_ast0@Forecast$PointForecast[,2]
MAPE_ETC_ast0 <- MAPE(sigma_ETC_ast0,realval_ETC)
alpha <- 0.01
VaR_ETC_ast0 <- quantile(Roll_ETC_ast0, probs = alpha) 
ES_ETC_ast0 <- ES(Roll_ETC_ast0, probs = alpha)
VaRBacktest_ETC_ast0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_ast0, alpha = alpha)
QL_ETC_ast0 <- round(VaRBacktest_ETC_ast0$Loss$Loss / VaRBacktest_ETC_norm0$Loss$Loss, 2)
#plot(Roll_ETC_ast0)

# AST ONE #
GASSpec_ETC_astone0 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_astone0 <- UniGASFit(GASSpec_ETC_astone0, CrCu_ETC_xts)
#Forecast_ETC_astone0 <- UniGASFor(Fit_ETC_astone0, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_astone0 <- UniGASRoll(CrCu_ETC_xts, GASSpec_ETC_astone0, ForecastLength = 350, RefitEvery = 50)
#Roll_ETC_astone0@Forecast$PointForecast[,2]
#Roll_ETC_astone0@Forecast$Coef
#Roll_ETC_astone0@Testing$PitTest
sigma_ETC_astone0 <- Roll_ETC_astone0@Forecast$PointForecast[,2]
MAPE_ETC_astone0 <- MAPE(sigma_ETC_astone0,realval_ETC)
alpha <- 0.01
VaR_ETC_astone0 <- quantile(Roll_ETC_astone0, probs = alpha) 
ES_ETC_astone0 <- ES(Roll_ETC_astone0, probs = alpha)
VaRBacktest_ETC_astone0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_astone0, alpha = alpha)
QL_ETC_astone0 <- round(VaRBacktest_ETC_astone0$Loss$Loss / VaRBacktest_ETC_norm0$Loss$Loss, 2)
#plot(Roll_ETC_astone0)

############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 1 ######

# NORM #
GASSpec_ETC_norm1 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_norm1 <- UniGASFit(GASSpec_ETC_norm1, CrCu_ETC_sub1)
Forecast_ETC_norm1 <- UniGASFor(Fit_ETC_norm1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_norm1 <- UniGASRoll(CrCu_ETC_sub1, GASSpec_ETC_norm1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETC_norm1@Forecast$PointForecast[,2]
#Roll_ETC_norm1@Forecast$Coef
#Roll_ETC_norm1@Testing$PitTest
sigma_ETC_norm1 <- Roll_ETC_norm1@Forecast$PointForecast[,2]
MAPE_ETC_norm1 <- MAPE(sigma_ETC_norm1,realval_SGED1_ETC)
alpha <- 0.01
VaR_ETC_norm1 <- quantile(Roll_ETC_norm1, probs = alpha) 
ES_ETC_norm1 <- ES(Roll_ETC_norm1, probs = alpha)
VaRBacktest_ETC_norm1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_norm1, alpha = alpha)
QL_ETC_norm1 <- round(VaRBacktest_ETC_norm1$Loss$Loss / VaRBacktest_ETC_norm1$Loss$Loss, 2)
#plot(Roll_ETC_norm1)

# STD #
GASSpec_ETC_std1 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_std1 <- UniGASFit(GASSpec_ETC_std1, CrCu_ETC_sub1)
Forecast_ETC_std1 <- UniGASFor(Fit_ETC_std1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_std1 <- UniGASRoll(CrCu_ETC_sub1, GASSpec_ETC_std1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETC_std1@Forecast$PointForecast[,2]
#Roll_ETC_std1@Forecast$Coef
#Roll_ETC_std1@Testing$PitTest
sigma_ETC_std1 <- Roll_ETC_std1@Forecast$PointForecast[,2]
MAPE_ETC_std1 <- MAPE(sigma_ETC_std1,realval_SGED1_ETC)
alpha <- 0.01
VaR_ETC_std1 <- quantile(Roll_ETC_std1, probs = alpha) 
ES_ETC_std1 <- ES(Roll_ETC_std1, probs = alpha)
VaRBacktest_ETC_std1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_std1, alpha = alpha)
QL_ETC_std1 <- round(VaRBacktest_ETC_std1$Loss$Loss / VaRBacktest_ETC_norm1$Loss$Loss, 2)
#plot(Roll_ETC_std1)

# SSTD #
GASSpec_ETC_sstd1 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_sstd1 <- UniGASFit(GASSpec_ETC_sstd1, CrCu_ETC_sub1)
Forecast_ETC_sstd1 <- UniGASFor(Fit_ETC_sstd1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_sstd1 <- UniGASRoll(CrCu_ETC_sub1, GASSpec_ETC_sstd1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETC_sstd1@Forecast$PointForecast[,2]
#Roll_ETC_sstd1@Forecast$Coef
#Roll_ETC_sstd1@Testing$PitTest
sigma_ETC_sstd1 <- Roll_ETC_sstd1@Forecast$PointForecast[,2]
MAPE_ETC_sstd1 <- MAPE(sigma_ETC_sstd1,realval_SGED1_ETC)
alpha <- 0.01
VaR_ETC_sstd1 <- quantile(Roll_ETC_sstd1, probs = alpha) 
ES_ETC_sstd1 <- ES(Roll_ETC_sstd1, probs = alpha)
VaRBacktest_ETC_sstd1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_sstd1, alpha = alpha)
QL_ETC_sstd1 <- round(VaRBacktest_ETC_sstd1$Loss$Loss / VaRBacktest_ETC_norm1$Loss$Loss, 2)
#plot(Roll_ETC_sstd1)

# AST #
GASSpec_ETC_ast1 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_ast1 <- UniGASFit(GASSpec_ETC_ast1, CrCu_ETC_sub1)
Forecast_ETC_ast1 <- UniGASFor(Fit_ETC_ast1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_ast1 <- UniGASRoll(CrCu_ETC_sub1, GASSpec_ETC_ast1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETC_ast1@Forecast$PointForecast[,2]
#Roll_ETC_ast1@Forecast$Coef
#Roll_ETC_ast1@Testing$PitTest
sigma_ETC_ast1 <- Roll_ETC_ast1@Forecast$PointForecast[,2]
MAPE_ETC_ast1 <- MAPE(sigma_ETC_ast1,realval_SGED1_ETC)
alpha <- 0.01
VaR_ETC_ast1 <- quantile(Roll_ETC_ast1, probs = alpha) 
ES_ETC_ast1 <- ES(Roll_ETC_ast1, probs = alpha)
VaRBacktest_ETC_ast1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_ast1, alpha = alpha)
QL_ETC_ast1 <- round(VaRBacktest_ETC_ast1$Loss$Loss / VaRBacktest_ETC_norm1$Loss$Loss, 2)
#plot(Roll_ETC_ast1)

# AST ONE#
GASSpec_ETC_astone1 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_astone1 <- UniGASFit(GASSpec_ETC_astone1, CrCu_ETC_sub1)
Forecast_ETC_astone1 <- UniGASFor(Fit_ETC_astone1, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_astone1 <- UniGASRoll(CrCu_ETC_sub1, GASSpec_ETC_astone1, ForecastLength = 60, RefitEvery = 20)
#Roll_ETC_astone1@Forecast$PointForecast[,2]
#Roll_ETC_astone1@Forecast$Coef
#Roll_ETC_astone1@Testing$PitTest
sigma_ETC_astone1 <- Roll_ETC_astone1@Forecast$PointForecast[,2]
MAPE_ETC_astone1 <- MAPE(sigma_ETC_astone1,realval_SGED1_ETC)
alpha <- 0.01
VaR_ETC_astone1 <- quantile(Roll_ETC_astone1, probs = alpha) 
ES_ETC_astone1 <- ES(Roll_ETC_astone1, probs = alpha)
VaRBacktest_ETC_astone1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_astone1, alpha = alpha)
QL_ETC_astone1 <- round(VaRBacktest_ETC_astone1$Loss$Loss / VaRBacktest_ETC_norm1$Loss$Loss, 2)
#plot(Roll_ETC_astone1)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 2 ######

# NORM #
GASSpec_ETC_norm2 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_norm2 <- UniGASFit(GASSpec_ETC_norm2, CrCu_ETC_sub2)
Forecast_ETC_norm2 <- UniGASFor(Fit_ETC_norm2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_norm2 <- UniGASRoll(CrCu_ETC_sub2, GASSpec_ETC_norm2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETC_norm2@Forecast$PointForecast[,2]
#Roll_ETC_norm2@Forecast$Coef
#Roll_ETC_norm2@Testing$PitTest
sigma_ETC_norm2 <- Roll_ETC_norm2@Forecast$PointForecast[,2]
MAPE_ETC_norm2 <- MAPE(sigma_ETC_norm2,realval_SGED2_ETC)
alpha <- 0.01
VaR_ETC_norm2 <- quantile(Roll_ETC_norm2, probs = alpha) 
ES_ETC_norm2 <- ES(Roll_ETC_norm2, probs = alpha)
VaRBacktest_ETC_norm2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_norm2, alpha = alpha)
QL_ETC_norm2 <- round(VaRBacktest_ETC_norm2$Loss$Loss / VaRBacktest_ETC_norm2$Loss$Loss, 2)
#plot(Roll_ETC_norm2)

# STD #
GASSpec_ETC_std2 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_std2 <- UniGASFit(GASSpec_ETC_std2, CrCu_ETC_sub2)
Forecast_ETC_std2 <- UniGASFor(Fit_ETC_std2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_std2 <- UniGASRoll(CrCu_ETC_sub2, GASSpec_ETC_std2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETC_std2@Forecast$PointForecast[,2]
#Roll_ETC_std2@Forecast$Coef
#Roll_ETC_std2@Testing$PitTest
sigma_ETC_std2 <- Roll_ETC_std2@Forecast$PointForecast[,2]
MAPE_ETC_std2 <- MAPE(sigma_ETC_std2,realval_SGED2_ETC)
alpha <- 0.01
VaR_ETC_std2 <- quantile(Roll_ETC_std2, probs = alpha) 
ES_ETC_std2 <- ES(Roll_ETC_std2, probs = alpha)
VaRBacktest_ETC_std2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_std2, alpha = alpha)
QL_ETC_std2 <- round(VaRBacktest_ETC_std2$Loss$Loss / VaRBacktest_ETC_norm2$Loss$Loss, 2)
#plot(Roll_ETC_std2)

# SSTD #
GASSpec_ETC_sstd2 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_sstd2 <- UniGASFit(GASSpec_ETC_sstd2, CrCu_ETC_sub2)
Forecast_ETC_sstd2 <- UniGASFor(Fit_ETC_sstd2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_sstd2 <- UniGASRoll(CrCu_ETC_sub2, GASSpec_ETC_sstd2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETC_sstd2@Forecast$PointForecast[,2]
#Roll_ETC_sstd2@Forecast$Coef
#Roll_ETC_sstd2@Testing$PitTest
sigma_ETC_sstd2 <- Roll_ETC_sstd2@Forecast$PointForecast[,2]
MAPE_ETC_sstd2 <- MAPE(sigma_ETC_sstd2,realval_SGED2_ETC)
alpha <- 0.01
VaR_ETC_sstd2 <- quantile(Roll_ETC_sstd2, probs = alpha) 
ES_ETC_sstd2 <- ES(Roll_ETC_sstd2, probs = alpha)
VaRBacktest_ETC_sstd2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_sstd2, alpha = alpha)
QL_ETC_sstd2 <- round(VaRBacktest_ETC_sstd2$Loss$Loss / VaRBacktest_ETC_norm2$Loss$Loss, 2)
#plot(Roll_ETC_sstd2)

# AST #
GASSpec_ETC_ast2 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_ast2 <- UniGASFit(GASSpec_ETC_ast2, CrCu_ETC_sub2)
Forecast_ETC_ast2 <- UniGASFor(Fit_ETC_ast2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_ast2 <- UniGASRoll(CrCu_ETC_sub2, GASSpec_ETC_ast2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETC_ast2@Forecast$PointForecast[,2]
#Roll_ETC_ast2@Forecast$Coef
#Roll_ETC_ast2@Testing$PitTest
sigma_ETC_ast2 <- Roll_ETC_ast2@Forecast$PointForecast[,2]
MAPE_ETC_ast2 <- MAPE(sigma_ETC_ast2,realval_SGED2_ETC)
alpha <- 0.01
VaR_ETC_ast2 <- quantile(Roll_ETC_ast2, probs = alpha) 
ES_ETC_ast2 <- ES(Roll_ETC_ast2, probs = alpha)
VaRBacktest_ETC_ast2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_ast2, alpha = alpha)
QL_ETC_ast2 <- round(VaRBacktest_ETC_ast2$Loss$Loss / VaRBacktest_ETC_norm2$Loss$Loss, 2)
#plot(Roll_ETC_ast2)

# AST ONE#
GASSpec_ETC_astone2 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_astone2 <- UniGASFit(GASSpec_ETC_astone2, CrCu_ETC_sub2)
Forecast_ETC_astone2 <- UniGASFor(Fit_ETC_astone2, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_astone2 <- UniGASRoll(CrCu_ETC_sub2, GASSpec_ETC_astone2, ForecastLength = 200, RefitEvery = 20)
#Roll_ETC_astone2@Forecast$PointForecast[,2]
#Roll_ETC_astone2@Forecast$Coef
#Roll_ETC_astone2@Testing$PitTest
sigma_ETC_astone2 <- Roll_ETC_astone2@Forecast$PointForecast[,2]
MAPE_ETC_astone2 <- MAPE(sigma_ETC_astone2,realval_SGED2_ETC)
alpha <- 0.01
VaR_ETC_astone2 <- quantile(Roll_ETC_astone2, probs = alpha) 
ES_ETC_astone2 <- ES(Roll_ETC_astone2, probs = alpha)
VaRBacktest_ETC_astone2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_astone2, alpha = alpha)
QL_ETC_astone2 <- round(VaRBacktest_ETC_astone2$Loss$Loss / VaRBacktest_ETC_norm2$Loss$Loss, 2)
#plot(Roll_ETC_astone2)


############ ::::::::::::::::::::::::::::::: ################
##### Sub Sample 3 ######

# NORM #
GASSpec_ETC_norm3 <- UniGASSpec(Dist = "norm", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_norm3 <- UniGASFit(GASSpec_ETC_norm3, CrCu_ETC_sub3)
Forecast_ETC_norm3 <- UniGASFor(Fit_ETC_norm3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_norm3 <- UniGASRoll(CrCu_ETC_sub3, GASSpec_ETC_norm3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETC_norm3@Forecast$PointForecast[,2]
#Roll_ETC_norm3@Forecast$Coef
#Roll_ETC_norm3@Testing$PitTest
sigma_ETC_norm3 <- Roll_ETC_norm3@Forecast$PointForecast[,2]
MAPE_ETC_norm3 <- MAPE(sigma_ETC_norm3,realval_SGED3_ETC)
alpha <- 0.01
VaR_ETC_norm3 <- quantile(Roll_ETC_norm3, probs = alpha) 
ES_ETC_norm3 <- ES(Roll_ETC_norm3, probs = alpha)
VaRBacktest_ETC_norm3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_norm3, alpha = alpha)
QL_ETC_norm3 <- round(VaRBacktest_ETC_norm3$Loss$Loss / VaRBacktest_ETC_norm3$Loss$Loss, 2)
#plot(Roll_ETC_norm3)

# STD #
GASSpec_ETC_std3 <- UniGASSpec(Dist = "std", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_std3 <- UniGASFit(GASSpec_ETC_std3, CrCu_ETC_sub3)
Forecast_ETC_std3 <- UniGASFor(Fit_ETC_std3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_std3 <- UniGASRoll(CrCu_ETC_sub3, GASSpec_ETC_std3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETC_std3@Forecast$PointForecast[,2]
#Roll_ETC_std3@Forecast$Coef
#Roll_ETC_std3@Testing$PitTest
sigma_ETC_std3 <- Roll_ETC_std3@Forecast$PointForecast[,2]
MAPE_ETC_std3 <- MAPE(sigma_ETC_std3,realval_SGED3_ETC)
alpha <- 0.01
VaR_ETC_std3 <- quantile(Roll_ETC_std3, probs = alpha) 
ES_ETC_std3 <- ES(Roll_ETC_std3, probs = alpha)
VaRBacktest_ETC_std3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_std3, alpha = alpha)
QL_ETC_std3 <- round(VaRBacktest_ETC_std3$Loss$Loss / VaRBacktest_ETC_norm3$Loss$Loss, 2)
#plot(Roll_ETC_std3)

# SSTD #
GASSpec_ETC_sstd3 <- UniGASSpec(Dist = "sstd", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_sstd3 <- UniGASFit(GASSpec_ETC_sstd3, CrCu_ETC_sub3)
Forecast_ETC_sstd3 <- UniGASFor(Fit_ETC_sstd3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_sstd3 <- UniGASRoll(CrCu_ETC_sub3, GASSpec_ETC_sstd3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETC_sstd3@Forecast$PointForecast[,2]
#Roll_ETC_sstd3@Forecast$Coef
#Roll_ETC_sstd3@Testing$PitTest
sigma_ETC_sstd3 <- Roll_ETC_sstd3@Forecast$PointForecast[,2]
MAPE_ETC_sstd3 <- MAPE(sigma_ETC_sstd3,realval_SGED3_ETC)
alpha <- 0.01
VaR_ETC_sstd3 <- quantile(Roll_ETC_sstd3, probs = alpha) 
ES_ETC_sstd3 <- ES(Roll_ETC_sstd3, probs = alpha)
VaRBacktest_ETC_sstd3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_sstd3, alpha = alpha)
QL_ETC_sstd3 <- round(VaRBacktest_ETC_sstd3$Loss$Loss / VaRBacktest_ETC_norm3$Loss$Loss, 2)
#plot(Roll_ETC_sstd3)

# AST #
GASSpec_ETC_ast3 <- UniGASSpec(Dist = "ast", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_ast3 <- UniGASFit(GASSpec_ETC_ast3, CrCu_ETC_sub3)
Forecast_ETC_ast3 <- UniGASFor(Fit_ETC_ast3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_ast3 <- UniGASRoll(CrCu_ETC_sub3, GASSpec_ETC_ast3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETC_ast3@Forecast$PointForecast[,2]
#Roll_ETC_ast3@Forecast$Coef
#Roll_ETC_ast3@Testing$PitTest
sigma_ETC_ast3 <- Roll_ETC_ast3@Forecast$PointForecast[,2]
MAPE_ETC_ast3 <- MAPE(sigma_ETC_ast3,realval_SGED3_ETC)
alpha <- 0.01
VaR_ETC_ast3 <- quantile(Roll_ETC_ast3, probs = alpha) 
ES_ETC_ast3 <- ES(Roll_ETC_ast3, probs = alpha)
VaRBacktest_ETC_ast3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_ast3, alpha = alpha)
QL_ETC_ast3 <- round(VaRBacktest_ETC_ast3$Loss$Loss / VaRBacktest_ETC_norm3$Loss$Loss, 2)
#plot(Roll_ETC_ast3)

# AST ONE#
GASSpec_ETC_astone3 <- UniGASSpec(Dist = "ast1", ScalingType = "Identity", GASPar = list(location = TRUE, scale = TRUE, shape = FALSE))
Fit_ETC_astone3 <- UniGASFit(GASSpec_ETC_astone3, CrCu_ETC_sub3)
Forecast_ETC_astone3 <- UniGASFor(Fit_ETC_astone3, H = 12)
# returns model information and predictions of future model paramters based on averages
Roll_ETC_astone3 <- UniGASRoll(CrCu_ETC_sub3, GASSpec_ETC_astone3, ForecastLength = 100, RefitEvery = 20)
#Roll_ETC_astone3@Forecast$PointForecast[,2]
#Roll_ETC_astone3@Forecast$Coef
#Roll_ETC_astone3@Testing$PitTest
sigma_ETC_astone3 <- Roll_ETC_astone3@Forecast$PointForecast[,2]
MAPE_ETC_astone3 <- MAPE(sigma_ETC_astone3,realval_SGED3_ETC)
alpha <- 0.01
VaR_ETC_astone3 <- quantile(Roll_ETC_astone3, probs = alpha) 
ES_ETC_astone3 <- ES(Roll_ETC_astone3, probs = alpha)
VaRBacktest_ETC_astone3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_astone3, alpha = alpha)
QL_ETC_astone3 <- round(VaRBacktest_ETC_astone3$Loss$Loss / VaRBacktest_ETC_norm3$Loss$Loss, 2)
#plot(Roll_ETC_astone3)

##########::::::::::::::::::;
RMSE_BTC_norm0 <- RMSE(sigma_BTC_norm0,realval_BTC)



