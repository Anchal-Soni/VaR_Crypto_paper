# =============================================================================
# 04_garch_mape_rmse.R
#
# Purpose : MAPE and RMSE of out-of-sample EGARCH volatility forecasts for all five
#           coins (full sample and SS1-SS3).
# Inputs  : objects from 03a-03e
# Outputs : MAPE_crypto, RMSE_crypto (LaTeX printed to the console)
# Paper   : Table A.8
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

############::::::::::::::: M A P E ::::::::::::::::::##############

###############::::::::::::::::: B I T C O I N ::::::::##########
# Full Sample

garchsigSGED_BTC <- CrCu_BTC_Roll6@forecast$density$Sigma
realval_BTC <- abs(CrCu_BTC_Roll6@forecast$density$Realized)
MAPEgvalSGED_BTC <- MAPE(garchsigSGED_BTC, realval_BTC)

garchsigSST_BTC <- CrCu_BTC_Roll5@forecast$density$Sigma
realval_BTC <- abs(CrCu_BTC_Roll5@forecast$density$Realized)
MAPEgvalSST_BTC <- MAPE(garchsigSST_BTC, realval_BTC)

garchsigJSU_BTC <- CrCu_BTC_Roll7@forecast$density$Sigma
realval_BTC <- abs(CrCu_BTC_Roll7@forecast$density$Realized)
MAPEgvalJSU_BTC <- MAPE(garchsigJSU_BTC, realval_BTC)

garchsigNorm_BTC <- CrCu_BTC_Roll8@forecast$density$Sigma
realval_BTC <- abs(CrCu_BTC_Roll8@forecast$density$Realized)
MAPEgvalNorm_BTC <- MAPE(garchsigNorm_BTC, realval_BTC)

# Sample 1

garchsig_SGED1_BTC <- CrCu_BTC_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_BTC <- abs(CrCu_BTC_Roll_SGED_Sample1@forecast$density$Realized)
MAPEval_SGED1_BTC <- MAPE(garchsig_SGED1_BTC, realval_SGED1_BTC)

garchsig_SST1_BTC <- CrCu_BTC_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_BTC <- abs(CrCu_BTC_Roll_SST_Sample1@forecast$density$Realized)
MAPEval_SST1_BTC <- MAPE(garchsig_SST1_BTC, realval_SST1_BTC)

garchsig_JSU1_BTC <- CrCu_BTC_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_BTC <- abs(CrCu_BTC_Roll_JSU_Sample1@forecast$density$Realized)
MAPEval_JSU1_BTC <- MAPE(garchsig_JSU1_BTC, realval_JSU1_BTC)

garchsig_Normal1_BTC <- CrCu_BTC_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_BTC <- abs(CrCu_BTC_Roll_Normal_Sample1@forecast$density$Realized)
MAPEval_Normal1_BTC <- MAPE(garchsig_Normal1_BTC, realval_Normal1_BTC)

# Sample 2

garchsig_SGED2_BTC <- CrCu_BTC_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_BTC <- abs(CrCu_BTC_Roll_SGED_Sample2@forecast$density$Realized)
MAPEval_SGED2_BTC <- MAPE(garchsig_SGED2_BTC, realval_SGED2_BTC)

garchsig_SST2_BTC <- CrCu_BTC_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_BTC <- abs(CrCu_BTC_Roll_SST_Sample2@forecast$density$Realized)
MAPEval_SST2_BTC <- MAPE(garchsig_SST2_BTC, realval_SST2_BTC)

garchsig_JSU2_BTC <- CrCu_BTC_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_BTC <- abs(CrCu_BTC_Roll_JSU_Sample2@forecast$density$Realized)
MAPEval_JSU2_BTC <- MAPE(garchsig_JSU2_BTC, realval_JSU2_BTC)

garchsig_Normal2_BTC <- CrCu_BTC_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_BTC <- abs(CrCu_BTC_Roll_Normal_Sample2@forecast$density$Realized)
MAPEval_Normal2_BTC <- MAPE(garchsig_Normal2_BTC, realval_Normal2_BTC)

# Sample 3

garchsig_SGED3_BTC <- CrCu_BTC_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_BTC <- abs(CrCu_BTC_Roll_SGED_Sample3@forecast$density$Realized)
MAPEval_SGED3_BTC <- MAPE(garchsig_SGED3_BTC, realval_SGED3_BTC)

garchsig_SST3_BTC <- CrCu_BTC_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_BTC <- abs(CrCu_BTC_Roll_SST_Sample3@forecast$density$Realized)
MAPEval_SST3_BTC <- MAPE(garchsig_SST3_BTC, realval_SST3_BTC)

garchsig_JSU3_BTC <- CrCu_BTC_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_BTC <- abs(CrCu_BTC_Roll_JSU_Sample3@forecast$density$Realized)
MAPEval_JSU3_BTC <- MAPE(garchsig_JSU3_BTC, realval_JSU3_BTC)

garchsig_Normal3_BTC <- CrCu_BTC_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_BTC <- abs(CrCu_BTC_Roll_Normal_Sample3@forecast$density$Realized)
MAPEval_Normal3_BTC <- MAPE(garchsig_Normal3_BTC, realval_Normal3_BTC)

MAPE_BTC <- c(MAPEgvalSGED_BTC, MAPEgvalSST_BTC, MAPEgvalJSU_BTC, MAPEgvalNorm_BTC, 
              MAPEval_SGED1_BTC, MAPEval_SST1_BTC, MAPEval_JSU1_BTC, MAPEval_Normal1_BTC,
              MAPEval_SGED2_BTC, MAPEval_SST2_BTC, MAPEval_JSU2_BTC, MAPEval_Normal2_BTC,
              MAPEval_SGED3_BTC, MAPEval_SST3_BTC, MAPEval_JSU3_BTC, MAPEval_Normal3_BTC)

MAPE_BTC <- matrix(data = MAPE_BTC, nrow = 4)
colnames(MAPE_BTC) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_BTC) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: E T H E R E U M ::::::::##########
# Full Sample

garchsigSGED_ETH <- CrCu_ETH_Roll6@forecast$density$Sigma
realval_ETH <- abs(CrCu_ETH_Roll6@forecast$density$Realized)
MAPEgvalSGED_ETH <- MAPE(garchsigSGED_ETH, realval_ETH)

garchsigSST_ETH <- CrCu_ETH_Roll5@forecast$density$Sigma
realval_ETH <- abs(CrCu_ETH_Roll5@forecast$density$Realized)
MAPEgvalSST_ETH <- MAPE(garchsigSST_ETH, realval_ETH)

garchsigJSU_ETH <- CrCu_ETH_Roll7@forecast$density$Sigma
realval_ETH <- abs(CrCu_ETH_Roll7@forecast$density$Realized)
MAPEgvalJSU_ETH <- MAPE(garchsigJSU_ETH, realval_ETH)

garchsigNorm_ETH <- CrCu_ETH_Roll8@forecast$density$Sigma
realval_ETH <- abs(CrCu_ETH_Roll8@forecast$density$Realized)
MAPEgvalNorm_ETH <- MAPE(garchsigNorm_ETH, realval_ETH)

# Sample 1

garchsig_SGED1_ETH <- CrCu_ETH_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_ETH <- abs(CrCu_ETH_Roll_SGED_Sample1@forecast$density$Realized)
MAPEval_SGED1_ETH <- MAPE(garchsig_SGED1_ETH, realval_SGED1_ETH)

garchsig_SST1_ETH <- CrCu_ETH_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_ETH <- abs(CrCu_ETH_Roll_SST_Sample1@forecast$density$Realized)
MAPEval_SST1_ETH <- MAPE(garchsig_SST1_ETH, realval_SST1_ETH)

garchsig_JSU1_ETH <- CrCu_ETH_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_ETH <- abs(CrCu_ETH_Roll_JSU_Sample1@forecast$density$Realized)
MAPEval_JSU1_ETH <- MAPE(garchsig_JSU1_ETH, realval_JSU1_ETH)

garchsig_Normal1_ETH <- CrCu_ETH_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_ETH <- abs(CrCu_ETH_Roll_Normal_Sample1@forecast$density$Realized)
MAPEval_Normal1_ETH <- MAPE(garchsig_Normal1_ETH, realval_Normal1_ETH)

# Sample 2

garchsig_SGED2_ETH <- CrCu_ETH_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_ETH <- abs(CrCu_ETH_Roll_SGED_Sample2@forecast$density$Realized)
MAPEval_SGED2_ETH <- MAPE(garchsig_SGED2_ETH, realval_SGED2_ETH)

garchsig_SST2_ETH <- CrCu_ETH_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_ETH <- abs(CrCu_ETH_Roll_SST_Sample2@forecast$density$Realized)
MAPEval_SST2_ETH <- MAPE(garchsig_SST2_ETH, realval_SST2_ETH)

garchsig_JSU2_ETH <- CrCu_ETH_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_ETH <- abs(CrCu_ETH_Roll_JSU_Sample2@forecast$density$Realized)
MAPEval_JSU2_ETH <- MAPE(garchsig_JSU2_ETH, realval_JSU2_ETH)

garchsig_Normal2_ETH <- CrCu_ETH_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_ETH <- abs(CrCu_ETH_Roll_Normal_Sample2@forecast$density$Realized)
MAPEval_Normal2_ETH <- MAPE(garchsig_Normal2_ETH, realval_Normal2_ETH)

# Sample 3

garchsig_SGED3_ETH <- CrCu_ETH_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_ETH <- abs(CrCu_ETH_Roll_SGED_Sample3@forecast$density$Realized)
MAPEval_SGED3_ETH <- MAPE(garchsig_SGED3_ETH, realval_SGED3_ETH)

garchsig_SST3_ETH <- CrCu_ETH_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_ETH <- abs(CrCu_ETH_Roll_SST_Sample3@forecast$density$Realized)
MAPEval_SST3_ETH <- MAPE(garchsig_SST3_ETH, realval_SST3_ETH)

garchsig_JSU3_ETH <- CrCu_ETH_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_ETH <- abs(CrCu_ETH_Roll_JSU_Sample3@forecast$density$Realized)
MAPEval_JSU3_ETH <- MAPE(garchsig_JSU3_ETH, realval_JSU3_ETH)

garchsig_Normal3_ETH <- CrCu_ETH_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_ETH <- abs(CrCu_ETH_Roll_Normal_Sample3@forecast$density$Realized)
MAPEval_Normal3_ETH <- MAPE(garchsig_Normal3_ETH, realval_Normal3_ETH)

MAPE_ETH <- c(MAPEgvalSGED_ETH, MAPEgvalSST_ETH, MAPEgvalJSU_ETH, MAPEgvalNorm_ETH, 
              MAPEval_SGED1_ETH, MAPEval_SST1_ETH, MAPEval_JSU1_ETH, MAPEval_Normal1_ETH,
              MAPEval_SGED2_ETH, MAPEval_SST2_ETH, MAPEval_JSU2_ETH, MAPEval_Normal2_ETH,
              MAPEval_SGED3_ETH, MAPEval_SST3_ETH, MAPEval_JSU3_ETH, MAPEval_Normal3_ETH)

MAPE_ETH <- matrix(data = MAPE_ETH, nrow = 4)
colnames(MAPE_ETH) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_ETH) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: L I T E C O I N ::::::::##########
# Full Sample

garchsigSGED_LTC <- CrCu_LTC_Roll6@forecast$density$Sigma
realval_LTC <- abs(CrCu_LTC_Roll6@forecast$density$Realized)
MAPEgvalSGED_LTC <- MAPE(garchsigSGED_LTC, realval_LTC)

garchsigSST_LTC <- CrCu_LTC_Roll5@forecast$density$Sigma
realval_LTC <- abs(CrCu_LTC_Roll5@forecast$density$Realized)
MAPEgvalSST_LTC <- MAPE(garchsigSST_LTC, realval_LTC)

garchsigJSU_LTC <- CrCu_LTC_Roll7@forecast$density$Sigma
realval_LTC <- abs(CrCu_LTC_Roll7@forecast$density$Realized)
MAPEgvalJSU_LTC <- MAPE(garchsigJSU_LTC, realval_LTC)

garchsigNorm_LTC <- CrCu_LTC_Roll8@forecast$density$Sigma
realval_LTC <- abs(CrCu_LTC_Roll8@forecast$density$Realized)
MAPEgvalNorm_LTC <- MAPE(garchsigNorm_LTC, realval_LTC)

# Sample 1

garchsig_SGED1_LTC <- CrCu_LTC_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_LTC <- abs(CrCu_LTC_Roll_SGED_Sample1@forecast$density$Realized)
MAPEval_SGED1_LTC <- MAPE(garchsig_SGED1_LTC, realval_SGED1_LTC)

garchsig_SST1_LTC <- CrCu_LTC_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_LTC <- abs(CrCu_LTC_Roll_SST_Sample1@forecast$density$Realized)
MAPEval_SST1_LTC <- MAPE(garchsig_SST1_LTC, realval_SST1_LTC)

garchsig_JSU1_LTC <- CrCu_LTC_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_LTC <- abs(CrCu_LTC_Roll_JSU_Sample1@forecast$density$Realized)
MAPEval_JSU1_LTC <- MAPE(garchsig_JSU1_LTC, realval_JSU1_LTC)

garchsig_Normal1_LTC <- CrCu_LTC_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_LTC <- abs(CrCu_LTC_Roll_Normal_Sample1@forecast$density$Realized)
MAPEval_Normal1_LTC <- MAPE(garchsig_Normal1_LTC, realval_Normal1_LTC)

# Sample 2

garchsig_SGED2_LTC <- CrCu_LTC_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_LTC <- abs(CrCu_LTC_Roll_SGED_Sample2@forecast$density$Realized)
MAPEval_SGED2_LTC <- MAPE(garchsig_SGED2_LTC, realval_SGED2_LTC)

garchsig_SST2_LTC <- CrCu_LTC_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_LTC <- abs(CrCu_LTC_Roll_SST_Sample2@forecast$density$Realized)
MAPEval_SST2_LTC <- MAPE(garchsig_SST2_LTC, realval_SST2_LTC)

garchsig_JSU2_LTC <- CrCu_LTC_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_LTC <- abs(CrCu_LTC_Roll_JSU_Sample2@forecast$density$Realized)
MAPEval_JSU2_LTC <- MAPE(garchsig_JSU2_LTC, realval_JSU2_LTC)

garchsig_Normal2_LTC <- CrCu_LTC_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_LTC <- abs(CrCu_LTC_Roll_Normal_Sample2@forecast$density$Realized)
MAPEval_Normal2_LTC <- MAPE(garchsig_Normal2_LTC, realval_Normal2_LTC)

# Sample 3

garchsig_SGED3_LTC <- CrCu_LTC_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_LTC <- abs(CrCu_LTC_Roll_SGED_Sample3@forecast$density$Realized)
MAPEval_SGED3_LTC <- MAPE(garchsig_SGED3_LTC, realval_SGED3_LTC)

garchsig_SST3_LTC <- CrCu_LTC_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_LTC <- abs(CrCu_LTC_Roll_SST_Sample3@forecast$density$Realized)
MAPEval_SST3_LTC <- MAPE(garchsig_SST3_LTC, realval_SST3_LTC)

garchsig_JSU3_LTC <- CrCu_LTC_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_LTC <- abs(CrCu_LTC_Roll_JSU_Sample3@forecast$density$Realized)
MAPEval_JSU3_LTC <- MAPE(garchsig_JSU3_LTC, realval_JSU3_LTC)

garchsig_Normal3_LTC <- CrCu_LTC_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_LTC <- abs(CrCu_LTC_Roll_Normal_Sample3@forecast$density$Realized)
MAPEval_Normal3_LTC <- MAPE(garchsig_Normal3_LTC, realval_Normal3_LTC)

MAPE_LTC <- c(MAPEgvalSGED_LTC, MAPEgvalSST_LTC, MAPEgvalJSU_LTC, MAPEgvalNorm_LTC, 
              MAPEval_SGED1_LTC, MAPEval_SST1_LTC, MAPEval_JSU1_LTC, MAPEval_Normal1_LTC,
              MAPEval_SGED2_LTC, MAPEval_SST2_LTC, MAPEval_JSU2_LTC, MAPEval_Normal2_LTC,
              MAPEval_SGED3_LTC, MAPEval_SST3_LTC, MAPEval_JSU3_LTC, MAPEval_Normal3_LTC)

MAPE_LTC <- matrix(data = MAPE_LTC, nrow = 4)
colnames(MAPE_LTC) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_LTC) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: M O N E R O ::::::::##########
# Full Sample

garchsigSGED_XMR <- CrCu_XMR_Roll6@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll6@forecast$density$Realized)
MAPEgvalSGED_XMR <- MAPE(garchsigSGED_XMR, realval_XMR)

garchsigSST_XMR <- CrCu_XMR_Roll5@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll5@forecast$density$Realized)
MAPEgvalSST_XMR <- MAPE(garchsigSST_XMR, realval_XMR)

garchsigJSU_XMR <- CrCu_XMR_Roll7@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll7@forecast$density$Realized)
MAPEgvalJSU_XMR <- MAPE(garchsigJSU_XMR, realval_XMR)

garchsigNorm_XMR <- CrCu_XMR_Roll8@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll8@forecast$density$Realized)
MAPEgvalNorm_XMR <- MAPE(garchsigNorm_XMR, realval_XMR)

# Sample 1

garchsig_SGED1_XMR <- CrCu_XMR_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_XMR <- abs(CrCu_XMR_Roll_SGED_Sample1@forecast$density$Realized)
MAPEval_SGED1_XMR <- MAPE(garchsig_SGED1_XMR, realval_SGED1_XMR)

garchsig_SST1_XMR <- CrCu_XMR_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_XMR <- abs(CrCu_XMR_Roll_SST_Sample1@forecast$density$Realized)
MAPEval_SST1_XMR <- MAPE(garchsig_SST1_XMR, realval_SST1_XMR)

garchsig_JSU1_XMR <- CrCu_XMR_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_XMR <- abs(CrCu_XMR_Roll_JSU_Sample1@forecast$density$Realized)
MAPEval_JSU1_XMR <- MAPE(garchsig_JSU1_XMR, realval_JSU1_XMR)

garchsig_Normal1_XMR <- CrCu_XMR_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_XMR <- abs(CrCu_XMR_Roll_Normal_Sample1@forecast$density$Realized)
MAPEval_Normal1_XMR <- MAPE(garchsig_Normal1_XMR, realval_Normal1_XMR)

# Sample 2

garchsig_SGED2_XMR <- CrCu_XMR_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_XMR <- abs(CrCu_XMR_Roll_SGED_Sample2@forecast$density$Realized)
MAPEval_SGED2_XMR <- MAPE(garchsig_SGED2_XMR, realval_SGED2_XMR)

garchsig_SST2_XMR <- CrCu_XMR_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_XMR <- abs(CrCu_XMR_Roll_SST_Sample2@forecast$density$Realized)
MAPEval_SST2_XMR <- MAPE(garchsig_SST2_XMR, realval_SST2_XMR)

garchsig_JSU2_XMR <- CrCu_XMR_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_XMR <- abs(CrCu_XMR_Roll_JSU_Sample2@forecast$density$Realized)
MAPEval_JSU2_XMR <- MAPE(garchsig_JSU2_XMR, realval_JSU2_XMR)

garchsig_Normal2_XMR <- CrCu_XMR_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_XMR <- abs(CrCu_XMR_Roll_Normal_Sample2@forecast$density$Realized)
MAPEval_Normal2_XMR <- MAPE(garchsig_Normal2_XMR, realval_Normal2_XMR)

# Sample 3

garchsig_SGED3_XMR <- CrCu_XMR_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_XMR <- abs(CrCu_XMR_Roll_SGED_Sample3@forecast$density$Realized)
MAPEval_SGED3_XMR <- MAPE(garchsig_SGED3_XMR, realval_SGED3_XMR)

garchsig_SST3_XMR <- CrCu_XMR_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_XMR <- abs(CrCu_XMR_Roll_SST_Sample3@forecast$density$Realized)
MAPEval_SST3_XMR <- MAPE(garchsig_SST3_XMR, realval_SST3_XMR)

garchsig_JSU3_XMR <- CrCu_XMR_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_XMR <- abs(CrCu_XMR_Roll_JSU_Sample3@forecast$density$Realized)
MAPEval_JSU3_XMR <- MAPE(garchsig_JSU3_XMR, realval_JSU3_XMR)

garchsig_Normal3_XMR <- CrCu_XMR_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_XMR <- abs(CrCu_XMR_Roll_Normal_Sample3@forecast$density$Realized)
MAPEval_Normal3_XMR <- MAPE(garchsig_Normal3_XMR, realval_Normal3_XMR)

MAPE_XMR <- c(MAPEgvalSGED_XMR, MAPEgvalSST_XMR, MAPEgvalJSU_XMR, MAPEgvalNorm_XMR, 
              MAPEval_SGED1_XMR, MAPEval_SST1_XMR, MAPEval_JSU1_XMR, MAPEval_Normal1_XMR,
              MAPEval_SGED2_XMR, MAPEval_SST2_XMR, MAPEval_JSU2_XMR, MAPEval_Normal2_XMR,
              MAPEval_SGED3_XMR, MAPEval_SST3_XMR, MAPEval_JSU3_XMR, MAPEval_Normal3_XMR)

MAPE_XMR <- matrix(data = MAPE_XMR, nrow = 4)
colnames(MAPE_XMR) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_XMR) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: E T H E R E U M  C L A S S I C ::::::::##########
# Full Sample

garchsigSGED_ETC <- CrCu_ETC_Roll6@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll6@forecast$density$Realized)
MAPEgvalSGED_ETC <- MAPE(garchsigSGED_ETC, realval_ETC)

garchsigSST_ETC <- CrCu_ETC_Roll5@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll5@forecast$density$Realized)
MAPEgvalSST_ETC <- MAPE(garchsigSST_ETC, realval_ETC)

garchsigJSU_ETC <- CrCu_ETC_Roll7@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll7@forecast$density$Realized)
MAPEgvalJSU_ETC <- MAPE(garchsigJSU_ETC, realval_ETC)

garchsigNorm_ETC <- CrCu_ETC_Roll8@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll8@forecast$density$Realized)
MAPEgvalNorm_ETC <- MAPE(garchsigNorm_ETC, realval_ETC)

# Sample 1

garchsig_SGED1_ETC <- CrCu_ETC_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_ETC <- abs(CrCu_ETC_Roll_SGED_Sample1@forecast$density$Realized)
MAPEval_SGED1_ETC <- MAPE(garchsig_SGED1_ETC, realval_SGED1_ETC)

garchsig_SST1_ETC <- CrCu_ETC_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_ETC <- abs(CrCu_ETC_Roll_SST_Sample1@forecast$density$Realized)
MAPEval_SST1_ETC <- MAPE(garchsig_SST1_ETC, realval_SST1_ETC)

garchsig_JSU1_ETC <- CrCu_ETC_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_ETC <- abs(CrCu_ETC_Roll_JSU_Sample1@forecast$density$Realized)
MAPEval_JSU1_ETC <- MAPE(garchsig_JSU1_ETC, realval_JSU1_ETC)

garchsig_Normal1_ETC <- CrCu_ETC_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_ETC <- abs(CrCu_ETC_Roll_Normal_Sample1@forecast$density$Realized)
MAPEval_Normal1_ETC <- MAPE(garchsig_Normal1_ETC, realval_Normal1_ETC)

# Sample 2

garchsig_SGED2_ETC <- CrCu_ETC_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_ETC <- abs(CrCu_ETC_Roll_SGED_Sample2@forecast$density$Realized)
MAPEval_SGED2_ETC <- MAPE(garchsig_SGED2_ETC, realval_SGED2_ETC)

garchsig_SST2_ETC <- CrCu_ETC_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_ETC <- abs(CrCu_ETC_Roll_SST_Sample2@forecast$density$Realized)
MAPEval_SST2_ETC <- MAPE(garchsig_SST2_ETC, realval_SST2_ETC)

garchsig_JSU2_ETC <- CrCu_ETC_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_ETC <- abs(CrCu_ETC_Roll_JSU_Sample2@forecast$density$Realized)
MAPEval_JSU2_ETC <- MAPE(garchsig_JSU2_ETC, realval_JSU2_ETC)

garchsig_Normal2_ETC <- CrCu_ETC_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_ETC <- abs(CrCu_ETC_Roll_Normal_Sample2@forecast$density$Realized)
MAPEval_Normal2_ETC <- MAPE(garchsig_Normal2_ETC, realval_Normal2_ETC)

# Sample 3

garchsig_SGED3_ETC <- CrCu_ETC_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_ETC <- abs(CrCu_ETC_Roll_SGED_Sample3@forecast$density$Realized)
MAPEval_SGED3_ETC <- MAPE(garchsig_SGED3_ETC, realval_SGED3_ETC)

garchsig_SST3_ETC <- CrCu_ETC_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_ETC <- abs(CrCu_ETC_Roll_SST_Sample3@forecast$density$Realized)
MAPEval_SST3_ETC <- MAPE(garchsig_SST3_ETC, realval_SST3_ETC)

garchsig_JSU3_ETC <- CrCu_ETC_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_ETC <- abs(CrCu_ETC_Roll_JSU_Sample3@forecast$density$Realized)
MAPEval_JSU3_ETC <- MAPE(garchsig_JSU3_ETC, realval_JSU3_ETC)

garchsig_Normal3_ETC <- CrCu_ETC_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_ETC <- abs(CrCu_ETC_Roll_Normal_Sample3@forecast$density$Realized)
MAPEval_Normal3_ETC <- MAPE(garchsig_Normal3_ETC, realval_Normal3_ETC)

MAPE_ETC <- c(MAPEgvalSGED_ETC, MAPEgvalSST_ETC, MAPEgvalJSU_ETC, MAPEgvalNorm_ETC, 
              MAPEval_SGED1_ETC, MAPEval_SST1_ETC, MAPEval_JSU1_ETC, MAPEval_Normal1_ETC,
              MAPEval_SGED2_ETC, MAPEval_SST2_ETC, MAPEval_JSU2_ETC, MAPEval_Normal2_ETC,
              MAPEval_SGED3_ETC, MAPEval_SST3_ETC, MAPEval_JSU3_ETC, MAPEval_Normal3_ETC)

MAPE_ETC <- matrix(data = MAPE_ETC, nrow = 4)
colnames(MAPE_ETC) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_ETC) <- c("SGED", "SST", "JSU", "Normal")


MAPE_crypto <- rbind("BTC", round(MAPE_BTC,5),
                     "ETH", round(MAPE_ETH,5),
                     "LTC", round(MAPE_LTC,5),
                     "XMR", round(MAPE_XMR,5),
                     "ETC", round(MAPE_ETC,5))

View(MAPE_crypto)

xtable(MAPE_crypto)

############::::::::::::::: R M S E ::::::::::::::::::##############

###############::::::::::::::::: B I T C O I N ::::::::##########
# Full Sample

garchsigSGED_BTC <- CrCu_BTC_Roll6@forecast$density$Sigma
realval_BTC <- abs(CrCu_BTC_Roll6@forecast$density$Realized)
RMSEgvalSGED_BTC <- RMSE(garchsigSGED_BTC, realval_BTC)

garchsigSST_BTC <- CrCu_BTC_Roll5@forecast$density$Sigma
realval_BTC <- abs(CrCu_BTC_Roll5@forecast$density$Realized)
RMSEgvalSST_BTC <- RMSE(garchsigSST_BTC, realval_BTC)

garchsigJSU_BTC <- CrCu_BTC_Roll7@forecast$density$Sigma
realval_BTC <- abs(CrCu_BTC_Roll7@forecast$density$Realized)
RMSEgvalJSU_BTC <- RMSE(garchsigJSU_BTC, realval_BTC)

garchsigNorm_BTC <- CrCu_BTC_Roll8@forecast$density$Sigma
realval_BTC <- abs(CrCu_BTC_Roll8@forecast$density$Realized)
RMSEgvalNorm_BTC <- RMSE(garchsigNorm_BTC, realval_BTC)

# Sample 1

garchsig_SGED1_BTC <- CrCu_BTC_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_BTC <- abs(CrCu_BTC_Roll_SGED_Sample1@forecast$density$Realized)
RMSEval_SGED1_BTC <- RMSE(garchsig_SGED1_BTC, realval_SGED1_BTC)

garchsig_SST1_BTC <- CrCu_BTC_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_BTC <- abs(CrCu_BTC_Roll_SST_Sample1@forecast$density$Realized)
RMSEval_SST1_BTC <- RMSE(garchsig_SST1_BTC, realval_SST1_BTC)

garchsig_JSU1_BTC <- CrCu_BTC_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_BTC <- abs(CrCu_BTC_Roll_JSU_Sample1@forecast$density$Realized)
RMSEval_JSU1_BTC <- RMSE(garchsig_JSU1_BTC, realval_JSU1_BTC)

garchsig_Normal1_BTC <- CrCu_BTC_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_BTC <- abs(CrCu_BTC_Roll_Normal_Sample1@forecast$density$Realized)
RMSEval_Normal1_BTC <- RMSE(garchsig_Normal1_BTC, realval_Normal1_BTC)

# Sample 2

garchsig_SGED2_BTC <- CrCu_BTC_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_BTC <- abs(CrCu_BTC_Roll_SGED_Sample2@forecast$density$Realized)
RMSEval_SGED2_BTC <- RMSE(garchsig_SGED2_BTC, realval_SGED2_BTC)

garchsig_SST2_BTC <- CrCu_BTC_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_BTC <- abs(CrCu_BTC_Roll_SST_Sample2@forecast$density$Realized)
RMSEval_SST2_BTC <- RMSE(garchsig_SST2_BTC, realval_SST2_BTC)

garchsig_JSU2_BTC <- CrCu_BTC_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_BTC <- abs(CrCu_BTC_Roll_JSU_Sample2@forecast$density$Realized)
RMSEval_JSU2_BTC <- RMSE(garchsig_JSU2_BTC, realval_JSU2_BTC)

garchsig_Normal2_BTC <- CrCu_BTC_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_BTC <- abs(CrCu_BTC_Roll_Normal_Sample2@forecast$density$Realized)
RMSEval_Normal2_BTC <- RMSE(garchsig_Normal2_BTC, realval_Normal2_BTC)

# Sample 3

garchsig_SGED3_BTC <- CrCu_BTC_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_BTC <- abs(CrCu_BTC_Roll_SGED_Sample3@forecast$density$Realized)
RMSEval_SGED3_BTC <- RMSE(garchsig_SGED3_BTC, realval_SGED3_BTC)

garchsig_SST3_BTC <- CrCu_BTC_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_BTC <- abs(CrCu_BTC_Roll_SST_Sample3@forecast$density$Realized)
RMSEval_SST3_BTC <- RMSE(garchsig_SST3_BTC, realval_SST3_BTC)

garchsig_JSU3_BTC <- CrCu_BTC_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_BTC <- abs(CrCu_BTC_Roll_JSU_Sample3@forecast$density$Realized)
RMSEval_JSU3_BTC <- RMSE(garchsig_JSU3_BTC, realval_JSU3_BTC)

garchsig_Normal3_BTC <- CrCu_BTC_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_BTC <- abs(CrCu_BTC_Roll_Normal_Sample3@forecast$density$Realized)
RMSEval_Normal3_BTC <- RMSE(garchsig_Normal3_BTC, realval_Normal3_BTC)

RMSE_BTC <- c(RMSEgvalSGED_BTC, RMSEgvalSST_BTC, RMSEgvalJSU_BTC, RMSEgvalNorm_BTC, 
              RMSEval_SGED1_BTC, RMSEval_SST1_BTC, RMSEval_JSU1_BTC, RMSEval_Normal1_BTC,
              RMSEval_SGED2_BTC, RMSEval_SST2_BTC, RMSEval_JSU2_BTC, RMSEval_Normal2_BTC,
              RMSEval_SGED3_BTC, RMSEval_SST3_BTC, RMSEval_JSU3_BTC, RMSEval_Normal3_BTC)

RMSE_BTC <- matrix(data = RMSE_BTC, nrow = 4)
colnames(RMSE_BTC) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_BTC) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: E T H E R E U M ::::::::##########
# Full Sample

garchsigSGED_ETH <- CrCu_ETH_Roll6@forecast$density$Sigma
realval_ETH <- abs(CrCu_ETH_Roll6@forecast$density$Realized)
RMSEgvalSGED_ETH <- RMSE(garchsigSGED_ETH, realval_ETH)

garchsigSST_ETH <- CrCu_ETH_Roll5@forecast$density$Sigma
realval_ETH <- abs(CrCu_ETH_Roll5@forecast$density$Realized)
RMSEgvalSST_ETH <- RMSE(garchsigSST_ETH, realval_ETH)

garchsigJSU_ETH <- CrCu_ETH_Roll7@forecast$density$Sigma
realval_ETH <- abs(CrCu_ETH_Roll7@forecast$density$Realized)
RMSEgvalJSU_ETH <- RMSE(garchsigJSU_ETH, realval_ETH)

garchsigNorm_ETH <- CrCu_ETH_Roll8@forecast$density$Sigma
realval_ETH <- abs(CrCu_ETH_Roll8@forecast$density$Realized)
RMSEgvalNorm_ETH <- RMSE(garchsigNorm_ETH, realval_ETH)

# Sample 1

garchsig_SGED1_ETH <- CrCu_ETH_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_ETH <- abs(CrCu_ETH_Roll_SGED_Sample1@forecast$density$Realized)
RMSEval_SGED1_ETH <- RMSE(garchsig_SGED1_ETH, realval_SGED1_ETH)

garchsig_SST1_ETH <- CrCu_ETH_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_ETH <- abs(CrCu_ETH_Roll_SST_Sample1@forecast$density$Realized)
RMSEval_SST1_ETH <- RMSE(garchsig_SST1_ETH, realval_SST1_ETH)

garchsig_JSU1_ETH <- CrCu_ETH_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_ETH <- abs(CrCu_ETH_Roll_JSU_Sample1@forecast$density$Realized)
RMSEval_JSU1_ETH <- RMSE(garchsig_JSU1_ETH, realval_JSU1_ETH)

garchsig_Normal1_ETH <- CrCu_ETH_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_ETH <- abs(CrCu_ETH_Roll_Normal_Sample1@forecast$density$Realized)
RMSEval_Normal1_ETH <- RMSE(garchsig_Normal1_ETH, realval_Normal1_ETH)

# Sample 2

garchsig_SGED2_ETH <- CrCu_ETH_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_ETH <- abs(CrCu_ETH_Roll_SGED_Sample2@forecast$density$Realized)
RMSEval_SGED2_ETH <- RMSE(garchsig_SGED2_ETH, realval_SGED2_ETH)

garchsig_SST2_ETH <- CrCu_ETH_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_ETH <- abs(CrCu_ETH_Roll_SST_Sample2@forecast$density$Realized)
RMSEval_SST2_ETH <- RMSE(garchsig_SST2_ETH, realval_SST2_ETH)

garchsig_JSU2_ETH <- CrCu_ETH_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_ETH <- abs(CrCu_ETH_Roll_JSU_Sample2@forecast$density$Realized)
RMSEval_JSU2_ETH <- RMSE(garchsig_JSU2_ETH, realval_JSU2_ETH)

garchsig_Normal2_ETH <- CrCu_ETH_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_ETH <- abs(CrCu_ETH_Roll_Normal_Sample2@forecast$density$Realized)
RMSEval_Normal2_ETH <- RMSE(garchsig_Normal2_ETH, realval_Normal2_ETH)

# Sample 3

garchsig_SGED3_ETH <- CrCu_ETH_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_ETH <- abs(CrCu_ETH_Roll_SGED_Sample3@forecast$density$Realized)
RMSEval_SGED3_ETH <- RMSE(garchsig_SGED3_ETH, realval_SGED3_ETH)

garchsig_SST3_ETH <- CrCu_ETH_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_ETH <- abs(CrCu_ETH_Roll_SST_Sample3@forecast$density$Realized)
RMSEval_SST3_ETH <- RMSE(garchsig_SST3_ETH, realval_SST3_ETH)

garchsig_JSU3_ETH <- CrCu_ETH_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_ETH <- abs(CrCu_ETH_Roll_JSU_Sample3@forecast$density$Realized)
RMSEval_JSU3_ETH <- RMSE(garchsig_JSU3_ETH, realval_JSU3_ETH)

garchsig_Normal3_ETH <- CrCu_ETH_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_ETH <- abs(CrCu_ETH_Roll_Normal_Sample3@forecast$density$Realized)
RMSEval_Normal3_ETH <- RMSE(garchsig_Normal3_ETH, realval_Normal3_ETH)

RMSE_ETH <- c(RMSEgvalSGED_ETH, RMSEgvalSST_ETH, RMSEgvalJSU_ETH, RMSEgvalNorm_ETH, 
              RMSEval_SGED1_ETH, RMSEval_SST1_ETH, RMSEval_JSU1_ETH, RMSEval_Normal1_ETH,
              RMSEval_SGED2_ETH, RMSEval_SST2_ETH, RMSEval_JSU2_ETH, RMSEval_Normal2_ETH,
              RMSEval_SGED3_ETH, RMSEval_SST3_ETH, RMSEval_JSU3_ETH, RMSEval_Normal3_ETH)

RMSE_ETH <- matrix(data = RMSE_ETH, nrow = 4)
colnames(RMSE_ETH) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_ETH) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: L I T E C O I N ::::::::##########
# Full Sample

garchsigSGED_LTC <- CrCu_LTC_Roll6@forecast$density$Sigma
realval_LTC <- abs(CrCu_LTC_Roll6@forecast$density$Realized)
RMSEgvalSGED_LTC <- RMSE(garchsigSGED_LTC, realval_LTC)

garchsigSST_LTC <- CrCu_LTC_Roll5@forecast$density$Sigma
realval_LTC <- abs(CrCu_LTC_Roll5@forecast$density$Realized)
RMSEgvalSST_LTC <- RMSE(garchsigSST_LTC, realval_LTC)

garchsigJSU_LTC <- CrCu_LTC_Roll7@forecast$density$Sigma
realval_LTC <- abs(CrCu_LTC_Roll7@forecast$density$Realized)
RMSEgvalJSU_LTC <- RMSE(garchsigJSU_LTC, realval_LTC)

garchsigNorm_LTC <- CrCu_LTC_Roll8@forecast$density$Sigma
realval_LTC <- abs(CrCu_LTC_Roll8@forecast$density$Realized)
RMSEgvalNorm_LTC <- RMSE(garchsigNorm_LTC, realval_LTC)

# Sample 1

garchsig_SGED1_LTC <- CrCu_LTC_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_LTC <- abs(CrCu_LTC_Roll_SGED_Sample1@forecast$density$Realized)
RMSEval_SGED1_LTC <- RMSE(garchsig_SGED1_LTC, realval_SGED1_LTC)

garchsig_SST1_LTC <- CrCu_LTC_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_LTC <- abs(CrCu_LTC_Roll_SST_Sample1@forecast$density$Realized)
RMSEval_SST1_LTC <- RMSE(garchsig_SST1_LTC, realval_SST1_LTC)

garchsig_JSU1_LTC <- CrCu_LTC_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_LTC <- abs(CrCu_LTC_Roll_JSU_Sample1@forecast$density$Realized)
RMSEval_JSU1_LTC <- RMSE(garchsig_JSU1_LTC, realval_JSU1_LTC)

garchsig_Normal1_LTC <- CrCu_LTC_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_LTC <- abs(CrCu_LTC_Roll_Normal_Sample1@forecast$density$Realized)
RMSEval_Normal1_LTC <- RMSE(garchsig_Normal1_LTC, realval_Normal1_LTC)

# Sample 2

garchsig_SGED2_LTC <- CrCu_LTC_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_LTC <- abs(CrCu_LTC_Roll_SGED_Sample2@forecast$density$Realized)
RMSEval_SGED2_LTC <- RMSE(garchsig_SGED2_LTC, realval_SGED2_LTC)

garchsig_SST2_LTC <- CrCu_LTC_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_LTC <- abs(CrCu_LTC_Roll_SST_Sample2@forecast$density$Realized)
RMSEval_SST2_LTC <- RMSE(garchsig_SST2_LTC, realval_SST2_LTC)

garchsig_JSU2_LTC <- CrCu_LTC_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_LTC <- abs(CrCu_LTC_Roll_JSU_Sample2@forecast$density$Realized)
RMSEval_JSU2_LTC <- RMSE(garchsig_JSU2_LTC, realval_JSU2_LTC)

garchsig_Normal2_LTC <- CrCu_LTC_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_LTC <- abs(CrCu_LTC_Roll_Normal_Sample2@forecast$density$Realized)
RMSEval_Normal2_LTC <- RMSE(garchsig_Normal2_LTC, realval_Normal2_LTC)

# Sample 3

garchsig_SGED3_LTC <- CrCu_LTC_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_LTC <- abs(CrCu_LTC_Roll_SGED_Sample3@forecast$density$Realized)
RMSEval_SGED3_LTC <- RMSE(garchsig_SGED3_LTC, realval_SGED3_LTC)

garchsig_SST3_LTC <- CrCu_LTC_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_LTC <- abs(CrCu_LTC_Roll_SST_Sample3@forecast$density$Realized)
RMSEval_SST3_LTC <- RMSE(garchsig_SST3_LTC, realval_SST3_LTC)

garchsig_JSU3_LTC <- CrCu_LTC_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_LTC <- abs(CrCu_LTC_Roll_JSU_Sample3@forecast$density$Realized)
RMSEval_JSU3_LTC <- RMSE(garchsig_JSU3_LTC, realval_JSU3_LTC)

garchsig_Normal3_LTC <- CrCu_LTC_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_LTC <- abs(CrCu_LTC_Roll_Normal_Sample3@forecast$density$Realized)
RMSEval_Normal3_LTC <- RMSE(garchsig_Normal3_LTC, realval_Normal3_LTC)

RMSE_LTC <- c(RMSEgvalSGED_LTC, RMSEgvalSST_LTC, RMSEgvalJSU_LTC, RMSEgvalNorm_LTC, 
              RMSEval_SGED1_LTC, RMSEval_SST1_LTC, RMSEval_JSU1_LTC, RMSEval_Normal1_LTC,
              RMSEval_SGED2_LTC, RMSEval_SST2_LTC, RMSEval_JSU2_LTC, RMSEval_Normal2_LTC,
              RMSEval_SGED3_LTC, RMSEval_SST3_LTC, RMSEval_JSU3_LTC, RMSEval_Normal3_LTC)

RMSE_LTC <- matrix(data = RMSE_LTC, nrow = 4)
colnames(RMSE_LTC) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_LTC) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: M O N E R O ::::::::##########
# Full Sample

garchsigSGED_XMR <- CrCu_XMR_Roll6@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll6@forecast$density$Realized)
RMSEgvalSGED_XMR <- RMSE(garchsigSGED_XMR, realval_XMR)

garchsigSST_XMR <- CrCu_XMR_Roll5@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll5@forecast$density$Realized)
RMSEgvalSST_XMR <- RMSE(garchsigSST_XMR, realval_XMR)

garchsigJSU_XMR <- CrCu_XMR_Roll7@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll7@forecast$density$Realized)
RMSEgvalJSU_XMR <- RMSE(garchsigJSU_XMR, realval_XMR)

garchsigNorm_XMR <- CrCu_XMR_Roll8@forecast$density$Sigma
realval_XMR <- abs(CrCu_XMR_Roll8@forecast$density$Realized)
RMSEgvalNorm_XMR <- RMSE(garchsigNorm_XMR, realval_XMR)

# Sample 1

garchsig_SGED1_XMR <- CrCu_XMR_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_XMR <- abs(CrCu_XMR_Roll_SGED_Sample1@forecast$density$Realized)
RMSEval_SGED1_XMR <- RMSE(garchsig_SGED1_XMR, realval_SGED1_XMR)

garchsig_SST1_XMR <- CrCu_XMR_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_XMR <- abs(CrCu_XMR_Roll_SST_Sample1@forecast$density$Realized)
RMSEval_SST1_XMR <- RMSE(garchsig_SST1_XMR, realval_SST1_XMR)

garchsig_JSU1_XMR <- CrCu_XMR_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_XMR <- abs(CrCu_XMR_Roll_JSU_Sample1@forecast$density$Realized)
RMSEval_JSU1_XMR <- RMSE(garchsig_JSU1_XMR, realval_JSU1_XMR)

garchsig_Normal1_XMR <- CrCu_XMR_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_XMR <- abs(CrCu_XMR_Roll_Normal_Sample1@forecast$density$Realized)
RMSEval_Normal1_XMR <- RMSE(garchsig_Normal1_XMR, realval_Normal1_XMR)

# Sample 2

garchsig_SGED2_XMR <- CrCu_XMR_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_XMR <- abs(CrCu_XMR_Roll_SGED_Sample2@forecast$density$Realized)
RMSEval_SGED2_XMR <- RMSE(garchsig_SGED2_XMR, realval_SGED2_XMR)

garchsig_SST2_XMR <- CrCu_XMR_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_XMR <- abs(CrCu_XMR_Roll_SST_Sample2@forecast$density$Realized)
RMSEval_SST2_XMR <- RMSE(garchsig_SST2_XMR, realval_SST2_XMR)

garchsig_JSU2_XMR <- CrCu_XMR_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_XMR <- abs(CrCu_XMR_Roll_JSU_Sample2@forecast$density$Realized)
RMSEval_JSU2_XMR <- RMSE(garchsig_JSU2_XMR, realval_JSU2_XMR)

garchsig_Normal2_XMR <- CrCu_XMR_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_XMR <- abs(CrCu_XMR_Roll_Normal_Sample2@forecast$density$Realized)
RMSEval_Normal2_XMR <- RMSE(garchsig_Normal2_XMR, realval_Normal2_XMR)

# Sample 3

garchsig_SGED3_XMR <- CrCu_XMR_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_XMR <- abs(CrCu_XMR_Roll_SGED_Sample3@forecast$density$Realized)
RMSEval_SGED3_XMR <- RMSE(garchsig_SGED3_XMR, realval_SGED3_XMR)

garchsig_SST3_XMR <- CrCu_XMR_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_XMR <- abs(CrCu_XMR_Roll_SST_Sample3@forecast$density$Realized)
RMSEval_SST3_XMR <- RMSE(garchsig_SST3_XMR, realval_SST3_XMR)

garchsig_JSU3_XMR <- CrCu_XMR_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_XMR <- abs(CrCu_XMR_Roll_JSU_Sample3@forecast$density$Realized)
RMSEval_JSU3_XMR <- RMSE(garchsig_JSU3_XMR, realval_JSU3_XMR)

garchsig_Normal3_XMR <- CrCu_XMR_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_XMR <- abs(CrCu_XMR_Roll_Normal_Sample3@forecast$density$Realized)
RMSEval_Normal3_XMR <- RMSE(garchsig_Normal3_XMR, realval_Normal3_XMR)

RMSE_XMR <- c(RMSEgvalSGED_XMR, RMSEgvalSST_XMR, RMSEgvalJSU_XMR, RMSEgvalNorm_XMR, 
              RMSEval_SGED1_XMR, RMSEval_SST1_XMR, RMSEval_JSU1_XMR, RMSEval_Normal1_XMR,
              RMSEval_SGED2_XMR, RMSEval_SST2_XMR, RMSEval_JSU2_XMR, RMSEval_Normal2_XMR,
              RMSEval_SGED3_XMR, RMSEval_SST3_XMR, RMSEval_JSU3_XMR, RMSEval_Normal3_XMR)

RMSE_XMR <- matrix(data = RMSE_XMR, nrow = 4)
colnames(RMSE_XMR) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_XMR) <- c("SGED", "SST", "JSU", "Normal")

###############::::::::::::::::: E T H E R E U M  C L A S S I C ::::::::##########
# Full Sample

garchsigSGED_ETC <- CrCu_ETC_Roll6@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll6@forecast$density$Realized)
RMSEgvalSGED_ETC <- RMSE(garchsigSGED_ETC, realval_ETC)

garchsigSST_ETC <- CrCu_ETC_Roll5@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll5@forecast$density$Realized)
RMSEgvalSST_ETC <- RMSE(garchsigSST_ETC, realval_ETC)

garchsigJSU_ETC <- CrCu_ETC_Roll7@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll7@forecast$density$Realized)
RMSEgvalJSU_ETC <- RMSE(garchsigJSU_ETC, realval_ETC)

garchsigNorm_ETC <- CrCu_ETC_Roll8@forecast$density$Sigma
realval_ETC <- abs(CrCu_ETC_Roll8@forecast$density$Realized)
RMSEgvalNorm_ETC <- RMSE(garchsigNorm_ETC, realval_ETC)

# Sample 1

garchsig_SGED1_ETC <- CrCu_ETC_Roll_SGED_Sample1@forecast$density$Sigma
realval_SGED1_ETC <- abs(CrCu_ETC_Roll_SGED_Sample1@forecast$density$Realized)
RMSEval_SGED1_ETC <- RMSE(garchsig_SGED1_ETC, realval_SGED1_ETC)

garchsig_SST1_ETC <- CrCu_ETC_Roll_SST_Sample1@forecast$density$Sigma
realval_SST1_ETC <- abs(CrCu_ETC_Roll_SST_Sample1@forecast$density$Realized)
RMSEval_SST1_ETC <- RMSE(garchsig_SST1_ETC, realval_SST1_ETC)

garchsig_JSU1_ETC <- CrCu_ETC_Roll_JSU_Sample1@forecast$density$Sigma
realval_JSU1_ETC <- abs(CrCu_ETC_Roll_JSU_Sample1@forecast$density$Realized)
RMSEval_JSU1_ETC <- RMSE(garchsig_JSU1_ETC, realval_JSU1_ETC)

garchsig_Normal1_ETC <- CrCu_ETC_Roll_Normal_Sample1@forecast$density$Sigma
realval_Normal1_ETC <- abs(CrCu_ETC_Roll_Normal_Sample1@forecast$density$Realized)
RMSEval_Normal1_ETC <- RMSE(garchsig_Normal1_ETC, realval_Normal1_ETC)

# Sample 2

garchsig_SGED2_ETC <- CrCu_ETC_Roll_SGED_Sample2@forecast$density$Sigma
realval_SGED2_ETC <- abs(CrCu_ETC_Roll_SGED_Sample2@forecast$density$Realized)
RMSEval_SGED2_ETC <- RMSE(garchsig_SGED2_ETC, realval_SGED2_ETC)

garchsig_SST2_ETC <- CrCu_ETC_Roll_SST_Sample2@forecast$density$Sigma
realval_SST2_ETC <- abs(CrCu_ETC_Roll_SST_Sample2@forecast$density$Realized)
RMSEval_SST2_ETC <- RMSE(garchsig_SST2_ETC, realval_SST2_ETC)

garchsig_JSU2_ETC <- CrCu_ETC_Roll_JSU_Sample2@forecast$density$Sigma
realval_JSU2_ETC <- abs(CrCu_ETC_Roll_JSU_Sample2@forecast$density$Realized)
RMSEval_JSU2_ETC <- RMSE(garchsig_JSU2_ETC, realval_JSU2_ETC)

garchsig_Normal2_ETC <- CrCu_ETC_Roll_Normal_Sample2@forecast$density$Sigma
realval_Normal2_ETC <- abs(CrCu_ETC_Roll_Normal_Sample2@forecast$density$Realized)
RMSEval_Normal2_ETC <- RMSE(garchsig_Normal2_ETC, realval_Normal2_ETC)

# Sample 3

garchsig_SGED3_ETC <- CrCu_ETC_Roll_SGED_Sample3@forecast$density$Sigma
realval_SGED3_ETC <- abs(CrCu_ETC_Roll_SGED_Sample3@forecast$density$Realized)
RMSEval_SGED3_ETC <- RMSE(garchsig_SGED3_ETC, realval_SGED3_ETC)

garchsig_SST3_ETC <- CrCu_ETC_Roll_SST_Sample3@forecast$density$Sigma
realval_SST3_ETC <- abs(CrCu_ETC_Roll_SST_Sample3@forecast$density$Realized)
RMSEval_SST3_ETC <- RMSE(garchsig_SST3_ETC, realval_SST3_ETC)

garchsig_JSU3_ETC <- CrCu_ETC_Roll_JSU_Sample3@forecast$density$Sigma
realval_JSU3_ETC <- abs(CrCu_ETC_Roll_JSU_Sample3@forecast$density$Realized)
RMSEval_JSU3_ETC <- RMSE(garchsig_JSU3_ETC, realval_JSU3_ETC)

garchsig_Normal3_ETC <- CrCu_ETC_Roll_Normal_Sample3@forecast$density$Sigma
realval_Normal3_ETC <- abs(CrCu_ETC_Roll_Normal_Sample3@forecast$density$Realized)
RMSEval_Normal3_ETC <- RMSE(garchsig_Normal3_ETC, realval_Normal3_ETC)

RMSE_ETC <- c(RMSEgvalSGED_ETC, RMSEgvalSST_ETC, RMSEgvalJSU_ETC, RMSEgvalNorm_ETC, 
              RMSEval_SGED1_ETC, RMSEval_SST1_ETC, RMSEval_JSU1_ETC, RMSEval_Normal1_ETC,
              RMSEval_SGED2_ETC, RMSEval_SST2_ETC, RMSEval_JSU2_ETC, RMSEval_Normal2_ETC,
              RMSEval_SGED3_ETC, RMSEval_SST3_ETC, RMSEval_JSU3_ETC, RMSEval_Normal3_ETC)

RMSE_ETC <- matrix(data = RMSE_ETC, nrow = 4)
colnames(RMSE_ETC) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_ETC) <- c("SGED", "SST", "JSU", "Normal")


RMSE_crypto <- rbind("BTC", round(RMSE_BTC,5),
                     "ETH", round(RMSE_ETH,5),
                     "LTC", round(RMSE_LTC,5),
                     "XMR", round(RMSE_XMR,5),
                     "ETC", round(RMSE_ETC,5))

View(RMSE_crypto)

xtable(RMSE_crypto)



