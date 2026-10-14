# =============================================================================
# 10_actual_error_garch_gas.R
#
# Purpose : Actual-over-expected exceedance ratio (AE) of 1% VaR for EGARCH and GAS
#           models.
# Inputs  : objects from 03a-06
# Outputs : AE_<coin>, AE_<coin>_wo, AE_<coin>_GAS
# Paper   : combined table in 13
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

################################################

#### ::::: G A R C H ::::: ####

################################################

### B T C ###
# Full Sample
BTC_Test_Norm_1  <- BacktestVaR(CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,1],0.01)
BTC_Test_SST_1  <- BacktestVaR(CrCu_BTC_VaR_SST[,4],CrCu_BTC_VaR_SST[,1],0.01)
BTC_Test_SGED_1  <- BacktestVaR(CrCu_BTC_VaR_SGED[,4],CrCu_BTC_VaR_SGED[,1],0.01)
BTC_Test_JSU_1  <- BacktestVaR(CrCu_BTC_VaR_JSU[,4],CrCu_BTC_VaR_JSU[,1],0.01)
BTC_Test_PSIV_1  <- BacktestVaR(CrCu_BTC_VaR_Norm[,4],CrCu_BTC_VaR_Norm[,1],0.01)

AE_BTC_Norm0 <- BTC_Test_Norm_1$AE
AE_BTC_SST0 <- BTC_Test_SST_1$AE
AE_BTC_SGED0 <- BTC_Test_SGED_1$AE
AE_BTC_JSU0 <- BTC_Test_JSU_1$AE
AE_BTC_PS0 <- BTC_Test_PSIV_1$AE

# Sample 1
BTC_Test_Normal1_Sample1 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample1@forecast$VaR[,1],0.01)
BTC_Test_SST1_Sample1 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample1@forecast$VaR[,1],0.01)
BTC_Test_SGED1_Sample1 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample1@forecast$VaR[,1],0.01)
BTC_Test_JSU1_Sample1 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample1@forecast$VaR[,1],0.01)
BTC_Test_PSIV1_Sample1 <- BacktestVaR(CrCu_BTC_VaR_SGED1[,4],Sample1_BTC_Holdout_VaR_01,0.01)

AE_BTC_Norm1 <- BTC_Test_Normal1_Sample1$AE
AE_BTC_SST1 <- BTC_Test_SST1_Sample1$AE
AE_BTC_SGED1 <- BTC_Test_SGED1_Sample1$AE
AE_BTC_JSU1 <- BTC_Test_JSU1_Sample1$AE
AE_BTC_PS1 <- BTC_Test_PSIV1_Sample1$AE

##### Sample 2
BTC_Test_Normal1_Sample2 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample2@forecast$VaR[,1],0.01)
BTC_Test_SST1_Sample2 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample2@forecast$VaR[,1],0.01)
BTC_Test_SGED1_Sample2 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample2@forecast$VaR[,1],0.01)
BTC_Test_JSU1_Sample2 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample2@forecast$VaR[,1],0.01)
BTC_Test_PSIV1_Sample2 <- BacktestVaR(CrCu_BTC_VaR_SGED2[,4],Sample2_BTC_Holdout_VaR_01,0.01)

AE_BTC_Norm2 <- BTC_Test_Normal1_Sample2$AE
AE_BTC_SST2 <- BTC_Test_SST1_Sample2$AE
AE_BTC_SGED2 <- BTC_Test_SGED1_Sample2$AE
AE_BTC_JSU2 <- BTC_Test_JSU1_Sample2$AE
AE_BTC_PS2 <- BTC_Test_PSIV1_Sample2$AE

### Sample3
BTC_Test_Normal1_Sample3 <- BacktestVaR(CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_Normal_Sample3@forecast$VaR[,1],0.01)
BTC_Test_SST1_Sample3 <- BacktestVaR(CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SST_Sample3@forecast$VaR[,1],0.01)
BTC_Test_SGED1_Sample3 <- BacktestVaR(CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_SGED_Sample3@forecast$VaR[,1],0.01)
BTC_Test_JSU1_Sample3 <- BacktestVaR(CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_BTC_Roll_JSU_Sample3@forecast$VaR[,1],0.01)
BTC_Test_PSIV1_Sample3 <- BacktestVaR(CrCu_BTC_VaR_SGED3[,4],Sample3_BTC_Holdout_VaR_01,0.01)

AE_BTC_Norm3 <- BTC_Test_Normal1_Sample3$AE
AE_BTC_SST3 <- BTC_Test_SST1_Sample3$AE
AE_BTC_SGED3 <- BTC_Test_SGED1_Sample3$AE
AE_BTC_JSU3 <- BTC_Test_JSU1_Sample3$AE
AE_BTC_PS3 <- BTC_Test_PSIV1_Sample3$AE

AE_BTC_Val <- c(AE_BTC_Norm0, AE_BTC_Norm1, AE_BTC_Norm2, AE_BTC_Norm3,
                AE_BTC_SST0, AE_BTC_SST1, AE_BTC_SST2,AE_BTC_SST3,
                AE_BTC_SGED0, AE_BTC_SGED1, AE_BTC_SGED2, AE_BTC_SGED3,
                AE_BTC_JSU0, AE_BTC_JSU1, AE_BTC_JSU2, AE_BTC_JSU3,
                AE_BTC_PS0, AE_BTC_PS1, AE_BTC_PS2, AE_BTC_PS3)

AE_BTC <- matrix(round(AE_BTC_Val,4), nrow = 4)
colnames(AE_BTC) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_BTC) <- c("FS", "SS1", "SS2", "SS3")

### E T H ###
# Full Sample
ETH_Test_Norm_1  <- BacktestVaR(CrCu_ETH_VaR_Norm[,4],CrCu_ETH_VaR_Norm[,1],0.01)
ETH_Test_SST_1  <- BacktestVaR(CrCu_ETH_VaR_SST[,4],CrCu_ETH_VaR_SST[,1],0.01)
ETH_Test_SGED_1  <- BacktestVaR(CrCu_ETH_VaR_SGED[,4],CrCu_ETH_VaR_SGED[,1],0.01)
ETH_Test_JSU_1  <- BacktestVaR(CrCu_ETH_VaR_JSU[,4],CrCu_ETH_VaR_JSU[,1],0.01)
ETH_Test_PSIV_1  <- BacktestVaR(CrCu_ETH_VaR_Norm[,4],CrCu_ETH_VaR_Norm[,1],0.01)

AE_ETH_Norm0 <- ETH_Test_Norm_1$AE
AE_ETH_SST0 <- ETH_Test_SST_1$AE
AE_ETH_SGED0 <- ETH_Test_SGED_1$AE
AE_ETH_JSU0 <- ETH_Test_JSU_1$AE
AE_ETH_PS0 <- ETH_Test_PSIV_1$AE

# Sample 1
ETH_Test_Normal1_Sample1 <- BacktestVaR(CrCu_ETH_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_ETH_Roll_Normal_Sample1@forecast$VaR[,1],0.01)
ETH_Test_SST1_Sample1 <- BacktestVaR(CrCu_ETH_Roll_SST_Sample1@forecast$VaR[,4],CrCu_ETH_Roll_SST_Sample1@forecast$VaR[,1],0.01)
ETH_Test_SGED1_Sample1 <- BacktestVaR(CrCu_ETH_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_ETH_Roll_SGED_Sample1@forecast$VaR[,1],0.01)
ETH_Test_JSU1_Sample1 <- BacktestVaR(CrCu_ETH_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_ETH_Roll_JSU_Sample1@forecast$VaR[,1],0.01)
ETH_Test_PSIV1_Sample1 <- BacktestVaR(CrCu_ETH_VaR_SGED1[,4],Sample1_ETH_Holdout_VaR_01,0.01)

AE_ETH_Norm1 <- ETH_Test_Normal1_Sample1$AE
AE_ETH_SST1 <- ETH_Test_SST1_Sample1$AE
AE_ETH_SGED1 <- ETH_Test_SGED1_Sample1$AE
AE_ETH_JSU1 <- ETH_Test_JSU1_Sample1$AE
AE_ETH_PS1 <- ETH_Test_PSIV1_Sample1$AE

##### Sample 2
ETH_Test_Normal1_Sample2 <- BacktestVaR(CrCu_ETH_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_ETH_Roll_Normal_Sample2@forecast$VaR[,1],0.01)
ETH_Test_SST1_Sample2 <- BacktestVaR(CrCu_ETH_Roll_SST_Sample2@forecast$VaR[,4],CrCu_ETH_Roll_SST_Sample2@forecast$VaR[,1],0.01)
ETH_Test_SGED1_Sample2 <- BacktestVaR(CrCu_ETH_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_ETH_Roll_SGED_Sample2@forecast$VaR[,1],0.01)
ETH_Test_JSU1_Sample2 <- BacktestVaR(CrCu_ETH_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_ETH_Roll_JSU_Sample2@forecast$VaR[,1],0.01)
ETH_Test_PSIV1_Sample2 <- BacktestVaR(CrCu_ETH_VaR_SGED2[,4],Sample2_ETH_Holdout_VaR_01,0.01)

AE_ETH_Norm2 <- ETH_Test_Normal1_Sample2$AE
AE_ETH_SST2 <- ETH_Test_SST1_Sample2$AE
AE_ETH_SGED2 <- ETH_Test_SGED1_Sample2$AE
AE_ETH_JSU2 <- ETH_Test_JSU1_Sample2$AE
AE_ETH_PS2 <- ETH_Test_PSIV1_Sample2$AE

### Sample3
ETH_Test_Normal1_Sample3 <- BacktestVaR(CrCu_ETH_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_ETH_Roll_Normal_Sample3@forecast$VaR[,1],0.01)
ETH_Test_SST1_Sample3 <- BacktestVaR(CrCu_ETH_Roll_SST_Sample3@forecast$VaR[,4],CrCu_ETH_Roll_SST_Sample3@forecast$VaR[,1],0.01)
ETH_Test_SGED1_Sample3 <- BacktestVaR(CrCu_ETH_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_ETH_Roll_SGED_Sample3@forecast$VaR[,1],0.01)
ETH_Test_JSU1_Sample3 <- BacktestVaR(CrCu_ETH_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_ETH_Roll_JSU_Sample3@forecast$VaR[,1],0.01)
ETH_Test_PSIV1_Sample3 <- BacktestVaR(CrCu_ETH_VaR_SGED3[,4],Sample3_ETH_Holdout_VaR_01,0.01)

AE_ETH_Norm3 <- ETH_Test_Normal1_Sample3$AE
AE_ETH_SST3 <- ETH_Test_SST1_Sample3$AE
AE_ETH_SGED3 <- ETH_Test_SGED1_Sample3$AE
AE_ETH_JSU3 <- ETH_Test_JSU1_Sample3$AE
AE_ETH_PS3 <- ETH_Test_PSIV1_Sample3$AE

AE_ETH_Val <- c(AE_ETH_Norm0, AE_ETH_Norm1, AE_ETH_Norm2, AE_ETH_Norm3,
                AE_ETH_SST0, AE_ETH_SST1, AE_ETH_SST2,AE_ETH_SST3,
                AE_ETH_SGED0, AE_ETH_SGED1, AE_ETH_SGED2, AE_ETH_SGED3,
                AE_ETH_JSU0, AE_ETH_JSU1, AE_ETH_JSU2, AE_ETH_JSU3,
                AE_ETH_PS0, AE_ETH_PS1, AE_ETH_PS2, AE_ETH_PS3)

AE_ETH <- matrix(round(AE_ETH_Val,4), nrow = 4)
colnames(AE_ETH) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_ETH) <- c("FS", "SS1", "SS2", "SS3")

### L T C ###
# Full Sample
LTC_Test_Norm_1  <- BacktestVaR(CrCu_LTC_VaR_Norm[,4],CrCu_LTC_VaR_Norm[,1],0.01)
LTC_Test_SST_1  <- BacktestVaR(CrCu_LTC_VaR_SST[,4],CrCu_LTC_VaR_SST[,1],0.01)
LTC_Test_SGED_1  <- BacktestVaR(CrCu_LTC_VaR_SGED[,4],CrCu_LTC_VaR_SGED[,1],0.01)
LTC_Test_JSU_1  <- BacktestVaR(CrCu_LTC_VaR_JSU[,4],CrCu_LTC_VaR_JSU[,1],0.01)
LTC_Test_PSIV_1  <- BacktestVaR(CrCu_LTC_VaR_Norm[,4],CrCu_LTC_VaR_Norm[,1],0.01)

AE_LTC_Norm0 <- LTC_Test_Norm_1$AE
AE_LTC_SST0 <- LTC_Test_SST_1$AE
AE_LTC_SGED0 <- LTC_Test_SGED_1$AE
AE_LTC_JSU0 <- LTC_Test_JSU_1$AE
AE_LTC_PS0 <- LTC_Test_PSIV_1$AE

# Sample 1
LTC_Test_Normal1_Sample1 <- BacktestVaR(CrCu_LTC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_LTC_Roll_Normal_Sample1@forecast$VaR[,1],0.01)
LTC_Test_SST1_Sample1 <- BacktestVaR(CrCu_LTC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_LTC_Roll_SST_Sample1@forecast$VaR[,1],0.01)
LTC_Test_SGED1_Sample1 <- BacktestVaR(CrCu_LTC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_LTC_Roll_SGED_Sample1@forecast$VaR[,1],0.01)
LTC_Test_JSU1_Sample1 <- BacktestVaR(CrCu_LTC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_LTC_Roll_JSU_Sample1@forecast$VaR[,1],0.01)
LTC_Test_PSIV1_Sample1 <- BacktestVaR(CrCu_LTC_VaR_SGED1[,4],Sample1_LTC_Holdout_VaR_01,0.01)

AE_LTC_Norm1 <- 0
AE_LTC_SST1 <- LTC_Test_SST1_Sample1$AE
AE_LTC_SGED1 <- LTC_Test_SGED1_Sample1$AE
AE_LTC_JSU1 <- LTC_Test_JSU1_Sample1$AE
AE_LTC_PS1 <- 0

##### Sample 2
LTC_Test_Normal1_Sample2 <- BacktestVaR(CrCu_LTC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_LTC_Roll_Normal_Sample2@forecast$VaR[,1],0.01)
LTC_Test_SST1_Sample2 <- BacktestVaR(CrCu_LTC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_LTC_Roll_SST_Sample2@forecast$VaR[,1],0.01)
LTC_Test_SGED1_Sample2 <- BacktestVaR(CrCu_LTC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_LTC_Roll_SGED_Sample2@forecast$VaR[,1],0.01)
LTC_Test_JSU1_Sample2 <- BacktestVaR(CrCu_LTC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_LTC_Roll_JSU_Sample2@forecast$VaR[,1],0.01)
LTC_Test_PSIV1_Sample2 <- BacktestVaR(CrCu_LTC_VaR_SGED2[,4],Sample2_LTC_Holdout_VaR_01,0.01)

AE_LTC_Norm2 <- LTC_Test_Normal1_Sample2$AE
AE_LTC_SST2 <- LTC_Test_SST1_Sample2$AE
AE_LTC_SGED2 <- LTC_Test_SGED1_Sample2$AE
AE_LTC_JSU2 <- LTC_Test_JSU1_Sample2$AE
AE_LTC_PS2 <- LTC_Test_PSIV1_Sample2$AE

### Sample3
LTC_Test_Normal1_Sample3 <- BacktestVaR(CrCu_LTC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_LTC_Roll_Normal_Sample3@forecast$VaR[,1],0.01)
LTC_Test_SST1_Sample3 <- BacktestVaR(CrCu_LTC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_LTC_Roll_SST_Sample3@forecast$VaR[,1],0.01)
LTC_Test_SGED1_Sample3 <- BacktestVaR(CrCu_LTC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_LTC_Roll_SGED_Sample3@forecast$VaR[,1],0.01)
LTC_Test_JSU1_Sample3 <- BacktestVaR(CrCu_LTC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_LTC_Roll_JSU_Sample3@forecast$VaR[,1],0.01)
LTC_Test_PSIV1_Sample3 <- BacktestVaR(CrCu_LTC_VaR_SGED3[,4],Sample3_LTC_Holdout_VaR_01,0.01)

AE_LTC_Norm3 <- LTC_Test_Normal1_Sample3$AE
AE_LTC_SST3 <- LTC_Test_SST1_Sample3$AE
AE_LTC_SGED3 <- LTC_Test_SGED1_Sample3$AE
AE_LTC_JSU3 <- LTC_Test_JSU1_Sample3$AE
AE_LTC_PS3 <- LTC_Test_PSIV1_Sample3$AE

AE_LTC_Val <- c(AE_LTC_Norm0, AE_LTC_Norm1, AE_LTC_Norm2, AE_LTC_Norm3,
                AE_LTC_SST0, AE_LTC_SST1, AE_LTC_SST2,AE_LTC_SST3,
                AE_LTC_SGED0, AE_LTC_SGED1, AE_LTC_SGED2, AE_LTC_SGED3,
                AE_LTC_JSU0, AE_LTC_JSU1, AE_LTC_JSU2, AE_LTC_JSU3,
                AE_LTC_PS0, AE_LTC_PS1, AE_LTC_PS2, AE_LTC_PS3)

AE_LTC <- matrix(round(AE_LTC_Val,4), nrow = 4)
colnames(AE_LTC) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_LTC) <- c("FS", "SS1", "SS2", "SS3")

### X M R ###
# Full Sample
XMR_Test_Norm_1  <- BacktestVaR(CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,1],0.01)
XMR_Test_SST_1  <- BacktestVaR(CrCu_XMR_VaR_SST[,4],CrCu_XMR_VaR_SST[,1],0.01)
XMR_Test_SGED_1  <- BacktestVaR(CrCu_XMR_VaR_SGED[,4],CrCu_XMR_VaR_SGED[,1],0.01)
XMR_Test_JSU_1  <- BacktestVaR(CrCu_XMR_VaR_JSU[,4],CrCu_XMR_VaR_JSU[,1],0.01)
XMR_Test_PSIV_1  <- BacktestVaR(CrCu_XMR_VaR_Norm[,4],CrCu_XMR_VaR_Norm[,1],0.01)

AE_XMR_Norm0 <- XMR_Test_Norm_1$AE
AE_XMR_SST0 <- XMR_Test_SST_1$AE
AE_XMR_SGED0 <- XMR_Test_SGED_1$AE
AE_XMR_JSU0 <- XMR_Test_JSU_1$AE
AE_XMR_PS0 <- XMR_Test_PSIV_1$AE

# Sample 1
XMR_Test_Normal1_Sample1 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample1@forecast$VaR[,1],0.01)
XMR_Test_SST1_Sample1 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample1@forecast$VaR[,1],0.01)
XMR_Test_SGED1_Sample1 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample1@forecast$VaR[,1],0.01)
XMR_Test_JSU1_Sample1 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample1@forecast$VaR[,1],0.01)
XMR_Test_PSIV1_Sample1 <- BacktestVaR(CrCu_XMR_VaR_SGED1[,4],Sample1_XMR_Holdout_VaR_01,0.01)

AE_XMR_Norm1 <- XMR_Test_Normal1_Sample1$AE
AE_XMR_SST1 <- XMR_Test_SST1_Sample1$AE
AE_XMR_SGED1 <- XMR_Test_SGED1_Sample1$AE
AE_XMR_JSU1 <- XMR_Test_JSU1_Sample1$AE
AE_XMR_PS1 <- XMR_Test_PSIV1_Sample1$AE

##### Sample 2
XMR_Test_Normal1_Sample2 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample2@forecast$VaR[,1],0.01)
XMR_Test_SST1_Sample2 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample2@forecast$VaR[,1],0.01)
XMR_Test_SGED1_Sample2 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample2@forecast$VaR[,1],0.01)
XMR_Test_JSU1_Sample2 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample2@forecast$VaR[,1],0.01)
XMR_Test_PSIV1_Sample2 <- BacktestVaR(CrCu_XMR_VaR_SGED2[,4],Sample2_XMR_Holdout_VaR_01,0.01)

AE_XMR_Norm2 <- XMR_Test_Normal1_Sample2$AE
AE_XMR_SST2 <- XMR_Test_SST1_Sample2$AE
AE_XMR_SGED2 <- XMR_Test_SGED1_Sample2$AE
AE_XMR_JSU2 <- XMR_Test_JSU1_Sample2$AE
AE_XMR_PS2 <- XMR_Test_PSIV1_Sample2$AE

### Sample3
XMR_Test_Normal1_Sample3 <- BacktestVaR(CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_Normal_Sample3@forecast$VaR[,1],0.01)
XMR_Test_SST1_Sample3 <- BacktestVaR(CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SST_Sample3@forecast$VaR[,1],0.01)
XMR_Test_SGED1_Sample3 <- BacktestVaR(CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_SGED_Sample3@forecast$VaR[,1],0.01)
XMR_Test_JSU1_Sample3 <- BacktestVaR(CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_XMR_Roll_JSU_Sample3@forecast$VaR[,1],0.01)
XMR_Test_PSIV1_Sample3 <- BacktestVaR(CrCu_XMR_VaR_SGED3[,4],Sample3_XMR_Holdout_VaR_01,0.01)

AE_XMR_Norm3 <- XMR_Test_Normal1_Sample3$AE
AE_XMR_SST3 <- XMR_Test_SST1_Sample3$AE
AE_XMR_SGED3 <- XMR_Test_SGED1_Sample3$AE
AE_XMR_JSU3 <- XMR_Test_JSU1_Sample3$AE
AE_XMR_PS3 <- XMR_Test_PSIV1_Sample3$AE

AE_XMR_Val <- c(AE_XMR_Norm0, AE_XMR_Norm1, AE_XMR_Norm2, AE_XMR_Norm3,
                AE_XMR_SST0, AE_XMR_SST1, AE_XMR_SST2,AE_XMR_SST3,
                AE_XMR_SGED0, AE_XMR_SGED1, AE_XMR_SGED2, AE_XMR_SGED3,
                AE_XMR_JSU0, AE_XMR_JSU1, AE_XMR_JSU2, AE_XMR_JSU3,
                AE_XMR_PS0, AE_XMR_PS1, AE_XMR_PS2, AE_XMR_PS3)

AE_XMR <- matrix(round(AE_XMR_Val,4), nrow = 4)
colnames(AE_XMR) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_XMR) <- c("FS", "SS1", "SS2", "SS3")

### E T C ###
# Full Sample
ETC_Test_Norm_1  <- BacktestVaR(CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,1],0.01)
ETC_Test_SST_1  <- BacktestVaR(CrCu_ETC_VaR_SST[,4],CrCu_ETC_VaR_SST[,1],0.01)
ETC_Test_SGED_1  <- BacktestVaR(CrCu_ETC_VaR_SGED[,4],CrCu_ETC_VaR_SGED[,1],0.01)
ETC_Test_JSU_1  <- BacktestVaR(CrCu_ETC_VaR_JSU[,4],CrCu_ETC_VaR_JSU[,1],0.01)
ETC_Test_PSIV_1  <- BacktestVaR(CrCu_ETC_VaR_Norm[,4],CrCu_ETC_VaR_Norm[,1],0.01)

AE_ETC_Norm0 <- ETC_Test_Norm_1$AE
AE_ETC_SST0 <- ETC_Test_SST_1$AE
AE_ETC_SGED0 <- ETC_Test_SGED_1$AE
AE_ETC_JSU0 <- ETC_Test_JSU_1$AE
AE_ETC_PS0 <- ETC_Test_PSIV_1$AE

# Sample 1
ETC_Test_Normal1_Sample1 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample1@forecast$VaR[,1],0.01)
ETC_Test_SST1_Sample1 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample1@forecast$VaR[,1],0.01)
ETC_Test_SGED1_Sample1 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample1@forecast$VaR[,1],0.01)
ETC_Test_JSU1_Sample1 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample1@forecast$VaR[,1],0.01)
ETC_Test_PSIV1_Sample1 <- BacktestVaR(CrCu_ETC_VaR_SGED1[,4],Sample1_ETC_Holdout_VaR_01,0.01)

AE_ETC_Norm1 <- ETC_Test_Normal1_Sample1$AE
AE_ETC_SST1 <- ETC_Test_SST1_Sample1$AE
AE_ETC_SGED1 <- ETC_Test_SGED1_Sample1$AE
AE_ETC_JSU1 <- ETC_Test_JSU1_Sample1$AE
AE_ETC_PS1 <- ETC_Test_PSIV1_Sample1$AE

##### Sample 2
ETC_Test_Normal1_Sample2 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample2@forecast$VaR[,1],0.01)
ETC_Test_SST1_Sample2 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample2@forecast$VaR[,1],0.01)
ETC_Test_SGED1_Sample2 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample2@forecast$VaR[,1],0.01)
ETC_Test_JSU1_Sample2 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample2@forecast$VaR[,1],0.01)
ETC_Test_PSIV1_Sample2 <- BacktestVaR(CrCu_ETC_VaR_SGED2[,4],Sample2_ETC_Holdout_VaR_01,0.01)

AE_ETC_Norm2 <- ETC_Test_Normal1_Sample2$AE
AE_ETC_SST2 <- ETC_Test_SST1_Sample2$AE
AE_ETC_SGED2 <- ETC_Test_SGED1_Sample2$AE
AE_ETC_JSU2 <- ETC_Test_JSU1_Sample2$AE
AE_ETC_PS2 <- ETC_Test_PSIV1_Sample2$AE

### Sample3
ETC_Test_Normal1_Sample3 <- BacktestVaR(CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_Normal_Sample3@forecast$VaR[,1],0.01)
ETC_Test_SST1_Sample3 <- BacktestVaR(CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SST_Sample3@forecast$VaR[,1],0.01)
ETC_Test_SGED1_Sample3 <- BacktestVaR(CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_SGED_Sample3@forecast$VaR[,1],0.01)
ETC_Test_JSU1_Sample3 <- BacktestVaR(CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,4],CrCu_ETC_Roll_JSU_Sample3@forecast$VaR[,1],0.01)
ETC_Test_PSIV1_Sample3 <- BacktestVaR(CrCu_ETC_VaR_SGED3[,4],Sample3_ETC_Holdout_VaR_01,0.01)

AE_ETC_Norm3 <- ETC_Test_Normal1_Sample3$AE
AE_ETC_SST3 <- ETC_Test_SST1_Sample3$AE
AE_ETC_SGED3 <- ETC_Test_SGED1_Sample3$AE
AE_ETC_JSU3 <- ETC_Test_JSU1_Sample3$AE
AE_ETC_PS3 <- ETC_Test_PSIV1_Sample3$AE

AE_ETC_Val <- c(AE_ETC_Norm0, AE_ETC_Norm1, AE_ETC_Norm2, AE_ETC_Norm3,
                AE_ETC_SST0, AE_ETC_SST1, AE_ETC_SST2,AE_ETC_SST3,
                AE_ETC_SGED0, AE_ETC_SGED1, AE_ETC_SGED2, AE_ETC_SGED3,
                AE_ETC_JSU0, AE_ETC_JSU1, AE_ETC_JSU2, AE_ETC_JSU3,
                AE_ETC_PS0, AE_ETC_PS1, AE_ETC_PS2, AE_ETC_PS3)

AE_ETC <- matrix(round(AE_ETC_Val,4), nrow = 4)
colnames(AE_ETC) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_ETC) <- c("FS", "SS1", "SS2", "SS3")

################################################

#### ::::: G A S ::::: ####

################################################

### B T C ###
# Full Sample
VaRBacktest_BTC_norm0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_norm0, alpha = alpha)
VaRBacktest_BTC_std0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_std0, alpha = alpha)
VaRBacktest_BTC_sstd0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_sstd0, alpha = alpha)
VaRBacktest_BTC_ast0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_ast0, alpha = alpha)
VaRBacktest_BTC_astone0 <- BacktestVaR(data = VR1_BTC[,4] , VaR = VaR_BTC_astone0, alpha = alpha)
AE_BTC_norm0 <- VaRBacktest_BTC_norm0$AE
AE_BTC_std0 <- VaRBacktest_BTC_std0$AE
AE_BTC_sstd0 <- VaRBacktest_BTC_sstd0$AE
AE_BTC_ast0 <- VaRBacktest_BTC_ast0$AE
AE_BTC_astone0 <- VaRBacktest_BTC_astone0$AE

VaRBacktest_BTC_norm1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_norm1, alpha = alpha)
VaRBacktest_BTC_std1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_std1, alpha = alpha)
VaRBacktest_BTC_sstd1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_sstd1, alpha = alpha)
VaRBacktest_BTC_ast1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_ast1, alpha = alpha)
VaRBacktest_BTC_astone1 <- BacktestVaR(data = VR11_BTC[,4] , VaR = VaR_BTC_astone1, alpha = alpha)
AE_BTC_norm1 <- VaRBacktest_BTC_norm1$AE
AE_BTC_std1 <- VaRBacktest_BTC_std1$AE
AE_BTC_sstd1 <- VaRBacktest_BTC_sstd1$AE
AE_BTC_ast1 <- VaRBacktest_BTC_ast1$AE
AE_BTC_astone1 <- VaRBacktest_BTC_astone1$AE

VaRBacktest_BTC_norm2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_norm2, alpha = alpha)
VaRBacktest_BTC_std2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_std2, alpha = alpha)
VaRBacktest_BTC_sstd2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_sstd2, alpha = alpha)
VaRBacktest_BTC_ast2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_ast2, alpha = alpha)
VaRBacktest_BTC_astone2 <- BacktestVaR(data = VR21_BTC[,4] , VaR = VaR_BTC_astone2, alpha = alpha)
AE_BTC_norm2 <- VaRBacktest_BTC_norm2$AE
AE_BTC_std2 <- VaRBacktest_BTC_std2$AE
AE_BTC_sstd2 <- VaRBacktest_BTC_sstd2$AE
AE_BTC_ast2 <- VaRBacktest_BTC_ast2$AE
AE_BTC_astone2 <- VaRBacktest_BTC_astone2$AE

VaRBacktest_BTC_norm3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_norm3, alpha = alpha)
VaRBacktest_BTC_std3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_std3, alpha = alpha)
VaRBacktest_BTC_sstd3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_sstd3, alpha = alpha)
VaRBacktest_BTC_ast3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_ast3, alpha = alpha)
VaRBacktest_BTC_astone3 <- BacktestVaR(data = VR31_BTC[,4] , VaR = VaR_BTC_astone3, alpha = alpha)
AE_BTC_norm3 <- VaRBacktest_BTC_norm3$AE
AE_BTC_std3 <- VaRBacktest_BTC_std3$AE
AE_BTC_sstd3 <- VaRBacktest_BTC_sstd3$AE
AE_BTC_ast3 <- VaRBacktest_BTC_ast3$AE
AE_BTC_astone3 <- VaRBacktest_BTC_astone3$AE

AE_BTC_Val_GAS <- c(AE_BTC_norm0, AE_BTC_norm1, AE_BTC_norm2, AE_BTC_norm3,
                AE_BTC_std0, AE_BTC_std1, AE_BTC_std2,AE_BTC_std3,
                AE_BTC_sstd0, AE_BTC_sstd1, AE_BTC_sstd2, AE_BTC_sstd3,
                AE_BTC_ast0, AE_BTC_ast1, AE_BTC_ast2, AE_BTC_ast3,
                AE_BTC_astone0, AE_BTC_astone1, AE_BTC_astone2, AE_BTC_astone3)

AE_BTC_GAS <- matrix(round(AE_BTC_Val_GAS,4), nrow = 4)
colnames(AE_BTC_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(AE_BTC_GAS) <- c("FS", "SS1", "SS2", "SS3")

### E T H ###
# Full Sample
VaRBacktest_ETH_norm0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_norm0, alpha = alpha)
VaRBacktest_ETH_std0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_std0, alpha = alpha)
VaRBacktest_ETH_sstd0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_sstd0, alpha = alpha)
VaRBacktest_ETH_ast0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_ast0, alpha = alpha)
VaRBacktest_ETH_astone0 <- BacktestVaR(data = VR1_ETH[,4] , VaR = VaR_ETH_astone0, alpha = alpha)
AE_ETH_norm0 <- VaRBacktest_ETH_norm0$AE
AE_ETH_std0 <- VaRBacktest_ETH_std0$AE
AE_ETH_sstd0 <- VaRBacktest_ETH_sstd0$AE
AE_ETH_ast0 <- VaRBacktest_ETH_ast0$AE
AE_ETH_astone0 <- VaRBacktest_ETH_astone0$AE

VaRBacktest_ETH_norm1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_norm1, alpha = alpha)
VaRBacktest_ETH_std1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_std1, alpha = alpha)
VaRBacktest_ETH_sstd1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_sstd1, alpha = alpha)
VaRBacktest_ETH_ast1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_ast1, alpha = alpha)
VaRBacktest_ETH_astone1 <- BacktestVaR(data = VR11_ETH[,4] , VaR = VaR_ETH_astone1, alpha = alpha)
AE_ETH_norm1 <- VaRBacktest_ETH_norm1$AE
AE_ETH_std1 <- VaRBacktest_ETH_std1$AE
AE_ETH_sstd1 <- VaRBacktest_ETH_sstd1$AE
AE_ETH_ast1 <- VaRBacktest_ETH_ast1$AE
AE_ETH_astone1 <- VaRBacktest_ETH_astone1$AE

VaRBacktest_ETH_norm2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_norm2, alpha = alpha)
VaRBacktest_ETH_std2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_std2, alpha = alpha)
VaRBacktest_ETH_sstd2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_sstd2, alpha = alpha)
VaRBacktest_ETH_ast2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_ast2, alpha = alpha)
VaRBacktest_ETH_astone2 <- BacktestVaR(data = VR21_ETH[,4] , VaR = VaR_ETH_astone2, alpha = alpha)
AE_ETH_norm2 <- VaRBacktest_ETH_norm2$AE
AE_ETH_std2 <- VaRBacktest_ETH_std2$AE
AE_ETH_sstd2 <- VaRBacktest_ETH_sstd2$AE
AE_ETH_ast2 <- VaRBacktest_ETH_ast2$AE
AE_ETH_astone2 <- VaRBacktest_ETH_astone2$AE

VaRBacktest_ETH_norm3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_norm3, alpha = alpha)
VaRBacktest_ETH_std3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_std3, alpha = alpha)
VaRBacktest_ETH_sstd3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_sstd3, alpha = alpha)
VaRBacktest_ETH_ast3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_ast3, alpha = alpha)
VaRBacktest_ETH_astone3 <- BacktestVaR(data = VR31_ETH[,4] , VaR = VaR_ETH_astone3, alpha = alpha)
AE_ETH_norm3 <- VaRBacktest_ETH_norm3$AE
AE_ETH_std3 <- VaRBacktest_ETH_std3$AE
AE_ETH_sstd3 <- VaRBacktest_ETH_sstd3$AE
AE_ETH_ast3 <- VaRBacktest_ETH_ast3$AE
AE_ETH_astone3 <- VaRBacktest_ETH_astone3$AE

AE_ETH_Val_GAS <- c(AE_ETH_norm0, AE_ETH_norm1, AE_ETH_norm2, AE_ETH_norm3,
                    AE_ETH_std0, AE_ETH_std1, AE_ETH_std2,AE_ETH_std3,
                    AE_ETH_sstd0, AE_ETH_sstd1, AE_ETH_sstd2, AE_ETH_sstd3,
                    AE_ETH_ast0, AE_ETH_ast1, AE_ETH_ast2, AE_ETH_ast3,
                    AE_ETH_astone0, AE_ETH_astone1, AE_ETH_astone2, AE_ETH_astone3)

AE_ETH_GAS <- matrix(round(AE_ETH_Val_GAS,4), nrow = 4)
colnames(AE_ETH_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(AE_ETH_GAS) <- c("FS", "SS1", "SS2", "SS3")

### L T C ###
# Full Sample
VaRBacktest_LTC_norm0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_norm0, alpha = alpha)
VaRBacktest_LTC_std0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_std0, alpha = alpha)
VaRBacktest_LTC_sstd0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_sstd0, alpha = alpha)
VaRBacktest_LTC_ast0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_ast0, alpha = alpha)
VaRBacktest_LTC_astone0 <- BacktestVaR(data = VR1_LTC[,4] , VaR = VaR_LTC_astone0, alpha = alpha)
AE_LTC_norm0 <- VaRBacktest_LTC_norm0$AE
AE_LTC_std0 <- VaRBacktest_LTC_std0$AE
AE_LTC_sstd0 <- VaRBacktest_LTC_sstd0$AE
AE_LTC_ast0 <- VaRBacktest_LTC_ast0$AE
AE_LTC_astone0 <- VaRBacktest_LTC_astone0$AE

VaRBacktest_LTC_norm1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_norm1, alpha = alpha)
VaRBacktest_LTC_std1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_std1, alpha = alpha)
VaRBacktest_LTC_sstd1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_sstd1, alpha = alpha)
VaRBacktest_LTC_ast1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_ast1, alpha = alpha)
VaRBacktest_LTC_astone1 <- BacktestVaR(data = VR11_LTC[,4] , VaR = VaR_LTC_astone1, alpha = alpha)
AE_LTC_norm1 <- VaRBacktest_LTC_norm1$AE
AE_LTC_std1 <- VaRBacktest_LTC_std1$AE
AE_LTC_sstd1 <- VaRBacktest_LTC_sstd1$AE
AE_LTC_ast1 <- VaRBacktest_LTC_ast1$AE
AE_LTC_astone1 <- VaRBacktest_LTC_astone1$AE

VaRBacktest_LTC_norm2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_norm2, alpha = alpha)
VaRBacktest_LTC_std2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_std2, alpha = alpha)
VaRBacktest_LTC_sstd2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_sstd2, alpha = alpha)
VaRBacktest_LTC_ast2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_ast2, alpha = alpha)
VaRBacktest_LTC_astone2 <- BacktestVaR(data = VR21_LTC[,4] , VaR = VaR_LTC_astone2, alpha = alpha)
AE_LTC_norm2 <- VaRBacktest_LTC_norm2$AE
AE_LTC_std2 <- VaRBacktest_LTC_std2$AE
AE_LTC_sstd2 <- VaRBacktest_LTC_sstd2$AE
AE_LTC_ast2 <- VaRBacktest_LTC_ast2$AE
AE_LTC_astone2 <- VaRBacktest_LTC_astone2$AE

VaRBacktest_LTC_norm3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_norm3, alpha = alpha)
VaRBacktest_LTC_std3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_std3, alpha = alpha)
VaRBacktest_LTC_sstd3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_sstd3, alpha = alpha)
VaRBacktest_LTC_ast3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_ast3, alpha = alpha)
VaRBacktest_LTC_astone3 <- BacktestVaR(data = VR31_LTC[,4] , VaR = VaR_LTC_astone3, alpha = alpha)
AE_LTC_norm3 <- VaRBacktest_LTC_norm3$AE
AE_LTC_std3 <- VaRBacktest_LTC_std3$AE
AE_LTC_sstd3 <- VaRBacktest_LTC_sstd3$AE
AE_LTC_ast3 <- VaRBacktest_LTC_ast3$AE
AE_LTC_astone3 <- VaRBacktest_LTC_astone3$AE

AE_LTC_Val_GAS <- c(AE_LTC_norm0, AE_LTC_norm1, AE_LTC_norm2, AE_LTC_norm3,
                    AE_LTC_std0, AE_LTC_std1, AE_LTC_std2,AE_LTC_std3,
                    AE_LTC_sstd0, AE_LTC_sstd1, AE_LTC_sstd2, AE_LTC_sstd3,
                    AE_LTC_ast0, AE_LTC_ast1, AE_LTC_ast2, AE_LTC_ast3,
                    AE_LTC_astone0, AE_LTC_astone1, AE_LTC_astone2, AE_LTC_astone3)

AE_LTC_GAS <- matrix(round(AE_LTC_Val_GAS,4), nrow = 4)
colnames(AE_LTC_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(AE_LTC_GAS) <- c("FS", "SS1", "SS2", "SS3")

### X M R ###
# Full Sample
VaRBacktest_XMR_norm0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_norm0, alpha = alpha)
VaRBacktest_XMR_std0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_std0, alpha = alpha)
VaRBacktest_XMR_sstd0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_sstd0, alpha = alpha)
VaRBacktest_XMR_ast0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_ast0, alpha = alpha)
VaRBacktest_XMR_astone0 <- BacktestVaR(data = VR1_XMR[,4] , VaR = VaR_XMR_astone0, alpha = alpha)
AE_XMR_norm0 <- VaRBacktest_XMR_norm0$AE
AE_XMR_std0 <- VaRBacktest_XMR_std0$AE
AE_XMR_sstd0 <- VaRBacktest_XMR_sstd0$AE
AE_XMR_ast0 <- VaRBacktest_XMR_ast0$AE
AE_XMR_astone0 <- VaRBacktest_XMR_astone0$AE

VaRBacktest_XMR_norm1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_norm1, alpha = alpha)
VaRBacktest_XMR_std1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_std1, alpha = alpha)
VaRBacktest_XMR_sstd1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_sstd1, alpha = alpha)
VaRBacktest_XMR_ast1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_ast1, alpha = alpha)
VaRBacktest_XMR_astone1 <- BacktestVaR(data = VR11_XMR[,4] , VaR = VaR_XMR_astone1, alpha = alpha)
AE_XMR_norm1 <- VaRBacktest_XMR_norm1$AE
AE_XMR_std1 <- VaRBacktest_XMR_std1$AE
AE_XMR_sstd1 <- VaRBacktest_XMR_sstd1$AE
AE_XMR_ast1 <- VaRBacktest_XMR_ast1$AE
AE_XMR_astone1 <- VaRBacktest_XMR_astone1$AE

VaRBacktest_XMR_norm2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_norm2, alpha = alpha)
VaRBacktest_XMR_std2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_std2, alpha = alpha)
VaRBacktest_XMR_sstd2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_sstd2, alpha = alpha)
VaRBacktest_XMR_ast2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_ast2, alpha = alpha)
VaRBacktest_XMR_astone2 <- BacktestVaR(data = VR21_XMR[,4] , VaR = VaR_XMR_astone2, alpha = alpha)
AE_XMR_norm2 <- VaRBacktest_XMR_norm2$AE
AE_XMR_std2 <- VaRBacktest_XMR_std2$AE
AE_XMR_sstd2 <- VaRBacktest_XMR_sstd2$AE
AE_XMR_ast2 <- VaRBacktest_XMR_ast2$AE
AE_XMR_astone2 <- VaRBacktest_XMR_astone2$AE

VaRBacktest_XMR_norm3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_norm3, alpha = alpha)
VaRBacktest_XMR_std3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_std3, alpha = alpha)
VaRBacktest_XMR_sstd3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_sstd3, alpha = alpha)
VaRBacktest_XMR_ast3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_ast3, alpha = alpha)
VaRBacktest_XMR_astone3 <- BacktestVaR(data = VR31_XMR[,4] , VaR = VaR_XMR_astone3, alpha = alpha)
AE_XMR_norm3 <- VaRBacktest_XMR_norm3$AE
AE_XMR_std3 <- VaRBacktest_XMR_std3$AE
AE_XMR_sstd3 <- VaRBacktest_XMR_sstd3$AE
AE_XMR_ast3 <- VaRBacktest_XMR_ast3$AE
AE_XMR_astone3 <- VaRBacktest_XMR_astone3$AE

AE_XMR_Val_GAS <- c(AE_XMR_norm0, AE_XMR_norm1, AE_XMR_norm2, AE_XMR_norm3,
                    AE_XMR_std0, AE_XMR_std1, AE_XMR_std2,AE_XMR_std3,
                    AE_XMR_sstd0, AE_XMR_sstd1, AE_XMR_sstd2, AE_XMR_sstd3,
                    AE_XMR_ast0, AE_XMR_ast1, AE_XMR_ast2, AE_XMR_ast3,
                    AE_XMR_astone0, AE_XMR_astone1, AE_XMR_astone2, AE_XMR_astone3)

AE_XMR_GAS <- matrix(round(AE_XMR_Val_GAS,4), nrow = 4)
colnames(AE_XMR_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(AE_XMR_GAS) <- c("FS", "SS1", "SS2", "SS3")

### E T C ###
# Full Sample
VaRBacktest_ETC_norm0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_norm0, alpha = alpha)
VaRBacktest_ETC_std0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_std0, alpha = alpha)
VaRBacktest_ETC_sstd0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_sstd0, alpha = alpha)
VaRBacktest_ETC_ast0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_ast0, alpha = alpha)
VaRBacktest_ETC_astone0 <- BacktestVaR(data = VR1_ETC[,4] , VaR = VaR_ETC_astone0, alpha = alpha)
AE_ETC_norm0 <- VaRBacktest_ETC_norm0$AE
AE_ETC_std0 <- VaRBacktest_ETC_std0$AE
AE_ETC_sstd0 <- VaRBacktest_ETC_sstd0$AE
AE_ETC_ast0 <- VaRBacktest_ETC_ast0$AE
AE_ETC_astone0 <- VaRBacktest_ETC_astone0$AE

VaRBacktest_ETC_norm1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_norm1, alpha = alpha)
VaRBacktest_ETC_std1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_std1, alpha = alpha)
VaRBacktest_ETC_sstd1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_sstd1, alpha = alpha)
VaRBacktest_ETC_ast1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_ast1, alpha = alpha)
VaRBacktest_ETC_astone1 <- BacktestVaR(data = VR11_ETC[,4] , VaR = VaR_ETC_astone1, alpha = alpha)
AE_ETC_norm1 <- VaRBacktest_ETC_norm1$AE
AE_ETC_std1 <- VaRBacktest_ETC_std1$AE
AE_ETC_sstd1 <- VaRBacktest_ETC_sstd1$AE
AE_ETC_ast1 <- VaRBacktest_ETC_ast1$AE
AE_ETC_astone1 <- VaRBacktest_ETC_astone1$AE

VaRBacktest_ETC_norm2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_norm2, alpha = alpha)
VaRBacktest_ETC_std2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_std2, alpha = alpha)
VaRBacktest_ETC_sstd2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_sstd2, alpha = alpha)
VaRBacktest_ETC_ast2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_ast2, alpha = alpha)
VaRBacktest_ETC_astone2 <- BacktestVaR(data = VR21_ETC[,4] , VaR = VaR_ETC_astone2, alpha = alpha)
AE_ETC_norm2 <- VaRBacktest_ETC_norm2$AE
AE_ETC_std2 <- VaRBacktest_ETC_std2$AE
AE_ETC_sstd2 <- VaRBacktest_ETC_sstd2$AE
AE_ETC_ast2 <- VaRBacktest_ETC_ast2$AE
AE_ETC_astone2 <- VaRBacktest_ETC_astone2$AE

VaRBacktest_ETC_norm3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_norm3, alpha = alpha)
VaRBacktest_ETC_std3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_std3, alpha = alpha)
VaRBacktest_ETC_sstd3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_sstd3, alpha = alpha)
VaRBacktest_ETC_ast3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_ast3, alpha = alpha)
VaRBacktest_ETC_astone3 <- BacktestVaR(data = VR31_ETC[,4] , VaR = VaR_ETC_astone3, alpha = alpha)
AE_ETC_norm3 <- VaRBacktest_ETC_norm3$AE
AE_ETC_std3 <- VaRBacktest_ETC_std3$AE
AE_ETC_sstd3 <- VaRBacktest_ETC_sstd3$AE
AE_ETC_ast3 <- VaRBacktest_ETC_ast3$AE
AE_ETC_astone3 <- VaRBacktest_ETC_astone3$AE

AE_ETC_Val_GAS <- c(AE_ETC_norm0, AE_ETC_norm1, AE_ETC_norm2, AE_ETC_norm3,
                    AE_ETC_std0, AE_ETC_std1, AE_ETC_std2,AE_ETC_std3,
                    AE_ETC_sstd0, AE_ETC_sstd1, AE_ETC_sstd2, AE_ETC_sstd3,
                    AE_ETC_ast0, AE_ETC_ast1, AE_ETC_ast2, AE_ETC_ast3,
                    AE_ETC_astone0, AE_ETC_astone1, AE_ETC_astone2, AE_ETC_astone3)

AE_ETC_GAS <- matrix(round(AE_ETC_Val_GAS,4), nrow = 4)
colnames(AE_ETC_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(AE_ETC_GAS) <- c("FS", "SS1", "SS2", "SS3")

################################################

#### ::::: G A R C H (WO) ::::: ####

################################################

### B T C ###
# Full Sample
BTC_Test_Norm_1_wo  <- BacktestVaR(CrCu_BTC_VaR_Norm_wo[,4],CrCu_BTC_VaR_Norm_wo[,1],0.01)
BTC_Test_SST_1_wo  <- BacktestVaR(CrCu_BTC_VaR_SST_wo[,4],CrCu_BTC_VaR_SST_wo[,1],0.01)
BTC_Test_SGED_1_wo  <- BacktestVaR(CrCu_BTC_VaR_SGED_wo[,4],CrCu_BTC_VaR_SGED_wo[,1],0.01)
BTC_Test_JSU_1_wo  <- BacktestVaR(CrCu_BTC_VaR_JSU_wo[,4],CrCu_BTC_VaR_JSU_wo[,1],0.01)
BTC_Test_PSIV_1_wo  <- BacktestVaR(CrCu_BTC_VaR_Norm_wo[,4],CrCu_BTC_VaR_Norm_wo[,1],0.01)

AE_BTC_Norm0_wo <- BTC_Test_Norm_1_wo$AE
AE_BTC_SST0_wo <- BTC_Test_SST_1_wo$AE
AE_BTC_SGED0_wo <- BTC_Test_SGED_1_wo$AE
AE_BTC_JSU0_wo <- BTC_Test_JSU_1_wo$AE
AE_BTC_PS0_wo <- BTC_Test_PSIV_1_wo$AE

AE_BTC_Val_wo <- c(AE_BTC_Norm0_wo,
                AE_BTC_SST0_wo, 
                AE_BTC_SGED0_wo, 
                AE_BTC_JSU0_wo, 
                AE_BTC_PS0_wo)

AE_BTC_wo <- matrix(round(AE_BTC_Val_wo,4), nrow = 1)
colnames(AE_BTC_wo) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_BTC_wo) <- c("FS_WO")

### E T H ###
# Full Sample
ETH_Test_Norm_1_wo  <- BacktestVaR(CrCu_ETH_VaR_Norm_wo[,4],CrCu_ETH_VaR_Norm_wo[,1],0.01)
ETH_Test_SST_1_wo  <- BacktestVaR(CrCu_ETH_VaR_SST_wo[,4],CrCu_ETH_VaR_SST_wo[,1],0.01)
ETH_Test_SGED_1_wo  <- BacktestVaR(CrCu_ETH_VaR_SGED_wo[,4],CrCu_ETH_VaR_SGED_wo[,1],0.01)
ETH_Test_JSU_1_wo  <- BacktestVaR(CrCu_ETH_VaR_JSU_wo[,4],CrCu_ETH_VaR_JSU_wo[,1],0.01)
ETH_Test_PSIV_1_wo  <- BacktestVaR(CrCu_ETH_VaR_Norm_wo[,4],CrCu_ETH_VaR_Norm_wo[,1],0.01)

AE_ETH_Norm0_wo <- ETH_Test_Norm_1_wo$AE
AE_ETH_SST0_wo <- ETH_Test_SST_1_wo$AE
AE_ETH_SGED0_wo <- ETH_Test_SGED_1_wo$AE
AE_ETH_JSU0_wo <- ETH_Test_JSU_1_wo$AE
AE_ETH_PS0_wo <- ETH_Test_PSIV_1_wo$AE

AE_ETH_Val_wo <- c(AE_ETH_Norm0_wo,
                   AE_ETH_SST0_wo, 
                   AE_ETH_SGED0_wo, 
                   AE_ETH_JSU0_wo, 
                   AE_ETH_PS0_wo)

AE_ETH_wo <- matrix(round(AE_ETH_Val_wo,4), nrow = 1)
colnames(AE_ETH_wo) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_ETH_wo) <- c("FS_WO")

### L T C ###
# Full Sample
LTC_Test_Norm_1_wo  <- BacktestVaR(CrCu_LTC_VaR_Norm_wo[,4],CrCu_LTC_VaR_Norm_wo[,1],0.01)
LTC_Test_SST_1_wo  <- BacktestVaR(CrCu_LTC_VaR_SST_wo[,4],CrCu_LTC_VaR_SST_wo[,1],0.01)
LTC_Test_SGED_1_wo  <- BacktestVaR(CrCu_LTC_VaR_SGED_wo[,4],CrCu_LTC_VaR_SGED_wo[,1],0.01)
LTC_Test_JSU_1_wo  <- BacktestVaR(CrCu_LTC_VaR_JSU_wo[,4],CrCu_LTC_VaR_JSU_wo[,1],0.01)
LTC_Test_PSIV_1_wo  <- BacktestVaR(CrCu_LTC_VaR_Norm_wo[,4],CrCu_LTC_VaR_Norm_wo[,1],0.01)

AE_LTC_Norm0_wo <- LTC_Test_Norm_1_wo$AE
AE_LTC_SST0_wo <- LTC_Test_SST_1_wo$AE
AE_LTC_SGED0_wo <- LTC_Test_SGED_1_wo$AE
AE_LTC_JSU0_wo <- LTC_Test_JSU_1_wo$AE
AE_LTC_PS0_wo <- LTC_Test_PSIV_1_wo$AE

AE_LTC_Val_wo <- c(AE_LTC_Norm0_wo,
                   AE_LTC_SST0_wo, 
                   AE_LTC_SGED0_wo, 
                   AE_LTC_JSU0_wo, 
                   AE_LTC_PS0_wo)

AE_LTC_wo <- matrix(round(AE_LTC_Val_wo,4), nrow = 1)
colnames(AE_LTC_wo) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_LTC_wo) <- c("FS_WO")

### X M R ###
# Full Sample
XMR_Test_Norm_1_wo  <- BacktestVaR(CrCu_XMR_VaR_Norm_wo[,4],CrCu_XMR_VaR_Norm_wo[,1],0.01)
XMR_Test_SST_1_wo  <- BacktestVaR(CrCu_XMR_VaR_SST_wo[,4],CrCu_XMR_VaR_SST_wo[,1],0.01)
XMR_Test_SGED_1_wo  <- BacktestVaR(CrCu_XMR_VaR_SGED_wo[,4],CrCu_XMR_VaR_SGED_wo[,1],0.01)
XMR_Test_JSU_1_wo  <- BacktestVaR(CrCu_XMR_VaR_JSU_wo[,4],CrCu_XMR_VaR_JSU_wo[,1],0.01)
XMR_Test_PSIV_1_wo  <- BacktestVaR(CrCu_XMR_VaR_Norm_wo[,4],CrCu_XMR_VaR_Norm_wo[,1],0.01)

AE_XMR_Norm0_wo <- XMR_Test_Norm_1_wo$AE
AE_XMR_SST0_wo <- XMR_Test_SST_1_wo$AE
AE_XMR_SGED0_wo <- XMR_Test_SGED_1_wo$AE
AE_XMR_JSU0_wo <- XMR_Test_JSU_1_wo$AE
AE_XMR_PS0_wo <- XMR_Test_PSIV_1_wo$AE

AE_XMR_Val_wo <- c(AE_XMR_Norm0_wo,
                   AE_XMR_SST0_wo, 
                   AE_XMR_SGED0_wo, 
                   AE_XMR_JSU0_wo, 
                   AE_XMR_PS0_wo)

AE_XMR_wo <- matrix(round(AE_XMR_Val_wo,4), nrow = 1)
colnames(AE_XMR_wo) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_XMR_wo) <- c("FS_WO")

### E T C ###
# Full Sample
ETC_Test_Norm_1_wo  <- BacktestVaR(CrCu_ETC_VaR_Norm_wo[,4],CrCu_ETC_VaR_Norm_wo[,1],0.01)
ETC_Test_SST_1_wo  <- BacktestVaR(CrCu_ETC_VaR_SST_wo[,4],CrCu_ETC_VaR_SST_wo[,1],0.01)
ETC_Test_SGED_1_wo  <- BacktestVaR(CrCu_ETC_VaR_SGED_wo[,4],CrCu_ETC_VaR_SGED_wo[,1],0.01)
ETC_Test_JSU_1_wo  <- BacktestVaR(CrCu_ETC_VaR_JSU_wo[,4],CrCu_ETC_VaR_JSU_wo[,1],0.01)
ETC_Test_PSIV_1_wo  <- BacktestVaR(CrCu_ETC_VaR_Norm_wo[,4],CrCu_ETC_VaR_Norm_wo[,1],0.01)

AE_ETC_Norm0_wo <- ETC_Test_Norm_1_wo$AE
AE_ETC_SST0_wo <- ETC_Test_SST_1_wo$AE
AE_ETC_SGED0_wo <- ETC_Test_SGED_1_wo$AE
AE_ETC_JSU0_wo <- ETC_Test_JSU_1_wo$AE
AE_ETC_PS0_wo <- ETC_Test_PSIV_1_wo$AE

AE_ETC_Val_wo <- c(AE_ETC_Norm0_wo,
                   AE_ETC_SST0_wo, 
                   AE_ETC_SGED0_wo, 
                   AE_ETC_JSU0_wo, 
                   AE_ETC_PS0_wo)

AE_ETC_wo <- matrix(round(AE_ETC_Val_wo,4), nrow = 1)
colnames(AE_ETC_wo) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(AE_ETC_wo) <- c("FS_WO")







