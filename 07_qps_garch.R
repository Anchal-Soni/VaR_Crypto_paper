# =============================================================================
# 07_qps_garch.R
#
# Purpose : Stage-2 backtest: quadratic probability score (QPS) of 1% VaR for EGARCH
#           models (Norm, SST, SGED, JSU, PSIV).
# Inputs  : VaR objects from 03a-03e
# Outputs : QPS_<coin>, QPS_GARCH
# Paper   : Table 5 (EGARCH columns)
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

###### QPS - GARCH, GARCH/Wo, GAS #######
###### LINE 7, LINE 1310, LINE 1646 #######

#### VaR accuracy Stage 2 - GARCH - QPS

##### :::::::: B T C :::::::: #####
# Full Sample
#Normal
View(VR1_BTC)
VaR_BTC_fs_norm <- VR1_BTC[,1]
realisedval_BTC_fs_norm <- VR1_BTC[,4]
zi1_BTC_fs_norm <- ifelse(realisedval_BTC_fs_norm < VaR_BTC_fs_norm, 1, 0)
zi2_BTC_fs_norm <- ifelse(realisedval_BTC_fs_norm < VaR_BTC_fs_norm, 1, 0)*(1+((realisedval_BTC_fs_norm - VaR_BTC_fs_norm)^2))
zi3_BTC_fs_norm <- ifelse(realisedval_BTC_fs_norm < VaR_BTC_fs_norm, 1, 0)*((realisedval_BTC_fs_norm - VaR_BTC_fs_norm)/VaR_BTC_fs_norm)

QPS1_BTC_fs_norm <- (2/350)*sum(((zi1_BTC_fs_norm - 0.01)^2))
QPS2_BTC_fs_norm <- (2/350)*sum(((zi2_BTC_fs_norm - 0.01)^2))
QPS3_BTC_fs_norm <- (2/350)*sum(((zi3_BTC_fs_norm - 0.01)^2))

QPS_BTC_fs_norm <- QPS1_BTC_fs_norm + QPS2_BTC_fs_norm + QPS3_BTC_fs_norm 
#SST
VaR_BTC_fs_SST <- VR2_BTC[,1]
realisedval_BTC_fs_SST <- VR2_BTC[,4]
zi1_BTC_fs_SST <- ifelse(realisedval_BTC_fs_SST < VaR_BTC_fs_SST, 1, 0)
zi2_BTC_fs_SST <- ifelse(realisedval_BTC_fs_SST < VaR_BTC_fs_SST, 1, 0)*(1+((realisedval_BTC_fs_SST - VaR_BTC_fs_SST)^2))
zi3_BTC_fs_SST <- ifelse(realisedval_BTC_fs_SST < VaR_BTC_fs_SST, 1, 0)*((realisedval_BTC_fs_SST - VaR_BTC_fs_SST)/VaR_BTC_fs_SST)

QPS1_BTC_fs_SST <- (2/350)*sum(((zi1_BTC_fs_SST - 0.01)^2))
QPS2_BTC_fs_SST <- (2/350)*sum(((zi2_BTC_fs_SST - 0.01)^2))
QPS3_BTC_fs_SST <- (2/350)*sum(((zi3_BTC_fs_SST - 0.01)^2))

QPS_BTC_fs_SST <- QPS1_BTC_fs_SST + QPS2_BTC_fs_SST + QPS3_BTC_fs_SST
#SGED
VaR_BTC_fs_SGED <- VR3_BTC[,1]
realisedval_BTC_fs_SGED <- VR3_BTC[,4]
zi1_BTC_fs_SGED <- ifelse(realisedval_BTC_fs_SGED < VaR_BTC_fs_SGED, 1, 0)
zi2_BTC_fs_SGED <- ifelse(realisedval_BTC_fs_SGED < VaR_BTC_fs_SGED, 1, 0)*(1+((realisedval_BTC_fs_SGED - VaR_BTC_fs_SGED)^2))
zi3_BTC_fs_SGED <- ifelse(realisedval_BTC_fs_SGED < VaR_BTC_fs_SGED, 1, 0)*((realisedval_BTC_fs_SGED - VaR_BTC_fs_SGED)/VaR_BTC_fs_SGED)

QPS1_BTC_fs_SGED <- (2/350)*sum(((zi1_BTC_fs_SGED - 0.01)^2))
QPS2_BTC_fs_SGED <- (2/350)*sum(((zi2_BTC_fs_SGED - 0.01)^2))
QPS3_BTC_fs_SGED <- (2/350)*sum(((zi3_BTC_fs_SGED - 0.01)^2))

QPS_BTC_fs_SGED <- QPS1_BTC_fs_SGED + QPS2_BTC_fs_SGED + QPS3_BTC_fs_SGED
#JSU
VaR_BTC_fs_JSU <- VR4_BTC[,1]
realisedval_BTC_fs_JSU <- VR4_BTC[,4]
zi1_BTC_fs_JSU <- ifelse(realisedval_BTC_fs_JSU < VaR_BTC_fs_JSU, 1, 0)
zi2_BTC_fs_JSU <- ifelse(realisedval_BTC_fs_JSU < VaR_BTC_fs_JSU, 1, 0)*(1+((realisedval_BTC_fs_JSU - VaR_BTC_fs_JSU)^2))
zi3_BTC_fs_JSU <- ifelse(realisedval_BTC_fs_JSU < VaR_BTC_fs_JSU, 1, 0)*((realisedval_BTC_fs_JSU - VaR_BTC_fs_JSU)/VaR_BTC_fs_JSU)

QPS1_BTC_fs_JSU <- (2/350)*sum(((zi1_BTC_fs_JSU - 0.01)^2))
QPS2_BTC_fs_JSU <- (2/350)*sum(((zi2_BTC_fs_JSU - 0.01)^2))
QPS3_BTC_fs_JSU <- (2/350)*sum(((zi3_BTC_fs_JSU - 0.01)^2))

QPS_BTC_fs_JSU <- QPS1_BTC_fs_JSU + QPS2_BTC_fs_JSU + QPS3_BTC_fs_JSU
#PSIV
VaR_BTC_fs_PSIV <- VR5_BTC
realisedval_BTC_fs_PSIV <- VR4_BTC[,4]
zi1_BTC_fs_PSIV <- ifelse(realisedval_BTC_fs_PSIV < VaR_BTC_fs_PSIV, 1, 0)
zi2_BTC_fs_PSIV <- ifelse(realisedval_BTC_fs_PSIV < VaR_BTC_fs_PSIV, 1, 0)*(1+((realisedval_BTC_fs_PSIV - VaR_BTC_fs_PSIV)^2))
zi3_BTC_fs_PSIV <- ifelse(realisedval_BTC_fs_PSIV < VaR_BTC_fs_PSIV, 1, 0)*((realisedval_BTC_fs_PSIV - VaR_BTC_fs_PSIV)/VaR_BTC_fs_PSIV)

QPS1_BTC_fs_PSIV <- (2/350)*sum(((zi1_BTC_fs_PSIV - 0.01)^2))
QPS2_BTC_fs_PSIV <- (2/350)*sum(((zi2_BTC_fs_PSIV - 0.01)^2))
QPS3_BTC_fs_PSIV <- (2/350)*sum(((zi3_BTC_fs_PSIV - 0.01)^2))

QPS_BTC_fs_PSIV <- QPS1_BTC_fs_PSIV + QPS2_BTC_fs_PSIV + QPS3_BTC_fs_PSIV

# Sub Sample 1
#Normal
VaR_BTC_ss1_norm <- VR11_BTC[,1]
realisedval_BTC_ss1_norm <- VR11_BTC[,4]
zi1_BTC_ss1_norm <- ifelse(realisedval_BTC_ss1_norm < VaR_BTC_ss1_norm, 1, 0)
zi2_BTC_ss1_norm <- ifelse(realisedval_BTC_ss1_norm < VaR_BTC_ss1_norm, 1, 0)*(1+((realisedval_BTC_ss1_norm - VaR_BTC_ss1_norm)^2))
zi3_BTC_ss1_norm <- ifelse(realisedval_BTC_ss1_norm < VaR_BTC_ss1_norm, 1, 0)*((realisedval_BTC_ss1_norm - VaR_BTC_ss1_norm)/VaR_BTC_ss1_norm)

QPS1_BTC_ss1_norm <- (2/60)*sum(((zi1_BTC_ss1_norm - 0.01)^2))
QPS2_BTC_ss1_norm <- (2/60)*sum(((zi2_BTC_ss1_norm - 0.01)^2))
QPS3_BTC_ss1_norm <- (2/60)*sum(((zi3_BTC_ss1_norm - 0.01)^2))

QPS_BTC_ss1_norm <- QPS1_BTC_ss1_norm + QPS2_BTC_ss1_norm + QPS3_BTC_ss1_norm
#SST
VaR_BTC_ss1_SST <- VR12_BTC[,1]
realisedval_BTC_ss1_SST <- VR12_BTC[,4]
zi1_BTC_ss1_SST <- ifelse(realisedval_BTC_ss1_SST < VaR_BTC_ss1_SST, 1, 0)
zi2_BTC_ss1_SST <- ifelse(realisedval_BTC_ss1_SST < VaR_BTC_ss1_SST, 1, 0)*(1+((realisedval_BTC_ss1_SST - VaR_BTC_ss1_SST)^2))
zi3_BTC_ss1_SST <- ifelse(realisedval_BTC_ss1_SST < VaR_BTC_ss1_SST, 1, 0)*((realisedval_BTC_ss1_SST - VaR_BTC_ss1_SST)/VaR_BTC_ss1_SST)

QPS1_BTC_ss1_SST <- (2/60)*sum(((zi1_BTC_ss1_SST - 0.01)^2))
QPS2_BTC_ss1_SST <- (2/60)*sum(((zi2_BTC_ss1_SST - 0.01)^2))
QPS3_BTC_ss1_SST <- (2/60)*sum(((zi3_BTC_ss1_SST - 0.01)^2))

QPS_BTC_ss1_SST <- QPS1_BTC_ss1_SST + QPS2_BTC_ss1_SST + QPS3_BTC_ss1_SST
#SGED
VaR_BTC_ss1_SGED <- VR13_BTC[,1]
realisedval_BTC_ss1_SGED <- VR13_BTC[,4]
zi1_BTC_ss1_SGED <- ifelse(realisedval_BTC_ss1_SGED < VaR_BTC_ss1_SGED, 1, 0)
zi2_BTC_ss1_SGED <- ifelse(realisedval_BTC_ss1_SGED < VaR_BTC_ss1_SGED, 1, 0)*(1+((realisedval_BTC_ss1_SGED - VaR_BTC_ss1_SGED)^2))
zi3_BTC_ss1_SGED <- ifelse(realisedval_BTC_ss1_SGED < VaR_BTC_ss1_SGED, 1, 0)*((realisedval_BTC_ss1_SGED - VaR_BTC_ss1_SGED)/VaR_BTC_ss1_SGED)

QPS1_BTC_ss1_SGED <- (2/60)*sum(((zi1_BTC_ss1_SGED - 0.01)^2))
QPS2_BTC_ss1_SGED <- (2/60)*sum(((zi2_BTC_ss1_SGED - 0.01)^2))
QPS3_BTC_ss1_SGED <- (2/60)*sum(((zi3_BTC_ss1_SGED - 0.01)^2))

QPS_BTC_ss1_SGED <- QPS1_BTC_ss1_SGED + QPS2_BTC_ss1_SGED + QPS3_BTC_ss1_SGED
#JSU
VaR_BTC_ss1_JSU <- VR14_BTC[,1]
realisedval_BTC_ss1_JSU <- VR14_BTC[,4]
zi1_BTC_ss1_JSU <- ifelse(realisedval_BTC_ss1_JSU < VaR_BTC_ss1_JSU, 1, 0)
zi2_BTC_ss1_JSU <- ifelse(realisedval_BTC_ss1_JSU < VaR_BTC_ss1_JSU, 1, 0)*(1+((realisedval_BTC_ss1_JSU - VaR_BTC_ss1_JSU)^2))
zi3_BTC_ss1_JSU <- ifelse(realisedval_BTC_ss1_JSU < VaR_BTC_ss1_JSU, 1, 0)*((realisedval_BTC_ss1_JSU - VaR_BTC_ss1_JSU)/VaR_BTC_ss1_JSU)

QPS1_BTC_ss1_JSU <- (2/60)*sum(((zi1_BTC_ss1_JSU - 0.01)^2))
QPS2_BTC_ss1_JSU <- (2/60)*sum(((zi2_BTC_ss1_JSU - 0.01)^2))
QPS3_BTC_ss1_JSU <- (2/60)*sum(((zi3_BTC_ss1_JSU - 0.01)^2))

QPS_BTC_ss1_JSU <- QPS1_BTC_ss1_JSU + QPS2_BTC_ss1_JSU + QPS3_BTC_ss1_JSU
#PSIV
VaR_BTC_ss1_PSIV <- VR15_BTC
realisedval_BTC_ss1_PSIV <- VR14_BTC[,4]
zi1_BTC_ss1_PSIV <- ifelse(realisedval_BTC_ss1_PSIV < VaR_BTC_ss1_PSIV, 1, 0)
zi2_BTC_ss1_PSIV <- ifelse(realisedval_BTC_ss1_PSIV < VaR_BTC_ss1_PSIV, 1, 0)*(1+((realisedval_BTC_ss1_PSIV - VaR_BTC_ss1_PSIV)^2))
zi3_BTC_ss1_PSIV <- ifelse(realisedval_BTC_ss1_PSIV < VaR_BTC_ss1_PSIV, 1, 0)*((realisedval_BTC_ss1_PSIV - VaR_BTC_ss1_PSIV)/VaR_BTC_ss1_PSIV)

QPS1_BTC_ss1_PSIV <- (2/60)*sum(((zi1_BTC_ss1_PSIV - 0.01)^2))
QPS2_BTC_ss1_PSIV <- (2/60)*sum(((zi2_BTC_ss1_PSIV - 0.01)^2))
QPS3_BTC_ss1_PSIV <- (2/60)*sum(((zi3_BTC_ss1_PSIV - 0.01)^2))

QPS_BTC_ss1_PSIV <- QPS1_BTC_ss1_PSIV + QPS2_BTC_ss1_PSIV + QPS3_BTC_ss1_PSIV

# Sub Sample 2
#Normal
VaR_BTC_ss2_norm <- VR21_BTC[,1]
realisedval_BTC_ss2_norm <- VR21_BTC[,4]
zi1_BTC_ss2_norm <- ifelse(realisedval_BTC_ss2_norm < VaR_BTC_ss2_norm, 1, 0)
zi2_BTC_ss2_norm <- ifelse(realisedval_BTC_ss2_norm < VaR_BTC_ss2_norm, 1, 0)*(1+((realisedval_BTC_ss2_norm - VaR_BTC_ss2_norm)^2))
zi3_BTC_ss2_norm <- ifelse(realisedval_BTC_ss2_norm < VaR_BTC_ss2_norm, 1, 0)*((realisedval_BTC_ss2_norm - VaR_BTC_ss2_norm)/VaR_BTC_ss2_norm)

QPS1_BTC_ss2_norm <- (2/200)*sum(((zi1_BTC_ss2_norm - 0.01)^2))
QPS2_BTC_ss2_norm <- (2/200)*sum(((zi2_BTC_ss2_norm - 0.01)^2))
QPS3_BTC_ss2_norm <- (2/200)*sum(((zi3_BTC_ss2_norm - 0.01)^2))

QPS_BTC_ss2_norm <- QPS1_BTC_ss2_norm + QPS2_BTC_ss2_norm + QPS3_BTC_ss2_norm
#SST
VaR_BTC_ss2_SST <- VR22_BTC[,1]
realisedval_BTC_ss2_SST <- VR22_BTC[,4]
zi1_BTC_ss2_SST <- ifelse(realisedval_BTC_ss2_SST < VaR_BTC_ss2_SST, 1, 0)
zi2_BTC_ss2_SST <- ifelse(realisedval_BTC_ss2_SST < VaR_BTC_ss2_SST, 1, 0)*(1+((realisedval_BTC_ss2_SST - VaR_BTC_ss2_SST)^2))
zi3_BTC_ss2_SST <- ifelse(realisedval_BTC_ss2_SST < VaR_BTC_ss2_SST, 1, 0)*((realisedval_BTC_ss2_SST - VaR_BTC_ss2_SST)/VaR_BTC_ss2_SST)

QPS1_BTC_ss2_SST <- (2/200)*sum(((zi1_BTC_ss2_SST - 0.01)^2))
QPS2_BTC_ss2_SST <- (2/200)*sum(((zi2_BTC_ss2_SST - 0.01)^2))
QPS3_BTC_ss2_SST <- (2/200)*sum(((zi3_BTC_ss2_SST - 0.01)^2))

QPS_BTC_ss2_SST <- QPS1_BTC_ss2_SST + QPS2_BTC_ss2_SST + QPS3_BTC_ss2_SST
#SGED
VaR_BTC_ss2_SGED <- VR23_BTC[,1]
realisedval_BTC_ss2_SGED <- VR23_BTC[,4]
zi1_BTC_ss2_SGED <- ifelse(realisedval_BTC_ss2_SGED < VaR_BTC_ss2_SGED, 1, 0)
zi2_BTC_ss2_SGED <- ifelse(realisedval_BTC_ss2_SGED < VaR_BTC_ss2_SGED, 1, 0)*(1+((realisedval_BTC_ss2_SGED - VaR_BTC_ss2_SGED)^2))
zi3_BTC_ss2_SGED <- ifelse(realisedval_BTC_ss2_SGED < VaR_BTC_ss2_SGED, 1, 0)*((realisedval_BTC_ss2_SGED - VaR_BTC_ss2_SGED)/VaR_BTC_ss2_SGED)

QPS1_BTC_ss2_SGED <- (2/200)*sum(((zi1_BTC_ss2_SGED - 0.01)^2))
QPS2_BTC_ss2_SGED <- (2/200)*sum(((zi2_BTC_ss2_SGED - 0.01)^2))
QPS3_BTC_ss2_SGED <- (2/200)*sum(((zi3_BTC_ss2_SGED - 0.01)^2))

QPS_BTC_ss2_SGED <- QPS1_BTC_ss2_SGED + QPS2_BTC_ss2_SGED + QPS3_BTC_ss2_SGED
#JSU
VaR_BTC_ss2_JSU <- VR24_BTC[,1]
realisedval_BTC_ss2_JSU <- VR24_BTC[,4]
zi1_BTC_ss2_JSU <- ifelse(realisedval_BTC_ss2_JSU < VaR_BTC_ss2_JSU, 1, 0)
zi2_BTC_ss2_JSU <- ifelse(realisedval_BTC_ss2_JSU < VaR_BTC_ss2_JSU, 1, 0)*(1+((realisedval_BTC_ss2_JSU - VaR_BTC_ss2_JSU)^2))
zi3_BTC_ss2_JSU <- ifelse(realisedval_BTC_ss2_JSU < VaR_BTC_ss2_JSU, 1, 0)*((realisedval_BTC_ss2_JSU - VaR_BTC_ss2_JSU)/VaR_BTC_ss2_JSU)

QPS1_BTC_ss2_JSU <- (2/200)*sum(((zi1_BTC_ss2_JSU - 0.01)^2))
QPS2_BTC_ss2_JSU <- (2/200)*sum(((zi2_BTC_ss2_JSU - 0.01)^2))
QPS3_BTC_ss2_JSU <- (2/200)*sum(((zi3_BTC_ss2_JSU - 0.01)^2))

QPS_BTC_ss2_JSU <- QPS1_BTC_ss2_JSU + QPS2_BTC_ss2_JSU + QPS3_BTC_ss2_JSU
#PSIV
VaR_BTC_ss2_PSIV <- VR25_BTC
realisedval_BTC_ss2_PSIV <- VR24_BTC[,4]
zi1_BTC_ss2_PSIV <- ifelse(realisedval_BTC_ss2_PSIV < VaR_BTC_ss2_PSIV, 1, 0)
zi2_BTC_ss2_PSIV <- ifelse(realisedval_BTC_ss2_PSIV < VaR_BTC_ss2_PSIV, 1, 0)*(1+((realisedval_BTC_ss2_PSIV - VaR_BTC_ss2_PSIV)^2))
zi3_BTC_ss2_PSIV <- ifelse(realisedval_BTC_ss2_PSIV < VaR_BTC_ss2_PSIV, 1, 0)*((realisedval_BTC_ss2_PSIV - VaR_BTC_ss2_PSIV)/VaR_BTC_ss2_PSIV)

QPS1_BTC_ss2_PSIV <- (2/200)*sum(((zi1_BTC_ss2_PSIV - 0.01)^2))
QPS2_BTC_ss2_PSIV <- (2/200)*sum(((zi2_BTC_ss2_PSIV - 0.01)^2))
QPS3_BTC_ss2_PSIV <- (2/200)*sum(((zi3_BTC_ss2_PSIV - 0.01)^2))

QPS_BTC_ss2_PSIV <- QPS1_BTC_ss2_PSIV + QPS2_BTC_ss2_PSIV + QPS3_BTC_ss2_PSIV

# Sub Sample 3
#Normal
VaR_BTC_ss3_norm <- VR31_BTC[,1]
realisedval_BTC_ss3_norm <- VR31_BTC[,4]
zi1_BTC_ss3_norm <- ifelse(realisedval_BTC_ss3_norm < VaR_BTC_ss3_norm, 1, 0)
zi2_BTC_ss3_norm <- ifelse(realisedval_BTC_ss3_norm < VaR_BTC_ss3_norm, 1, 0)*(1+((realisedval_BTC_ss3_norm - VaR_BTC_ss3_norm)^2))
zi3_BTC_ss3_norm <- ifelse(realisedval_BTC_ss3_norm < VaR_BTC_ss3_norm, 1, 0)*((realisedval_BTC_ss3_norm - VaR_BTC_ss3_norm)/VaR_BTC_ss3_norm)

QPS1_BTC_ss3_norm <- (2/100)*sum(((zi1_BTC_ss3_norm - 0.01)^2))
QPS2_BTC_ss3_norm <- (2/100)*sum(((zi2_BTC_ss3_norm - 0.01)^2))
QPS3_BTC_ss3_norm <- (2/100)*sum(((zi3_BTC_ss3_norm - 0.01)^2))

QPS_BTC_ss3_norm <- QPS1_BTC_ss3_norm + QPS2_BTC_ss3_norm + QPS3_BTC_ss3_norm
#SST
VaR_BTC_ss3_SST <- VR32_BTC[,1]
realisedval_BTC_ss3_SST <- VR32_BTC[,4]
zi1_BTC_ss3_SST <- ifelse(realisedval_BTC_ss3_SST < VaR_BTC_ss3_SST, 1, 0)
zi2_BTC_ss3_SST <- ifelse(realisedval_BTC_ss3_SST < VaR_BTC_ss3_SST, 1, 0)*(1+((realisedval_BTC_ss3_SST - VaR_BTC_ss3_SST)^2))
zi3_BTC_ss3_SST <- ifelse(realisedval_BTC_ss3_SST < VaR_BTC_ss3_SST, 1, 0)*((realisedval_BTC_ss3_SST - VaR_BTC_ss3_SST)/VaR_BTC_ss3_SST)

QPS1_BTC_ss3_SST <- (2/100)*sum(((zi1_BTC_ss3_SST - 0.01)^2))
QPS2_BTC_ss3_SST <- (2/100)*sum(((zi2_BTC_ss3_SST - 0.01)^2))
QPS3_BTC_ss3_SST <- (2/100)*sum(((zi3_BTC_ss3_SST - 0.01)^2))

QPS_BTC_ss3_SST <- QPS1_BTC_ss3_SST + QPS2_BTC_ss3_SST + QPS3_BTC_ss3_SST
#SGED
VaR_BTC_ss3_SGED <- VR33_BTC[,1]
realisedval_BTC_ss3_SGED <- VR33_BTC[,4]
zi1_BTC_ss3_SGED <- ifelse(realisedval_BTC_ss3_SGED < VaR_BTC_ss3_SGED, 1, 0)
zi2_BTC_ss3_SGED <- ifelse(realisedval_BTC_ss3_SGED < VaR_BTC_ss3_SGED, 1, 0)*(1+((realisedval_BTC_ss3_SGED - VaR_BTC_ss3_SGED)^2))
zi3_BTC_ss3_SGED <- ifelse(realisedval_BTC_ss3_SGED < VaR_BTC_ss3_SGED, 1, 0)*((realisedval_BTC_ss3_SGED - VaR_BTC_ss3_SGED)/VaR_BTC_ss3_SGED)

QPS1_BTC_ss3_SGED <- (2/100)*sum(((zi1_BTC_ss3_SGED - 0.01)^2))
QPS2_BTC_ss3_SGED <- (2/100)*sum(((zi2_BTC_ss3_SGED - 0.01)^2))
QPS3_BTC_ss3_SGED <- (2/100)*sum(((zi3_BTC_ss3_SGED - 0.01)^2))

QPS_BTC_ss3_SGED <- QPS1_BTC_ss3_SGED + QPS2_BTC_ss3_SGED + QPS3_BTC_ss3_SGED
#JSU
VaR_BTC_ss3_JSU <- VR34_BTC[,1]
realisedval_BTC_ss3_JSU <- VR34_BTC[,4]
zi1_BTC_ss3_JSU <- ifelse(realisedval_BTC_ss3_JSU < VaR_BTC_ss3_JSU, 1, 0)
zi2_BTC_ss3_JSU <- ifelse(realisedval_BTC_ss3_JSU < VaR_BTC_ss3_JSU, 1, 0)*(1+((realisedval_BTC_ss3_JSU - VaR_BTC_ss3_JSU)^2))
zi3_BTC_ss3_JSU <- ifelse(realisedval_BTC_ss3_JSU < VaR_BTC_ss3_JSU, 1, 0)*((realisedval_BTC_ss3_JSU - VaR_BTC_ss3_JSU)/VaR_BTC_ss3_JSU)

QPS1_BTC_ss3_JSU <- (2/100)*sum(((zi1_BTC_ss3_JSU - 0.01)^2))
QPS2_BTC_ss3_JSU <- (2/100)*sum(((zi2_BTC_ss3_JSU - 0.01)^2))
QPS3_BTC_ss3_JSU <- (2/100)*sum(((zi3_BTC_ss3_JSU - 0.01)^2))

QPS_BTC_ss3_JSU <- QPS1_BTC_ss3_JSU + QPS2_BTC_ss3_JSU + QPS3_BTC_ss3_JSU
#PSIV
VaR_BTC_ss3_PSIV <- VR35_BTC
realisedval_BTC_ss3_PSIV <- VR34_BTC[,4]
zi1_BTC_ss3_PSIV <- ifelse(realisedval_BTC_ss3_PSIV < VaR_BTC_ss3_PSIV, 1, 0)
zi2_BTC_ss3_PSIV <- ifelse(realisedval_BTC_ss3_PSIV < VaR_BTC_ss3_PSIV, 1, 0)*(1+((realisedval_BTC_ss3_PSIV - VaR_BTC_ss3_PSIV)^2))
zi3_BTC_ss3_PSIV <- ifelse(realisedval_BTC_ss3_PSIV < VaR_BTC_ss3_PSIV, 1, 0)*((realisedval_BTC_ss3_PSIV - VaR_BTC_ss3_PSIV)/VaR_BTC_ss3_PSIV)

QPS1_BTC_ss3_PSIV <- (2/100)*sum(((zi1_BTC_ss3_PSIV - 0.01)^2))
QPS2_BTC_ss3_PSIV <- (2/100)*sum(((zi2_BTC_ss3_PSIV - 0.01)^2))
QPS3_BTC_ss3_PSIV <- (2/100)*sum(((zi3_BTC_ss3_PSIV - 0.01)^2))

QPS_BTC_ss3_PSIV <- QPS1_BTC_ss3_PSIV + QPS2_BTC_ss3_PSIV + QPS3_BTC_ss3_PSIV

QPS_BTC_fs <- c(QPS_BTC_fs_norm, QPS_BTC_fs_SST, QPS_BTC_fs_SGED, QPS_BTC_fs_JSU, QPS_BTC_fs_PSIV)
QPS_BTC_ss1 <- c(QPS_BTC_ss1_norm, QPS_BTC_ss1_SST, QPS_BTC_ss1_SGED, QPS_BTC_ss1_JSU, QPS_BTC_ss1_PSIV)
QPS_BTC_ss2 <- c(QPS_BTC_ss2_norm, QPS_BTC_ss2_SST, QPS_BTC_ss2_SGED, QPS_BTC_ss2_JSU, QPS_BTC_ss2_PSIV)
QPS_BTC_ss3 <- c(QPS_BTC_ss3_norm, QPS_BTC_ss3_SST, QPS_BTC_ss3_SGED, QPS_BTC_ss3_JSU, QPS_BTC_ss3_PSIV)

QPS_BTC <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_BTC <- rbind(QPS_BTC_fs, QPS_BTC_ss1, QPS_BTC_ss2, QPS_BTC_ss3)
colnames(QPS_BTC) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(QPS_BTC) <- c("FS", "SS1", "SS2", "SS3")

##### :::::::: E T H :::::::: #####
# Full Sample
#Normal
VaR_ETH_fs_norm <- VR1_ETH[,1]
realisedval_ETH_fs_norm <- VR1_ETH[,4]
zi1_ETH_fs_norm <- ifelse(realisedval_ETH_fs_norm < VaR_ETH_fs_norm, 1, 0)
zi2_ETH_fs_norm <- ifelse(realisedval_ETH_fs_norm < VaR_ETH_fs_norm, 1, 0)*(1+((realisedval_ETH_fs_norm - VaR_ETH_fs_norm)^2))
zi3_ETH_fs_norm <- ifelse(realisedval_ETH_fs_norm < VaR_ETH_fs_norm, 1, 0)*((realisedval_ETH_fs_norm - VaR_ETH_fs_norm)/VaR_ETH_fs_norm)

QPS1_ETH_fs_norm <- (2/350)*sum(((zi1_ETH_fs_norm - 0.01)^2))
QPS2_ETH_fs_norm <- (2/350)*sum(((zi2_ETH_fs_norm - 0.01)^2))
QPS3_ETH_fs_norm <- (2/350)*sum(((zi3_ETH_fs_norm - 0.01)^2))

QPS_ETH_fs_norm <- QPS1_ETH_fs_norm + QPS2_ETH_fs_norm + QPS3_ETH_fs_norm 
#SST
VaR_ETH_fs_SST <- VR2_ETH[,1]
realisedval_ETH_fs_SST <- VR2_ETH[,4]
zi1_ETH_fs_SST <- ifelse(realisedval_ETH_fs_SST < VaR_ETH_fs_SST, 1, 0)
zi2_ETH_fs_SST <- ifelse(realisedval_ETH_fs_SST < VaR_ETH_fs_SST, 1, 0)*(1+((realisedval_ETH_fs_SST - VaR_ETH_fs_SST)^2))
zi3_ETH_fs_SST <- ifelse(realisedval_ETH_fs_SST < VaR_ETH_fs_SST, 1, 0)*((realisedval_ETH_fs_SST - VaR_ETH_fs_SST)/VaR_ETH_fs_SST)

QPS1_ETH_fs_SST <- (2/350)*sum(((zi1_ETH_fs_SST - 0.01)^2))
QPS2_ETH_fs_SST <- (2/350)*sum(((zi2_ETH_fs_SST - 0.01)^2))
QPS3_ETH_fs_SST <- (2/350)*sum(((zi3_ETH_fs_SST - 0.01)^2))

QPS_ETH_fs_SST <- QPS1_ETH_fs_SST + QPS2_ETH_fs_SST + QPS3_ETH_fs_SST
#SGED
VaR_ETH_fs_SGED <- VR3_ETH[,1]
realisedval_ETH_fs_SGED <- VR3_ETH[,4]
zi1_ETH_fs_SGED <- ifelse(realisedval_ETH_fs_SGED < VaR_ETH_fs_SGED, 1, 0)
zi2_ETH_fs_SGED <- ifelse(realisedval_ETH_fs_SGED < VaR_ETH_fs_SGED, 1, 0)*(1+((realisedval_ETH_fs_SGED - VaR_ETH_fs_SGED)^2))
zi3_ETH_fs_SGED <- ifelse(realisedval_ETH_fs_SGED < VaR_ETH_fs_SGED, 1, 0)*((realisedval_ETH_fs_SGED - VaR_ETH_fs_SGED)/VaR_ETH_fs_SGED)

QPS1_ETH_fs_SGED <- (2/350)*sum(((zi1_ETH_fs_SGED - 0.01)^2))
QPS2_ETH_fs_SGED <- (2/350)*sum(((zi2_ETH_fs_SGED - 0.01)^2))
QPS3_ETH_fs_SGED <- (2/350)*sum(((zi3_ETH_fs_SGED - 0.01)^2))

QPS_ETH_fs_SGED <- QPS1_ETH_fs_SGED + QPS2_ETH_fs_SGED + QPS3_ETH_fs_SGED
#JSU
VaR_ETH_fs_JSU <- VR4_ETH[,1]
realisedval_ETH_fs_JSU <- VR4_ETH[,4]
zi1_ETH_fs_JSU <- ifelse(realisedval_ETH_fs_JSU < VaR_ETH_fs_JSU, 1, 0)
zi2_ETH_fs_JSU <- ifelse(realisedval_ETH_fs_JSU < VaR_ETH_fs_JSU, 1, 0)*(1+((realisedval_ETH_fs_JSU - VaR_ETH_fs_JSU)^2))
zi3_ETH_fs_JSU <- ifelse(realisedval_ETH_fs_JSU < VaR_ETH_fs_JSU, 1, 0)*((realisedval_ETH_fs_JSU - VaR_ETH_fs_JSU)/VaR_ETH_fs_JSU)

QPS1_ETH_fs_JSU <- (2/350)*sum(((zi1_ETH_fs_JSU - 0.01)^2))
QPS2_ETH_fs_JSU <- (2/350)*sum(((zi2_ETH_fs_JSU - 0.01)^2))
QPS3_ETH_fs_JSU <- (2/350)*sum(((zi3_ETH_fs_JSU - 0.01)^2))

QPS_ETH_fs_JSU <- QPS1_ETH_fs_JSU + QPS2_ETH_fs_JSU + QPS3_ETH_fs_JSU
#PSIV
VaR_ETH_fs_PSIV <- VR5_ETH
realisedval_ETH_fs_PSIV <- VR4_ETH[,4]
zi1_ETH_fs_PSIV <- ifelse(realisedval_ETH_fs_PSIV < VaR_ETH_fs_PSIV, 1, 0)
zi2_ETH_fs_PSIV <- ifelse(realisedval_ETH_fs_PSIV < VaR_ETH_fs_PSIV, 1, 0)*(1+((realisedval_ETH_fs_PSIV - VaR_ETH_fs_PSIV)^2))
zi3_ETH_fs_PSIV <- ifelse(realisedval_ETH_fs_PSIV < VaR_ETH_fs_PSIV, 1, 0)*((realisedval_ETH_fs_PSIV - VaR_ETH_fs_PSIV)/VaR_ETH_fs_PSIV)

QPS1_ETH_fs_PSIV <- (2/350)*sum(((zi1_ETH_fs_PSIV - 0.01)^2))
QPS2_ETH_fs_PSIV <- (2/350)*sum(((zi2_ETH_fs_PSIV - 0.01)^2))
QPS3_ETH_fs_PSIV <- (2/350)*sum(((zi3_ETH_fs_PSIV - 0.01)^2))

QPS_ETH_fs_PSIV <- QPS1_ETH_fs_PSIV + QPS2_ETH_fs_PSIV + QPS3_ETH_fs_PSIV

# Sub Sample 1
#Normal
VaR_ETH_ss1_norm <- VR11_ETH[,1]
realisedval_ETH_ss1_norm <- VR11_ETH[,4]
zi1_ETH_ss1_norm <- ifelse(realisedval_ETH_ss1_norm < VaR_ETH_ss1_norm, 1, 0)
zi2_ETH_ss1_norm <- ifelse(realisedval_ETH_ss1_norm < VaR_ETH_ss1_norm, 1, 0)*(1+((realisedval_ETH_ss1_norm - VaR_ETH_ss1_norm)^2))
zi3_ETH_ss1_norm <- ifelse(realisedval_ETH_ss1_norm < VaR_ETH_ss1_norm, 1, 0)*((realisedval_ETH_ss1_norm - VaR_ETH_ss1_norm)/VaR_ETH_ss1_norm)

QPS1_ETH_ss1_norm <- (2/60)*sum(((zi1_ETH_ss1_norm - 0.01)^2))
QPS2_ETH_ss1_norm <- (2/60)*sum(((zi2_ETH_ss1_norm - 0.01)^2))
QPS3_ETH_ss1_norm <- (2/60)*sum(((zi3_ETH_ss1_norm - 0.01)^2))

QPS_ETH_ss1_norm <- QPS1_ETH_ss1_norm + QPS2_ETH_ss1_norm + QPS3_ETH_ss1_norm
#SST
VaR_ETH_ss1_SST <- VR12_ETH[,1]
realisedval_ETH_ss1_SST <- VR12_ETH[,4]
zi1_ETH_ss1_SST <- ifelse(realisedval_ETH_ss1_SST < VaR_ETH_ss1_SST, 1, 0)
zi2_ETH_ss1_SST <- ifelse(realisedval_ETH_ss1_SST < VaR_ETH_ss1_SST, 1, 0)*(1+((realisedval_ETH_ss1_SST - VaR_ETH_ss1_SST)^2))
zi3_ETH_ss1_SST <- ifelse(realisedval_ETH_ss1_SST < VaR_ETH_ss1_SST, 1, 0)*((realisedval_ETH_ss1_SST - VaR_ETH_ss1_SST)/VaR_ETH_ss1_SST)

QPS1_ETH_ss1_SST <- (2/60)*sum(((zi1_ETH_ss1_SST - 0.01)^2))
QPS2_ETH_ss1_SST <- (2/60)*sum(((zi2_ETH_ss1_SST - 0.01)^2))
QPS3_ETH_ss1_SST <- (2/60)*sum(((zi3_ETH_ss1_SST - 0.01)^2))

QPS_ETH_ss1_SST <- QPS1_ETH_ss1_SST + QPS2_ETH_ss1_SST + QPS3_ETH_ss1_SST
#SGED
VaR_ETH_ss1_SGED <- VR13_ETH[,1]
realisedval_ETH_ss1_SGED <- VR13_ETH[,4]
zi1_ETH_ss1_SGED <- ifelse(realisedval_ETH_ss1_SGED < VaR_ETH_ss1_SGED, 1, 0)
zi2_ETH_ss1_SGED <- ifelse(realisedval_ETH_ss1_SGED < VaR_ETH_ss1_SGED, 1, 0)*(1+((realisedval_ETH_ss1_SGED - VaR_ETH_ss1_SGED)^2))
zi3_ETH_ss1_SGED <- ifelse(realisedval_ETH_ss1_SGED < VaR_ETH_ss1_SGED, 1, 0)*((realisedval_ETH_ss1_SGED - VaR_ETH_ss1_SGED)/VaR_ETH_ss1_SGED)

QPS1_ETH_ss1_SGED <- (2/60)*sum(((zi1_ETH_ss1_SGED - 0.01)^2))
QPS2_ETH_ss1_SGED <- (2/60)*sum(((zi2_ETH_ss1_SGED - 0.01)^2))
QPS3_ETH_ss1_SGED <- (2/60)*sum(((zi3_ETH_ss1_SGED - 0.01)^2))

QPS_ETH_ss1_SGED <- QPS1_ETH_ss1_SGED + QPS2_ETH_ss1_SGED + QPS3_ETH_ss1_SGED
#JSU
VaR_ETH_ss1_JSU <- VR14_ETH[,1]
realisedval_ETH_ss1_JSU <- VR14_ETH[,4]
zi1_ETH_ss1_JSU <- ifelse(realisedval_ETH_ss1_JSU < VaR_ETH_ss1_JSU, 1, 0)
zi2_ETH_ss1_JSU <- ifelse(realisedval_ETH_ss1_JSU < VaR_ETH_ss1_JSU, 1, 0)*(1+((realisedval_ETH_ss1_JSU - VaR_ETH_ss1_JSU)^2))
zi3_ETH_ss1_JSU <- ifelse(realisedval_ETH_ss1_JSU < VaR_ETH_ss1_JSU, 1, 0)*((realisedval_ETH_ss1_JSU - VaR_ETH_ss1_JSU)/VaR_ETH_ss1_JSU)

QPS1_ETH_ss1_JSU <- (2/60)*sum(((zi1_ETH_ss1_JSU - 0.01)^2))
QPS2_ETH_ss1_JSU <- (2/60)*sum(((zi2_ETH_ss1_JSU - 0.01)^2))
QPS3_ETH_ss1_JSU <- (2/60)*sum(((zi3_ETH_ss1_JSU - 0.01)^2))

QPS_ETH_ss1_JSU <- QPS1_ETH_ss1_JSU + QPS2_ETH_ss1_JSU + QPS3_ETH_ss1_JSU
#PSIV
VaR_ETH_ss1_PSIV <- VR15_ETH
realisedval_ETH_ss1_PSIV <- VR14_ETH[,4]
zi1_ETH_ss1_PSIV <- ifelse(realisedval_ETH_ss1_PSIV < VaR_ETH_ss1_PSIV, 1, 0)
zi2_ETH_ss1_PSIV <- ifelse(realisedval_ETH_ss1_PSIV < VaR_ETH_ss1_PSIV, 1, 0)*(1+((realisedval_ETH_ss1_PSIV - VaR_ETH_ss1_PSIV)^2))
zi3_ETH_ss1_PSIV <- ifelse(realisedval_ETH_ss1_PSIV < VaR_ETH_ss1_PSIV, 1, 0)*((realisedval_ETH_ss1_PSIV - VaR_ETH_ss1_PSIV)/VaR_ETH_ss1_PSIV)

QPS1_ETH_ss1_PSIV <- (2/60)*sum(((zi1_ETH_ss1_PSIV - 0.01)^2))
QPS2_ETH_ss1_PSIV <- (2/60)*sum(((zi2_ETH_ss1_PSIV - 0.01)^2))
QPS3_ETH_ss1_PSIV <- (2/60)*sum(((zi3_ETH_ss1_PSIV - 0.01)^2))

QPS_ETH_ss1_PSIV <- QPS1_ETH_ss1_PSIV + QPS2_ETH_ss1_PSIV + QPS3_ETH_ss1_PSIV

# Sub Sample 2
#Normal
VaR_ETH_ss2_norm <- VR21_ETH[,1]
realisedval_ETH_ss2_norm <- VR21_ETH[,4]
zi1_ETH_ss2_norm <- ifelse(realisedval_ETH_ss2_norm < VaR_ETH_ss2_norm, 1, 0)
zi2_ETH_ss2_norm <- ifelse(realisedval_ETH_ss2_norm < VaR_ETH_ss2_norm, 1, 0)*(1+((realisedval_ETH_ss2_norm - VaR_ETH_ss2_norm)^2))
zi3_ETH_ss2_norm <- ifelse(realisedval_ETH_ss2_norm < VaR_ETH_ss2_norm, 1, 0)*((realisedval_ETH_ss2_norm - VaR_ETH_ss2_norm)/VaR_ETH_ss2_norm)

QPS1_ETH_ss2_norm <- (2/200)*sum(((zi1_ETH_ss2_norm - 0.01)^2))
QPS2_ETH_ss2_norm <- (2/200)*sum(((zi2_ETH_ss2_norm - 0.01)^2))
QPS3_ETH_ss2_norm <- (2/200)*sum(((zi3_ETH_ss2_norm - 0.01)^2))

QPS_ETH_ss2_norm <- QPS1_ETH_ss2_norm + QPS2_ETH_ss2_norm + QPS3_ETH_ss2_norm
#SST
VaR_ETH_ss2_SST <- VR22_ETH[,1]
realisedval_ETH_ss2_SST <- VR22_ETH[,4]
zi1_ETH_ss2_SST <- ifelse(realisedval_ETH_ss2_SST < VaR_ETH_ss2_SST, 1, 0)
zi2_ETH_ss2_SST <- ifelse(realisedval_ETH_ss2_SST < VaR_ETH_ss2_SST, 1, 0)*(1+((realisedval_ETH_ss2_SST - VaR_ETH_ss2_SST)^2))
zi3_ETH_ss2_SST <- ifelse(realisedval_ETH_ss2_SST < VaR_ETH_ss2_SST, 1, 0)*((realisedval_ETH_ss2_SST - VaR_ETH_ss2_SST)/VaR_ETH_ss2_SST)

QPS1_ETH_ss2_SST <- (2/200)*sum(((zi1_ETH_ss2_SST - 0.01)^2))
QPS2_ETH_ss2_SST <- (2/200)*sum(((zi2_ETH_ss2_SST - 0.01)^2))
QPS3_ETH_ss2_SST <- (2/200)*sum(((zi3_ETH_ss2_SST - 0.01)^2))

QPS_ETH_ss2_SST <- QPS1_ETH_ss2_SST + QPS2_ETH_ss2_SST + QPS3_ETH_ss2_SST
#SGED
VaR_ETH_ss2_SGED <- VR23_ETH[,1]
realisedval_ETH_ss2_SGED <- VR23_ETH[,4]
zi1_ETH_ss2_SGED <- ifelse(realisedval_ETH_ss2_SGED < VaR_ETH_ss2_SGED, 1, 0)
zi2_ETH_ss2_SGED <- ifelse(realisedval_ETH_ss2_SGED < VaR_ETH_ss2_SGED, 1, 0)*(1+((realisedval_ETH_ss2_SGED - VaR_ETH_ss2_SGED)^2))
zi3_ETH_ss2_SGED <- ifelse(realisedval_ETH_ss2_SGED < VaR_ETH_ss2_SGED, 1, 0)*((realisedval_ETH_ss2_SGED - VaR_ETH_ss2_SGED)/VaR_ETH_ss2_SGED)

QPS1_ETH_ss2_SGED <- (2/200)*sum(((zi1_ETH_ss2_SGED - 0.01)^2))
QPS2_ETH_ss2_SGED <- (2/200)*sum(((zi2_ETH_ss2_SGED - 0.01)^2))
QPS3_ETH_ss2_SGED <- (2/200)*sum(((zi3_ETH_ss2_SGED - 0.01)^2))

QPS_ETH_ss2_SGED <- QPS1_ETH_ss2_SGED + QPS2_ETH_ss2_SGED + QPS3_ETH_ss2_SGED
#JSU
VaR_ETH_ss2_JSU <- VR24_ETH[,1]
realisedval_ETH_ss2_JSU <- VR24_ETH[,4]
zi1_ETH_ss2_JSU <- ifelse(realisedval_ETH_ss2_JSU < VaR_ETH_ss2_JSU, 1, 0)
zi2_ETH_ss2_JSU <- ifelse(realisedval_ETH_ss2_JSU < VaR_ETH_ss2_JSU, 1, 0)*(1+((realisedval_ETH_ss2_JSU - VaR_ETH_ss2_JSU)^2))
zi3_ETH_ss2_JSU <- ifelse(realisedval_ETH_ss2_JSU < VaR_ETH_ss2_JSU, 1, 0)*((realisedval_ETH_ss2_JSU - VaR_ETH_ss2_JSU)/VaR_ETH_ss2_JSU)

QPS1_ETH_ss2_JSU <- (2/200)*sum(((zi1_ETH_ss2_JSU - 0.01)^2))
QPS2_ETH_ss2_JSU <- (2/200)*sum(((zi2_ETH_ss2_JSU - 0.01)^2))
QPS3_ETH_ss2_JSU <- (2/200)*sum(((zi3_ETH_ss2_JSU - 0.01)^2))

QPS_ETH_ss2_JSU <- QPS1_ETH_ss2_JSU + QPS2_ETH_ss2_JSU + QPS3_ETH_ss2_JSU
#PSIV
VaR_ETH_ss2_PSIV <- VR25_ETH
realisedval_ETH_ss2_PSIV <- VR24_ETH[,4]
zi1_ETH_ss2_PSIV <- ifelse(realisedval_ETH_ss2_PSIV < VaR_ETH_ss2_PSIV, 1, 0)
zi2_ETH_ss2_PSIV <- ifelse(realisedval_ETH_ss2_PSIV < VaR_ETH_ss2_PSIV, 1, 0)*(1+((realisedval_ETH_ss2_PSIV - VaR_ETH_ss2_PSIV)^2))
zi3_ETH_ss2_PSIV <- ifelse(realisedval_ETH_ss2_PSIV < VaR_ETH_ss2_PSIV, 1, 0)*((realisedval_ETH_ss2_PSIV - VaR_ETH_ss2_PSIV)/VaR_ETH_ss2_PSIV)

QPS1_ETH_ss2_PSIV <- (2/200)*sum(((zi1_ETH_ss2_PSIV - 0.01)^2))
QPS2_ETH_ss2_PSIV <- (2/200)*sum(((zi2_ETH_ss2_PSIV - 0.01)^2))
QPS3_ETH_ss2_PSIV <- (2/200)*sum(((zi3_ETH_ss2_PSIV - 0.01)^2))

QPS_ETH_ss2_PSIV <- QPS1_ETH_ss2_PSIV + QPS2_ETH_ss2_PSIV + QPS3_ETH_ss2_PSIV

# Sub Sample 3
#Normal
VaR_ETH_ss3_norm <- VR31_ETH[,1]
realisedval_ETH_ss3_norm <- VR31_ETH[,4]
zi1_ETH_ss3_norm <- ifelse(realisedval_ETH_ss3_norm < VaR_ETH_ss3_norm, 1, 0)
zi2_ETH_ss3_norm <- ifelse(realisedval_ETH_ss3_norm < VaR_ETH_ss3_norm, 1, 0)*(1+((realisedval_ETH_ss3_norm - VaR_ETH_ss3_norm)^2))
zi3_ETH_ss3_norm <- ifelse(realisedval_ETH_ss3_norm < VaR_ETH_ss3_norm, 1, 0)*((realisedval_ETH_ss3_norm - VaR_ETH_ss3_norm)/VaR_ETH_ss3_norm)

QPS1_ETH_ss3_norm <- (2/100)*sum(((zi1_ETH_ss3_norm - 0.01)^2))
QPS2_ETH_ss3_norm <- (2/100)*sum(((zi2_ETH_ss3_norm - 0.01)^2))
QPS3_ETH_ss3_norm <- (2/100)*sum(((zi3_ETH_ss3_norm - 0.01)^2))

QPS_ETH_ss3_norm <- QPS1_ETH_ss3_norm + QPS2_ETH_ss3_norm + QPS3_ETH_ss3_norm
#SST
VaR_ETH_ss3_SST <- VR32_ETH[,1]
realisedval_ETH_ss3_SST <- VR32_ETH[,4]
zi1_ETH_ss3_SST <- ifelse(realisedval_ETH_ss3_SST < VaR_ETH_ss3_SST, 1, 0)
zi2_ETH_ss3_SST <- ifelse(realisedval_ETH_ss3_SST < VaR_ETH_ss3_SST, 1, 0)*(1+((realisedval_ETH_ss3_SST - VaR_ETH_ss3_SST)^2))
zi3_ETH_ss3_SST <- ifelse(realisedval_ETH_ss3_SST < VaR_ETH_ss3_SST, 1, 0)*((realisedval_ETH_ss3_SST - VaR_ETH_ss3_SST)/VaR_ETH_ss3_SST)

QPS1_ETH_ss3_SST <- (2/100)*sum(((zi1_ETH_ss3_SST - 0.01)^2))
QPS2_ETH_ss3_SST <- (2/100)*sum(((zi2_ETH_ss3_SST - 0.01)^2))
QPS3_ETH_ss3_SST <- (2/100)*sum(((zi3_ETH_ss3_SST - 0.01)^2))

QPS_ETH_ss3_SST <- QPS1_ETH_ss3_SST + QPS2_ETH_ss3_SST + QPS3_ETH_ss3_SST
#SGED
VaR_ETH_ss3_SGED <- VR33_ETH[,1]
realisedval_ETH_ss3_SGED <- VR33_ETH[,4]
zi1_ETH_ss3_SGED <- ifelse(realisedval_ETH_ss3_SGED < VaR_ETH_ss3_SGED, 1, 0)
zi2_ETH_ss3_SGED <- ifelse(realisedval_ETH_ss3_SGED < VaR_ETH_ss3_SGED, 1, 0)*(1+((realisedval_ETH_ss3_SGED - VaR_ETH_ss3_SGED)^2))
zi3_ETH_ss3_SGED <- ifelse(realisedval_ETH_ss3_SGED < VaR_ETH_ss3_SGED, 1, 0)*((realisedval_ETH_ss3_SGED - VaR_ETH_ss3_SGED)/VaR_ETH_ss3_SGED)

QPS1_ETH_ss3_SGED <- (2/100)*sum(((zi1_ETH_ss3_SGED - 0.01)^2))
QPS2_ETH_ss3_SGED <- (2/100)*sum(((zi2_ETH_ss3_SGED - 0.01)^2))
QPS3_ETH_ss3_SGED <- (2/100)*sum(((zi3_ETH_ss3_SGED - 0.01)^2))

QPS_ETH_ss3_SGED <- QPS1_ETH_ss3_SGED + QPS2_ETH_ss3_SGED + QPS3_ETH_ss3_SGED
#JSU
VaR_ETH_ss3_JSU <- VR34_ETH[,1]
realisedval_ETH_ss3_JSU <- VR34_ETH[,4]
zi1_ETH_ss3_JSU <- ifelse(realisedval_ETH_ss3_JSU < VaR_ETH_ss3_JSU, 1, 0)
zi2_ETH_ss3_JSU <- ifelse(realisedval_ETH_ss3_JSU < VaR_ETH_ss3_JSU, 1, 0)*(1+((realisedval_ETH_ss3_JSU - VaR_ETH_ss3_JSU)^2))
zi3_ETH_ss3_JSU <- ifelse(realisedval_ETH_ss3_JSU < VaR_ETH_ss3_JSU, 1, 0)*((realisedval_ETH_ss3_JSU - VaR_ETH_ss3_JSU)/VaR_ETH_ss3_JSU)

QPS1_ETH_ss3_JSU <- (2/100)*sum(((zi1_ETH_ss3_JSU - 0.01)^2))
QPS2_ETH_ss3_JSU <- (2/100)*sum(((zi2_ETH_ss3_JSU - 0.01)^2))
QPS3_ETH_ss3_JSU <- (2/100)*sum(((zi3_ETH_ss3_JSU - 0.01)^2))

QPS_ETH_ss3_JSU <- QPS1_ETH_ss3_JSU + QPS2_ETH_ss3_JSU + QPS3_ETH_ss3_JSU
#PSIV
VaR_ETH_ss3_PSIV <- VR35_ETH
realisedval_ETH_ss3_PSIV <- VR34_ETH[,4]
zi1_ETH_ss3_PSIV <- ifelse(realisedval_ETH_ss3_PSIV < VaR_ETH_ss3_PSIV, 1, 0)
zi2_ETH_ss3_PSIV <- ifelse(realisedval_ETH_ss3_PSIV < VaR_ETH_ss3_PSIV, 1, 0)*(1+((realisedval_ETH_ss3_PSIV - VaR_ETH_ss3_PSIV)^2))
zi3_ETH_ss3_PSIV <- ifelse(realisedval_ETH_ss3_PSIV < VaR_ETH_ss3_PSIV, 1, 0)*((realisedval_ETH_ss3_PSIV - VaR_ETH_ss3_PSIV)/VaR_ETH_ss3_PSIV)

QPS1_ETH_ss3_PSIV <- (2/100)*sum(((zi1_ETH_ss3_PSIV - 0.01)^2))
QPS2_ETH_ss3_PSIV <- (2/100)*sum(((zi2_ETH_ss3_PSIV - 0.01)^2))
QPS3_ETH_ss3_PSIV <- (2/100)*sum(((zi3_ETH_ss3_PSIV - 0.01)^2))

QPS_ETH_ss3_PSIV <- QPS1_ETH_ss3_PSIV + QPS2_ETH_ss3_PSIV + QPS3_ETH_ss3_PSIV

QPS_ETH_fs <- c(QPS_ETH_fs_norm, QPS_ETH_fs_SST, QPS_ETH_fs_SGED, QPS_ETH_fs_JSU, QPS_ETH_fs_PSIV)
QPS_ETH_ss1 <- c(QPS_ETH_ss1_norm, QPS_ETH_ss1_SST, QPS_ETH_ss1_SGED, QPS_ETH_ss1_JSU, QPS_ETH_ss1_PSIV)
QPS_ETH_ss2 <- c(QPS_ETH_ss2_norm, QPS_ETH_ss2_SST, QPS_ETH_ss2_SGED, QPS_ETH_ss2_JSU, QPS_ETH_ss2_PSIV)
QPS_ETH_ss3 <- c(QPS_ETH_ss3_norm, QPS_ETH_ss3_SST, QPS_ETH_ss3_SGED, QPS_ETH_ss3_JSU, QPS_ETH_ss3_PSIV)

QPS_ETH <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_ETH <- rbind(QPS_ETH_fs, QPS_ETH_ss1, QPS_ETH_ss2, QPS_ETH_ss3)
colnames(QPS_ETH) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(QPS_ETH) <- c("Full Sample", "SS1", "SS2", "SS3")


##### :::::::: L T C :::::::: #####
# Full Sample
#Normal
VaR_LTC_fs_norm <- VR1_LTC[,1]
realisedval_LTC_fs_norm <- VR1_LTC[,4]
zi1_LTC_fs_norm <- ifelse(realisedval_LTC_fs_norm < VaR_LTC_fs_norm, 1, 0)
zi2_LTC_fs_norm <- ifelse(realisedval_LTC_fs_norm < VaR_LTC_fs_norm, 1, 0)*(1+((realisedval_LTC_fs_norm - VaR_LTC_fs_norm)^2))
zi3_LTC_fs_norm <- ifelse(realisedval_LTC_fs_norm < VaR_LTC_fs_norm, 1, 0)*((realisedval_LTC_fs_norm - VaR_LTC_fs_norm)/VaR_LTC_fs_norm)

QPS1_LTC_fs_norm <- (2/350)*sum(((zi1_LTC_fs_norm - 0.01)^2))
QPS2_LTC_fs_norm <- (2/350)*sum(((zi2_LTC_fs_norm - 0.01)^2))
QPS3_LTC_fs_norm <- (2/350)*sum(((zi3_LTC_fs_norm - 0.01)^2))

QPS_LTC_fs_norm <- QPS1_LTC_fs_norm + QPS2_LTC_fs_norm + QPS3_LTC_fs_norm 
#SST
VaR_LTC_fs_SST <- VR2_LTC[,1]
realisedval_LTC_fs_SST <- VR2_LTC[,4]
zi1_LTC_fs_SST <- ifelse(realisedval_LTC_fs_SST < VaR_LTC_fs_SST, 1, 0)
zi2_LTC_fs_SST <- ifelse(realisedval_LTC_fs_SST < VaR_LTC_fs_SST, 1, 0)*(1+((realisedval_LTC_fs_SST - VaR_LTC_fs_SST)^2))
zi3_LTC_fs_SST <- ifelse(realisedval_LTC_fs_SST < VaR_LTC_fs_SST, 1, 0)*((realisedval_LTC_fs_SST - VaR_LTC_fs_SST)/VaR_LTC_fs_SST)

QPS1_LTC_fs_SST <- (2/350)*sum(((zi1_LTC_fs_SST - 0.01)^2))
QPS2_LTC_fs_SST <- (2/350)*sum(((zi2_LTC_fs_SST - 0.01)^2))
QPS3_LTC_fs_SST <- (2/350)*sum(((zi3_LTC_fs_SST - 0.01)^2))

QPS_LTC_fs_SST <- QPS1_LTC_fs_SST + QPS2_LTC_fs_SST + QPS3_LTC_fs_SST
#SGED
VaR_LTC_fs_SGED <- VR3_LTC[,1]
realisedval_LTC_fs_SGED <- VR3_LTC[,4]
zi1_LTC_fs_SGED <- ifelse(realisedval_LTC_fs_SGED < VaR_LTC_fs_SGED, 1, 0)
zi2_LTC_fs_SGED <- ifelse(realisedval_LTC_fs_SGED < VaR_LTC_fs_SGED, 1, 0)*(1+((realisedval_LTC_fs_SGED - VaR_LTC_fs_SGED)^2))
zi3_LTC_fs_SGED <- ifelse(realisedval_LTC_fs_SGED < VaR_LTC_fs_SGED, 1, 0)*((realisedval_LTC_fs_SGED - VaR_LTC_fs_SGED)/VaR_LTC_fs_SGED)

QPS1_LTC_fs_SGED <- (2/350)*sum(((zi1_LTC_fs_SGED - 0.01)^2))
QPS2_LTC_fs_SGED <- (2/350)*sum(((zi2_LTC_fs_SGED - 0.01)^2))
QPS3_LTC_fs_SGED <- (2/350)*sum(((zi3_LTC_fs_SGED - 0.01)^2))

QPS_LTC_fs_SGED <- QPS1_LTC_fs_SGED + QPS2_LTC_fs_SGED + QPS3_LTC_fs_SGED
#JSU
VaR_LTC_fs_JSU <- VR4_LTC[,1]
realisedval_LTC_fs_JSU <- VR4_LTC[,4]
zi1_LTC_fs_JSU <- ifelse(realisedval_LTC_fs_JSU < VaR_LTC_fs_JSU, 1, 0)
zi2_LTC_fs_JSU <- ifelse(realisedval_LTC_fs_JSU < VaR_LTC_fs_JSU, 1, 0)*(1+((realisedval_LTC_fs_JSU - VaR_LTC_fs_JSU)^2))
zi3_LTC_fs_JSU <- ifelse(realisedval_LTC_fs_JSU < VaR_LTC_fs_JSU, 1, 0)*((realisedval_LTC_fs_JSU - VaR_LTC_fs_JSU)/VaR_LTC_fs_JSU)

QPS1_LTC_fs_JSU <- (2/350)*sum(((zi1_LTC_fs_JSU - 0.01)^2))
QPS2_LTC_fs_JSU <- (2/350)*sum(((zi2_LTC_fs_JSU - 0.01)^2))
QPS3_LTC_fs_JSU <- (2/350)*sum(((zi3_LTC_fs_JSU - 0.01)^2))

QPS_LTC_fs_JSU <- QPS1_LTC_fs_JSU + QPS2_LTC_fs_JSU + QPS3_LTC_fs_JSU
#PSIV
VaR_LTC_fs_PSIV <- VR5_LTC
realisedval_LTC_fs_PSIV <- VR4_LTC[,4]
zi1_LTC_fs_PSIV <- ifelse(realisedval_LTC_fs_PSIV < VaR_LTC_fs_PSIV, 1, 0)
zi2_LTC_fs_PSIV <- ifelse(realisedval_LTC_fs_PSIV < VaR_LTC_fs_PSIV, 1, 0)*(1+((realisedval_LTC_fs_PSIV - VaR_LTC_fs_PSIV)^2))
zi3_LTC_fs_PSIV <- ifelse(realisedval_LTC_fs_PSIV < VaR_LTC_fs_PSIV, 1, 0)*((realisedval_LTC_fs_PSIV - VaR_LTC_fs_PSIV)/VaR_LTC_fs_PSIV)

QPS1_LTC_fs_PSIV <- (2/350)*sum(((zi1_LTC_fs_PSIV - 0.01)^2))
QPS2_LTC_fs_PSIV <- (2/350)*sum(((zi2_LTC_fs_PSIV - 0.01)^2))
QPS3_LTC_fs_PSIV <- (2/350)*sum(((zi3_LTC_fs_PSIV - 0.01)^2))

QPS_LTC_fs_PSIV <- QPS1_LTC_fs_PSIV + QPS2_LTC_fs_PSIV + QPS3_LTC_fs_PSIV

# Sub Sample 1
#Normal
VaR_LTC_ss1_norm <- VR11_LTC[,1]
realisedval_LTC_ss1_norm <- VR11_LTC[,4]
zi1_LTC_ss1_norm <- ifelse(realisedval_LTC_ss1_norm < VaR_LTC_ss1_norm, 1, 0)
zi2_LTC_ss1_norm <- ifelse(realisedval_LTC_ss1_norm < VaR_LTC_ss1_norm, 1, 0)*(1+((realisedval_LTC_ss1_norm - VaR_LTC_ss1_norm)^2))
zi3_LTC_ss1_norm <- ifelse(realisedval_LTC_ss1_norm < VaR_LTC_ss1_norm, 1, 0)*((realisedval_LTC_ss1_norm - VaR_LTC_ss1_norm)/VaR_LTC_ss1_norm)

QPS1_LTC_ss1_norm <- (2/60)*sum(((zi1_LTC_ss1_norm - 0.01)^2))
QPS2_LTC_ss1_norm <- (2/60)*sum(((zi2_LTC_ss1_norm - 0.01)^2))
QPS3_LTC_ss1_norm <- (2/60)*sum(((zi3_LTC_ss1_norm - 0.01)^2))

QPS_LTC_ss1_norm <- QPS1_LTC_ss1_norm + QPS2_LTC_ss1_norm + QPS3_LTC_ss1_norm
#SST
VaR_LTC_ss1_SST <- VR12_LTC[,1]
realisedval_LTC_ss1_SST <- VR12_LTC[,4]
zi1_LTC_ss1_SST <- ifelse(realisedval_LTC_ss1_SST < VaR_LTC_ss1_SST, 1, 0)
zi2_LTC_ss1_SST <- ifelse(realisedval_LTC_ss1_SST < VaR_LTC_ss1_SST, 1, 0)*(1+((realisedval_LTC_ss1_SST - VaR_LTC_ss1_SST)^2))
zi3_LTC_ss1_SST <- ifelse(realisedval_LTC_ss1_SST < VaR_LTC_ss1_SST, 1, 0)*((realisedval_LTC_ss1_SST - VaR_LTC_ss1_SST)/VaR_LTC_ss1_SST)

QPS1_LTC_ss1_SST <- (2/60)*sum(((zi1_LTC_ss1_SST - 0.01)^2))
QPS2_LTC_ss1_SST <- (2/60)*sum(((zi2_LTC_ss1_SST - 0.01)^2))
QPS3_LTC_ss1_SST <- (2/60)*sum(((zi3_LTC_ss1_SST - 0.01)^2))

QPS_LTC_ss1_SST <- QPS1_LTC_ss1_SST + QPS2_LTC_ss1_SST + QPS3_LTC_ss1_SST
#SGED
VaR_LTC_ss1_SGED <- VR13_LTC[,1]
realisedval_LTC_ss1_SGED <- VR13_LTC[,4]
zi1_LTC_ss1_SGED <- ifelse(realisedval_LTC_ss1_SGED < VaR_LTC_ss1_SGED, 1, 0)
zi2_LTC_ss1_SGED <- ifelse(realisedval_LTC_ss1_SGED < VaR_LTC_ss1_SGED, 1, 0)*(1+((realisedval_LTC_ss1_SGED - VaR_LTC_ss1_SGED)^2))
zi3_LTC_ss1_SGED <- ifelse(realisedval_LTC_ss1_SGED < VaR_LTC_ss1_SGED, 1, 0)*((realisedval_LTC_ss1_SGED - VaR_LTC_ss1_SGED)/VaR_LTC_ss1_SGED)

QPS1_LTC_ss1_SGED <- (2/60)*sum(((zi1_LTC_ss1_SGED - 0.01)^2))
QPS2_LTC_ss1_SGED <- (2/60)*sum(((zi2_LTC_ss1_SGED - 0.01)^2))
QPS3_LTC_ss1_SGED <- (2/60)*sum(((zi3_LTC_ss1_SGED - 0.01)^2))

QPS_LTC_ss1_SGED <- QPS1_LTC_ss1_SGED + QPS2_LTC_ss1_SGED + QPS3_LTC_ss1_SGED
#JSU
VaR_LTC_ss1_JSU <- VR14_LTC[,1]
realisedval_LTC_ss1_JSU <- VR14_LTC[,4]
zi1_LTC_ss1_JSU <- ifelse(realisedval_LTC_ss1_JSU < VaR_LTC_ss1_JSU, 1, 0)
zi2_LTC_ss1_JSU <- ifelse(realisedval_LTC_ss1_JSU < VaR_LTC_ss1_JSU, 1, 0)*(1+((realisedval_LTC_ss1_JSU - VaR_LTC_ss1_JSU)^2))
zi3_LTC_ss1_JSU <- ifelse(realisedval_LTC_ss1_JSU < VaR_LTC_ss1_JSU, 1, 0)*((realisedval_LTC_ss1_JSU - VaR_LTC_ss1_JSU)/VaR_LTC_ss1_JSU)

QPS1_LTC_ss1_JSU <- (2/60)*sum(((zi1_LTC_ss1_JSU - 0.01)^2))
QPS2_LTC_ss1_JSU <- (2/60)*sum(((zi2_LTC_ss1_JSU - 0.01)^2))
QPS3_LTC_ss1_JSU <- (2/60)*sum(((zi3_LTC_ss1_JSU - 0.01)^2))

QPS_LTC_ss1_JSU <- QPS1_LTC_ss1_JSU + QPS2_LTC_ss1_JSU + QPS3_LTC_ss1_JSU
#PSIV
VaR_LTC_ss1_PSIV <- VR15_LTC
realisedval_LTC_ss1_PSIV <- VR14_LTC[,4]
zi1_LTC_ss1_PSIV <- ifelse(realisedval_LTC_ss1_PSIV < VaR_LTC_ss1_PSIV, 1, 0)
zi2_LTC_ss1_PSIV <- ifelse(realisedval_LTC_ss1_PSIV < VaR_LTC_ss1_PSIV, 1, 0)*(1+((realisedval_LTC_ss1_PSIV - VaR_LTC_ss1_PSIV)^2))
zi3_LTC_ss1_PSIV <- ifelse(realisedval_LTC_ss1_PSIV < VaR_LTC_ss1_PSIV, 1, 0)*((realisedval_LTC_ss1_PSIV - VaR_LTC_ss1_PSIV)/VaR_LTC_ss1_PSIV)

QPS1_LTC_ss1_PSIV <- (2/60)*sum(((zi1_LTC_ss1_PSIV - 0.01)^2))
QPS2_LTC_ss1_PSIV <- (2/60)*sum(((zi2_LTC_ss1_PSIV - 0.01)^2))
QPS3_LTC_ss1_PSIV <- (2/60)*sum(((zi3_LTC_ss1_PSIV - 0.01)^2))

QPS_LTC_ss1_PSIV <- QPS1_LTC_ss1_PSIV + QPS2_LTC_ss1_PSIV + QPS3_LTC_ss1_PSIV

# Sub Sample 2
#Normal
VaR_LTC_ss2_norm <- VR21_LTC[,1]
realisedval_LTC_ss2_norm <- VR21_LTC[,4]
zi1_LTC_ss2_norm <- ifelse(realisedval_LTC_ss2_norm < VaR_LTC_ss2_norm, 1, 0)
zi2_LTC_ss2_norm <- ifelse(realisedval_LTC_ss2_norm < VaR_LTC_ss2_norm, 1, 0)*(1+((realisedval_LTC_ss2_norm - VaR_LTC_ss2_norm)^2))
zi3_LTC_ss2_norm <- ifelse(realisedval_LTC_ss2_norm < VaR_LTC_ss2_norm, 1, 0)*((realisedval_LTC_ss2_norm - VaR_LTC_ss2_norm)/VaR_LTC_ss2_norm)

QPS1_LTC_ss2_norm <- (2/200)*sum(((zi1_LTC_ss2_norm - 0.01)^2))
QPS2_LTC_ss2_norm <- (2/200)*sum(((zi2_LTC_ss2_norm - 0.01)^2))
QPS3_LTC_ss2_norm <- (2/200)*sum(((zi3_LTC_ss2_norm - 0.01)^2))

QPS_LTC_ss2_norm <- QPS1_LTC_ss2_norm + QPS2_LTC_ss2_norm + QPS3_LTC_ss2_norm
#SST
VaR_LTC_ss2_SST <- VR22_LTC[,1]
realisedval_LTC_ss2_SST <- VR22_LTC[,4]
zi1_LTC_ss2_SST <- ifelse(realisedval_LTC_ss2_SST < VaR_LTC_ss2_SST, 1, 0)
zi2_LTC_ss2_SST <- ifelse(realisedval_LTC_ss2_SST < VaR_LTC_ss2_SST, 1, 0)*(1+((realisedval_LTC_ss2_SST - VaR_LTC_ss2_SST)^2))
zi3_LTC_ss2_SST <- ifelse(realisedval_LTC_ss2_SST < VaR_LTC_ss2_SST, 1, 0)*((realisedval_LTC_ss2_SST - VaR_LTC_ss2_SST)/VaR_LTC_ss2_SST)

QPS1_LTC_ss2_SST <- (2/200)*sum(((zi1_LTC_ss2_SST - 0.01)^2))
QPS2_LTC_ss2_SST <- (2/200)*sum(((zi2_LTC_ss2_SST - 0.01)^2))
QPS3_LTC_ss2_SST <- (2/200)*sum(((zi3_LTC_ss2_SST - 0.01)^2))

QPS_LTC_ss2_SST <- QPS1_LTC_ss2_SST + QPS2_LTC_ss2_SST + QPS3_LTC_ss2_SST
#SGED
VaR_LTC_ss2_SGED <- VR23_LTC[,1]
realisedval_LTC_ss2_SGED <- VR23_LTC[,4]
zi1_LTC_ss2_SGED <- ifelse(realisedval_LTC_ss2_SGED < VaR_LTC_ss2_SGED, 1, 0)
zi2_LTC_ss2_SGED <- ifelse(realisedval_LTC_ss2_SGED < VaR_LTC_ss2_SGED, 1, 0)*(1+((realisedval_LTC_ss2_SGED - VaR_LTC_ss2_SGED)^2))
zi3_LTC_ss2_SGED <- ifelse(realisedval_LTC_ss2_SGED < VaR_LTC_ss2_SGED, 1, 0)*((realisedval_LTC_ss2_SGED - VaR_LTC_ss2_SGED)/VaR_LTC_ss2_SGED)

QPS1_LTC_ss2_SGED <- (2/200)*sum(((zi1_LTC_ss2_SGED - 0.01)^2))
QPS2_LTC_ss2_SGED <- (2/200)*sum(((zi2_LTC_ss2_SGED - 0.01)^2))
QPS3_LTC_ss2_SGED <- (2/200)*sum(((zi3_LTC_ss2_SGED - 0.01)^2))

QPS_LTC_ss2_SGED <- QPS1_LTC_ss2_SGED + QPS2_LTC_ss2_SGED + QPS3_LTC_ss2_SGED
#JSU
VaR_LTC_ss2_JSU <- VR24_LTC[,1]
realisedval_LTC_ss2_JSU <- VR24_LTC[,4]
zi1_LTC_ss2_JSU <- ifelse(realisedval_LTC_ss2_JSU < VaR_LTC_ss2_JSU, 1, 0)
zi2_LTC_ss2_JSU <- ifelse(realisedval_LTC_ss2_JSU < VaR_LTC_ss2_JSU, 1, 0)*(1+((realisedval_LTC_ss2_JSU - VaR_LTC_ss2_JSU)^2))
zi3_LTC_ss2_JSU <- ifelse(realisedval_LTC_ss2_JSU < VaR_LTC_ss2_JSU, 1, 0)*((realisedval_LTC_ss2_JSU - VaR_LTC_ss2_JSU)/VaR_LTC_ss2_JSU)

QPS1_LTC_ss2_JSU <- (2/200)*sum(((zi1_LTC_ss2_JSU - 0.01)^2))
QPS2_LTC_ss2_JSU <- (2/200)*sum(((zi2_LTC_ss2_JSU - 0.01)^2))
QPS3_LTC_ss2_JSU <- (2/200)*sum(((zi3_LTC_ss2_JSU - 0.01)^2))

QPS_LTC_ss2_JSU <- QPS1_LTC_ss2_JSU + QPS2_LTC_ss2_JSU + QPS3_LTC_ss2_JSU
#PSIV
VaR_LTC_ss2_PSIV <- VR25_LTC
realisedval_LTC_ss2_PSIV <- VR24_LTC[,4]
zi1_LTC_ss2_PSIV <- ifelse(realisedval_LTC_ss2_PSIV < VaR_LTC_ss2_PSIV, 1, 0)
zi2_LTC_ss2_PSIV <- ifelse(realisedval_LTC_ss2_PSIV < VaR_LTC_ss2_PSIV, 1, 0)*(1+((realisedval_LTC_ss2_PSIV - VaR_LTC_ss2_PSIV)^2))
zi3_LTC_ss2_PSIV <- ifelse(realisedval_LTC_ss2_PSIV < VaR_LTC_ss2_PSIV, 1, 0)*((realisedval_LTC_ss2_PSIV - VaR_LTC_ss2_PSIV)/VaR_LTC_ss2_PSIV)

QPS1_LTC_ss2_PSIV <- (2/200)*sum(((zi1_LTC_ss2_PSIV - 0.01)^2))
QPS2_LTC_ss2_PSIV <- (2/200)*sum(((zi2_LTC_ss2_PSIV - 0.01)^2))
QPS3_LTC_ss2_PSIV <- (2/200)*sum(((zi3_LTC_ss2_PSIV - 0.01)^2))

QPS_LTC_ss2_PSIV <- QPS1_LTC_ss2_PSIV + QPS2_LTC_ss2_PSIV + QPS3_LTC_ss2_PSIV

# Sub Sample 3
#Normal
VaR_LTC_ss3_norm <- VR31_LTC[,1]
realisedval_LTC_ss3_norm <- VR31_LTC[,4]
zi1_LTC_ss3_norm <- ifelse(realisedval_LTC_ss3_norm < VaR_LTC_ss3_norm, 1, 0)
zi2_LTC_ss3_norm <- ifelse(realisedval_LTC_ss3_norm < VaR_LTC_ss3_norm, 1, 0)*(1+((realisedval_LTC_ss3_norm - VaR_LTC_ss3_norm)^2))
zi3_LTC_ss3_norm <- ifelse(realisedval_LTC_ss3_norm < VaR_LTC_ss3_norm, 1, 0)*((realisedval_LTC_ss3_norm - VaR_LTC_ss3_norm)/VaR_LTC_ss3_norm)

QPS1_LTC_ss3_norm <- (2/100)*sum(((zi1_LTC_ss3_norm - 0.01)^2))
QPS2_LTC_ss3_norm <- (2/100)*sum(((zi2_LTC_ss3_norm - 0.01)^2))
QPS3_LTC_ss3_norm <- (2/100)*sum(((zi3_LTC_ss3_norm - 0.01)^2))

QPS_LTC_ss3_norm <- QPS1_LTC_ss3_norm + QPS2_LTC_ss3_norm + QPS3_LTC_ss3_norm
#SST
VaR_LTC_ss3_SST <- VR32_LTC[,1]
realisedval_LTC_ss3_SST <- VR32_LTC[,4]
zi1_LTC_ss3_SST <- ifelse(realisedval_LTC_ss3_SST < VaR_LTC_ss3_SST, 1, 0)
zi2_LTC_ss3_SST <- ifelse(realisedval_LTC_ss3_SST < VaR_LTC_ss3_SST, 1, 0)*(1+((realisedval_LTC_ss3_SST - VaR_LTC_ss3_SST)^2))
zi3_LTC_ss3_SST <- ifelse(realisedval_LTC_ss3_SST < VaR_LTC_ss3_SST, 1, 0)*((realisedval_LTC_ss3_SST - VaR_LTC_ss3_SST)/VaR_LTC_ss3_SST)

QPS1_LTC_ss3_SST <- (2/100)*sum(((zi1_LTC_ss3_SST - 0.01)^2))
QPS2_LTC_ss3_SST <- (2/100)*sum(((zi2_LTC_ss3_SST - 0.01)^2))
QPS3_LTC_ss3_SST <- (2/100)*sum(((zi3_LTC_ss3_SST - 0.01)^2))

QPS_LTC_ss3_SST <- QPS1_LTC_ss3_SST + QPS2_LTC_ss3_SST + QPS3_LTC_ss3_SST
#SGED
VaR_LTC_ss3_SGED <- VR33_LTC[,1]
realisedval_LTC_ss3_SGED <- VR33_LTC[,4]
zi1_LTC_ss3_SGED <- ifelse(realisedval_LTC_ss3_SGED < VaR_LTC_ss3_SGED, 1, 0)
zi2_LTC_ss3_SGED <- ifelse(realisedval_LTC_ss3_SGED < VaR_LTC_ss3_SGED, 1, 0)*(1+((realisedval_LTC_ss3_SGED - VaR_LTC_ss3_SGED)^2))
zi3_LTC_ss3_SGED <- ifelse(realisedval_LTC_ss3_SGED < VaR_LTC_ss3_SGED, 1, 0)*((realisedval_LTC_ss3_SGED - VaR_LTC_ss3_SGED)/VaR_LTC_ss3_SGED)

QPS1_LTC_ss3_SGED <- (2/100)*sum(((zi1_LTC_ss3_SGED - 0.01)^2))
QPS2_LTC_ss3_SGED <- (2/100)*sum(((zi2_LTC_ss3_SGED - 0.01)^2))
QPS3_LTC_ss3_SGED <- (2/100)*sum(((zi3_LTC_ss3_SGED - 0.01)^2))

QPS_LTC_ss3_SGED <- QPS1_LTC_ss3_SGED + QPS2_LTC_ss3_SGED + QPS3_LTC_ss3_SGED
#JSU
VaR_LTC_ss3_JSU <- VR34_LTC[,1]
realisedval_LTC_ss3_JSU <- VR34_LTC[,4]
zi1_LTC_ss3_JSU <- ifelse(realisedval_LTC_ss3_JSU < VaR_LTC_ss3_JSU, 1, 0)
zi2_LTC_ss3_JSU <- ifelse(realisedval_LTC_ss3_JSU < VaR_LTC_ss3_JSU, 1, 0)*(1+((realisedval_LTC_ss3_JSU - VaR_LTC_ss3_JSU)^2))
zi3_LTC_ss3_JSU <- ifelse(realisedval_LTC_ss3_JSU < VaR_LTC_ss3_JSU, 1, 0)*((realisedval_LTC_ss3_JSU - VaR_LTC_ss3_JSU)/VaR_LTC_ss3_JSU)

QPS1_LTC_ss3_JSU <- (2/100)*sum(((zi1_LTC_ss3_JSU - 0.01)^2))
QPS2_LTC_ss3_JSU <- (2/100)*sum(((zi2_LTC_ss3_JSU - 0.01)^2))
QPS3_LTC_ss3_JSU <- (2/100)*sum(((zi3_LTC_ss3_JSU - 0.01)^2))

QPS_LTC_ss3_JSU <- QPS1_LTC_ss3_JSU + QPS2_LTC_ss3_JSU + QPS3_LTC_ss3_JSU
#PSIV
VaR_LTC_ss3_PSIV <- VR35_LTC
realisedval_LTC_ss3_PSIV <- VR34_LTC[,4]
zi1_LTC_ss3_PSIV <- ifelse(realisedval_LTC_ss3_PSIV < VaR_LTC_ss3_PSIV, 1, 0)
zi2_LTC_ss3_PSIV <- ifelse(realisedval_LTC_ss3_PSIV < VaR_LTC_ss3_PSIV, 1, 0)*(1+((realisedval_LTC_ss3_PSIV - VaR_LTC_ss3_PSIV)^2))
zi3_LTC_ss3_PSIV <- ifelse(realisedval_LTC_ss3_PSIV < VaR_LTC_ss3_PSIV, 1, 0)*((realisedval_LTC_ss3_PSIV - VaR_LTC_ss3_PSIV)/VaR_LTC_ss3_PSIV)

QPS1_LTC_ss3_PSIV <- (2/100)*sum(((zi1_LTC_ss3_PSIV - 0.01)^2))
QPS2_LTC_ss3_PSIV <- (2/100)*sum(((zi2_LTC_ss3_PSIV - 0.01)^2))
QPS3_LTC_ss3_PSIV <- (2/100)*sum(((zi3_LTC_ss3_PSIV - 0.01)^2))

QPS_LTC_ss3_PSIV <- QPS1_LTC_ss3_PSIV + QPS2_LTC_ss3_PSIV + QPS3_LTC_ss3_PSIV

QPS_LTC_fs <- c(QPS_LTC_fs_norm, QPS_LTC_fs_SST, QPS_LTC_fs_SGED, QPS_LTC_fs_JSU, QPS_LTC_fs_PSIV)
QPS_LTC_ss1 <- c(QPS_LTC_ss1_norm, QPS_LTC_ss1_SST, QPS_LTC_ss1_SGED, QPS_LTC_ss1_JSU, QPS_LTC_ss1_PSIV)
QPS_LTC_ss2 <- c(QPS_LTC_ss2_norm, QPS_LTC_ss2_SST, QPS_LTC_ss2_SGED, QPS_LTC_ss2_JSU, QPS_LTC_ss2_PSIV)
QPS_LTC_ss3 <- c(QPS_LTC_ss3_norm, QPS_LTC_ss3_SST, QPS_LTC_ss3_SGED, QPS_LTC_ss3_JSU, QPS_LTC_ss3_PSIV)

QPS_LTC <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_LTC <- rbind(QPS_LTC_fs, QPS_LTC_ss1, QPS_LTC_ss2, QPS_LTC_ss3)
colnames(QPS_LTC) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(QPS_LTC) <- c("Full Sample", "SS1", "SS2", "SS3")


##### :::::::: X M R :::::::: #####
# Full Sample
#Normal
VaR_XMR_fs_norm <- VR1_XMR[,1]
realisedval_XMR_fs_norm <- VR1_XMR[,4]
zi1_XMR_fs_norm <- ifelse(realisedval_XMR_fs_norm < VaR_XMR_fs_norm, 1, 0)
zi2_XMR_fs_norm <- ifelse(realisedval_XMR_fs_norm < VaR_XMR_fs_norm, 1, 0)*(1+((realisedval_XMR_fs_norm - VaR_XMR_fs_norm)^2))
zi3_XMR_fs_norm <- ifelse(realisedval_XMR_fs_norm < VaR_XMR_fs_norm, 1, 0)*((realisedval_XMR_fs_norm - VaR_XMR_fs_norm)/VaR_XMR_fs_norm)

QPS1_XMR_fs_norm <- (2/350)*sum(((zi1_XMR_fs_norm - 0.01)^2))
QPS2_XMR_fs_norm <- (2/350)*sum(((zi2_XMR_fs_norm - 0.01)^2))
QPS3_XMR_fs_norm <- (2/350)*sum(((zi3_XMR_fs_norm - 0.01)^2))

QPS_XMR_fs_norm <- QPS1_XMR_fs_norm + QPS2_XMR_fs_norm + QPS3_XMR_fs_norm 
#SST
VaR_XMR_fs_SST <- VR2_XMR[,1]
realisedval_XMR_fs_SST <- VR2_XMR[,4]
zi1_XMR_fs_SST <- ifelse(realisedval_XMR_fs_SST < VaR_XMR_fs_SST, 1, 0)
zi2_XMR_fs_SST <- ifelse(realisedval_XMR_fs_SST < VaR_XMR_fs_SST, 1, 0)*(1+((realisedval_XMR_fs_SST - VaR_XMR_fs_SST)^2))
zi3_XMR_fs_SST <- ifelse(realisedval_XMR_fs_SST < VaR_XMR_fs_SST, 1, 0)*((realisedval_XMR_fs_SST - VaR_XMR_fs_SST)/VaR_XMR_fs_SST)

QPS1_XMR_fs_SST <- (2/350)*sum(((zi1_XMR_fs_SST - 0.01)^2))
QPS2_XMR_fs_SST <- (2/350)*sum(((zi2_XMR_fs_SST - 0.01)^2))
QPS3_XMR_fs_SST <- (2/350)*sum(((zi3_XMR_fs_SST - 0.01)^2))

QPS_XMR_fs_SST <- QPS1_XMR_fs_SST + QPS2_XMR_fs_SST + QPS3_XMR_fs_SST
#SGED
VaR_XMR_fs_SGED <- VR3_XMR[,1]
realisedval_XMR_fs_SGED <- VR3_XMR[,4]
zi1_XMR_fs_SGED <- ifelse(realisedval_XMR_fs_SGED < VaR_XMR_fs_SGED, 1, 0)
zi2_XMR_fs_SGED <- ifelse(realisedval_XMR_fs_SGED < VaR_XMR_fs_SGED, 1, 0)*(1+((realisedval_XMR_fs_SGED - VaR_XMR_fs_SGED)^2))
zi3_XMR_fs_SGED <- ifelse(realisedval_XMR_fs_SGED < VaR_XMR_fs_SGED, 1, 0)*((realisedval_XMR_fs_SGED - VaR_XMR_fs_SGED)/VaR_XMR_fs_SGED)

QPS1_XMR_fs_SGED <- (2/350)*sum(((zi1_XMR_fs_SGED - 0.01)^2))
QPS2_XMR_fs_SGED <- (2/350)*sum(((zi2_XMR_fs_SGED - 0.01)^2))
QPS3_XMR_fs_SGED <- (2/350)*sum(((zi3_XMR_fs_SGED - 0.01)^2))

QPS_XMR_fs_SGED <- QPS1_XMR_fs_SGED + QPS2_XMR_fs_SGED + QPS3_XMR_fs_SGED
#JSU
VaR_XMR_fs_JSU <- VR4_XMR[,1]
realisedval_XMR_fs_JSU <- VR4_XMR[,4]
zi1_XMR_fs_JSU <- ifelse(realisedval_XMR_fs_JSU < VaR_XMR_fs_JSU, 1, 0)
zi2_XMR_fs_JSU <- ifelse(realisedval_XMR_fs_JSU < VaR_XMR_fs_JSU, 1, 0)*(1+((realisedval_XMR_fs_JSU - VaR_XMR_fs_JSU)^2))
zi3_XMR_fs_JSU <- ifelse(realisedval_XMR_fs_JSU < VaR_XMR_fs_JSU, 1, 0)*((realisedval_XMR_fs_JSU - VaR_XMR_fs_JSU)/VaR_XMR_fs_JSU)

QPS1_XMR_fs_JSU <- (2/350)*sum(((zi1_XMR_fs_JSU - 0.01)^2))
QPS2_XMR_fs_JSU <- (2/350)*sum(((zi2_XMR_fs_JSU - 0.01)^2))
QPS3_XMR_fs_JSU <- (2/350)*sum(((zi3_XMR_fs_JSU - 0.01)^2))

QPS_XMR_fs_JSU <- QPS1_XMR_fs_JSU + QPS2_XMR_fs_JSU + QPS3_XMR_fs_JSU
#PSIV
VaR_XMR_fs_PSIV <- VR5_XMR
realisedval_XMR_fs_PSIV <- VR4_XMR[,4]
zi1_XMR_fs_PSIV <- ifelse(realisedval_XMR_fs_PSIV < VaR_XMR_fs_PSIV, 1, 0)
zi2_XMR_fs_PSIV <- ifelse(realisedval_XMR_fs_PSIV < VaR_XMR_fs_PSIV, 1, 0)*(1+((realisedval_XMR_fs_PSIV - VaR_XMR_fs_PSIV)^2))
zi3_XMR_fs_PSIV <- ifelse(realisedval_XMR_fs_PSIV < VaR_XMR_fs_PSIV, 1, 0)*((realisedval_XMR_fs_PSIV - VaR_XMR_fs_PSIV)/VaR_XMR_fs_PSIV)

QPS1_XMR_fs_PSIV <- (2/350)*sum(((zi1_XMR_fs_PSIV - 0.01)^2))
QPS2_XMR_fs_PSIV <- (2/350)*sum(((zi2_XMR_fs_PSIV - 0.01)^2))
QPS3_XMR_fs_PSIV <- (2/350)*sum(((zi3_XMR_fs_PSIV - 0.01)^2))

QPS_XMR_fs_PSIV <- QPS1_XMR_fs_PSIV + QPS2_XMR_fs_PSIV + QPS3_XMR_fs_PSIV

# Sub Sample 1
#Normal
VaR_XMR_ss1_norm <- VR11_XMR[,1]
realisedval_XMR_ss1_norm <- VR11_XMR[,4]
zi1_XMR_ss1_norm <- ifelse(realisedval_XMR_ss1_norm < VaR_XMR_ss1_norm, 1, 0)
zi2_XMR_ss1_norm <- ifelse(realisedval_XMR_ss1_norm < VaR_XMR_ss1_norm, 1, 0)*(1+((realisedval_XMR_ss1_norm - VaR_XMR_ss1_norm)^2))
zi3_XMR_ss1_norm <- ifelse(realisedval_XMR_ss1_norm < VaR_XMR_ss1_norm, 1, 0)*((realisedval_XMR_ss1_norm - VaR_XMR_ss1_norm)/VaR_XMR_ss1_norm)

QPS1_XMR_ss1_norm <- (2/60)*sum(((zi1_XMR_ss1_norm - 0.01)^2))
QPS2_XMR_ss1_norm <- (2/60)*sum(((zi2_XMR_ss1_norm - 0.01)^2))
QPS3_XMR_ss1_norm <- (2/60)*sum(((zi3_XMR_ss1_norm - 0.01)^2))

QPS_XMR_ss1_norm <- QPS1_XMR_ss1_norm + QPS2_XMR_ss1_norm + QPS3_XMR_ss1_norm
#SST
VaR_XMR_ss1_SST <- VR12_XMR[,1]
realisedval_XMR_ss1_SST <- VR12_XMR[,4]
zi1_XMR_ss1_SST <- ifelse(realisedval_XMR_ss1_SST < VaR_XMR_ss1_SST, 1, 0)
zi2_XMR_ss1_SST <- ifelse(realisedval_XMR_ss1_SST < VaR_XMR_ss1_SST, 1, 0)*(1+((realisedval_XMR_ss1_SST - VaR_XMR_ss1_SST)^2))
zi3_XMR_ss1_SST <- ifelse(realisedval_XMR_ss1_SST < VaR_XMR_ss1_SST, 1, 0)*((realisedval_XMR_ss1_SST - VaR_XMR_ss1_SST)/VaR_XMR_ss1_SST)

QPS1_XMR_ss1_SST <- (2/60)*sum(((zi1_XMR_ss1_SST - 0.01)^2))
QPS2_XMR_ss1_SST <- (2/60)*sum(((zi2_XMR_ss1_SST - 0.01)^2))
QPS3_XMR_ss1_SST <- (2/60)*sum(((zi3_XMR_ss1_SST - 0.01)^2))

QPS_XMR_ss1_SST <- QPS1_XMR_ss1_SST + QPS2_XMR_ss1_SST + QPS3_XMR_ss1_SST
#SGED
VaR_XMR_ss1_SGED <- VR13_XMR[,1]
realisedval_XMR_ss1_SGED <- VR13_XMR[,4]
zi1_XMR_ss1_SGED <- ifelse(realisedval_XMR_ss1_SGED < VaR_XMR_ss1_SGED, 1, 0)
zi2_XMR_ss1_SGED <- ifelse(realisedval_XMR_ss1_SGED < VaR_XMR_ss1_SGED, 1, 0)*(1+((realisedval_XMR_ss1_SGED - VaR_XMR_ss1_SGED)^2))
zi3_XMR_ss1_SGED <- ifelse(realisedval_XMR_ss1_SGED < VaR_XMR_ss1_SGED, 1, 0)*((realisedval_XMR_ss1_SGED - VaR_XMR_ss1_SGED)/VaR_XMR_ss1_SGED)

QPS1_XMR_ss1_SGED <- (2/60)*sum(((zi1_XMR_ss1_SGED - 0.01)^2))
QPS2_XMR_ss1_SGED <- (2/60)*sum(((zi2_XMR_ss1_SGED - 0.01)^2))
QPS3_XMR_ss1_SGED <- (2/60)*sum(((zi3_XMR_ss1_SGED - 0.01)^2))

QPS_XMR_ss1_SGED <- QPS1_XMR_ss1_SGED + QPS2_XMR_ss1_SGED + QPS3_XMR_ss1_SGED
#JSU
VaR_XMR_ss1_JSU <- VR14_XMR[,1]
realisedval_XMR_ss1_JSU <- VR14_XMR[,4]
zi1_XMR_ss1_JSU <- ifelse(realisedval_XMR_ss1_JSU < VaR_XMR_ss1_JSU, 1, 0)
zi2_XMR_ss1_JSU <- ifelse(realisedval_XMR_ss1_JSU < VaR_XMR_ss1_JSU, 1, 0)*(1+((realisedval_XMR_ss1_JSU - VaR_XMR_ss1_JSU)^2))
zi3_XMR_ss1_JSU <- ifelse(realisedval_XMR_ss1_JSU < VaR_XMR_ss1_JSU, 1, 0)*((realisedval_XMR_ss1_JSU - VaR_XMR_ss1_JSU)/VaR_XMR_ss1_JSU)

QPS1_XMR_ss1_JSU <- (2/60)*sum(((zi1_XMR_ss1_JSU - 0.01)^2))
QPS2_XMR_ss1_JSU <- (2/60)*sum(((zi2_XMR_ss1_JSU - 0.01)^2))
QPS3_XMR_ss1_JSU <- (2/60)*sum(((zi3_XMR_ss1_JSU - 0.01)^2))

QPS_XMR_ss1_JSU <- QPS1_XMR_ss1_JSU + QPS2_XMR_ss1_JSU + QPS3_XMR_ss1_JSU
#PSIV
VaR_XMR_ss1_PSIV <- VR15_XMR
realisedval_XMR_ss1_PSIV <- VR14_XMR[,4]
zi1_XMR_ss1_PSIV <- ifelse(realisedval_XMR_ss1_PSIV < VaR_XMR_ss1_PSIV, 1, 0)
zi2_XMR_ss1_PSIV <- ifelse(realisedval_XMR_ss1_PSIV < VaR_XMR_ss1_PSIV, 1, 0)*(1+((realisedval_XMR_ss1_PSIV - VaR_XMR_ss1_PSIV)^2))
zi3_XMR_ss1_PSIV <- ifelse(realisedval_XMR_ss1_PSIV < VaR_XMR_ss1_PSIV, 1, 0)*((realisedval_XMR_ss1_PSIV - VaR_XMR_ss1_PSIV)/VaR_XMR_ss1_PSIV)

QPS1_XMR_ss1_PSIV <- (2/60)*sum(((zi1_XMR_ss1_PSIV - 0.01)^2))
QPS2_XMR_ss1_PSIV <- (2/60)*sum(((zi2_XMR_ss1_PSIV - 0.01)^2))
QPS3_XMR_ss1_PSIV <- (2/60)*sum(((zi3_XMR_ss1_PSIV - 0.01)^2))

QPS_XMR_ss1_PSIV <- QPS1_XMR_ss1_PSIV + QPS2_XMR_ss1_PSIV + QPS3_XMR_ss1_PSIV

# Sub Sample 2
#Normal
VaR_XMR_ss2_norm <- VR21_XMR[,1]
realisedval_XMR_ss2_norm <- VR21_XMR[,4]
zi1_XMR_ss2_norm <- ifelse(realisedval_XMR_ss2_norm < VaR_XMR_ss2_norm, 1, 0)
zi2_XMR_ss2_norm <- ifelse(realisedval_XMR_ss2_norm < VaR_XMR_ss2_norm, 1, 0)*(1+((realisedval_XMR_ss2_norm - VaR_XMR_ss2_norm)^2))
zi3_XMR_ss2_norm <- ifelse(realisedval_XMR_ss2_norm < VaR_XMR_ss2_norm, 1, 0)*((realisedval_XMR_ss2_norm - VaR_XMR_ss2_norm)/VaR_XMR_ss2_norm)

QPS1_XMR_ss2_norm <- (2/200)*sum(((zi1_XMR_ss2_norm - 0.01)^2))
QPS2_XMR_ss2_norm <- (2/200)*sum(((zi2_XMR_ss2_norm - 0.01)^2))
QPS3_XMR_ss2_norm <- (2/200)*sum(((zi3_XMR_ss2_norm - 0.01)^2))

QPS_XMR_ss2_norm <- QPS1_XMR_ss2_norm + QPS2_XMR_ss2_norm + QPS3_XMR_ss2_norm
#SST
VaR_XMR_ss2_SST <- VR22_XMR[,1]
realisedval_XMR_ss2_SST <- VR22_XMR[,4]
zi1_XMR_ss2_SST <- ifelse(realisedval_XMR_ss2_SST < VaR_XMR_ss2_SST, 1, 0)
zi2_XMR_ss2_SST <- ifelse(realisedval_XMR_ss2_SST < VaR_XMR_ss2_SST, 1, 0)*(1+((realisedval_XMR_ss2_SST - VaR_XMR_ss2_SST)^2))
zi3_XMR_ss2_SST <- ifelse(realisedval_XMR_ss2_SST < VaR_XMR_ss2_SST, 1, 0)*((realisedval_XMR_ss2_SST - VaR_XMR_ss2_SST)/VaR_XMR_ss2_SST)

QPS1_XMR_ss2_SST <- (2/200)*sum(((zi1_XMR_ss2_SST - 0.01)^2))
QPS2_XMR_ss2_SST <- (2/200)*sum(((zi2_XMR_ss2_SST - 0.01)^2))
QPS3_XMR_ss2_SST <- (2/200)*sum(((zi3_XMR_ss2_SST - 0.01)^2))

QPS_XMR_ss2_SST <- QPS1_XMR_ss2_SST + QPS2_XMR_ss2_SST + QPS3_XMR_ss2_SST
#SGED
VaR_XMR_ss2_SGED <- VR23_XMR[,1]
realisedval_XMR_ss2_SGED <- VR23_XMR[,4]
zi1_XMR_ss2_SGED <- ifelse(realisedval_XMR_ss2_SGED < VaR_XMR_ss2_SGED, 1, 0)
zi2_XMR_ss2_SGED <- ifelse(realisedval_XMR_ss2_SGED < VaR_XMR_ss2_SGED, 1, 0)*(1+((realisedval_XMR_ss2_SGED - VaR_XMR_ss2_SGED)^2))
zi3_XMR_ss2_SGED <- ifelse(realisedval_XMR_ss2_SGED < VaR_XMR_ss2_SGED, 1, 0)*((realisedval_XMR_ss2_SGED - VaR_XMR_ss2_SGED)/VaR_XMR_ss2_SGED)

QPS1_XMR_ss2_SGED <- (2/200)*sum(((zi1_XMR_ss2_SGED - 0.01)^2))
QPS2_XMR_ss2_SGED <- (2/200)*sum(((zi2_XMR_ss2_SGED - 0.01)^2))
QPS3_XMR_ss2_SGED <- (2/200)*sum(((zi3_XMR_ss2_SGED - 0.01)^2))

QPS_XMR_ss2_SGED <- QPS1_XMR_ss2_SGED + QPS2_XMR_ss2_SGED + QPS3_XMR_ss2_SGED
#JSU
VaR_XMR_ss2_JSU <- VR24_XMR[,1]
realisedval_XMR_ss2_JSU <- VR24_XMR[,4]
zi1_XMR_ss2_JSU <- ifelse(realisedval_XMR_ss2_JSU < VaR_XMR_ss2_JSU, 1, 0)
zi2_XMR_ss2_JSU <- ifelse(realisedval_XMR_ss2_JSU < VaR_XMR_ss2_JSU, 1, 0)*(1+((realisedval_XMR_ss2_JSU - VaR_XMR_ss2_JSU)^2))
zi3_XMR_ss2_JSU <- ifelse(realisedval_XMR_ss2_JSU < VaR_XMR_ss2_JSU, 1, 0)*((realisedval_XMR_ss2_JSU - VaR_XMR_ss2_JSU)/VaR_XMR_ss2_JSU)

QPS1_XMR_ss2_JSU <- (2/200)*sum(((zi1_XMR_ss2_JSU - 0.01)^2))
QPS2_XMR_ss2_JSU <- (2/200)*sum(((zi2_XMR_ss2_JSU - 0.01)^2))
QPS3_XMR_ss2_JSU <- (2/200)*sum(((zi3_XMR_ss2_JSU - 0.01)^2))

QPS_XMR_ss2_JSU <- QPS1_XMR_ss2_JSU + QPS2_XMR_ss2_JSU + QPS3_XMR_ss2_JSU
#PSIV
VaR_XMR_ss2_PSIV <- VR25_XMR
realisedval_XMR_ss2_PSIV <- VR24_XMR[,4]
zi1_XMR_ss2_PSIV <- ifelse(realisedval_XMR_ss2_PSIV < VaR_XMR_ss2_PSIV, 1, 0)
zi2_XMR_ss2_PSIV <- ifelse(realisedval_XMR_ss2_PSIV < VaR_XMR_ss2_PSIV, 1, 0)*(1+((realisedval_XMR_ss2_PSIV - VaR_XMR_ss2_PSIV)^2))
zi3_XMR_ss2_PSIV <- ifelse(realisedval_XMR_ss2_PSIV < VaR_XMR_ss2_PSIV, 1, 0)*((realisedval_XMR_ss2_PSIV - VaR_XMR_ss2_PSIV)/VaR_XMR_ss2_PSIV)

QPS1_XMR_ss2_PSIV <- (2/200)*sum(((zi1_XMR_ss2_PSIV - 0.01)^2))
QPS2_XMR_ss2_PSIV <- (2/200)*sum(((zi2_XMR_ss2_PSIV - 0.01)^2))
QPS3_XMR_ss2_PSIV <- (2/200)*sum(((zi3_XMR_ss2_PSIV - 0.01)^2))

QPS_XMR_ss2_PSIV <- QPS1_XMR_ss2_PSIV + QPS2_XMR_ss2_PSIV + QPS3_XMR_ss2_PSIV

# Sub Sample 3
#Normal
VaR_XMR_ss3_norm <- VR31_XMR[,1]
realisedval_XMR_ss3_norm <- VR31_XMR[,4]
zi1_XMR_ss3_norm <- ifelse(realisedval_XMR_ss3_norm < VaR_XMR_ss3_norm, 1, 0)
zi2_XMR_ss3_norm <- ifelse(realisedval_XMR_ss3_norm < VaR_XMR_ss3_norm, 1, 0)*(1+((realisedval_XMR_ss3_norm - VaR_XMR_ss3_norm)^2))
zi3_XMR_ss3_norm <- ifelse(realisedval_XMR_ss3_norm < VaR_XMR_ss3_norm, 1, 0)*((realisedval_XMR_ss3_norm - VaR_XMR_ss3_norm)/VaR_XMR_ss3_norm)

QPS1_XMR_ss3_norm <- (2/100)*sum(((zi1_XMR_ss3_norm - 0.01)^2))
QPS2_XMR_ss3_norm <- (2/100)*sum(((zi2_XMR_ss3_norm - 0.01)^2))
QPS3_XMR_ss3_norm <- (2/100)*sum(((zi3_XMR_ss3_norm - 0.01)^2))

QPS_XMR_ss3_norm <- QPS1_XMR_ss3_norm + QPS2_XMR_ss3_norm + QPS3_XMR_ss3_norm
#SST
VaR_XMR_ss3_SST <- VR32_XMR[,1]
realisedval_XMR_ss3_SST <- VR32_XMR[,4]
zi1_XMR_ss3_SST <- ifelse(realisedval_XMR_ss3_SST < VaR_XMR_ss3_SST, 1, 0)
zi2_XMR_ss3_SST <- ifelse(realisedval_XMR_ss3_SST < VaR_XMR_ss3_SST, 1, 0)*(1+((realisedval_XMR_ss3_SST - VaR_XMR_ss3_SST)^2))
zi3_XMR_ss3_SST <- ifelse(realisedval_XMR_ss3_SST < VaR_XMR_ss3_SST, 1, 0)*((realisedval_XMR_ss3_SST - VaR_XMR_ss3_SST)/VaR_XMR_ss3_SST)

QPS1_XMR_ss3_SST <- (2/100)*sum(((zi1_XMR_ss3_SST - 0.01)^2))
QPS2_XMR_ss3_SST <- (2/100)*sum(((zi2_XMR_ss3_SST - 0.01)^2))
QPS3_XMR_ss3_SST <- (2/100)*sum(((zi3_XMR_ss3_SST - 0.01)^2))

QPS_XMR_ss3_SST <- QPS1_XMR_ss3_SST + QPS2_XMR_ss3_SST + QPS3_XMR_ss3_SST
#SGED
VaR_XMR_ss3_SGED <- VR33_XMR[,1]
realisedval_XMR_ss3_SGED <- VR33_XMR[,4]
zi1_XMR_ss3_SGED <- ifelse(realisedval_XMR_ss3_SGED < VaR_XMR_ss3_SGED, 1, 0)
zi2_XMR_ss3_SGED <- ifelse(realisedval_XMR_ss3_SGED < VaR_XMR_ss3_SGED, 1, 0)*(1+((realisedval_XMR_ss3_SGED - VaR_XMR_ss3_SGED)^2))
zi3_XMR_ss3_SGED <- ifelse(realisedval_XMR_ss3_SGED < VaR_XMR_ss3_SGED, 1, 0)*((realisedval_XMR_ss3_SGED - VaR_XMR_ss3_SGED)/VaR_XMR_ss3_SGED)

QPS1_XMR_ss3_SGED <- (2/100)*sum(((zi1_XMR_ss3_SGED - 0.01)^2))
QPS2_XMR_ss3_SGED <- (2/100)*sum(((zi2_XMR_ss3_SGED - 0.01)^2))
QPS3_XMR_ss3_SGED <- (2/100)*sum(((zi3_XMR_ss3_SGED - 0.01)^2))

QPS_XMR_ss3_SGED <- QPS1_XMR_ss3_SGED + QPS2_XMR_ss3_SGED + QPS3_XMR_ss3_SGED
#JSU
VaR_XMR_ss3_JSU <- VR34_XMR[,1]
realisedval_XMR_ss3_JSU <- VR34_XMR[,4]
zi1_XMR_ss3_JSU <- ifelse(realisedval_XMR_ss3_JSU < VaR_XMR_ss3_JSU, 1, 0)
zi2_XMR_ss3_JSU <- ifelse(realisedval_XMR_ss3_JSU < VaR_XMR_ss3_JSU, 1, 0)*(1+((realisedval_XMR_ss3_JSU - VaR_XMR_ss3_JSU)^2))
zi3_XMR_ss3_JSU <- ifelse(realisedval_XMR_ss3_JSU < VaR_XMR_ss3_JSU, 1, 0)*((realisedval_XMR_ss3_JSU - VaR_XMR_ss3_JSU)/VaR_XMR_ss3_JSU)

QPS1_XMR_ss3_JSU <- (2/100)*sum(((zi1_XMR_ss3_JSU - 0.01)^2))
QPS2_XMR_ss3_JSU <- (2/100)*sum(((zi2_XMR_ss3_JSU - 0.01)^2))
QPS3_XMR_ss3_JSU <- (2/100)*sum(((zi3_XMR_ss3_JSU - 0.01)^2))

QPS_XMR_ss3_JSU <- QPS1_XMR_ss3_JSU + QPS2_XMR_ss3_JSU + QPS3_XMR_ss3_JSU
#PSIV
VaR_XMR_ss3_PSIV <- VR35_XMR
realisedval_XMR_ss3_PSIV <- VR34_XMR[,4]
zi1_XMR_ss3_PSIV <- ifelse(realisedval_XMR_ss3_PSIV < VaR_XMR_ss3_PSIV, 1, 0)
zi2_XMR_ss3_PSIV <- ifelse(realisedval_XMR_ss3_PSIV < VaR_XMR_ss3_PSIV, 1, 0)*(1+((realisedval_XMR_ss3_PSIV - VaR_XMR_ss3_PSIV)^2))
zi3_XMR_ss3_PSIV <- ifelse(realisedval_XMR_ss3_PSIV < VaR_XMR_ss3_PSIV, 1, 0)*((realisedval_XMR_ss3_PSIV - VaR_XMR_ss3_PSIV)/VaR_XMR_ss3_PSIV)

QPS1_XMR_ss3_PSIV <- (2/100)*sum(((zi1_XMR_ss3_PSIV - 0.01)^2))
QPS2_XMR_ss3_PSIV <- (2/100)*sum(((zi2_XMR_ss3_PSIV - 0.01)^2))
QPS3_XMR_ss3_PSIV <- (2/100)*sum(((zi3_XMR_ss3_PSIV - 0.01)^2))

QPS_XMR_ss3_PSIV <- QPS1_XMR_ss3_PSIV + QPS2_XMR_ss3_PSIV + QPS3_XMR_ss3_PSIV

QPS_XMR_fs <- c(QPS_XMR_fs_norm, QPS_XMR_fs_SST, QPS_XMR_fs_SGED, QPS_XMR_fs_JSU, QPS_XMR_fs_PSIV)
QPS_XMR_ss1 <- c(QPS_XMR_ss1_norm, QPS_XMR_ss1_SST, QPS_XMR_ss1_SGED, QPS_XMR_ss1_JSU, QPS_XMR_ss1_PSIV)
QPS_XMR_ss2 <- c(QPS_XMR_ss2_norm, QPS_XMR_ss2_SST, QPS_XMR_ss2_SGED, QPS_XMR_ss2_JSU, QPS_XMR_ss2_PSIV)
QPS_XMR_ss3 <- c(QPS_XMR_ss3_norm, QPS_XMR_ss3_SST, QPS_XMR_ss3_SGED, QPS_XMR_ss3_JSU, QPS_XMR_ss3_PSIV)

QPS_XMR <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_XMR <- rbind(QPS_XMR_fs, QPS_XMR_ss1, QPS_XMR_ss2, QPS_XMR_ss3)
colnames(QPS_XMR) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(QPS_XMR) <- c("Full Sample", "SS1", "SS2", "SS3")


##### :::::::: E T C :::::::: #####
# Full Sample
#Normal
VaR_ETC_fs_norm <- VR1_ETC[,1]
realisedval_ETC_fs_norm <- VR1_ETC[,4]
zi1_ETC_fs_norm <- ifelse(realisedval_ETC_fs_norm < VaR_ETC_fs_norm, 1, 0)
zi2_ETC_fs_norm <- ifelse(realisedval_ETC_fs_norm < VaR_ETC_fs_norm, 1, 0)*(1+((realisedval_ETC_fs_norm - VaR_ETC_fs_norm)^2))
zi3_ETC_fs_norm <- ifelse(realisedval_ETC_fs_norm < VaR_ETC_fs_norm, 1, 0)*((realisedval_ETC_fs_norm - VaR_ETC_fs_norm)/VaR_ETC_fs_norm)

QPS1_ETC_fs_norm <- (2/350)*sum(((zi1_ETC_fs_norm - 0.01)^2))
QPS2_ETC_fs_norm <- (2/350)*sum(((zi2_ETC_fs_norm - 0.01)^2))
QPS3_ETC_fs_norm <- (2/350)*sum(((zi3_ETC_fs_norm - 0.01)^2))

QPS_ETC_fs_norm <- QPS1_ETC_fs_norm + QPS2_ETC_fs_norm + QPS3_ETC_fs_norm 
#SST
VaR_ETC_fs_SST <- VR2_ETC[,1]
realisedval_ETC_fs_SST <- VR2_ETC[,4]
zi1_ETC_fs_SST <- ifelse(realisedval_ETC_fs_SST < VaR_ETC_fs_SST, 1, 0)
zi2_ETC_fs_SST <- ifelse(realisedval_ETC_fs_SST < VaR_ETC_fs_SST, 1, 0)*(1+((realisedval_ETC_fs_SST - VaR_ETC_fs_SST)^2))
zi3_ETC_fs_SST <- ifelse(realisedval_ETC_fs_SST < VaR_ETC_fs_SST, 1, 0)*((realisedval_ETC_fs_SST - VaR_ETC_fs_SST)/VaR_ETC_fs_SST)

QPS1_ETC_fs_SST <- (2/350)*sum(((zi1_ETC_fs_SST - 0.01)^2))
QPS2_ETC_fs_SST <- (2/350)*sum(((zi2_ETC_fs_SST - 0.01)^2))
QPS3_ETC_fs_SST <- (2/350)*sum(((zi3_ETC_fs_SST - 0.01)^2))

QPS_ETC_fs_SST <- QPS1_ETC_fs_SST + QPS2_ETC_fs_SST + QPS3_ETC_fs_SST
#SGED
VaR_ETC_fs_SGED <- VR3_ETC[,1]
realisedval_ETC_fs_SGED <- VR3_ETC[,4]
zi1_ETC_fs_SGED <- ifelse(realisedval_ETC_fs_SGED < VaR_ETC_fs_SGED, 1, 0)
zi2_ETC_fs_SGED <- ifelse(realisedval_ETC_fs_SGED < VaR_ETC_fs_SGED, 1, 0)*(1+((realisedval_ETC_fs_SGED - VaR_ETC_fs_SGED)^2))
zi3_ETC_fs_SGED <- ifelse(realisedval_ETC_fs_SGED < VaR_ETC_fs_SGED, 1, 0)*((realisedval_ETC_fs_SGED - VaR_ETC_fs_SGED)/VaR_ETC_fs_SGED)

QPS1_ETC_fs_SGED <- (2/350)*sum(((zi1_ETC_fs_SGED - 0.01)^2))
QPS2_ETC_fs_SGED <- (2/350)*sum(((zi2_ETC_fs_SGED - 0.01)^2))
QPS3_ETC_fs_SGED <- (2/350)*sum(((zi3_ETC_fs_SGED - 0.01)^2))

QPS_ETC_fs_SGED <- QPS1_ETC_fs_SGED + QPS2_ETC_fs_SGED + QPS3_ETC_fs_SGED
#JSU
VaR_ETC_fs_JSU <- VR4_ETC[,1]
realisedval_ETC_fs_JSU <- VR4_ETC[,4]
zi1_ETC_fs_JSU <- ifelse(realisedval_ETC_fs_JSU < VaR_ETC_fs_JSU, 1, 0)
zi2_ETC_fs_JSU <- ifelse(realisedval_ETC_fs_JSU < VaR_ETC_fs_JSU, 1, 0)*(1+((realisedval_ETC_fs_JSU - VaR_ETC_fs_JSU)^2))
zi3_ETC_fs_JSU <- ifelse(realisedval_ETC_fs_JSU < VaR_ETC_fs_JSU, 1, 0)*((realisedval_ETC_fs_JSU - VaR_ETC_fs_JSU)/VaR_ETC_fs_JSU)

QPS1_ETC_fs_JSU <- (2/350)*sum(((zi1_ETC_fs_JSU - 0.01)^2))
QPS2_ETC_fs_JSU <- (2/350)*sum(((zi2_ETC_fs_JSU - 0.01)^2))
QPS3_ETC_fs_JSU <- (2/350)*sum(((zi3_ETC_fs_JSU - 0.01)^2))

QPS_ETC_fs_JSU <- QPS1_ETC_fs_JSU + QPS2_ETC_fs_JSU + QPS3_ETC_fs_JSU
#PSIV
VaR_ETC_fs_PSIV <- VR5_ETC
realisedval_ETC_fs_PSIV <- VR4_ETC[,4]
zi1_ETC_fs_PSIV <- ifelse(realisedval_ETC_fs_PSIV < VaR_ETC_fs_PSIV, 1, 0)
zi2_ETC_fs_PSIV <- ifelse(realisedval_ETC_fs_PSIV < VaR_ETC_fs_PSIV, 1, 0)*(1+((realisedval_ETC_fs_PSIV - VaR_ETC_fs_PSIV)^2))
zi3_ETC_fs_PSIV <- ifelse(realisedval_ETC_fs_PSIV < VaR_ETC_fs_PSIV, 1, 0)*((realisedval_ETC_fs_PSIV - VaR_ETC_fs_PSIV)/VaR_ETC_fs_PSIV)

QPS1_ETC_fs_PSIV <- (2/350)*sum(((zi1_ETC_fs_PSIV - 0.01)^2))
QPS2_ETC_fs_PSIV <- (2/350)*sum(((zi2_ETC_fs_PSIV - 0.01)^2))
QPS3_ETC_fs_PSIV <- (2/350)*sum(((zi3_ETC_fs_PSIV - 0.01)^2))

QPS_ETC_fs_PSIV <- QPS1_ETC_fs_PSIV + QPS2_ETC_fs_PSIV + QPS3_ETC_fs_PSIV

# Sub Sample 1
#Normal
VaR_ETC_ss1_norm <- VR11_ETC[,1]
realisedval_ETC_ss1_norm <- VR11_ETC[,4]
zi1_ETC_ss1_norm <- ifelse(realisedval_ETC_ss1_norm < VaR_ETC_ss1_norm, 1, 0)
zi2_ETC_ss1_norm <- ifelse(realisedval_ETC_ss1_norm < VaR_ETC_ss1_norm, 1, 0)*(1+((realisedval_ETC_ss1_norm - VaR_ETC_ss1_norm)^2))
zi3_ETC_ss1_norm <- ifelse(realisedval_ETC_ss1_norm < VaR_ETC_ss1_norm, 1, 0)*((realisedval_ETC_ss1_norm - VaR_ETC_ss1_norm)/VaR_ETC_ss1_norm)

QPS1_ETC_ss1_norm <- (2/60)*sum(((zi1_ETC_ss1_norm - 0.01)^2))
QPS2_ETC_ss1_norm <- (2/60)*sum(((zi2_ETC_ss1_norm - 0.01)^2))
QPS3_ETC_ss1_norm <- (2/60)*sum(((zi3_ETC_ss1_norm - 0.01)^2))

QPS_ETC_ss1_norm <- QPS1_ETC_ss1_norm + QPS2_ETC_ss1_norm + QPS3_ETC_ss1_norm
#SST
VaR_ETC_ss1_SST <- VR12_ETC[,1]
realisedval_ETC_ss1_SST <- VR12_ETC[,4]
zi1_ETC_ss1_SST <- ifelse(realisedval_ETC_ss1_SST < VaR_ETC_ss1_SST, 1, 0)
zi2_ETC_ss1_SST <- ifelse(realisedval_ETC_ss1_SST < VaR_ETC_ss1_SST, 1, 0)*(1+((realisedval_ETC_ss1_SST - VaR_ETC_ss1_SST)^2))
zi3_ETC_ss1_SST <- ifelse(realisedval_ETC_ss1_SST < VaR_ETC_ss1_SST, 1, 0)*((realisedval_ETC_ss1_SST - VaR_ETC_ss1_SST)/VaR_ETC_ss1_SST)

QPS1_ETC_ss1_SST <- (2/60)*sum(((zi1_ETC_ss1_SST - 0.01)^2))
QPS2_ETC_ss1_SST <- (2/60)*sum(((zi2_ETC_ss1_SST - 0.01)^2))
QPS3_ETC_ss1_SST <- (2/60)*sum(((zi3_ETC_ss1_SST - 0.01)^2))

QPS_ETC_ss1_SST <- QPS1_ETC_ss1_SST + QPS2_ETC_ss1_SST + QPS3_ETC_ss1_SST
#SGED
VaR_ETC_ss1_SGED <- VR13_ETC[,1]
realisedval_ETC_ss1_SGED <- VR13_ETC[,4]
zi1_ETC_ss1_SGED <- ifelse(realisedval_ETC_ss1_SGED < VaR_ETC_ss1_SGED, 1, 0)
zi2_ETC_ss1_SGED <- ifelse(realisedval_ETC_ss1_SGED < VaR_ETC_ss1_SGED, 1, 0)*(1+((realisedval_ETC_ss1_SGED - VaR_ETC_ss1_SGED)^2))
zi3_ETC_ss1_SGED <- ifelse(realisedval_ETC_ss1_SGED < VaR_ETC_ss1_SGED, 1, 0)*((realisedval_ETC_ss1_SGED - VaR_ETC_ss1_SGED)/VaR_ETC_ss1_SGED)

QPS1_ETC_ss1_SGED <- (2/60)*sum(((zi1_ETC_ss1_SGED - 0.01)^2))
QPS2_ETC_ss1_SGED <- (2/60)*sum(((zi2_ETC_ss1_SGED - 0.01)^2))
QPS3_ETC_ss1_SGED <- (2/60)*sum(((zi3_ETC_ss1_SGED - 0.01)^2))

QPS_ETC_ss1_SGED <- QPS1_ETC_ss1_SGED + QPS2_ETC_ss1_SGED + QPS3_ETC_ss1_SGED
#JSU
VaR_ETC_ss1_JSU <- VR14_ETC[,1]
realisedval_ETC_ss1_JSU <- VR14_ETC[,4]
zi1_ETC_ss1_JSU <- ifelse(realisedval_ETC_ss1_JSU < VaR_ETC_ss1_JSU, 1, 0)
zi2_ETC_ss1_JSU <- ifelse(realisedval_ETC_ss1_JSU < VaR_ETC_ss1_JSU, 1, 0)*(1+((realisedval_ETC_ss1_JSU - VaR_ETC_ss1_JSU)^2))
zi3_ETC_ss1_JSU <- ifelse(realisedval_ETC_ss1_JSU < VaR_ETC_ss1_JSU, 1, 0)*((realisedval_ETC_ss1_JSU - VaR_ETC_ss1_JSU)/VaR_ETC_ss1_JSU)

QPS1_ETC_ss1_JSU <- (2/60)*sum(((zi1_ETC_ss1_JSU - 0.01)^2))
QPS2_ETC_ss1_JSU <- (2/60)*sum(((zi2_ETC_ss1_JSU - 0.01)^2))
QPS3_ETC_ss1_JSU <- (2/60)*sum(((zi3_ETC_ss1_JSU - 0.01)^2))

QPS_ETC_ss1_JSU <- QPS1_ETC_ss1_JSU + QPS2_ETC_ss1_JSU + QPS3_ETC_ss1_JSU
#PSIV
VaR_ETC_ss1_PSIV <- VR15_ETC
realisedval_ETC_ss1_PSIV <- VR14_ETC[,4]
zi1_ETC_ss1_PSIV <- ifelse(realisedval_ETC_ss1_PSIV < VaR_ETC_ss1_PSIV, 1, 0)
zi2_ETC_ss1_PSIV <- ifelse(realisedval_ETC_ss1_PSIV < VaR_ETC_ss1_PSIV, 1, 0)*(1+((realisedval_ETC_ss1_PSIV - VaR_ETC_ss1_PSIV)^2))
zi3_ETC_ss1_PSIV <- ifelse(realisedval_ETC_ss1_PSIV < VaR_ETC_ss1_PSIV, 1, 0)*((realisedval_ETC_ss1_PSIV - VaR_ETC_ss1_PSIV)/VaR_ETC_ss1_PSIV)

QPS1_ETC_ss1_PSIV <- (2/60)*sum(((zi1_ETC_ss1_PSIV - 0.01)^2))
QPS2_ETC_ss1_PSIV <- (2/60)*sum(((zi2_ETC_ss1_PSIV - 0.01)^2))
QPS3_ETC_ss1_PSIV <- (2/60)*sum(((zi3_ETC_ss1_PSIV - 0.01)^2))

QPS_ETC_ss1_PSIV <- QPS1_ETC_ss1_PSIV + QPS2_ETC_ss1_PSIV + QPS3_ETC_ss1_PSIV

# Sub Sample 2
#Normal
VaR_ETC_ss2_norm <- VR21_ETC[,1]
realisedval_ETC_ss2_norm <- VR21_ETC[,4]
zi1_ETC_ss2_norm <- ifelse(realisedval_ETC_ss2_norm < VaR_ETC_ss2_norm, 1, 0)
zi2_ETC_ss2_norm <- ifelse(realisedval_ETC_ss2_norm < VaR_ETC_ss2_norm, 1, 0)*(1+((realisedval_ETC_ss2_norm - VaR_ETC_ss2_norm)^2))
zi3_ETC_ss2_norm <- ifelse(realisedval_ETC_ss2_norm < VaR_ETC_ss2_norm, 1, 0)*((realisedval_ETC_ss2_norm - VaR_ETC_ss2_norm)/VaR_ETC_ss2_norm)

QPS1_ETC_ss2_norm <- (2/200)*sum(((zi1_ETC_ss2_norm - 0.01)^2))
QPS2_ETC_ss2_norm <- (2/200)*sum(((zi2_ETC_ss2_norm - 0.01)^2))
QPS3_ETC_ss2_norm <- (2/200)*sum(((zi3_ETC_ss2_norm - 0.01)^2))

QPS_ETC_ss2_norm <- QPS1_ETC_ss2_norm + QPS2_ETC_ss2_norm + QPS3_ETC_ss2_norm
#SST
VaR_ETC_ss2_SST <- VR22_ETC[,1]
realisedval_ETC_ss2_SST <- VR22_ETC[,4]
zi1_ETC_ss2_SST <- ifelse(realisedval_ETC_ss2_SST < VaR_ETC_ss2_SST, 1, 0)
zi2_ETC_ss2_SST <- ifelse(realisedval_ETC_ss2_SST < VaR_ETC_ss2_SST, 1, 0)*(1+((realisedval_ETC_ss2_SST - VaR_ETC_ss2_SST)^2))
zi3_ETC_ss2_SST <- ifelse(realisedval_ETC_ss2_SST < VaR_ETC_ss2_SST, 1, 0)*((realisedval_ETC_ss2_SST - VaR_ETC_ss2_SST)/VaR_ETC_ss2_SST)

QPS1_ETC_ss2_SST <- (2/200)*sum(((zi1_ETC_ss2_SST - 0.01)^2))
QPS2_ETC_ss2_SST <- (2/200)*sum(((zi2_ETC_ss2_SST - 0.01)^2))
QPS3_ETC_ss2_SST <- (2/200)*sum(((zi3_ETC_ss2_SST - 0.01)^2))

QPS_ETC_ss2_SST <- QPS1_ETC_ss2_SST + QPS2_ETC_ss2_SST + QPS3_ETC_ss2_SST
#SGED
VaR_ETC_ss2_SGED <- VR23_ETC[,1]
realisedval_ETC_ss2_SGED <- VR23_ETC[,4]
zi1_ETC_ss2_SGED <- ifelse(realisedval_ETC_ss2_SGED < VaR_ETC_ss2_SGED, 1, 0)
zi2_ETC_ss2_SGED <- ifelse(realisedval_ETC_ss2_SGED < VaR_ETC_ss2_SGED, 1, 0)*(1+((realisedval_ETC_ss2_SGED - VaR_ETC_ss2_SGED)^2))
zi3_ETC_ss2_SGED <- ifelse(realisedval_ETC_ss2_SGED < VaR_ETC_ss2_SGED, 1, 0)*((realisedval_ETC_ss2_SGED - VaR_ETC_ss2_SGED)/VaR_ETC_ss2_SGED)

QPS1_ETC_ss2_SGED <- (2/200)*sum(((zi1_ETC_ss2_SGED - 0.01)^2))
QPS2_ETC_ss2_SGED <- (2/200)*sum(((zi2_ETC_ss2_SGED - 0.01)^2))
QPS3_ETC_ss2_SGED <- (2/200)*sum(((zi3_ETC_ss2_SGED - 0.01)^2))

QPS_ETC_ss2_SGED <- QPS1_ETC_ss2_SGED + QPS2_ETC_ss2_SGED + QPS3_ETC_ss2_SGED
#JSU
VaR_ETC_ss2_JSU <- VR24_ETC[,1]
realisedval_ETC_ss2_JSU <- VR24_ETC[,4]
zi1_ETC_ss2_JSU <- ifelse(realisedval_ETC_ss2_JSU < VaR_ETC_ss2_JSU, 1, 0)
zi2_ETC_ss2_JSU <- ifelse(realisedval_ETC_ss2_JSU < VaR_ETC_ss2_JSU, 1, 0)*(1+((realisedval_ETC_ss2_JSU - VaR_ETC_ss2_JSU)^2))
zi3_ETC_ss2_JSU <- ifelse(realisedval_ETC_ss2_JSU < VaR_ETC_ss2_JSU, 1, 0)*((realisedval_ETC_ss2_JSU - VaR_ETC_ss2_JSU)/VaR_ETC_ss2_JSU)

QPS1_ETC_ss2_JSU <- (2/200)*sum(((zi1_ETC_ss2_JSU - 0.01)^2))
QPS2_ETC_ss2_JSU <- (2/200)*sum(((zi2_ETC_ss2_JSU - 0.01)^2))
QPS3_ETC_ss2_JSU <- (2/200)*sum(((zi3_ETC_ss2_JSU - 0.01)^2))

QPS_ETC_ss2_JSU <- QPS1_ETC_ss2_JSU + QPS2_ETC_ss2_JSU + QPS3_ETC_ss2_JSU
#PSIV
VaR_ETC_ss2_PSIV <- VR25_ETC
realisedval_ETC_ss2_PSIV <- VR24_ETC[,4]
zi1_ETC_ss2_PSIV <- ifelse(realisedval_ETC_ss2_PSIV < VaR_ETC_ss2_PSIV, 1, 0)
zi2_ETC_ss2_PSIV <- ifelse(realisedval_ETC_ss2_PSIV < VaR_ETC_ss2_PSIV, 1, 0)*(1+((realisedval_ETC_ss2_PSIV - VaR_ETC_ss2_PSIV)^2))
zi3_ETC_ss2_PSIV <- ifelse(realisedval_ETC_ss2_PSIV < VaR_ETC_ss2_PSIV, 1, 0)*((realisedval_ETC_ss2_PSIV - VaR_ETC_ss2_PSIV)/VaR_ETC_ss2_PSIV)

QPS1_ETC_ss2_PSIV <- (2/200)*sum(((zi1_ETC_ss2_PSIV - 0.01)^2))
QPS2_ETC_ss2_PSIV <- (2/200)*sum(((zi2_ETC_ss2_PSIV - 0.01)^2))
QPS3_ETC_ss2_PSIV <- (2/200)*sum(((zi3_ETC_ss2_PSIV - 0.01)^2))

QPS_ETC_ss2_PSIV <- QPS1_ETC_ss2_PSIV + QPS2_ETC_ss2_PSIV + QPS3_ETC_ss2_PSIV

# Sub Sample 3
#Normal
VaR_ETC_ss3_norm <- VR31_ETC[,1]
realisedval_ETC_ss3_norm <- VR31_ETC[,4]
zi1_ETC_ss3_norm <- ifelse(realisedval_ETC_ss3_norm < VaR_ETC_ss3_norm, 1, 0)
zi2_ETC_ss3_norm <- ifelse(realisedval_ETC_ss3_norm < VaR_ETC_ss3_norm, 1, 0)*(1+((realisedval_ETC_ss3_norm - VaR_ETC_ss3_norm)^2))
zi3_ETC_ss3_norm <- ifelse(realisedval_ETC_ss3_norm < VaR_ETC_ss3_norm, 1, 0)*((realisedval_ETC_ss3_norm - VaR_ETC_ss3_norm)/VaR_ETC_ss3_norm)

QPS1_ETC_ss3_norm <- (2/100)*sum(((zi1_ETC_ss3_norm - 0.01)^2))
QPS2_ETC_ss3_norm <- (2/100)*sum(((zi2_ETC_ss3_norm - 0.01)^2))
QPS3_ETC_ss3_norm <- (2/100)*sum(((zi3_ETC_ss3_norm - 0.01)^2))

QPS_ETC_ss3_norm <- QPS1_ETC_ss3_norm + QPS2_ETC_ss3_norm + QPS3_ETC_ss3_norm
#SST
VaR_ETC_ss3_SST <- VR32_ETC[,1]
realisedval_ETC_ss3_SST <- VR32_ETC[,4]
zi1_ETC_ss3_SST <- ifelse(realisedval_ETC_ss3_SST < VaR_ETC_ss3_SST, 1, 0)
zi2_ETC_ss3_SST <- ifelse(realisedval_ETC_ss3_SST < VaR_ETC_ss3_SST, 1, 0)*(1+((realisedval_ETC_ss3_SST - VaR_ETC_ss3_SST)^2))
zi3_ETC_ss3_SST <- ifelse(realisedval_ETC_ss3_SST < VaR_ETC_ss3_SST, 1, 0)*((realisedval_ETC_ss3_SST - VaR_ETC_ss3_SST)/VaR_ETC_ss3_SST)

QPS1_ETC_ss3_SST <- (2/100)*sum(((zi1_ETC_ss3_SST - 0.01)^2))
QPS2_ETC_ss3_SST <- (2/100)*sum(((zi2_ETC_ss3_SST - 0.01)^2))
QPS3_ETC_ss3_SST <- (2/100)*sum(((zi3_ETC_ss3_SST - 0.01)^2))

QPS_ETC_ss3_SST <- QPS1_ETC_ss3_SST + QPS2_ETC_ss3_SST + QPS3_ETC_ss3_SST
#SGED
VaR_ETC_ss3_SGED <- VR33_ETC[,1]
realisedval_ETC_ss3_SGED <- VR33_ETC[,4]
zi1_ETC_ss3_SGED <- ifelse(realisedval_ETC_ss3_SGED < VaR_ETC_ss3_SGED, 1, 0)
zi2_ETC_ss3_SGED <- ifelse(realisedval_ETC_ss3_SGED < VaR_ETC_ss3_SGED, 1, 0)*(1+((realisedval_ETC_ss3_SGED - VaR_ETC_ss3_SGED)^2))
zi3_ETC_ss3_SGED <- ifelse(realisedval_ETC_ss3_SGED < VaR_ETC_ss3_SGED, 1, 0)*((realisedval_ETC_ss3_SGED - VaR_ETC_ss3_SGED)/VaR_ETC_ss3_SGED)

QPS1_ETC_ss3_SGED <- (2/100)*sum(((zi1_ETC_ss3_SGED - 0.01)^2))
QPS2_ETC_ss3_SGED <- (2/100)*sum(((zi2_ETC_ss3_SGED - 0.01)^2))
QPS3_ETC_ss3_SGED <- (2/100)*sum(((zi3_ETC_ss3_SGED - 0.01)^2))

QPS_ETC_ss3_SGED <- QPS1_ETC_ss3_SGED + QPS2_ETC_ss3_SGED + QPS3_ETC_ss3_SGED
#JSU
VaR_ETC_ss3_JSU <- VR34_ETC[,1]
realisedval_ETC_ss3_JSU <- VR34_ETC[,4]
zi1_ETC_ss3_JSU <- ifelse(realisedval_ETC_ss3_JSU < VaR_ETC_ss3_JSU, 1, 0)
zi2_ETC_ss3_JSU <- ifelse(realisedval_ETC_ss3_JSU < VaR_ETC_ss3_JSU, 1, 0)*(1+((realisedval_ETC_ss3_JSU - VaR_ETC_ss3_JSU)^2))
zi3_ETC_ss3_JSU <- ifelse(realisedval_ETC_ss3_JSU < VaR_ETC_ss3_JSU, 1, 0)*((realisedval_ETC_ss3_JSU - VaR_ETC_ss3_JSU)/VaR_ETC_ss3_JSU)

QPS1_ETC_ss3_JSU <- (2/100)*sum(((zi1_ETC_ss3_JSU - 0.01)^2))
QPS2_ETC_ss3_JSU <- (2/100)*sum(((zi2_ETC_ss3_JSU - 0.01)^2))
QPS3_ETC_ss3_JSU <- (2/100)*sum(((zi3_ETC_ss3_JSU - 0.01)^2))

QPS_ETC_ss3_JSU <- QPS1_ETC_ss3_JSU + QPS2_ETC_ss3_JSU + QPS3_ETC_ss3_JSU
#PSIV
VaR_ETC_ss3_PSIV <- VR35_ETC
realisedval_ETC_ss3_PSIV <- VR34_ETC[,4]
zi1_ETC_ss3_PSIV <- ifelse(realisedval_ETC_ss3_PSIV < VaR_ETC_ss3_PSIV, 1, 0)
zi2_ETC_ss3_PSIV <- ifelse(realisedval_ETC_ss3_PSIV < VaR_ETC_ss3_PSIV, 1, 0)*(1+((realisedval_ETC_ss3_PSIV - VaR_ETC_ss3_PSIV)^2))
zi3_ETC_ss3_PSIV <- ifelse(realisedval_ETC_ss3_PSIV < VaR_ETC_ss3_PSIV, 1, 0)*((realisedval_ETC_ss3_PSIV - VaR_ETC_ss3_PSIV)/VaR_ETC_ss3_PSIV)

QPS1_ETC_ss3_PSIV <- (2/100)*sum(((zi1_ETC_ss3_PSIV - 0.01)^2))
QPS2_ETC_ss3_PSIV <- (2/100)*sum(((zi2_ETC_ss3_PSIV - 0.01)^2))
QPS3_ETC_ss3_PSIV <- (2/100)*sum(((zi3_ETC_ss3_PSIV - 0.01)^2))

QPS_ETC_ss3_PSIV <- QPS1_ETC_ss3_PSIV + QPS2_ETC_ss3_PSIV + QPS3_ETC_ss3_PSIV

QPS_ETC_fs <- c(QPS_ETC_fs_norm, QPS_ETC_fs_SST, QPS_ETC_fs_SGED, QPS_ETC_fs_JSU, QPS_ETC_fs_PSIV)
QPS_ETC_ss1 <- c(QPS_ETC_ss1_norm, QPS_ETC_ss1_SST, QPS_ETC_ss1_SGED, QPS_ETC_ss1_JSU, QPS_ETC_ss1_PSIV)
QPS_ETC_ss2 <- c(QPS_ETC_ss2_norm, QPS_ETC_ss2_SST, QPS_ETC_ss2_SGED, QPS_ETC_ss2_JSU, QPS_ETC_ss2_PSIV)
QPS_ETC_ss3 <- c(QPS_ETC_ss3_norm, QPS_ETC_ss3_SST, QPS_ETC_ss3_SGED, QPS_ETC_ss3_JSU, QPS_ETC_ss3_PSIV)

QPS_ETC <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_ETC <- rbind(QPS_ETC_fs, QPS_ETC_ss1, QPS_ETC_ss2, QPS_ETC_ss3)
colnames(QPS_ETC) <- c("Norm", "SST", "SGED", "JSU", "PSIV")
rownames(QPS_ETC) <- c("Full Sample", "SS1", "SS2", "SS3")

QPS_GARCH <- rbind(QPS_BTC, QPS_ETH, QPS_LTC, QPS_XMR, QPS_ETC)
View(QPS_GARCH)

