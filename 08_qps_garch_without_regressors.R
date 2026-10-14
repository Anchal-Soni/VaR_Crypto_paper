# =============================================================================
# 08_qps_garch_without_regressors.R
#
# Purpose : QPS of 1% VaR for full-sample EGARCH models without break dummies.
# Inputs  : objects from 05
# Outputs : QPS_<coin>_fs_wo, QPS_GARCH_wo
# Paper   : comparison row in 13
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

# Without regressors - QPS
##### :::::::: B T C :::::::: #####
# Full Sample

# Normal
VaR_BTC_fs_norm_wo <- VR1_BTC_wo[,1]
realisedval_BTC_fs_norm_wo <- VR1_BTC_wo[,4]
zi1_BTC_fs_norm_wo <- ifelse(realisedval_BTC_fs_norm_wo < VaR_BTC_fs_norm_wo, 1, 0)
zi2_BTC_fs_norm_wo <- ifelse(realisedval_BTC_fs_norm_wo < VaR_BTC_fs_norm_wo, 1, 0)*(1+((realisedval_BTC_fs_norm_wo - VaR_BTC_fs_norm_wo)^2))
zi3_BTC_fs_norm_wo <- ifelse(realisedval_BTC_fs_norm_wo < VaR_BTC_fs_norm_wo, 1, 0)*((realisedval_BTC_fs_norm_wo - VaR_BTC_fs_norm_wo)/VaR_BTC_fs_norm_wo)

QPS1_BTC_fs_norm_wo <- (2/350)*sum(((zi1_BTC_fs_norm_wo - 0.01)^2))
QPS2_BTC_fs_norm_wo <- (2/350)*sum(((zi2_BTC_fs_norm_wo - 0.01)^2))
QPS3_BTC_fs_norm_wo <- (2/350)*sum(((zi3_BTC_fs_norm_wo - 0.01)^2))

QPS_BTC_fs_norm_wo <- QPS1_BTC_fs_norm_wo + QPS2_BTC_fs_norm_wo + QPS3_BTC_fs_norm_wo 
#SST
VaR_BTC_fs_SST_wo <- VR2_BTC_wo[,1]
realisedval_BTC_fs_SST_wo <- VR2_BTC_wo[,4]
zi1_BTC_fs_SST_wo <- ifelse(realisedval_BTC_fs_SST_wo < VaR_BTC_fs_SST_wo, 1, 0)
zi2_BTC_fs_SST_wo <- ifelse(realisedval_BTC_fs_SST_wo < VaR_BTC_fs_SST_wo, 1, 0)*(1+((realisedval_BTC_fs_SST_wo - VaR_BTC_fs_SST_wo)^2))
zi3_BTC_fs_SST_wo <- ifelse(realisedval_BTC_fs_SST_wo < VaR_BTC_fs_SST_wo, 1, 0)*((realisedval_BTC_fs_SST_wo - VaR_BTC_fs_SST_wo)/VaR_BTC_fs_SST_wo)

QPS1_BTC_fs_SST_wo <- (2/350)*sum(((zi1_BTC_fs_SST_wo - 0.01)^2))
QPS2_BTC_fs_SST_wo <- (2/350)*sum(((zi2_BTC_fs_SST_wo - 0.01)^2))
QPS3_BTC_fs_SST_wo <- (2/350)*sum(((zi3_BTC_fs_SST_wo - 0.01)^2))

QPS_BTC_fs_SST_wo <- QPS1_BTC_fs_SST_wo + QPS2_BTC_fs_SST_wo + QPS3_BTC_fs_SST_wo
#SGED
VaR_BTC_fs_SGED_wo <- VR3_BTC_wo[,1]
realisedval_BTC_fs_SGED_wo <- VR3_BTC_wo[,4]
zi1_BTC_fs_SGED_wo <- ifelse(realisedval_BTC_fs_SGED_wo < VaR_BTC_fs_SGED_wo, 1, 0)
zi2_BTC_fs_SGED_wo <- ifelse(realisedval_BTC_fs_SGED_wo < VaR_BTC_fs_SGED_wo, 1, 0)*(1+((realisedval_BTC_fs_SGED_wo - VaR_BTC_fs_SGED_wo)^2))
zi3_BTC_fs_SGED_wo <- ifelse(realisedval_BTC_fs_SGED_wo < VaR_BTC_fs_SGED_wo, 1, 0)*((realisedval_BTC_fs_SGED_wo - VaR_BTC_fs_SGED_wo)/VaR_BTC_fs_SGED_wo)

QPS1_BTC_fs_SGED_wo <- (2/350)*sum(((zi1_BTC_fs_SGED_wo - 0.01)^2))
QPS2_BTC_fs_SGED_wo <- (2/350)*sum(((zi2_BTC_fs_SGED_wo - 0.01)^2))
QPS3_BTC_fs_SGED_wo <- (2/350)*sum(((zi3_BTC_fs_SGED_wo - 0.01)^2))

QPS_BTC_fs_SGED_wo <- QPS1_BTC_fs_SGED_wo + QPS2_BTC_fs_SGED_wo + QPS3_BTC_fs_SGED_wo
#JSU
VaR_BTC_fs_JSU_wo <- VR4_BTC_wo[,1]
realisedval_BTC_fs_JSU_wo <- VR4_BTC_wo[,4]
zi1_BTC_fs_JSU_wo <- ifelse(realisedval_BTC_fs_JSU_wo < VaR_BTC_fs_JSU_wo, 1, 0)
zi2_BTC_fs_JSU_wo <- ifelse(realisedval_BTC_fs_JSU_wo < VaR_BTC_fs_JSU_wo, 1, 0)*(1+((realisedval_BTC_fs_JSU_wo - VaR_BTC_fs_JSU_wo)^2))
zi3_BTC_fs_JSU_wo <- ifelse(realisedval_BTC_fs_JSU_wo < VaR_BTC_fs_JSU_wo, 1, 0)*((realisedval_BTC_fs_JSU_wo - VaR_BTC_fs_JSU_wo)/VaR_BTC_fs_JSU_wo)

QPS1_BTC_fs_JSU_wo <- (2/350)*sum(((zi1_BTC_fs_JSU_wo - 0.01)^2))
QPS2_BTC_fs_JSU_wo <- (2/350)*sum(((zi2_BTC_fs_JSU_wo - 0.01)^2))
QPS3_BTC_fs_JSU_wo <- (2/350)*sum(((zi3_BTC_fs_JSU_wo - 0.01)^2))

QPS_BTC_fs_JSU_wo <- QPS1_BTC_fs_JSU_wo + QPS2_BTC_fs_JSU_wo + QPS3_BTC_fs_JSU_wo
#PSIV
VaR_BTC_fs_PSIV_wo <- VR5_BTC_wo
realisedval_BTC_fs_PSIV_wo <- VR4_BTC_wo[,4]
zi1_BTC_fs_PSIV_wo <- ifelse(realisedval_BTC_fs_PSIV_wo < VaR_BTC_fs_PSIV_wo, 1, 0)
zi2_BTC_fs_PSIV_wo <- ifelse(realisedval_BTC_fs_PSIV_wo < VaR_BTC_fs_PSIV_wo, 1, 0)*(1+((realisedval_BTC_fs_PSIV_wo - VaR_BTC_fs_PSIV_wo)^2))
zi3_BTC_fs_PSIV_wo <- ifelse(realisedval_BTC_fs_PSIV_wo < VaR_BTC_fs_PSIV_wo, 1, 0)*((realisedval_BTC_fs_PSIV_wo - VaR_BTC_fs_PSIV_wo)/VaR_BTC_fs_PSIV_wo)

QPS1_BTC_fs_PSIV_wo <- (2/350)*sum(((zi1_BTC_fs_PSIV_wo - 0.01)^2))
QPS2_BTC_fs_PSIV_wo <- (2/350)*sum(((zi2_BTC_fs_PSIV_wo - 0.01)^2))
QPS3_BTC_fs_PSIV_wo <- (2/350)*sum(((zi3_BTC_fs_PSIV_wo - 0.01)^2))

QPS_BTC_fs_PSIV_wo <- QPS1_BTC_fs_PSIV_wo + QPS2_BTC_fs_PSIV_wo + QPS3_BTC_fs_PSIV_wo

QPS_BTC_fs_wo <- cbind(QPS_BTC_fs_norm_wo, QPS_BTC_fs_SST_wo, QPS_BTC_fs_SGED_wo, QPS_BTC_fs_JSU_wo, QPS_BTC_fs_PSIV_wo)

##### :::::::: E T H  :::::::: #####
# Full Sample

# Normal
VaR_ETH_fs_norm_wo <- VR1_ETH_wo[,1]
realisedval_ETH_fs_norm_wo <- VR1_ETH_wo[,4]
zi1_ETH_fs_norm_wo <- ifelse(realisedval_ETH_fs_norm_wo < VaR_ETH_fs_norm_wo, 1, 0)
zi2_ETH_fs_norm_wo <- ifelse(realisedval_ETH_fs_norm_wo < VaR_ETH_fs_norm_wo, 1, 0)*(1+((realisedval_ETH_fs_norm_wo - VaR_ETH_fs_norm_wo)^2))
zi3_ETH_fs_norm_wo <- ifelse(realisedval_ETH_fs_norm_wo < VaR_ETH_fs_norm_wo, 1, 0)*((realisedval_ETH_fs_norm_wo - VaR_ETH_fs_norm_wo)/VaR_ETH_fs_norm_wo)

QPS1_ETH_fs_norm_wo <- (2/350)*sum(((zi1_ETH_fs_norm_wo - 0.01)^2))
QPS2_ETH_fs_norm_wo <- (2/350)*sum(((zi2_ETH_fs_norm_wo - 0.01)^2))
QPS3_ETH_fs_norm_wo <- (2/350)*sum(((zi3_ETH_fs_norm_wo - 0.01)^2))

QPS_ETH_fs_norm_wo <- QPS1_ETH_fs_norm_wo + QPS2_ETH_fs_norm_wo + QPS3_ETH_fs_norm_wo 
#SST
VaR_ETH_fs_SST_wo <- VR2_ETH_wo[,1]
realisedval_ETH_fs_SST_wo <- VR2_ETH_wo[,4]
zi1_ETH_fs_SST_wo <- ifelse(realisedval_ETH_fs_SST_wo < VaR_ETH_fs_SST_wo, 1, 0)
zi2_ETH_fs_SST_wo <- ifelse(realisedval_ETH_fs_SST_wo < VaR_ETH_fs_SST_wo, 1, 0)*(1+((realisedval_ETH_fs_SST_wo - VaR_ETH_fs_SST_wo)^2))
zi3_ETH_fs_SST_wo <- ifelse(realisedval_ETH_fs_SST_wo < VaR_ETH_fs_SST_wo, 1, 0)*((realisedval_ETH_fs_SST_wo - VaR_ETH_fs_SST_wo)/VaR_ETH_fs_SST_wo)

QPS1_ETH_fs_SST_wo <- (2/350)*sum(((zi1_ETH_fs_SST_wo - 0.01)^2))
QPS2_ETH_fs_SST_wo <- (2/350)*sum(((zi2_ETH_fs_SST_wo - 0.01)^2))
QPS3_ETH_fs_SST_wo <- (2/350)*sum(((zi3_ETH_fs_SST_wo - 0.01)^2))

QPS_ETH_fs_SST_wo <- QPS1_ETH_fs_SST_wo + QPS2_ETH_fs_SST_wo + QPS3_ETH_fs_SST_wo
#SGED
VaR_ETH_fs_SGED_wo <- VR3_ETH_wo[,1]
realisedval_ETH_fs_SGED_wo <- VR3_ETH_wo[,4]
zi1_ETH_fs_SGED_wo <- ifelse(realisedval_ETH_fs_SGED_wo < VaR_ETH_fs_SGED_wo, 1, 0)
zi2_ETH_fs_SGED_wo <- ifelse(realisedval_ETH_fs_SGED_wo < VaR_ETH_fs_SGED_wo, 1, 0)*(1+((realisedval_ETH_fs_SGED_wo - VaR_ETH_fs_SGED_wo)^2))
zi3_ETH_fs_SGED_wo <- ifelse(realisedval_ETH_fs_SGED_wo < VaR_ETH_fs_SGED_wo, 1, 0)*((realisedval_ETH_fs_SGED_wo - VaR_ETH_fs_SGED_wo)/VaR_ETH_fs_SGED_wo)

QPS1_ETH_fs_SGED_wo <- (2/350)*sum(((zi1_ETH_fs_SGED_wo - 0.01)^2))
QPS2_ETH_fs_SGED_wo <- (2/350)*sum(((zi2_ETH_fs_SGED_wo - 0.01)^2))
QPS3_ETH_fs_SGED_wo <- (2/350)*sum(((zi3_ETH_fs_SGED_wo - 0.01)^2))

QPS_ETH_fs_SGED_wo <- QPS1_ETH_fs_SGED_wo + QPS2_ETH_fs_SGED_wo + QPS3_ETH_fs_SGED_wo
#JSU
VaR_ETH_fs_JSU_wo <- VR4_ETH_wo[,1]
realisedval_ETH_fs_JSU_wo <- VR4_ETH_wo[,4]
zi1_ETH_fs_JSU_wo <- ifelse(realisedval_ETH_fs_JSU_wo < VaR_ETH_fs_JSU_wo, 1, 0)
zi2_ETH_fs_JSU_wo <- ifelse(realisedval_ETH_fs_JSU_wo < VaR_ETH_fs_JSU_wo, 1, 0)*(1+((realisedval_ETH_fs_JSU_wo - VaR_ETH_fs_JSU_wo)^2))
zi3_ETH_fs_JSU_wo <- ifelse(realisedval_ETH_fs_JSU_wo < VaR_ETH_fs_JSU_wo, 1, 0)*((realisedval_ETH_fs_JSU_wo - VaR_ETH_fs_JSU_wo)/VaR_ETH_fs_JSU_wo)

QPS1_ETH_fs_JSU_wo <- (2/350)*sum(((zi1_ETH_fs_JSU_wo - 0.01)^2))
QPS2_ETH_fs_JSU_wo <- (2/350)*sum(((zi2_ETH_fs_JSU_wo - 0.01)^2))
QPS3_ETH_fs_JSU_wo <- (2/350)*sum(((zi3_ETH_fs_JSU_wo - 0.01)^2))

QPS_ETH_fs_JSU_wo <- QPS1_ETH_fs_JSU_wo + QPS2_ETH_fs_JSU_wo + QPS3_ETH_fs_JSU_wo
#PSIV
VaR_ETH_fs_PSIV_wo <- VR5_ETH_wo
realisedval_ETH_fs_PSIV_wo <- VR4_ETH_wo[,4]
zi1_ETH_fs_PSIV_wo <- ifelse(realisedval_ETH_fs_PSIV_wo < VaR_ETH_fs_PSIV_wo, 1, 0)
zi2_ETH_fs_PSIV_wo <- ifelse(realisedval_ETH_fs_PSIV_wo < VaR_ETH_fs_PSIV_wo, 1, 0)*(1+((realisedval_ETH_fs_PSIV_wo - VaR_ETH_fs_PSIV_wo)^2))
zi3_ETH_fs_PSIV_wo <- ifelse(realisedval_ETH_fs_PSIV_wo < VaR_ETH_fs_PSIV_wo, 1, 0)*((realisedval_ETH_fs_PSIV_wo - VaR_ETH_fs_PSIV_wo)/VaR_ETH_fs_PSIV_wo)

QPS1_ETH_fs_PSIV_wo <- (2/350)*sum(((zi1_ETH_fs_PSIV_wo - 0.01)^2))
QPS2_ETH_fs_PSIV_wo <- (2/350)*sum(((zi2_ETH_fs_PSIV_wo - 0.01)^2))
QPS3_ETH_fs_PSIV_wo <- (2/350)*sum(((zi3_ETH_fs_PSIV_wo - 0.01)^2))

QPS_ETH_fs_PSIV_wo <- QPS1_ETH_fs_PSIV_wo + QPS2_ETH_fs_PSIV_wo + QPS3_ETH_fs_PSIV_wo

QPS_ETH_fs_wo <- cbind(QPS_ETH_fs_norm_wo, QPS_ETH_fs_SST_wo, QPS_ETH_fs_SGED_wo, QPS_ETH_fs_JSU_wo, QPS_ETH_fs_PSIV_wo)

##### :::::::: L T C  :::::::: #####
# Full Sample

# Normal
VaR_LTC_fs_norm_wo <- VR1_LTC_wo[,1]
realisedval_LTC_fs_norm_wo <- VR1_LTC_wo[,4]
zi1_LTC_fs_norm_wo <- ifelse(realisedval_LTC_fs_norm_wo < VaR_LTC_fs_norm_wo, 1, 0)
zi2_LTC_fs_norm_wo <- ifelse(realisedval_LTC_fs_norm_wo < VaR_LTC_fs_norm_wo, 1, 0)*(1+((realisedval_LTC_fs_norm_wo - VaR_LTC_fs_norm_wo)^2))
zi3_LTC_fs_norm_wo <- ifelse(realisedval_LTC_fs_norm_wo < VaR_LTC_fs_norm_wo, 1, 0)*((realisedval_LTC_fs_norm_wo - VaR_LTC_fs_norm_wo)/VaR_LTC_fs_norm_wo)

QPS1_LTC_fs_norm_wo <- (2/350)*sum(((zi1_LTC_fs_norm_wo - 0.01)^2))
QPS2_LTC_fs_norm_wo <- (2/350)*sum(((zi2_LTC_fs_norm_wo - 0.01)^2))
QPS3_LTC_fs_norm_wo <- (2/350)*sum(((zi3_LTC_fs_norm_wo - 0.01)^2))

QPS_LTC_fs_norm_wo <- QPS1_LTC_fs_norm_wo + QPS2_LTC_fs_norm_wo + QPS3_LTC_fs_norm_wo 
#SST
VaR_LTC_fs_SST_wo <- VR2_LTC_wo[,1]
realisedval_LTC_fs_SST_wo <- VR2_LTC_wo[,4]
zi1_LTC_fs_SST_wo <- ifelse(realisedval_LTC_fs_SST_wo < VaR_LTC_fs_SST_wo, 1, 0)
zi2_LTC_fs_SST_wo <- ifelse(realisedval_LTC_fs_SST_wo < VaR_LTC_fs_SST_wo, 1, 0)*(1+((realisedval_LTC_fs_SST_wo - VaR_LTC_fs_SST_wo)^2))
zi3_LTC_fs_SST_wo <- ifelse(realisedval_LTC_fs_SST_wo < VaR_LTC_fs_SST_wo, 1, 0)*((realisedval_LTC_fs_SST_wo - VaR_LTC_fs_SST_wo)/VaR_LTC_fs_SST_wo)

QPS1_LTC_fs_SST_wo <- (2/350)*sum(((zi1_LTC_fs_SST_wo - 0.01)^2))
QPS2_LTC_fs_SST_wo <- (2/350)*sum(((zi2_LTC_fs_SST_wo - 0.01)^2))
QPS3_LTC_fs_SST_wo <- (2/350)*sum(((zi3_LTC_fs_SST_wo - 0.01)^2))

QPS_LTC_fs_SST_wo <- QPS1_LTC_fs_SST_wo + QPS2_LTC_fs_SST_wo + QPS3_LTC_fs_SST_wo
#SGED
VaR_LTC_fs_SGED_wo <- VR3_LTC_wo[,1]
realisedval_LTC_fs_SGED_wo <- VR3_LTC_wo[,4]
zi1_LTC_fs_SGED_wo <- ifelse(realisedval_LTC_fs_SGED_wo < VaR_LTC_fs_SGED_wo, 1, 0)
zi2_LTC_fs_SGED_wo <- ifelse(realisedval_LTC_fs_SGED_wo < VaR_LTC_fs_SGED_wo, 1, 0)*(1+((realisedval_LTC_fs_SGED_wo - VaR_LTC_fs_SGED_wo)^2))
zi3_LTC_fs_SGED_wo <- ifelse(realisedval_LTC_fs_SGED_wo < VaR_LTC_fs_SGED_wo, 1, 0)*((realisedval_LTC_fs_SGED_wo - VaR_LTC_fs_SGED_wo)/VaR_LTC_fs_SGED_wo)

QPS1_LTC_fs_SGED_wo <- (2/350)*sum(((zi1_LTC_fs_SGED_wo - 0.01)^2))
QPS2_LTC_fs_SGED_wo <- (2/350)*sum(((zi2_LTC_fs_SGED_wo - 0.01)^2))
QPS3_LTC_fs_SGED_wo <- (2/350)*sum(((zi3_LTC_fs_SGED_wo - 0.01)^2))

QPS_LTC_fs_SGED_wo <- QPS1_LTC_fs_SGED_wo + QPS2_LTC_fs_SGED_wo + QPS3_LTC_fs_SGED_wo
#JSU
VaR_LTC_fs_JSU_wo <- VR4_LTC_wo[,1]
realisedval_LTC_fs_JSU_wo <- VR4_LTC_wo[,4]
zi1_LTC_fs_JSU_wo <- ifelse(realisedval_LTC_fs_JSU_wo < VaR_LTC_fs_JSU_wo, 1, 0)
zi2_LTC_fs_JSU_wo <- ifelse(realisedval_LTC_fs_JSU_wo < VaR_LTC_fs_JSU_wo, 1, 0)*(1+((realisedval_LTC_fs_JSU_wo - VaR_LTC_fs_JSU_wo)^2))
zi3_LTC_fs_JSU_wo <- ifelse(realisedval_LTC_fs_JSU_wo < VaR_LTC_fs_JSU_wo, 1, 0)*((realisedval_LTC_fs_JSU_wo - VaR_LTC_fs_JSU_wo)/VaR_LTC_fs_JSU_wo)

QPS1_LTC_fs_JSU_wo <- (2/350)*sum(((zi1_LTC_fs_JSU_wo - 0.01)^2))
QPS2_LTC_fs_JSU_wo <- (2/350)*sum(((zi2_LTC_fs_JSU_wo - 0.01)^2))
QPS3_LTC_fs_JSU_wo <- (2/350)*sum(((zi3_LTC_fs_JSU_wo - 0.01)^2))

QPS_LTC_fs_JSU_wo <- QPS1_LTC_fs_JSU_wo + QPS2_LTC_fs_JSU_wo + QPS3_LTC_fs_JSU_wo
#PSIV
VaR_LTC_fs_PSIV_wo <- VR5_LTC_wo
realisedval_LTC_fs_PSIV_wo <- VR4_LTC_wo[,4]
zi1_LTC_fs_PSIV_wo <- ifelse(realisedval_LTC_fs_PSIV_wo < VaR_LTC_fs_PSIV_wo, 1, 0)
zi2_LTC_fs_PSIV_wo <- ifelse(realisedval_LTC_fs_PSIV_wo < VaR_LTC_fs_PSIV_wo, 1, 0)*(1+((realisedval_LTC_fs_PSIV_wo - VaR_LTC_fs_PSIV_wo)^2))
zi3_LTC_fs_PSIV_wo <- ifelse(realisedval_LTC_fs_PSIV_wo < VaR_LTC_fs_PSIV_wo, 1, 0)*((realisedval_LTC_fs_PSIV_wo - VaR_LTC_fs_PSIV_wo)/VaR_LTC_fs_PSIV_wo)

QPS1_LTC_fs_PSIV_wo <- (2/350)*sum(((zi1_LTC_fs_PSIV_wo - 0.01)^2))
QPS2_LTC_fs_PSIV_wo <- (2/350)*sum(((zi2_LTC_fs_PSIV_wo - 0.01)^2))
QPS3_LTC_fs_PSIV_wo <- (2/350)*sum(((zi3_LTC_fs_PSIV_wo - 0.01)^2))

QPS_LTC_fs_PSIV_wo <- QPS1_LTC_fs_PSIV_wo + QPS2_LTC_fs_PSIV_wo + QPS3_LTC_fs_PSIV_wo

QPS_LTC_fs_wo <- cbind(QPS_LTC_fs_norm_wo, QPS_LTC_fs_SST_wo, QPS_LTC_fs_SGED_wo, QPS_LTC_fs_JSU_wo, QPS_LTC_fs_PSIV_wo)

##### :::::::: X M R  :::::::: #####
# Full Sample

# Normal
VaR_XMR_fs_norm_wo <- VR1_XMR_wo[,1]
realisedval_XMR_fs_norm_wo <- VR1_XMR_wo[,4]
zi1_XMR_fs_norm_wo <- ifelse(realisedval_XMR_fs_norm_wo < VaR_XMR_fs_norm_wo, 1, 0)
zi2_XMR_fs_norm_wo <- ifelse(realisedval_XMR_fs_norm_wo < VaR_XMR_fs_norm_wo, 1, 0)*(1+((realisedval_XMR_fs_norm_wo - VaR_XMR_fs_norm_wo)^2))
zi3_XMR_fs_norm_wo <- ifelse(realisedval_XMR_fs_norm_wo < VaR_XMR_fs_norm_wo, 1, 0)*((realisedval_XMR_fs_norm_wo - VaR_XMR_fs_norm_wo)/VaR_XMR_fs_norm_wo)

QPS1_XMR_fs_norm_wo <- (2/350)*sum(((zi1_XMR_fs_norm_wo - 0.01)^2))
QPS2_XMR_fs_norm_wo <- (2/350)*sum(((zi2_XMR_fs_norm_wo - 0.01)^2))
QPS3_XMR_fs_norm_wo <- (2/350)*sum(((zi3_XMR_fs_norm_wo - 0.01)^2))

QPS_XMR_fs_norm_wo <- QPS1_XMR_fs_norm_wo + QPS2_XMR_fs_norm_wo + QPS3_XMR_fs_norm_wo 
#SST
VaR_XMR_fs_SST_wo <- VR2_XMR_wo[,1]
realisedval_XMR_fs_SST_wo <- VR2_XMR_wo[,4]
zi1_XMR_fs_SST_wo <- ifelse(realisedval_XMR_fs_SST_wo < VaR_XMR_fs_SST_wo, 1, 0)
zi2_XMR_fs_SST_wo <- ifelse(realisedval_XMR_fs_SST_wo < VaR_XMR_fs_SST_wo, 1, 0)*(1+((realisedval_XMR_fs_SST_wo - VaR_XMR_fs_SST_wo)^2))
zi3_XMR_fs_SST_wo <- ifelse(realisedval_XMR_fs_SST_wo < VaR_XMR_fs_SST_wo, 1, 0)*((realisedval_XMR_fs_SST_wo - VaR_XMR_fs_SST_wo)/VaR_XMR_fs_SST_wo)

QPS1_XMR_fs_SST_wo <- (2/350)*sum(((zi1_XMR_fs_SST_wo - 0.01)^2))
QPS2_XMR_fs_SST_wo <- (2/350)*sum(((zi2_XMR_fs_SST_wo - 0.01)^2))
QPS3_XMR_fs_SST_wo <- (2/350)*sum(((zi3_XMR_fs_SST_wo - 0.01)^2))

QPS_XMR_fs_SST_wo <- QPS1_XMR_fs_SST_wo + QPS2_XMR_fs_SST_wo + QPS3_XMR_fs_SST_wo
#SGED
VaR_XMR_fs_SGED_wo <- VR3_XMR_wo[,1]
realisedval_XMR_fs_SGED_wo <- VR3_XMR_wo[,4]
zi1_XMR_fs_SGED_wo <- ifelse(realisedval_XMR_fs_SGED_wo < VaR_XMR_fs_SGED_wo, 1, 0)
zi2_XMR_fs_SGED_wo <- ifelse(realisedval_XMR_fs_SGED_wo < VaR_XMR_fs_SGED_wo, 1, 0)*(1+((realisedval_XMR_fs_SGED_wo - VaR_XMR_fs_SGED_wo)^2))
zi3_XMR_fs_SGED_wo <- ifelse(realisedval_XMR_fs_SGED_wo < VaR_XMR_fs_SGED_wo, 1, 0)*((realisedval_XMR_fs_SGED_wo - VaR_XMR_fs_SGED_wo)/VaR_XMR_fs_SGED_wo)

QPS1_XMR_fs_SGED_wo <- (2/350)*sum(((zi1_XMR_fs_SGED_wo - 0.01)^2))
QPS2_XMR_fs_SGED_wo <- (2/350)*sum(((zi2_XMR_fs_SGED_wo - 0.01)^2))
QPS3_XMR_fs_SGED_wo <- (2/350)*sum(((zi3_XMR_fs_SGED_wo - 0.01)^2))

QPS_XMR_fs_SGED_wo <- QPS1_XMR_fs_SGED_wo + QPS2_XMR_fs_SGED_wo + QPS3_XMR_fs_SGED_wo
#JSU
VaR_XMR_fs_JSU_wo <- VR4_XMR_wo[,1]
realisedval_XMR_fs_JSU_wo <- VR4_XMR_wo[,4]
zi1_XMR_fs_JSU_wo <- ifelse(realisedval_XMR_fs_JSU_wo < VaR_XMR_fs_JSU_wo, 1, 0)
zi2_XMR_fs_JSU_wo <- ifelse(realisedval_XMR_fs_JSU_wo < VaR_XMR_fs_JSU_wo, 1, 0)*(1+((realisedval_XMR_fs_JSU_wo - VaR_XMR_fs_JSU_wo)^2))
zi3_XMR_fs_JSU_wo <- ifelse(realisedval_XMR_fs_JSU_wo < VaR_XMR_fs_JSU_wo, 1, 0)*((realisedval_XMR_fs_JSU_wo - VaR_XMR_fs_JSU_wo)/VaR_XMR_fs_JSU_wo)

QPS1_XMR_fs_JSU_wo <- (2/350)*sum(((zi1_XMR_fs_JSU_wo - 0.01)^2))
QPS2_XMR_fs_JSU_wo <- (2/350)*sum(((zi2_XMR_fs_JSU_wo - 0.01)^2))
QPS3_XMR_fs_JSU_wo <- (2/350)*sum(((zi3_XMR_fs_JSU_wo - 0.01)^2))

QPS_XMR_fs_JSU_wo <- QPS1_XMR_fs_JSU_wo + QPS2_XMR_fs_JSU_wo + QPS3_XMR_fs_JSU_wo
#PSIV
VaR_XMR_fs_PSIV_wo <- VR5_XMR_wo
realisedval_XMR_fs_PSIV_wo <- VR4_XMR_wo[,4]
zi1_XMR_fs_PSIV_wo <- ifelse(realisedval_XMR_fs_PSIV_wo < VaR_XMR_fs_PSIV_wo, 1, 0)
zi2_XMR_fs_PSIV_wo <- ifelse(realisedval_XMR_fs_PSIV_wo < VaR_XMR_fs_PSIV_wo, 1, 0)*(1+((realisedval_XMR_fs_PSIV_wo - VaR_XMR_fs_PSIV_wo)^2))
zi3_XMR_fs_PSIV_wo <- ifelse(realisedval_XMR_fs_PSIV_wo < VaR_XMR_fs_PSIV_wo, 1, 0)*((realisedval_XMR_fs_PSIV_wo - VaR_XMR_fs_PSIV_wo)/VaR_XMR_fs_PSIV_wo)

QPS1_XMR_fs_PSIV_wo <- (2/350)*sum(((zi1_XMR_fs_PSIV_wo - 0.01)^2))
QPS2_XMR_fs_PSIV_wo <- (2/350)*sum(((zi2_XMR_fs_PSIV_wo - 0.01)^2))
QPS3_XMR_fs_PSIV_wo <- (2/350)*sum(((zi3_XMR_fs_PSIV_wo - 0.01)^2))

QPS_XMR_fs_PSIV_wo <- QPS1_XMR_fs_PSIV_wo + QPS2_XMR_fs_PSIV_wo + QPS3_XMR_fs_PSIV_wo

QPS_XMR_fs_wo <- cbind(QPS_XMR_fs_norm_wo, QPS_XMR_fs_SST_wo, QPS_XMR_fs_SGED_wo, QPS_XMR_fs_JSU_wo, QPS_XMR_fs_PSIV_wo)

##### :::::::: E T C  :::::::: #####
# Full Sample

# Normal
VaR_ETC_fs_norm_wo <- VR1_ETC_wo[,1]
realisedval_ETC_fs_norm_wo <- VR1_ETC_wo[,4]
zi1_ETC_fs_norm_wo <- ifelse(realisedval_ETC_fs_norm_wo < VaR_ETC_fs_norm_wo, 1, 0)
zi2_ETC_fs_norm_wo <- ifelse(realisedval_ETC_fs_norm_wo < VaR_ETC_fs_norm_wo, 1, 0)*(1+((realisedval_ETC_fs_norm_wo - VaR_ETC_fs_norm_wo)^2))
zi3_ETC_fs_norm_wo <- ifelse(realisedval_ETC_fs_norm_wo < VaR_ETC_fs_norm_wo, 1, 0)*((realisedval_ETC_fs_norm_wo - VaR_ETC_fs_norm_wo)/VaR_ETC_fs_norm_wo)

QPS1_ETC_fs_norm_wo <- (2/350)*sum(((zi1_ETC_fs_norm_wo - 0.01)^2))
QPS2_ETC_fs_norm_wo <- (2/350)*sum(((zi2_ETC_fs_norm_wo - 0.01)^2))
QPS3_ETC_fs_norm_wo <- (2/350)*sum(((zi3_ETC_fs_norm_wo - 0.01)^2))

QPS_ETC_fs_norm_wo <- QPS1_ETC_fs_norm_wo + QPS2_ETC_fs_norm_wo + QPS3_ETC_fs_norm_wo 
#SST
VaR_ETC_fs_SST_wo <- VR2_ETC_wo[,1]
realisedval_ETC_fs_SST_wo <- VR2_ETC_wo[,4]
zi1_ETC_fs_SST_wo <- ifelse(realisedval_ETC_fs_SST_wo < VaR_ETC_fs_SST_wo, 1, 0)
zi2_ETC_fs_SST_wo <- ifelse(realisedval_ETC_fs_SST_wo < VaR_ETC_fs_SST_wo, 1, 0)*(1+((realisedval_ETC_fs_SST_wo - VaR_ETC_fs_SST_wo)^2))
zi3_ETC_fs_SST_wo <- ifelse(realisedval_ETC_fs_SST_wo < VaR_ETC_fs_SST_wo, 1, 0)*((realisedval_ETC_fs_SST_wo - VaR_ETC_fs_SST_wo)/VaR_ETC_fs_SST_wo)

QPS1_ETC_fs_SST_wo <- (2/350)*sum(((zi1_ETC_fs_SST_wo - 0.01)^2))
QPS2_ETC_fs_SST_wo <- (2/350)*sum(((zi2_ETC_fs_SST_wo - 0.01)^2))
QPS3_ETC_fs_SST_wo <- (2/350)*sum(((zi3_ETC_fs_SST_wo - 0.01)^2))

QPS_ETC_fs_SST_wo <- QPS1_ETC_fs_SST_wo + QPS2_ETC_fs_SST_wo + QPS3_ETC_fs_SST_wo
#SGED
VaR_ETC_fs_SGED_wo <- VR3_ETC_wo[,1]
realisedval_ETC_fs_SGED_wo <- VR3_ETC_wo[,4]
zi1_ETC_fs_SGED_wo <- ifelse(realisedval_ETC_fs_SGED_wo < VaR_ETC_fs_SGED_wo, 1, 0)
zi2_ETC_fs_SGED_wo <- ifelse(realisedval_ETC_fs_SGED_wo < VaR_ETC_fs_SGED_wo, 1, 0)*(1+((realisedval_ETC_fs_SGED_wo - VaR_ETC_fs_SGED_wo)^2))
zi3_ETC_fs_SGED_wo <- ifelse(realisedval_ETC_fs_SGED_wo < VaR_ETC_fs_SGED_wo, 1, 0)*((realisedval_ETC_fs_SGED_wo - VaR_ETC_fs_SGED_wo)/VaR_ETC_fs_SGED_wo)

QPS1_ETC_fs_SGED_wo <- (2/350)*sum(((zi1_ETC_fs_SGED_wo - 0.01)^2))
QPS2_ETC_fs_SGED_wo <- (2/350)*sum(((zi2_ETC_fs_SGED_wo - 0.01)^2))
QPS3_ETC_fs_SGED_wo <- (2/350)*sum(((zi3_ETC_fs_SGED_wo - 0.01)^2))

QPS_ETC_fs_SGED_wo <- QPS1_ETC_fs_SGED_wo + QPS2_ETC_fs_SGED_wo + QPS3_ETC_fs_SGED_wo
#JSU
VaR_ETC_fs_JSU_wo <- VR4_ETC_wo[,1]
realisedval_ETC_fs_JSU_wo <- VR4_ETC_wo[,4]
zi1_ETC_fs_JSU_wo <- ifelse(realisedval_ETC_fs_JSU_wo < VaR_ETC_fs_JSU_wo, 1, 0)
zi2_ETC_fs_JSU_wo <- ifelse(realisedval_ETC_fs_JSU_wo < VaR_ETC_fs_JSU_wo, 1, 0)*(1+((realisedval_ETC_fs_JSU_wo - VaR_ETC_fs_JSU_wo)^2))
zi3_ETC_fs_JSU_wo <- ifelse(realisedval_ETC_fs_JSU_wo < VaR_ETC_fs_JSU_wo, 1, 0)*((realisedval_ETC_fs_JSU_wo - VaR_ETC_fs_JSU_wo)/VaR_ETC_fs_JSU_wo)

QPS1_ETC_fs_JSU_wo <- (2/350)*sum(((zi1_ETC_fs_JSU_wo - 0.01)^2))
QPS2_ETC_fs_JSU_wo <- (2/350)*sum(((zi2_ETC_fs_JSU_wo - 0.01)^2))
QPS3_ETC_fs_JSU_wo <- (2/350)*sum(((zi3_ETC_fs_JSU_wo - 0.01)^2))

QPS_ETC_fs_JSU_wo <- QPS1_ETC_fs_JSU_wo + QPS2_ETC_fs_JSU_wo + QPS3_ETC_fs_JSU_wo
#PSIV
VaR_ETC_fs_PSIV_wo <- VR5_ETC_wo
realisedval_ETC_fs_PSIV_wo <- VR4_ETC_wo[,4]
zi1_ETC_fs_PSIV_wo <- ifelse(realisedval_ETC_fs_PSIV_wo < VaR_ETC_fs_PSIV_wo, 1, 0)
zi2_ETC_fs_PSIV_wo <- ifelse(realisedval_ETC_fs_PSIV_wo < VaR_ETC_fs_PSIV_wo, 1, 0)*(1+((realisedval_ETC_fs_PSIV_wo - VaR_ETC_fs_PSIV_wo)^2))
zi3_ETC_fs_PSIV_wo <- ifelse(realisedval_ETC_fs_PSIV_wo < VaR_ETC_fs_PSIV_wo, 1, 0)*((realisedval_ETC_fs_PSIV_wo - VaR_ETC_fs_PSIV_wo)/VaR_ETC_fs_PSIV_wo)

QPS1_ETC_fs_PSIV_wo <- (2/350)*sum(((zi1_ETC_fs_PSIV_wo - 0.01)^2))
QPS2_ETC_fs_PSIV_wo <- (2/350)*sum(((zi2_ETC_fs_PSIV_wo - 0.01)^2))
QPS3_ETC_fs_PSIV_wo <- (2/350)*sum(((zi3_ETC_fs_PSIV_wo - 0.01)^2))

QPS_ETC_fs_PSIV_wo <- QPS1_ETC_fs_PSIV_wo + QPS2_ETC_fs_PSIV_wo + QPS3_ETC_fs_PSIV_wo

QPS_ETC_fs_wo <- cbind(QPS_ETC_fs_norm_wo, QPS_ETC_fs_SST_wo, QPS_ETC_fs_SGED_wo, QPS_ETC_fs_JSU_wo, QPS_ETC_fs_PSIV_wo)

QPS_GARCH_wo <- rbind(QPS_BTC_fs_wo, QPS_ETH_fs_wo, QPS_LTC_fs_wo, QPS_XMR_fs_wo, QPS_ETC_fs_wo)
View(QPS_GARCH_wo)