# =============================================================================
# 09_qps_gas.R
#
# Purpose : QPS of 1% VaR for GAS models.
# Inputs  : objects from 06
# Outputs : QPS_<coin>_GAS, QPS_GAS
# Paper   : Table 5 (GAS columns)
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

#### VaR accuracy Stage 2 - GAS models - QPS

##### :::::::: B T C :::::::: #####
# Full Sample
#Normal
VaR_BTC_fs_norm0 <- VaR_BTC_norm0[,1]
realisedval_BTC_fs_norm0 <- VR1_BTC[,4]
zi1_BTC_fs_norm0 <- ifelse(realisedval_BTC_fs_norm0 < VaR_BTC_fs_norm0, 1, 0)
zi2_BTC_fs_norm0 <- ifelse(realisedval_BTC_fs_norm0 < VaR_BTC_fs_norm0, 1, 0)*(1+((realisedval_BTC_fs_norm0 - VaR_BTC_fs_norm0)^2))
zi3_BTC_fs_norm0 <- ifelse(realisedval_BTC_fs_norm0 < VaR_BTC_fs_norm0, 1, 0)*((realisedval_BTC_fs_norm0 - VaR_BTC_fs_norm0)/VaR_BTC_fs_norm0)

QPS1_BTC_fs_norm0 <- (2/350)*sum(((zi1_BTC_fs_norm0 - 0.01)^2))
QPS2_BTC_fs_norm0 <- (2/350)*sum(((zi2_BTC_fs_norm0 - 0.01)^2))
QPS3_BTC_fs_norm0 <- (2/350)*sum(((zi3_BTC_fs_norm0 - 0.01)^2))

QPS_BTC_fs_norm0 <- QPS1_BTC_fs_norm0 + QPS2_BTC_fs_norm0 + QPS3_BTC_fs_norm0 
#ST
VaR_BTC_fs_ST0 <- VaR_BTC_std0[,1]
realisedval_BTC_fs_ST0 <- VR2_BTC[,4]
zi1_BTC_fs_ST0 <- ifelse(realisedval_BTC_fs_ST0 < VaR_BTC_fs_ST0, 1, 0)
zi2_BTC_fs_ST0 <- ifelse(realisedval_BTC_fs_ST0 < VaR_BTC_fs_ST0, 1, 0)*(1+((realisedval_BTC_fs_ST0 - VaR_BTC_fs_ST0)^2))
zi3_BTC_fs_ST0 <- ifelse(realisedval_BTC_fs_ST0 < VaR_BTC_fs_ST0, 1, 0)*((realisedval_BTC_fs_ST0 - VaR_BTC_fs_ST0)/VaR_BTC_fs_ST0)

QPS1_BTC_fs_ST0 <- (2/350)*sum(((zi1_BTC_fs_ST0 - 0.01)^2))
QPS2_BTC_fs_ST0 <- (2/350)*sum(((zi2_BTC_fs_ST0 - 0.01)^2))
QPS3_BTC_fs_ST0 <- (2/350)*sum(((zi3_BTC_fs_ST0 - 0.01)^2))

QPS_BTC_fs_ST0 <- QPS1_BTC_fs_ST0 + QPS2_BTC_fs_ST0 + QPS3_BTC_fs_ST0
#SST
VaR_BTC_fs_SST0 <- VaR_BTC_sstd0[,1]
realisedval_BTC_fs_SST0 <- VR3_BTC[,4]
zi1_BTC_fs_SST0 <- ifelse(realisedval_BTC_fs_SST0 < VaR_BTC_fs_SST0, 1, 0)
zi2_BTC_fs_SST0 <- ifelse(realisedval_BTC_fs_SST0 < VaR_BTC_fs_SST0, 1, 0)*(1+((realisedval_BTC_fs_SST0 - VaR_BTC_fs_SST0)^2))
zi3_BTC_fs_SST0 <- ifelse(realisedval_BTC_fs_SST0 < VaR_BTC_fs_SST0, 1, 0)*((realisedval_BTC_fs_SST0 - VaR_BTC_fs_SST0)/VaR_BTC_fs_SST0)

QPS1_BTC_fs_SST0 <- (2/350)*sum(((zi1_BTC_fs_SST0 - 0.01)^2))
QPS2_BTC_fs_SST0 <- (2/350)*sum(((zi2_BTC_fs_SST0 - 0.01)^2))
QPS3_BTC_fs_SST0 <- (2/350)*sum(((zi3_BTC_fs_SST0 - 0.01)^2))

QPS_BTC_fs_SST0 <- QPS1_BTC_fs_SST0 + QPS2_BTC_fs_SST0 + QPS3_BTC_fs_SST0
#AST
VaR_BTC_fs_ast0 <- VaR_BTC_ast0[,1]
realisedval_BTC_fs_ast0 <- VR4_BTC[,4]
zi1_BTC_fs_ast0 <- ifelse(realisedval_BTC_fs_ast0 < VaR_BTC_fs_ast0, 1, 0)
zi2_BTC_fs_ast0 <- ifelse(realisedval_BTC_fs_ast0 < VaR_BTC_fs_ast0, 1, 0)*(1+((realisedval_BTC_fs_ast0 - VaR_BTC_fs_ast0)^2))
zi3_BTC_fs_ast0 <- ifelse(realisedval_BTC_fs_ast0 < VaR_BTC_fs_ast0, 1, 0)*((realisedval_BTC_fs_ast0 - VaR_BTC_fs_ast0)/VaR_BTC_fs_ast0)

QPS1_BTC_fs_ast0 <- (2/350)*sum(((zi1_BTC_fs_ast0 - 0.01)^2))
QPS2_BTC_fs_ast0 <- (2/350)*sum(((zi2_BTC_fs_ast0 - 0.01)^2))
QPS3_BTC_fs_ast0 <- (2/350)*sum(((zi3_BTC_fs_ast0 - 0.01)^2))

QPS_BTC_fs_ast0 <- QPS1_BTC_fs_ast0 + QPS2_BTC_fs_ast0 + QPS3_BTC_fs_ast0
#AST1
VaR_BTC_fs_astone0 <- VaR_BTC_astone0[,1]
realisedval_BTC_fs_astone0 <- VR4_BTC[,4]
zi1_BTC_fs_astone0 <- ifelse(realisedval_BTC_fs_astone0 < VaR_BTC_fs_astone0, 1, 0)
zi2_BTC_fs_astone0 <- ifelse(realisedval_BTC_fs_astone0 < VaR_BTC_fs_astone0, 1, 0)*(1+((realisedval_BTC_fs_astone0 - VaR_BTC_fs_astone0)^2))
zi3_BTC_fs_astone0 <- ifelse(realisedval_BTC_fs_astone0 < VaR_BTC_fs_astone0, 1, 0)*((realisedval_BTC_fs_astone0 - VaR_BTC_fs_astone0)/VaR_BTC_fs_astone0)

QPS1_BTC_fs_astone0 <- (2/350)*sum(((zi1_BTC_fs_astone0 - 0.01)^2))
QPS2_BTC_fs_astone0 <- (2/350)*sum(((zi2_BTC_fs_astone0 - 0.01)^2))
QPS3_BTC_fs_astone0 <- (2/350)*sum(((zi3_BTC_fs_astone0 - 0.01)^2))

QPS_BTC_fs_astone0 <- QPS1_BTC_fs_astone0 + QPS2_BTC_fs_astone0 + QPS3_BTC_fs_astone0

# Sub Sample 1
#Normal
VaR_BTC_ss1_norm1 <- VaR_BTC_norm1[,1]
realisedval_BTC_ss1_norm1 <- VR11_BTC[,4]
zi1_BTC_ss1_norm1 <- ifelse(realisedval_BTC_ss1_norm1 < VaR_BTC_ss1_norm1, 1, 0)
zi2_BTC_ss1_norm1 <- ifelse(realisedval_BTC_ss1_norm1 < VaR_BTC_ss1_norm1, 1, 0)*(1+((realisedval_BTC_ss1_norm1 - VaR_BTC_ss1_norm1)^2))
zi3_BTC_ss1_norm1 <- ifelse(realisedval_BTC_ss1_norm1 < VaR_BTC_ss1_norm1, 1, 0)*((realisedval_BTC_ss1_norm1 - VaR_BTC_ss1_norm1)/VaR_BTC_ss1_norm1)

QPS1_BTC_ss1_norm1 <- (2/60)*sum(((zi1_BTC_ss1_norm1 - 0.01)^2))
QPS2_BTC_ss1_norm1 <- (2/60)*sum(((zi2_BTC_ss1_norm1 - 0.01)^2))
QPS3_BTC_ss1_norm1 <- (2/60)*sum(((zi3_BTC_ss1_norm1 - 0.01)^2))

QPS_BTC_ss1_norm1 <- QPS1_BTC_ss1_norm1 + QPS2_BTC_ss1_norm1 + QPS3_BTC_ss1_norm1 
#ST
VaR_BTC_ss1_ST1 <- VaR_BTC_std1[,1]
realisedval_BTC_ss1_ST1 <- VR12_BTC[,4]
zi1_BTC_ss1_ST1 <- ifelse(realisedval_BTC_ss1_ST1 < VaR_BTC_ss1_ST1, 1, 0)
zi2_BTC_ss1_ST1 <- ifelse(realisedval_BTC_ss1_ST1 < VaR_BTC_ss1_ST1, 1, 0)*(1+((realisedval_BTC_ss1_ST1 - VaR_BTC_ss1_ST1)^2))
zi3_BTC_ss1_ST1 <- ifelse(realisedval_BTC_ss1_ST1 < VaR_BTC_ss1_ST1, 1, 0)*((realisedval_BTC_ss1_ST1 - VaR_BTC_ss1_ST1)/VaR_BTC_ss1_ST1)

QPS1_BTC_ss1_ST1 <- (2/60)*sum(((zi1_BTC_ss1_ST1 - 0.01)^2))
QPS2_BTC_ss1_ST1 <- (2/60)*sum(((zi2_BTC_ss1_ST1 - 0.01)^2))
QPS3_BTC_ss1_ST1 <- (2/60)*sum(((zi3_BTC_ss1_ST1 - 0.01)^2))

QPS_BTC_ss1_ST1 <- QPS1_BTC_ss1_ST1 + QPS2_BTC_ss1_ST1 + QPS3_BTC_ss1_ST1
#SST
VaR_BTC_ss1_SST1 <- VaR_BTC_sstd1[,1]
realisedval_BTC_ss1_SST1 <- VR13_BTC[,4]
zi1_BTC_ss1_SST1 <- ifelse(realisedval_BTC_ss1_SST1 < VaR_BTC_ss1_SST1, 1, 0)
zi2_BTC_ss1_SST1 <- ifelse(realisedval_BTC_ss1_SST1 < VaR_BTC_ss1_SST1, 1, 0)*(1+((realisedval_BTC_ss1_SST1 - VaR_BTC_ss1_SST1)^2))
zi3_BTC_ss1_SST1 <- ifelse(realisedval_BTC_ss1_SST1 < VaR_BTC_ss1_SST1, 1, 0)*((realisedval_BTC_ss1_SST1 - VaR_BTC_ss1_SST1)/VaR_BTC_ss1_SST1)

QPS1_BTC_ss1_SST1 <- (2/60)*sum(((zi1_BTC_ss1_SST1 - 0.01)^2))
QPS2_BTC_ss1_SST1 <- (2/60)*sum(((zi2_BTC_ss1_SST1 - 0.01)^2))
QPS3_BTC_ss1_SST1 <- (2/60)*sum(((zi3_BTC_ss1_SST1 - 0.01)^2))

QPS_BTC_ss1_SST1 <- QPS1_BTC_ss1_SST1 + QPS2_BTC_ss1_SST1 + QPS3_BTC_ss1_SST1
#AST
VaR_BTC_ss1_ast1 <- VaR_BTC_ast1[,1]
realisedval_BTC_ss1_ast1 <- VR14_BTC[,4]
zi1_BTC_ss1_ast1 <- ifelse(realisedval_BTC_ss1_ast1 < VaR_BTC_ss1_ast1, 1, 0)
zi2_BTC_ss1_ast1 <- ifelse(realisedval_BTC_ss1_ast1 < VaR_BTC_ss1_ast1, 1, 0)*(1+((realisedval_BTC_ss1_ast1 - VaR_BTC_ss1_ast1)^2))
zi3_BTC_ss1_ast1 <- ifelse(realisedval_BTC_ss1_ast1 < VaR_BTC_ss1_ast1, 1, 0)*((realisedval_BTC_ss1_ast1 - VaR_BTC_ss1_ast1)/VaR_BTC_ss1_ast1)

QPS1_BTC_ss1_ast1 <- (2/60)*sum(((zi1_BTC_ss1_ast1 - 0.01)^2))
QPS2_BTC_ss1_ast1 <- (2/60)*sum(((zi2_BTC_ss1_ast1 - 0.01)^2))
QPS3_BTC_ss1_ast1 <- (2/60)*sum(((zi3_BTC_ss1_ast1 - 0.01)^2))

QPS_BTC_ss1_ast1 <- QPS1_BTC_ss1_ast1 + QPS2_BTC_ss1_ast1 + QPS3_BTC_ss1_ast1
#AST1
VaR_BTC_ss1_astone1 <- VaR_BTC_astone1[,1]
realisedval_BTC_ss1_astone1 <- VR14_BTC[,4]
zi1_BTC_ss1_astone1 <- ifelse(realisedval_BTC_ss1_astone1 < VaR_BTC_ss1_astone1, 1, 0)
zi2_BTC_ss1_astone1 <- ifelse(realisedval_BTC_ss1_astone1 < VaR_BTC_ss1_astone1, 1, 0)*(1+((realisedval_BTC_ss1_astone1 - VaR_BTC_ss1_astone1)^2))
zi3_BTC_ss1_astone1 <- ifelse(realisedval_BTC_ss1_astone1 < VaR_BTC_ss1_astone1, 1, 0)*((realisedval_BTC_ss1_astone1 - VaR_BTC_ss1_astone1)/VaR_BTC_ss1_astone1)

QPS1_BTC_ss1_astone1 <- (2/60)*sum(((zi1_BTC_ss1_astone1 - 0.01)^2))
QPS2_BTC_ss1_astone1 <- (2/60)*sum(((zi2_BTC_ss1_astone1 - 0.01)^2))
QPS3_BTC_ss1_astone1 <- (2/60)*sum(((zi3_BTC_ss1_astone1 - 0.01)^2))

QPS_BTC_ss1_astone1 <- QPS1_BTC_ss1_astone1 + QPS2_BTC_ss1_astone1 + QPS3_BTC_ss1_astone1

# Sub Sample 2
#Normal
VaR_BTC_ss2_norm2 <- VaR_BTC_norm2[,1]
realisedval_BTC_ss2_norm2 <- VR21_BTC[,4]
zi1_BTC_ss2_norm2 <- ifelse(realisedval_BTC_ss2_norm2 < VaR_BTC_ss2_norm2, 1, 0)
zi2_BTC_ss2_norm2 <- ifelse(realisedval_BTC_ss2_norm2 < VaR_BTC_ss2_norm2, 1, 0)*(1+((realisedval_BTC_ss2_norm2 - VaR_BTC_ss2_norm2)^2))
zi3_BTC_ss2_norm2 <- ifelse(realisedval_BTC_ss2_norm2 < VaR_BTC_ss2_norm2, 1, 0)*((realisedval_BTC_ss2_norm2 - VaR_BTC_ss2_norm2)/VaR_BTC_ss2_norm2)

QPS1_BTC_ss2_norm2 <- (2/200)*sum(((zi1_BTC_ss2_norm2 - 0.01)^2))
QPS2_BTC_ss2_norm2 <- (2/200)*sum(((zi2_BTC_ss2_norm2 - 0.01)^2))
QPS3_BTC_ss2_norm2 <- (2/200)*sum(((zi3_BTC_ss2_norm2 - 0.01)^2))

QPS_BTC_ss2_norm2 <- QPS1_BTC_ss2_norm2 + QPS2_BTC_ss2_norm2 + QPS3_BTC_ss2_norm2 
#ST
VaR_BTC_ss2_ST2 <- VaR_BTC_std2[,1]
realisedval_BTC_ss2_ST2 <- VR22_BTC[,4]
zi1_BTC_ss2_ST2 <- ifelse(realisedval_BTC_ss2_ST2 < VaR_BTC_ss2_ST2, 1, 0)
zi2_BTC_ss2_ST2 <- ifelse(realisedval_BTC_ss2_ST2 < VaR_BTC_ss2_ST2, 1, 0)*(1+((realisedval_BTC_ss2_ST2 - VaR_BTC_ss2_ST2)^2))
zi3_BTC_ss2_ST2 <- ifelse(realisedval_BTC_ss2_ST2 < VaR_BTC_ss2_ST2, 1, 0)*((realisedval_BTC_ss2_ST2 - VaR_BTC_ss2_ST2)/VaR_BTC_ss2_ST2)

QPS1_BTC_ss2_ST2 <- (2/200)*sum(((zi1_BTC_ss2_ST2 - 0.01)^2))
QPS2_BTC_ss2_ST2 <- (2/200)*sum(((zi2_BTC_ss2_ST2 - 0.01)^2))
QPS3_BTC_ss2_ST2 <- (2/200)*sum(((zi3_BTC_ss2_ST2 - 0.01)^2))

QPS_BTC_ss2_ST2 <- QPS1_BTC_ss2_ST2 + QPS2_BTC_ss2_ST2 + QPS3_BTC_ss2_ST2
#SST
VaR_BTC_ss2_SST2 <- VaR_BTC_sstd2[,1]
realisedval_BTC_ss2_SST2 <- VR23_BTC[,4]
zi1_BTC_ss2_SST2 <- ifelse(realisedval_BTC_ss2_SST2 < VaR_BTC_ss2_SST2, 1, 0)
zi2_BTC_ss2_SST2 <- ifelse(realisedval_BTC_ss2_SST2 < VaR_BTC_ss2_SST2, 1, 0)*(1+((realisedval_BTC_ss2_SST2 - VaR_BTC_ss2_SST2)^2))
zi3_BTC_ss2_SST2 <- ifelse(realisedval_BTC_ss2_SST2 < VaR_BTC_ss2_SST2, 1, 0)*((realisedval_BTC_ss2_SST2 - VaR_BTC_ss2_SST2)/VaR_BTC_ss2_SST2)

QPS1_BTC_ss2_SST2 <- (2/200)*sum(((zi1_BTC_ss2_SST2 - 0.01)^2))
QPS2_BTC_ss2_SST2 <- (2/200)*sum(((zi2_BTC_ss2_SST2 - 0.01)^2))
QPS3_BTC_ss2_SST2 <- (2/200)*sum(((zi3_BTC_ss2_SST2 - 0.01)^2))

QPS_BTC_ss2_SST2 <- QPS1_BTC_ss2_SST2 + QPS2_BTC_ss2_SST2 + QPS3_BTC_ss2_SST2
#AST
VaR_BTC_ss2_ast2 <- VaR_BTC_ast2[,1]
realisedval_BTC_ss2_ast2 <- VR24_BTC[,4]
zi1_BTC_ss2_ast2 <- ifelse(realisedval_BTC_ss2_ast2 < VaR_BTC_ss2_ast2, 1, 0)
zi2_BTC_ss2_ast2 <- ifelse(realisedval_BTC_ss2_ast2 < VaR_BTC_ss2_ast2, 1, 0)*(1+((realisedval_BTC_ss2_ast2 - VaR_BTC_ss2_ast2)^2))
zi3_BTC_ss2_ast2 <- ifelse(realisedval_BTC_ss2_ast2 < VaR_BTC_ss2_ast2, 1, 0)*((realisedval_BTC_ss2_ast2 - VaR_BTC_ss2_ast2)/VaR_BTC_ss2_ast2)

QPS1_BTC_ss2_ast2 <- (2/200)*sum(((zi1_BTC_ss2_ast2 - 0.01)^2))
QPS2_BTC_ss2_ast2 <- (2/200)*sum(((zi2_BTC_ss2_ast2 - 0.01)^2))
QPS3_BTC_ss2_ast2 <- (2/200)*sum(((zi3_BTC_ss2_ast2 - 0.01)^2))

QPS_BTC_ss2_ast2 <- QPS1_BTC_ss2_ast2 + QPS2_BTC_ss2_ast2 + QPS3_BTC_ss2_ast2
#AST1
VaR_BTC_ss2_astone2 <- VaR_BTC_astone2[,1]
realisedval_BTC_ss2_astone2 <- VR24_BTC[,4]
zi1_BTC_ss2_astone2 <- ifelse(realisedval_BTC_ss2_astone2 < VaR_BTC_ss2_astone2, 1, 0)
zi2_BTC_ss2_astone2 <- ifelse(realisedval_BTC_ss2_astone2 < VaR_BTC_ss2_astone2, 1, 0)*(1+((realisedval_BTC_ss2_astone2 - VaR_BTC_ss2_astone2)^2))
zi3_BTC_ss2_astone2 <- ifelse(realisedval_BTC_ss2_astone2 < VaR_BTC_ss2_astone2, 1, 0)*((realisedval_BTC_ss2_astone2 - VaR_BTC_ss2_astone2)/VaR_BTC_ss2_astone2)

QPS1_BTC_ss2_astone2 <- (2/200)*sum(((zi1_BTC_ss2_astone2 - 0.01)^2))
QPS2_BTC_ss2_astone2 <- (2/200)*sum(((zi2_BTC_ss2_astone2 - 0.01)^2))
QPS3_BTC_ss2_astone2 <- (2/200)*sum(((zi3_BTC_ss2_astone2 - 0.01)^2))

QPS_BTC_ss2_astone2 <- QPS1_BTC_ss2_astone2 + QPS2_BTC_ss2_astone2 + QPS3_BTC_ss2_astone2

# Sub Sample 3
#Normal
VaR_BTC_ss3_norm3 <- VaR_BTC_norm3[,1]
realisedval_BTC_ss3_norm3 <- VR31_BTC[,4]
zi1_BTC_ss3_norm3 <- ifelse(realisedval_BTC_ss3_norm3 < VaR_BTC_ss3_norm3, 1, 0)
zi2_BTC_ss3_norm3 <- ifelse(realisedval_BTC_ss3_norm3 < VaR_BTC_ss3_norm3, 1, 0)*(1+((realisedval_BTC_ss3_norm3 - VaR_BTC_ss3_norm3)^2))
zi3_BTC_ss3_norm3 <- ifelse(realisedval_BTC_ss3_norm3 < VaR_BTC_ss3_norm3, 1, 0)*((realisedval_BTC_ss3_norm3 - VaR_BTC_ss3_norm3)/VaR_BTC_ss3_norm3)

QPS1_BTC_ss3_norm3 <- (2/100)*sum(((zi1_BTC_ss3_norm3 - 0.01)^2))
QPS2_BTC_ss3_norm3 <- (2/100)*sum(((zi2_BTC_ss3_norm3 - 0.01)^2))
QPS3_BTC_ss3_norm3 <- (2/100)*sum(((zi3_BTC_ss3_norm3 - 0.01)^2))

QPS_BTC_ss3_norm3 <- QPS1_BTC_ss3_norm3 + QPS2_BTC_ss3_norm3 + QPS3_BTC_ss3_norm3 
#ST
VaR_BTC_ss3_ST3 <- VaR_BTC_std3[,1]
realisedval_BTC_ss3_ST3 <- VR32_BTC[,4]
zi1_BTC_ss3_ST3 <- ifelse(realisedval_BTC_ss3_ST3 < VaR_BTC_ss3_ST3, 1, 0)
zi2_BTC_ss3_ST3 <- ifelse(realisedval_BTC_ss3_ST3 < VaR_BTC_ss3_ST3, 1, 0)*(1+((realisedval_BTC_ss3_ST3 - VaR_BTC_ss3_ST3)^2))
zi3_BTC_ss3_ST3 <- ifelse(realisedval_BTC_ss3_ST3 < VaR_BTC_ss3_ST3, 1, 0)*((realisedval_BTC_ss3_ST3 - VaR_BTC_ss3_ST3)/VaR_BTC_ss3_ST3)

QPS1_BTC_ss3_ST3 <- (2/100)*sum(((zi1_BTC_ss3_ST3 - 0.01)^2))
QPS2_BTC_ss3_ST3 <- (2/100)*sum(((zi2_BTC_ss3_ST3 - 0.01)^2))
QPS3_BTC_ss3_ST3 <- (2/100)*sum(((zi3_BTC_ss3_ST3 - 0.01)^2))

QPS_BTC_ss3_ST3 <- QPS1_BTC_ss3_ST3 + QPS2_BTC_ss3_ST3 + QPS3_BTC_ss3_ST3
#SST
VaR_BTC_ss3_SST3 <- VaR_BTC_sstd3[,1]
realisedval_BTC_ss3_SST3 <- VR33_BTC[,4]
zi1_BTC_ss3_SST3 <- ifelse(realisedval_BTC_ss3_SST3 < VaR_BTC_ss3_SST3, 1, 0)
zi2_BTC_ss3_SST3 <- ifelse(realisedval_BTC_ss3_SST3 < VaR_BTC_ss3_SST3, 1, 0)*(1+((realisedval_BTC_ss3_SST3 - VaR_BTC_ss3_SST3)^2))
zi3_BTC_ss3_SST3 <- ifelse(realisedval_BTC_ss3_SST3 < VaR_BTC_ss3_SST3, 1, 0)*((realisedval_BTC_ss3_SST3 - VaR_BTC_ss3_SST3)/VaR_BTC_ss3_SST3)

QPS1_BTC_ss3_SST3 <- (2/100)*sum(((zi1_BTC_ss3_SST3 - 0.01)^2))
QPS2_BTC_ss3_SST3 <- (2/100)*sum(((zi2_BTC_ss3_SST3 - 0.01)^2))
QPS3_BTC_ss3_SST3 <- (2/100)*sum(((zi3_BTC_ss3_SST3 - 0.01)^2))

QPS_BTC_ss3_SST3 <- QPS1_BTC_ss3_SST3 + QPS2_BTC_ss3_SST3 + QPS3_BTC_ss3_SST3
#AST
VaR_BTC_ss3_ast3 <- VaR_BTC_ast3[,1]
realisedval_BTC_ss3_ast3 <- VR34_BTC[,4]
zi1_BTC_ss3_ast3 <- ifelse(realisedval_BTC_ss3_ast3 < VaR_BTC_ss3_ast3, 1, 0)
zi2_BTC_ss3_ast3 <- ifelse(realisedval_BTC_ss3_ast3 < VaR_BTC_ss3_ast3, 1, 0)*(1+((realisedval_BTC_ss3_ast3 - VaR_BTC_ss3_ast3)^2))
zi3_BTC_ss3_ast3 <- ifelse(realisedval_BTC_ss3_ast3 < VaR_BTC_ss3_ast3, 1, 0)*((realisedval_BTC_ss3_ast3 - VaR_BTC_ss3_ast3)/VaR_BTC_ss3_ast3)

QPS1_BTC_ss3_ast3 <- (2/100)*sum(((zi1_BTC_ss3_ast3 - 0.01)^2))
QPS2_BTC_ss3_ast3 <- (2/100)*sum(((zi2_BTC_ss3_ast3 - 0.01)^2))
QPS3_BTC_ss3_ast3 <- (2/100)*sum(((zi3_BTC_ss3_ast3 - 0.01)^2))

QPS_BTC_ss3_ast3 <- QPS1_BTC_ss3_ast3 + QPS2_BTC_ss3_ast3 + QPS3_BTC_ss3_ast3
#AST1
VaR_BTC_ss3_astone3 <- VaR_BTC_astone3[,1]
realisedval_BTC_ss3_astone3 <- VR34_BTC[,4]
zi1_BTC_ss3_astone3 <- ifelse(realisedval_BTC_ss3_astone3 < VaR_BTC_ss3_astone3, 1, 0)
zi2_BTC_ss3_astone3 <- ifelse(realisedval_BTC_ss3_astone3 < VaR_BTC_ss3_astone3, 1, 0)*(1+((realisedval_BTC_ss3_astone3 - VaR_BTC_ss3_astone3)^2))
zi3_BTC_ss3_astone3 <- ifelse(realisedval_BTC_ss3_astone3 < VaR_BTC_ss3_astone3, 1, 0)*((realisedval_BTC_ss3_astone3 - VaR_BTC_ss3_astone3)/VaR_BTC_ss3_astone3)

QPS1_BTC_ss3_astone3 <- (2/100)*sum(((zi1_BTC_ss3_astone3 - 0.01)^2))
QPS2_BTC_ss3_astone3 <- (2/100)*sum(((zi2_BTC_ss3_astone3 - 0.01)^2))
QPS3_BTC_ss3_astone3 <- (2/100)*sum(((zi3_BTC_ss3_astone3 - 0.01)^2))

QPS_BTC_ss3_astone3 <- QPS1_BTC_ss3_astone3 + QPS2_BTC_ss3_astone3 + QPS3_BTC_ss3_astone3

QPS_BTC_fs_GAS <- c(QPS_BTC_fs_norm0, QPS_BTC_fs_ST0, QPS_BTC_fs_SST0, QPS_BTC_fs_ast0, QPS_BTC_fs_astone0)
QPS_BTC_ss1_GAS <- c(QPS_BTC_ss1_norm1, QPS_BTC_ss1_ST1, QPS_BTC_ss1_SST1, QPS_BTC_ss1_ast1, QPS_BTC_ss1_astone1)
QPS_BTC_ss2_GAS <- c(QPS_BTC_ss2_norm2, QPS_BTC_ss2_ST2, QPS_BTC_ss2_SST2, QPS_BTC_ss2_ast2, QPS_BTC_ss2_astone2)
QPS_BTC_ss3_GAS <- c(QPS_BTC_ss3_norm3, QPS_BTC_ss3_ST3, QPS_BTC_ss3_SST3, QPS_BTC_ss3_ast3, QPS_BTC_ss3_astone3)

QPS_BTC_GAS <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_BTC_GAS <- rbind(QPS_BTC_fs_GAS, QPS_BTC_ss1_GAS, QPS_BTC_ss2_GAS, QPS_BTC_ss3_GAS)
colnames(QPS_BTC_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(QPS_BTC_GAS) <- c("FS", "SS1", "SS2", "SS3")

##### :::::::: E T H :::::::: #####
# Full Sample
#Normal
VaR_ETH_fs_norm0 <- VaR_ETH_norm0[,1]
realisedval_ETH_fs_norm0 <- VR1_ETH[,4]
zi1_ETH_fs_norm0 <- ifelse(realisedval_ETH_fs_norm0 < VaR_ETH_fs_norm0, 1, 0)
zi2_ETH_fs_norm0 <- ifelse(realisedval_ETH_fs_norm0 < VaR_ETH_fs_norm0, 1, 0)*(1+((realisedval_ETH_fs_norm0 - VaR_ETH_fs_norm0)^2))
zi3_ETH_fs_norm0 <- ifelse(realisedval_ETH_fs_norm0 < VaR_ETH_fs_norm0, 1, 0)*((realisedval_ETH_fs_norm0 - VaR_ETH_fs_norm0)/VaR_ETH_fs_norm0)

QPS1_ETH_fs_norm0 <- (2/350)*sum(((zi1_ETH_fs_norm0 - 0.01)^2))
QPS2_ETH_fs_norm0 <- (2/350)*sum(((zi2_ETH_fs_norm0 - 0.01)^2))
QPS3_ETH_fs_norm0 <- (2/350)*sum(((zi3_ETH_fs_norm0 - 0.01)^2))

QPS_ETH_fs_norm0 <- QPS1_ETH_fs_norm0 + QPS2_ETH_fs_norm0 + QPS3_ETH_fs_norm0 
#ST
VaR_ETH_fs_ST0 <- VaR_ETH_std0[,1]
realisedval_ETH_fs_ST0 <- VR2_ETH[,4]
zi1_ETH_fs_ST0 <- ifelse(realisedval_ETH_fs_ST0 < VaR_ETH_fs_ST0, 1, 0)
zi2_ETH_fs_ST0 <- ifelse(realisedval_ETH_fs_ST0 < VaR_ETH_fs_ST0, 1, 0)*(1+((realisedval_ETH_fs_ST0 - VaR_ETH_fs_ST0)^2))
zi3_ETH_fs_ST0 <- ifelse(realisedval_ETH_fs_ST0 < VaR_ETH_fs_ST0, 1, 0)*((realisedval_ETH_fs_ST0 - VaR_ETH_fs_ST0)/VaR_ETH_fs_ST0)

QPS1_ETH_fs_ST0 <- (2/350)*sum(((zi1_ETH_fs_ST0 - 0.01)^2))
QPS2_ETH_fs_ST0 <- (2/350)*sum(((zi2_ETH_fs_ST0 - 0.01)^2))
QPS3_ETH_fs_ST0 <- (2/350)*sum(((zi3_ETH_fs_ST0 - 0.01)^2))

QPS_ETH_fs_ST0 <- QPS1_ETH_fs_ST0 + QPS2_ETH_fs_ST0 + QPS3_ETH_fs_ST0
#SST
VaR_ETH_fs_SST0 <- VaR_ETH_sstd0[,1]
realisedval_ETH_fs_SST0 <- VR3_ETH[,4]
zi1_ETH_fs_SST0 <- ifelse(realisedval_ETH_fs_SST0 < VaR_ETH_fs_SST0, 1, 0)
zi2_ETH_fs_SST0 <- ifelse(realisedval_ETH_fs_SST0 < VaR_ETH_fs_SST0, 1, 0)*(1+((realisedval_ETH_fs_SST0 - VaR_ETH_fs_SST0)^2))
zi3_ETH_fs_SST0 <- ifelse(realisedval_ETH_fs_SST0 < VaR_ETH_fs_SST0, 1, 0)*((realisedval_ETH_fs_SST0 - VaR_ETH_fs_SST0)/VaR_ETH_fs_SST0)

QPS1_ETH_fs_SST0 <- (2/350)*sum(((zi1_ETH_fs_SST0 - 0.01)^2))
QPS2_ETH_fs_SST0 <- (2/350)*sum(((zi2_ETH_fs_SST0 - 0.01)^2))
QPS3_ETH_fs_SST0 <- (2/350)*sum(((zi3_ETH_fs_SST0 - 0.01)^2))

QPS_ETH_fs_SST0 <- QPS1_ETH_fs_SST0 + QPS2_ETH_fs_SST0 + QPS3_ETH_fs_SST0
#AST
VaR_ETH_fs_ast0 <- VaR_ETH_ast0[,1]
realisedval_ETH_fs_ast0 <- VR4_ETH[,4]
zi1_ETH_fs_ast0 <- ifelse(realisedval_ETH_fs_ast0 < VaR_ETH_fs_ast0, 1, 0)
zi2_ETH_fs_ast0 <- ifelse(realisedval_ETH_fs_ast0 < VaR_ETH_fs_ast0, 1, 0)*(1+((realisedval_ETH_fs_ast0 - VaR_ETH_fs_ast0)^2))
zi3_ETH_fs_ast0 <- ifelse(realisedval_ETH_fs_ast0 < VaR_ETH_fs_ast0, 1, 0)*((realisedval_ETH_fs_ast0 - VaR_ETH_fs_ast0)/VaR_ETH_fs_ast0)

QPS1_ETH_fs_ast0 <- (2/350)*sum(((zi1_ETH_fs_ast0 - 0.01)^2))
QPS2_ETH_fs_ast0 <- (2/350)*sum(((zi2_ETH_fs_ast0 - 0.01)^2))
QPS3_ETH_fs_ast0 <- (2/350)*sum(((zi3_ETH_fs_ast0 - 0.01)^2))

QPS_ETH_fs_ast0 <- QPS1_ETH_fs_ast0 + QPS2_ETH_fs_ast0 + QPS3_ETH_fs_ast0
#AST1
VaR_ETH_fs_astone0 <- VaR_ETH_astone0[,1]
realisedval_ETH_fs_astone0 <- VR4_ETH[,4]
zi1_ETH_fs_astone0 <- ifelse(realisedval_ETH_fs_astone0 < VaR_ETH_fs_astone0, 1, 0)
zi2_ETH_fs_astone0 <- ifelse(realisedval_ETH_fs_astone0 < VaR_ETH_fs_astone0, 1, 0)*(1+((realisedval_ETH_fs_astone0 - VaR_ETH_fs_astone0)^2))
zi3_ETH_fs_astone0 <- ifelse(realisedval_ETH_fs_astone0 < VaR_ETH_fs_astone0, 1, 0)*((realisedval_ETH_fs_astone0 - VaR_ETH_fs_astone0)/VaR_ETH_fs_astone0)

QPS1_ETH_fs_astone0 <- (2/350)*sum(((zi1_ETH_fs_astone0 - 0.01)^2))
QPS2_ETH_fs_astone0 <- (2/350)*sum(((zi2_ETH_fs_astone0 - 0.01)^2))
QPS3_ETH_fs_astone0 <- (2/350)*sum(((zi3_ETH_fs_astone0 - 0.01)^2))

QPS_ETH_fs_astone0 <- QPS1_ETH_fs_astone0 + QPS2_ETH_fs_astone0 + QPS3_ETH_fs_astone0

# Sub Sample 1
#Normal
VaR_ETH_ss1_norm1 <- VaR_ETH_norm1[,1]
realisedval_ETH_ss1_norm1 <- VR11_ETH[,4]
zi1_ETH_ss1_norm1 <- ifelse(realisedval_ETH_ss1_norm1 < VaR_ETH_ss1_norm1, 1, 0)
zi2_ETH_ss1_norm1 <- ifelse(realisedval_ETH_ss1_norm1 < VaR_ETH_ss1_norm1, 1, 0)*(1+((realisedval_ETH_ss1_norm1 - VaR_ETH_ss1_norm1)^2))
zi3_ETH_ss1_norm1 <- ifelse(realisedval_ETH_ss1_norm1 < VaR_ETH_ss1_norm1, 1, 0)*((realisedval_ETH_ss1_norm1 - VaR_ETH_ss1_norm1)/VaR_ETH_ss1_norm1)

QPS1_ETH_ss1_norm1 <- (2/60)*sum(((zi1_ETH_ss1_norm1 - 0.01)^2))
QPS2_ETH_ss1_norm1 <- (2/60)*sum(((zi2_ETH_ss1_norm1 - 0.01)^2))
QPS3_ETH_ss1_norm1 <- (2/60)*sum(((zi3_ETH_ss1_norm1 - 0.01)^2))

QPS_ETH_ss1_norm1 <- QPS1_ETH_ss1_norm1 + QPS2_ETH_ss1_norm1 + QPS3_ETH_ss1_norm1 
#ST
VaR_ETH_ss1_ST1 <- VaR_ETH_std1[,1]
realisedval_ETH_ss1_ST1 <- VR12_ETH[,4]
zi1_ETH_ss1_ST1 <- ifelse(realisedval_ETH_ss1_ST1 < VaR_ETH_ss1_ST1, 1, 0)
zi2_ETH_ss1_ST1 <- ifelse(realisedval_ETH_ss1_ST1 < VaR_ETH_ss1_ST1, 1, 0)*(1+((realisedval_ETH_ss1_ST1 - VaR_ETH_ss1_ST1)^2))
zi3_ETH_ss1_ST1 <- ifelse(realisedval_ETH_ss1_ST1 < VaR_ETH_ss1_ST1, 1, 0)*((realisedval_ETH_ss1_ST1 - VaR_ETH_ss1_ST1)/VaR_ETH_ss1_ST1)

QPS1_ETH_ss1_ST1 <- (2/60)*sum(((zi1_ETH_ss1_ST1 - 0.01)^2))
QPS2_ETH_ss1_ST1 <- (2/60)*sum(((zi2_ETH_ss1_ST1 - 0.01)^2))
QPS3_ETH_ss1_ST1 <- (2/60)*sum(((zi3_ETH_ss1_ST1 - 0.01)^2))

QPS_ETH_ss1_ST1 <- QPS1_ETH_ss1_ST1 + QPS2_ETH_ss1_ST1 + QPS3_ETH_ss1_ST1
#SST
VaR_ETH_ss1_SST1 <- VaR_ETH_sstd1[,1]
realisedval_ETH_ss1_SST1 <- VR13_ETH[,4]
zi1_ETH_ss1_SST1 <- ifelse(realisedval_ETH_ss1_SST1 < VaR_ETH_ss1_SST1, 1, 0)
zi2_ETH_ss1_SST1 <- ifelse(realisedval_ETH_ss1_SST1 < VaR_ETH_ss1_SST1, 1, 0)*(1+((realisedval_ETH_ss1_SST1 - VaR_ETH_ss1_SST1)^2))
zi3_ETH_ss1_SST1 <- ifelse(realisedval_ETH_ss1_SST1 < VaR_ETH_ss1_SST1, 1, 0)*((realisedval_ETH_ss1_SST1 - VaR_ETH_ss1_SST1)/VaR_ETH_ss1_SST1)

QPS1_ETH_ss1_SST1 <- (2/60)*sum(((zi1_ETH_ss1_SST1 - 0.01)^2))
QPS2_ETH_ss1_SST1 <- (2/60)*sum(((zi2_ETH_ss1_SST1 - 0.01)^2))
QPS3_ETH_ss1_SST1 <- (2/60)*sum(((zi3_ETH_ss1_SST1 - 0.01)^2))

QPS_ETH_ss1_SST1 <- QPS1_ETH_ss1_SST1 + QPS2_ETH_ss1_SST1 + QPS3_ETH_ss1_SST1
#AST
VaR_ETH_ss1_ast1 <- VaR_ETH_ast1[,1]
realisedval_ETH_ss1_ast1 <- VR14_ETH[,4]
zi1_ETH_ss1_ast1 <- ifelse(realisedval_ETH_ss1_ast1 < VaR_ETH_ss1_ast1, 1, 0)
zi2_ETH_ss1_ast1 <- ifelse(realisedval_ETH_ss1_ast1 < VaR_ETH_ss1_ast1, 1, 0)*(1+((realisedval_ETH_ss1_ast1 - VaR_ETH_ss1_ast1)^2))
zi3_ETH_ss1_ast1 <- ifelse(realisedval_ETH_ss1_ast1 < VaR_ETH_ss1_ast1, 1, 0)*((realisedval_ETH_ss1_ast1 - VaR_ETH_ss1_ast1)/VaR_ETH_ss1_ast1)

QPS1_ETH_ss1_ast1 <- (2/60)*sum(((zi1_ETH_ss1_ast1 - 0.01)^2))
QPS2_ETH_ss1_ast1 <- (2/60)*sum(((zi2_ETH_ss1_ast1 - 0.01)^2))
QPS3_ETH_ss1_ast1 <- (2/60)*sum(((zi3_ETH_ss1_ast1 - 0.01)^2))

QPS_ETH_ss1_ast1 <- QPS1_ETH_ss1_ast1 + QPS2_ETH_ss1_ast1 + QPS3_ETH_ss1_ast1
#AST1
VaR_ETH_ss1_astone1 <- VaR_ETH_astone1[,1]
realisedval_ETH_ss1_astone1 <- VR14_ETH[,4]
zi1_ETH_ss1_astone1 <- ifelse(realisedval_ETH_ss1_astone1 < VaR_ETH_ss1_astone1, 1, 0)
zi2_ETH_ss1_astone1 <- ifelse(realisedval_ETH_ss1_astone1 < VaR_ETH_ss1_astone1, 1, 0)*(1+((realisedval_ETH_ss1_astone1 - VaR_ETH_ss1_astone1)^2))
zi3_ETH_ss1_astone1 <- ifelse(realisedval_ETH_ss1_astone1 < VaR_ETH_ss1_astone1, 1, 0)*((realisedval_ETH_ss1_astone1 - VaR_ETH_ss1_astone1)/VaR_ETH_ss1_astone1)

QPS1_ETH_ss1_astone1 <- (2/60)*sum(((zi1_ETH_ss1_astone1 - 0.01)^2))
QPS2_ETH_ss1_astone1 <- (2/60)*sum(((zi2_ETH_ss1_astone1 - 0.01)^2))
QPS3_ETH_ss1_astone1 <- (2/60)*sum(((zi3_ETH_ss1_astone1 - 0.01)^2))

QPS_ETH_ss1_astone1 <- QPS1_ETH_ss1_astone1 + QPS2_ETH_ss1_astone1 + QPS3_ETH_ss1_astone1

# Sub Sample 2
#Normal
VaR_ETH_ss2_norm2 <- VaR_ETH_norm2[,1]
realisedval_ETH_ss2_norm2 <- VR21_ETH[,4]
zi1_ETH_ss2_norm2 <- ifelse(realisedval_ETH_ss2_norm2 < VaR_ETH_ss2_norm2, 1, 0)
zi2_ETH_ss2_norm2 <- ifelse(realisedval_ETH_ss2_norm2 < VaR_ETH_ss2_norm2, 1, 0)*(1+((realisedval_ETH_ss2_norm2 - VaR_ETH_ss2_norm2)^2))
zi3_ETH_ss2_norm2 <- ifelse(realisedval_ETH_ss2_norm2 < VaR_ETH_ss2_norm2, 1, 0)*((realisedval_ETH_ss2_norm2 - VaR_ETH_ss2_norm2)/VaR_ETH_ss2_norm2)

QPS1_ETH_ss2_norm2 <- (2/200)*sum(((zi1_ETH_ss2_norm2 - 0.01)^2))
QPS2_ETH_ss2_norm2 <- (2/200)*sum(((zi2_ETH_ss2_norm2 - 0.01)^2))
QPS3_ETH_ss2_norm2 <- (2/200)*sum(((zi3_ETH_ss2_norm2 - 0.01)^2))

QPS_ETH_ss2_norm2 <- QPS1_ETH_ss2_norm2 + QPS2_ETH_ss2_norm2 + QPS3_ETH_ss2_norm2 
#ST
VaR_ETH_ss2_ST2 <- VaR_ETH_std2[,1]
realisedval_ETH_ss2_ST2 <- VR22_ETH[,4]
zi1_ETH_ss2_ST2 <- ifelse(realisedval_ETH_ss2_ST2 < VaR_ETH_ss2_ST2, 1, 0)
zi2_ETH_ss2_ST2 <- ifelse(realisedval_ETH_ss2_ST2 < VaR_ETH_ss2_ST2, 1, 0)*(1+((realisedval_ETH_ss2_ST2 - VaR_ETH_ss2_ST2)^2))
zi3_ETH_ss2_ST2 <- ifelse(realisedval_ETH_ss2_ST2 < VaR_ETH_ss2_ST2, 1, 0)*((realisedval_ETH_ss2_ST2 - VaR_ETH_ss2_ST2)/VaR_ETH_ss2_ST2)

QPS1_ETH_ss2_ST2 <- (2/200)*sum(((zi1_ETH_ss2_ST2 - 0.01)^2))
QPS2_ETH_ss2_ST2 <- (2/200)*sum(((zi2_ETH_ss2_ST2 - 0.01)^2))
QPS3_ETH_ss2_ST2 <- (2/200)*sum(((zi3_ETH_ss2_ST2 - 0.01)^2))

QPS_ETH_ss2_ST2 <- QPS1_ETH_ss2_ST2 + QPS2_ETH_ss2_ST2 + QPS3_ETH_ss2_ST2
#SST
VaR_ETH_ss2_SST2 <- VaR_ETH_sstd2[,1]
realisedval_ETH_ss2_SST2 <- VR23_ETH[,4]
zi1_ETH_ss2_SST2 <- ifelse(realisedval_ETH_ss2_SST2 < VaR_ETH_ss2_SST2, 1, 0)
zi2_ETH_ss2_SST2 <- ifelse(realisedval_ETH_ss2_SST2 < VaR_ETH_ss2_SST2, 1, 0)*(1+((realisedval_ETH_ss2_SST2 - VaR_ETH_ss2_SST2)^2))
zi3_ETH_ss2_SST2 <- ifelse(realisedval_ETH_ss2_SST2 < VaR_ETH_ss2_SST2, 1, 0)*((realisedval_ETH_ss2_SST2 - VaR_ETH_ss2_SST2)/VaR_ETH_ss2_SST2)

QPS1_ETH_ss2_SST2 <- (2/200)*sum(((zi1_ETH_ss2_SST2 - 0.01)^2))
QPS2_ETH_ss2_SST2 <- (2/200)*sum(((zi2_ETH_ss2_SST2 - 0.01)^2))
QPS3_ETH_ss2_SST2 <- (2/200)*sum(((zi3_ETH_ss2_SST2 - 0.01)^2))

QPS_ETH_ss2_SST2 <- QPS1_ETH_ss2_SST2 + QPS2_ETH_ss2_SST2 + QPS3_ETH_ss2_SST2
#AST
VaR_ETH_ss2_ast2 <- VaR_ETH_ast2[,1]
realisedval_ETH_ss2_ast2 <- VR24_ETH[,4]
zi1_ETH_ss2_ast2 <- ifelse(realisedval_ETH_ss2_ast2 < VaR_ETH_ss2_ast2, 1, 0)
zi2_ETH_ss2_ast2 <- ifelse(realisedval_ETH_ss2_ast2 < VaR_ETH_ss2_ast2, 1, 0)*(1+((realisedval_ETH_ss2_ast2 - VaR_ETH_ss2_ast2)^2))
zi3_ETH_ss2_ast2 <- ifelse(realisedval_ETH_ss2_ast2 < VaR_ETH_ss2_ast2, 1, 0)*((realisedval_ETH_ss2_ast2 - VaR_ETH_ss2_ast2)/VaR_ETH_ss2_ast2)

QPS1_ETH_ss2_ast2 <- (2/200)*sum(((zi1_ETH_ss2_ast2 - 0.01)^2))
QPS2_ETH_ss2_ast2 <- (2/200)*sum(((zi2_ETH_ss2_ast2 - 0.01)^2))
QPS3_ETH_ss2_ast2 <- (2/200)*sum(((zi3_ETH_ss2_ast2 - 0.01)^2))

QPS_ETH_ss2_ast2 <- QPS1_ETH_ss2_ast2 + QPS2_ETH_ss2_ast2 + QPS3_ETH_ss2_ast2
#AST1
VaR_ETH_ss2_astone2 <- VaR_ETH_astone2[,1]
realisedval_ETH_ss2_astone2 <- VR24_ETH[,4]
zi1_ETH_ss2_astone2 <- ifelse(realisedval_ETH_ss2_astone2 < VaR_ETH_ss2_astone2, 1, 0)
zi2_ETH_ss2_astone2 <- ifelse(realisedval_ETH_ss2_astone2 < VaR_ETH_ss2_astone2, 1, 0)*(1+((realisedval_ETH_ss2_astone2 - VaR_ETH_ss2_astone2)^2))
zi3_ETH_ss2_astone2 <- ifelse(realisedval_ETH_ss2_astone2 < VaR_ETH_ss2_astone2, 1, 0)*((realisedval_ETH_ss2_astone2 - VaR_ETH_ss2_astone2)/VaR_ETH_ss2_astone2)

QPS1_ETH_ss2_astone2 <- (2/200)*sum(((zi1_ETH_ss2_astone2 - 0.01)^2))
QPS2_ETH_ss2_astone2 <- (2/200)*sum(((zi2_ETH_ss2_astone2 - 0.01)^2))
QPS3_ETH_ss2_astone2 <- (2/200)*sum(((zi3_ETH_ss2_astone2 - 0.01)^2))

QPS_ETH_ss2_astone2 <- QPS1_ETH_ss2_astone2 + QPS2_ETH_ss2_astone2 + QPS3_ETH_ss2_astone2

# Sub Sample 3
#Normal
VaR_ETH_ss3_norm3 <- VaR_ETH_norm3[,1]
realisedval_ETH_ss3_norm3 <- VR31_ETH[,4]
zi1_ETH_ss3_norm3 <- ifelse(realisedval_ETH_ss3_norm3 < VaR_ETH_ss3_norm3, 1, 0)
zi2_ETH_ss3_norm3 <- ifelse(realisedval_ETH_ss3_norm3 < VaR_ETH_ss3_norm3, 1, 0)*(1+((realisedval_ETH_ss3_norm3 - VaR_ETH_ss3_norm3)^2))
zi3_ETH_ss3_norm3 <- ifelse(realisedval_ETH_ss3_norm3 < VaR_ETH_ss3_norm3, 1, 0)*((realisedval_ETH_ss3_norm3 - VaR_ETH_ss3_norm3)/VaR_ETH_ss3_norm3)

QPS1_ETH_ss3_norm3 <- (2/100)*sum(((zi1_ETH_ss3_norm3 - 0.01)^2))
QPS2_ETH_ss3_norm3 <- (2/100)*sum(((zi2_ETH_ss3_norm3 - 0.01)^2))
QPS3_ETH_ss3_norm3 <- (2/100)*sum(((zi3_ETH_ss3_norm3 - 0.01)^2))

QPS_ETH_ss3_norm3 <- QPS1_ETH_ss3_norm3 + QPS2_ETH_ss3_norm3 + QPS3_ETH_ss3_norm3 
#ST
VaR_ETH_ss3_ST3 <- VaR_ETH_std3[,1]
realisedval_ETH_ss3_ST3 <- VR32_ETH[,4]
zi1_ETH_ss3_ST3 <- ifelse(realisedval_ETH_ss3_ST3 < VaR_ETH_ss3_ST3, 1, 0)
zi2_ETH_ss3_ST3 <- ifelse(realisedval_ETH_ss3_ST3 < VaR_ETH_ss3_ST3, 1, 0)*(1+((realisedval_ETH_ss3_ST3 - VaR_ETH_ss3_ST3)^2))
zi3_ETH_ss3_ST3 <- ifelse(realisedval_ETH_ss3_ST3 < VaR_ETH_ss3_ST3, 1, 0)*((realisedval_ETH_ss3_ST3 - VaR_ETH_ss3_ST3)/VaR_ETH_ss3_ST3)

QPS1_ETH_ss3_ST3 <- (2/100)*sum(((zi1_ETH_ss3_ST3 - 0.01)^2))
QPS2_ETH_ss3_ST3 <- (2/100)*sum(((zi2_ETH_ss3_ST3 - 0.01)^2))
QPS3_ETH_ss3_ST3 <- (2/100)*sum(((zi3_ETH_ss3_ST3 - 0.01)^2))

QPS_ETH_ss3_ST3 <- QPS1_ETH_ss3_ST3 + QPS2_ETH_ss3_ST3 + QPS3_ETH_ss3_ST3
#SST
VaR_ETH_ss3_SST3 <- VaR_ETH_sstd3[,1]
realisedval_ETH_ss3_SST3 <- VR33_ETH[,4]
zi1_ETH_ss3_SST3 <- ifelse(realisedval_ETH_ss3_SST3 < VaR_ETH_ss3_SST3, 1, 0)
zi2_ETH_ss3_SST3 <- ifelse(realisedval_ETH_ss3_SST3 < VaR_ETH_ss3_SST3, 1, 0)*(1+((realisedval_ETH_ss3_SST3 - VaR_ETH_ss3_SST3)^2))
zi3_ETH_ss3_SST3 <- ifelse(realisedval_ETH_ss3_SST3 < VaR_ETH_ss3_SST3, 1, 0)*((realisedval_ETH_ss3_SST3 - VaR_ETH_ss3_SST3)/VaR_ETH_ss3_SST3)

QPS1_ETH_ss3_SST3 <- (2/100)*sum(((zi1_ETH_ss3_SST3 - 0.01)^2))
QPS2_ETH_ss3_SST3 <- (2/100)*sum(((zi2_ETH_ss3_SST3 - 0.01)^2))
QPS3_ETH_ss3_SST3 <- (2/100)*sum(((zi3_ETH_ss3_SST3 - 0.01)^2))

QPS_ETH_ss3_SST3 <- QPS1_ETH_ss3_SST3 + QPS2_ETH_ss3_SST3 + QPS3_ETH_ss3_SST3
#AST
VaR_ETH_ss3_ast3 <- VaR_ETH_ast3[,1]
realisedval_ETH_ss3_ast3 <- VR34_ETH[,4]
zi1_ETH_ss3_ast3 <- ifelse(realisedval_ETH_ss3_ast3 < VaR_ETH_ss3_ast3, 1, 0)
zi2_ETH_ss3_ast3 <- ifelse(realisedval_ETH_ss3_ast3 < VaR_ETH_ss3_ast3, 1, 0)*(1+((realisedval_ETH_ss3_ast3 - VaR_ETH_ss3_ast3)^2))
zi3_ETH_ss3_ast3 <- ifelse(realisedval_ETH_ss3_ast3 < VaR_ETH_ss3_ast3, 1, 0)*((realisedval_ETH_ss3_ast3 - VaR_ETH_ss3_ast3)/VaR_ETH_ss3_ast3)

QPS1_ETH_ss3_ast3 <- (2/100)*sum(((zi1_ETH_ss3_ast3 - 0.01)^2))
QPS2_ETH_ss3_ast3 <- (2/100)*sum(((zi2_ETH_ss3_ast3 - 0.01)^2))
QPS3_ETH_ss3_ast3 <- (2/100)*sum(((zi3_ETH_ss3_ast3 - 0.01)^2))

QPS_ETH_ss3_ast3 <- QPS1_ETH_ss3_ast3 + QPS2_ETH_ss3_ast3 + QPS3_ETH_ss3_ast3
#AST1
VaR_ETH_ss3_astone3 <- VaR_ETH_astone3[,1]
realisedval_ETH_ss3_astone3 <- VR34_ETH[,4]
zi1_ETH_ss3_astone3 <- ifelse(realisedval_ETH_ss3_astone3 < VaR_ETH_ss3_astone3, 1, 0)
zi2_ETH_ss3_astone3 <- ifelse(realisedval_ETH_ss3_astone3 < VaR_ETH_ss3_astone3, 1, 0)*(1+((realisedval_ETH_ss3_astone3 - VaR_ETH_ss3_astone3)^2))
zi3_ETH_ss3_astone3 <- ifelse(realisedval_ETH_ss3_astone3 < VaR_ETH_ss3_astone3, 1, 0)*((realisedval_ETH_ss3_astone3 - VaR_ETH_ss3_astone3)/VaR_ETH_ss3_astone3)

QPS1_ETH_ss3_astone3 <- (2/100)*sum(((zi1_ETH_ss3_astone3 - 0.01)^2))
QPS2_ETH_ss3_astone3 <- (2/100)*sum(((zi2_ETH_ss3_astone3 - 0.01)^2))
QPS3_ETH_ss3_astone3 <- (2/100)*sum(((zi3_ETH_ss3_astone3 - 0.01)^2))

QPS_ETH_ss3_astone3 <- QPS1_ETH_ss3_astone3 + QPS2_ETH_ss3_astone3 + QPS3_ETH_ss3_astone3

QPS_ETH_fs_GAS <- c(QPS_ETH_fs_norm0, QPS_ETH_fs_ST0, QPS_ETH_fs_SST0, QPS_ETH_fs_ast0, QPS_ETH_fs_astone0)
QPS_ETH_ss1_GAS <- c(QPS_ETH_ss1_norm1, QPS_ETH_ss1_ST1, QPS_ETH_ss1_SST1, QPS_ETH_ss1_ast1, QPS_ETH_ss1_astone1)
QPS_ETH_ss2_GAS <- c(QPS_ETH_ss2_norm2, QPS_ETH_ss2_ST2, QPS_ETH_ss2_SST2, QPS_ETH_ss2_ast2, QPS_ETH_ss2_astone2)
QPS_ETH_ss3_GAS <- c(QPS_ETH_ss3_norm3, QPS_ETH_ss3_ST3, QPS_ETH_ss3_SST3, QPS_ETH_ss3_ast3, QPS_ETH_ss3_astone3)

QPS_ETH_GAS <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_ETH_GAS <- rbind(QPS_ETH_fs_GAS, QPS_ETH_ss1_GAS, QPS_ETH_ss2_GAS, QPS_ETH_ss3_GAS)
colnames(QPS_ETH_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(QPS_ETH_GAS) <- c("Full Sample", "SS1", "SS2", "SS3")

##### :::::::: L T C :::::::: #####
# Full Sample
#Normal
VaR_LTC_fs_norm0 <- VaR_LTC_norm0[,1]
realisedval_LTC_fs_norm0 <- VR1_LTC[,4]
zi1_LTC_fs_norm0 <- ifelse(realisedval_LTC_fs_norm0 < VaR_LTC_fs_norm0, 1, 0)
zi2_LTC_fs_norm0 <- ifelse(realisedval_LTC_fs_norm0 < VaR_LTC_fs_norm0, 1, 0)*(1+((realisedval_LTC_fs_norm0 - VaR_LTC_fs_norm0)^2))
zi3_LTC_fs_norm0 <- ifelse(realisedval_LTC_fs_norm0 < VaR_LTC_fs_norm0, 1, 0)*((realisedval_LTC_fs_norm0 - VaR_LTC_fs_norm0)/VaR_LTC_fs_norm0)

QPS1_LTC_fs_norm0 <- (2/350)*sum(((zi1_LTC_fs_norm0 - 0.01)^2))
QPS2_LTC_fs_norm0 <- (2/350)*sum(((zi2_LTC_fs_norm0 - 0.01)^2))
QPS3_LTC_fs_norm0 <- (2/350)*sum(((zi3_LTC_fs_norm0 - 0.01)^2))

QPS_LTC_fs_norm0 <- QPS1_LTC_fs_norm0 + QPS2_LTC_fs_norm0 + QPS3_LTC_fs_norm0 
#ST
VaR_LTC_fs_ST0 <- VaR_LTC_std0[,1]
realisedval_LTC_fs_ST0 <- VR2_LTC[,4]
zi1_LTC_fs_ST0 <- ifelse(realisedval_LTC_fs_ST0 < VaR_LTC_fs_ST0, 1, 0)
zi2_LTC_fs_ST0 <- ifelse(realisedval_LTC_fs_ST0 < VaR_LTC_fs_ST0, 1, 0)*(1+((realisedval_LTC_fs_ST0 - VaR_LTC_fs_ST0)^2))
zi3_LTC_fs_ST0 <- ifelse(realisedval_LTC_fs_ST0 < VaR_LTC_fs_ST0, 1, 0)*((realisedval_LTC_fs_ST0 - VaR_LTC_fs_ST0)/VaR_LTC_fs_ST0)

QPS1_LTC_fs_ST0 <- (2/350)*sum(((zi1_LTC_fs_ST0 - 0.01)^2))
QPS2_LTC_fs_ST0 <- (2/350)*sum(((zi2_LTC_fs_ST0 - 0.01)^2))
QPS3_LTC_fs_ST0 <- (2/350)*sum(((zi3_LTC_fs_ST0 - 0.01)^2))

QPS_LTC_fs_ST0 <- QPS1_LTC_fs_ST0 + QPS2_LTC_fs_ST0 + QPS3_LTC_fs_ST0
#SST
VaR_LTC_fs_SST0 <- VaR_LTC_sstd0[,1]
realisedval_LTC_fs_SST0 <- VR3_LTC[,4]
zi1_LTC_fs_SST0 <- ifelse(realisedval_LTC_fs_SST0 < VaR_LTC_fs_SST0, 1, 0)
zi2_LTC_fs_SST0 <- ifelse(realisedval_LTC_fs_SST0 < VaR_LTC_fs_SST0, 1, 0)*(1+((realisedval_LTC_fs_SST0 - VaR_LTC_fs_SST0)^2))
zi3_LTC_fs_SST0 <- ifelse(realisedval_LTC_fs_SST0 < VaR_LTC_fs_SST0, 1, 0)*((realisedval_LTC_fs_SST0 - VaR_LTC_fs_SST0)/VaR_LTC_fs_SST0)

QPS1_LTC_fs_SST0 <- (2/350)*sum(((zi1_LTC_fs_SST0 - 0.01)^2))
QPS2_LTC_fs_SST0 <- (2/350)*sum(((zi2_LTC_fs_SST0 - 0.01)^2))
QPS3_LTC_fs_SST0 <- (2/350)*sum(((zi3_LTC_fs_SST0 - 0.01)^2))

QPS_LTC_fs_SST0 <- QPS1_LTC_fs_SST0 + QPS2_LTC_fs_SST0 + QPS3_LTC_fs_SST0
#AST
VaR_LTC_fs_ast0 <- VaR_LTC_ast0[,1]
realisedval_LTC_fs_ast0 <- VR4_LTC[,4]
zi1_LTC_fs_ast0 <- ifelse(realisedval_LTC_fs_ast0 < VaR_LTC_fs_ast0, 1, 0)
zi2_LTC_fs_ast0 <- ifelse(realisedval_LTC_fs_ast0 < VaR_LTC_fs_ast0, 1, 0)*(1+((realisedval_LTC_fs_ast0 - VaR_LTC_fs_ast0)^2))
zi3_LTC_fs_ast0 <- ifelse(realisedval_LTC_fs_ast0 < VaR_LTC_fs_ast0, 1, 0)*((realisedval_LTC_fs_ast0 - VaR_LTC_fs_ast0)/VaR_LTC_fs_ast0)

QPS1_LTC_fs_ast0 <- (2/350)*sum(((zi1_LTC_fs_ast0 - 0.01)^2))
QPS2_LTC_fs_ast0 <- (2/350)*sum(((zi2_LTC_fs_ast0 - 0.01)^2))
QPS3_LTC_fs_ast0 <- (2/350)*sum(((zi3_LTC_fs_ast0 - 0.01)^2))

QPS_LTC_fs_ast0 <- QPS1_LTC_fs_ast0 + QPS2_LTC_fs_ast0 + QPS3_LTC_fs_ast0
#AST1
VaR_LTC_fs_astone0 <- VaR_LTC_astone0[,1]
realisedval_LTC_fs_astone0 <- VR4_LTC[,4]
zi1_LTC_fs_astone0 <- ifelse(realisedval_LTC_fs_astone0 < VaR_LTC_fs_astone0, 1, 0)
zi2_LTC_fs_astone0 <- ifelse(realisedval_LTC_fs_astone0 < VaR_LTC_fs_astone0, 1, 0)*(1+((realisedval_LTC_fs_astone0 - VaR_LTC_fs_astone0)^2))
zi3_LTC_fs_astone0 <- ifelse(realisedval_LTC_fs_astone0 < VaR_LTC_fs_astone0, 1, 0)*((realisedval_LTC_fs_astone0 - VaR_LTC_fs_astone0)/VaR_LTC_fs_astone0)

QPS1_LTC_fs_astone0 <- (2/350)*sum(((zi1_LTC_fs_astone0 - 0.01)^2))
QPS2_LTC_fs_astone0 <- (2/350)*sum(((zi2_LTC_fs_astone0 - 0.01)^2))
QPS3_LTC_fs_astone0 <- (2/350)*sum(((zi3_LTC_fs_astone0 - 0.01)^2))

QPS_LTC_fs_astone0 <- QPS1_LTC_fs_astone0 + QPS2_LTC_fs_astone0 + QPS3_LTC_fs_astone0

# Sub Sample 1
#Normal
VaR_LTC_ss1_norm1 <- VaR_LTC_norm1[,1]
realisedval_LTC_ss1_norm1 <- VR11_LTC[,4]
zi1_LTC_ss1_norm1 <- ifelse(realisedval_LTC_ss1_norm1 < VaR_LTC_ss1_norm1, 1, 0)
zi2_LTC_ss1_norm1 <- ifelse(realisedval_LTC_ss1_norm1 < VaR_LTC_ss1_norm1, 1, 0)*(1+((realisedval_LTC_ss1_norm1 - VaR_LTC_ss1_norm1)^2))
zi3_LTC_ss1_norm1 <- ifelse(realisedval_LTC_ss1_norm1 < VaR_LTC_ss1_norm1, 1, 0)*((realisedval_LTC_ss1_norm1 - VaR_LTC_ss1_norm1)/VaR_LTC_ss1_norm1)

QPS1_LTC_ss1_norm1 <- (2/60)*sum(((zi1_LTC_ss1_norm1 - 0.01)^2))
QPS2_LTC_ss1_norm1 <- (2/60)*sum(((zi2_LTC_ss1_norm1 - 0.01)^2))
QPS3_LTC_ss1_norm1 <- (2/60)*sum(((zi3_LTC_ss1_norm1 - 0.01)^2))

QPS_LTC_ss1_norm1 <- QPS1_LTC_ss1_norm1 + QPS2_LTC_ss1_norm1 + QPS3_LTC_ss1_norm1 
#ST
VaR_LTC_ss1_ST1 <- VaR_LTC_std1[,1]
realisedval_LTC_ss1_ST1 <- VR12_LTC[,4]
zi1_LTC_ss1_ST1 <- ifelse(realisedval_LTC_ss1_ST1 < VaR_LTC_ss1_ST1, 1, 0)
zi2_LTC_ss1_ST1 <- ifelse(realisedval_LTC_ss1_ST1 < VaR_LTC_ss1_ST1, 1, 0)*(1+((realisedval_LTC_ss1_ST1 - VaR_LTC_ss1_ST1)^2))
zi3_LTC_ss1_ST1 <- ifelse(realisedval_LTC_ss1_ST1 < VaR_LTC_ss1_ST1, 1, 0)*((realisedval_LTC_ss1_ST1 - VaR_LTC_ss1_ST1)/VaR_LTC_ss1_ST1)

QPS1_LTC_ss1_ST1 <- (2/60)*sum(((zi1_LTC_ss1_ST1 - 0.01)^2))
QPS2_LTC_ss1_ST1 <- (2/60)*sum(((zi2_LTC_ss1_ST1 - 0.01)^2))
QPS3_LTC_ss1_ST1 <- (2/60)*sum(((zi3_LTC_ss1_ST1 - 0.01)^2))

QPS_LTC_ss1_ST1 <- QPS1_LTC_ss1_ST1 + QPS2_LTC_ss1_ST1 + QPS3_LTC_ss1_ST1
#SST
VaR_LTC_ss1_SST1 <- VaR_LTC_sstd1[,1]
realisedval_LTC_ss1_SST1 <- VR13_LTC[,4]
zi1_LTC_ss1_SST1 <- ifelse(realisedval_LTC_ss1_SST1 < VaR_LTC_ss1_SST1, 1, 0)
zi2_LTC_ss1_SST1 <- ifelse(realisedval_LTC_ss1_SST1 < VaR_LTC_ss1_SST1, 1, 0)*(1+((realisedval_LTC_ss1_SST1 - VaR_LTC_ss1_SST1)^2))
zi3_LTC_ss1_SST1 <- ifelse(realisedval_LTC_ss1_SST1 < VaR_LTC_ss1_SST1, 1, 0)*((realisedval_LTC_ss1_SST1 - VaR_LTC_ss1_SST1)/VaR_LTC_ss1_SST1)

QPS1_LTC_ss1_SST1 <- (2/60)*sum(((zi1_LTC_ss1_SST1 - 0.01)^2))
QPS2_LTC_ss1_SST1 <- (2/60)*sum(((zi2_LTC_ss1_SST1 - 0.01)^2))
QPS3_LTC_ss1_SST1 <- (2/60)*sum(((zi3_LTC_ss1_SST1 - 0.01)^2))

QPS_LTC_ss1_SST1 <- QPS1_LTC_ss1_SST1 + QPS2_LTC_ss1_SST1 + QPS3_LTC_ss1_SST1
#AST
VaR_LTC_ss1_ast1 <- VaR_LTC_ast1[,1]
realisedval_LTC_ss1_ast1 <- VR14_LTC[,4]
zi1_LTC_ss1_ast1 <- ifelse(realisedval_LTC_ss1_ast1 < VaR_LTC_ss1_ast1, 1, 0)
zi2_LTC_ss1_ast1 <- ifelse(realisedval_LTC_ss1_ast1 < VaR_LTC_ss1_ast1, 1, 0)*(1+((realisedval_LTC_ss1_ast1 - VaR_LTC_ss1_ast1)^2))
zi3_LTC_ss1_ast1 <- ifelse(realisedval_LTC_ss1_ast1 < VaR_LTC_ss1_ast1, 1, 0)*((realisedval_LTC_ss1_ast1 - VaR_LTC_ss1_ast1)/VaR_LTC_ss1_ast1)

QPS1_LTC_ss1_ast1 <- (2/60)*sum(((zi1_LTC_ss1_ast1 - 0.01)^2))
QPS2_LTC_ss1_ast1 <- (2/60)*sum(((zi2_LTC_ss1_ast1 - 0.01)^2))
QPS3_LTC_ss1_ast1 <- (2/60)*sum(((zi3_LTC_ss1_ast1 - 0.01)^2))

QPS_LTC_ss1_ast1 <- QPS1_LTC_ss1_ast1 + QPS2_LTC_ss1_ast1 + QPS3_LTC_ss1_ast1
#AST1
VaR_LTC_ss1_astone1 <- VaR_LTC_astone1[,1]
realisedval_LTC_ss1_astone1 <- VR14_LTC[,4]
zi1_LTC_ss1_astone1 <- ifelse(realisedval_LTC_ss1_astone1 < VaR_LTC_ss1_astone1, 1, 0)
zi2_LTC_ss1_astone1 <- ifelse(realisedval_LTC_ss1_astone1 < VaR_LTC_ss1_astone1, 1, 0)*(1+((realisedval_LTC_ss1_astone1 - VaR_LTC_ss1_astone1)^2))
zi3_LTC_ss1_astone1 <- ifelse(realisedval_LTC_ss1_astone1 < VaR_LTC_ss1_astone1, 1, 0)*((realisedval_LTC_ss1_astone1 - VaR_LTC_ss1_astone1)/VaR_LTC_ss1_astone1)

QPS1_LTC_ss1_astone1 <- (2/60)*sum(((zi1_LTC_ss1_astone1 - 0.01)^2))
QPS2_LTC_ss1_astone1 <- (2/60)*sum(((zi2_LTC_ss1_astone1 - 0.01)^2))
QPS3_LTC_ss1_astone1 <- (2/60)*sum(((zi3_LTC_ss1_astone1 - 0.01)^2))

QPS_LTC_ss1_astone1 <- QPS1_LTC_ss1_astone1 + QPS2_LTC_ss1_astone1 + QPS3_LTC_ss1_astone1

# Sub Sample 2
#Normal
VaR_LTC_ss2_norm2 <- VaR_LTC_norm2[,1]
realisedval_LTC_ss2_norm2 <- VR21_LTC[,4]
zi1_LTC_ss2_norm2 <- ifelse(realisedval_LTC_ss2_norm2 < VaR_LTC_ss2_norm2, 1, 0)
zi2_LTC_ss2_norm2 <- ifelse(realisedval_LTC_ss2_norm2 < VaR_LTC_ss2_norm2, 1, 0)*(1+((realisedval_LTC_ss2_norm2 - VaR_LTC_ss2_norm2)^2))
zi3_LTC_ss2_norm2 <- ifelse(realisedval_LTC_ss2_norm2 < VaR_LTC_ss2_norm2, 1, 0)*((realisedval_LTC_ss2_norm2 - VaR_LTC_ss2_norm2)/VaR_LTC_ss2_norm2)

QPS1_LTC_ss2_norm2 <- (2/200)*sum(((zi1_LTC_ss2_norm2 - 0.01)^2))
QPS2_LTC_ss2_norm2 <- (2/200)*sum(((zi2_LTC_ss2_norm2 - 0.01)^2))
QPS3_LTC_ss2_norm2 <- (2/200)*sum(((zi3_LTC_ss2_norm2 - 0.01)^2))

QPS_LTC_ss2_norm2 <- QPS1_LTC_ss2_norm2 + QPS2_LTC_ss2_norm2 + QPS3_LTC_ss2_norm2 
#ST
VaR_LTC_ss2_ST2 <- VaR_LTC_std2[,1]
realisedval_LTC_ss2_ST2 <- VR22_LTC[,4]
zi1_LTC_ss2_ST2 <- ifelse(realisedval_LTC_ss2_ST2 < VaR_LTC_ss2_ST2, 1, 0)
zi2_LTC_ss2_ST2 <- ifelse(realisedval_LTC_ss2_ST2 < VaR_LTC_ss2_ST2, 1, 0)*(1+((realisedval_LTC_ss2_ST2 - VaR_LTC_ss2_ST2)^2))
zi3_LTC_ss2_ST2 <- ifelse(realisedval_LTC_ss2_ST2 < VaR_LTC_ss2_ST2, 1, 0)*((realisedval_LTC_ss2_ST2 - VaR_LTC_ss2_ST2)/VaR_LTC_ss2_ST2)

QPS1_LTC_ss2_ST2 <- (2/200)*sum(((zi1_LTC_ss2_ST2 - 0.01)^2))
QPS2_LTC_ss2_ST2 <- (2/200)*sum(((zi2_LTC_ss2_ST2 - 0.01)^2))
QPS3_LTC_ss2_ST2 <- (2/200)*sum(((zi3_LTC_ss2_ST2 - 0.01)^2))

QPS_LTC_ss2_ST2 <- QPS1_LTC_ss2_ST2 + QPS2_LTC_ss2_ST2 + QPS3_LTC_ss2_ST2
#SST
VaR_LTC_ss2_SST2 <- VaR_LTC_sstd2[,1]
realisedval_LTC_ss2_SST2 <- VR23_LTC[,4]
zi1_LTC_ss2_SST2 <- ifelse(realisedval_LTC_ss2_SST2 < VaR_LTC_ss2_SST2, 1, 0)
zi2_LTC_ss2_SST2 <- ifelse(realisedval_LTC_ss2_SST2 < VaR_LTC_ss2_SST2, 1, 0)*(1+((realisedval_LTC_ss2_SST2 - VaR_LTC_ss2_SST2)^2))
zi3_LTC_ss2_SST2 <- ifelse(realisedval_LTC_ss2_SST2 < VaR_LTC_ss2_SST2, 1, 0)*((realisedval_LTC_ss2_SST2 - VaR_LTC_ss2_SST2)/VaR_LTC_ss2_SST2)

QPS1_LTC_ss2_SST2 <- (2/200)*sum(((zi1_LTC_ss2_SST2 - 0.01)^2))
QPS2_LTC_ss2_SST2 <- (2/200)*sum(((zi2_LTC_ss2_SST2 - 0.01)^2))
QPS3_LTC_ss2_SST2 <- (2/200)*sum(((zi3_LTC_ss2_SST2 - 0.01)^2))

QPS_LTC_ss2_SST2 <- QPS1_LTC_ss2_SST2 + QPS2_LTC_ss2_SST2 + QPS3_LTC_ss2_SST2
#AST
VaR_LTC_ss2_ast2 <- VaR_LTC_ast2[,1]
realisedval_LTC_ss2_ast2 <- VR24_LTC[,4]
zi1_LTC_ss2_ast2 <- ifelse(realisedval_LTC_ss2_ast2 < VaR_LTC_ss2_ast2, 1, 0)
zi2_LTC_ss2_ast2 <- ifelse(realisedval_LTC_ss2_ast2 < VaR_LTC_ss2_ast2, 1, 0)*(1+((realisedval_LTC_ss2_ast2 - VaR_LTC_ss2_ast2)^2))
zi3_LTC_ss2_ast2 <- ifelse(realisedval_LTC_ss2_ast2 < VaR_LTC_ss2_ast2, 1, 0)*((realisedval_LTC_ss2_ast2 - VaR_LTC_ss2_ast2)/VaR_LTC_ss2_ast2)

QPS1_LTC_ss2_ast2 <- (2/200)*sum(((zi1_LTC_ss2_ast2 - 0.01)^2))
QPS2_LTC_ss2_ast2 <- (2/200)*sum(((zi2_LTC_ss2_ast2 - 0.01)^2))
QPS3_LTC_ss2_ast2 <- (2/200)*sum(((zi3_LTC_ss2_ast2 - 0.01)^2))

QPS_LTC_ss2_ast2 <- QPS1_LTC_ss2_ast2 + QPS2_LTC_ss2_ast2 + QPS3_LTC_ss2_ast2
#AST1
VaR_LTC_ss2_astone2 <- VaR_LTC_astone2[,1]
realisedval_LTC_ss2_astone2 <- VR24_LTC[,4]
zi1_LTC_ss2_astone2 <- ifelse(realisedval_LTC_ss2_astone2 < VaR_LTC_ss2_astone2, 1, 0)
zi2_LTC_ss2_astone2 <- ifelse(realisedval_LTC_ss2_astone2 < VaR_LTC_ss2_astone2, 1, 0)*(1+((realisedval_LTC_ss2_astone2 - VaR_LTC_ss2_astone2)^2))
zi3_LTC_ss2_astone2 <- ifelse(realisedval_LTC_ss2_astone2 < VaR_LTC_ss2_astone2, 1, 0)*((realisedval_LTC_ss2_astone2 - VaR_LTC_ss2_astone2)/VaR_LTC_ss2_astone2)

QPS1_LTC_ss2_astone2 <- (2/200)*sum(((zi1_LTC_ss2_astone2 - 0.01)^2))
QPS2_LTC_ss2_astone2 <- (2/200)*sum(((zi2_LTC_ss2_astone2 - 0.01)^2))
QPS3_LTC_ss2_astone2 <- (2/200)*sum(((zi3_LTC_ss2_astone2 - 0.01)^2))

QPS_LTC_ss2_astone2 <- QPS1_LTC_ss2_astone2 + QPS2_LTC_ss2_astone2 + QPS3_LTC_ss2_astone2

# Sub Sample 3
#Normal
VaR_LTC_ss3_norm3 <- VaR_LTC_norm3[,1]
realisedval_LTC_ss3_norm3 <- VR31_LTC[,4]
zi1_LTC_ss3_norm3 <- ifelse(realisedval_LTC_ss3_norm3 < VaR_LTC_ss3_norm3, 1, 0)
zi2_LTC_ss3_norm3 <- ifelse(realisedval_LTC_ss3_norm3 < VaR_LTC_ss3_norm3, 1, 0)*(1+((realisedval_LTC_ss3_norm3 - VaR_LTC_ss3_norm3)^2))
zi3_LTC_ss3_norm3 <- ifelse(realisedval_LTC_ss3_norm3 < VaR_LTC_ss3_norm3, 1, 0)*((realisedval_LTC_ss3_norm3 - VaR_LTC_ss3_norm3)/VaR_LTC_ss3_norm3)

QPS1_LTC_ss3_norm3 <- (2/100)*sum(((zi1_LTC_ss3_norm3 - 0.01)^2))
QPS2_LTC_ss3_norm3 <- (2/100)*sum(((zi2_LTC_ss3_norm3 - 0.01)^2))
QPS3_LTC_ss3_norm3 <- (2/100)*sum(((zi3_LTC_ss3_norm3 - 0.01)^2))

QPS_LTC_ss3_norm3 <- QPS1_LTC_ss3_norm3 + QPS2_LTC_ss3_norm3 + QPS3_LTC_ss3_norm3 
#ST
VaR_LTC_ss3_ST3 <- VaR_LTC_std3[,1]
realisedval_LTC_ss3_ST3 <- VR32_LTC[,4]
zi1_LTC_ss3_ST3 <- ifelse(realisedval_LTC_ss3_ST3 < VaR_LTC_ss3_ST3, 1, 0)
zi2_LTC_ss3_ST3 <- ifelse(realisedval_LTC_ss3_ST3 < VaR_LTC_ss3_ST3, 1, 0)*(1+((realisedval_LTC_ss3_ST3 - VaR_LTC_ss3_ST3)^2))
zi3_LTC_ss3_ST3 <- ifelse(realisedval_LTC_ss3_ST3 < VaR_LTC_ss3_ST3, 1, 0)*((realisedval_LTC_ss3_ST3 - VaR_LTC_ss3_ST3)/VaR_LTC_ss3_ST3)

QPS1_LTC_ss3_ST3 <- (2/100)*sum(((zi1_LTC_ss3_ST3 - 0.01)^2))
QPS2_LTC_ss3_ST3 <- (2/100)*sum(((zi2_LTC_ss3_ST3 - 0.01)^2))
QPS3_LTC_ss3_ST3 <- (2/100)*sum(((zi3_LTC_ss3_ST3 - 0.01)^2))

QPS_LTC_ss3_ST3 <- QPS1_LTC_ss3_ST3 + QPS2_LTC_ss3_ST3 + QPS3_LTC_ss3_ST3
#SST
VaR_LTC_ss3_SST3 <- VaR_LTC_sstd3[,1]
realisedval_LTC_ss3_SST3 <- VR33_LTC[,4]
zi1_LTC_ss3_SST3 <- ifelse(realisedval_LTC_ss3_SST3 < VaR_LTC_ss3_SST3, 1, 0)
zi2_LTC_ss3_SST3 <- ifelse(realisedval_LTC_ss3_SST3 < VaR_LTC_ss3_SST3, 1, 0)*(1+((realisedval_LTC_ss3_SST3 - VaR_LTC_ss3_SST3)^2))
zi3_LTC_ss3_SST3 <- ifelse(realisedval_LTC_ss3_SST3 < VaR_LTC_ss3_SST3, 1, 0)*((realisedval_LTC_ss3_SST3 - VaR_LTC_ss3_SST3)/VaR_LTC_ss3_SST3)

QPS1_LTC_ss3_SST3 <- (2/100)*sum(((zi1_LTC_ss3_SST3 - 0.01)^2))
QPS2_LTC_ss3_SST3 <- (2/100)*sum(((zi2_LTC_ss3_SST3 - 0.01)^2))
QPS3_LTC_ss3_SST3 <- (2/100)*sum(((zi3_LTC_ss3_SST3 - 0.01)^2))

QPS_LTC_ss3_SST3 <- QPS1_LTC_ss3_SST3 + QPS2_LTC_ss3_SST3 + QPS3_LTC_ss3_SST3
#AST
VaR_LTC_ss3_ast3 <- VaR_LTC_ast3[,1]
realisedval_LTC_ss3_ast3 <- VR34_LTC[,4]
zi1_LTC_ss3_ast3 <- ifelse(realisedval_LTC_ss3_ast3 < VaR_LTC_ss3_ast3, 1, 0)
zi2_LTC_ss3_ast3 <- ifelse(realisedval_LTC_ss3_ast3 < VaR_LTC_ss3_ast3, 1, 0)*(1+((realisedval_LTC_ss3_ast3 - VaR_LTC_ss3_ast3)^2))
zi3_LTC_ss3_ast3 <- ifelse(realisedval_LTC_ss3_ast3 < VaR_LTC_ss3_ast3, 1, 0)*((realisedval_LTC_ss3_ast3 - VaR_LTC_ss3_ast3)/VaR_LTC_ss3_ast3)

QPS1_LTC_ss3_ast3 <- (2/100)*sum(((zi1_LTC_ss3_ast3 - 0.01)^2))
QPS2_LTC_ss3_ast3 <- (2/100)*sum(((zi2_LTC_ss3_ast3 - 0.01)^2))
QPS3_LTC_ss3_ast3 <- (2/100)*sum(((zi3_LTC_ss3_ast3 - 0.01)^2))

QPS_LTC_ss3_ast3 <- QPS1_LTC_ss3_ast3 + QPS2_LTC_ss3_ast3 + QPS3_LTC_ss3_ast3
#AST1
VaR_LTC_ss3_astone3 <- VaR_LTC_astone3[,1]
realisedval_LTC_ss3_astone3 <- VR34_LTC[,4]
zi1_LTC_ss3_astone3 <- ifelse(realisedval_LTC_ss3_astone3 < VaR_LTC_ss3_astone3, 1, 0)
zi2_LTC_ss3_astone3 <- ifelse(realisedval_LTC_ss3_astone3 < VaR_LTC_ss3_astone3, 1, 0)*(1+((realisedval_LTC_ss3_astone3 - VaR_LTC_ss3_astone3)^2))
zi3_LTC_ss3_astone3 <- ifelse(realisedval_LTC_ss3_astone3 < VaR_LTC_ss3_astone3, 1, 0)*((realisedval_LTC_ss3_astone3 - VaR_LTC_ss3_astone3)/VaR_LTC_ss3_astone3)

QPS1_LTC_ss3_astone3 <- (2/100)*sum(((zi1_LTC_ss3_astone3 - 0.01)^2))
QPS2_LTC_ss3_astone3 <- (2/100)*sum(((zi2_LTC_ss3_astone3 - 0.01)^2))
QPS3_LTC_ss3_astone3 <- (2/100)*sum(((zi3_LTC_ss3_astone3 - 0.01)^2))

QPS_LTC_ss3_astone3 <- QPS1_LTC_ss3_astone3 + QPS2_LTC_ss3_astone3 + QPS3_LTC_ss3_astone3

QPS_LTC_fs_GAS <- c(QPS_LTC_fs_norm0, QPS_LTC_fs_ST0, QPS_LTC_fs_SST0, QPS_LTC_fs_ast0, QPS_LTC_fs_astone0)
QPS_LTC_ss1_GAS <- c(QPS_LTC_ss1_norm1, QPS_LTC_ss1_ST1, QPS_LTC_ss1_SST1, QPS_LTC_ss1_ast1, QPS_LTC_ss1_astone1)
QPS_LTC_ss2_GAS <- c(QPS_LTC_ss2_norm2, QPS_LTC_ss2_ST2, QPS_LTC_ss2_SST2, QPS_LTC_ss2_ast2, QPS_LTC_ss2_astone2)
QPS_LTC_ss3_GAS <- c(QPS_LTC_ss3_norm3, QPS_LTC_ss3_ST3, QPS_LTC_ss3_SST3, QPS_LTC_ss3_ast3, QPS_LTC_ss3_astone3)

QPS_LTC_GAS <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_LTC_GAS <- rbind(QPS_LTC_fs_GAS, QPS_LTC_ss1_GAS, QPS_LTC_ss2_GAS, QPS_LTC_ss3_GAS)
colnames(QPS_LTC_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(QPS_LTC_GAS) <- c("Full Sample", "SS1", "SS2", "SS3")

##### :::::::: X M R :::::::: #####
# Full Sample
#Normal
VaR_XMR_fs_norm0 <- VaR_XMR_norm0[,1]
realisedval_XMR_fs_norm0 <- VR1_XMR[,4]
zi1_XMR_fs_norm0 <- ifelse(realisedval_XMR_fs_norm0 < VaR_XMR_fs_norm0, 1, 0)
zi2_XMR_fs_norm0 <- ifelse(realisedval_XMR_fs_norm0 < VaR_XMR_fs_norm0, 1, 0)*(1+((realisedval_XMR_fs_norm0 - VaR_XMR_fs_norm0)^2))
zi3_XMR_fs_norm0 <- ifelse(realisedval_XMR_fs_norm0 < VaR_XMR_fs_norm0, 1, 0)*((realisedval_XMR_fs_norm0 - VaR_XMR_fs_norm0)/VaR_XMR_fs_norm0)

QPS1_XMR_fs_norm0 <- (2/350)*sum(((zi1_XMR_fs_norm0 - 0.01)^2))
QPS2_XMR_fs_norm0 <- (2/350)*sum(((zi2_XMR_fs_norm0 - 0.01)^2))
QPS3_XMR_fs_norm0 <- (2/350)*sum(((zi3_XMR_fs_norm0 - 0.01)^2))

QPS_XMR_fs_norm0 <- QPS1_XMR_fs_norm0 + QPS2_XMR_fs_norm0 + QPS3_XMR_fs_norm0 
#ST
VaR_XMR_fs_ST0 <- VaR_XMR_std0[,1]
realisedval_XMR_fs_ST0 <- VR2_XMR[,4]
zi1_XMR_fs_ST0 <- ifelse(realisedval_XMR_fs_ST0 < VaR_XMR_fs_ST0, 1, 0)
zi2_XMR_fs_ST0 <- ifelse(realisedval_XMR_fs_ST0 < VaR_XMR_fs_ST0, 1, 0)*(1+((realisedval_XMR_fs_ST0 - VaR_XMR_fs_ST0)^2))
zi3_XMR_fs_ST0 <- ifelse(realisedval_XMR_fs_ST0 < VaR_XMR_fs_ST0, 1, 0)*((realisedval_XMR_fs_ST0 - VaR_XMR_fs_ST0)/VaR_XMR_fs_ST0)

QPS1_XMR_fs_ST0 <- (2/350)*sum(((zi1_XMR_fs_ST0 - 0.01)^2))
QPS2_XMR_fs_ST0 <- (2/350)*sum(((zi2_XMR_fs_ST0 - 0.01)^2))
QPS3_XMR_fs_ST0 <- (2/350)*sum(((zi3_XMR_fs_ST0 - 0.01)^2))

QPS_XMR_fs_ST0 <- QPS1_XMR_fs_ST0 + QPS2_XMR_fs_ST0 + QPS3_XMR_fs_ST0
#SST
VaR_XMR_fs_SST0 <- VaR_XMR_sstd0[,1]
realisedval_XMR_fs_SST0 <- VR3_XMR[,4]
zi1_XMR_fs_SST0 <- ifelse(realisedval_XMR_fs_SST0 < VaR_XMR_fs_SST0, 1, 0)
zi2_XMR_fs_SST0 <- ifelse(realisedval_XMR_fs_SST0 < VaR_XMR_fs_SST0, 1, 0)*(1+((realisedval_XMR_fs_SST0 - VaR_XMR_fs_SST0)^2))
zi3_XMR_fs_SST0 <- ifelse(realisedval_XMR_fs_SST0 < VaR_XMR_fs_SST0, 1, 0)*((realisedval_XMR_fs_SST0 - VaR_XMR_fs_SST0)/VaR_XMR_fs_SST0)

QPS1_XMR_fs_SST0 <- (2/350)*sum(((zi1_XMR_fs_SST0 - 0.01)^2))
QPS2_XMR_fs_SST0 <- (2/350)*sum(((zi2_XMR_fs_SST0 - 0.01)^2))
QPS3_XMR_fs_SST0 <- (2/350)*sum(((zi3_XMR_fs_SST0 - 0.01)^2))

QPS_XMR_fs_SST0 <- QPS1_XMR_fs_SST0 + QPS2_XMR_fs_SST0 + QPS3_XMR_fs_SST0
#AST
VaR_XMR_fs_ast0 <- VaR_XMR_ast0[,1]
realisedval_XMR_fs_ast0 <- VR4_XMR[,4]
zi1_XMR_fs_ast0 <- ifelse(realisedval_XMR_fs_ast0 < VaR_XMR_fs_ast0, 1, 0)
zi2_XMR_fs_ast0 <- ifelse(realisedval_XMR_fs_ast0 < VaR_XMR_fs_ast0, 1, 0)*(1+((realisedval_XMR_fs_ast0 - VaR_XMR_fs_ast0)^2))
zi3_XMR_fs_ast0 <- ifelse(realisedval_XMR_fs_ast0 < VaR_XMR_fs_ast0, 1, 0)*((realisedval_XMR_fs_ast0 - VaR_XMR_fs_ast0)/VaR_XMR_fs_ast0)

QPS1_XMR_fs_ast0 <- (2/350)*sum(((zi1_XMR_fs_ast0 - 0.01)^2))
QPS2_XMR_fs_ast0 <- (2/350)*sum(((zi2_XMR_fs_ast0 - 0.01)^2))
QPS3_XMR_fs_ast0 <- (2/350)*sum(((zi3_XMR_fs_ast0 - 0.01)^2))

QPS_XMR_fs_ast0 <- QPS1_XMR_fs_ast0 + QPS2_XMR_fs_ast0 + QPS3_XMR_fs_ast0
#AST1
VaR_XMR_fs_astone0 <- VaR_XMR_astone0[,1]
realisedval_XMR_fs_astone0 <- VR4_XMR[,4]
zi1_XMR_fs_astone0 <- ifelse(realisedval_XMR_fs_astone0 < VaR_XMR_fs_astone0, 1, 0)
zi2_XMR_fs_astone0 <- ifelse(realisedval_XMR_fs_astone0 < VaR_XMR_fs_astone0, 1, 0)*(1+((realisedval_XMR_fs_astone0 - VaR_XMR_fs_astone0)^2))
zi3_XMR_fs_astone0 <- ifelse(realisedval_XMR_fs_astone0 < VaR_XMR_fs_astone0, 1, 0)*((realisedval_XMR_fs_astone0 - VaR_XMR_fs_astone0)/VaR_XMR_fs_astone0)

QPS1_XMR_fs_astone0 <- (2/350)*sum(((zi1_XMR_fs_astone0 - 0.01)^2))
QPS2_XMR_fs_astone0 <- (2/350)*sum(((zi2_XMR_fs_astone0 - 0.01)^2))
QPS3_XMR_fs_astone0 <- (2/350)*sum(((zi3_XMR_fs_astone0 - 0.01)^2))

QPS_XMR_fs_astone0 <- QPS1_XMR_fs_astone0 + QPS2_XMR_fs_astone0 + QPS3_XMR_fs_astone0

# Sub Sample 1
#Normal
VaR_XMR_ss1_norm1 <- VaR_XMR_norm1[,1]
realisedval_XMR_ss1_norm1 <- VR11_XMR[,4]
zi1_XMR_ss1_norm1 <- ifelse(realisedval_XMR_ss1_norm1 < VaR_XMR_ss1_norm1, 1, 0)
zi2_XMR_ss1_norm1 <- ifelse(realisedval_XMR_ss1_norm1 < VaR_XMR_ss1_norm1, 1, 0)*(1+((realisedval_XMR_ss1_norm1 - VaR_XMR_ss1_norm1)^2))
zi3_XMR_ss1_norm1 <- ifelse(realisedval_XMR_ss1_norm1 < VaR_XMR_ss1_norm1, 1, 0)*((realisedval_XMR_ss1_norm1 - VaR_XMR_ss1_norm1)/VaR_XMR_ss1_norm1)

QPS1_XMR_ss1_norm1 <- (2/60)*sum(((zi1_XMR_ss1_norm1 - 0.01)^2))
QPS2_XMR_ss1_norm1 <- (2/60)*sum(((zi2_XMR_ss1_norm1 - 0.01)^2))
QPS3_XMR_ss1_norm1 <- (2/60)*sum(((zi3_XMR_ss1_norm1 - 0.01)^2))

QPS_XMR_ss1_norm1 <- QPS1_XMR_ss1_norm1 + QPS2_XMR_ss1_norm1 + QPS3_XMR_ss1_norm1 
#ST
VaR_XMR_ss1_ST1 <- VaR_XMR_std1[,1]
realisedval_XMR_ss1_ST1 <- VR12_XMR[,4]
zi1_XMR_ss1_ST1 <- ifelse(realisedval_XMR_ss1_ST1 < VaR_XMR_ss1_ST1, 1, 0)
zi2_XMR_ss1_ST1 <- ifelse(realisedval_XMR_ss1_ST1 < VaR_XMR_ss1_ST1, 1, 0)*(1+((realisedval_XMR_ss1_ST1 - VaR_XMR_ss1_ST1)^2))
zi3_XMR_ss1_ST1 <- ifelse(realisedval_XMR_ss1_ST1 < VaR_XMR_ss1_ST1, 1, 0)*((realisedval_XMR_ss1_ST1 - VaR_XMR_ss1_ST1)/VaR_XMR_ss1_ST1)

QPS1_XMR_ss1_ST1 <- (2/60)*sum(((zi1_XMR_ss1_ST1 - 0.01)^2))
QPS2_XMR_ss1_ST1 <- (2/60)*sum(((zi2_XMR_ss1_ST1 - 0.01)^2))
QPS3_XMR_ss1_ST1 <- (2/60)*sum(((zi3_XMR_ss1_ST1 - 0.01)^2))

QPS_XMR_ss1_ST1 <- QPS1_XMR_ss1_ST1 + QPS2_XMR_ss1_ST1 + QPS3_XMR_ss1_ST1
#SST
VaR_XMR_ss1_SST1 <- VaR_XMR_sstd1[,1]
realisedval_XMR_ss1_SST1 <- VR13_XMR[,4]
zi1_XMR_ss1_SST1 <- ifelse(realisedval_XMR_ss1_SST1 < VaR_XMR_ss1_SST1, 1, 0)
zi2_XMR_ss1_SST1 <- ifelse(realisedval_XMR_ss1_SST1 < VaR_XMR_ss1_SST1, 1, 0)*(1+((realisedval_XMR_ss1_SST1 - VaR_XMR_ss1_SST1)^2))
zi3_XMR_ss1_SST1 <- ifelse(realisedval_XMR_ss1_SST1 < VaR_XMR_ss1_SST1, 1, 0)*((realisedval_XMR_ss1_SST1 - VaR_XMR_ss1_SST1)/VaR_XMR_ss1_SST1)

QPS1_XMR_ss1_SST1 <- (2/60)*sum(((zi1_XMR_ss1_SST1 - 0.01)^2))
QPS2_XMR_ss1_SST1 <- (2/60)*sum(((zi2_XMR_ss1_SST1 - 0.01)^2))
QPS3_XMR_ss1_SST1 <- (2/60)*sum(((zi3_XMR_ss1_SST1 - 0.01)^2))

QPS_XMR_ss1_SST1 <- QPS1_XMR_ss1_SST1 + QPS2_XMR_ss1_SST1 + QPS3_XMR_ss1_SST1
#AST
VaR_XMR_ss1_ast1 <- VaR_XMR_ast1[,1]
realisedval_XMR_ss1_ast1 <- VR14_XMR[,4]
zi1_XMR_ss1_ast1 <- ifelse(realisedval_XMR_ss1_ast1 < VaR_XMR_ss1_ast1, 1, 0)
zi2_XMR_ss1_ast1 <- ifelse(realisedval_XMR_ss1_ast1 < VaR_XMR_ss1_ast1, 1, 0)*(1+((realisedval_XMR_ss1_ast1 - VaR_XMR_ss1_ast1)^2))
zi3_XMR_ss1_ast1 <- ifelse(realisedval_XMR_ss1_ast1 < VaR_XMR_ss1_ast1, 1, 0)*((realisedval_XMR_ss1_ast1 - VaR_XMR_ss1_ast1)/VaR_XMR_ss1_ast1)

QPS1_XMR_ss1_ast1 <- (2/60)*sum(((zi1_XMR_ss1_ast1 - 0.01)^2))
QPS2_XMR_ss1_ast1 <- (2/60)*sum(((zi2_XMR_ss1_ast1 - 0.01)^2))
QPS3_XMR_ss1_ast1 <- (2/60)*sum(((zi3_XMR_ss1_ast1 - 0.01)^2))

QPS_XMR_ss1_ast1 <- QPS1_XMR_ss1_ast1 + QPS2_XMR_ss1_ast1 + QPS3_XMR_ss1_ast1
#AST1
VaR_XMR_ss1_astone1 <- VaR_XMR_astone1[,1]
realisedval_XMR_ss1_astone1 <- VR14_XMR[,4]
zi1_XMR_ss1_astone1 <- ifelse(realisedval_XMR_ss1_astone1 < VaR_XMR_ss1_astone1, 1, 0)
zi2_XMR_ss1_astone1 <- ifelse(realisedval_XMR_ss1_astone1 < VaR_XMR_ss1_astone1, 1, 0)*(1+((realisedval_XMR_ss1_astone1 - VaR_XMR_ss1_astone1)^2))
zi3_XMR_ss1_astone1 <- ifelse(realisedval_XMR_ss1_astone1 < VaR_XMR_ss1_astone1, 1, 0)*((realisedval_XMR_ss1_astone1 - VaR_XMR_ss1_astone1)/VaR_XMR_ss1_astone1)

QPS1_XMR_ss1_astone1 <- (2/60)*sum(((zi1_XMR_ss1_astone1 - 0.01)^2))
QPS2_XMR_ss1_astone1 <- (2/60)*sum(((zi2_XMR_ss1_astone1 - 0.01)^2))
QPS3_XMR_ss1_astone1 <- (2/60)*sum(((zi3_XMR_ss1_astone1 - 0.01)^2))

QPS_XMR_ss1_astone1 <- QPS1_XMR_ss1_astone1 + QPS2_XMR_ss1_astone1 + QPS3_XMR_ss1_astone1

# Sub Sample 2
#Normal
VaR_XMR_ss2_norm2 <- VaR_XMR_norm2[,1]
realisedval_XMR_ss2_norm2 <- VR21_XMR[,4]
zi1_XMR_ss2_norm2 <- ifelse(realisedval_XMR_ss2_norm2 < VaR_XMR_ss2_norm2, 1, 0)
zi2_XMR_ss2_norm2 <- ifelse(realisedval_XMR_ss2_norm2 < VaR_XMR_ss2_norm2, 1, 0)*(1+((realisedval_XMR_ss2_norm2 - VaR_XMR_ss2_norm2)^2))
zi3_XMR_ss2_norm2 <- ifelse(realisedval_XMR_ss2_norm2 < VaR_XMR_ss2_norm2, 1, 0)*((realisedval_XMR_ss2_norm2 - VaR_XMR_ss2_norm2)/VaR_XMR_ss2_norm2)

QPS1_XMR_ss2_norm2 <- (2/200)*sum(((zi1_XMR_ss2_norm2 - 0.01)^2))
QPS2_XMR_ss2_norm2 <- (2/200)*sum(((zi2_XMR_ss2_norm2 - 0.01)^2))
QPS3_XMR_ss2_norm2 <- (2/200)*sum(((zi3_XMR_ss2_norm2 - 0.01)^2))

QPS_XMR_ss2_norm2 <- QPS1_XMR_ss2_norm2 + QPS2_XMR_ss2_norm2 + QPS3_XMR_ss2_norm2 
#ST
VaR_XMR_ss2_ST2 <- VaR_XMR_std2[,1]
realisedval_XMR_ss2_ST2 <- VR22_XMR[,4]
zi1_XMR_ss2_ST2 <- ifelse(realisedval_XMR_ss2_ST2 < VaR_XMR_ss2_ST2, 1, 0)
zi2_XMR_ss2_ST2 <- ifelse(realisedval_XMR_ss2_ST2 < VaR_XMR_ss2_ST2, 1, 0)*(1+((realisedval_XMR_ss2_ST2 - VaR_XMR_ss2_ST2)^2))
zi3_XMR_ss2_ST2 <- ifelse(realisedval_XMR_ss2_ST2 < VaR_XMR_ss2_ST2, 1, 0)*((realisedval_XMR_ss2_ST2 - VaR_XMR_ss2_ST2)/VaR_XMR_ss2_ST2)

QPS1_XMR_ss2_ST2 <- (2/200)*sum(((zi1_XMR_ss2_ST2 - 0.01)^2))
QPS2_XMR_ss2_ST2 <- (2/200)*sum(((zi2_XMR_ss2_ST2 - 0.01)^2))
QPS3_XMR_ss2_ST2 <- (2/200)*sum(((zi3_XMR_ss2_ST2 - 0.01)^2))

QPS_XMR_ss2_ST2 <- QPS1_XMR_ss2_ST2 + QPS2_XMR_ss2_ST2 + QPS3_XMR_ss2_ST2
#SST
VaR_XMR_ss2_SST2 <- VaR_XMR_sstd2[,1]
realisedval_XMR_ss2_SST2 <- VR23_XMR[,4]
zi1_XMR_ss2_SST2 <- ifelse(realisedval_XMR_ss2_SST2 < VaR_XMR_ss2_SST2, 1, 0)
zi2_XMR_ss2_SST2 <- ifelse(realisedval_XMR_ss2_SST2 < VaR_XMR_ss2_SST2, 1, 0)*(1+((realisedval_XMR_ss2_SST2 - VaR_XMR_ss2_SST2)^2))
zi3_XMR_ss2_SST2 <- ifelse(realisedval_XMR_ss2_SST2 < VaR_XMR_ss2_SST2, 1, 0)*((realisedval_XMR_ss2_SST2 - VaR_XMR_ss2_SST2)/VaR_XMR_ss2_SST2)

QPS1_XMR_ss2_SST2 <- (2/200)*sum(((zi1_XMR_ss2_SST2 - 0.01)^2))
QPS2_XMR_ss2_SST2 <- (2/200)*sum(((zi2_XMR_ss2_SST2 - 0.01)^2))
QPS3_XMR_ss2_SST2 <- (2/200)*sum(((zi3_XMR_ss2_SST2 - 0.01)^2))

QPS_XMR_ss2_SST2 <- QPS1_XMR_ss2_SST2 + QPS2_XMR_ss2_SST2 + QPS3_XMR_ss2_SST2
#AST
VaR_XMR_ss2_ast2 <- VaR_XMR_ast2[,1]
realisedval_XMR_ss2_ast2 <- VR24_XMR[,4]
zi1_XMR_ss2_ast2 <- ifelse(realisedval_XMR_ss2_ast2 < VaR_XMR_ss2_ast2, 1, 0)
zi2_XMR_ss2_ast2 <- ifelse(realisedval_XMR_ss2_ast2 < VaR_XMR_ss2_ast2, 1, 0)*(1+((realisedval_XMR_ss2_ast2 - VaR_XMR_ss2_ast2)^2))
zi3_XMR_ss2_ast2 <- ifelse(realisedval_XMR_ss2_ast2 < VaR_XMR_ss2_ast2, 1, 0)*((realisedval_XMR_ss2_ast2 - VaR_XMR_ss2_ast2)/VaR_XMR_ss2_ast2)

QPS1_XMR_ss2_ast2 <- (2/200)*sum(((zi1_XMR_ss2_ast2 - 0.01)^2))
QPS2_XMR_ss2_ast2 <- (2/200)*sum(((zi2_XMR_ss2_ast2 - 0.01)^2))
QPS3_XMR_ss2_ast2 <- (2/200)*sum(((zi3_XMR_ss2_ast2 - 0.01)^2))

QPS_XMR_ss2_ast2 <- QPS1_XMR_ss2_ast2 + QPS2_XMR_ss2_ast2 + QPS3_XMR_ss2_ast2
#AST1
VaR_XMR_ss2_astone2 <- VaR_XMR_astone2[,1]
realisedval_XMR_ss2_astone2 <- VR24_XMR[,4]
zi1_XMR_ss2_astone2 <- ifelse(realisedval_XMR_ss2_astone2 < VaR_XMR_ss2_astone2, 1, 0)
zi2_XMR_ss2_astone2 <- ifelse(realisedval_XMR_ss2_astone2 < VaR_XMR_ss2_astone2, 1, 0)*(1+((realisedval_XMR_ss2_astone2 - VaR_XMR_ss2_astone2)^2))
zi3_XMR_ss2_astone2 <- ifelse(realisedval_XMR_ss2_astone2 < VaR_XMR_ss2_astone2, 1, 0)*((realisedval_XMR_ss2_astone2 - VaR_XMR_ss2_astone2)/VaR_XMR_ss2_astone2)

QPS1_XMR_ss2_astone2 <- (2/200)*sum(((zi1_XMR_ss2_astone2 - 0.01)^2))
QPS2_XMR_ss2_astone2 <- (2/200)*sum(((zi2_XMR_ss2_astone2 - 0.01)^2))
QPS3_XMR_ss2_astone2 <- (2/200)*sum(((zi3_XMR_ss2_astone2 - 0.01)^2))

QPS_XMR_ss2_astone2 <- QPS1_XMR_ss2_astone2 + QPS2_XMR_ss2_astone2 + QPS3_XMR_ss2_astone2

# Sub Sample 3
#Normal
VaR_XMR_ss3_norm3 <- VaR_XMR_norm3[,1]
realisedval_XMR_ss3_norm3 <- VR31_XMR[,4]
zi1_XMR_ss3_norm3 <- ifelse(realisedval_XMR_ss3_norm3 < VaR_XMR_ss3_norm3, 1, 0)
zi2_XMR_ss3_norm3 <- ifelse(realisedval_XMR_ss3_norm3 < VaR_XMR_ss3_norm3, 1, 0)*(1+((realisedval_XMR_ss3_norm3 - VaR_XMR_ss3_norm3)^2))
zi3_XMR_ss3_norm3 <- ifelse(realisedval_XMR_ss3_norm3 < VaR_XMR_ss3_norm3, 1, 0)*((realisedval_XMR_ss3_norm3 - VaR_XMR_ss3_norm3)/VaR_XMR_ss3_norm3)

QPS1_XMR_ss3_norm3 <- (2/100)*sum(((zi1_XMR_ss3_norm3 - 0.01)^2))
QPS2_XMR_ss3_norm3 <- (2/100)*sum(((zi2_XMR_ss3_norm3 - 0.01)^2))
QPS3_XMR_ss3_norm3 <- (2/100)*sum(((zi3_XMR_ss3_norm3 - 0.01)^2))

QPS_XMR_ss3_norm3 <- QPS1_XMR_ss3_norm3 + QPS2_XMR_ss3_norm3 + QPS3_XMR_ss3_norm3 
#ST
VaR_XMR_ss3_ST3 <- VaR_XMR_std3[,1]
realisedval_XMR_ss3_ST3 <- VR32_XMR[,4]
zi1_XMR_ss3_ST3 <- ifelse(realisedval_XMR_ss3_ST3 < VaR_XMR_ss3_ST3, 1, 0)
zi2_XMR_ss3_ST3 <- ifelse(realisedval_XMR_ss3_ST3 < VaR_XMR_ss3_ST3, 1, 0)*(1+((realisedval_XMR_ss3_ST3 - VaR_XMR_ss3_ST3)^2))
zi3_XMR_ss3_ST3 <- ifelse(realisedval_XMR_ss3_ST3 < VaR_XMR_ss3_ST3, 1, 0)*((realisedval_XMR_ss3_ST3 - VaR_XMR_ss3_ST3)/VaR_XMR_ss3_ST3)

QPS1_XMR_ss3_ST3 <- (2/100)*sum(((zi1_XMR_ss3_ST3 - 0.01)^2))
QPS2_XMR_ss3_ST3 <- (2/100)*sum(((zi2_XMR_ss3_ST3 - 0.01)^2))
QPS3_XMR_ss3_ST3 <- (2/100)*sum(((zi3_XMR_ss3_ST3 - 0.01)^2))

QPS_XMR_ss3_ST3 <- QPS1_XMR_ss3_ST3 + QPS2_XMR_ss3_ST3 + QPS3_XMR_ss3_ST3
#SST
VaR_XMR_ss3_SST3 <- VaR_XMR_sstd3[,1]
realisedval_XMR_ss3_SST3 <- VR33_XMR[,4]
zi1_XMR_ss3_SST3 <- ifelse(realisedval_XMR_ss3_SST3 < VaR_XMR_ss3_SST3, 1, 0)
zi2_XMR_ss3_SST3 <- ifelse(realisedval_XMR_ss3_SST3 < VaR_XMR_ss3_SST3, 1, 0)*(1+((realisedval_XMR_ss3_SST3 - VaR_XMR_ss3_SST3)^2))
zi3_XMR_ss3_SST3 <- ifelse(realisedval_XMR_ss3_SST3 < VaR_XMR_ss3_SST3, 1, 0)*((realisedval_XMR_ss3_SST3 - VaR_XMR_ss3_SST3)/VaR_XMR_ss3_SST3)

QPS1_XMR_ss3_SST3 <- (2/100)*sum(((zi1_XMR_ss3_SST3 - 0.01)^2))
QPS2_XMR_ss3_SST3 <- (2/100)*sum(((zi2_XMR_ss3_SST3 - 0.01)^2))
QPS3_XMR_ss3_SST3 <- (2/100)*sum(((zi3_XMR_ss3_SST3 - 0.01)^2))

QPS_XMR_ss3_SST3 <- QPS1_XMR_ss3_SST3 + QPS2_XMR_ss3_SST3 + QPS3_XMR_ss3_SST3
#AST
VaR_XMR_ss3_ast3 <- VaR_XMR_ast3[,1]
realisedval_XMR_ss3_ast3 <- VR34_XMR[,4]
zi1_XMR_ss3_ast3 <- ifelse(realisedval_XMR_ss3_ast3 < VaR_XMR_ss3_ast3, 1, 0)
zi2_XMR_ss3_ast3 <- ifelse(realisedval_XMR_ss3_ast3 < VaR_XMR_ss3_ast3, 1, 0)*(1+((realisedval_XMR_ss3_ast3 - VaR_XMR_ss3_ast3)^2))
zi3_XMR_ss3_ast3 <- ifelse(realisedval_XMR_ss3_ast3 < VaR_XMR_ss3_ast3, 1, 0)*((realisedval_XMR_ss3_ast3 - VaR_XMR_ss3_ast3)/VaR_XMR_ss3_ast3)

QPS1_XMR_ss3_ast3 <- (2/100)*sum(((zi1_XMR_ss3_ast3 - 0.01)^2))
QPS2_XMR_ss3_ast3 <- (2/100)*sum(((zi2_XMR_ss3_ast3 - 0.01)^2))
QPS3_XMR_ss3_ast3 <- (2/100)*sum(((zi3_XMR_ss3_ast3 - 0.01)^2))

QPS_XMR_ss3_ast3 <- QPS1_XMR_ss3_ast3 + QPS2_XMR_ss3_ast3 + QPS3_XMR_ss3_ast3
#AST1
VaR_XMR_ss3_astone3 <- VaR_XMR_astone3[,1]
realisedval_XMR_ss3_astone3 <- VR34_XMR[,4]
zi1_XMR_ss3_astone3 <- ifelse(realisedval_XMR_ss3_astone3 < VaR_XMR_ss3_astone3, 1, 0)
zi2_XMR_ss3_astone3 <- ifelse(realisedval_XMR_ss3_astone3 < VaR_XMR_ss3_astone3, 1, 0)*(1+((realisedval_XMR_ss3_astone3 - VaR_XMR_ss3_astone3)^2))
zi3_XMR_ss3_astone3 <- ifelse(realisedval_XMR_ss3_astone3 < VaR_XMR_ss3_astone3, 1, 0)*((realisedval_XMR_ss3_astone3 - VaR_XMR_ss3_astone3)/VaR_XMR_ss3_astone3)

QPS1_XMR_ss3_astone3 <- (2/100)*sum(((zi1_XMR_ss3_astone3 - 0.01)^2))
QPS2_XMR_ss3_astone3 <- (2/100)*sum(((zi2_XMR_ss3_astone3 - 0.01)^2))
QPS3_XMR_ss3_astone3 <- (2/100)*sum(((zi3_XMR_ss3_astone3 - 0.01)^2))

QPS_XMR_ss3_astone3 <- QPS1_XMR_ss3_astone3 + QPS2_XMR_ss3_astone3 + QPS3_XMR_ss3_astone3

QPS_XMR_fs_GAS <- c(QPS_XMR_fs_norm0, QPS_XMR_fs_ST0, QPS_XMR_fs_SST0, QPS_XMR_fs_ast0, QPS_XMR_fs_astone0)
QPS_XMR_ss1_GAS <- c(QPS_XMR_ss1_norm1, QPS_XMR_ss1_ST1, QPS_XMR_ss1_SST1, QPS_XMR_ss1_ast1, QPS_XMR_ss1_astone1)
QPS_XMR_ss2_GAS <- c(QPS_XMR_ss2_norm2, QPS_XMR_ss2_ST2, QPS_XMR_ss2_SST2, QPS_XMR_ss2_ast2, QPS_XMR_ss2_astone2)
QPS_XMR_ss3_GAS <- c(QPS_XMR_ss3_norm3, QPS_XMR_ss3_ST3, QPS_XMR_ss3_SST3, QPS_XMR_ss3_ast3, QPS_XMR_ss3_astone3)

QPS_XMR_GAS <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_XMR_GAS <- rbind(QPS_XMR_fs_GAS, QPS_XMR_ss1_GAS, QPS_XMR_ss2_GAS, QPS_XMR_ss3_GAS)
colnames(QPS_XMR_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(QPS_XMR_GAS) <- c("Full Sample", "SS1", "SS2", "SS3")

##### :::::::: E T C :::::::: #####
# Full Sample
#Normal
VaR_ETC_fs_norm0 <- VaR_ETC_norm0[,1]
realisedval_ETC_fs_norm0 <- VR1_ETC[,4]
zi1_ETC_fs_norm0 <- ifelse(realisedval_ETC_fs_norm0 < VaR_ETC_fs_norm0, 1, 0)
zi2_ETC_fs_norm0 <- ifelse(realisedval_ETC_fs_norm0 < VaR_ETC_fs_norm0, 1, 0)*(1+((realisedval_ETC_fs_norm0 - VaR_ETC_fs_norm0)^2))
zi3_ETC_fs_norm0 <- ifelse(realisedval_ETC_fs_norm0 < VaR_ETC_fs_norm0, 1, 0)*((realisedval_ETC_fs_norm0 - VaR_ETC_fs_norm0)/VaR_ETC_fs_norm0)

QPS1_ETC_fs_norm0 <- (2/350)*sum(((zi1_ETC_fs_norm0 - 0.01)^2))
QPS2_ETC_fs_norm0 <- (2/350)*sum(((zi2_ETC_fs_norm0 - 0.01)^2))
QPS3_ETC_fs_norm0 <- (2/350)*sum(((zi3_ETC_fs_norm0 - 0.01)^2))

QPS_ETC_fs_norm0 <- QPS1_ETC_fs_norm0 + QPS2_ETC_fs_norm0 + QPS3_ETC_fs_norm0 
#ST
VaR_ETC_fs_ST0 <- VaR_ETC_std0[,1]
realisedval_ETC_fs_ST0 <- VR2_ETC[,4]
zi1_ETC_fs_ST0 <- ifelse(realisedval_ETC_fs_ST0 < VaR_ETC_fs_ST0, 1, 0)
zi2_ETC_fs_ST0 <- ifelse(realisedval_ETC_fs_ST0 < VaR_ETC_fs_ST0, 1, 0)*(1+((realisedval_ETC_fs_ST0 - VaR_ETC_fs_ST0)^2))
zi3_ETC_fs_ST0 <- ifelse(realisedval_ETC_fs_ST0 < VaR_ETC_fs_ST0, 1, 0)*((realisedval_ETC_fs_ST0 - VaR_ETC_fs_ST0)/VaR_ETC_fs_ST0)

QPS1_ETC_fs_ST0 <- (2/350)*sum(((zi1_ETC_fs_ST0 - 0.01)^2))
QPS2_ETC_fs_ST0 <- (2/350)*sum(((zi2_ETC_fs_ST0 - 0.01)^2))
QPS3_ETC_fs_ST0 <- (2/350)*sum(((zi3_ETC_fs_ST0 - 0.01)^2))

QPS_ETC_fs_ST0 <- QPS1_ETC_fs_ST0 + QPS2_ETC_fs_ST0 + QPS3_ETC_fs_ST0
#SST
VaR_ETC_fs_SST0 <- VaR_ETC_sstd0[,1]
realisedval_ETC_fs_SST0 <- VR3_ETC[,4]
zi1_ETC_fs_SST0 <- ifelse(realisedval_ETC_fs_SST0 < VaR_ETC_fs_SST0, 1, 0)
zi2_ETC_fs_SST0 <- ifelse(realisedval_ETC_fs_SST0 < VaR_ETC_fs_SST0, 1, 0)*(1+((realisedval_ETC_fs_SST0 - VaR_ETC_fs_SST0)^2))
zi3_ETC_fs_SST0 <- ifelse(realisedval_ETC_fs_SST0 < VaR_ETC_fs_SST0, 1, 0)*((realisedval_ETC_fs_SST0 - VaR_ETC_fs_SST0)/VaR_ETC_fs_SST0)

QPS1_ETC_fs_SST0 <- (2/350)*sum(((zi1_ETC_fs_SST0 - 0.01)^2))
QPS2_ETC_fs_SST0 <- (2/350)*sum(((zi2_ETC_fs_SST0 - 0.01)^2))
QPS3_ETC_fs_SST0 <- (2/350)*sum(((zi3_ETC_fs_SST0 - 0.01)^2))

QPS_ETC_fs_SST0 <- QPS1_ETC_fs_SST0 + QPS2_ETC_fs_SST0 + QPS3_ETC_fs_SST0
#AST
VaR_ETC_fs_ast0 <- VaR_ETC_ast0[,1]
realisedval_ETC_fs_ast0 <- VR4_ETC[,4]
zi1_ETC_fs_ast0 <- ifelse(realisedval_ETC_fs_ast0 < VaR_ETC_fs_ast0, 1, 0)
zi2_ETC_fs_ast0 <- ifelse(realisedval_ETC_fs_ast0 < VaR_ETC_fs_ast0, 1, 0)*(1+((realisedval_ETC_fs_ast0 - VaR_ETC_fs_ast0)^2))
zi3_ETC_fs_ast0 <- ifelse(realisedval_ETC_fs_ast0 < VaR_ETC_fs_ast0, 1, 0)*((realisedval_ETC_fs_ast0 - VaR_ETC_fs_ast0)/VaR_ETC_fs_ast0)

QPS1_ETC_fs_ast0 <- (2/350)*sum(((zi1_ETC_fs_ast0 - 0.01)^2))
QPS2_ETC_fs_ast0 <- (2/350)*sum(((zi2_ETC_fs_ast0 - 0.01)^2))
QPS3_ETC_fs_ast0 <- (2/350)*sum(((zi3_ETC_fs_ast0 - 0.01)^2))

QPS_ETC_fs_ast0 <- QPS1_ETC_fs_ast0 + QPS2_ETC_fs_ast0 + QPS3_ETC_fs_ast0
#AST1
VaR_ETC_fs_astone0 <- VaR_ETC_astone0[,1]
realisedval_ETC_fs_astone0 <- VR4_ETC[,4]
zi1_ETC_fs_astone0 <- ifelse(realisedval_ETC_fs_astone0 < VaR_ETC_fs_astone0, 1, 0)
zi2_ETC_fs_astone0 <- ifelse(realisedval_ETC_fs_astone0 < VaR_ETC_fs_astone0, 1, 0)*(1+((realisedval_ETC_fs_astone0 - VaR_ETC_fs_astone0)^2))
zi3_ETC_fs_astone0 <- ifelse(realisedval_ETC_fs_astone0 < VaR_ETC_fs_astone0, 1, 0)*((realisedval_ETC_fs_astone0 - VaR_ETC_fs_astone0)/VaR_ETC_fs_astone0)

QPS1_ETC_fs_astone0 <- (2/350)*sum(((zi1_ETC_fs_astone0 - 0.01)^2))
QPS2_ETC_fs_astone0 <- (2/350)*sum(((zi2_ETC_fs_astone0 - 0.01)^2))
QPS3_ETC_fs_astone0 <- (2/350)*sum(((zi3_ETC_fs_astone0 - 0.01)^2))

QPS_ETC_fs_astone0 <- QPS1_ETC_fs_astone0 + QPS2_ETC_fs_astone0 + QPS3_ETC_fs_astone0

# Sub Sample 1
#Normal
VaR_ETC_ss1_norm1 <- VaR_ETC_norm1[,1]
realisedval_ETC_ss1_norm1 <- VR11_ETC[,4]
zi1_ETC_ss1_norm1 <- ifelse(realisedval_ETC_ss1_norm1 < VaR_ETC_ss1_norm1, 1, 0)
zi2_ETC_ss1_norm1 <- ifelse(realisedval_ETC_ss1_norm1 < VaR_ETC_ss1_norm1, 1, 0)*(1+((realisedval_ETC_ss1_norm1 - VaR_ETC_ss1_norm1)^2))
zi3_ETC_ss1_norm1 <- ifelse(realisedval_ETC_ss1_norm1 < VaR_ETC_ss1_norm1, 1, 0)*((realisedval_ETC_ss1_norm1 - VaR_ETC_ss1_norm1)/VaR_ETC_ss1_norm1)

QPS1_ETC_ss1_norm1 <- (2/60)*sum(((zi1_ETC_ss1_norm1 - 0.01)^2))
QPS2_ETC_ss1_norm1 <- (2/60)*sum(((zi2_ETC_ss1_norm1 - 0.01)^2))
QPS3_ETC_ss1_norm1 <- (2/60)*sum(((zi3_ETC_ss1_norm1 - 0.01)^2))

QPS_ETC_ss1_norm1 <- QPS1_ETC_ss1_norm1 + QPS2_ETC_ss1_norm1 + QPS3_ETC_ss1_norm1 
#ST
VaR_ETC_ss1_ST1 <- VaR_ETC_std1[,1]
realisedval_ETC_ss1_ST1 <- VR12_ETC[,4]
zi1_ETC_ss1_ST1 <- ifelse(realisedval_ETC_ss1_ST1 < VaR_ETC_ss1_ST1, 1, 0)
zi2_ETC_ss1_ST1 <- ifelse(realisedval_ETC_ss1_ST1 < VaR_ETC_ss1_ST1, 1, 0)*(1+((realisedval_ETC_ss1_ST1 - VaR_ETC_ss1_ST1)^2))
zi3_ETC_ss1_ST1 <- ifelse(realisedval_ETC_ss1_ST1 < VaR_ETC_ss1_ST1, 1, 0)*((realisedval_ETC_ss1_ST1 - VaR_ETC_ss1_ST1)/VaR_ETC_ss1_ST1)

QPS1_ETC_ss1_ST1 <- (2/60)*sum(((zi1_ETC_ss1_ST1 - 0.01)^2))
QPS2_ETC_ss1_ST1 <- (2/60)*sum(((zi2_ETC_ss1_ST1 - 0.01)^2))
QPS3_ETC_ss1_ST1 <- (2/60)*sum(((zi3_ETC_ss1_ST1 - 0.01)^2))

QPS_ETC_ss1_ST1 <- QPS1_ETC_ss1_ST1 + QPS2_ETC_ss1_ST1 + QPS3_ETC_ss1_ST1
#SST
VaR_ETC_ss1_SST1 <- VaR_ETC_sstd1[,1]
realisedval_ETC_ss1_SST1 <- VR13_ETC[,4]
zi1_ETC_ss1_SST1 <- ifelse(realisedval_ETC_ss1_SST1 < VaR_ETC_ss1_SST1, 1, 0)
zi2_ETC_ss1_SST1 <- ifelse(realisedval_ETC_ss1_SST1 < VaR_ETC_ss1_SST1, 1, 0)*(1+((realisedval_ETC_ss1_SST1 - VaR_ETC_ss1_SST1)^2))
zi3_ETC_ss1_SST1 <- ifelse(realisedval_ETC_ss1_SST1 < VaR_ETC_ss1_SST1, 1, 0)*((realisedval_ETC_ss1_SST1 - VaR_ETC_ss1_SST1)/VaR_ETC_ss1_SST1)

QPS1_ETC_ss1_SST1 <- (2/60)*sum(((zi1_ETC_ss1_SST1 - 0.01)^2))
QPS2_ETC_ss1_SST1 <- (2/60)*sum(((zi2_ETC_ss1_SST1 - 0.01)^2))
QPS3_ETC_ss1_SST1 <- (2/60)*sum(((zi3_ETC_ss1_SST1 - 0.01)^2))

QPS_ETC_ss1_SST1 <- QPS1_ETC_ss1_SST1 + QPS2_ETC_ss1_SST1 + QPS3_ETC_ss1_SST1
#AST
VaR_ETC_ss1_ast1 <- VaR_ETC_ast1[,1]
realisedval_ETC_ss1_ast1 <- VR14_ETC[,4]
zi1_ETC_ss1_ast1 <- ifelse(realisedval_ETC_ss1_ast1 < VaR_ETC_ss1_ast1, 1, 0)
zi2_ETC_ss1_ast1 <- ifelse(realisedval_ETC_ss1_ast1 < VaR_ETC_ss1_ast1, 1, 0)*(1+((realisedval_ETC_ss1_ast1 - VaR_ETC_ss1_ast1)^2))
zi3_ETC_ss1_ast1 <- ifelse(realisedval_ETC_ss1_ast1 < VaR_ETC_ss1_ast1, 1, 0)*((realisedval_ETC_ss1_ast1 - VaR_ETC_ss1_ast1)/VaR_ETC_ss1_ast1)

QPS1_ETC_ss1_ast1 <- (2/60)*sum(((zi1_ETC_ss1_ast1 - 0.01)^2))
QPS2_ETC_ss1_ast1 <- (2/60)*sum(((zi2_ETC_ss1_ast1 - 0.01)^2))
QPS3_ETC_ss1_ast1 <- (2/60)*sum(((zi3_ETC_ss1_ast1 - 0.01)^2))

QPS_ETC_ss1_ast1 <- QPS1_ETC_ss1_ast1 + QPS2_ETC_ss1_ast1 + QPS3_ETC_ss1_ast1
#AST1
VaR_ETC_ss1_astone1 <- VaR_ETC_astone1[,1]
realisedval_ETC_ss1_astone1 <- VR14_ETC[,4]
zi1_ETC_ss1_astone1 <- ifelse(realisedval_ETC_ss1_astone1 < VaR_ETC_ss1_astone1, 1, 0)
zi2_ETC_ss1_astone1 <- ifelse(realisedval_ETC_ss1_astone1 < VaR_ETC_ss1_astone1, 1, 0)*(1+((realisedval_ETC_ss1_astone1 - VaR_ETC_ss1_astone1)^2))
zi3_ETC_ss1_astone1 <- ifelse(realisedval_ETC_ss1_astone1 < VaR_ETC_ss1_astone1, 1, 0)*((realisedval_ETC_ss1_astone1 - VaR_ETC_ss1_astone1)/VaR_ETC_ss1_astone1)

QPS1_ETC_ss1_astone1 <- (2/60)*sum(((zi1_ETC_ss1_astone1 - 0.01)^2))
QPS2_ETC_ss1_astone1 <- (2/60)*sum(((zi2_ETC_ss1_astone1 - 0.01)^2))
QPS3_ETC_ss1_astone1 <- (2/60)*sum(((zi3_ETC_ss1_astone1 - 0.01)^2))

QPS_ETC_ss1_astone1 <- QPS1_ETC_ss1_astone1 + QPS2_ETC_ss1_astone1 + QPS3_ETC_ss1_astone1

# Sub Sample 2
#Normal
VaR_ETC_ss2_norm2 <- VaR_ETC_norm2[,1]
realisedval_ETC_ss2_norm2 <- VR21_ETC[,4]
zi1_ETC_ss2_norm2 <- ifelse(realisedval_ETC_ss2_norm2 < VaR_ETC_ss2_norm2, 1, 0)
zi2_ETC_ss2_norm2 <- ifelse(realisedval_ETC_ss2_norm2 < VaR_ETC_ss2_norm2, 1, 0)*(1+((realisedval_ETC_ss2_norm2 - VaR_ETC_ss2_norm2)^2))
zi3_ETC_ss2_norm2 <- ifelse(realisedval_ETC_ss2_norm2 < VaR_ETC_ss2_norm2, 1, 0)*((realisedval_ETC_ss2_norm2 - VaR_ETC_ss2_norm2)/VaR_ETC_ss2_norm2)

QPS1_ETC_ss2_norm2 <- (2/200)*sum(((zi1_ETC_ss2_norm2 - 0.01)^2))
QPS2_ETC_ss2_norm2 <- (2/200)*sum(((zi2_ETC_ss2_norm2 - 0.01)^2))
QPS3_ETC_ss2_norm2 <- (2/200)*sum(((zi3_ETC_ss2_norm2 - 0.01)^2))

QPS_ETC_ss2_norm2 <- QPS1_ETC_ss2_norm2 + QPS2_ETC_ss2_norm2 + QPS3_ETC_ss2_norm2 
#ST
VaR_ETC_ss2_ST2 <- VaR_ETC_std2[,1]
realisedval_ETC_ss2_ST2 <- VR22_ETC[,4]
zi1_ETC_ss2_ST2 <- ifelse(realisedval_ETC_ss2_ST2 < VaR_ETC_ss2_ST2, 1, 0)
zi2_ETC_ss2_ST2 <- ifelse(realisedval_ETC_ss2_ST2 < VaR_ETC_ss2_ST2, 1, 0)*(1+((realisedval_ETC_ss2_ST2 - VaR_ETC_ss2_ST2)^2))
zi3_ETC_ss2_ST2 <- ifelse(realisedval_ETC_ss2_ST2 < VaR_ETC_ss2_ST2, 1, 0)*((realisedval_ETC_ss2_ST2 - VaR_ETC_ss2_ST2)/VaR_ETC_ss2_ST2)

QPS1_ETC_ss2_ST2 <- (2/200)*sum(((zi1_ETC_ss2_ST2 - 0.01)^2))
QPS2_ETC_ss2_ST2 <- (2/200)*sum(((zi2_ETC_ss2_ST2 - 0.01)^2))
QPS3_ETC_ss2_ST2 <- (2/200)*sum(((zi3_ETC_ss2_ST2 - 0.01)^2))

QPS_ETC_ss2_ST2 <- QPS1_ETC_ss2_ST2 + QPS2_ETC_ss2_ST2 + QPS3_ETC_ss2_ST2
#SST
VaR_ETC_ss2_SST2 <- VaR_ETC_sstd2[,1]
realisedval_ETC_ss2_SST2 <- VR23_ETC[,4]
zi1_ETC_ss2_SST2 <- ifelse(realisedval_ETC_ss2_SST2 < VaR_ETC_ss2_SST2, 1, 0)
zi2_ETC_ss2_SST2 <- ifelse(realisedval_ETC_ss2_SST2 < VaR_ETC_ss2_SST2, 1, 0)*(1+((realisedval_ETC_ss2_SST2 - VaR_ETC_ss2_SST2)^2))
zi3_ETC_ss2_SST2 <- ifelse(realisedval_ETC_ss2_SST2 < VaR_ETC_ss2_SST2, 1, 0)*((realisedval_ETC_ss2_SST2 - VaR_ETC_ss2_SST2)/VaR_ETC_ss2_SST2)

QPS1_ETC_ss2_SST2 <- (2/200)*sum(((zi1_ETC_ss2_SST2 - 0.01)^2))
QPS2_ETC_ss2_SST2 <- (2/200)*sum(((zi2_ETC_ss2_SST2 - 0.01)^2))
QPS3_ETC_ss2_SST2 <- (2/200)*sum(((zi3_ETC_ss2_SST2 - 0.01)^2))

QPS_ETC_ss2_SST2 <- QPS1_ETC_ss2_SST2 + QPS2_ETC_ss2_SST2 + QPS3_ETC_ss2_SST2
#AST
VaR_ETC_ss2_ast2 <- VaR_ETC_ast2[,1]
realisedval_ETC_ss2_ast2 <- VR24_ETC[,4]
zi1_ETC_ss2_ast2 <- ifelse(realisedval_ETC_ss2_ast2 < VaR_ETC_ss2_ast2, 1, 0)
zi2_ETC_ss2_ast2 <- ifelse(realisedval_ETC_ss2_ast2 < VaR_ETC_ss2_ast2, 1, 0)*(1+((realisedval_ETC_ss2_ast2 - VaR_ETC_ss2_ast2)^2))
zi3_ETC_ss2_ast2 <- ifelse(realisedval_ETC_ss2_ast2 < VaR_ETC_ss2_ast2, 1, 0)*((realisedval_ETC_ss2_ast2 - VaR_ETC_ss2_ast2)/VaR_ETC_ss2_ast2)

QPS1_ETC_ss2_ast2 <- (2/200)*sum(((zi1_ETC_ss2_ast2 - 0.01)^2))
QPS2_ETC_ss2_ast2 <- (2/200)*sum(((zi2_ETC_ss2_ast2 - 0.01)^2))
QPS3_ETC_ss2_ast2 <- (2/200)*sum(((zi3_ETC_ss2_ast2 - 0.01)^2))

QPS_ETC_ss2_ast2 <- QPS1_ETC_ss2_ast2 + QPS2_ETC_ss2_ast2 + QPS3_ETC_ss2_ast2
#AST1
VaR_ETC_ss2_astone2 <- VaR_ETC_astone2[,1]
realisedval_ETC_ss2_astone2 <- VR24_ETC[,4]
zi1_ETC_ss2_astone2 <- ifelse(realisedval_ETC_ss2_astone2 < VaR_ETC_ss2_astone2, 1, 0)
zi2_ETC_ss2_astone2 <- ifelse(realisedval_ETC_ss2_astone2 < VaR_ETC_ss2_astone2, 1, 0)*(1+((realisedval_ETC_ss2_astone2 - VaR_ETC_ss2_astone2)^2))
zi3_ETC_ss2_astone2 <- ifelse(realisedval_ETC_ss2_astone2 < VaR_ETC_ss2_astone2, 1, 0)*((realisedval_ETC_ss2_astone2 - VaR_ETC_ss2_astone2)/VaR_ETC_ss2_astone2)

QPS1_ETC_ss2_astone2 <- (2/200)*sum(((zi1_ETC_ss2_astone2 - 0.01)^2))
QPS2_ETC_ss2_astone2 <- (2/200)*sum(((zi2_ETC_ss2_astone2 - 0.01)^2))
QPS3_ETC_ss2_astone2 <- (2/200)*sum(((zi3_ETC_ss2_astone2 - 0.01)^2))

QPS_ETC_ss2_astone2 <- QPS1_ETC_ss2_astone2 + QPS2_ETC_ss2_astone2 + QPS3_ETC_ss2_astone2

# Sub Sample 3
#Normal
VaR_ETC_ss3_norm3 <- VaR_ETC_norm3[,1]
realisedval_ETC_ss3_norm3 <- VR31_ETC[,4]
zi1_ETC_ss3_norm3 <- ifelse(realisedval_ETC_ss3_norm3 < VaR_ETC_ss3_norm3, 1, 0)
zi2_ETC_ss3_norm3 <- ifelse(realisedval_ETC_ss3_norm3 < VaR_ETC_ss3_norm3, 1, 0)*(1+((realisedval_ETC_ss3_norm3 - VaR_ETC_ss3_norm3)^2))
zi3_ETC_ss3_norm3 <- ifelse(realisedval_ETC_ss3_norm3 < VaR_ETC_ss3_norm3, 1, 0)*((realisedval_ETC_ss3_norm3 - VaR_ETC_ss3_norm3)/VaR_ETC_ss3_norm3)

QPS1_ETC_ss3_norm3 <- (2/100)*sum(((zi1_ETC_ss3_norm3 - 0.01)^2))
QPS2_ETC_ss3_norm3 <- (2/100)*sum(((zi2_ETC_ss3_norm3 - 0.01)^2))
QPS3_ETC_ss3_norm3 <- (2/100)*sum(((zi3_ETC_ss3_norm3 - 0.01)^2))

QPS_ETC_ss3_norm3 <- QPS1_ETC_ss3_norm3 + QPS2_ETC_ss3_norm3 + QPS3_ETC_ss3_norm3 
#ST
VaR_ETC_ss3_ST3 <- VaR_ETC_std3[,1]
realisedval_ETC_ss3_ST3 <- VR32_ETC[,4]
zi1_ETC_ss3_ST3 <- ifelse(realisedval_ETC_ss3_ST3 < VaR_ETC_ss3_ST3, 1, 0)
zi2_ETC_ss3_ST3 <- ifelse(realisedval_ETC_ss3_ST3 < VaR_ETC_ss3_ST3, 1, 0)*(1+((realisedval_ETC_ss3_ST3 - VaR_ETC_ss3_ST3)^2))
zi3_ETC_ss3_ST3 <- ifelse(realisedval_ETC_ss3_ST3 < VaR_ETC_ss3_ST3, 1, 0)*((realisedval_ETC_ss3_ST3 - VaR_ETC_ss3_ST3)/VaR_ETC_ss3_ST3)

QPS1_ETC_ss3_ST3 <- (2/100)*sum(((zi1_ETC_ss3_ST3 - 0.01)^2))
QPS2_ETC_ss3_ST3 <- (2/100)*sum(((zi2_ETC_ss3_ST3 - 0.01)^2))
QPS3_ETC_ss3_ST3 <- (2/100)*sum(((zi3_ETC_ss3_ST3 - 0.01)^2))

QPS_ETC_ss3_ST3 <- QPS1_ETC_ss3_ST3 + QPS2_ETC_ss3_ST3 + QPS3_ETC_ss3_ST3
#SST
VaR_ETC_ss3_SST3 <- VaR_ETC_sstd3[,1]
realisedval_ETC_ss3_SST3 <- VR33_ETC[,4]
zi1_ETC_ss3_SST3 <- ifelse(realisedval_ETC_ss3_SST3 < VaR_ETC_ss3_SST3, 1, 0)
zi2_ETC_ss3_SST3 <- ifelse(realisedval_ETC_ss3_SST3 < VaR_ETC_ss3_SST3, 1, 0)*(1+((realisedval_ETC_ss3_SST3 - VaR_ETC_ss3_SST3)^2))
zi3_ETC_ss3_SST3 <- ifelse(realisedval_ETC_ss3_SST3 < VaR_ETC_ss3_SST3, 1, 0)*((realisedval_ETC_ss3_SST3 - VaR_ETC_ss3_SST3)/VaR_ETC_ss3_SST3)

QPS1_ETC_ss3_SST3 <- (2/100)*sum(((zi1_ETC_ss3_SST3 - 0.01)^2))
QPS2_ETC_ss3_SST3 <- (2/100)*sum(((zi2_ETC_ss3_SST3 - 0.01)^2))
QPS3_ETC_ss3_SST3 <- (2/100)*sum(((zi3_ETC_ss3_SST3 - 0.01)^2))

QPS_ETC_ss3_SST3 <- QPS1_ETC_ss3_SST3 + QPS2_ETC_ss3_SST3 + QPS3_ETC_ss3_SST3
#AST
VaR_ETC_ss3_ast3 <- VaR_ETC_ast3[,1]
realisedval_ETC_ss3_ast3 <- VR34_ETC[,4]
zi1_ETC_ss3_ast3 <- ifelse(realisedval_ETC_ss3_ast3 < VaR_ETC_ss3_ast3, 1, 0)
zi2_ETC_ss3_ast3 <- ifelse(realisedval_ETC_ss3_ast3 < VaR_ETC_ss3_ast3, 1, 0)*(1+((realisedval_ETC_ss3_ast3 - VaR_ETC_ss3_ast3)^2))
zi3_ETC_ss3_ast3 <- ifelse(realisedval_ETC_ss3_ast3 < VaR_ETC_ss3_ast3, 1, 0)*((realisedval_ETC_ss3_ast3 - VaR_ETC_ss3_ast3)/VaR_ETC_ss3_ast3)

QPS1_ETC_ss3_ast3 <- (2/100)*sum(((zi1_ETC_ss3_ast3 - 0.01)^2))
QPS2_ETC_ss3_ast3 <- (2/100)*sum(((zi2_ETC_ss3_ast3 - 0.01)^2))
QPS3_ETC_ss3_ast3 <- (2/100)*sum(((zi3_ETC_ss3_ast3 - 0.01)^2))

QPS_ETC_ss3_ast3 <- QPS1_ETC_ss3_ast3 + QPS2_ETC_ss3_ast3 + QPS3_ETC_ss3_ast3
#AST1
VaR_ETC_ss3_astone3 <- VaR_ETC_astone3[,1]
realisedval_ETC_ss3_astone3 <- VR34_ETC[,4]
zi1_ETC_ss3_astone3 <- ifelse(realisedval_ETC_ss3_astone3 < VaR_ETC_ss3_astone3, 1, 0)
zi2_ETC_ss3_astone3 <- ifelse(realisedval_ETC_ss3_astone3 < VaR_ETC_ss3_astone3, 1, 0)*(1+((realisedval_ETC_ss3_astone3 - VaR_ETC_ss3_astone3)^2))
zi3_ETC_ss3_astone3 <- ifelse(realisedval_ETC_ss3_astone3 < VaR_ETC_ss3_astone3, 1, 0)*((realisedval_ETC_ss3_astone3 - VaR_ETC_ss3_astone3)/VaR_ETC_ss3_astone3)

QPS1_ETC_ss3_astone3 <- (2/100)*sum(((zi1_ETC_ss3_astone3 - 0.01)^2))
QPS2_ETC_ss3_astone3 <- (2/100)*sum(((zi2_ETC_ss3_astone3 - 0.01)^2))
QPS3_ETC_ss3_astone3 <- (2/100)*sum(((zi3_ETC_ss3_astone3 - 0.01)^2))

QPS_ETC_ss3_astone3 <- QPS1_ETC_ss3_astone3 + QPS2_ETC_ss3_astone3 + QPS3_ETC_ss3_astone3

QPS_ETC_fs_GAS <- c(QPS_ETC_fs_norm0, QPS_ETC_fs_ST0, QPS_ETC_fs_SST0, QPS_ETC_fs_ast0, QPS_ETC_fs_astone0)
QPS_ETC_ss1_GAS <- c(QPS_ETC_ss1_norm1, QPS_ETC_ss1_ST1, QPS_ETC_ss1_SST1, QPS_ETC_ss1_ast1, QPS_ETC_ss1_astone1)
QPS_ETC_ss2_GAS <- c(QPS_ETC_ss2_norm2, QPS_ETC_ss2_ST2, QPS_ETC_ss2_SST2, QPS_ETC_ss2_ast2, QPS_ETC_ss2_astone2)
QPS_ETC_ss3_GAS <- c(QPS_ETC_ss3_norm3, QPS_ETC_ss3_ST3, QPS_ETC_ss3_SST3, QPS_ETC_ss3_ast3, QPS_ETC_ss3_astone3)

QPS_ETC_GAS <- matrix(data = 0, nrow = 4, ncol = 5,byrow = TRUE)
QPS_ETC_GAS <- rbind(QPS_ETC_fs_GAS, QPS_ETC_ss1_GAS, QPS_ETC_ss2_GAS, QPS_ETC_ss3_GAS)
colnames(QPS_ETC_GAS) <- c("Norm", "ST", "SST", "AST", "AST1")
rownames(QPS_ETC_GAS) <- c("Full Sample", "SS1", "SS2", "SS3")

QPS_GAS <- rbind(QPS_BTC_GAS, QPS_ETH_GAS, QPS_LTC_GAS, QPS_XMR_GAS, QPS_ETC_GAS)
View(QPS_GAS)


