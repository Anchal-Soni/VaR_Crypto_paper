# =============================================================================
# 13_tables_qps_ae.R
#
# Purpose : Per-coin tables combining QPS and AE for EGARCH, EGARCH without dummies
#           and GAS.
# Inputs  : objects from 07-10
# Outputs : LaTeX tables printed to the console
# Paper   : Table 5
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

QPS_BTC
QPS_BTC_fs_wo
rownames(QPS_BTC_fs_wo) <- "FS_WO"
Head <- c("Norm", "STD", "SSTD", "AST", "AST1")
QPS_BTC_GAS
BTC_Table <- rbind("QPS", round(QPS_BTC,4), round(QPS_BTC_fs_wo,4), Head, round(QPS_BTC_GAS,4))

AE_BTC
AE_BTC_wo
BTC_Table_AE <- rbind("Actual Error", round(AE_BTC,4), round(AE_BTC_wo,4), Head, round(AE_BTC_GAS,4))

BTC_Table_F <- cbind(BTC_Table, BTC_Table_AE)
View(BTC_Table_F)


QPS_ETH
QPS_ETH_fs_wo
rownames(QPS_ETH_fs_wo) <- "FS_WO"
Head <- c("Norm", "STD", "SSTD", "AST", "AST1")
QPS_ETH_GAS
ETH_Table <- rbind("QPS", round(QPS_ETH,4), round(QPS_ETH_fs_wo,4), Head, round(QPS_ETH_GAS,4))

AE_ETH
AE_ETH_wo
ETH_Table_AE <- rbind("Actual Error", round(AE_ETH,4), round(AE_ETH_wo,4), Head, round(AE_ETH_GAS,4))

ETH_Table_F <- cbind(ETH_Table, ETH_Table_AE)
View(ETH_Table_F)


QPS_LTC
QPS_LTC_fs_wo
rownames(QPS_LTC_fs_wo) <- "FS_WO"
Head <- c("Norm", "STD", "SSTD", "AST", "AST1")
QPS_LTC_GAS
LTC_Table <- rbind("QPS", round(QPS_LTC,4), round(QPS_LTC_fs_wo,4), Head, round(QPS_LTC_GAS,4))

AE_LTC
AE_LTC_wo
LTC_Table_AE <- rbind("Actual Error", round(AE_LTC,4), round(AE_LTC_wo,4), Head, round(AE_LTC_GAS,4))

LTC_Table_F <- cbind(LTC_Table, LTC_Table_AE)
View(LTC_Table_F)


QPS_XMR
QPS_XMR_fs_wo
rownames(QPS_XMR_fs_wo) <- "FS_WO"
Head <- c("Norm", "STD", "SSTD", "AST", "AST1")
QPS_XMR_GAS
XMR_Table <- rbind("QPS", round(QPS_XMR,4), round(QPS_XMR_fs_wo,4), Head, round(QPS_XMR_GAS,4))

AE_XMR
AE_XMR_wo
XMR_Table_AE <- rbind("Actual Error", round(AE_XMR,4), round(AE_XMR_wo,4), Head, round(AE_XMR_GAS,4))

XMR_Table_F <- cbind(XMR_Table, XMR_Table_AE)
View(XMR_Table_F)


QPS_ETC
QPS_ETC_fs_wo
rownames(QPS_ETC_fs_wo) <- "FS_WO"
Head <- c("Norm", "STD", "SSTD", "AST", "AST1")
QPS_ETC_GAS
ETC_Table <- rbind("QPS", round(QPS_ETC,4), round(QPS_ETC_fs_wo,4), Head, round(QPS_ETC_GAS,4))

AE_ETC
AE_ETC_wo
ETC_Table_AE <- rbind("Actual Error", round(AE_ETC,4), round(AE_ETC_wo,4), Head, round(AE_ETC_GAS,4))

ETC_Table_F <- cbind(ETC_Table, ETC_Table_AE)
View(ETC_Table_F)



xtable(BTC_Table_F)
xtable(ETH_Table_F)
xtable(LTC_Table_F)
xtable(XMR_Table_F)
xtable(ETC_Table_F)
