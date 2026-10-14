# =============================================================================
# 12_tables_gas.R
#
# Purpose : AIC, MAPE, RMSE and Kupiec/Christoffersen tables for the GAS models.
# Inputs  : objects from 06
# Outputs : LaTeX tables printed to the console
# Paper   : Table 4, Tables A.9-A.10
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

### AIC, MAPE, RMSE, Kupiec, Chrisoffersen
AIC_BTC_Val <- c(Fit_BTC_norm0@Estimates$IC[1], Fit_BTC_std0@Estimates$IC[1], Fit_BTC_sstd0@Estimates$IC[1], Fit_BTC_ast0@Estimates$IC[1], Fit_BTC_astone0@Estimates$IC[1],
                 Fit_BTC_norm1@Estimates$IC[1], Fit_BTC_std1@Estimates$IC[1], Fit_BTC_sstd1@Estimates$IC[1], Fit_BTC_ast1@Estimates$IC[1], Fit_BTC_astone1@Estimates$IC[1],
                 Fit_BTC_norm2@Estimates$IC[1], Fit_BTC_std2@Estimates$IC[1], Fit_BTC_sstd2@Estimates$IC[1], Fit_BTC_ast2@Estimates$IC[1], Fit_BTC_astone2@Estimates$IC[1],
                 Fit_BTC_norm3@Estimates$IC[1], Fit_BTC_std3@Estimates$IC[1], Fit_BTC_sstd3@Estimates$IC[1], Fit_BTC_ast3@Estimates$IC[1], Fit_BTC_astone3@Estimates$IC[1])

AIC_BTC_GAS <- matrix(data = AIC_BTC_Val, nrow = 5)
colnames(AIC_BTC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(AIC_BTC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")
View(AIC_BTC_GAS)

AIC_ETH_Val <- c(Fit_ETH_norm0@Estimates$IC[1], Fit_ETH_std0@Estimates$IC[1], Fit_ETH_sstd0@Estimates$IC[1], Fit_ETH_ast0@Estimates$IC[1], Fit_ETH_astone0@Estimates$IC[1],
                 Fit_ETH_norm1@Estimates$IC[1], Fit_ETH_std1@Estimates$IC[1], Fit_ETH_sstd1@Estimates$IC[1], Fit_ETH_ast1@Estimates$IC[1], Fit_ETH_astone1@Estimates$IC[1],
                 Fit_ETH_norm2@Estimates$IC[1], Fit_ETH_std2@Estimates$IC[1], Fit_ETH_sstd2@Estimates$IC[1], Fit_ETH_ast2@Estimates$IC[1], Fit_ETH_astone2@Estimates$IC[1],
                 Fit_ETH_norm3@Estimates$IC[1], Fit_ETH_std3@Estimates$IC[1], Fit_ETH_sstd3@Estimates$IC[1], Fit_ETH_ast3@Estimates$IC[1], Fit_ETH_astone3@Estimates$IC[1])

AIC_ETH_GAS <- matrix(data = AIC_ETH_Val, nrow = 5)
colnames(AIC_ETH_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(AIC_ETH_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


AIC_LTC_Val <- c(Fit_LTC_norm0@Estimates$IC[1], Fit_LTC_std0@Estimates$IC[1], Fit_LTC_sstd0@Estimates$IC[1], Fit_LTC_ast0@Estimates$IC[1], Fit_LTC_astone0@Estimates$IC[1],
                 Fit_LTC_norm1@Estimates$IC[1], Fit_LTC_std1@Estimates$IC[1], Fit_LTC_sstd1@Estimates$IC[1], Fit_LTC_ast1@Estimates$IC[1], Fit_LTC_astone1@Estimates$IC[1],
                 Fit_LTC_norm2@Estimates$IC[1], Fit_LTC_std2@Estimates$IC[1], Fit_LTC_sstd2@Estimates$IC[1], Fit_LTC_ast2@Estimates$IC[1], Fit_LTC_astone2@Estimates$IC[1],
                 Fit_LTC_norm3@Estimates$IC[1], Fit_LTC_std3@Estimates$IC[1], Fit_LTC_sstd3@Estimates$IC[1], Fit_LTC_ast3@Estimates$IC[1], Fit_LTC_astone3@Estimates$IC[1])

AIC_LTC_GAS <- matrix(data = AIC_LTC_Val, nrow = 5)
colnames(AIC_LTC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(AIC_LTC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


AIC_XMR_Val <- c(Fit_XMR_norm0@Estimates$IC[1], Fit_XMR_std0@Estimates$IC[1], Fit_XMR_sstd0@Estimates$IC[1], Fit_XMR_ast0@Estimates$IC[1], Fit_XMR_astone0@Estimates$IC[1],
                 Fit_XMR_norm1@Estimates$IC[1], Fit_XMR_std1@Estimates$IC[1], Fit_XMR_sstd1@Estimates$IC[1], Fit_XMR_ast1@Estimates$IC[1], Fit_XMR_astone1@Estimates$IC[1],
                 Fit_XMR_norm2@Estimates$IC[1], Fit_XMR_std2@Estimates$IC[1], Fit_XMR_sstd2@Estimates$IC[1], Fit_XMR_ast2@Estimates$IC[1], Fit_XMR_astone2@Estimates$IC[1],
                 Fit_XMR_norm3@Estimates$IC[1], Fit_XMR_std3@Estimates$IC[1], Fit_XMR_sstd3@Estimates$IC[1], Fit_XMR_ast3@Estimates$IC[1], Fit_XMR_astone3@Estimates$IC[1])

AIC_XMR_GAS <- matrix(data = AIC_XMR_Val, nrow = 5)
colnames(AIC_XMR_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(AIC_XMR_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


AIC_ETC_Val <- c(Fit_ETC_norm0@Estimates$IC[1], Fit_ETC_std0@Estimates$IC[1], Fit_ETC_sstd0@Estimates$IC[1], Fit_ETC_ast0@Estimates$IC[1], Fit_ETC_astone0@Estimates$IC[1],
                 Fit_ETC_norm1@Estimates$IC[1], Fit_ETC_std1@Estimates$IC[1], Fit_ETC_sstd1@Estimates$IC[1], Fit_ETC_ast1@Estimates$IC[1], Fit_ETC_astone1@Estimates$IC[1],
                 Fit_ETC_norm2@Estimates$IC[1], Fit_ETC_std2@Estimates$IC[1], Fit_ETC_sstd2@Estimates$IC[1], Fit_ETC_ast2@Estimates$IC[1], Fit_ETC_astone2@Estimates$IC[1],
                 Fit_ETC_norm3@Estimates$IC[1], Fit_ETC_std3@Estimates$IC[1], Fit_ETC_sstd3@Estimates$IC[1], Fit_ETC_ast3@Estimates$IC[1], Fit_ETC_astone3@Estimates$IC[1])

AIC_ETC_GAS <- matrix(data = AIC_ETC_Val, nrow = 5)
colnames(AIC_ETC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(AIC_ETC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


AIC_GAS <- rbind("BTC", round(AIC_BTC_GAS,3),
                 "ETH", round(AIC_ETH_GAS,3),
                 "LTC", round(AIC_LTC_GAS,3),
                 "XMR", round(AIC_XMR_GAS,3),
                 "ETC", round(AIC_ETC_GAS,3))

xtable(AIC_GAS)

############################### :::::::::::::::::::::::::::::: ###############################

MAPE_BTC_Val <- c(MAPE_BTC_norm0, MAPE_BTC_std0, MAPE_BTC_sstd0, MAPE_BTC_ast0, MAPE_BTC_astone0,
                  MAPE_BTC_norm1, MAPE_BTC_std1, MAPE_BTC_sstd1, MAPE_BTC_ast1, MAPE_BTC_astone1,
                  MAPE_BTC_norm2, MAPE_BTC_std2, MAPE_BTC_sstd2, MAPE_BTC_ast2, MAPE_BTC_astone2,
                  MAPE_BTC_norm3, MAPE_BTC_std3, MAPE_BTC_sstd3, MAPE_BTC_ast3, MAPE_BTC_astone3)

MAPE_BTC_GAS <- matrix(data = MAPE_BTC_Val, nrow = 5)
colnames(MAPE_BTC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_BTC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


MAPE_ETH_Val <- c(MAPE_ETH_norm0, MAPE_ETH_std0, MAPE_ETH_sstd0, MAPE_ETH_ast0, MAPE_ETH_astone0,
                  MAPE_ETH_norm1, MAPE_ETH_std1, MAPE_ETH_sstd1, MAPE_ETH_ast1, MAPE_ETH_astone1,
                  MAPE_ETH_norm2, MAPE_ETH_std2, MAPE_ETH_sstd2, MAPE_ETH_ast2, MAPE_ETH_astone2,
                  MAPE_ETH_norm3, MAPE_ETH_std3, MAPE_ETH_sstd3, MAPE_ETH_ast3, MAPE_ETH_astone3)

MAPE_ETH_GAS <- matrix(data = MAPE_ETH_Val, nrow = 5)
colnames(MAPE_ETH_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_ETH_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


MAPE_LTC_Val <- c(MAPE_LTC_norm0, MAPE_LTC_std0, MAPE_LTC_sstd0, MAPE_LTC_ast0, MAPE_LTC_astone0,
                  MAPE_LTC_norm1, MAPE_LTC_std1, MAPE_LTC_sstd1, MAPE_LTC_ast1, MAPE_LTC_astone1,
                  MAPE_LTC_norm2, MAPE_LTC_std2, MAPE_LTC_sstd2, MAPE_LTC_ast2, MAPE_LTC_astone2,
                  MAPE_LTC_norm3, MAPE_LTC_std3, MAPE_LTC_sstd3, MAPE_LTC_ast3, MAPE_LTC_astone3)

MAPE_LTC_GAS <- matrix(data = MAPE_LTC_Val, nrow = 5)
colnames(MAPE_LTC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_LTC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


MAPE_XMR_Val <- c(MAPE_XMR_norm0, MAPE_XMR_std0, MAPE_XMR_sstd0, MAPE_XMR_ast0, MAPE_XMR_astone0,
                  MAPE_XMR_norm1, MAPE_XMR_std1, MAPE_XMR_sstd1, MAPE_XMR_ast1, MAPE_XMR_astone1,
                  MAPE_XMR_norm2, MAPE_XMR_std2, MAPE_XMR_sstd2, MAPE_XMR_ast2, MAPE_XMR_astone2,
                  MAPE_XMR_norm3, MAPE_XMR_std3, MAPE_XMR_sstd3, MAPE_XMR_ast3, MAPE_XMR_astone3)

MAPE_XMR_GAS <- matrix(data = MAPE_XMR_Val, nrow = 5)
colnames(MAPE_XMR_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_XMR_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


MAPE_ETC_Val <- c(MAPE_ETC_norm0, MAPE_ETC_std0, MAPE_ETC_sstd0, MAPE_ETC_ast0, MAPE_ETC_astone0,
                  MAPE_ETC_norm1, MAPE_ETC_std1, MAPE_ETC_sstd1, MAPE_ETC_ast1, MAPE_ETC_astone1,
                  MAPE_ETC_norm2, MAPE_ETC_std2, MAPE_ETC_sstd2, MAPE_ETC_ast2, MAPE_ETC_astone2,
                  MAPE_ETC_norm3, MAPE_ETC_std3, MAPE_ETC_sstd3, MAPE_ETC_ast3, MAPE_ETC_astone3)

MAPE_ETC_GAS <- matrix(data = MAPE_ETC_Val, nrow = 5)
colnames(MAPE_ETC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(MAPE_ETC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


MAPE_GAS <- rbind("BTC", round(MAPE_BTC_GAS,3),
                 "ETH", round(MAPE_ETH_GAS,3),
                 "LTC", round(MAPE_LTC_GAS,3),
                 "XMR", round(MAPE_XMR_GAS,3),
                 "ETC", round(MAPE_ETC_GAS,3))

t(MAPE_BTC_GAS)

xtable(MAPE_GAS)
#########################

RMSE_BTC_Val <- c(RMSE_BTC_norm0, RMSE_BTC_std0, RMSE_BTC_sstd0, RMSE_BTC_ast0, RMSE_BTC_astone0,
                  RMSE_BTC_norm1, RMSE_BTC_std1, RMSE_BTC_sstd1, RMSE_BTC_ast1, RMSE_BTC_astone1,
                  RMSE_BTC_norm2, RMSE_BTC_std2, RMSE_BTC_sstd2, RMSE_BTC_ast2, RMSE_BTC_astone2,
                  RMSE_BTC_norm3, RMSE_BTC_std3, RMSE_BTC_sstd3, RMSE_BTC_ast3, RMSE_BTC_astone3)

RMSE_BTC_GAS <- matrix(data = RMSE_BTC_Val, nrow = 5)
colnames(RMSE_BTC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_BTC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


RMSE_ETH_Val <- c(RMSE_ETH_norm0, RMSE_ETH_std0, RMSE_ETH_sstd0, RMSE_ETH_ast0, RMSE_ETH_astone0,
                  RMSE_ETH_norm1, RMSE_ETH_std1, RMSE_ETH_sstd1, RMSE_ETH_ast1, RMSE_ETH_astone1,
                  RMSE_ETH_norm2, RMSE_ETH_std2, RMSE_ETH_sstd2, RMSE_ETH_ast2, RMSE_ETH_astone2,
                  RMSE_ETH_norm3, RMSE_ETH_std3, RMSE_ETH_sstd3, RMSE_ETH_ast3, RMSE_ETH_astone3)

RMSE_ETH_GAS <- matrix(data = RMSE_ETH_Val, nrow = 5)
colnames(RMSE_ETH_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_ETH_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


RMSE_LTC_Val <- c(RMSE_LTC_norm0, RMSE_LTC_std0, RMSE_LTC_sstd0, RMSE_LTC_ast0, RMSE_LTC_astone0,
                  RMSE_LTC_norm1, RMSE_LTC_std1, RMSE_LTC_sstd1, RMSE_LTC_ast1, RMSE_LTC_astone1,
                  RMSE_LTC_norm2, RMSE_LTC_std2, RMSE_LTC_sstd2, RMSE_LTC_ast2, RMSE_LTC_astone2,
                  RMSE_LTC_norm3, RMSE_LTC_std3, RMSE_LTC_sstd3, RMSE_LTC_ast3, RMSE_LTC_astone3)

RMSE_LTC_GAS <- matrix(data = RMSE_LTC_Val, nrow = 5)
colnames(RMSE_LTC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_LTC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


RMSE_XMR_Val <- c(RMSE_XMR_norm0, RMSE_XMR_std0, RMSE_XMR_sstd0, RMSE_XMR_ast0, RMSE_XMR_astone0,
                  RMSE_XMR_norm1, RMSE_XMR_std1, RMSE_XMR_sstd1, RMSE_XMR_ast1, RMSE_XMR_astone1,
                  RMSE_XMR_norm2, RMSE_XMR_std2, RMSE_XMR_sstd2, RMSE_XMR_ast2, RMSE_XMR_astone2,
                  RMSE_XMR_norm3, RMSE_XMR_std3, RMSE_XMR_sstd3, RMSE_XMR_ast3, RMSE_XMR_astone3)

RMSE_XMR_GAS <- matrix(data = RMSE_XMR_Val, nrow = 5)
colnames(RMSE_XMR_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_XMR_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


RMSE_ETC_Val <- c(RMSE_ETC_norm0, RMSE_ETC_std0, RMSE_ETC_sstd0, RMSE_ETC_ast0, RMSE_ETC_astone0,
                  RMSE_ETC_norm1, RMSE_ETC_std1, RMSE_ETC_sstd1, RMSE_ETC_ast1, RMSE_ETC_astone1,
                  RMSE_ETC_norm2, RMSE_ETC_std2, RMSE_ETC_sstd2, RMSE_ETC_ast2, RMSE_ETC_astone2,
                  RMSE_ETC_norm3, RMSE_ETC_std3, RMSE_ETC_sstd3, RMSE_ETC_ast3, RMSE_ETC_astone3)

RMSE_ETC_GAS <- matrix(data = RMSE_ETC_Val, nrow = 5)
colnames(RMSE_ETC_GAS) <- c("Full Sample", "SubSample 1", "SubSample 2","SubSample 3")
rownames(RMSE_ETC_GAS) <- c("Normal", "STD", "SSTD", "AST", "AST1")


RMSE_GAS <- rbind("BTC", round(RMSE_BTC_GAS,3),
                  "ETH", round(RMSE_ETH_GAS,3),
                  "LTC", round(RMSE_LTC_GAS,3),
                  "XMR", round(RMSE_XMR_GAS,3),
                  "ETC", round(RMSE_ETC_GAS,3))

View(RMSE_GAS)

xtable(RMSE_GAS)

############################### :::::::::::::::::::::::::::::: ###############################

VaRBacktest_BTC_norm0$LRcc

# Backtesting results
# Kupiec
ss1_backtest_BTC_GAS <- 
  c(paste0(round(VaRBacktest_BTC_norm1$LRuc[1],3),"(",round(VaRBacktest_BTC_norm1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_std1$LRuc[1],3),"(",round(VaRBacktest_BTC_std1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_sstd1$LRuc[1],3),"(",round(VaRBacktest_BTC_sstd1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_ast1$LRuc[1],3),"(",round(VaRBacktest_BTC_ast1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_astone1$LRuc[1],3),"(",round(VaRBacktest_BTC_astone1$LRuc[2],3),")"))

ss2_backtest_BTC_GAS <- 
  c(paste0(round(VaRBacktest_BTC_norm2$LRuc[1],3),"(",round(VaRBacktest_BTC_norm2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_std2$LRuc[1],3),"(",round(VaRBacktest_BTC_std2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_sstd2$LRuc[1],3),"(",round(VaRBacktest_BTC_sstd2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_ast2$LRuc[1],3),"(",round(VaRBacktest_BTC_ast2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_astone2$LRuc[1],3),"(",round(VaRBacktest_BTC_astone2$LRuc[2],3),")"))

ss3_backtest_BTC_GAS <- 
  c(paste0(round(VaRBacktest_BTC_norm3$LRuc[1],3),"(",round(VaRBacktest_BTC_norm3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_std3$LRuc[1],3),"(",round(VaRBacktest_BTC_std3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_sstd3$LRuc[1],3),"(",round(VaRBacktest_BTC_sstd3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_ast3$LRuc[1],3),"(",round(VaRBacktest_BTC_ast3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_BTC_astone3$LRuc[1],3),"(",round(VaRBacktest_BTC_astone3$LRuc[2],3),")"))


ss1_backtest_ETH_GAS <- 
  c(paste0(round(VaRBacktest_ETH_norm1$LRuc[1],3),"(",round(VaRBacktest_ETH_norm1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_std1$LRuc[1],3),"(",round(VaRBacktest_ETH_std1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_sstd1$LRuc[1],3),"(",round(VaRBacktest_ETH_sstd1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_ast1$LRuc[1],3),"(",round(VaRBacktest_ETH_ast1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_astone1$LRuc[1],3),"(",round(VaRBacktest_ETH_astone1$LRuc[2],3),")"))

ss2_backtest_ETH_GAS <- 
  c(paste0(round(VaRBacktest_ETH_norm2$LRuc[1],3),"(",round(VaRBacktest_ETH_norm2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_std2$LRuc[1],3),"(",round(VaRBacktest_ETH_std2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_sstd2$LRuc[1],3),"(",round(VaRBacktest_ETH_sstd2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_ast2$LRuc[1],3),"(",round(VaRBacktest_ETH_ast2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_astone2$LRuc[1],3),"(",round(VaRBacktest_ETH_astone2$LRuc[2],3),")"))

ss3_backtest_ETH_GAS <- 
  c(paste0(round(VaRBacktest_ETH_norm3$LRuc[1],3),"(",round(VaRBacktest_ETH_norm3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_std3$LRuc[1],3),"(",round(VaRBacktest_ETH_std3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_sstd3$LRuc[1],3),"(",round(VaRBacktest_ETH_sstd3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_ast3$LRuc[1],3),"(",round(VaRBacktest_ETH_ast3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETH_astone3$LRuc[1],3),"(",round(VaRBacktest_ETH_astone3$LRuc[2],3),")"))


ss1_backtest_LTC_GAS <- 
  c(paste0(round(VaRBacktest_LTC_norm1$LRuc[1],3),"(",round(VaRBacktest_LTC_norm1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_std1$LRuc[1],3),"(",round(VaRBacktest_LTC_std1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_sstd1$LRuc[1],3),"(",round(VaRBacktest_LTC_sstd1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_ast1$LRuc[1],3),"(",round(VaRBacktest_LTC_ast1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_astone1$LRuc[1],3),"(",round(VaRBacktest_LTC_astone1$LRuc[2],3),")"))

ss2_backtest_LTC_GAS <- 
  c(paste0(round(VaRBacktest_LTC_norm2$LRuc[1],3),"(",round(VaRBacktest_LTC_norm2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_std2$LRuc[1],3),"(",round(VaRBacktest_LTC_std2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_sstd2$LRuc[1],3),"(",round(VaRBacktest_LTC_sstd2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_ast2$LRuc[1],3),"(",round(VaRBacktest_LTC_ast2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_astone2$LRuc[1],3),"(",round(VaRBacktest_LTC_astone2$LRuc[2],3),")"))

ss3_backtest_LTC_GAS <- 
  c(paste0(round(VaRBacktest_LTC_norm3$LRuc[1],3),"(",round(VaRBacktest_LTC_norm3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_std3$LRuc[1],3),"(",round(VaRBacktest_LTC_std3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_sstd3$LRuc[1],3),"(",round(VaRBacktest_LTC_sstd3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_ast3$LRuc[1],3),"(",round(VaRBacktest_LTC_ast3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_LTC_astone3$LRuc[1],3),"(",round(VaRBacktest_LTC_astone3$LRuc[2],3),")"))


ss1_backtest_XMR_GAS <- 
  c(paste0(round(VaRBacktest_XMR_norm1$LRuc[1],3),"(",round(VaRBacktest_XMR_norm1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_std1$LRuc[1],3),"(",round(VaRBacktest_XMR_std1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_sstd1$LRuc[1],3),"(",round(VaRBacktest_XMR_sstd1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_ast1$LRuc[1],3),"(",round(VaRBacktest_XMR_ast1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_astone1$LRuc[1],3),"(",round(VaRBacktest_XMR_astone1$LRuc[2],3),")"))

ss2_backtest_XMR_GAS <- 
  c(paste0(round(VaRBacktest_XMR_norm2$LRuc[1],3),"(",round(VaRBacktest_XMR_norm2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_std2$LRuc[1],3),"(",round(VaRBacktest_XMR_std2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_sstd2$LRuc[1],3),"(",round(VaRBacktest_XMR_sstd2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_ast2$LRuc[1],3),"(",round(VaRBacktest_XMR_ast2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_astone2$LRuc[1],3),"(",round(VaRBacktest_XMR_astone2$LRuc[2],3),")"))

ss3_backtest_XMR_GAS <- 
  c(paste0(round(VaRBacktest_XMR_norm3$LRuc[1],3),"(",round(VaRBacktest_XMR_norm3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_std3$LRuc[1],3),"(",round(VaRBacktest_XMR_std3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_sstd3$LRuc[1],3),"(",round(VaRBacktest_XMR_sstd3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_ast3$LRuc[1],3),"(",round(VaRBacktest_XMR_ast3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_XMR_astone3$LRuc[1],3),"(",round(VaRBacktest_XMR_astone3$LRuc[2],3),")"))


ss1_backtest_ETC_GAS <- 
  c(paste0(round(VaRBacktest_ETC_norm1$LRuc[1],3),"(",round(VaRBacktest_ETC_norm1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_std1$LRuc[1],3),"(",round(VaRBacktest_ETC_std1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_sstd1$LRuc[1],3),"(",round(VaRBacktest_ETC_sstd1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_ast1$LRuc[1],3),"(",round(VaRBacktest_ETC_ast1$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_astone1$LRuc[1],3),"(",round(VaRBacktest_ETC_astone1$LRuc[2],3),")"))

ss2_backtest_ETC_GAS <- 
  c(paste0(round(VaRBacktest_ETC_norm2$LRuc[1],3),"(",round(VaRBacktest_ETC_norm2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_std2$LRuc[1],3),"(",round(VaRBacktest_ETC_std2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_sstd2$LRuc[1],3),"(",round(VaRBacktest_ETC_sstd2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_ast2$LRuc[1],3),"(",round(VaRBacktest_ETC_ast2$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_astone2$LRuc[1],3),"(",round(VaRBacktest_ETC_astone2$LRuc[2],3),")"))

ss3_backtest_ETC_GAS <- 
  c(paste0(round(VaRBacktest_ETC_norm3$LRuc[1],3),"(",round(VaRBacktest_ETC_norm3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_std3$LRuc[1],3),"(",round(VaRBacktest_ETC_std3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_sstd3$LRuc[1],3),"(",round(VaRBacktest_ETC_sstd3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_ast3$LRuc[1],3),"(",round(VaRBacktest_ETC_ast3$LRuc[2],3),")"),
    paste0(round(VaRBacktest_ETC_astone3$LRuc[1],3),"(",round(VaRBacktest_ETC_astone3$LRuc[2],3),")"))

backtest_kupiec_GAS <- rbind("BTC", ss1_backtest_BTC_GAS, ss2_backtest_BTC_GAS, ss3_backtest_BTC_GAS,
                         "ETH", ss1_backtest_ETH_GAS, ss2_backtest_ETH_GAS, ss3_backtest_ETH_GAS,
                         "LTC", ss1_backtest_LTC_GAS, ss2_backtest_LTC_GAS, ss3_backtest_LTC_GAS,
                         "XMR", ss1_backtest_XMR_GAS, ss2_backtest_XMR_GAS, ss3_backtest_XMR_GAS,
                         "ETC", ss1_backtest_ETC_GAS, ss2_backtest_ETC_GAS, ss3_backtest_ETC_GAS)
rownames(backtest_kupiec_GAS) <- c("BTC", "SubSample 1", "SubSample2", "SubSample3",
                               "ETH", "SubSample 1", "SubSample2", "SubSample3",
                               "LTC", "SubSample 1", "SubSample2", "SubSample3",
                               "XMR", "SubSample 1", "SubSample2", "SubSample3",
                               "ETC", "SubSample 1", "SubSample 2", "SubSample3")
colnames(backtest_kupiec_GAS) <- c("Normal", "STD", "SSTD", "AST","AST1")
xtable(backtest_kupiec_GAS)  
View(backtest_kupiec_GAS) 

# Christoffersen
ss1_backtestc_BTC_GAS <- 
  c(paste0(round(VaRBacktest_BTC_norm1$LRcc[1],3),"(",round(VaRBacktest_BTC_norm1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_std1$LRcc[1],3),"(",round(VaRBacktest_BTC_std1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_sstd1$LRcc[1],3),"(",round(VaRBacktest_BTC_sstd1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_ast1$LRcc[1],3),"(",round(VaRBacktest_BTC_ast1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_astone1$LRcc[1],3),"(",round(VaRBacktest_BTC_astone1$LRcc[2],3),")"))

ss2_backtestc_BTC_GAS <- 
  c(paste0(round(VaRBacktest_BTC_norm2$LRcc[1],3),"(",round(VaRBacktest_BTC_norm2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_std2$LRcc[1],3),"(",round(VaRBacktest_BTC_std2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_sstd2$LRcc[1],3),"(",round(VaRBacktest_BTC_sstd2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_ast2$LRcc[1],3),"(",round(VaRBacktest_BTC_ast2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_astone2$LRcc[1],3),"(",round(VaRBacktest_BTC_astone2$LRcc[2],3),")"))

ss3_backtestc_BTC_GAS <- 
  c(paste0(round(VaRBacktest_BTC_norm3$LRcc[1],3),"(",round(VaRBacktest_BTC_norm3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_std3$LRcc[1],3),"(",round(VaRBacktest_BTC_std3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_sstd3$LRcc[1],3),"(",round(VaRBacktest_BTC_sstd3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_ast3$LRcc[1],3),"(",round(VaRBacktest_BTC_ast3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_BTC_astone3$LRcc[1],3),"(",round(VaRBacktest_BTC_astone3$LRcc[2],3),")"))


ss1_backtestc_ETH_GAS <- 
  c(paste0(round(VaRBacktest_ETH_norm1$LRcc[1],3),"(",round(VaRBacktest_ETH_norm1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_std1$LRcc[1],3),"(",round(VaRBacktest_ETH_std1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_sstd1$LRcc[1],3),"(",round(VaRBacktest_ETH_sstd1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_ast1$LRcc[1],3),"(",round(VaRBacktest_ETH_ast1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_astone1$LRcc[1],3),"(",round(VaRBacktest_ETH_astone1$LRcc[2],3),")"))

ss2_backtestc_ETH_GAS <- 
  c(paste0(round(VaRBacktest_ETH_norm2$LRcc[1],3),"(",round(VaRBacktest_ETH_norm2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_std2$LRcc[1],3),"(",round(VaRBacktest_ETH_std2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_sstd2$LRcc[1],3),"(",round(VaRBacktest_ETH_sstd2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_ast2$LRcc[1],3),"(",round(VaRBacktest_ETH_ast2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_astone2$LRcc[1],3),"(",round(VaRBacktest_ETH_astone2$LRcc[2],3),")"))

ss3_backtestc_ETH_GAS <- 
  c(paste0(round(VaRBacktest_ETH_norm3$LRcc[1],3),"(",round(VaRBacktest_ETH_norm3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_std3$LRcc[1],3),"(",round(VaRBacktest_ETH_std3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_sstd3$LRcc[1],3),"(",round(VaRBacktest_ETH_sstd3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_ast3$LRcc[1],3),"(",round(VaRBacktest_ETH_ast3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETH_astone3$LRcc[1],3),"(",round(VaRBacktest_ETH_astone3$LRcc[2],3),")"))


ss1_backtestc_LTC_GAS <- 
  c(paste0(round(VaRBacktest_LTC_norm1$LRcc[1],3),"(",round(VaRBacktest_LTC_norm1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_std1$LRcc[1],3),"(",round(VaRBacktest_LTC_std1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_sstd1$LRcc[1],3),"(",round(VaRBacktest_LTC_sstd1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_ast1$LRcc[1],3),"(",round(VaRBacktest_LTC_ast1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_astone1$LRcc[1],3),"(",round(VaRBacktest_LTC_astone1$LRcc[2],3),")"))

ss2_backtestc_LTC_GAS <- 
  c(paste0(round(VaRBacktest_LTC_norm2$LRcc[1],3),"(",round(VaRBacktest_LTC_norm2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_std2$LRcc[1],3),"(",round(VaRBacktest_LTC_std2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_sstd2$LRcc[1],3),"(",round(VaRBacktest_LTC_sstd2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_ast2$LRcc[1],3),"(",round(VaRBacktest_LTC_ast2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_astone2$LRcc[1],3),"(",round(VaRBacktest_LTC_astone2$LRcc[2],3),")"))

ss3_backtestc_LTC_GAS <- 
  c(paste0(round(VaRBacktest_LTC_norm3$LRcc[1],3),"(",round(VaRBacktest_LTC_norm3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_std3$LRcc[1],3),"(",round(VaRBacktest_LTC_std3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_sstd3$LRcc[1],3),"(",round(VaRBacktest_LTC_sstd3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_ast3$LRcc[1],3),"(",round(VaRBacktest_LTC_ast3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_LTC_astone3$LRcc[1],3),"(",round(VaRBacktest_LTC_astone3$LRcc[2],3),")"))


ss1_backtestc_XMR_GAS <- 
  c(paste0(round(VaRBacktest_XMR_norm1$LRcc[1],3),"(",round(VaRBacktest_XMR_norm1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_std1$LRcc[1],3),"(",round(VaRBacktest_XMR_std1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_sstd1$LRcc[1],3),"(",round(VaRBacktest_XMR_sstd1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_ast1$LRcc[1],3),"(",round(VaRBacktest_XMR_ast1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_astone1$LRcc[1],3),"(",round(VaRBacktest_XMR_astone1$LRcc[2],3),")"))

ss2_backtestc_XMR_GAS <- 
  c(paste0(round(VaRBacktest_XMR_norm2$LRcc[1],3),"(",round(VaRBacktest_XMR_norm2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_std2$LRcc[1],3),"(",round(VaRBacktest_XMR_std2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_sstd2$LRcc[1],3),"(",round(VaRBacktest_XMR_sstd2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_ast2$LRcc[1],3),"(",round(VaRBacktest_XMR_ast2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_astone2$LRcc[1],3),"(",round(VaRBacktest_XMR_astone2$LRcc[2],3),")"))

ss3_backtestc_XMR_GAS <- 
  c(paste0(round(VaRBacktest_XMR_norm3$LRcc[1],3),"(",round(VaRBacktest_XMR_norm3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_std3$LRcc[1],3),"(",round(VaRBacktest_XMR_std3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_sstd3$LRcc[1],3),"(",round(VaRBacktest_XMR_sstd3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_ast3$LRcc[1],3),"(",round(VaRBacktest_XMR_ast3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_XMR_astone3$LRcc[1],3),"(",round(VaRBacktest_XMR_astone3$LRcc[2],3),")"))


ss1_backtestc_ETC_GAS <- 
  c(paste0(round(VaRBacktest_ETC_norm1$LRcc[1],3),"(",round(VaRBacktest_ETC_norm1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_std1$LRcc[1],3),"(",round(VaRBacktest_ETC_std1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_sstd1$LRcc[1],3),"(",round(VaRBacktest_ETC_sstd1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_ast1$LRcc[1],3),"(",round(VaRBacktest_ETC_ast1$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_astone1$LRcc[1],3),"(",round(VaRBacktest_ETC_astone1$LRcc[2],3),")"))

ss2_backtestc_ETC_GAS <- 
  c(paste0(round(VaRBacktest_ETC_norm2$LRcc[1],3),"(",round(VaRBacktest_ETC_norm2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_std2$LRcc[1],3),"(",round(VaRBacktest_ETC_std2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_sstd2$LRcc[1],3),"(",round(VaRBacktest_ETC_sstd2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_ast2$LRcc[1],3),"(",round(VaRBacktest_ETC_ast2$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_astone2$LRcc[1],3),"(",round(VaRBacktest_ETC_astone2$LRcc[2],3),")"))

ss3_backtestc_ETC_GAS <- 
  c(paste0(round(VaRBacktest_ETC_norm3$LRcc[1],3),"(",round(VaRBacktest_ETC_norm3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_std3$LRcc[1],3),"(",round(VaRBacktest_ETC_std3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_sstd3$LRcc[1],3),"(",round(VaRBacktest_ETC_sstd3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_ast3$LRcc[1],3),"(",round(VaRBacktest_ETC_ast3$LRcc[2],3),")"),
    paste0(round(VaRBacktest_ETC_astone3$LRcc[1],3),"(",round(VaRBacktest_ETC_astone3$LRcc[2],3),")"))

backtest_christ_GAS <- rbind("BTC", ss1_backtestc_BTC_GAS, ss2_backtestc_BTC_GAS, ss3_backtestc_BTC_GAS,
                             "ETH", ss1_backtestc_ETH_GAS, ss2_backtestc_ETH_GAS, ss3_backtestc_ETH_GAS,
                             "LTC", ss1_backtestc_LTC_GAS, ss2_backtestc_LTC_GAS, ss3_backtestc_LTC_GAS,
                             "XMR", ss1_backtestc_XMR_GAS, ss2_backtestc_XMR_GAS, ss3_backtestc_XMR_GAS,
                             "ETC", ss1_backtestc_ETC_GAS, ss2_backtestc_ETC_GAS, ss3_backtestc_ETC_GAS)
rownames(backtest_christ_GAS) <- c("BTC", "SubSample1", "SubSample2", "SubSample3",
                                   "ETH", "SubSample1", "SubSample2", "SubSample3",
                                   "LTC", "SubSample1", "SubSample2", "SubSample3",
                                   "XMR", "SubSample1", "SubSample2", "SubSample3",
                                   "ETC", "SubSample1", "SubSample2", "SubSample3")
colnames(backtest_christ_GAS) <- c("Normal", "STD", "SSTD", "AST","AST1")

xtable(backtest_christ_GAS)

############################### :::::::::::::::::::::::::::::: ###############################

