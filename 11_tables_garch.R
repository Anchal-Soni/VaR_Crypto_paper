# =============================================================================
# 11_tables_garch.R
#
# Purpose : Builds the paper tables for the EGARCH part and the price plot with the
#           two common breaks.
# Inputs  : objects from 01-05
# Outputs : output/figures/str_brk_plot.eps; LaTeX tables printed to the console
# Paper   : Figure 3; Tables 1, 3, A.2-A.8
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

################# ::::::::::::::::::::::: ################# 
##### Strc Brk plot, Des Stats FS & SS, TIme series, AIC, GARCH Coef, MAPE, RMSE, Backtesting, Unused #######################


library(xtable)

# 1. Plot for all the series with common structural break

Crypto2 <- read.csv(DATA_FILE, header = TRUE)
Crypto2$Date = as.Date.character(Crypto2$Date,format = "%d/%m/%Y")
colnames(Crypto2) <- c("Date", "BTC","ETH","LTC","XMR","ETC")

# prepare plot data as appropriate
plotdata = data.frame(
  Date = rep(Crypto2$Date,5),
  Currency = rep(colnames(Crypto2)[-1],each = nrow(Crypto2)),
  Value = unlist(Crypto2[,-1])
) 

View(plotdata)
# check factor levels
levels(plotdata$Currency)
levels(plotdata$Currency) <- c("BTC","ETH","LTC","XMR","ETC")

# changing the level to ETC makes the plot label correct
str_brks = as.Date.character(Crypto2$Date[c(300,1300)])
str_brk_plot = plotdata %>%
  ggplot(aes(x = Date, color = factor(Currency))) +
  geom_line(aes(y = Value)) +
  facet_grid(vars(Currency),scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = "free_y") +
  sapply(str_brks,function(i) geom_vline(aes(xintercept = i),lty = 2,col = 9)) + 
  theme(panel.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.position = "Bottom",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15)) +
  scale_x_date(breaks = str_brks, labels = str_brks)

#ggexport(str_brk_plot, filename = file.path(FIG_DIR, "Strbrkcryp.png"), width = 300, height = 400)

ggsave(filename = file.path(FIG_DIR, "str_brk_plot.eps"),
       plot = str_brk_plot,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

################# ::::::::::::::::::::::: ################# 

# 2. Summary Statistics of all cryptocurrencies

DesStat.Tab.FullSample <- matrix(data = 0, nrow = 4, ncol = 5)
rownames(DesStat.Tab.FullSample) <- c("Mean", "Standard Deviation", "Skewness", "Excess Kurtosis")
colnames(DesStat.Tab.FullSample) <- c("Bitcoin", "Ethereum", "Litecoin", "Monero", "Ethereum Classic")
DesStat.Tab.FullSample[1,1] = round(mean(Returns_CrCu_BTC),4)
DesStat.Tab.FullSample[2,1] = stdev(Returns_CrCu_BTC)
DesStat.Tab.FullSample[3,1] = skewness(Returns_CrCu_BTC)
DesStat.Tab.FullSample[4,1] = kurtosis(Returns_CrCu_BTC)

DesStat.Tab.FullSample[1,2] = round(mean(Returns_CrCu_ETH),4)
DesStat.Tab.FullSample[2,2] = stdev(Returns_CrCu_ETH)
DesStat.Tab.FullSample[3,2] = skewness(Returns_CrCu_ETH)
DesStat.Tab.FullSample[4,2] = kurtosis(Returns_CrCu_ETH)

DesStat.Tab.FullSample[1,3] = round(mean(Returns_CrCu_LTC),4)
DesStat.Tab.FullSample[2,3] = stdev(Returns_CrCu_LTC)
DesStat.Tab.FullSample[3,3] = skewness(Returns_CrCu_LTC)
DesStat.Tab.FullSample[4,3] = kurtosis(Returns_CrCu_LTC)

DesStat.Tab.FullSample[1,4] = round(mean(Returns_CrCu_XMR),4)
DesStat.Tab.FullSample[2,4] = stdev(Returns_CrCu_XMR)
DesStat.Tab.FullSample[3,4] = skewness(Returns_CrCu_XMR)
DesStat.Tab.FullSample[4,4] = kurtosis(Returns_CrCu_XMR)

DesStat.Tab.FullSample[1,5] = round(mean(Returns_CrCu_ETC),4)
DesStat.Tab.FullSample[2,5] = stdev(Returns_CrCu_ETC)
DesStat.Tab.FullSample[3,5] = skewness(Returns_CrCu_ETC)
DesStat.Tab.FullSample[4,5] = kurtosis(Returns_CrCu_ETC)
#  Excess kurtosis by default
xtable(DesStat.Tab.FullSample, caption = "Descriptive Statistics of cryptocurrencies", digits = 5)

# 3. Descriptive Statistics of all cryptocurrencies subsamples

DesS_BTC <- rbind(Descr_CrCu_BTC[7,], Descr_CrCu_BTC[14,], Descr_CrCu_BTC[15,], Descr_CrCu_BTC[16,])
colnames(DesS_BTC) <- c("SubSample1", "SubSample2", "Subsample3")

DesS_ETH <- rbind(Descr_CrCu_ETH[7,], Descr_CrCu_ETH[14,], Descr_CrCu_ETH[15,], Descr_CrCu_ETH[16,])
colnames(DesS_ETH) <- c("SubSample1", "SubSample2", "Subsample3")

DesS_LTC <- rbind(Descr_CrCu_LTC[7,], Descr_CrCu_LTC[14,], Descr_CrCu_LTC[15,], Descr_CrCu_LTC[16,])
colnames(DesS_LTC) <- c("SubSample1", "SubSample2", "Subsample3")

DesS_XMR <- rbind(Descr_CrCu_XMR[7,], Descr_CrCu_XMR[14,], Descr_CrCu_XMR[15,], Descr_CrCu_XMR[16,])
colnames(DesS_XMR) <- c("SubSample1", "SubSample2", "Subsample3")

DesS_ETC <- rbind(Descr_CrCu_ETC[7,], Descr_CrCu_ETC[14,], Descr_CrCu_ETC[15,], Descr_CrCu_ETC[16,])
colnames(DesS_ETC) <- c("SubSample1", "SubSample2", "Subsample3")

DesS <- rbind("Bitcoin", round(t(DesS_BTC),5),
              "Ethereum", round(t(DesS_ETH),5),
              "Litecoin", round(t(DesS_LTC),5),
              "Monero", round(t(DesS_XMR),5),
              "Ethereum Classic", round(t(DesS_ETC),5))

xtable(DesS,caption = "Descriptive Statistics")

################# ::::::::::::::::::::::: ################# 
 
# 4. Time series properties of return series

adftestres <- c(paste(round(adf_BTC$statistic,4),"(",adf_BTC$p.value,")"), paste(round(adf_ETH$statistic,4),"(",adf_ETH$p.value,")"),
                paste(round(adf_LTC$statistic,4),"(",adf_LTC$p.value,")"), paste(round(adf_XMR$statistic,4),"(",adf_XMR$p.value,")"),
                paste(round(adf_ETC$statistic,4),"(",adf_ETC$p.value,")"))
jbtestres <- c(paste(round(jb_BTC$statistic,4),"(",round(jb_BTC$p.value,4),")"), paste(round(jb_ETH$statistic,4),"(",round(jb_ETH$p.value,4),")"),
                paste(round(jb_LTC$statistic,4),"(",round(jb_LTC$p.value,4),")"), paste(round(jb_XMR$statistic,4),"(",round(jb_XMR$p.value,4),")"),
                paste(round(jb_ETC$statistic,4),"(",round(jb_ETC$p.value,4),")"))
Boxtestres <- c(paste(round(Bt_BTC$statistic,4),"(",round(Bt_BTC$p.value,4),")"), paste(round(Bt_ETH$statistic,4),"(",round(Bt_ETH$p.value,4),")"),
                paste(round(Bt_LTC$statistic,4),"(",round(Bt_LTC$p.value,4),")"), paste(round(Bt_XMR$statistic,4),"(",round(Bt_XMR$p.value,4),")"),
                paste(round(Bt_ETC$statistic,4),"(",round(Bt_ETC$p.value,4),")"))
Boxtestsqres <- c(paste(round(Bt_BTC_sq$statistic,4),"(",round(Bt_BTC_sq$p.value,4),")"), paste(round(Bt_ETH_sq$statistic,4),"(",round(Bt_ETH_sq$p.value,4),")"),
                paste(round(Bt_LTC_sq$statistic,4),"(",round(Bt_LTC_sq$p.value,4),")"), paste(round(Bt_XMR_sq$statistic,4),"(",round(Bt_XMR_sq$p.value,4),")"),
                paste(round(Bt_ETC_sq$statistic,4),"(",round(Bt_ETC_sq$p.value,4),")"))
archtestres <- c(paste(round(arch_BTC$statistic,4),"(",round(arch_BTC$p.value,4),")"), paste(round(arch_ETH$statistic,4),"(",round(arch_ETH$p.value,4),")"),
                paste(round(arch_LTC$statistic,4),"(",round(arch_LTC$p.value,4),")"), paste(round(arch_XMR$statistic,4),"(",round(arch_XMR$p.value,4),")"),
                paste(round(arch_ETC$statistic,4),"(",round(arch_ETC$p.value,4),")"))
returnprops <- rbind(adftestres, jbtestres, Boxtestres,Boxtestsqres,archtestres)
returnprops <- matrix(data = 0, nrow = 5, ncol = 5)
rownames(returnprops) <- c("ADF for return", "JB test for return","LB test for return", "LB test for squared return", "Arch LM test for return")
colnames(returnprops) <- c("Bitcoin", "Ethereum", "Litecoin", "Monero", "Ethereum Classic")
xtable(returnprops, caption = "Values of Augmented Dickey Fuller test, Jarque Bera test, Box Ljung test for return series as well as squared returns and ARCH test for return series", digits = 4)
                
################# ::::::::::::::::::::::: #################

# 5. AIC of full sample and subsamples for all cryptocurrencies

aic_fs_BTC <- c(infocriteria(CrCu_BTC_Model7)[1], infocriteria(CrCu_BTC_Model6)[1], infocriteria(CrCu_BTC_Model5)[1], infocriteria(CrCu_BTC_Model8)[1], 
                infocriteria(CrCu_BTC_Model_Sample_JSU1)[1], infocriteria(CrCu_BTC_Model_Sample_SGED1)[1], infocriteria(CrCu_BTC_Model_Sample_SST1)[1], infocriteria(CrCu_BTC_Model_Sample_Normal1)[1],
                infocriteria(CrCu_BTC_Model_Sample_JSU2)[1], infocriteria(CrCu_BTC_Model_Sample_SGED2)[1], infocriteria(CrCu_BTC_Model_Sample_SST2)[1], infocriteria(CrCu_BTC_Model_Sample_Normal2)[1],
                infocriteria(CrCu_BTC_Model_Sample_JSU3)[1], infocriteria(CrCu_BTC_Model_Sample_SGED3)[1], infocriteria(CrCu_BTC_Model_Sample_SST3)[1], infocriteria(CrCu_BTC_Model_Sample_Normal3)[1])

aic_fs_sub_BTC <- matrix(data=aic_fs_BTC, nrow = 4, ncol = 4)
rownames(aic_fs_sub_BTC) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
colnames(aic_fs_sub_BTC) <- c("BTC Full Sample", "SubSample1", "SubSample2", "SubSample3")


aic_fs_ETH <- c(infocriteria(CrCu_ETH_Model7)[1], infocriteria(CrCu_ETH_Model6)[1], infocriteria(CrCu_ETH_Model5)[1], infocriteria(CrCu_ETH_Model8)[1], 
                infocriteria(CrCu_ETH_Model_Sample_JSU1)[1], infocriteria(CrCu_ETH_Model_Sample_SGED1)[1], infocriteria(CrCu_ETH_Model_Sample_SST1)[1], infocriteria(CrCu_ETH_Model_Sample_Normal1)[1],
                infocriteria(CrCu_ETH_Model_Sample_JSU2)[1], infocriteria(CrCu_ETH_Model_Sample_SGED2)[1], infocriteria(CrCu_ETH_Model_Sample_SST2)[1], infocriteria(CrCu_ETH_Model_Sample_Normal2)[1],
                infocriteria(CrCu_ETH_Model_Sample_JSU3)[1], infocriteria(CrCu_ETH_Model_Sample_SGED3)[1], infocriteria(CrCu_ETH_Model_Sample_SST3)[1], infocriteria(CrCu_ETH_Model_Sample_Normal3)[1])

aic_fs_sub_ETH <- matrix(data=aic_fs_ETH, nrow = 4, ncol = 4)
rownames(aic_fs_sub_ETH) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
colnames(aic_fs_sub_ETH) <- c(" ETH Full Sample", "ETH SS1", "ETH SS2", "ETH SS3")


aic_fs_LTC <- c(infocriteria(CrCu_LTC_Model7)[1], infocriteria(CrCu_LTC_Model6)[1], infocriteria(CrCu_LTC_Model5)[1], infocriteria(CrCu_LTC_Model8)[1], 
                infocriteria(CrCu_LTC_Model_Sample_JSU1)[1], infocriteria(CrCu_LTC_Model_Sample_SGED1)[1], infocriteria(CrCu_LTC_Model_Sample_SST1)[1], infocriteria(CrCu_LTC_Model_Sample_Normal1)[1],
                infocriteria(CrCu_LTC_Model_Sample_JSU2)[1], infocriteria(CrCu_LTC_Model_Sample_SGED2)[1], infocriteria(CrCu_LTC_Model_Sample_SST2)[1], infocriteria(CrCu_LTC_Model_Sample_Normal2)[1],
                infocriteria(CrCu_LTC_Model_Sample_JSU3)[1], infocriteria(CrCu_LTC_Model_Sample_SGED3)[1], infocriteria(CrCu_LTC_Model_Sample_SST3)[1], infocriteria(CrCu_LTC_Model_Sample_Normal3)[1])

aic_fs_sub_LTC <- matrix(data=aic_fs_LTC, nrow = 4, ncol = 4)
rownames(aic_fs_sub_LTC) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
colnames(aic_fs_sub_LTC) <- c(" LTC Full Sample", "LTC SS1", "LTC SS2", "LTC SS3")


aic_fs_XMR <- c(infocriteria(CrCu_XMR_Model7)[1], infocriteria(CrCu_XMR_Model6)[1], infocriteria(CrCu_XMR_Model5)[1], infocriteria(CrCu_XMR_Model8)[1], 
                infocriteria(CrCu_XMR_Model_Sample_JSU1)[1], infocriteria(CrCu_XMR_Model_Sample_SGED1)[1], infocriteria(CrCu_XMR_Model_Sample_SST1)[1], infocriteria(CrCu_XMR_Model_Sample_Normal1)[1],
                infocriteria(CrCu_XMR_Model_Sample_JSU2)[1], infocriteria(CrCu_XMR_Model_Sample_SGED2)[1], infocriteria(CrCu_XMR_Model_Sample_SST2)[1], infocriteria(CrCu_XMR_Model_Sample_Normal2)[1],
                infocriteria(CrCu_XMR_Model_Sample_JSU3)[1], infocriteria(CrCu_XMR_Model_Sample_SGED3)[1], infocriteria(CrCu_XMR_Model_Sample_SST3)[1], infocriteria(CrCu_XMR_Model_Sample_Normal3)[1])

aic_fs_sub_XMR <- matrix(data=aic_fs_XMR, nrow = 4, ncol = 4)
rownames(aic_fs_sub_XMR) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
colnames(aic_fs_sub_XMR) <- c(" XMR Full Sample", "XMR SS1", "XMR SS2", "XMR SS3")


aic_fs_ETC <- c(infocriteria(CrCu_ETC_Model7)[1], infocriteria(CrCu_ETC_Model6)[1], infocriteria(CrCu_ETC_Model5)[1], infocriteria(CrCu_ETC_Model8)[1], 
                infocriteria(CrCu_ETC_Model_Sample_JSU1)[1], infocriteria(CrCu_ETC_Model_Sample_SGED1)[1], infocriteria(CrCu_ETC_Model_Sample_SST1)[1], infocriteria(CrCu_ETC_Model_Sample_Normal1)[1],
                infocriteria(CrCu_ETC_Model_Sample_JSU2)[1], infocriteria(CrCu_ETC_Model_Sample_SGED2)[1], infocriteria(CrCu_ETC_Model_Sample_SST2)[1], infocriteria(CrCu_ETC_Model_Sample_Normal2)[1],
                infocriteria(CrCu_ETC_Model_Sample_JSU3)[1], infocriteria(CrCu_ETC_Model_Sample_SGED3)[1], infocriteria(CrCu_ETC_Model_Sample_SST3)[1], infocriteria(CrCu_ETC_Model_Sample_Normal3)[1])

aic_fs_sub_ETC <- matrix(data=aic_fs_ETC, nrow = 4, ncol = 4)
rownames(aic_fs_sub_ETC) <- c("eGarch-JSU", "eGarch-SGED", "eGarch-SST", "eGarch-Normal")
colnames(aic_fs_sub_ETC) <- c(" ETC Full Sample", "ETC SS1", "ETC SS2", "ETC SS3")

aicval <- rbind("Bitcoin", round(aic_fs_sub_BTC,4), 
                "Ethereum", round(aic_fs_sub_ETH,4),
                "Litecoin", round(aic_fs_sub_LTC,4),
                "Monero", round(aic_fs_sub_XMR,4),
                "Ethereum Classic", round(aic_fs_sub_ETC,4))

View(aicval)
xtable(aicval,caption = "AIC values", digits = 4)

# 5. GARCH coefficients of all cryptocurrencies for the best model

######## :::::::::::::: B I T C O I N

coef_BTC_7 <- coef(CrCu_BTC_Model7)
coef_BTC_sub1_SGED <- round(coef(CrCu_BTC_Model_Sample_SGED1),4) # SS1
coef_BTC_sub2_JSU <- round(coef(CrCu_BTC_Model_Sample_JSU2),4) # SS2
coef_BTC_sub3_SST <- round(coef(CrCu_BTC_Model_Sample_SST3),4) # SS3

garch_coef_BTC_fsub <- bind_rows(coef_BTC_7, coef_BTC_sub1_SGED, coef_BTC_sub2_JSU, coef_BTC_sub3_SST)

garch_coef_BTC1_fsub <- as.data.frame(garch_coef_BTC_fsub)
rownames(garch_coef_BTC1_fsub) <- c("FullSample-eGarchJSU","SS1-eGarchSGED","SS2-eGarchJSU","SS3-eGarchSST")
garch_coef_BTC1_fsubt <- (t(garch_coef_BTC1_fsub))
View(t(garch_coef_BTC1_fsub))
xtable(garch_coef_BTC1_fsubt, caption = "Bitcoin: Coefficients of eGarch models based on the best AIC values", digits = 4)

######## :::::::::::::: E T H E R E U M

coef_ETH_6 <- coef(CrCu_ETH_Model6)
coef_ETH_sub1_SGED <- round(coef(CrCu_ETH_Model_Sample_SGED1),4) # SS1
coef_ETH_sub2_SGED <- round(coef(CrCu_ETH_Model_Sample_SGED2),4) # SS2
coef_ETH_sub3_SST <- round(coef(CrCu_ETH_Model_Sample_SST3),4) # SS3

garch_coef_ETH_fsub <- bind_rows(coef_ETH_6, coef_ETH_sub1_SGED, coef_ETH_sub2_SGED, coef_ETH_sub3_SST)

garch_coef_ETH1_fsub <- as.data.frame(garch_coef_ETH_fsub)
rownames(garch_coef_ETH1_fsub) <- c("FullSample-eGarchSGED","SS1-eGarchSGED","SS2-eGarchSGED","SS3-eGarchSST")
garch_coef_ETH1_fsubt <- (t(garch_coef_ETH1_fsub))
View(t(garch_coef_ETH1_fsub))
xtable(garch_coef_ETH1_fsubt, caption = "Ethereum: Coefficients of eGarch models based on the best AIC values", digits = 4)

######## :::::::::::::: L I T E C O I N

coef_LTC_5 <- coef(CrCu_ETH_Model5)
coef_LTC_sub1_SST <- round(coef(CrCu_LTC_Model_Sample_SST1),4) # SS1
coef_LTC_sub2_JSU <- round(coef(CrCu_LTC_Model_Sample_JSU2),4) # SS2
coef_LTC_sub3_SST <- round(coef(CrCu_LTC_Model_Sample_SST3),4) # SS3

garch_coef_LTC_fsub <- bind_rows(coef_LTC_5, coef_LTC_sub1_SST, coef_LTC_sub2_JSU , coef_LTC_sub3_SST)

garch_coef_LTC1_fsub <- as.data.frame(garch_coef_LTC_fsub)
rownames(garch_coef_LTC1_fsub) <- c("FullSample-eGarchSST","SS1-eGarchSST","SS2-eGarchJSU","SS3-eGarchSST")
garch_coef_LTC1_fsubt <- (t(garch_coef_LTC1_fsub))
View(t(garch_coef_LTC1_fsub))
xtable(garch_coef_LTC1_fsubt, caption = "Litecoin: Coefficients of eGarch models based on the best AIC values", digits = 4)

######## :::::::::::::: M O N E R O

coef_XMR_7 <- round(coef(CrCu_XMR_Model7),4)
coef_XMR_sub1_JSU <- round(coef(CrCu_XMR_Model_Sample_JSU1),4) # SS1
coef_XMR_sub2_SGED <- round(coef(CrCu_XMR_Model_Sample_SGED2),4) # SS2
coef_XMR_sub3_SST <- round(coef(CrCu_XMR_Model_Sample_SST3),4) # SS3

garch_coef_XMR_fsub <- bind_rows(coef_XMR_7, coef_XMR_sub1_JSU, coef_XMR_sub2_SGED, coef_XMR_sub3_SST)

garch_coef_XMR1_fsub <- as.data.frame(garch_coef_XMR_fsub)
rownames(garch_coef_XMR1_fsub) <- c("FullSample-eGarchJSU","SS1-eGarchJSU","SS2-eGarchSGED","SS3-eGarchSST")
garch_coef_XMR1_fsubt <- (t(garch_coef_XMR1_fsub))
View(t(garch_coef_XMR1_fsub))
xtable(garch_coef_XMR1_fsubt, caption = "Monero: Coefficients of eGarch models based on the best AIC values", digits = 4)

######## :::::::::::::: E T H E R E U M  C L A S S I C

coef_ETC_7 <- round(coef(CrCu_ETC_Model7),4)
coef_ETC_sub1_SGED <- round(coef(CrCu_ETC_Model_Sample_SGED1),4) # SS1
coef_ETC_sub2_SST <- round(coef(CrCu_ETC_Model_Sample_SST2),4) # SS2
coef_ETC_sub3_SST <- round(coef(CrCu_ETC_Model_Sample_SST3),4) # SS3

garch_coef_ETC_fsub <- bind_rows(coef_ETC_7, coef_ETC_sub1_SGED, coef_ETC_sub2_SST, coef_ETC_sub3_SST)

garch_coef_ETC1_fsub <- as.data.frame(garch_coef_ETC_fsub)
rownames(garch_coef_ETC1_fsub) <- c("FullSample-eGarchJSU","SS1-eGarchSGED","SS2-eGarchSST","SS3-eGarchSST")
garch_coef_ETC1_fsubt <- (t(garch_coef_ETC1_fsub))
View(t(garch_coef_ETC1_fsub))
xtable(garch_coef_ETC1_fsubt, caption = "Ethereum Classic: Coefficients of eGarch models based on the best AIC values", digits = 4)

####################################################################################
# 6. MAPE and RMSE of the GARCH volatility forecasts
# (identical code lives in R/04_garch_mape_rmse.R; sourced here instead of repeating ~850 lines)
source(file.path("R", "04_garch_mape_rmse.R"), print.eval = TRUE)

# 7. Backtesting results
# Kupiec
ss2_backtest_BTC_01 <- 
  c(paste0(round(CrCu_BTC_SGED2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_SGED2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_BTC_SST2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_SST2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_BTC_JSU2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_JSU2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_BTC_Normal2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_Normal2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_BTC_PSIV2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_PSIV2_VaR_Test1$uc.LRp,3),")"))
ss3_backtest_BTC_01 <- 
  c(paste0(round(CrCu_BTC_SGED3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_SGED3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_BTC_SST3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_SST3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_BTC_JSU3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_JSU3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_BTC_Normal3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_Normal3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_BTC_PSIV3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_BTC_PSIV3_VaR_Test1$uc.LRp,3),")"))

ss2_backtest_ETH_01 <- 
  c(paste0(round(CrCu_ETH_SGED2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_SGED2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETH_SST2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_SST2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETH_JSU2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_JSU2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETH_Normal2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_Normal2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETH_PSIV2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_PSIV2_VaR_Test1$uc.LRp,3),")"))
ss3_backtest_ETH_01 <- 
  c(paste0(round(CrCu_ETH_SGED3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_SGED3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETH_SST3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_SST3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETH_JSU3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_JSU3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETH_Normal3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_Normal3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETH_PSIV3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETH_PSIV3_VaR_Test1$uc.LRp,3),")"))

ss2_backtest_LTC_01 <- 
  c(paste0(round(CrCu_LTC_SGED2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_SGED2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_LTC_SST2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_SST2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_LTC_JSU2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_JSU2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_LTC_Normal2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_Normal2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_LTC_PSIV2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_PSIV2_VaR_Test1$uc.LRp,3),")"))
ss3_backtest_LTC_01 <- 
  c(paste0(round(CrCu_LTC_SGED3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_SGED3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_LTC_SST3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_SST3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_LTC_JSU3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_JSU3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_LTC_Normal3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_Normal3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_LTC_PSIV3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_LTC_PSIV3_VaR_Test1$uc.LRp,3),")"))

ss2_backtest_XMR_01 <- 
  c(paste0(round(CrCu_XMR_SGED2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_SGED2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_XMR_SST2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_SST2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_XMR_JSU2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_JSU2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_XMR_Normal2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_Normal2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_XMR_PSIV2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_PSIV2_VaR_Test1$uc.LRp,3),")"))
ss3_backtest_XMR_01 <- 
  c(paste0(round(CrCu_XMR_SGED3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_SGED3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_XMR_SST3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_SST3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_XMR_JSU3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_JSU3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_XMR_Normal3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_Normal3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_XMR_PSIV3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_XMR_PSIV3_VaR_Test1$uc.LRp,3),")"))

ss2_backtest_ETC_01 <- 
  c(paste0(round(CrCu_ETC_SGED2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_SGED2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETC_SST2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_SST2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETC_JSU2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_JSU2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETC_Normal2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_Normal2_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETC_PSIV2_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_PSIV2_VaR_Test1$uc.LRp,3),")"))
ss3_backtest_ETC_01 <- 
  c(paste0(round(CrCu_ETC_SGED3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_SGED3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETC_SST3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_SST3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETC_JSU3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_JSU3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETC_Normal3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_Normal3_VaR_Test1$uc.LRp,3),")"),
    paste0(round(CrCu_ETC_PSIV3_VaR_Test1$uc.LRstat,3),"(",round(CrCu_ETC_PSIV3_VaR_Test1$uc.LRp,3),")"))

backtest_kupiec <- rbind("Bitcoin",ss2_backtest_BTC_01,ss3_backtest_BTC_01,
                         "Ethereum",ss2_backtest_ETH_01,ss3_backtest_ETH_01,
                         "Litecoin",ss2_backtest_LTC_01,ss3_backtest_LTC_01,
                         "Monero",ss2_backtest_XMR_01,ss3_backtest_XMR_01,
                         "Ethereum Classic",ss2_backtest_ETC_01,ss3_backtest_ETC_01)
rownames(backtest_kupiec) <- c("Bitcoin", "SubSample2","SubSample3",
                               "Ethereum", "SubSample2","SubSample3",
                               "Litecoin", "SubSample2","SubSample3",
                               "Monero", "SubSample2","SubSample3",
                               "Ethereum Classic", "SubSample 2","SubSample3")
colnames(backtest_kupiec) <- c("SGED", "SST", "JSU", "Normal","PSIV")
xtable(backtest_kupiec)  

# Christoffersen
ss2_backtestc_BTC_01 <- 
  c(paste0(round(CrCu_BTC_SGED2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_SGED2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_BTC_SST2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_SST2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_BTC_JSU2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_JSU2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_BTC_Normal2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_Normal2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_BTC_PSIV2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_PSIV2_VaR_Test1$cc.LRp,3),")"))
ss3_backtestc_BTC_01 <- 
  c(paste0(round(CrCu_BTC_SGED3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_SGED3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_BTC_SST3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_SST3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_BTC_JSU3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_JSU3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_BTC_Normal3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_Normal3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_BTC_PSIV3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_BTC_PSIV3_VaR_Test1$cc.LRp,3),")"))

ss2_backtestc_ETH_01 <- 
  c(paste0(round(CrCu_ETH_SGED2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_SGED2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETH_SST2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_SST2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETH_JSU2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_JSU2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETH_Normal2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_Normal2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETH_PSIV2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_PSIV2_VaR_Test1$cc.LRp,3),")"))
ss3_backtestc_ETH_01 <- 
  c(paste0(round(CrCu_ETH_SGED3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_SGED3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETH_SST3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_SST3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETH_JSU3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_JSU3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETH_Normal3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_Normal3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETH_PSIV3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETH_PSIV3_VaR_Test1$cc.LRp,3),")"))

ss2_backtestc_LTC_01 <- 
  c(paste0(round(CrCu_LTC_SGED2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_SGED2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_LTC_SST2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_SST2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_LTC_JSU2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_JSU2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_LTC_Normal2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_Normal2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_LTC_PSIV2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_PSIV2_VaR_Test1$cc.LRp,3),")"))
ss3_backtestc_LTC_01 <- 
  c(paste0(round(CrCu_LTC_SGED3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_SGED3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_LTC_SST3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_SST3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_LTC_JSU3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_JSU3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_LTC_Normal3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_Normal3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_LTC_PSIV3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_LTC_PSIV3_VaR_Test1$cc.LRp,3),")"))

ss2_backtestc_XMR_01 <- 
  c(paste0(round(CrCu_XMR_SGED2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_SGED2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_XMR_SST2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_SST2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_XMR_JSU2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_JSU2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_XMR_Normal2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_Normal2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_XMR_PSIV2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_PSIV2_VaR_Test1$cc.LRp,3),")"))
ss3_backtestc_XMR_01 <- 
  c(paste0(round(CrCu_XMR_SGED3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_SGED3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_XMR_SST3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_SST3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_XMR_JSU3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_JSU3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_XMR_Normal3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_Normal3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_XMR_PSIV3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_XMR_PSIV3_VaR_Test1$cc.LRp,3),")"))

ss2_backtestc_ETC_01 <- 
  c(paste0(round(CrCu_ETC_SGED2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_SGED2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETC_SST2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_SST2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETC_JSU2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_JSU2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETC_Normal2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_Normal2_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETC_PSIV2_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_PSIV2_VaR_Test1$cc.LRp,3),")"))
ss3_backtestc_ETC_01 <- 
  c(paste0(round(CrCu_ETC_SGED3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_SGED3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETC_SST3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_SST3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETC_JSU3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_JSU3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETC_Normal3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_Normal3_VaR_Test1$cc.LRp,3),")"),
    paste0(round(CrCu_ETC_PSIV3_VaR_Test1$cc.LRstat,3),"(",round(CrCu_ETC_PSIV3_VaR_Test1$cc.LRp,3),")"))

backtest_christ <- rbind("Bitcoin",ss2_backtestc_BTC_01,ss3_backtestc_BTC_01,
                         "Ethereum",ss2_backtestc_ETH_01,ss3_backtestc_ETH_01,
                         "Litecoin",ss2_backtestc_LTC_01,ss3_backtestc_LTC_01,
                         "Monero",ss2_backtestc_XMR_01,ss3_backtestc_XMR_01,
                         "Ethereum Classic",ss2_backtestc_ETC_01,ss3_backtestc_ETC_01)
rownames(backtest_christ) <- c("Bitcoin", "SubSample2","SubSample3",
                               "Ethereum", "SubSample2","SubSample3",
                               "Litecoin", "SubSample2","SubSample3",
                               "Monero", "SubSample2","SubSample3",
                               "Ethereum Classic", "SubSample 2","SubSample3")
colnames(backtest_christ) <- c("SGED", "SST", "JSU", "Normal","PSIV")
xtable(backtest_christ) 

## TABLES not used ##

library(xtable)

Str.Tab <- matrix(data = 0, nrow = 4, ncol = 3)
colnames(Str.Tab) <- c("From Date", "To Date", "Number of observations")
rownames(Str.Tab) <- c("Full Sample", "SubSample-1", "SubSample-2", "SubSample-3")
Str.Tab[1,1] = format(Crypto_Date[1],"%d-%b-%y")
Str.Tab[1,2] = format(max(Crypto_Date),"%d-%b-%y")
Str.Tab[1,3] = nrow(Crypto)
Str.Tab[2,1] = format(Crypto_Date[1],"%d-%b-%y")
Str.Tab[2,2] = format(Crypto_Date[CPPP[1]],"%d-%b-%y")
Str.Tab[2,3] = CPPP[1]
Str.Tab[3,1] = format(Crypto_Date[CPPP[1]+1],"%d-%b-%y")
Str.Tab[3,2] = format(Crypto_Date[CPPP[2]],"%d-%b-%y")
Str.Tab[3,3] = (CPPP[2] - CPPP[1])
Str.Tab[4,1] = format(Crypto_Date[CPPP[2]+1],"%d-%b-%y")
Str.Tab[4,2] = format(max(Crypto_Date),"%d-%b-%y")
Str.Tab[4,3] = (nrow(Crypto) - CPPP[2])

View(data.frame(Str.Tab))

xtable(Str.Tab, caption = "Structural Breaks")

################# ::::::::::::::::::::::: ################# 

#########################################################################################
## ::::::::::::::::: mean of VaR

### BITCOIN

# Full Sample
bs_fs_BTC_SST <- basicStats(VR2_BTC[,1])
bs_fs_BTC_SGED <- basicStats(VR3_BTC[,1])
bs_fs_BTC_JSU <- basicStats(VR4_BTC[,1])
bs_fs_BTC_PSIV <- basicStats(VR5_BTC[,1])
bs_fs_BTC_norm <- basicStats(VR1_BTC[,1])

fs_BTC_DS <- rbind(bs_fs_BTC_SST[7,], bs_fs_BTC_SGED[7,], bs_fs_BTC_JSU[7,], bs_fs_BTC_PSIV[7,], bs_fs_BTC_norm[7,])
colnames(fs_BTC_DS) <- c("Mean-FS")
rownames(fs_BTC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_fs_BTC_SST_7D <- basicStats(VR2_BTC[1:7,1])
bs_fs_BTC_SGED_7D <- basicStats(VR3_BTC[1:7,1])
bs_fs_BTC_JSU_7D <- basicStats(VR4_BTC[1:7,1])
bs_fs_BTC_PSIV_7D <- basicStats(VR5_BTC[1:7,1])
bs_fs_BTC_norm_7D <- basicStats(VR1_BTC[1:7,1])

fs_BTC_7DS <- rbind(bs_fs_BTC_SST_7D[7,], bs_fs_BTC_SGED_7D[7,], bs_fs_BTC_JSU_7D[7,], bs_fs_BTC_PSIV_7D[7,], bs_fs_BTC_norm_7D[7,])
colnames(fs_BTC_7DS) <- c("Mean-7Day")
rownames(fs_BTC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_BTC_fs <- cbind(fs_BTC_DS, fs_BTC_7DS)

# Subsample 2

bs_ss2_BTC_SST <- basicStats(VR22_BTC[,1])
bs_ss2_BTC_SGED <- basicStats(VR23_BTC[,1])
bs_ss2_BTC_JSU <- basicStats(VR24_BTC[,1])
bs_ss2_BTC_PSIV <- basicStats(VR25_BTC[,1])
bs_ss2_BTC_norm <- basicStats(VR21_BTC[,1])

ss2_BTC_DS <- rbind(bs_ss2_BTC_SST[7,], bs_ss2_BTC_SGED[7,], bs_ss2_BTC_JSU[7,], bs_ss2_BTC_PSIV[7,], bs_ss2_BTC_norm[7,])
colnames(ss2_BTC_DS) <- c("Mean-FS")
rownames(ss2_BTC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss2_BTC_SST_7D <- basicStats(VR22_BTC[1:7,1])
bs_ss2_BTC_SGED_7D <- basicStats(VR23_BTC[1:7,1])
bs_ss2_BTC_JSU_7D <- basicStats(VR24_BTC[1:7,1])
bs_ss2_BTC_PSIV_7D <- basicStats(VR25_BTC[1:7,1])
bs_ss2_BTC_norm_7D <- basicStats(VR21_BTC[1:7,1])

ss2_BTC_7DS <- rbind(bs_ss2_BTC_SST_7D[7,], bs_ss2_BTC_SGED_7D[7,], bs_ss2_BTC_JSU_7D[7,], bs_ss2_BTC_PSIV_7D[7,], bs_ss2_BTC_norm_7D[7,])
colnames(ss2_BTC_7DS) <- c("Mean-7Day")
rownames(ss2_BTC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_BTC_ss2 <- cbind(ss2_BTC_DS, ss2_BTC_7DS)

# Subsample 3

bs_ss3_BTC_SST <- basicStats(VR32_BTC[,1])
bs_ss3_BTC_SGED <- basicStats(VR33_BTC[,1])
bs_ss3_BTC_JSU <- basicStats(VR34_BTC[,1])
bs_ss3_BTC_PSIV <- basicStats(VR35_BTC[,1])
bs_ss3_BTC_norm <- basicStats(VR31_BTC[,1])

ss3_BTC_DS <- rbind(bs_ss3_BTC_SST[7,], bs_ss3_BTC_SGED[7,], bs_ss3_BTC_JSU[7,], bs_ss3_BTC_PSIV[7,], bs_ss3_BTC_norm[7,])
colnames(ss3_BTC_DS) <- c("Mean-FS")
rownames(ss3_BTC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss3_BTC_SST_7D <- basicStats(VR32_BTC[1:7,1])
bs_ss3_BTC_SGED_7D <- basicStats(VR33_BTC[1:7,1])
bs_ss3_BTC_JSU_7D <- basicStats(VR34_BTC[1:7,1])
bs_ss3_BTC_PSIV_7D <- basicStats(VR35_BTC[1:7,1])
bs_ss3_BTC_norm_7D <- basicStats(VR31_BTC[1:7,1])

ss3_BTC_7DS <- rbind(bs_ss3_BTC_SST_7D[7,], bs_ss3_BTC_SGED_7D[7,], bs_ss3_BTC_JSU_7D[7,], bs_ss3_BTC_PSIV_7D[7,], bs_ss3_BTC_norm_7D[7,])
colnames(ss3_BTC_7DS) <- c("Mean-7Day")
rownames(ss3_BTC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_BTC_ss3 <- cbind(ss3_BTC_DS, ss3_BTC_7DS)

# Final clubbing BTC
BTC_DS_VaR <- rbind("Full Sample", round(DS_BTC_fs,4), "SubSample 2",round(DS_BTC_ss2,4), "SubSample 3", round(DS_BTC_ss3,4))

################:::::::::::::::::::::::::::::::::
### ETHEREUM

# Full Sample
bs_fs_ETH_SST <- basicStats(VR2_ETH[,1])
bs_fs_ETH_SGED <- basicStats(VR3_ETH[,1])
bs_fs_ETH_JSU <- basicStats(VR4_ETH[,1])
bs_fs_ETH_PSIV <- basicStats(VR5_ETH[,1])
bs_fs_ETH_norm <- basicStats(VR1_ETH[,1])

fs_ETH_DS <- rbind(bs_fs_ETH_SST[7,], bs_fs_ETH_SGED[7,], bs_fs_ETH_JSU[7,], bs_fs_ETH_PSIV[7,], bs_fs_ETH_norm[7,])
colnames(fs_ETH_DS) <- c("Mean-FS")
rownames(fs_ETH_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_fs_ETH_SST_7D <- basicStats(VR2_ETH[1:7,1])
bs_fs_ETH_SGED_7D <- basicStats(VR3_ETH[1:7,1])
bs_fs_ETH_JSU_7D <- basicStats(VR4_ETH[1:7,1])
bs_fs_ETH_PSIV_7D <- basicStats(VR5_ETH[1:7,1])
bs_fs_ETH_norm_7D <- basicStats(VR1_ETH[1:7,1])

fs_ETH_7DS <- rbind(bs_fs_ETH_SST_7D[7,], bs_fs_ETH_SGED_7D[7,], bs_fs_ETH_JSU_7D[7,], bs_fs_ETH_PSIV_7D[7,], bs_fs_ETH_norm_7D[7,])
colnames(fs_ETH_7DS) <- c("Mean-7Day")
rownames(fs_ETH_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_ETH_fs <- cbind(fs_ETH_DS, fs_ETH_7DS)

# Subsample 2

bs_ss2_ETH_SST <- basicStats(VR22_ETH[,1])
bs_ss2_ETH_SGED <- basicStats(VR23_ETH[,1])
bs_ss2_ETH_JSU <- basicStats(VR24_ETH[,1])
bs_ss2_ETH_PSIV <- basicStats(VR25_ETH[,1])
bs_ss2_ETH_norm <- basicStats(VR21_ETH[,1])

ss2_ETH_DS <- rbind(bs_ss2_ETH_SST[7,], bs_ss2_ETH_SGED[7,], bs_ss2_ETH_JSU[7,], bs_ss2_ETH_PSIV[7,], bs_ss2_ETH_norm[7,])
colnames(ss2_ETH_DS) <- c("Mean-FS")
rownames(ss2_ETH_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss2_ETH_SST_7D <- basicStats(VR22_ETH[1:7,1])
bs_ss2_ETH_SGED_7D <- basicStats(VR23_ETH[1:7,1])
bs_ss2_ETH_JSU_7D <- basicStats(VR24_ETH[1:7,1])
bs_ss2_ETH_PSIV_7D <- basicStats(VR25_ETH[1:7,1])
bs_ss2_ETH_norm_7D <- basicStats(VR21_ETH[1:7,1])

ss2_ETH_7DS <- rbind(bs_ss2_ETH_SST_7D[7,], bs_ss2_ETH_SGED_7D[7,], bs_ss2_ETH_JSU_7D[7,], bs_ss2_ETH_PSIV_7D[7,], bs_ss2_ETH_norm_7D[7,])
colnames(ss2_ETH_7DS) <- c("Mean-7Day")
rownames(ss2_ETH_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_ETH_ss2 <- cbind(ss2_ETH_DS, ss2_ETH_7DS)

# Subsample 3

bs_ss3_ETH_SST <- basicStats(VR32_ETH[,1])
bs_ss3_ETH_SGED <- basicStats(VR33_ETH[,1])
bs_ss3_ETH_JSU <- basicStats(VR34_ETH[,1])
bs_ss3_ETH_PSIV <- basicStats(VR35_ETH[,1])
bs_ss3_ETH_norm <- basicStats(VR31_ETH[,1])

ss3_ETH_DS <- rbind(bs_ss3_ETH_SST[7,], bs_ss3_ETH_SGED[7,], bs_ss3_ETH_JSU[7,], bs_ss3_ETH_PSIV[7,], bs_ss3_ETH_norm[7,])
colnames(ss3_ETH_DS) <- c("Mean-FS")
rownames(ss3_ETH_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss3_ETH_SST_7D <- basicStats(VR32_ETH[1:7,1])
bs_ss3_ETH_SGED_7D <- basicStats(VR33_ETH[1:7,1])
bs_ss3_ETH_JSU_7D <- basicStats(VR34_ETH[1:7,1])
bs_ss3_ETH_PSIV_7D <- basicStats(VR35_ETH[1:7,1])
bs_ss3_ETH_norm_7D <- basicStats(VR31_ETH[1:7,1])

ss3_ETH_7DS <- rbind(bs_ss3_ETH_SST_7D[7,], bs_ss3_ETH_SGED_7D[7,], bs_ss3_ETH_JSU_7D[7,], bs_ss3_ETH_PSIV_7D[7,], bs_ss3_ETH_norm_7D[7,])
colnames(ss3_ETH_7DS) <- c("Mean-7Day")
rownames(ss3_ETH_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_ETH_ss3 <- cbind(ss3_ETH_DS, ss3_ETH_7DS)

# Final clubbing ETH
ETH_DS_VaR <- rbind("Full Sample", round(DS_ETH_fs,4), "SubSample 2",round(DS_ETH_ss2,4), "SubSample 3", round(DS_ETH_ss3,4))

################:::::::::::::::::::::::::::::::::
### LITECOIN

# Full Sample
bs_fs_LTC_SST <- basicStats(VR2_LTC[,1])
bs_fs_LTC_SGED <- basicStats(VR3_LTC[,1])
bs_fs_LTC_JSU <- basicStats(VR4_LTC[,1])
bs_fs_LTC_PSIV <- basicStats(VR5_LTC[,1])
bs_fs_LTC_norm <- basicStats(VR1_LTC[,1])

fs_LTC_DS <- rbind(bs_fs_LTC_SST[7,], bs_fs_LTC_SGED[7,], bs_fs_LTC_JSU[7,], bs_fs_LTC_PSIV[7,], bs_fs_LTC_norm[7,])
colnames(fs_LTC_DS) <- c("Mean-FS")
rownames(fs_LTC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_fs_LTC_SST_7D <- basicStats(VR2_LTC[1:7,1])
bs_fs_LTC_SGED_7D <- basicStats(VR3_LTC[1:7,1])
bs_fs_LTC_JSU_7D <- basicStats(VR4_LTC[1:7,1])
bs_fs_LTC_PSIV_7D <- basicStats(VR5_LTC[1:7,1])
bs_fs_LTC_norm_7D <- basicStats(VR1_LTC[1:7,1])

fs_LTC_7DS <- rbind(bs_fs_LTC_SST_7D[7,], bs_fs_LTC_SGED_7D[7,], bs_fs_LTC_JSU_7D[7,], bs_fs_LTC_PSIV_7D[7,], bs_fs_LTC_norm_7D[7,])
colnames(fs_LTC_7DS) <- c("Mean-7Day")
rownames(fs_LTC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_LTC_fs <- cbind(fs_LTC_DS, fs_LTC_7DS)

# Subsample 2

bs_ss2_LTC_SST <- basicStats(VR22_LTC[,1])
bs_ss2_LTC_SGED <- basicStats(VR23_LTC[,1])
bs_ss2_LTC_JSU <- basicStats(VR24_LTC[,1])
bs_ss2_LTC_PSIV <- basicStats(VR25_LTC[,1])
bs_ss2_LTC_norm <- basicStats(VR21_LTC[,1])

ss2_LTC_DS <- rbind(bs_ss2_LTC_SST[7,], bs_ss2_LTC_SGED[7,], bs_ss2_LTC_JSU[7,], bs_ss2_LTC_PSIV[7,], bs_ss2_LTC_norm[7,])
colnames(ss2_LTC_DS) <- c("Mean-FS")
rownames(ss2_LTC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss2_LTC_SST_7D <- basicStats(VR22_LTC[1:7,1])
bs_ss2_LTC_SGED_7D <- basicStats(VR23_LTC[1:7,1])
bs_ss2_LTC_JSU_7D <- basicStats(VR24_LTC[1:7,1])
bs_ss2_LTC_PSIV_7D <- basicStats(VR25_LTC[1:7,1])
bs_ss2_LTC_norm_7D <- basicStats(VR21_LTC[1:7,1])

ss2_LTC_7DS <- rbind(bs_ss2_LTC_SST_7D[7,], bs_ss2_LTC_SGED_7D[7,], bs_ss2_LTC_JSU_7D[7,], bs_ss2_LTC_PSIV_7D[7,], bs_ss2_LTC_norm_7D[7,])
colnames(ss2_LTC_7DS) <- c("Mean-7Day")
rownames(ss2_LTC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_LTC_ss2 <- cbind(ss2_LTC_DS, ss2_LTC_7DS)

# Subsample 3

bs_ss3_LTC_SST <- basicStats(VR32_LTC[,1])
bs_ss3_LTC_SGED <- basicStats(VR33_LTC[,1])
bs_ss3_LTC_JSU <- basicStats(VR34_LTC[,1])
bs_ss3_LTC_PSIV <- basicStats(VR35_LTC[,1])
bs_ss3_LTC_norm <- basicStats(VR31_LTC[,1])

ss3_LTC_DS <- rbind(bs_ss3_LTC_SST[7,], bs_ss3_LTC_SGED[7,], bs_ss3_LTC_JSU[7,], bs_ss3_LTC_PSIV[7,], bs_ss3_LTC_norm[7,])
colnames(ss3_LTC_DS) <- c("Mean-FS")
rownames(ss3_LTC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss3_LTC_SST_7D <- basicStats(VR32_LTC[1:7,1])
bs_ss3_LTC_SGED_7D <- basicStats(VR33_LTC[1:7,1])
bs_ss3_LTC_JSU_7D <- basicStats(VR34_LTC[1:7,1])
bs_ss3_LTC_PSIV_7D <- basicStats(VR35_LTC[1:7,1])
bs_ss3_LTC_norm_7D <- basicStats(VR31_LTC[1:7,1])

ss3_LTC_7DS <- rbind(bs_ss3_LTC_SST_7D[7,], bs_ss3_LTC_SGED_7D[7,], bs_ss3_LTC_JSU_7D[7,], bs_ss3_LTC_PSIV_7D[7,], bs_ss3_LTC_norm_7D[7,])
colnames(ss3_LTC_7DS) <- c("Mean-7Day")
rownames(ss3_LTC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_LTC_ss3 <- cbind(ss3_LTC_DS, ss3_LTC_7DS)

# Final clubbing LTC
LTC_DS_VaR <- rbind("Full Sample", round(DS_LTC_fs,4), "SubSample 2",round(DS_LTC_ss2,4), "SubSample 3", round(DS_LTC_ss3,4))

################:::::::::::::::::::::::::::::::::
### MONERO

# Full Sample
bs_fs_XMR_SST <- basicStats(VR2_XMR[,1])
bs_fs_XMR_SGED <- basicStats(VR3_XMR[,1])
bs_fs_XMR_JSU <- basicStats(VR4_XMR[,1])
bs_fs_XMR_PSIV <- basicStats(VR5_XMR[,1])
bs_fs_XMR_norm <- basicStats(VR1_XMR[,1])

fs_XMR_DS <- rbind(bs_fs_XMR_SST[7,], bs_fs_XMR_SGED[7,], bs_fs_XMR_JSU[7,], bs_fs_XMR_PSIV[7,], bs_fs_XMR_norm[7,])
colnames(fs_XMR_DS) <- c("Mean-FS")
rownames(fs_XMR_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_fs_XMR_SST_7D <- basicStats(VR2_XMR[1:7,1])
bs_fs_XMR_SGED_7D <- basicStats(VR3_XMR[1:7,1])
bs_fs_XMR_JSU_7D <- basicStats(VR4_XMR[1:7,1])
bs_fs_XMR_PSIV_7D <- basicStats(VR5_XMR[1:7,1])
bs_fs_XMR_norm_7D <- basicStats(VR1_XMR[1:7,1])

fs_XMR_7DS <- rbind(bs_fs_XMR_SST_7D[7,], bs_fs_XMR_SGED_7D[7,], bs_fs_XMR_JSU_7D[7,], bs_fs_XMR_PSIV_7D[7,], bs_fs_XMR_norm_7D[7,])
colnames(fs_XMR_7DS) <- c("Mean-7Day")
rownames(fs_XMR_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_XMR_fs <- cbind(fs_XMR_DS, fs_XMR_7DS)

# Subsample 2

bs_ss2_XMR_SST <- basicStats(VR22_XMR[,1])
bs_ss2_XMR_SGED <- basicStats(VR23_XMR[,1])
bs_ss2_XMR_JSU <- basicStats(VR24_XMR[,1])
bs_ss2_XMR_PSIV <- basicStats(VR25_XMR[,1])
bs_ss2_XMR_norm <- basicStats(VR21_XMR[,1])

ss2_XMR_DS <- rbind(bs_ss2_XMR_SST[7,], bs_ss2_XMR_SGED[7,], bs_ss2_XMR_JSU[7,], bs_ss2_XMR_PSIV[7,], bs_ss2_XMR_norm[7,])
colnames(ss2_XMR_DS) <- c("Mean-FS")
rownames(ss2_XMR_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss2_XMR_SST_7D <- basicStats(VR22_XMR[1:7,1])
bs_ss2_XMR_SGED_7D <- basicStats(VR23_XMR[1:7,1])
bs_ss2_XMR_JSU_7D <- basicStats(VR24_XMR[1:7,1])
bs_ss2_XMR_PSIV_7D <- basicStats(VR25_XMR[1:7,1])
bs_ss2_XMR_norm_7D <- basicStats(VR21_XMR[1:7,1])

ss2_XMR_7DS <- rbind(bs_ss2_XMR_SST_7D[7,], bs_ss2_XMR_SGED_7D[7,], bs_ss2_XMR_JSU_7D[7,], bs_ss2_XMR_PSIV_7D[7,], bs_ss2_XMR_norm_7D[7,])
colnames(ss2_XMR_7DS) <- c("Mean-7Day")
rownames(ss2_XMR_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_XMR_ss2 <- cbind(ss2_XMR_DS, ss2_XMR_7DS)

# Subsample 3

bs_ss3_XMR_SST <- basicStats(VR32_XMR[,1])
bs_ss3_XMR_SGED <- basicStats(VR33_XMR[,1])
bs_ss3_XMR_JSU <- basicStats(VR34_XMR[,1])
bs_ss3_XMR_PSIV <- basicStats(VR35_XMR[,1])
bs_ss3_XMR_norm <- basicStats(VR31_XMR[,1])

ss3_XMR_DS <- rbind(bs_ss3_XMR_SST[7,], bs_ss3_XMR_SGED[7,], bs_ss3_XMR_JSU[7,], bs_ss3_XMR_PSIV[7,], bs_ss3_XMR_norm[7,])
colnames(ss3_XMR_DS) <- c("Mean-FS")
rownames(ss3_XMR_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss3_XMR_SST_7D <- basicStats(VR32_XMR[1:7,1])
bs_ss3_XMR_SGED_7D <- basicStats(VR33_XMR[1:7,1])
bs_ss3_XMR_JSU_7D <- basicStats(VR34_XMR[1:7,1])
bs_ss3_XMR_PSIV_7D <- basicStats(VR35_XMR[1:7,1])
bs_ss3_XMR_norm_7D <- basicStats(VR31_XMR[1:7,1])

ss3_XMR_7DS <- rbind(bs_ss3_XMR_SST_7D[7,], bs_ss3_XMR_SGED_7D[7,], bs_ss3_XMR_JSU_7D[7,], bs_ss3_XMR_PSIV_7D[7,], bs_ss3_XMR_norm_7D[7,])
colnames(ss3_XMR_7DS) <- c("Mean-7Day")
rownames(ss3_XMR_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_XMR_ss3 <- cbind(ss3_XMR_DS, ss3_XMR_7DS)

# Final clubbing XMR
XMR_DS_VaR <- rbind("Full Sample", round(DS_XMR_fs,4), "SubSample 2",round(DS_XMR_ss2,4), "SubSample 3", round(DS_XMR_ss3,4))

################:::::::::::::::::::::::::::::::::
### ETHEREUM CLASSIC

# Full Sample
bs_fs_ETC_SST <- basicStats(VR2_ETC[,1])
bs_fs_ETC_SGED <- basicStats(VR3_ETC[,1])
bs_fs_ETC_JSU <- basicStats(VR4_ETC[,1])
bs_fs_ETC_PSIV <- basicStats(VR5_ETC[,1])
bs_fs_ETC_norm <- basicStats(VR1_ETC[,1])

fs_ETC_DS <- rbind(bs_fs_ETC_SST[7,], bs_fs_ETC_SGED[7,], bs_fs_ETC_JSU[7,], bs_fs_ETC_PSIV[7,], bs_fs_ETC_norm[7,])
colnames(fs_ETC_DS) <- c("Mean-FS")
rownames(fs_ETC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_fs_ETC_SST_7D <- basicStats(VR2_ETC[1:7,1])
bs_fs_ETC_SGED_7D <- basicStats(VR3_ETC[1:7,1])
bs_fs_ETC_JSU_7D <- basicStats(VR4_ETC[1:7,1])
bs_fs_ETC_PSIV_7D <- basicStats(VR5_ETC[1:7,1])
bs_fs_ETC_norm_7D <- basicStats(VR1_ETC[1:7,1])

fs_ETC_7DS <- rbind(bs_fs_ETC_SST_7D[7,], bs_fs_ETC_SGED_7D[7,], bs_fs_ETC_JSU_7D[7,], bs_fs_ETC_PSIV_7D[7,], bs_fs_ETC_norm_7D[7,])
colnames(fs_ETC_7DS) <- c("Mean-7Day")
rownames(fs_ETC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_ETC_fs <- cbind(fs_ETC_DS, fs_ETC_7DS)

# Subsample 2

bs_ss2_ETC_SST <- basicStats(VR22_ETC[,1])
bs_ss2_ETC_SGED <- basicStats(VR23_ETC[,1])
bs_ss2_ETC_JSU <- basicStats(VR24_ETC[,1])
bs_ss2_ETC_PSIV <- basicStats(VR25_ETC[,1])
bs_ss2_ETC_norm <- basicStats(VR21_ETC[,1])

ss2_ETC_DS <- rbind(bs_ss2_ETC_SST[7,], bs_ss2_ETC_SGED[7,], bs_ss2_ETC_JSU[7,], bs_ss2_ETC_PSIV[7,], bs_ss2_ETC_norm[7,])
colnames(ss2_ETC_DS) <- c("Mean-FS")
rownames(ss2_ETC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss2_ETC_SST_7D <- basicStats(VR22_ETC[1:7,1])
bs_ss2_ETC_SGED_7D <- basicStats(VR23_ETC[1:7,1])
bs_ss2_ETC_JSU_7D <- basicStats(VR24_ETC[1:7,1])
bs_ss2_ETC_PSIV_7D <- basicStats(VR25_ETC[1:7,1])
bs_ss2_ETC_norm_7D <- basicStats(VR21_ETC[1:7,1])

ss2_ETC_7DS <- rbind(bs_ss2_ETC_SST_7D[7,], bs_ss2_ETC_SGED_7D[7,], bs_ss2_ETC_JSU_7D[7,], bs_ss2_ETC_PSIV_7D[7,], bs_ss2_ETC_norm_7D[7,])
colnames(ss2_ETC_7DS) <- c("Mean-7Day")
rownames(ss2_ETC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_ETC_ss2 <- cbind(ss2_ETC_DS, ss2_ETC_7DS)

# Subsample 3

bs_ss3_ETC_SST <- basicStats(VR32_ETC[,1])
bs_ss3_ETC_SGED <- basicStats(VR33_ETC[,1])
bs_ss3_ETC_JSU <- basicStats(VR34_ETC[,1])
bs_ss3_ETC_PSIV <- basicStats(VR35_ETC[,1])
bs_ss3_ETC_norm <- basicStats(VR31_ETC[,1])

ss3_ETC_DS <- rbind(bs_ss3_ETC_SST[7,], bs_ss3_ETC_SGED[7,], bs_ss3_ETC_JSU[7,], bs_ss3_ETC_PSIV[7,], bs_ss3_ETC_norm[7,])
colnames(ss3_ETC_DS) <- c("Mean-FS")
rownames(ss3_ETC_DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

bs_ss3_ETC_SST_7D <- basicStats(VR32_ETC[1:7,1])
bs_ss3_ETC_SGED_7D <- basicStats(VR33_ETC[1:7,1])
bs_ss3_ETC_JSU_7D <- basicStats(VR34_ETC[1:7,1])
bs_ss3_ETC_PSIV_7D <- basicStats(VR35_ETC[1:7,1])
bs_ss3_ETC_norm_7D <- basicStats(VR31_ETC[1:7,1])

ss3_ETC_7DS <- rbind(bs_ss3_ETC_SST_7D[7,], bs_ss3_ETC_SGED_7D[7,], bs_ss3_ETC_JSU_7D[7,], bs_ss3_ETC_PSIV_7D[7,], bs_ss3_ETC_norm_7D[7,])
colnames(ss3_ETC_7DS) <- c("Mean-7Day")
rownames(ss3_ETC_7DS) <- c("SST", "SGED", "JSU", "PSIV", "Norm")

DS_ETC_ss3 <- cbind(ss3_ETC_DS, ss3_ETC_7DS)

# Final clubbing ETC
ETC_DS_VaR <- rbind("Full Sample", round(DS_ETC_fs,4), "SubSample 2",round(DS_ETC_ss2,4), "SubSample 3", round(DS_ETC_ss3,4))

############################################
DS_VaR <- cbind(BTC_DS_VaR, ETH_DS_VaR, LTC_DS_VaR,XMR_DS_VaR,ETC_DS_VaR)
DS_VaR <- rbind(c("BTC","BTC","ETH","ETH","LTC","LTC","XMR","XMR","ETC","ETC"),DS_VaR)
#colnames(DS_VaR) <- c("BTC","ETH","LTC","XMR","ETC")

xtable(DS_VaR, caption = "Descriptive Stats for VaR")
##########################


