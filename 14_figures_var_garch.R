# =============================================================================
# 14_figures_var_garch.R
#
# Purpose : 1% VaR forecasts vs realised returns for the EGARCH models (Norm,
#           SST, SGED, JSU, PSIV), all five coins, for SS1, SS2, SS3 and the
#           full sample.
# Inputs  : VR* objects from 03f_var_series_1pct.R
# Outputs : output/figures/FSVaR_SS1_01.eps, FSVaR_G2_01.eps, FSVaR_G3_01.eps,
#           FSVaR_G_01.eps
# Paper   : Figure B.1 (SS2 and SS3 panels)
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# Source  : second part of the original VaR_1_.R (replaces FSVaR_2CP.R, which
#           plotted the 5% VaR)
# =============================================================================


###### SUB SAMPLE 1 ########

plotdata_VaR1_BTC_01 <- data.frame(VR_Date1_BTC, "BTC", VR11_BTC[,4], VR11_BTC[,1], VR12_BTC[,1], VR13_BTC[,1], VR14_BTC[,1], VR15_BTC[,1])
colnames(plotdata_VaR1_BTC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR1_ETH_01 <- data.frame(VR_Date1_ETH, "ETH", VR11_ETH[,4], VR11_ETH[,1], VR12_ETH[,1], VR13_ETH[,1], VR14_ETH[,1], VR15_ETH[,1])
colnames(plotdata_VaR1_ETH_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR1_XMR_01 <- data.frame(VR_Date1_XMR, "XMR", VR11_XMR[,4], VR11_XMR[,1], VR12_XMR[,1], VR13_XMR[,1], VR14_XMR[,1], VR15_XMR[,1])
colnames(plotdata_VaR1_XMR_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR1_LTC_01 <- data.frame(VR_Date1_LTC, "LTC", VR11_LTC[,4], VR11_LTC[,1], VR12_LTC[,1], VR13_LTC[,1], VR14_LTC[,1], VR15_LTC[,1])
colnames(plotdata_VaR1_LTC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR1_ETC_01 <- data.frame(VR_Date1_ETC, "ETC", VR11_ETC[,4], VR11_ETC[,1], VR12_ETC[,1], VR13_ETC[,1], VR14_ETC[,1], VR15_ETC[,1])
colnames(plotdata_VaR1_ETC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_S1_01 <- rbind(plotdata_VaR1_BTC_01, plotdata_VaR1_ETH_01, plotdata_VaR1_XMR_01, plotdata_VaR1_LTC_01, plotdata_VaR1_ETC_01)
View(plotdata_VaR_S1_01)

FSVaR_G1_01 <- ggplot(plotdata_VaR_S1_01, aes(x = Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Norm, colour = "Norm")) +
  geom_line(aes(x = Date, y = SST, color = "SST")) +
  geom_line(aes(x = Date, y = SGED, color = "SGED")) +
  geom_line(aes(x = Date, y = JSU, color = "JSU")) + 
  geom_line(aes(x = Date, y = PSIV, color = "PSIV")) +
  facet_grid(vars(plotdata_VaR_S1_01$Currency),scales = "free_y") +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Norm", "SST", "SGED", "JSU", "PSIV"),
                     values = c("Return" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PSIV" = "black")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for sub sample 1") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.key = element_rect(),
        legend.text = element_text(size = 7),
        legend.position = "bottom", 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 1))

ggexport(FSVaR_G1_01, filename = file.path(FIG_DIR, "FSVaR_SS1_01.eps"), height = 600, width = 300)

###::::::::::::::::::::::::::::::::::::::::############

###### SUB SAMPLE 2 ########

plotdata_VaR2_BTC_01 <- data.frame(VR_Date2_BTC, "BTC", VR21_BTC[,4], VR21_BTC[,1], VR22_BTC[,1], VR23_BTC[,1], VR24_BTC[,1], VR25_BTC[,1])
colnames(plotdata_VaR2_BTC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR2_ETH_01 <- data.frame(VR_Date2_ETH, "ETH", VR21_ETH[,4], VR21_ETH[,1], VR22_ETH[,1], VR23_ETH[,1], VR24_ETH[,1], VR25_ETH[,1])
colnames(plotdata_VaR2_ETH_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR2_XMR_01 <- data.frame(VR_Date2_XMR, "XMR", VR21_XMR[,4], VR21_XMR[,1], VR22_XMR[,1], VR23_XMR[,1], VR24_XMR[,1], VR25_XMR[,1])
colnames(plotdata_VaR2_XMR_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR2_LTC_01 <- data.frame(VR_Date2_LTC, "LTC", VR21_LTC[,4], VR21_LTC[,1], VR22_LTC[,1], VR23_LTC[,1], VR24_LTC[,1], VR25_LTC[,1])
colnames(plotdata_VaR2_LTC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR2_ETC_01 <- data.frame(VR_Date2_ETC, "ETC", VR21_ETC[,4], VR21_ETC[,1], VR22_ETC[,1], VR23_ETC[,1], VR24_ETC[,1], VR25_ETC[,1])
colnames(plotdata_VaR2_ETC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_S2_01 <- rbind(plotdata_VaR2_BTC_01, plotdata_VaR2_ETH_01, plotdata_VaR2_XMR_01, plotdata_VaR2_LTC_01, plotdata_VaR2_ETC_01)
View(plotdata_VaR_S2_01)

FSVaR_G2_01 <- ggplot(plotdata_VaR_S2_01, aes(x = plotdata_VaR_S2_01$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Norm, colour = "Norm")) +
  geom_line(aes(x = Date, y = SST, color = "SST")) +
  geom_line(aes(x = Date, y = SGED, color = "SGED")) +
  geom_line(aes(x = Date, y = JSU, color = "JSU")) + 
  geom_line(aes(x = Date, y = PSIV, color = "PSIV")) +
  facet_grid(vars(plotdata_VaR_S2_01$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~.) +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Norm", "SST", "SGED", "JSU", "PSIV"),
                     values = c("Return" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PSIV" = "black")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for sub sample 2") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0.5,size = 15),
        legend.title = element_blank(),
        legend.key = element_rect(),
        legend.text = element_text(size = 15),
        legend.position = "bottom", 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 1))

#ggsave(FSVaR_G2_01, filename = file.path(FIG_DIR, "FSVaR_S2_01.eps"), height = 600, width = 450)
ggsave(filename = file.path(FIG_DIR, "FSVaR_G2_01.eps"),
       plot = FSVaR_G2_01,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")
###::::::::::::::::::::::::::::::::::::::::############

###### SUB SAMPLE 3 ########

plotdata_VaR3_BTC_01 <- data.frame(VR_Date3_BTC, "BTC", VR31_BTC[,4], VR31_BTC[,1], VR32_BTC[,1], VR33_BTC[,1], VR34_BTC[,1], VR35_BTC[,1])
colnames(plotdata_VaR3_BTC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR3_ETH_01 <- data.frame(VR_Date3_ETH, "ETH", VR31_ETH[,4], VR31_ETH[,1], VR32_ETH[,1], VR33_ETH[,1], VR34_ETH[,1], VR35_ETH[,1])
colnames(plotdata_VaR3_ETH_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR3_XMR_01 <- data.frame(VR_Date3_XMR, "XMR", VR31_XMR[,4], VR31_XMR[,1], VR32_XMR[,1], VR33_XMR[,1], VR34_XMR[,1], VR35_XMR[,1])
colnames(plotdata_VaR3_XMR_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR3_LTC_01 <- data.frame(VR_Date3_LTC, "LTC", VR31_LTC[,4], VR31_LTC[,1], VR32_LTC[,1], VR33_LTC[,1], VR34_LTC[,1], VR35_LTC[,1])
colnames(plotdata_VaR3_LTC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR3_ETC_01 <- data.frame(VR_Date3_ETC, "ETC", VR31_ETC[,4], VR31_ETC[,1], VR32_ETC[,1], VR33_ETC[,1], VR34_ETC[,1], VR35_ETC[,1])
colnames(plotdata_VaR3_ETC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_S3_01 <- rbind(plotdata_VaR3_BTC_01, plotdata_VaR3_ETH_01, plotdata_VaR3_XMR_01, plotdata_VaR3_LTC_01, plotdata_VaR3_ETC_01)
View(plotdata_VaR_S3_01)

FSVaR_G3_01 <- ggplot(plotdata_VaR_S3_01, aes(x = plotdata_VaR_S3_01$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Norm, colour = "Norm")) +
  geom_line(aes(x = Date, y = SST, color = "SST")) +
  geom_line(aes(x = Date, y = SGED, color = "SGED")) +
  geom_line(aes(x = Date, y = JSU, color = "JSU")) + 
  geom_line(aes(x = Date, y = PSIV, color = "PSIV")) +
  facet_grid(vars(plotdata_VaR_S3_01$Currency),scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~.) +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Norm", "SST", "SGED", "JSU", "PSIV"),
                     values = c("Return" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PSIV" = "black")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for sub sample 3") +
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

#ggexport(FSVaR_G3_01, filename = file.path(FIG_DIR, "FSVaR_S3_01.eps"), height = 600, width = 450)

ggsave(filename = file.path(FIG_DIR, "FSVaR_G3_01.eps"),
       plot = FSVaR_G3_01,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")


###::::::::::::::::::::::::::::::::::::::::############

###### FULL SAMPLE ########

plotdata_VaR_BTC_01 <- data.frame(VR_Date_BTC, "BTC", VR1_BTC[,4], VR1_BTC[,2], VR2_BTC[,2], VR3_BTC[,2], VR4_BTC[,2], VR5_BTC[,1])
colnames(plotdata_VaR_BTC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_ETH_01 <- data.frame(VR_Date_ETH, "ETH", VR1_ETH[,4], VR1_ETH[,2], VR2_ETH[,2], VR3_ETH[,2], VR4_ETH[,2], VR5_ETH[,1])
colnames(plotdata_VaR_ETH_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_XMR_01 <- data.frame(VR_Date_XMR, "XMR", VR1_XMR[,4], VR1_XMR[,2], VR2_XMR[,2], VR3_XMR[,2], VR4_XMR[,2], VR5_XMR[,1])
colnames(plotdata_VaR_XMR_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_LTC_01 <- data.frame(VR_Date_LTC, "LTC", VR1_LTC[,4], VR1_LTC[,2], VR2_LTC[,2], VR3_LTC[,2], VR4_LTC[,2], VR5_LTC[,1])
colnames(plotdata_VaR_LTC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_ETC_01 <- data.frame(VR_Date_ETC, "ETC", VR1_ETC[,4], VR1_ETC[,2], VR2_ETC[,2], VR3_ETC[,2], VR4_ETC[,2], VR5_ETC[,1])
colnames(plotdata_VaR_ETC_01) <- c("Date", "Currency", "Returns","Norm", "SST", "SGED", "JSU", "PSIV")

plotdata_VaR_S_01 <- rbind(plotdata_VaR_BTC_01, plotdata_VaR_ETH_01, plotdata_VaR_XMR_01, plotdata_VaR_LTC_01, plotdata_VaR_ETC_01)
View(plotdata_VaR_S_01)

FSVaR_G_01 <- ggplot(plotdata_VaR_S_01, aes(x = plotdata_VaR_S_01$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Norm, colour = "Norm")) +
  geom_line(aes(x = Date, y = SST, color = "SST")) +
  geom_line(aes(x = Date, y = SGED, color = "SGED")) +
  geom_line(aes(x = Date, y = JSU, color = "JSU")) + 
  geom_line(aes(x = Date, y = PSIV, color = "PSIV")) +
  facet_grid(vars(plotdata_VaR_S_01$Currency),scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~.) +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Norm", "SST", "SGED", "JSU", "PSIV"),
                     values = c("Return" = "blue","Norm" = "yellow", "SST" = "green", "SGED" = "pink", "JSU" = "brown", "PSIV" = "black")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for entire data") +
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

#ggexport(FSVaR_G_01, filename = file.path(FIG_DIR, "FSVaR_S_01.eps"), height = 600, width = 450)

ggsave(filename = file.path(FIG_DIR, "FSVaR_G_01.eps"),
       plot = FSVaR_G_01,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

###::::::::::::::::::::::::::::::::::::::::############
