# =============================================================================
# 15_figures_var_gas.R
#
# Purpose : 1% VaR forecasts vs realised returns, GAS models, with and without
#           AST/AST1.
# Inputs  : objects from 03a-06
# Outputs : output/figures/FSVaR_*_GAS*.eps
# Paper   : Figure B.2
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

######## :::::::::: VaR plots of GAS models - with and without AST and AST1
####### Full Sample

plotdata_VaR_BTC_GAS <- data.frame(VR_Date_BTC, "BTC", VR1_BTC[,4], VaR_BTC_norm0, VaR_BTC_std0, VaR_BTC_sstd0, VaR_BTC_ast0, VaR_BTC_astone0)
colnames(plotdata_VaR_BTC_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_ETH_GAS <- data.frame(VR_Date_ETH, "ETH", VR1_ETH[,4], VaR_ETH_norm0, VaR_ETH_std0, VaR_ETH_sstd0, VaR_ETH_ast0, VaR_ETH_astone0)
colnames(plotdata_VaR_ETH_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_LTC_GAS <- data.frame(VR_Date_LTC, "LTC", VR1_LTC[,4], VaR_LTC_norm0, VaR_LTC_std0, VaR_LTC_sstd0, VaR_LTC_ast0, VaR_LTC_astone0)
colnames(plotdata_VaR_LTC_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_XMR_GAS <- data.frame(VR_Date_XMR, "XMR", VR1_XMR[,4], VaR_XMR_norm0, VaR_XMR_std0, VaR_XMR_sstd0, VaR_XMR_ast0, VaR_XMR_astone0)
colnames(plotdata_VaR_XMR_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_ETC_GAS <- data.frame(VR_Date_ETC, "ETC", VR1_ETC[,4], VaR_ETC_norm0, VaR_ETC_std0, VaR_ETC_sstd0, VaR_ETC_ast0, VaR_ETC_astone0)
colnames(plotdata_VaR_ETC_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_FS_GAS <- rbind(plotdata_VaR_BTC_GAS, plotdata_VaR_ETH_GAS, plotdata_VaR_LTC_GAS, plotdata_VaR_XMR_GAS, plotdata_VaR_ETC_GAS)
View(plotdata_VaR_FS_GAS)

FSVaR_FS_GAS <- ggplot(plotdata_VaR_FS_GAS, aes(x = plotdata_VaR_FS_GAS$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Normal, colour = "Normal")) +
  geom_line(aes(x = Date, y = STD, color = "STD")) +
  geom_line(aes(x = Date, y = SSTD, color = "SSTD")) +
  geom_line(aes(x = Date, y = AST, color = "AST")) + 
  geom_line(aes(x = Date, y = AST1, color = "AST1")) +
  facet_grid(vars(plotdata_VaR_FS_GAS$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = 'free') +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Normal", "STD", "SSTD", "AST", "AST1"),
                     values = c("Return" = "blue","Normal" = "yellow", "STD" = "green", "SSTD" = "pink", "AST" = "brown", "AST1" = "black")) +
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

ggsave(filename = file.path(FIG_DIR, "FSVaR_FS_GAS_T.eps"),
       plot = FSVaR_FS_GAS,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

####### SubSample 1

plotdata_VaR_BTC1_GAS <- data.frame(VR_Date1_BTC, "BTC", VR11_BTC[,4], VaR_BTC_norm1, VaR_BTC_std1, VaR_BTC_sstd1, VaR_BTC_ast1, VaR_BTC_astone1)
colnames(plotdata_VaR_BTC1_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_ETH1_GAS <- data.frame(VR_Date1_ETH, "ETH", VR11_ETH[,4], VaR_ETH_norm1, VaR_ETH_std1, VaR_ETH_sstd1, VaR_ETH_ast1, VaR_ETH_astone1)
colnames(plotdata_VaR_ETH1_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_LTC1_GAS <- data.frame(VR_Date1_LTC, "LTC", VR11_LTC[,4], VaR_LTC_norm1, VaR_LTC_std1, VaR_LTC_sstd1, VaR_LTC_ast1, VaR_LTC_astone1)
colnames(plotdata_VaR_LTC1_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_XMR1_GAS <- data.frame(VR_Date1_XMR, "XMR", VR11_XMR[,4], VaR_XMR_norm1, VaR_XMR_std1, VaR_XMR_sstd1, VaR_XMR_ast1, VaR_XMR_astone1)
colnames(plotdata_VaR_XMR1_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_ETC1_GAS <- data.frame(VR_Date1_ETC, "ETC", VR11_ETC[,4], VaR_ETC_norm1, VaR_ETC_std1, VaR_ETC_sstd1, VaR_ETC_ast1, VaR_ETC_astone1)
colnames(plotdata_VaR_ETC1_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_S1_GAS <- rbind(plotdata_VaR_BTC1_GAS, plotdata_VaR_ETH1_GAS, plotdata_VaR_LTC1_GAS, plotdata_VaR_XMR1_GAS, plotdata_VaR_ETC1_GAS)
View(plotdata_VaR_S1_GAS)

FSVaR_S1_GAS <- ggplot(plotdata_VaR_S1_GAS, aes(x = plotdata_VaR_S1_GAS$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Normal, colour = "Normal")) +
  geom_line(aes(x = Date, y = STD, color = "STD")) +
  geom_line(aes(x = Date, y = SSTD, color = "SSTD")) +
  geom_line(aes(x = Date, y = AST, color = "AST")) + 
  geom_line(aes(x = Date, y = AST1, color = "AST1")) +
  facet_grid(vars(plotdata_VaR_S1_GAS$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = 'free') +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Normal", "STD", "SSTD", "AST", "AST1"),
                     values = c("Return" = "blue","Normal" = "yellow", "STD" = "green", "SSTD" = "pink", "AST" = "brown", "AST1" = "black")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for SS1 data") +
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

ggsave(filename = file.path(FIG_DIR, "FSVaR_S1_GAS_T.eps"),
       plot = FSVaR_S1_GAS,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

####### SubSample 2

plotdata_VaR_BTC2_GAS <- data.frame(VR_Date2_BTC, "BTC", VR21_BTC[,4], VaR_BTC_norm2, VaR_BTC_std2, VaR_BTC_sstd2, VaR_BTC_ast2, VaR_BTC_astone2)
colnames(plotdata_VaR_BTC2_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_ETH2_GAS <- data.frame(VR_Date2_ETH, "ETH", VR21_ETH[,4], VaR_ETH_norm2, VaR_ETH_std2, VaR_ETH_sstd2, VaR_ETH_ast2, VaR_ETH_astone2)
colnames(plotdata_VaR_ETH2_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_LTC2_GAS <- data.frame(VR_Date2_LTC, "LTC", VR21_LTC[,4], VaR_LTC_norm2, VaR_LTC_std2, VaR_LTC_sstd2, VaR_LTC_ast2, VaR_LTC_astone2)
colnames(plotdata_VaR_LTC2_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_XMR2_GAS <- data.frame(VR_Date2_XMR, "XMR", VR21_XMR[,4], VaR_XMR_norm2, VaR_XMR_std2, VaR_XMR_sstd2, VaR_XMR_ast2, VaR_XMR_astone2)
colnames(plotdata_VaR_XMR2_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_ETC2_GAS <- data.frame(VR_Date2_ETC, "ETC", VR21_ETC[,4], VaR_ETC_norm2, VaR_ETC_std2, VaR_ETC_sstd2, VaR_ETC_ast2, VaR_ETC_astone2)
colnames(plotdata_VaR_ETC2_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_S2_GAS <- rbind(plotdata_VaR_BTC2_GAS, plotdata_VaR_ETH2_GAS, plotdata_VaR_LTC2_GAS, plotdata_VaR_XMR2_GAS, plotdata_VaR_ETC2_GAS)
View(plotdata_VaR_S2_GAS)

FSVaR_S2_GAS <- ggplot(plotdata_VaR_S2_GAS, aes(x = plotdata_VaR_S2_GAS$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Normal, colour = "Normal")) +
  geom_line(aes(x = Date, y = STD, color = "STD")) +
  geom_line(aes(x = Date, y = SSTD, color = "SSTD")) +
  geom_line(aes(x = Date, y = AST, color = "AST")) + 
  geom_line(aes(x = Date, y = AST1, color = "AST1")) +
  facet_grid(vars(plotdata_VaR_S2_GAS$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = 'free') +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Normal", "STD", "SSTD", "AST", "AST1"),
                     values = c("Return" = "blue","Normal" = "yellow", "STD" = "green", "SSTD" = "pink", "AST" = "brown", "AST1" = "black")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for SS2 data") +
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

ggsave(filename = file.path(FIG_DIR, "FSVaR_S2_GAS_T.eps"),
       plot = FSVaR_S2_GAS,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

####### SubSample 3

plotdata_VaR_BTC3_GAS <- data.frame(VR_Date3_BTC, "BTC", VR31_BTC[,4], VaR_BTC_norm3, VaR_BTC_std3, VaR_BTC_sstd3, VaR_BTC_ast3, VaR_BTC_astone3)
colnames(plotdata_VaR_BTC3_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_ETH3_GAS <- data.frame(VR_Date3_ETH, "ETH", VR31_ETH[,4], VaR_ETH_norm3, VaR_ETH_std3, VaR_ETH_sstd3, VaR_ETH_ast3, VaR_ETH_astone3)
colnames(plotdata_VaR_ETH3_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_LTC3_GAS <- data.frame(VR_Date3_LTC, "LTC", VR31_LTC[,4], VaR_LTC_norm3, VaR_LTC_std3, VaR_LTC_sstd3, VaR_LTC_ast3, VaR_LTC_astone3)
colnames(plotdata_VaR_LTC3_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_XMR3_GAS <- data.frame(VR_Date3_XMR, "XMR", VR31_XMR[,4], VaR_XMR_norm3, VaR_XMR_std3, VaR_XMR_sstd3, VaR_XMR_ast3, VaR_XMR_astone3)
colnames(plotdata_VaR_XMR3_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_ETC3_GAS <- data.frame(VR_Date3_ETC, "ETC", VR31_ETC[,4], VaR_ETC_norm3, VaR_ETC_std3, VaR_ETC_sstd3, VaR_ETC_ast3, VaR_ETC_astone3)
colnames(plotdata_VaR_ETC3_GAS) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD", "AST", "AST1")

plotdata_VaR_S3_GAS <- rbind(plotdata_VaR_BTC3_GAS, plotdata_VaR_ETH3_GAS, plotdata_VaR_LTC3_GAS, plotdata_VaR_XMR3_GAS, plotdata_VaR_ETC3_GAS)
View(plotdata_VaR_S3_GAS)

FSVaR_S3_GAS <- ggplot(plotdata_VaR_S3_GAS, aes(x = plotdata_VaR_S3_GAS$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Normal, colour = "Normal")) +
  geom_line(aes(x = Date, y = STD, color = "STD")) +
  geom_line(aes(x = Date, y = SSTD, color = "SSTD")) +
  geom_line(aes(x = Date, y = AST, color = "AST")) + 
  geom_line(aes(x = Date, y = AST1, color = "AST1")) +
  facet_grid(vars(plotdata_VaR_S3_GAS$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = 'free') +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Normal", "STD", "SSTD", "AST", "AST1"),
                     values = c("Return" = "blue","Normal" = "yellow", "STD" = "green", "SSTD" = "pink", "AST" = "brown", "AST1" = "black")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for SS3 data") +
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

ggsave(filename = file.path(FIG_DIR, "FSVaR_S3_GAS_T.eps"),
       plot = FSVaR_S3_GAS,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

############################### :::::::::::::::::::::::::::::: ###############################

####:::::: Without AST and AST1

plotdata_VaR_BTC_GAS_try <- data.frame(VR_Date_BTC, "BTC", VR1_BTC[,4], VaR_BTC_norm0, VaR_BTC_std0, VaR_BTC_sstd0)
colnames(plotdata_VaR_BTC_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_ETH_GAS_try <- data.frame(VR_Date_ETH, "ETH", VR1_ETH[,4], VaR_ETH_norm0, VaR_ETH_std0, VaR_ETH_sstd0)
colnames(plotdata_VaR_ETH_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_LTC_GAS_try <- data.frame(VR_Date_LTC, "LTC", VR1_LTC[,4], VaR_LTC_norm0, VaR_LTC_std0, VaR_LTC_sstd0)
colnames(plotdata_VaR_LTC_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_XMR_GAS_try <- data.frame(VR_Date_XMR, "XMR", VR1_XMR[,4], VaR_XMR_norm0, VaR_XMR_std0, VaR_XMR_sstd0)
colnames(plotdata_VaR_XMR_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_ETC_GAS_try <- data.frame(VR_Date_ETC, "ETC", VR1_ETC[,4], VaR_ETC_norm0, VaR_ETC_std0, VaR_ETC_sstd0)
colnames(plotdata_VaR_ETC_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_FS_GAS_try <- rbind(plotdata_VaR_BTC_GAS_try, plotdata_VaR_ETH_GAS_try, plotdata_VaR_LTC_GAS_try, plotdata_VaR_XMR_GAS_try, plotdata_VaR_ETC_GAS_try)
View(plotdata_VaR_FS_GAS_try)

FSVaR_FS_GAS_try <- ggplot(plotdata_VaR_FS_GAS_try, aes(x = plotdata_VaR_FS_GAS_try$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Normal, colour = "Normal")) +
  geom_line(aes(x = Date, y = STD, color = "STD")) +
  geom_line(aes(x = Date, y = SSTD, color = "SSTD")) +
  facet_grid(vars(plotdata_VaR_FS_GAS_try$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = 'free') +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Normal", "STD", "SSTD"),
                     values = c("Return" = "blue","Normal" = "red", "STD" = "green", "SSTD" = "pink")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for entire data without Asymmetric student-t distribution") +
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

ggsave(filename = file.path(FIG_DIR, "FSVaR_FS_GAS_try_T.eps"),
       plot = FSVaR_FS_GAS_try,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

####### SubSample 1

plotdata_VaR_BTC1_GAS_try <- data.frame(VR_Date1_BTC, "BTC", VR11_BTC[,4], VaR_BTC_norm1, VaR_BTC_std1, VaR_BTC_sstd1)
colnames(plotdata_VaR_BTC1_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_ETH1_GAS_try <- data.frame(VR_Date1_ETH, "ETH", VR11_ETH[,4], VaR_ETH_norm1, VaR_ETH_std1, VaR_ETH_sstd1)
colnames(plotdata_VaR_ETH1_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_LTC1_GAS_try <- data.frame(VR_Date1_LTC, "LTC", VR11_LTC[,4], VaR_LTC_norm1, VaR_LTC_std1, VaR_LTC_sstd1)
colnames(plotdata_VaR_LTC1_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_XMR1_GAS_try <- data.frame(VR_Date1_XMR, "XMR", VR11_XMR[,4], VaR_XMR_norm1, VaR_XMR_std1, VaR_XMR_sstd1)
colnames(plotdata_VaR_XMR1_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_ETC1_GAS_try <- data.frame(VR_Date1_ETC, "ETC", VR11_ETC[,4], VaR_ETC_norm1, VaR_ETC_std1, VaR_ETC_sstd1)
colnames(plotdata_VaR_ETC1_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_S1_GAS_try <- rbind(plotdata_VaR_BTC1_GAS_try, plotdata_VaR_ETH1_GAS_try, plotdata_VaR_LTC1_GAS_try, plotdata_VaR_XMR1_GAS_try, plotdata_VaR_ETC1_GAS_try)
View(plotdata_VaR_S1_GAS_try)

FSVaR_S1_GAS_try <- ggplot(plotdata_VaR_S1_GAS_try, aes(x = plotdata_VaR_S1_GAS_try$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Normal, colour = "Normal")) +
  geom_line(aes(x = Date, y = STD, color = "STD")) +
  geom_line(aes(x = Date, y = SSTD, color = "SSTD")) +
  facet_grid(vars(plotdata_VaR_S1_GAS_try$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = 'free') +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Normal", "STD", "SSTD"),
                     values = c("Return" = "blue","Normal" = "red", "STD" = "green", "SSTD" = "pink")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for SS1 data without Asymmetric student-t distribution") +
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

ggsave(filename = file.path(FIG_DIR, "FSVaR_S1_GAS_try_T.eps"),
       plot = FSVaR_S1_GAS_try,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

####### SubSample 2

plotdata_VaR_BTC2_GAS_try <- data.frame(VR_Date2_BTC, "BTC", VR21_BTC[,4], VaR_BTC_norm2, VaR_BTC_std2, VaR_BTC_sstd2)
colnames(plotdata_VaR_BTC2_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_ETH2_GAS_try <- data.frame(VR_Date2_ETH, "ETH", VR21_ETH[,4], VaR_ETH_norm2, VaR_ETH_std2, VaR_ETH_sstd2)
colnames(plotdata_VaR_ETH2_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_LTC2_GAS_try <- data.frame(VR_Date2_LTC, "LTC", VR21_LTC[,4], VaR_LTC_norm2, VaR_LTC_std2, VaR_LTC_sstd2)
colnames(plotdata_VaR_LTC2_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_XMR2_GAS_try <- data.frame(VR_Date2_XMR, "XMR", VR21_XMR[,4], VaR_XMR_norm2, VaR_XMR_std2, VaR_XMR_sstd2)
colnames(plotdata_VaR_XMR2_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_ETC2_GAS_try <- data.frame(VR_Date2_ETC, "ETC", VR21_ETC[,4], VaR_ETC_norm2, VaR_ETC_std2, VaR_ETC_sstd2)
colnames(plotdata_VaR_ETC2_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_S2_GAS_try <- rbind(plotdata_VaR_BTC2_GAS_try, plotdata_VaR_ETH2_GAS_try, plotdata_VaR_LTC2_GAS_try, plotdata_VaR_XMR2_GAS_try, plotdata_VaR_ETC2_GAS_try)
View(plotdata_VaR_S2_GAS_try)

FSVaR_S2_GAS_try <- ggplot(plotdata_VaR_S2_GAS_try, aes(x = plotdata_VaR_S2_GAS_try$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Normal, colour = "Normal")) +
  geom_line(aes(x = Date, y = STD, color = "STD")) +
  geom_line(aes(x = Date, y = SSTD, color = "SSTD")) +
  facet_grid(vars(plotdata_VaR_S2_GAS_try$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = 'free') +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Normal", "STD", "SSTD"),
                     values = c("Return" = "blue","Normal" = "red", "STD" = "green", "SSTD" = "pink")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for SS2 data without Asymmetric student-t distribution") +
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

ggsave(filename = file.path(FIG_DIR, "FSVaR_S2_GAS_try_T.eps"),
       plot = FSVaR_S2_GAS_try,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

####### SubSample 3

plotdata_VaR_BTC3_GAS_try <- data.frame(VR_Date3_BTC, "BTC", VR31_BTC[,4], VaR_BTC_norm3, VaR_BTC_std3, VaR_BTC_sstd3)
colnames(plotdata_VaR_BTC3_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_ETH3_GAS_try <- data.frame(VR_Date3_ETH, "ETH", VR31_ETH[,4], VaR_ETH_norm3, VaR_ETH_std3, VaR_ETH_sstd3)
colnames(plotdata_VaR_ETH3_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_LTC3_GAS_try <- data.frame(VR_Date3_LTC, "LTC", VR31_LTC[,4], VaR_LTC_norm3, VaR_LTC_std3, VaR_LTC_sstd3)
colnames(plotdata_VaR_LTC3_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_XMR3_GAS_try <- data.frame(VR_Date3_XMR, "XMR", VR31_XMR[,4], VaR_XMR_norm3, VaR_XMR_std3, VaR_XMR_sstd3)
colnames(plotdata_VaR_XMR3_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_ETC3_GAS_try <- data.frame(VR_Date3_ETC, "ETC", VR31_ETC[,4], VaR_ETC_norm3, VaR_ETC_std3, VaR_ETC_sstd3)
colnames(plotdata_VaR_ETC3_GAS_try) <- c("Date", "Currency", "Returns","Normal", "STD", "SSTD")

plotdata_VaR_S3_GAS_try <- rbind(plotdata_VaR_BTC3_GAS_try, plotdata_VaR_ETH3_GAS_try, plotdata_VaR_LTC3_GAS_try, plotdata_VaR_XMR3_GAS_try, plotdata_VaR_ETC3_GAS_try)
View(plotdata_VaR_S3_GAS_try)

FSVaR_S3_GAS_try <- ggplot(plotdata_VaR_S3_GAS_try, aes(x = plotdata_VaR_S3_GAS_try$Date)) +
  geom_line(aes(y = Returns, colour = "Return")) +
  geom_line(aes(x = Date, y = Normal, colour = "Normal")) +
  geom_line(aes(x = Date, y = STD, color = "STD")) +
  geom_line(aes(x = Date, y = SSTD, color = "SSTD")) +
  facet_grid(vars(plotdata_VaR_S3_GAS_try$Currency), scales = "free_y") +
  facet_grid(factor(Currency, levels = c("BTC", "ETH", "LTC", "XMR", "ETC")) ~., scales = 'free') +
  scale_color_manual(name = "Models", 
                     labels = c("Return","Normal", "STD", "SSTD"),
                     values = c("Return" = "blue","Normal" = "red", "STD" = "green", "SSTD" = "pink")) +
  ylab("Log Returns/ VaR at 1%") + 
  xlab("Date") +
  ggtitle("Forecasting sample for SS3 data without Asymmetric student-t distribution") +
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

ggsave(filename = file.path(FIG_DIR, "FSVaR_S3_GAS_try_T.eps"),
       plot = FSVaR_S3_GAS_try,
       width = 300,    # you can play with width and height
       height = 200,
       dpi = 800,
       units = "mm")

############################### :::::::::::::::::::::::::::::: ###############################