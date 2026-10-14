# =============================================================================
# 03f_var_series_1pct.R
#
# Purpose : Collects the 1% VaR forecasts of every EGARCH model (Norm, SST,
#           SGED, JSU, PSIV) for the full sample and SS1-SS3 into the VR*
#           objects, together with their dates. In particular, the PSIV series
#           (VR5_*, VR15_*, VR25_*, VR35_*) are re-built here at the 1% level;
#           the coin scripts 03a-03e leave them at the 5% level.
# Inputs  : objects from 03a-03e
# Outputs : VR1_<coin> ... VR5_<coin>, VR11_<coin> ... VR35_<coin>, VR_Date*_<coin>
# Paper   : used by the QPS (Table 5), GAS and figure scripts that follow
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# Source  : first part of the original VaR_1_.R
# =============================================================================

######### VAR PLOTS - GARCH, GAS ##########

# Sub Sample 1

Z_Pearson_Sample1_BTC_01   <- qpearson(0.01,BTC_Param_Pearson_Sample1)
Sample1_BTC_Holdout_VaR_01   <- Sample1_BTC_Holdout_Mu + Sample1_BTC_Holdout_Sigma*Z_Pearson_Sample1_BTC_01
VR11_BTC <- data.frame(CrCu_BTC_VaR_Normal1)
VR12_BTC <- data.frame(CrCu_BTC_VaR_SST1)
VR13_BTC <- data.frame(CrCu_BTC_VaR_SGED1)
VR14_BTC <- data.frame(CrCu_BTC_VaR_JSU1)
VR15_BTC <- data.frame(Sample1_BTC_Holdout_VaR_01)
VR_Date1_BTC <- CrCu_BTC$Crypto_Date[(CrCu_BTC_samples[1]-59):CrCu_BTC_samples[1]]

Z_Pearson_Sample1_ETH_01   <- qpearson(0.01,ETH_Param_Pearson_Sample1)
Sample1_ETH_Holdout_VaR_01   <- Sample1_ETH_Holdout_Mu + Sample1_ETH_Holdout_Sigma*Z_Pearson_Sample1_ETH_01
VR11_ETH <- data.frame(CrCu_ETH_VaR_Normal1)
VR12_ETH <- data.frame(CrCu_ETH_VaR_SST1)
VR13_ETH <- data.frame(CrCu_ETH_VaR_SGED1)
VR14_ETH <- data.frame(CrCu_ETH_VaR_JSU1)
VR15_ETH <- data.frame(Sample1_ETH_Holdout_VaR_01)
VR_Date1_ETH <- CrCu_ETH$Crypto_Date[(CrCu_ETH_samples[1]-59):CrCu_ETH_samples[1]]

Z_Pearson_Sample1_LTC_01   <- qpearson(0.01,LTC_Param_Pearson_Sample1)
Sample1_LTC_Holdout_VaR_01   <- Sample1_LTC_Holdout_Mu + Sample1_LTC_Holdout_Sigma*Z_Pearson_Sample1_LTC_01
VR11_LTC <- data.frame(CrCu_LTC_VaR_Normal1)
VR12_LTC <- data.frame(CrCu_LTC_VaR_SST1)
VR13_LTC <- data.frame(CrCu_LTC_VaR_SGED1)
VR14_LTC <- data.frame(CrCu_LTC_VaR_JSU1)
VR15_LTC <- data.frame(Sample1_LTC_Holdout_VaR_01)
VR_Date1_LTC <- CrCu_LTC$Crypto_Date[(CrCu_LTC_samples[1]-59):CrCu_LTC_samples[1]]

Z_Pearson_Sample1_XMR_01   <- qpearson(0.01,XMR_Param_Pearson_Sample1)
Sample1_XMR_Holdout_VaR_01   <- Sample1_XMR_Holdout_Mu + Sample1_XMR_Holdout_Sigma*Z_Pearson_Sample1_XMR_01
VR11_XMR <- data.frame(CrCu_XMR_VaR_Normal1)
VR12_XMR <- data.frame(CrCu_XMR_VaR_SST1)
VR13_XMR <- data.frame(CrCu_XMR_VaR_SGED1)
VR14_XMR <- data.frame(CrCu_XMR_VaR_JSU1)
VR15_XMR <- data.frame(Sample1_XMR_Holdout_VaR_01)
VR_Date1_XMR <- CrCu_XMR$Crypto_Date[(CrCu_XMR_samples[1]-59):CrCu_XMR_samples[1]]

Z_Pearson_Sample1_ETC_01   <- qpearson(0.01,ETC_Param_Pearson_Sample1)
Sample1_ETC_Holdout_VaR_01   <- Sample1_ETC_Holdout_Mu + Sample1_ETC_Holdout_Sigma*Z_Pearson_Sample1_ETC_01
VR11_ETC <- data.frame(CrCu_ETC_VaR_Normal1)
VR12_ETC <- data.frame(CrCu_ETC_VaR_SST1)
VR13_ETC <- data.frame(CrCu_ETC_VaR_SGED1)
VR14_ETC <- data.frame(CrCu_ETC_VaR_JSU1)
VR15_ETC <- data.frame(Sample1_ETC_Holdout_VaR_01)
VR_Date1_ETC <- CrCu_ETC$Crypto_Date[(CrCu_ETC_samples[1]-59):CrCu_ETC_samples[1]]

# Sub Sample 2

Z_Pearson_Sample2_BTC_01   <- qpearson(0.01,BTC_Param_Pearson_Sample2)
Sample2_BTC_Holdout_VaR_01   <- Sample2_BTC_Holdout_Mu + Sample2_BTC_Holdout_Sigma*Z_Pearson_Sample2_BTC_01

VR21_BTC <- data.frame(CrCu_BTC_VaR_Normal2)
VR22_BTC <- data.frame(CrCu_BTC_VaR_SST2)
VR23_BTC <- data.frame(CrCu_BTC_VaR_SGED2)
VR24_BTC <- data.frame(CrCu_BTC_VaR_JSU2)
VR25_BTC <- data.frame(Sample2_BTC_Holdout_VaR_01)
VR_Date2_BTC <- CrCu_BTC$Crypto_Date[(CrCu_BTC_samples[2]-199):CrCu_BTC_samples[2]]

Z_Pearson_Sample2_ETH_01   <- qpearson(0.01,ETH_Param_Pearson_Sample2)
Sample2_ETH_Holdout_VaR_01   <- Sample2_ETH_Holdout_Mu + Sample2_ETH_Holdout_Sigma*Z_Pearson_Sample2_ETH_01

VR21_ETH <- data.frame(CrCu_ETH_VaR_Normal2)
VR22_ETH <- data.frame(CrCu_ETH_VaR_SST2)
VR23_ETH <- data.frame(CrCu_ETH_VaR_SGED2)
VR24_ETH <- data.frame(CrCu_ETH_VaR_JSU2)
VR25_ETH <- data.frame(Sample2_ETH_Holdout_VaR_01)
VR_Date2_ETH <- CrCu_ETH$Crypto_Date[(CrCu_ETH_samples[2]-199):CrCu_ETH_samples[2]]

Z_Pearson_Sample2_LTC_01   <- qpearson(0.01,LTC_Param_Pearson_Sample2)
Sample2_LTC_Holdout_VaR_01   <- Sample2_LTC_Holdout_Mu + Sample2_LTC_Holdout_Sigma*Z_Pearson_Sample2_LTC_01

VR21_LTC <- data.frame(CrCu_LTC_VaR_Normal2)
VR22_LTC <- data.frame(CrCu_LTC_VaR_SST2)
VR23_LTC <- data.frame(CrCu_LTC_VaR_SGED2)
VR24_LTC <- data.frame(CrCu_LTC_VaR_JSU2)
VR25_LTC <- data.frame(Sample2_LTC_Holdout_VaR_01)
VR_Date2_LTC <- CrCu_LTC$Crypto_Date[(CrCu_LTC_samples[2]-199):CrCu_LTC_samples[2]]

Z_Pearson_Sample2_XMR_01   <- qpearson(0.01,XMR_Param_Pearson_Sample2)
Sample2_XMR_Holdout_VaR_01   <- Sample2_XMR_Holdout_Mu + Sample2_XMR_Holdout_Sigma*Z_Pearson_Sample2_XMR_01

VR21_XMR <- data.frame(CrCu_XMR_VaR_Normal2)
VR22_XMR <- data.frame(CrCu_XMR_VaR_SST2)
VR23_XMR <- data.frame(CrCu_XMR_VaR_SGED2)
VR24_XMR <- data.frame(CrCu_XMR_VaR_JSU2)
VR25_XMR <- data.frame(Sample2_XMR_Holdout_VaR_01)
VR_Date2_XMR <- CrCu_XMR$Crypto_Date[(CrCu_XMR_samples[2]-199):CrCu_XMR_samples[2]]

Z_Pearson_Sample2_ETC_01   <- qpearson(0.01,ETC_Param_Pearson_Sample2)
Sample2_ETC_Holdout_VaR_01   <- Sample2_ETC_Holdout_Mu + Sample2_ETC_Holdout_Sigma*Z_Pearson_Sample2_ETC_01

VR21_ETC <- data.frame(CrCu_ETC_VaR_Normal2)
VR22_ETC <- data.frame(CrCu_ETC_VaR_SST2)
VR23_ETC <- data.frame(CrCu_ETC_VaR_SGED2)
VR24_ETC <- data.frame(CrCu_ETC_VaR_JSU2)
VR25_ETC <- data.frame(Sample2_ETC_Holdout_VaR_01)
VR_Date2_ETC <- CrCu_ETC$Crypto_Date[(CrCu_ETC_samples[2]-199):CrCu_ETC_samples[2]]

# Sub Sample 3

Z_Pearson_Sample3_BTC_01   <- qpearson(0.01,BTC_Param_Pearson_Sample3)
Sample3_BTC_Holdout_VaR_01   <- Sample3_BTC_Holdout_Mu + Sample3_BTC_Holdout_Sigma*Z_Pearson_Sample3_BTC_01

VR31_BTC <- data.frame(CrCu_BTC_VaR_Normal3)
VR32_BTC <- data.frame(CrCu_BTC_VaR_SST3)
VR33_BTC <- data.frame(CrCu_BTC_VaR_SGED3)
VR34_BTC <- data.frame(CrCu_BTC_VaR_JSU3)
VR35_BTC <- data.frame(Sample3_BTC_Holdout_VaR_01)
VR_Date3_BTC <- CrCu_BTC$Crypto_Date[(length(CrCu_BTC_xts)-99):length(CrCu_BTC_xts)]

Z_Pearson_Sample3_ETH_01   <- qpearson(0.01,ETH_Param_Pearson_Sample3)
Sample3_ETH_Holdout_VaR_01   <- Sample3_ETH_Holdout_Mu + Sample3_ETH_Holdout_Sigma*Z_Pearson_Sample3_ETH_01

VR31_ETH <- data.frame(CrCu_ETH_VaR_Normal3)
VR32_ETH <- data.frame(CrCu_ETH_VaR_SST3)
VR33_ETH <- data.frame(CrCu_ETH_VaR_SGED3)
VR34_ETH <- data.frame(CrCu_ETH_VaR_JSU3)
VR35_ETH <- data.frame(Sample3_ETH_Holdout_VaR_01)
VR_Date3_ETH <- CrCu_ETH$Crypto_Date[(length(CrCu_ETH_xts)-99):length(CrCu_ETH_xts)]

Z_Pearson_Sample3_LTC_01   <- qpearson(0.01,LTC_Param_Pearson_Sample3)
Sample3_LTC_Holdout_VaR_01   <- Sample3_LTC_Holdout_Mu + Sample3_LTC_Holdout_Sigma*Z_Pearson_Sample3_LTC_01

VR31_LTC <- data.frame(CrCu_LTC_VaR_Normal3)
VR32_LTC <- data.frame(CrCu_LTC_VaR_SST3)
VR33_LTC <- data.frame(CrCu_LTC_VaR_SGED3)
VR34_LTC <- data.frame(CrCu_LTC_VaR_JSU3)
VR35_LTC <- data.frame(Sample3_LTC_Holdout_VaR_01)
VR_Date3_LTC <- CrCu_LTC$Crypto_Date[(length(CrCu_LTC_xts)-99):length(CrCu_LTC_xts)]

Z_Pearson_Sample3_XMR_01   <- qpearson(0.01,XMR_Param_Pearson_Sample3)
Sample3_XMR_Holdout_VaR_01   <- Sample3_XMR_Holdout_Mu + Sample3_XMR_Holdout_Sigma*Z_Pearson_Sample3_XMR_01

VR31_XMR <- data.frame(CrCu_XMR_VaR_Normal3)
VR32_XMR <- data.frame(CrCu_XMR_VaR_SST3)
VR33_XMR <- data.frame(CrCu_XMR_VaR_SGED3)
VR34_XMR <- data.frame(CrCu_XMR_VaR_JSU3)
VR35_XMR <- data.frame(Sample3_XMR_Holdout_VaR_01)
VR_Date3_XMR <- CrCu_XMR$Crypto_Date[(length(CrCu_XMR_xts)-99):length(CrCu_XMR_xts)]

Z_Pearson_Sample3_ETC_01   <- qpearson(0.01,ETC_Param_Pearson_Sample3)
Sample3_ETC_Holdout_VaR_01   <- Sample3_ETC_Holdout_Mu + Sample3_ETC_Holdout_Sigma*Z_Pearson_Sample3_ETC_01

VR31_ETC <- data.frame(CrCu_ETC_VaR_Normal3)
VR32_ETC <- data.frame(CrCu_ETC_VaR_SST3)
VR33_ETC <- data.frame(CrCu_ETC_VaR_SGED3)
VR34_ETC <- data.frame(CrCu_ETC_VaR_JSU3)
VR35_ETC <- data.frame(Sample3_ETC_Holdout_VaR_01)
VR_Date3_ETC <- CrCu_ETC$Crypto_Date[(length(CrCu_ETC_xts)-99):length(CrCu_ETC_xts)]

#Full Data

Z_Pearson_BTC_01   <- qpearson(0.01,CrCu_BTC_Dist8)
BTC_Holdout_VaR_01   <- BTC_Holdout_Mu + BTC_Holdout_Sigma*Z_Pearson_BTC_01

VR1_BTC <- data.frame(CrCu_BTC_Roll8@forecast$VaR)
VR2_BTC <- data.frame(CrCu_BTC_Roll5@forecast$VaR)
VR3_BTC <- data.frame(CrCu_BTC_Roll6@forecast$VaR)
VR4_BTC <- data.frame(CrCu_BTC_Roll7@forecast$VaR)
VR5_BTC <- data.frame(BTC_Holdout_VaR_01)
VR_Date_BTC <- CrCu_BTC$Crypto_Date[(length(CrCu_BTC_xts)-349):length(CrCu_BTC_xts)]

Z_Pearson_ETH_01   <- qpearson(0.01,CrCu_ETH_Dist8)
ETH_Holdout_VaR_01   <- ETH_Holdout_Mu + ETH_Holdout_Sigma*Z_Pearson_ETH_01

VR1_ETH <- data.frame(CrCu_ETH_Roll8@forecast$VaR)
VR2_ETH <- data.frame(CrCu_ETH_Roll5@forecast$VaR)
VR3_ETH <- data.frame(CrCu_ETH_Roll6@forecast$VaR)
VR4_ETH <- data.frame(CrCu_ETH_Roll7@forecast$VaR)
VR5_ETH <- data.frame(ETH_Holdout_VaR_01)
VR_Date_ETH <- CrCu_ETH$Crypto_Date[(length(CrCu_ETH_xts)-349):length(CrCu_ETH_xts)]

Z_Pearson_LTC_01   <- qpearson(0.01,CrCu_LTC_Dist8)
LTC_Holdout_VaR_01   <- LTC_Holdout_Mu + LTC_Holdout_Sigma*Z_Pearson_LTC_01

VR1_LTC <- data.frame(CrCu_LTC_Roll8@forecast$VaR)
VR2_LTC <- data.frame(CrCu_LTC_Roll5@forecast$VaR)
VR3_LTC <- data.frame(CrCu_LTC_Roll6@forecast$VaR)
VR4_LTC <- data.frame(CrCu_LTC_Roll7@forecast$VaR)
VR5_LTC <- data.frame(LTC_Holdout_VaR_01)
VR_Date_LTC <- CrCu_LTC$Crypto_Date[(length(CrCu_LTC_xts)-349):length(CrCu_LTC_xts)]

Z_Pearson_XMR_01   <- qpearson(0.01,CrCu_XMR_Dist8)
XMR_Holdout_VaR_01   <- XMR_Holdout_Mu + XMR_Holdout_Sigma*Z_Pearson_XMR_01

VR1_XMR <- data.frame(CrCu_XMR_Roll8@forecast$VaR)
VR2_XMR <- data.frame(CrCu_XMR_Roll5@forecast$VaR)
VR3_XMR <- data.frame(CrCu_XMR_Roll6@forecast$VaR)
VR4_XMR <- data.frame(CrCu_XMR_Roll7@forecast$VaR)
VR5_XMR <- data.frame(XMR_Holdout_VaR_01)
VR_Date_XMR <- CrCu_XMR$Crypto_Date[(length(CrCu_XMR_xts)-349):length(CrCu_XMR_xts)]

Z_Pearson_ETC_01   <- qpearson(0.01,CrCu_ETC_Dist8)
ETC_Holdout_VaR_01   <- ETC_Holdout_Mu + ETC_Holdout_Sigma*Z_Pearson_ETC_01

VR1_ETC <- data.frame(CrCu_ETC_Roll8@forecast$VaR)
VR2_ETC <- data.frame(CrCu_ETC_Roll5@forecast$VaR)
VR3_ETC <- data.frame(CrCu_ETC_Roll6@forecast$VaR)
VR4_ETC <- data.frame(CrCu_ETC_Roll7@forecast$VaR)
VR5_ETC <- data.frame(ETC_Holdout_VaR_01)
VR_Date_ETC <- CrCu_ETC$Crypto_Date[(length(CrCu_ETC_xts)-349):length(CrCu_ETC_xts)]
