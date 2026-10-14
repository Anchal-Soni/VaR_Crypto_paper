# =============================================================================
# 01_change_points.R
#
# Purpose : Common structural breaks in the 5-coin price panel: t-SNE + CUSUM binary
#           segmentation (main method) and three PCA-based benchmarks (PC1, PC1^2,
#           |PC1|).
# Inputs  : data/Cryptocurrency6.csv; R/functions/strucbreak.R; R/functions/cp_pca*.R
# Outputs : break locations printed to the console (CPPP.tsne, CPPP, CPPPs, CPPPa)
# Paper   : Section 4.1, Figure 3 dates, PCA comparison. CPPP.tsne (300, 1300)
#           is used by 02_descriptive_stats_subsamples.R
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

#### ::::::::: Value at Risk for Cryptocurrencies ####
#### ::::::::: Objective is to find the 1-day ahead VaR for Cryptocurrencies ####
#### ::::::::: find structural breaks using tSNE and CUSUM based Binary Segmentation ####

## Load the following libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(fBasics)
library(dplyr) 
library(MLmetrics) # MAPE and MSE
library(forecast)
library(rugarch)
library(fGarch)
library(Johnson)
library(SuppDists)   # Johnson's SU distribution
library(PerformanceAnalytics) # For estimating the ES
library(xts)## Plot in ugarchroll works best with xts objects
library(Dowd) ## Package for test of independence of VaR
library(tseries) ## For conducting basic tests

# :::::::::::: Read the file
# setwd() removed: paths are set in R/00_setup.R
# import dataset
Crypto1 <- read.csv(DATA_FILE, header = TRUE)
Crypto <- read.csv(DATA_FILE, header = TRUE)
Crypto_Date   <- as.Date.character(Crypto$Date,format = "%d/%m/%Y")
Crypto_Date[1600]

# Crypto1 - without date column #

Crypto1 <- data.frame(Crypto1$Bitcoin, Crypto1$Ethereum, Crypto1$Litecoin, Crypto1$Monero, Crypto1$Ethereum.Classic)
colnames(Crypto1) <- c("BTC", "ETH", "LTC", "XMR", "ETC")
View(Crypto1)

# ::::::::: Find structural breaks
# ::::::::: tSNE and CUSUM based Binary Segmentation
# ::::::::: TWO change points

CPPP = 0
CPPPs = 0
CPPPa = 0
CPPP.tsne = 0


source(file.path("R", "functions", "cp_pca.R"))
Crypto1.pca <- CW.Anchal.multi.PCA(Crypto1, 100, 2)

CPPP <- data.frame(Crypto1.pca)
CPPP <- CPPP[,1]
CPPP <- sort(CPPP)
CPPP

source(file.path("R", "functions", "cp_pca_squared.R"))
Crypto1.pcas <- CW.Anchal.multi.PCAs(Crypto1, 100, 2)

CPPPs <- data.frame(Crypto1.pcas)
CPPPs <- CPPPs[,1]
CPPPs <- sort(CPPPs)
CPPPs


source(file.path("R", "functions", "cp_pca_abs.R"))
Crypto1.pcaa <- CW.Anchal.multi.PCAa(Crypto1, 100, 2)

CPPPa <- data.frame(Crypto1.pcaa)
CPPPa <- CPPPa[,1]
CPPPa <- sort(CPPPa)
CPPPa

source(file.path("R", "functions", "strucbreak.R"))
library(Rtsne)
set.seed(1)
Crypto1.cp <- CW.Anchal.multi(Crypto1, 100, 2)

CPPP.tsne <- data.frame(Crypto1.cp)
CPPP.tsne <- CPPP.tsne[,1]
CPPP.tsne <- sort(CPPP.tsne)
CPPP.tsne
