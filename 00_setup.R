# =============================================================================
# 00_setup.R
# Paths, packages and small helpers shared by every analysis script.
# Run this first, from an R session whose working directory is the repository
# root:  source("R/00_setup.R")   -- then run the numbered scripts in order.
# =============================================================================

# ---- Paths (all relative to the repository root) ----------------------------
if (!file.exists(file.path("data", "Cryptocurrency6.csv"))) {
  stop("Please set the working directory to the repository root ",
       "(the folder that contains README.md and the data/ folder) before running the code.")
}

DATA_FILE <- file.path("data", "Cryptocurrency6.csv")
FIG_DIR   <- file.path("output", "figures")
TAB_DIR   <- file.path("output", "tables")
for (d in c(FIG_DIR, TAB_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- Packages ----------------------------------------------------------------
# Attached in the same order as in the original analysis scripts. GAS is
# attached last on purpose: GAS::ES() and GAS::BacktestVaR() must take
# precedence over PerformanceAnalytics::ES() (the EGARCH scripts call
# BacktestVaR() before they load GAS themselves).
pkgs <- c("tidyverse", "ggplot2", "ggpubr", "gridExtra", "fBasics", "dplyr",
          "readr", "MLmetrics", "forecast", "rugarch", "fGarch", "Johnson",
          "SuppDists", "PerformanceAnalytics", "xts", "cvar", "Dowd", "tseries",
          "Rtsne", "FinTS", "PearsonDS", "gsl", "xtable", "e1071", "RTransferEntropy", "GAS")

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Please install the missing packages first:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))\n",
       "If a package is no longer on CRAN, install its archived version, e.g.\n",
       '  remotes::install_version("Dowd", version = "<version>")\n',
       "(the versions used are listed in sessionInfo.txt).")
}

# e1071 is only used through e1071:: in 02_descriptive_stats_subsamples.R,
# so it is not attached here.
for (p in setdiff(pkgs, "e1071")) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

# ---- Helpers ----------------------------------------------------------------
# The scripts were written interactively and call View() to inspect objects.
# View() is turned into a no-op when the code runs non-interactively
# (e.g. when the scripts are sourced from Rscript).
if (!interactive()) View <- function(...) invisible(NULL)

# Reproducibility of the t-SNE step and of any random starting values
set.seed(1)
