# =============================================================================
# 02_descriptive_stats_subsamples.R
#
# Purpose : Descriptive statistics of log returns by subsample (SS1-SS3): skewness,
#           excess kurtosis, volatility persistence, share of |z|>2 and |z|>3 days.
# Inputs  : data/Cryptocurrency6.csv; CPPP.tsne and Crypto_Date from
#           01_change_points.R
# Outputs : output/tables/subsample_descriptive_stats.csv
# Paper   : Table 6, Table A.1
#
# Run after R/00_setup.R and the preceding scripts (order: see README.md).
# =============================================================================

# ============================================================
# Subsample descriptive statistics for five cryptocurrencies
# Subsample boundaries are the t-SNE change points from 01_change_points.R
# (CPPP.tsne = 300, 1300  ->  26 Sep 2017 and 22 Jun 2020)
# SS2: Crypto_Date[300]  - day before Crypto_Date[1300]
# SS3: Crypto_Date[1300] - end of sample
# ============================================================

library(dplyr)
library(readr)
# e1071 is called with e1071:: below (not attached) so that it does not mask
# skewness()/kurtosis() from fBasics/PerformanceAnalytics used in later scripts

# ---- 1. Input: single combined file, one price column per coin ----
# Expected columns: Date, Bitcoin, Ethereum, Litecoin, Monero, Ethereum.Classic

input_file <- DATA_FILE   # set in R/00_setup.R

date_col <- "Date"
date_fmt <- "%d/%m/%Y"   # dates in data/Cryptocurrency6.csv are dd/mm/yyyy

# maps a short coin label -> standardized column name
# (standardization below turns spaces/dots/underscores into a single underscore,
#  so "Ethereum Classic", "Ethereum.Classic", "Ethereum_Classic" all become "Ethereum_Classic")
coin_cols <- list(
  BTC = "Bitcoin",
  ETH = "Ethereum",
  LTC = "Litecoin",
  XMR = "Monero",
  ETC = "Ethereum_Classic"
)

# ---- 2. Subsample cut points: from the t-SNE change points ----
# CPPP.tsne (row indices of the two breaks) and Crypto_Date are created in
# R/01_change_points.R; the indices are converted to dates here.
if (!exists("CPPP.tsne") || !exists("Crypto_Date")) {
  stop("Run R/01_change_points.R first: it creates CPPP.tsne and Crypto_Date.")
}
cp_index <- sort(as.numeric(CPPP.tsne))
cp1 <- Crypto_Date[cp_index[1]]   # SS1/SS2 boundary (index 300  -> 2017-09-26)
cp2 <- Crypto_Date[cp_index[2]]   # SS2/SS3 boundary (index 1300 -> 2020-06-22)
cat("Subsample boundaries: cp1 =", format(cp1), " cp2 =", format(cp2), "\n")

# ---- 3. Load the combined file once, standardize column names, compute log returns per coin ----
raw <- read_csv(input_file, show_col_types = FALSE)

cat("Original columns in file:\n")
print(colnames(raw))

# Standardize: trim whitespace, collapse spaces/dots/underscores into a single underscore
clean_names <- gsub("_+$", "", gsub("[\\. _]+", "_", trimws(colnames(raw))))
colnames(raw) <- clean_names

cat("\nStandardized columns:\n")
print(colnames(raw))

# Safety check: stop with a clear message if any expected coin column is missing
missing_cols <- setdiff(unlist(coin_cols), colnames(raw))
if (length(missing_cols) > 0) {
  stop(
    "These expected columns were not found after standardization: ",
    paste(missing_cols, collapse = ", "),
    "\nActual standardized columns are: ", paste(colnames(raw), collapse = ", "),
    "\nEdit coin_cols above to match, then re-run."
  )
}

raw <- raw %>%
  dplyr::mutate(Date = as.Date(.data[[date_col]], format = date_fmt)) %>%
  dplyr::arrange(Date)

load_returns <- function(raw, price_col) {
  df <- raw %>%
    dplyr::transmute(
      Date  = Date,
      Price = as.numeric(gsub(",", "", .data[[price_col]]))  # strip thousands separators if present
    ) %>%
    dplyr::filter(!is.na(Date), !is.na(Price)) %>%
    dplyr::arrange(Date)
  
  df <- df %>%
    dplyr::mutate(logret = c(NA, diff(log(Price)))) %>%
    dplyr::filter(!is.na(logret)) %>%
    dplyr::select(Date, logret)
  
  df
}

# ---- 4. Helper: assign subsample label ----
assign_subsample <- function(dates, cp1, cp2) {
  dplyr::case_when(
    dates < cp1 ~ "SS1",
    dates >= cp1 & dates < cp2 ~ "SS2",
    dates >= cp2 ~ "SS3"
  )
}

# ---- 5. Helper: compute all stats for one vector of returns ----
compute_stats <- function(r, full_sd, full_mean) {
  n <- length(r)
  
  # volatility persistence: lag-1 autocorrelation of squared returns
  sq <- r^2
  persistence <- if (n > 2) acf(sq, lag.max = 1, plot = FALSE)$acf[2] else NA
  
  # frequency of extreme events, standardized using FULL-SAMPLE mean/sd
  z <- (r - full_mean) / full_sd
  freq_2sd <- mean(abs(z) > 2) * 100
  freq_3sd <- mean(abs(z) > 3) * 100
  
  dplyr::tibble(
    N                = n,
    Mean             = mean(r),
    SD               = sd(r),
    Skewness         = e1071::skewness(r, type = 2),
    ExcessKurtosis   = e1071::kurtosis(r, type = 2),
    Persistence_L1   = as.numeric(persistence),
    Freq_2SD_pct     = freq_2sd,
    Freq_3SD_pct     = freq_3sd
  )
}

# ---- 6. Loop over coins, compute stats per subsample ----
results <- list()

for (coin in names(coin_cols)) {
  
  ret_df <- load_returns(raw, coin_cols[[coin]])
  ret_df$Subsample <- assign_subsample(ret_df$Date, cp1, cp2)
  
  # full-sample mean/sd used to standardize extreme-event counts
  full_mean <- mean(ret_df$logret)
  full_sd   <- sd(ret_df$logret)
  
  coin_stats <- ret_df %>%
    dplyr::filter(Subsample %in% c("SS1", "SS2", "SS3")) %>%
    dplyr::group_by(Subsample) %>%
    dplyr::group_modify(~ compute_stats(.x$logret, full_sd, full_mean)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Cryptocurrency = coin, .before = 1)
  
  results[[coin]] <- coin_stats
}

final_table <- dplyr::bind_rows(results) %>%
  dplyr::mutate(Subsample = factor(Subsample, levels = c("SS1", "SS2", "SS3"))) %>%
  dplyr::arrange(Cryptocurrency, Subsample)

print(final_table, n = Inf)

# ---- 7. Save to CSV ----
write_csv(final_table, file.path(TAB_DIR, "subsample_descriptive_stats.csv"))
